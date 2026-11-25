use crate::nodes::{LiteralNode, Node, VarExpr};
use std::any::TypeId;
use std::collections::HashSet;

pub trait Context: Send + Sync {
    type ContextItem<'w, 's>;
}

pub trait ContextMut: Context {
    type ContextItemMut<'w, 's>;

    fn as_readonly<'w, 's, 'a>(
        mut_item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's>;
}

pub trait Expr<N, C: Context> {
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N;

    /// Collects the dependency keys of every leaf reachable from this node.
    ///
    /// The result is a **may-read superset**, not the set of attributes a given
    /// evaluation actually touches: the walk visits every child of every node,
    /// while `eval` may skip some of them. `&&` and `||` are the case that
    /// matters — `a && b` reports both operands even though a live `eval`
    /// short-circuits and never reads `b` once `a` is false. Arithmetic is exact
    /// by contrast, since every arithmetic node (`+ - * / %`, `powf`, `min`,
    /// `max`, `abs`, `sqrt`, `clamp(..)`, casts) evaluates all of its children.
    /// Watching a leaf that a particular evaluation skipped only costs a
    /// redundant re-evaluation, so the superset is a documented property rather
    /// than a defect, and callers get a conservative answer they can register
    /// edges from ahead of the first evaluation.
    ///
    /// Leaf identity is up to the leaf: the default implementation contributes
    /// nothing, which is correct for nodes that hold no variables (a literal).
    /// Order is unspecified — the sink is a `HashSet`, so a caller that depends
    /// on a particular order must sort the collected keys.
    fn get_dependencies(&self, _deps: &mut HashSet<TypeId>) {}
}

pub type BoolExpr<C> = Node<bool, C, VarExpr<bool, C>>;

/// A type-erased expression: the shape a tree holds when a sub-expression is
/// chosen at runtime (or stored in a heterogeneous collection) instead of being
/// named in the type.
///
/// One limitation follows from the box itself: it is never `Copy`, so a
/// `StoredExpr` composes with the arithmetic operators and the method nodes
/// (`max`, `clamp`, `sqrt`, …) but not with the comparison (`.ge()`, `.le()`, …)
/// or logical (`.and()`, `.or()`, …) combinators, whose impls require
/// `L: Expr<N, C> + Copy`.
pub type StoredExpr<N, C> = Box<dyn Expr<N, C> + Send + Sync>;

// Forwards through `Box` so the alias above composes into a tree: without it a
// `StoredExpr` implements nothing and cannot be a `Node` payload, an operand of
// `+ - * /`, or the expression of a plan step.
impl<N, C: Context, E: Expr<N, C> + ?Sized> Expr<N, C> for Box<E> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        (**self).eval(ctx)
    }

    #[inline(always)]
    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        (**self).get_dependencies(deps);
    }
}

pub trait AsExpression<N, C: Context> {
    type Target: Expr<N, C> + Send + Sync + 'static;

    /// Consumes the wrapper and hands back the expression it holds.
    ///
    /// Named `as_expr` rather than `into_expr` because it takes `self` by value:
    /// `Node` is a zero-cost builder wrapper, so stripping it is a free cast, and
    /// every operator already hands back a fresh `Node` that cannot be borrowed
    /// past this point. Not an `as_` reference cast, hence the allow.
    #[allow(clippy::wrong_self_convention)]
    fn as_expr(self) -> Self::Target;
}

impl<N, C: Context, E> AsExpression<N, C> for Node<N, C, E>
where
    E: Expr<N, C> + Clone + Send + Sync + 'static,
{
    type Target = E;

    #[inline(always)]
    fn as_expr(self) -> Self::Target {
        self.expr
    }
}

macro_rules! impl_as_expression_for_primitives {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<C: Context> AsExpression<$ty, C> for $ty {
                type Target = LiteralNode<$ty>;

                #[inline(always)]
                fn as_expr(self) -> Self::Target {
                    LiteralNode { value: self }
                }
            }
        )*
    };
}

impl_as_expression_for_primitives!(
    f32, f64, i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, bool, char,
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::{LiteralNode, Node};

    pub struct Ctx;

    impl Context for Ctx {
        type ContextItem<'w, 's> = ();
    }

    #[test]
    fn test_as_expression_node_unwrap() {
        let wrapped: Node<i32, Ctx, _> = Node::lit(7);
        let expr: LiteralNode<i32> = wrapped.as_expr();
        assert_eq!(expr.value, 7);
    }

    #[test]
    fn test_as_expression_primitives() {
        let f: LiteralNode<f32> = AsExpression::<f32, Ctx>::as_expr(2.75f32);
        assert_eq!(f.value, 2.75);

        let i: LiteralNode<i32> = AsExpression::<i32, Ctx>::as_expr(-42i32);
        assert_eq!(i.value, -42);

        let u: LiteralNode<u32> = AsExpression::<u32, Ctx>::as_expr(99u32);
        assert_eq!(u.value, 99);

        let b: LiteralNode<bool> = AsExpression::<bool, Ctx>::as_expr(true);
        assert!(b.value);

        let c: LiteralNode<char> = AsExpression::<char, Ctx>::as_expr('x');
        assert_eq!(c.value, 'x');
    }

    #[test]
    fn stored_expr_composes_into_a_tree() {
        use crate::dep_probe::{Attack, Defense, ProbeCtx, attr, key, keys, set};

        // A boxed expression as the payload of a node, then an operand of a
        // binary node: exactly the composition the exported `StoredExpr` alias
        // could not express before there was an `Expr` impl forwarding `Box`.
        let boxed: StoredExpr<f32, ProbeCtx> = Box::new(attr::<Attack>().expr);
        let boxed_node: Node<f32, ProbeCtx, _> = Node::from_expr(boxed);

        let tree = boxed_node + attr::<Defense>();

        assert_eq!(tree.eval(&()), 12.0, "the boxed operand still evaluates");
        assert_eq!(
            keys(&tree),
            set([key::<Attack>(), key::<Defense>()]),
            "get_dependencies forwards through the box"
        );
    }
}
