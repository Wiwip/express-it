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
    fn get_dependencies(&self, _deps: &mut HashSet<TypeId>) {}
}

pub type BoolExpr<C> = Node<bool, C, VarExpr<bool, C>>;

pub type StoredExpr<N, C> = Box<dyn Expr<N, C> + Send + Sync>;

pub trait AsExpression<N, C: Context> {
    type Target: Expr<N, C> + Send + Sync + 'static;

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
        let f: LiteralNode<f32> = AsExpression::<f32, Ctx>::as_expr(3.14f32);
        assert_eq!(f.value, 3.14);

        let i: LiteralNode<i32> = AsExpression::<i32, Ctx>::as_expr(-42i32);
        assert_eq!(i.value, -42);

        let u: LiteralNode<u32> = AsExpression::<u32, Ctx>::as_expr(99u32);
        assert_eq!(u.value, 99);

        let b: LiteralNode<bool> = AsExpression::<bool, Ctx>::as_expr(true);
        assert!(b.value);

        let c: LiteralNode<char> = AsExpression::<char, Ctx>::as_expr('x');
        assert_eq!(c.value, 'x');
    }
}
