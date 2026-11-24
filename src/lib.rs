pub mod expr;
pub mod logic;
pub mod nodes;
pub mod ops;
pub mod plan;

/// Test-only leaf with per-attribute identity, for the `get_dependencies` tests.
///
/// A variable built through `Node::new` reports the key of `VarExpr<N, C>`, so
/// every variable of one value type and context shares a single key and cannot
/// be told apart. A consumer that needs per-attribute identity supplies its own
/// leaf as the `E` parameter of `Node`, so the forwarding tests do the same.
#[cfg(test)]
pub(crate) mod dep_probe {
    use crate::expr::{Context, Expr};
    use crate::nodes::Node;
    use std::any::TypeId;
    use std::collections::HashSet;
    use std::marker::PhantomData;

    pub struct ProbeCtx;

    impl Context for ProbeCtx {
        type ContextItem<'w, 's> = ();
    }

    /// A named attribute, evaluating to a fixed value.
    pub trait Attribute: 'static {
        const VALUE: f32;
    }

    pub struct Attack;
    pub struct Defense;
    pub struct Haste;
    pub struct Speed;

    impl Attribute for Attack {
        const VALUE: f32 = 10.0;
    }
    impl Attribute for Defense {
        const VALUE: f32 = 2.0;
    }
    impl Attribute for Haste {
        const VALUE: f32 = 3.0;
    }
    impl Attribute for Speed {
        const VALUE: f32 = 4.0;
    }

    pub struct Probe<A: Attribute>(PhantomData<A>);

    impl<A: Attribute> Clone for Probe<A> {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl<A: Attribute> Copy for Probe<A> {}

    impl<A: Attribute, C: Context> Expr<f32, C> for Probe<A> {
        fn eval(&self, _ctx: &C::ContextItem<'_, '_>) -> f32 {
            A::VALUE
        }

        fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
            deps.insert(TypeId::of::<A>());
        }
    }

    pub fn attr<A: Attribute>() -> Node<f32, ProbeCtx, Probe<A>> {
        Node {
            expr: Probe(PhantomData),
            _marker: PhantomData,
        }
    }

    pub fn key<A: 'static>() -> TypeId {
        TypeId::of::<A>()
    }

    pub fn set(keys: impl IntoIterator<Item = TypeId>) -> HashSet<TypeId> {
        keys.into_iter().collect()
    }

    /// The keys `expr` reports, walked from this node down.
    pub fn keys<N, E: Expr<N, ProbeCtx>>(expr: &E) -> HashSet<TypeId> {
        let mut deps = HashSet::new();
        expr.get_dependencies(&mut deps);
        deps
    }
}
