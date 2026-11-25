#![doc = include_str!("../README.md")]

pub mod expr;
pub mod logic;
pub mod nodes;
pub mod ops;
pub mod plan;

/// Everything needed to build, compose, and run an expression, in one import.
///
/// The arithmetic operators (`+ - * / %`, unary `-` and `!`) are trait impls on
/// `std::ops`, so they need no import at all. Everything else — `.max()`, `.lt()`,
/// `.and()`, `.eval()` — is a method of a trait in this crate, and each one has to
/// be in scope for its own method to resolve. The prelude carries all of them,
/// plus the items a caller has to name rather than call: `StepExecutor` and the
/// three step types for a custom step, `PlanCache` for its signatures, and
/// `AsCast` for a generic cast bound. It also re-exports all six exported macros —
/// `all!`, `any!`, `save!`, `save_cache!`, `load_cache!`, and `var_leaf!` — so the
/// one glob carries the macros along with the traits.
///
/// Purely additive: every item stays at its original path, so `use
/// express_it::ops::ExprMax;` and friends keep working.
///
/// ```
/// use express_it::prelude::*;
///
/// struct Stats {
///     hp: f32,
/// }
///
/// struct GameCtx;
///
/// impl Context for GameCtx {
///     type ContextItem<'w, 's> = Stats;
/// }
///
/// let hp = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.hp);
///
/// // `.lt`, `.and`, `.max` and `eval` all come from the glob import above.
/// let low = hp.lt(25.0).and(all![]);
/// let floored = hp.max(25.0);
///
/// let wounded = Stats { hp: 10.0 };
/// assert!(low.eval(&wounded));
/// assert_eq!(floored.eval(&wounded), 25.0);
/// ```
pub mod prelude {
    pub use crate::expr::{AsExpression, BoolExpr, Context, ContextMut, Expr, StoredExpr};
    pub use crate::logic::{
        ExprCmpEq, ExprCmpGe, ExprCmpGt, ExprCmpLe, ExprCmpLt, ExprCmpNotEq, ExprLogicNot,
        ExprLogicalAnd, ExprLogicalNand, ExprLogicalNor, ExprLogicalNxor, ExprLogicalOr,
        ExprLogicalXor,
    };
    pub use crate::nodes::{AsCast, CacheSaveStep, Node, Var, VarExpr};
    pub use crate::ops::{ExprAbs, ExprAcos, ExprClamp, ExprMax, ExprMin, ExprPow, ExprSqrt};
    pub use crate::plan::{
        AssignmentStep, CacheLookupStep, Plan, PlanCache, PlanError, StepExecutor,
    };
    pub use crate::{all, any, load_cache, save, save_cache, var_leaf};
}

/// Test-only leaf with per-attribute identity, for the `get_dependencies` tests.
///
/// A variable built through `Node::new` reports the key of `VarExpr<N, C>`, so
/// every variable of one value type and context shares a single key and cannot
/// be told apart. A consumer that needs per-attribute identity supplies its own
/// leaf as the `E` parameter of `Node` through `Node::from_expr`, so the
/// forwarding tests do the same.
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
        Node::from_expr(Probe(PhantomData))
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
