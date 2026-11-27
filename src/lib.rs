#![doc = include_str!("../README.md")]

/// The traits everything else is built on.
///
/// [`Expr`](expr::Expr) is what a tree node does, [`Context`](expr::Context) and
/// [`ContextMut`](expr::ContextMut) say what it reads and what a plan may write,
/// [`AsExpression`](expr::AsExpression) decides what an operand may be, and
/// [`Deps`](expr::Deps)/[`DepKey`](expr::DepKey) are the sink a node reports its
/// dependencies into.
pub mod expr;
/// Flat n-ary reductions: [`Node::sum`](nodes::Node::sum) and
/// [`Node::product`](nodes::Node::product) fold a `Vec` of children in a loop.
///
/// These exist because of stack depth. Accumulating a tree in a loop means erasing
/// each step, and an erased chain costs one `eval` frame per element — measured at
/// 1 MiB stack at 60,000 levels and the 8 MiB main thread at 100,000, with an
/// abort rather than an error.
/// Accumulate the children into a vector instead and the depth stays at one node.
pub mod fold;
/// Comparison and boolean structure: the `.lt()`/`.le()`/`.gt()`/`.ge()`/`.eq()`
/// nodes, the `.and()`/`.or()`/`.not()` … combinators, and the `any!`/`all!`
/// macros.
///
/// The combinators take their operands by value and ask nothing of them beyond
/// `Expr`, so a boxed sub-expression works in logic exactly as it does in
/// arithmetic. What boxing costs is `Copy`: a node you have moved into a tree comes
/// back by `clone`, not by copy.
pub mod logic;
/// The tree itself: [`Node`](nodes::Node), the leaves it is built from
/// ([`LiteralNode`](nodes::LiteralNode), [`VarExpr`](nodes::VarExpr),
/// [`CapturedVar`](nodes::CapturedVar)), the constructors (`lit`, `new`, `var`,
/// `var_capture`, `from_expr`), the `var_leaf!` macro, the `cast` conversions, and
/// the cache-save step.
pub mod nodes;
/// Arithmetic: the `+ - * / %` binary operator impls on `Node` — for `+ - * /` a
/// primitive may sit on the left, while `%` needs a `Node` on the left — the
/// unary `-` and `!` impls, and the method nodes `powf`, `min`, `max`, `abs`,
/// `sqrt`, `acos`, `clamp`. The three arguments of `clamp` are either all nodes
/// or all primitives; `node.clamp(0.0, node)` is a type error.
///
/// Its public items are the node structs those operators and methods build
/// (`AddNode`, `NegNode`, `MaxNode`, `FloatClampNode`, …) and the seven extension
/// traits that attach the methods to `Node` (`ExprMax`, `ExprClamp`, …), all
/// seven of which the prelude re-exports. There are no free functions.
pub mod ops;
/// Ordered side effects: [`Plan`](plan::Plan), the step types, the
/// [`PlanCache`](plan::PlanCache), [`PlanError`](plan::PlanError), and the
/// `save!` / `save_cache!` / `load_cache!` macros.
pub mod plan;

/// Everything needed to build, compose, and run an expression, in one import.
///
/// The arithmetic operators (`+ - * / %`, unary `-` and `!`) are trait impls on
/// `std::ops`, so they need no import at all. Everything else — `.max()`, `.lt()`,
/// `.and()`, `.eval()` — is a method of a trait in this crate, and each one has to
/// be in scope for its own method to resolve. The prelude carries all of them,
/// plus the items a caller has to name rather than call: `StepExecutor` and the
/// three step types for a custom step, `PlanCache` for its signatures,
/// `AsCast` for a generic cast bound, and `Deps` with `DepKey` for a leaf of your
/// own that reports dependencies. It also re-exports all seven exported macros —
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
    pub use crate::expr::{
        AsExpression, BoolExpr, Context, ContextMut, DepKey, Deps, Expr, StoredExpr,
    };
    pub use crate::fold::{FoldNode, Product, Sum};
    pub use crate::logic::{
        ExprCmpEq, ExprCmpGe, ExprCmpGt, ExprCmpLe, ExprCmpLt, ExprCmpNotEq, ExprLogicNot,
        ExprLogicalAnd, ExprLogicalNand, ExprLogicalNor, ExprLogicalNxor, ExprLogicalOr,
        ExprLogicalXor,
    };
    pub use crate::nodes::{AsCast, CacheSaveStep, CapturedFetch, CapturedVar, Node, Var, VarExpr};
    pub use crate::ops::{ExprAbs, ExprAcos, ExprClamp, ExprMax, ExprMin, ExprPow, ExprSqrt};
    pub use crate::plan::{
        AssignmentStep, CacheLookupStep, Plan, PlanCache, PlanError, StepExecutor,
    };
    pub use crate::{all, any, load_cache, save, save_cache, save_with, var_leaf};
}

/// Test-only leaves and key helpers, for the `get_dependencies` tests.
///
/// Two ways to get per-attribute identity, and the one that does not: a leaf with
/// its own type (`Probe<A>`, keyed by `DepKey::type_of::<A>()`), a named variable
/// through `Node::var` (keyed by `DepKey::name`), and a `Node::new` variable,
/// which reports the key of `VarExpr<N, C>` so every variable of one value type
/// and context shares a single key — the tests in `nodes.rs` pin that.
#[cfg(test)]
pub(crate) mod dep_probe {
    use crate::expr::{Context, DepKey, Deps, Expr};
    use crate::nodes::Node;
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

        fn get_dependencies(&self, deps: &mut Deps) {
            deps.insert(DepKey::type_of::<A>());
        }

        fn render(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}", std::any::type_name::<A>())
        }
    }

    pub fn attr<A: Attribute>() -> Node<f32, ProbeCtx, Probe<A>> {
        Node::from_expr(Probe(PhantomData))
    }

    /// The key a `Probe<A>` leaf reports — the type-derived one.
    pub fn key<A: 'static>() -> DepKey {
        DepKey::type_of::<A>()
    }

    /// Builds a key set, to compare against the keys a whole tree reports.
    pub fn set(keys: impl IntoIterator<Item = DepKey>) -> Deps {
        keys.into_iter().collect()
    }

    /// The keys `expr` reports, walked from this node down.
    pub fn keys<N, E: Expr<N, ProbeCtx>>(expr: &E) -> Deps {
        let mut deps = Deps::new();
        expr.get_dependencies(&mut deps);
        deps
    }
}
