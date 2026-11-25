//! One glob import has to be enough: every trait whose method the workload calls,
//! every type a caller has to write down, plus the four exported macros, arrive
//! through `express_it::prelude`.
//!
//! The last test pins the other half of the contract — the prelude is additive,
//! so the pre-existing full paths keep resolving and name the very same items.

use express_it::prelude::*;
use std::any::Any;

#[derive(Clone, Copy, Debug)]
struct State {
    hp: f32,
    atk: f32,
    def: f32,
    shield: f32,
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

impl ContextMut for Ctx {
    type ContextItemMut<'w, 's> = State;

    fn as_readonly<'w, 's, 'a>(
        mut_item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's> {
        *mut_item
    }
}

fn hp() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|state: &State| state.hp)
}

fn atk() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|state: &State| state.atk)
}

fn def() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|state: &State| state.def)
}

fn start() -> State {
    State {
        hp: 10.0,
        atk: 20.0,
        def: 5.0,
        shield: 0.0,
    }
}

#[test]
fn prelude_alone_supports_a_boolean_and_math_workload() {
    let state = start();

    // `any!`, `all!`, `.gt`, `.or`, `.and`, `.not`, `eval`.
    let cond = any![hp().gt(0.0), all![hp().lt(25.0), atk().gt(def())].not()];
    assert!(cond.eval(&state), "the first disjunct alone is enough");

    // `.max`, `.sqrt`, `.clamp`, `.abs`, `.powf`, `.acos`, unary `-`, `+`.
    assert_eq!((hp() + 1.0).max(25.0).sqrt().eval(&state), 5.0);
    assert_eq!(hp().clamp(20.0, 30.0).eval(&state), 20.0);
    assert_eq!((-hp()).abs().eval(&state), 10.0);
    assert_eq!(atk().powf(2.0).eval(&state), 400.0);
    assert!(
        def().acos().eval(&state).is_nan(),
        "acos of 5.0, out of the domain: the math traits are in scope and reach f32"
    );
}

#[test]
fn prelude_alone_supports_a_plan_built_with_the_three_step_macros() {
    let mut state = start();

    let plan = Plan::<Ctx>::new()
        .step(save!(
            State,
            shield,
            (atk() - def()).max(0.0),
            "shield_applied"
        ))
        .step(save_cache!("hp_after", hp()))
        .step(load_cache!(f32, "shield_applied"));

    plan.run(&mut state)
        .expect("every key the plan reads exists, so the run succeeds");
    assert_eq!(state.shield, 15.0);

    let err: PlanError = Plan::<Ctx>::new()
        .step(load_cache!(f32, "nobody_wrote_this"))
        .run(&mut state)
        .expect_err("a missing key is an Err, not a panic");
    assert!(matches!(err, PlanError::CacheMiss { .. }));
}

/// A step written by hand, named entirely through the prelude glob: `StepExecutor`,
/// `PlanCache`, `PlanError`, `CacheLookupStep`.
///
/// It delegates the read to a `CacheLookupStep`, which is also how a step outside
/// the crate reports what it can report: `PlanError`'s variants are
/// `#[non_exhaustive]`, so downstream code propagates the delegate's error instead
/// of constructing one.
struct ShieldFromCache {
    lookup: CacheLookupStep<f32>,
}

impl StepExecutor<Ctx> for ShieldFromCache {
    fn evaluate(&self, ctx: &State, cache: &PlanCache) -> Result<Box<dyn Any>, PlanError> {
        // Qualified because `C` is only reachable through an associated type here.
        <_ as StepExecutor<Ctx>>::evaluate(&self.lookup, ctx, cache)
    }

    fn apply(
        &self,
        ctx: &mut State,
        value: Box<dyn Any>,
        _cache: &mut PlanCache,
    ) -> Result<(), PlanError> {
        // Under `Plan::run` the value always comes from this step's own `evaluate`,
        // so the downcast below succeeds. A hand-call with another type cannot be
        // reported downstream — there is no way to name a `PlanError` variant here.
        if let Ok(value) = value.downcast::<f32>() {
            ctx.shield = *value;
        }
        Ok(())
    }
}

#[test]
fn prelude_alone_supports_a_hand_written_step() {
    let mut state = start();

    let plan = Plan::<Ctx>::new()
        .step(save_cache!("shield_seed", (atk() - def()).max(0.0)))
        .step(ShieldFromCache {
            lookup: CacheLookupStep::new("shield_seed"),
        });

    plan.run(&mut state)
        .expect("the seed key was written by the step before");
    assert_eq!(state.shield, 15.0);
}

#[test]
fn a_hand_written_step_reports_a_miss_from_the_lookup_it_delegates() {
    let mut state = start();

    let err = Plan::<Ctx>::new()
        .step(ShieldFromCache {
            lookup: CacheLookupStep::new("nobody_wrote_this"),
        })
        .run(&mut state)
        .expect_err("the delegate's CacheMiss travels through the custom step");

    assert!(matches!(err, PlanError::CacheMiss { .. }));
    assert_eq!(state.shield, 0.0, "a failed run writes nothing");
}

#[test]
fn prelude_alone_names_a_cast_bound_and_the_three_step_types() {
    // The bound a generic cast helper needs, spelled with the prelude's `AsCast`.
    fn as_i32<T: AsCast<i32>>(value: T) -> i32 {
        value.as_cast()
    }

    fn as_u8<T: AsCast<u8>>(value: T) -> u8 {
        value.as_cast()
    }

    assert_eq!(as_i32(9.7f32), 9, "fraction dropped toward zero");
    assert_eq!(as_i32(1e30f64), i32::MAX, "float to int saturates");
    assert_eq!(as_u8(-1i32), 255, "int to int wraps on two's complement");
    assert_eq!(
        hp().cast::<i32>().eval(&start()),
        10,
        "and the node cast works"
    );

    // The three step types, named from the prelude rather than their home modules.
    let assignment: AssignmentStep<f32, _, _> = save!(State, shield, hp());
    let cache_save: CacheSaveStep<f32, Ctx, _> = save_cache!("k", hp());
    let lookup: CacheLookupStep<f32> = load_cache!(f32, "k");

    // Each one is a step this crate can run.
    let mut state = start();
    let plan = Plan::<Ctx>::new()
        .step(assignment)
        .step(cache_save)
        .step(lookup);
    plan.run(&mut state).expect("three plain steps must run");
    assert_eq!(state.shield, 10.0);
}

#[test]
fn existing_full_paths_still_resolve_and_name_the_same_items() {
    // Same trait, two spellings: a generic bounded on the prelude re-export only
    // typechecks when the re-export is the original item.
    fn via_prelude<T: ExprMax<f32>>(_value: &T) {}
    fn via_module_path<T: express_it::ops::ExprMax<f32>>(_value: &T) {}

    via_prelude(&hp());
    via_module_path(&hp());

    let _plan: express_it::plan::Plan<Ctx> = express_it::plan::Plan::<Ctx>::new();
    let _boxed: Option<express_it::expr::StoredExpr<f32, Ctx>> = None;
    let _bool_expr: Option<express_it::expr::BoolExpr<Ctx>> = None;
    let _alias: Option<express_it::nodes::Var<f32, Ctx>> = None;
    let _lookup: express_it::plan::CacheLookupStep<f32> = load_cache!(f32, "k");

    // Item 7 from the outside: plan steps clone even though `Ctx` is a plain unit
    // struct that is neither Clone nor Copy. The helper is generic so clippy's
    // `clone_on_copy` does not fire on the `Copy` case.
    fn cloned<T: Clone>(step: T) -> T {
        step.clone()
    }

    let _saved: express_it::nodes::CacheSaveStep<f32, Ctx, _> = cloned(save_cache!("k", hp()));
    let _assigned: express_it::plan::AssignmentStep<f32, _, _> = cloned(save!(State, shield, hp()));
}
