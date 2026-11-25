//! End-to-end coverage for the plan layer: `Plan::run` writes through to the
//! context, threads one shared `PlanCache` between steps, and reports failures as
//! `Err(PlanError)` instead of panicking.
//!
//! The assertions are ported from `examples/plan.rs`, which only checks anything
//! when someone remembers to run it; here `cargo test` runs them.

use express_it::expr::{Context, ContextMut};
use express_it::nodes::Node;
use express_it::plan::{CacheLookupStep, Plan, PlanCache, PlanError, StepExecutor};
use express_it::{load_cache, save, save_cache};

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct CombatState {
    pub health: i32,
    pub max_health: i32,
    pub damage_dealt: i32,
    pub heal_received: i32,
    pub sidebar: i32,
}

pub struct CombatCtx;

impl Context for CombatCtx {
    type ContextItem<'w, 's> = CombatState;
}

impl ContextMut for CombatCtx {
    type ContextItemMut<'w, 's> = CombatState;

    fn as_readonly<'w, 's, 'a>(
        mut_item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's> {
        *mut_item
    }
}

fn combat() -> CombatState {
    CombatState {
        health: 100,
        max_health: 100,
        ..Default::default()
    }
}

#[test]
fn plan_writes_context_and_cache_in_step_order() {
    // The same plan as examples/plan.rs: cache-keyed write, a step that reads what
    // the previous step wrote, cache-only steps, and Node::alias.
    let plan = Plan::<CombatCtx>::new()
        .step(save!(
            CombatState,
            damage_dealt,
            Node::lit(35),
            "applied_damage"
        ))
        .step(save!(
            CombatState,
            health,
            Node::new(|ctx: &CombatState| ctx.health - ctx.damage_dealt)
        ))
        .step(save_cache!(
            "applied_heal",
            Node::new(|ctx: &CombatState| ctx.damage_dealt / 2)
        ))
        .step(save!(
            CombatState,
            heal_received,
            Node::new(|ctx: &CombatState| ctx.damage_dealt / 2)
        ))
        .step(save_cache!("sidebar_note", Node::lit(999)))
        .step(Node::lit(42).alias("reusable_literal"));

    let mut state = combat();
    plan.run(&mut state)
        .expect("a plan whose keys all exist must succeed");

    assert_eq!(state.damage_dealt, 35);
    assert_eq!(state.health, 65, "step 2 read step 1's write");
    assert_eq!(state.heal_received, 17);
    assert_eq!(state.max_health, 100, "untouched fields stay untouched");
}

#[test]
fn save_cache_then_load_cache_roundtrips_within_one_run() {
    let plan = Plan::<CombatCtx>::new()
        .step(save_cache!("heal", Node::lit(24)))
        // Reads the key the previous step stored: fails the run if the shared
        // cache were not threaded from step to step.
        .step(load_cache!(i32, "heal"));

    let mut state = combat();
    plan.run(&mut state).expect("roundtrip must succeed");

    // The value itself is observable through a lookup step evaluated by hand.
    let empty = PlanCache::default();
    let mut filled = PlanCache::default();
    filled.set("heal", 24i32);
    let value =
        <_ as StepExecutor<CombatCtx>>::evaluate(&load_cache!(i32, "heal"), &state, &filled)
            .expect("the key is present with the right type");
    assert_eq!(*value.downcast::<i32>().unwrap(), 24);
    assert!(
        <_ as StepExecutor<CombatCtx>>::evaluate(&load_cache!(i32, "heal"), &state, &empty)
            .is_err(),
        "an empty cache has nothing to hand back"
    );
}

#[test]
fn cache_save_step_populates_a_cache_the_caller_owns() {
    let mut state = combat();
    let mut cache = PlanCache::default();
    let step = save_cache!("reusable_literal", Node::lit(77));

    let value = <_ as StepExecutor<CombatCtx>>::evaluate(&step, &state, &cache)
        .expect("a cache-save step always evaluates");
    <_ as StepExecutor<CombatCtx>>::apply(&step, &mut state, value, &mut cache)
        .expect("the value came from this step");

    assert_eq!(cache.get::<i32>("reusable_literal"), Some(&77));
    assert!(cache.has::<i32>("reusable_literal"));
    assert!(
        !cache.has::<f32>("reusable_literal"),
        "has is type-strict, like get"
    );

    let looked_up = <CacheLookupStep<i32> as StepExecutor<CombatCtx>>::evaluate(
        &CacheLookupStep::<i32>::new("reusable_literal"),
        &state,
        &cache,
    )
    .expect("the save step wrote an i32 under this key");
    assert_eq!(*looked_up.downcast::<i32>().unwrap(), 77);
}

#[test]
fn later_step_reads_the_field_an_earlier_step_wrote() {
    let plan = Plan::<CombatCtx>::new()
        .step(save!(CombatState, damage_dealt, Node::lit(40)))
        .step(save!(
            CombatState,
            health,
            Node::new(|ctx: &CombatState| ctx.health - ctx.damage_dealt)
        ))
        .step(save!(
            CombatState,
            sidebar,
            Node::new(|ctx: &CombatState| ctx.health * 2)
        ));

    let mut state = combat();
    plan.run(&mut state).expect("plan must succeed");

    assert_eq!(state.damage_dealt, 40);
    assert_eq!(state.health, 60, "step 2 saw step 1");
    assert_eq!(state.sidebar, 120, "step 3 saw step 2");
}

#[test]
fn loading_a_key_nobody_wrote_reports_err() {
    let plan = Plan::<CombatCtx>::new()
        .step(save!(CombatState, damage_dealt, Node::lit(35)))
        .step(load_cache!(i32, "never_written"));

    let mut state = combat();
    let err = plan
        .run(&mut state)
        .expect_err("a missing key must surface as Err, not a panic");

    // The fields are still readable; the `..` is what a `#[non_exhaustive]` variant
    // demands of a pattern written outside the crate that defines it, so this is the
    // shape every downstream match has to take. Borrowed, so `err` stays usable.
    let PlanError::CacheMiss { key, .. } = &err else {
        panic!("expected CacheMiss, got {err:?}");
    };
    assert_eq!(key, "never_written");
    assert!(err.to_string().contains("never_written"));
    assert_eq!(
        state.damage_dealt, 35,
        "steps before the failure stay applied: a failed run is not rolled back"
    );
}

#[test]
fn loading_a_key_of_the_wrong_type_reports_a_distinguishable_err() {
    let plan = Plan::<CombatCtx>::new()
        .step(save_cache!("sidebar_note", Node::lit(999i32)))
        .step(load_cache!(f32, "sidebar_note"));

    let mut state = combat();
    let err = plan
        .run(&mut state)
        .expect_err("reading an i32 entry as f32 must be Err");

    let PlanError::CacheTypeMismatch { key, expected, .. } = err else {
        panic!("expected CacheTypeMismatch, got {err:?}");
    };
    assert_eq!(key, "sidebar_note");
    assert_eq!(expected, "f32");
}
