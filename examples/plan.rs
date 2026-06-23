use express_it::expr::Context;
use express_it::nodes::Node;
use express_it::plan::{CacheLookupStep, Plan, StepExecutor};
use express_it::{load_cache, save, save_cache};
use std::any::Any;

#[derive(Debug)]
pub struct CombatState {
    pub health: i32,
    pub max_health: i32,
    pub damage_dealt: i32,
    pub heal_received: i32,
}

pub struct CombatCtx;

impl Context for CombatCtx {
    type ContextItem<'w, 's> = CombatState;
}

fn main() {
    let mut state = CombatState {
        health: 100,
        max_health: 100,
        damage_dealt: 0,
        heal_received: 0,
    };
    println!("Initial State: {:?}", state);

    let plan = Plan::<CombatCtx>::new()
        // Save base damage into context + cache via save! macro
        .step(save!(
            CombatState,
            damage_dealt,
            Node::lit(35),
            "applied_damage"
        ))
        // Apply damage from context
        .step(save!(
            CombatState,
            health,
            Node::new(|ctx: &CombatState| ctx.health - ctx.damage_dealt)
        ))
        // Save heal to cache via save_cache! macro
        .step(save_cache!(
            "applied_heal",
            Node::new(|ctx: &CombatState| ctx.damage_dealt / 2)
        ))
        // Write heal into context
        .step(save!(
            CombatState,
            heal_received,
            Node::new(|ctx: &CombatState| ctx.damage_dealt / 2)
        ))
        // Standalone cache-only save via macro
        .step(save_cache!("sidebar_note", Node::lit(999)))
        // Use Node::alias() to save any expression directly into cache
        .step(Node::lit(42).alias("reusable_literal"));

    plan.run(&mut state);

    println!("Final State:   {:?}", state);
    assert_eq!(state.damage_dealt, 35);
    assert_eq!(state.health, 65); // 100 - 35 from step 2
    assert_eq!(state.heal_received, 17);
    assert_eq!(state.max_health, 100);

    // ---- Easy mechanism: load a value back from the cache ----
    let mut test_cache = express_it::plan::PlanCache::default();

    // Simulate the plan having written these keys earlier
    test_cache.set::<i32>("applied_damage", 73);
    test_cache.set::<i32>("reusable_literal", 77);

    // Macro form
    let dmg_lookup = load_cache!(i32, "applied_damage");
    let dmg_val: Box<dyn Any> =
        <_ as StepExecutor<CombatCtx>>::evaluate(&dmg_lookup, &state, &test_cache);
    assert_eq!(*dmg_val.downcast::<i32>().unwrap(), 73);

    // Explicit construction
    let lit_lookup = CacheLookupStep::<i32>::new("reusable_literal");
    let lit_val: Box<dyn Any> = <CacheLookupStep<i32> as StepExecutor<CombatCtx>>::evaluate(
        &lit_lookup,
        &state,
        &test_cache,
    );
    assert_eq!(*lit_val.downcast::<i32>().unwrap(), 77);

    println!("Cache save/load roundtrip verified!");
}
