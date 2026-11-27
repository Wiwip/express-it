//! `Node::var_capture` is the only way to read an attribute through an environment
//! the fetch closes over, and none of it may depend on being inside the crate: the
//! payload type is exported, the constructor is on `Node`, and the prelude re-exports
//! both.
//!
//! The interesting assertions are the two that used to be impossible. A captured
//! variable is `Clone` and never `Copy`, so before the combinators stopped asking
//! their operands to be `Copy`, a captured variable could have been arithmetic-only —
//! unable to take `.gt()` or `.and()` at all. `a_captured_variable_composes_in_boolean
//! _positions` is the guard, and `a_captured_variable_clones_into_two_trees` is the
//! price that came with the capture.

use express_it::prelude::*;

#[derive(Clone, Copy, Debug, Default)]
struct State {
    hp: f32,
    attack: f32,
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

fn scaled(scale: f32) -> Node<f32, Ctx, CapturedVar<f32, Ctx>> {
    Node::var_capture("scaled", move |state: &State| state.hp * scale)
}

#[test]
fn a_captured_variable_reads_through_its_capture() {
    let half = scaled(0.5);
    let doubled = scaled(2.0);
    let state = State {
        hp: 10.0,
        ..Default::default()
    };

    assert_eq!(half.eval(&state), 5.0);
    assert_eq!(doubled.eval(&state), 20.0);
    assert_eq!(
        half.expr.name(),
        "scaled",
        "the payload keeps the name it was keyed by"
    );
}

#[test]
fn a_captured_variable_reports_its_name_and_prints_it() {
    let tree = scaled(2.0) + 1.0;
    let deps = tree.dependencies();

    assert_eq!(deps.len(), 1, "one variable, one key");
    assert!(deps.contains_name("scaled"));
    assert_eq!(
        format!("{tree}"),
        "(var:scaled + 1)",
        "the same rendering a name-keyed `Node::var` gives"
    );
}

#[test]
fn a_captured_variable_composes_in_boolean_positions() {
    // Two captured variables on opposite sides of a comparison, then boolean
    // structure. Neither operand is `Copy`, which is exactly what the combinators
    // used to require.
    let threshold = 5.0;
    let hp = Node::<f32, Ctx, _>::var_capture("hp_less_threshold", move |state: &State| {
        state.hp - threshold
    });
    let attack = Node::<f32, Ctx, _>::var_capture("attack", |state: &State| state.attack);

    let dangerous = hp.gt(10.0).and(attack.ge(2.0));
    let deps = dangerous.dependencies();

    assert!(dangerous.eval(&State {
        hp: 40.0,
        attack: 3.0
    }));
    assert!(!dangerous.eval(&State {
        hp: 40.0,
        attack: 1.0
    }));
    assert!(deps.contains_name("hp_less_threshold"));
    assert!(deps.contains_name("attack"));
}

#[test]
fn a_captured_variable_clones_into_two_trees() {
    // The price of the capture: `Clone`, not `Copy`, so each tree gets a clone and
    // both see the same captured environment.
    let variable = scaled(3.0);
    let first = variable.clone() + 1.0;
    let second = variable * 2.0;
    let state = State {
        hp: 2.0,
        ..Default::default()
    };

    assert_eq!(first.eval(&state), 7.0);
    assert_eq!(second.eval(&state), 12.0);
}

#[test]
fn a_captured_variable_is_the_expression_of_a_plan_step() {
    let boost = 4.0;
    let mut state = State {
        hp: 5.0,
        ..Default::default()
    };

    let plan = Plan::<Ctx>::new().step(save!(
        State,
        hp,
        Node::<f32, Ctx, _>::var_capture("hp_boosted", move |state: &State| state.hp * boost)
    ));
    plan.run(&mut state)
        .expect("an assignment step never fails to evaluate");

    assert_eq!(state.hp, 20.0, "the step saw the captured 4.0");
}
