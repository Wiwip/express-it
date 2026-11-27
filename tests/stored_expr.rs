//! `StoredExpr` has to be usable from outside the crate: it is exported, so a
//! boxed expression must be composable into a tree like any other sub-expression.
//!
//! Before the `Expr` impl that forwards through `Box`, this file did not compile —
//! the alias implemented nothing, so a `StoredExpr` could be neither a node payload
//! nor an operand.
//!
//! Scope note: arithmetic (`+ - * / %`) and the method nodes (`max`, `min`, `clamp`,
//! `sqrt`, …) have always taken any operand that implements `Expr`, so a boxed
//! operand works there. The comparison (`gt`, `le`, …) and boolean (`and`, `or`, …)
//! methods used to ask their operands for `Copy` as well, which a trait object never
//! is; that bound is gone, so logic takes a boxed operand exactly as arithmetic
//! does. `a_boxed_sub_expression_composes_in_boolean_positions` pins it, because the
//! bound was easy to write again by accident.

use express_it::expr::{Context, ContextMut, DepKey, Deps, Expr, StoredExpr};
use express_it::logic::{ExprCmpGt, ExprCmpLe, ExprCmpLt, ExprLogicalAnd, ExprLogicalOr};
use express_it::nodes::{Node, VarExpr};
use express_it::ops::ExprClamp;
use express_it::plan::Plan;
use express_it::save;

#[derive(Clone, Copy, Debug, Default)]
struct State {
    hp: f32,
    healed: f32,
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

/// A leaf with its own dependency key, so the test can see the boxed leaf's key
/// arrive through the box.
struct Regen;

impl Expr<f32, Ctx> for Regen {
    fn eval(&self, _ctx: &State) -> f32 {
        4.0
    }

    fn get_dependencies(&self, deps: &mut Deps) {
        deps.insert(DepKey::type_of::<Self>());
    }
}

fn hp() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|state: &State| state.hp)
}

/// `Regen` behind the exported type-erasing alias.
fn boxed_regen() -> Node<f32, Ctx, StoredExpr<f32, Ctx>> {
    Node::from_expr(Box::new(Regen))
}

/// The variable behind the same alias, so a test can put two non-`Copy`,
/// non-`Clone` operands on opposite sides of one operator.
fn boxed_hp() -> Node<f32, Ctx, StoredExpr<f32, Ctx>> {
    Node::from_expr(Box::new(hp()))
}

fn state() -> State {
    State {
        hp: 8.0,
        ..Default::default()
    }
}

#[test]
fn a_boxed_sub_expression_composes_into_a_tree() {
    // Boxed operand inside a tree built with the `+` operator and with a method.
    let healing = boxed_regen() + hp();
    assert_eq!(healing.eval(&state()), 12.0);

    // A tree holding a boxed node is not `Copy` (neither is `Box<dyn ..>`), so
    // composing it further consumes it.
    let capped = healing.clamp(0.0, 10.0);
    assert_eq!(capped.eval(&state()), 10.0);
}

#[test]
fn a_boxed_sub_expression_forwards_its_dependency_keys() {
    let deps = (boxed_regen() + hp()).dependencies();

    assert!(
        deps.contains_type_of::<Regen>(),
        "the key of the boxed leaf arrives through the box"
    );
    assert!(
        deps.contains_type_of::<VarExpr<f32, Ctx>>(),
        "and so does the key of the variable beside it"
    );
    assert_eq!(
        deps.len(),
        2,
        "one from each child, neither swallowed by the box"
    );
}

#[test]
fn a_boxed_sub_expression_is_the_expression_of_a_plan_step() {
    // A step needs `Expr + Send + Sync`, which the alias provides.
    let mut state = state();

    let plan = Plan::<Ctx>::new().step(save!(State, healed, boxed_regen() + hp()));
    plan.run(&mut state)
        .expect("an assignment step never fails to evaluate");

    assert_eq!(state.healed, 12.0);
}

#[test]
fn a_boxed_sub_expression_composes_in_boolean_positions() {
    // Every operand here is a trait object: never `Copy`, never `Clone`. Comparison
    // between two of them, then boolean structure, then a boxed boolean fed back
    // into another comparison's `and`, which is where the removed `Copy` bound used
    // to stop the program dead.
    //
    // regen is 4.0, hp is 8.0.
    let regen_wins = boxed_regen().gt(boxed_hp()); // false
    let not_falling = boxed_regen().le(boxed_regen()); // true
    let either = regen_wins.or(not_falling); // true

    let boxed_flag = Node::from_expr(Box::new(either)); // bool, erased
    let regen_positive = boxed_regen().gt(0.0); // true
    let both = boxed_flag.and(regen_positive); // true

    assert!(both.eval(&state()));

    // Dependency keys still arrive through all that boxing.
    let deps = (boxed_regen().gt(boxed_hp())).dependencies();
    assert!(
        deps.contains_type_of::<Regen>(),
        "the boxed leaf keeps its key through a comparison"
    );
    assert!(
        deps.contains_type_of::<VarExpr<f32, Ctx>>(),
        "and so does the boxed variable beside it"
    );

    // A boxed boolean in a method chain of its own: `.lt()` on boxed operands,
    // negated by the boolean layer, still evaluates.
    let falling = Node::from_expr(Box::new(boxed_regen().lt(boxed_hp())));
    assert!(falling.eval(&state()));
}
