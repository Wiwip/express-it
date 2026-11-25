//! `StoredExpr` has to be usable from outside the crate: it is exported, so a
//! boxed expression must be composable into a tree like any other sub-expression.
//!
//! Before the `Expr` impl that forwards through `Box`, this file did not compile —
//! the alias implemented nothing, so a `StoredExpr` could be neither a node payload
//! nor an operand.
//!
//! Scope note: arithmetic (`+ - * / %`) and the method nodes (`max`, `min`, `clamp`,
//! `sqrt`, …) only require their operands to implement `Expr`, so a boxed operand
//! works there. The comparison (`gt`, `le`, …) and boolean (`and`, `or`, …) methods
//! additionally require `Copy` operands, which a trait object never is; feeding a
//! `StoredExpr` straight into `.ge(..)` is still a compile error, and that is a
//! pre-existing bound of those two modules rather than something this alias can fix.

use express_it::expr::{Context, ContextMut, Expr, StoredExpr};
use express_it::nodes::{Node, VarExpr};
use express_it::ops::ExprClamp;
use express_it::plan::Plan;
use express_it::save;
use std::any::TypeId;
use std::collections::HashSet;
use std::marker::PhantomData;

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

    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        deps.insert(TypeId::of::<Self>());
    }
}

fn hp() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|state: &State| state.hp)
}

/// `Regen` behind the exported type-erasing alias.
fn boxed_regen() -> Node<f32, Ctx, StoredExpr<f32, Ctx>> {
    Node {
        expr: Box::new(Regen),
        _marker: PhantomData,
    }
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
    let mut deps = HashSet::new();
    (boxed_regen() + hp()).get_dependencies(&mut deps);

    assert!(
        deps.contains(&TypeId::of::<Regen>()),
        "the key of the boxed leaf arrives through the box"
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
