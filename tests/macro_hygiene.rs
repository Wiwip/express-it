//! The exported macros must expand without borrowing names from the call site.
//!
//! `all![]` and `any![]` used to expand to a bare `Node::lit(..)`, so they only
//! compiled where the caller happened to have `Node` in scope. There is no `Node`
//! import in this file: every `Node` spelling below is written out through its
//! full path, which is what makes these tests fail if the `$crate` paths regress.

use express_it::expr::{Context, Expr};
use express_it::logic::{ExprCmpGt, ExprLogicalAnd, ExprLogicalOr};
use express_it::{all, any};

#[derive(Clone, Copy)]
struct State {
    hp: f32,
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

#[test]
fn all_macro_expands_without_node_in_scope() {
    // The empty conjunction is the identity of `and`, i.e. true. The context type
    // is the one thing the nullary form cannot infer, hence the annotation.
    let conjunct: express_it::nodes::Node<bool, Ctx, _> = all![];
    assert!(conjunct.eval(&State { hp: 1.0 }));
}

#[test]
fn any_macro_expands_without_node_in_scope() {
    // The empty disjunction is the identity of `or`, i.e. false.
    let disjunct: express_it::nodes::Node<bool, Ctx, _> = any![];
    assert!(!disjunct.eval(&State { hp: 1.0 }));
}

#[test]
fn nullary_forms_compose_when_the_context_comes_from_another_operand() {
    // No annotation anywhere: `C` is fixed by the sibling operand, so the nullary
    // arm is usable unannotated inside a real expression.
    let hp = express_it::nodes::Node::<f32, Ctx, _>::new(|state: &State| state.hp);

    let conjunct = hp.gt(0.0).and(all![]);
    assert!(conjunct.eval(&State { hp: 1.0 }));

    let disjunct = hp.gt(100.0).or(any![]);
    assert!(!disjunct.eval(&State { hp: 1.0 }));
}
