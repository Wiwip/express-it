//! The exported macros must expand without borrowing names from the call site.
//!
//! `all![]` and `any![]` used to expand to a bare `Node::lit(..)`, so they only
//! compiled where the caller happened to have `Node` in scope. There is no `Node`
//! import in this file: every `Node` spelling below is written out through its
//! full path, which is what makes these tests fail if the `$crate` paths regress.
//!
//! The last case goes further and imports nothing from the crate at all, inside a
//! module of its own — see the comment there.

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

/// `var_leaf!` has to expand with nothing imported from the crate: every path in
/// the generated leaf goes through `$crate`, and `Node::from_expr` is an inherent
/// method, so no `use` line is needed to build a node from a generated leaf.
///
/// A nested module is the proof — a `use` at the top of this file is not visible
/// in here, so every express-it name below is spelled out by hand.
mod leaf_without_crate_imports {
    #[derive(Clone, Copy)]
    struct State {
        hp: f32,
        attack: f32,
    }

    struct Ctx;

    impl express_it::expr::Context for Ctx {
        type ContextItem<'w, 's> = State;
    }

    express_it::var_leaf!(Health, f32, Ctx, |state| state.hp);
    express_it::var_leaf!(Attack, f32, Ctx, |state| state.attack);

    fn eval<E: express_it::expr::Expr<f32, Ctx>>(
        node: &express_it::nodes::Node<f32, Ctx, E>,
        state: &State,
    ) -> f32 {
        express_it::expr::Expr::eval(node, state)
    }

    fn keys<E: express_it::expr::Expr<f32, Ctx>>(
        node: &express_it::nodes::Node<f32, Ctx, E>,
    ) -> std::collections::HashSet<std::any::TypeId> {
        let mut deps = std::collections::HashSet::new();
        express_it::expr::Expr::get_dependencies(node, &mut deps);
        deps
    }

    #[test]
    fn var_leaf_and_from_expr_need_no_crate_imports() {
        let state = State {
            hp: 7.0,
            attack: 3.0,
        };

        let health = express_it::nodes::Node::<f32, Ctx, _>::from_expr(Health);
        let attack = express_it::nodes::Node::<f32, Ctx, _>::from_expr(Attack);

        assert_eq!(eval(&health, &state), 7.0, "the body reads the context");
        assert_eq!(eval(&attack, &state), 3.0);
        assert_eq!(
            health.to_string(),
            "Health",
            "Display prints the leaf's own name, not `var`"
        );
        assert_ne!(keys(&health), keys(&attack), "one key per leaf");
    }
}
