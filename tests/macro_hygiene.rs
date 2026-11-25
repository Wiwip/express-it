//! The exported macros must expand without borrowing names from the call site.
//!
//! `all![]` and `any![]` used to expand to a bare `Node::lit(..)`, so they only
//! compiled where the caller happened to have `Node` in scope. There is no `Node`
//! import in this file: every `Node` spelling below is written out through its
//! full path, which is what makes these tests fail if the `$crate` paths regress.
//!
//! The `var_leaf!` cases go further and import nothing from the crate at all,
//! inside modules of their own — see the comment there. One of those modules also
//! exercises the visibility token the 0.3 macro pattern takes.

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

    // Written with no visibility modifier: both leaves are named only by the test
    // below, in this same module, and a bare name is how a private leaf is asked
    // for. The 0.3 pattern takes a visibility on every call, including an empty one.
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

/// The visibility written in a `var_leaf!` call is the visibility of the generated
/// struct — the macro inserts none of its own.
///
/// Three forms, one per leaf: no modifier at all (private to this module),
/// `pub(crate)` (the rest of this test crate), and `pub` (outward-facing, reached
/// by path from the tests at the bottom of this file). Nothing is imported here and
/// every path is spelled out by hand, as above.
///
/// Inside one crate `pub` and `pub(crate)` look alike, because the difference only
/// shows to a crate downstream, and a private item compiles wherever a public one
/// does. So what these tests pin is the positive half: each form compiles, the leaf
/// behaves, and the outward-facing ones are nameable by path from outside their
/// module. That a leaf written with no visibility is *not* nameable from out here
/// would need a compile-fail harness, which this crate deliberately does not carry.
mod visibility {
    #[derive(Clone, Copy)]
    pub struct State {
        pub hp: f32,
    }

    pub struct Ctx;

    impl express_it::expr::Context for Ctx {
        type ContextItem<'w, 's> = State;
    }

    express_it::var_leaf!(pub Shadow, f32, Ctx, |state| state.hp * 2.0);
    express_it::var_leaf!(pub(crate) Regen, f32, Ctx, |state| state.hp / 2.0);
    express_it::var_leaf!(LocalHp, f32, Ctx, |state| state.hp);

    #[test]
    fn a_leaf_with_no_visibility_modifier_works_inside_its_own_module() {
        // `LocalHp` is written bare, so the generated struct is private to this
        // module — which is all the leaf needs, because this module is where it
        // is used.
        let hp = express_it::nodes::Node::<f32, Ctx, _>::from_expr(LocalHp);

        assert_eq!(
            express_it::expr::Expr::eval(&hp, &State { hp: 4.0 }),
            4.0,
            "a private leaf evaluates like any other"
        );
        assert_eq!(hp.to_string(), "LocalHp", "and still prints its own name");
    }
}

#[test]
fn a_pub_leaf_is_reachable_by_path_from_its_module() {
    let shadow = express_it::nodes::Node::<f32, visibility::Ctx, _>::from_expr(visibility::Shadow);

    assert_eq!(shadow.eval(&visibility::State { hp: 4.0 }), 8.0);
    assert_eq!(shadow.to_string(), "Shadow");
}

#[test]
fn a_pub_crate_leaf_is_reachable_from_the_rest_of_the_crate() {
    let regen = express_it::nodes::Node::<f32, visibility::Ctx, _>::from_expr(visibility::Regen);

    assert_eq!(regen.eval(&visibility::State { hp: 4.0 }), 2.0);
    assert_eq!(regen.to_string(), "Regen");
}
