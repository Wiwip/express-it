//! Flat folds: `Node::sum` and `Node::product` take a `Vec` of children and fold
//! them in a loop, so accumulating a list of records does not build a chain.
//!
//! The comparison throughout is against the erased chain, which is what a loop
//! forces you to build otherwise. Measured on this toolchain (rustc 1.99.1, release
//! profile, x86-64 Linux), that chain overflows a 256 KiB stack somewhere between
//! 10,000 and 20,000 levels, and the 8 MiB main thread between 60,000 and 100,000,
//! aborting rather than returning an error.
//! `a_hundred_thousand_children_evaluate_on_a_small_stack` builds the same shape of
//! tree at the depth that kills the chain, on that small stack.

use express_it::expr::{Deps, Expr, StoredExpr};
use express_it::fold::{FoldNode, Sum};
use express_it::nodes::Node;
use express_it::plan::Plan;
use express_it::prelude::*;
use express_it::{load_cache, save, save_cache, var_leaf};

#[derive(Clone, Copy, Debug, Default)]
struct State {
    hp: f32,
    bonus: f32,
    total: f32,
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

var_leaf!(Hp, f32, Ctx, |state| state.hp);
var_leaf!(Bonus, f32, Ctx, |state| state.bonus);

fn state() -> State {
    State {
        hp: 2.0,
        bonus: 3.0,
        ..Default::default()
    }
}

/// Two leaves with two different keys, rebuilt on each call because a fold takes
/// its children by value.
fn hp() -> Node<f32, Ctx, Hp> {
    Node::from_expr(Hp)
}

fn bonus() -> Node<f32, Ctx, Bonus> {
    Node::from_expr(Bonus)
}

/// Mixed leaf types share one type only once erased, and a `Vec` holds one type, so
/// this is the route for a fold over different attributes.
fn erased(children: Vec<StoredExpr<f32, Ctx>>) -> Vec<StoredExpr<f32, Ctx>> {
    children
}

fn box_it<N: Send + Sync + 'static, E: Expr<N, Ctx> + Send + Sync + 'static>(
    node: Node<N, Ctx, E>,
) -> StoredExpr<N, Ctx> {
    Box::new(node)
}

#[test]
fn a_sum_of_a_vector_evaluates_exactly_like_the_nested_chain() {
    let ctx = state();

    let flat = Node::sum(erased(vec![box_it(hp()), box_it(bonus()), box_it(hp())]));
    let nested = (hp() + bonus()) + hp();

    // Bits, not an approximate comparison: the fold starts at its first child, so
    // the association order is exactly the left-nested one.
    assert_eq!(flat.eval(&ctx).to_bits(), nested.eval(&ctx).to_bits());
    assert_eq!(flat.eval(&ctx), 7.0);
}

#[test]
fn a_product_of_a_vector_evaluates_exactly_like_the_nested_chain() {
    let ctx = state();

    let flat = Node::product(erased(vec![box_it(hp()), box_it(bonus()), box_it(hp())]));
    let nested = (hp() * bonus()) * hp();

    assert_eq!(flat.eval(&ctx).to_bits(), nested.eval(&ctx).to_bits());
    assert_eq!(flat.eval(&ctx), 12.0);
}

/// The accumulator is seeded with the first child rather than with `N::zero()`, and
/// this is where that shows: `0.0 + (-0.0)` is `+0.0`, so a zero-seeded fold turns a
/// lone negative zero into positive zero where `(a)` and even `a` alone keep it.
#[test]
fn a_fold_preserves_negative_zero_where_a_zero_seeded_fold_would_not() {
    let flat = Node::sum(vec![Node::<f32, Ctx, _>::lit(-0.0f32)]);
    let bare = Node::<f32, Ctx, _>::lit(-0.0f32);

    assert_eq!(
        flat.eval(&state()).to_bits(),
        bare.eval(&state()).to_bits(),
        "the fold's value is the child's value, sign bit included"
    );
    assert!(flat.eval(&state()).is_sign_negative());
    // The fold still prints as a fold: one child, parenthesised.
    assert_eq!(flat.to_string(), "(-0)");
    assert_eq!(bare.to_string(), "-0");

    let neg = || Node::<f32, Ctx, _>::lit(-0.0f32);
    let two = Node::sum(vec![neg(), neg()]);
    let nested = neg() + neg();
    assert_eq!(
        two.eval(&state()).to_bits(),
        nested.eval(&state()).to_bits()
    );
}

#[test]
fn an_empty_fold_is_the_identity_and_prints_as_one() {
    let empty_sum = Node::sum(Vec::<Node<f32, Ctx, Hp>>::new());
    let empty_product = Node::product(Vec::<Node<f32, Ctx, Hp>>::new());

    assert_eq!(empty_sum.eval(&state()), 0.0);
    assert_eq!(empty_product.eval(&state()), 1.0);
    assert_eq!(empty_sum.to_string(), "0");
    assert_eq!(empty_product.to_string(), "1");
    assert!(empty_sum.dependencies().is_empty());
}

#[test]
fn a_fold_prints_every_child_and_no_nesting_that_is_not_there() {
    let flat = Node::sum(erased(vec![box_it(hp()), box_it(bonus()), box_it(hp())]));
    assert_eq!(flat.to_string(), "(Hp + Bonus + Hp)");

    // One level of fold, so a child that is itself a fold keeps its own brackets.
    let inner = Node::product(erased(vec![box_it(hp()), box_it(Node::lit(2.0f32))]));
    let outer = Node::sum(erased(vec![box_it(inner), box_it(bonus())]));
    assert_eq!(outer.to_string(), "((Hp * 2) + Bonus)");
}

#[test]
fn a_fold_reports_every_child_key_once() {
    let flat = Node::sum(erased(vec![box_it(hp()), box_it(bonus()), box_it(hp())]));

    let deps = flat.dependencies();
    assert_eq!(deps.len(), 2, "Hp twice is still one key");
    assert!(deps.contains_type_of::<Hp>());
    assert!(deps.contains_type_of::<Bonus>());
    assert!(!deps.contains_name("hp"));
}

/// Erased children of mixed types, which is the case the chain was built for.
#[test]
fn a_fold_of_erased_children_evaluates_prints_and_composes() {
    // A fold's type is worth naming once: the payload says which operator, which
    // value type, which context and which child type, and users of a fold that
    // goes into a struct field will write this alias too.
    type Mixed = Node<f32, Ctx, FoldNode<f32, Ctx, Sum, StoredExpr<f32, Ctx>>>;

    // Built twice, because a fold is not `Copy` and an erased one is not `Clone`
    // either: composing a tree moves it, and an erased tree moves once.
    fn mixed() -> Mixed {
        Node::sum(erased(vec![
            box_it(hp()),
            box_it(bonus() * 2.0f32),
            box_it(Node::lit(1.0f32)),
        ]))
    }

    let flat = mixed();
    assert_eq!(flat.eval(&state()), 9.0);
    assert_eq!(flat.to_string(), "(Hp + (Bonus * 2) + 1)");

    let guarded = mixed().gt(5.0);
    assert!(guarded.eval(&state()));
    assert_eq!(
        guarded.to_string(),
        "((Hp + (Bonus * 2) + 1) > 5)",
        "an erased child prints inside the fold, and the fold inside the comparison"
    );

    assert_eq!(mixed().dependencies().len(), 2);
}

/// The point of the module: 100,000 terms, on a stack small enough that the erased
/// chain of the same size aborts the process.
#[test]
fn a_hundred_thousand_children_evaluate_on_a_small_stack() {
    const N: usize = 100_000;

    let handle = std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            let terms: Vec<StoredExpr<f32, Ctx>> = (0..N)
                .map(|_| Box::new(hp()) as StoredExpr<f32, Ctx>)
                .collect();
            let total = Node::sum(terms);
            (total.eval(&state()), total.dependencies().len())
        })
        .expect("spawn a thread with a 256 KiB stack");

    let (total, keys) = handle
        .join()
        .expect("the fold neither panics nor overflows");
    assert_eq!(total, 2.0 * N as f32, "N reads of hp, and hp is 2.0");
    assert_eq!(keys, 1, "the same leaf N times is still one key");
}

/// A chain does survive a thousand levels; the fold agrees with it there, which is
/// the equivalence this file relies on at depths where the chain cannot go.
#[test]
fn a_fold_agrees_with_a_thousand_deep_chain() {
    // The element type is fixed at `StoredExpr` from the first step, which is what
    // lets the loop grow the tree at all: a typed accumulator would need a new type
    // per iteration.
    let mut chain: Node<f32, Ctx, StoredExpr<f32, Ctx>> =
        Node::from_expr(Box::new(hp()) as StoredExpr<f32, Ctx>);
    for _ in 1..1_000 {
        chain = Node::from_expr(Box::new(chain + hp()) as StoredExpr<f32, Ctx>);
    }
    let terms: Vec<StoredExpr<f32, Ctx>> = (0..1_000)
        .map(|_| Box::new(hp()) as StoredExpr<f32, Ctx>)
        .collect();
    let flat = Node::sum(terms);

    assert_eq!(flat.eval(&state()), 2_000.0);
    assert_eq!(
        chain.eval(&state()),
        2_000.0,
        "the chain and the fold agree"
    );
}

#[test]
fn a_fold_runs_in_a_plan_and_writes_through_save() {
    let plan: Plan<Ctx> = Plan::new().step(save!(
        State,
        total,
        Node::sum(erased(vec![box_it(hp()), box_it(bonus())]))
    ));

    let mut state = state();
    plan.run(&mut state).expect("plan runs");
    assert_eq!(state.total, 5.0);
}

/// A fold is `Clone` when its children are, which is what lets a cache step hold
/// the recipe. It is never `Copy`: the vector is in the way, and that is the price
/// of a node whose children live on the heap.
#[test]
fn a_homogeneous_fold_clones_and_goes_into_a_cache_step() {
    let flat = Node::sum(vec![hp(), hp(), hp()]);

    let again = flat.clone();
    assert_eq!(again.eval(&state()), 6.0);
    assert_eq!(again.to_string(), "(Hp + Hp + Hp)");
    assert_eq!(
        again.to_string(),
        flat.to_string(),
        "the clone is identical"
    );

    // `save_cache!` keeps a cloneable recipe, so a fold of one node type qualifies
    // and an erased fold does not.
    let mut state = state();
    Plan::<Ctx>::new()
        .step(save_cache!("hps", Node::sum(vec![hp(), hp(), hp()])))
        .step(load_cache!(f32, "hps"))
        .run(&mut state)
        .expect("the fold's value round-trips through the cache");

    // And the miss is loud: the same lookup with nothing written first is an error,
    // so the run above really did carry the fold's value.
    let missed = Plan::<Ctx>::new()
        .step(load_cache!(f32, "hps"))
        .run(&mut state)
        .expect_err("a key nobody wrote is a PlanError");
    assert!(
        format!("{missed}").contains("hps"),
        "the error names the key: {missed}"
    );
}

/// Twenty children, two keys: the deduplication happens in the `Deps` sink, not in
/// the vector, so a fold over repeating leaves costs nothing extra to report.
#[test]
fn a_fold_deduplicates_through_the_sink_not_the_vector() {
    let terms: Vec<StoredExpr<f32, Ctx>> = (0..20)
        .map(|_| Box::new(hp() + bonus()) as StoredExpr<f32, Ctx>)
        .collect();
    let flat = Node::sum(terms);

    let deps: Deps = flat.dependencies();
    assert_eq!(deps.len(), 2);
    assert_eq!(deps.types().count(), 2);

    let one_pair = Node::sum(erased(vec![box_it(hp()), box_it(bonus())])).dependencies();
    assert!(deps.is_subset_of(&one_pair));
    assert!(one_pair.is_subset_of(&deps));
}
