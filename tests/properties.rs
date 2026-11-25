//! Randomised property tests for the 0.3 expression tree and the plan runner.
//!
//! Where the unit tests in `src/` pin one example each, these assert the rule the
//! example was an instance of: dependency keys propagate through every
//! combinator, the two keying schemes stay apart, evaluation is a pure function
//! of tree and context, the arithmetic and boolean laws the docs lean on hold for
//! any input, `cast` really is `as`, and a plan stops exactly where it failed.
//!
//! Configuration: 300 cases per property, a little above the 256 default, which
//! runs the fifteen property tests in about a tenth of a second. The case count is
//! the knob for a deeper sweep and it is what sets the wall time, not the number of
//! properties: `PROPTEST_CASES=10000` takes 2.2 s and `PROPTEST_CASES=25000` takes
//! 5.4 s, both green.
//!
//! Reading a failure: each property states its precondition in its doc comment
//! and, where the library would panic there, rejects the input rather than
//! dodging the case. Nothing here is `#[ignore]`d, and nothing is filtered except
//! where float semantics or a documented panic genuinely requires it.

use express_it::expr::{Context, ContextMut, DepKey, Deps, Expr, StoredExpr};
use express_it::logic::{
    ExprCmpEq, ExprCmpGe, ExprCmpGt, ExprCmpLe, ExprCmpLt, ExprCmpNotEq, ExprLogicNot,
    ExprLogicalAnd, ExprLogicalNand, ExprLogicalNor, ExprLogicalNxor, ExprLogicalOr,
    ExprLogicalXor, LessThanNode, LogicalAndNode, LogicalNotNode,
};
use express_it::nodes::{Node, Var, VarExpr};
use express_it::ops::{ExprAbs, ExprAcos, ExprClamp, ExprMax, ExprMin, ExprPow, ExprSqrt};
use express_it::plan::{AssignmentStep, CacheLookupStep, Plan, PlanError};
use express_it::{all, any, var_leaf};
use proptest::prelude::*;

// ---------------------------------------------------------------------------
// Fixtures: a context to read, leaves with their own keys, and a tree builder
// ---------------------------------------------------------------------------

/// The read-only context: three `f64` attributes and eight booleans.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
struct State {
    hp: f64,
    defense: f64,
    haste: f64,
    flags: [bool; 8],
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

fn read_hp(state: &State) -> f64 {
    state.hp
}

fn read_defense(state: &State) -> f64 {
    state.defense
}

fn read_haste(state: &State) -> f64 {
    state.haste
}

/// Names a [`Node::var`] variable may carry.
///
/// `hp`, `HP` and `"hp "` are three spellings of one attribute on purpose: keys
/// compare by exact string equality, so they must stay three separate keys.
const NAMES: [&str; 5] = ["hp", "HP", "hp ", "defense", "haste"];

/// Names no tree in this file ever reports, for the negative `contains_name` checks.
const ABSENT_NAMES: [&str; 3] = ["attack", "Hp", "defenses"];

fn flag<const I: usize>(state: &State) -> bool {
    state.flags[I]
}

/// The name of each of the eight boolean variables: one name, one key each.
const FLAGS: [&str; 8] = [
    "flag0", "flag1", "flag2", "flag3", "flag4", "flag5", "flag6", "flag7",
];

// Three type-keyed leaves — what `var_leaf!` generates, the `Type` half of the
// dependency-key story. Private to this module, hence no visibility token.
var_leaf!(ProbeA, f64, Ctx, |state| state.hp + 1.0);
var_leaf!(ProbeB, f64, Ctx, |state| state.defense * 2.0);
var_leaf!(ProbeFlag, bool, Ctx, |state| state.flags[0]);

/// A tree whose shape is chosen at run time: every node is boxed on the way up,
/// so one Rust type holds every shape the generator produces. Boxing goes through
/// the `Expr` impl that forwards through `Box`, so neither keys nor values change.
type FTree = Node<f64, Ctx, StoredExpr<f64, Ctx>>;
type BTree = Node<bool, Ctx, StoredExpr<bool, Ctx>>;

fn erase<N, E: Expr<N, Ctx> + Send + Sync + 'static>(
    node: Node<N, Ctx, E>,
) -> Node<N, Ctx, StoredExpr<N, Ctx>> {
    let boxed: StoredExpr<N, Ctx> = Box::new(node.expr);
    Node::from_expr(boxed)
}

fn band(lhs: BTree, rhs: BTree) -> BTree {
    erase(Node::from_expr(LogicalAndNode {
        lhs: lhs.expr,
        rhs: rhs.expr,
    }))
}

fn bnot(inner: BTree) -> BTree {
    erase(Node::from_expr(LogicalNotNode { inner: inner.expr }))
}

fn blt(lhs: FTree, rhs: FTree) -> BTree {
    erase(Node::from_expr(LessThanNode::<f64, _, _>::new(
        lhs.expr, rhs.expr,
    )))
}

fn probe_flag() -> BTree {
    erase(Node::<bool, Ctx, _>::from_expr(ProbeFlag))
}

/// One generated leaf: a byte choosing the kind, and the value a literal carries.
type Leaf = (u8, f64);

/// A non-empty run of leaves — one per node of the tree's frontier.
fn leaves() -> impl Strategy<Value = Vec<Leaf>> {
    prop::collection::vec((any::<u8>(), any::<f64>()), 1..16)
}

fn states() -> impl Strategy<Value = State> {
    (
        any::<f64>(),
        any::<f64>(),
        any::<f64>(),
        prop::array::uniform8(any::<bool>()),
    )
        .prop_map(|(hp, defense, haste, flags)| State {
            hp,
            defense,
            haste,
            flags,
        })
}

/// A finite `f64`.
///
/// A guard rather than a restriction: `any::<f64>()` in proptest 1.11 draws only
/// zero, subnormal and normal values (`f64::Any(POSITIVE | NEGATIVE | ZERO |
/// SUBNORMAL | NORMAL)`), so it never yields NaN or an infinity and this filter
/// rejects nothing today. It is written because the properties genuinely do not
/// hold for those values — `x - x` is NaN when `x` is, `clamp` panics on a NaN
/// bound, and `a < b == !(a >= b)` is simply false for NaN — so if the upstream
/// strategy ever widens, these properties stay honest instead of turning red for
/// the wrong reason. Coverage of NaN and infinity flowing through a tree lives in
/// the explicit specials below and in the unit tests in `src/ops.rs`.
fn finite_f64() -> impl Strategy<Value = f64> {
    any::<f64>().prop_filter("finite", |value| value.is_finite())
}

/// A pair with `lo <= hi`, by sorting rather than rejecting. Every ordered pair is
/// still reachable — a pair generated the other way round sorts into it — while
/// the documented `min > max` panic stays out of the inputs.
fn ordered_bounds() -> impl Strategy<Value = (f64, f64)> {
    (finite_f64(), finite_f64()).prop_map(|(first, second)| {
        if first <= second {
            (first, second)
        } else {
            (second, first)
        }
    })
}

/// How a leaf reports its key, if it reports one at all.
enum LeafKind {
    Literal,
    /// `Node::var`: a `DepKey::Name`.
    Named(usize),
    /// `Node::new`: the `VarExpr<f64, Ctx>` type key, shared by every such variable.
    NewVar,
    /// A `var_leaf!` leaf: a key of its own.
    ProbeA,
    ProbeB,
}

fn leaf_kind(byte: u8) -> LeafKind {
    match byte % 5 {
        0 => LeafKind::Literal,
        1 => LeafKind::Named((byte as usize / 5) % NAMES.len()),
        2 => LeafKind::NewVar,
        3 => LeafKind::ProbeA,
        _ => LeafKind::ProbeB,
    }
}

fn leaf_f64(kind: &LeafKind, value: f64) -> FTree {
    match kind {
        LeafKind::Literal => erase(Node::<f64, Ctx, _>::lit(value)),
        LeafKind::Named(index) => erase(named_f64(*index)),
        LeafKind::NewVar => erase(typed_f64(0)),
        LeafKind::ProbeA => erase(Node::<f64, Ctx, _>::from_expr(ProbeA)),
        LeafKind::ProbeB => erase(Node::<f64, Ctx, _>::from_expr(ProbeB)),
    }
}

/// The binary combinators the tree builder folds with.
#[derive(Clone, Copy, Debug)]
enum Bin {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Max,
    Min,
    Powf,
}

impl Bin {
    const ALL: [Bin; 8] = [
        Bin::Add,
        Bin::Sub,
        Bin::Mul,
        Bin::Div,
        Bin::Rem,
        Bin::Max,
        Bin::Min,
        Bin::Powf,
    ];

    fn name(self) -> &'static str {
        match self {
            Bin::Add => "+",
            Bin::Sub => "-",
            Bin::Mul => "*",
            Bin::Div => "/",
            Bin::Rem => "%",
            Bin::Max => "max(..)",
            Bin::Min => "min(..)",
            Bin::Powf => "powf(..)",
        }
    }
}

fn combine_f64(lhs: FTree, rhs: FTree, bin: Bin) -> FTree {
    match bin {
        Bin::Add => erase(lhs + rhs),
        Bin::Sub => erase(lhs - rhs),
        Bin::Mul => erase(lhs * rhs),
        Bin::Div => erase(lhs / rhs),
        Bin::Rem => erase(lhs % rhs),
        Bin::Max => erase(lhs.max(rhs)),
        Bin::Min => erase(lhs.min(rhs)),
        Bin::Powf => erase(lhs.powf(rhs)),
    }
}

/// The one-operand combinators, including the cast that changes the value type.
#[derive(Clone, Copy, Debug)]
enum Unary {
    Sqrt,
    Abs,
    Neg,
    Acos,
    CastI64,
}

impl Unary {
    const ALL: [Unary; 5] = [
        Unary::Sqrt,
        Unary::Abs,
        Unary::Neg,
        Unary::Acos,
        Unary::CastI64,
    ];

    fn name(self) -> &'static str {
        match self {
            Unary::Sqrt => "sqrt()",
            Unary::Abs => "abs()",
            Unary::Neg => "-(..)",
            Unary::Acos => "acos()",
            Unary::CastI64 => "cast::<i64>()",
        }
    }
}

fn deps_of_unary(node: FTree, unary: Unary) -> Deps {
    match unary {
        Unary::Sqrt => node.sqrt().dependencies(),
        Unary::Abs => node.abs().dependencies(),
        Unary::Neg => (-node).dependencies(),
        Unary::Acos => node.acos().dependencies(),
        Unary::CastI64 => node.cast::<i64>().dependencies(),
    }
}

/// Builds a randomly-shaped `f64` tree: one leaf per generated byte, then the
/// last two nodes on a stack folded together until one is left, with the
/// combinator read off the same bytes. The shapes are branchy, not a chain.
///
/// With `literal_only` every leaf is a `Node::lit`; otherwise the first leaf is
/// forced to a named variable, so the tree always reports at least one key and a
/// superset check against it cannot pass by accident.
fn build_f64(generated: &[Leaf], literal_only: bool) -> FTree {
    let mut stack: Vec<FTree> = generated
        .iter()
        .enumerate()
        .map(|(index, (byte, value))| {
            if literal_only {
                leaf_f64(&LeafKind::Literal, *value)
            } else if index == 0 {
                leaf_f64(&LeafKind::Named(0), *value)
            } else {
                leaf_f64(&leaf_kind(*byte), *value)
            }
        })
        .collect();

    let mut round = 0usize;
    while stack.len() > 1 {
        let rhs = stack.pop().expect("a second node on the stack");
        let lhs = stack.pop().expect("a second node on the stack");
        let byte = usize::from(generated[round % generated.len()].0);
        stack.push(combine_f64(lhs, rhs, Bin::ALL[byte % Bin::ALL.len()]));
        round += 1;
    }
    stack.pop().expect("the loop leaves exactly one node")
}

// The fetches below are named in `match` arms instead of being pulled out of a
// table: a node's fetch parameter is a `fn` pointer whose signature names the
// context's associated type, and while a path to a function item coerces to it, a
// pointer read out of an array does not.

/// A name-keyed `f64` variable: entry `index` of [`NAMES`], reading the attribute
/// that name spells.
fn named_f64(index: usize) -> Var<f64, Ctx> {
    let name = NAMES[index];
    match index {
        3 => Node::<f64, Ctx, _>::var(name, read_defense),
        4 => Node::<f64, Ctx, _>::var(name, read_haste),
        // "hp", "HP" and "hp " read the same attribute under three separate keys.
        _ => Node::<f64, Ctx, _>::var(name, read_hp),
    }
}

/// The same attribute read through `Node::new`, which carries no name and so
/// reports the shared `VarExpr<f64, Ctx>` type key instead.
fn typed_f64(index: usize) -> Var<f64, Ctx> {
    match index {
        3 => Node::<f64, Ctx, _>::new(read_defense),
        4 => Node::<f64, Ctx, _>::new(read_haste),
        _ => Node::<f64, Ctx, _>::new(read_hp),
    }
}

/// The value entry `index` of [`NAMES`] reads, mirroring the two dispatches above.
fn read_indexed(index: usize, state: &State) -> f64 {
    match index {
        3 => state.defense,
        4 => state.haste,
        _ => state.hp,
    }
}

/// A `f64` variable: named (`Node::var`) or type-keyed (`Node::new`). Both
/// constructors hand back the same node type, which is exactly why telling the two
/// keying schemes apart needs a property of its own.
fn f64_var(named: bool, index: usize) -> Var<f64, Ctx> {
    if named {
        named_f64(index)
    } else {
        typed_f64(index)
    }
}

/// A name-keyed boolean: entry `index` of [`FLAGS`], reading `flags[index]`.
fn named_bool(index: usize) -> Var<bool, Ctx> {
    let name = FLAGS[index];
    match index {
        0 => Node::<bool, Ctx, _>::var(name, flag::<0>),
        1 => Node::<bool, Ctx, _>::var(name, flag::<1>),
        2 => Node::<bool, Ctx, _>::var(name, flag::<2>),
        3 => Node::<bool, Ctx, _>::var(name, flag::<3>),
        4 => Node::<bool, Ctx, _>::var(name, flag::<4>),
        5 => Node::<bool, Ctx, _>::var(name, flag::<5>),
        6 => Node::<bool, Ctx, _>::var(name, flag::<6>),
        _ => Node::<bool, Ctx, _>::var(name, flag::<7>),
    }
}

/// A boolean read through `Node::new`. Its key is the leaf's type, so it is one
/// key for every attribute — which is the point of the contrast.
fn typed_bool() -> Var<bool, Ctx> {
    Node::<bool, Ctx, _>::new(flag::<0>)
}

fn bool_var(named: bool, index: usize) -> Var<bool, Ctx> {
    if named {
        named_bool(index)
    } else {
        typed_bool()
    }
}

// ---------------------------------------------------------------------------
// Assertions shared by the dependency properties
// ---------------------------------------------------------------------------

/// Asserts `composite` holds every key of `lhs` and of `rhs`, and nothing else.
///
/// The superset half is the property asked for; the equality half stops a
/// forwarding bug being papered over, since a composite that silently *gained* a
/// key would still be a superset. `what` names the operand pair and `op` the
/// combinator, so a failure points at one combinator.
///
/// Messages pass their arguments positionally throughout this file: a format
/// string reaching `format_args!` through `prop_assert!` cannot capture names
/// from the caller's scope.
fn check_superset(
    composite: Deps,
    lhs: Deps,
    rhs: Deps,
    what: &'static str,
    op: &'static str,
) -> Result<(), TestCaseError> {
    prop_assert!(
        composite.is_superset_of(&lhs),
        "{} {}: the composite reports {:?}, missing a key its left operand reports ({:?})",
        what,
        op,
        composite,
        lhs,
    );
    prop_assert!(
        composite.is_superset_of(&rhs),
        "{} {}: the composite reports {:?}, missing a key its right operand reports ({:?})",
        what,
        op,
        composite,
        rhs,
    );
    let mut union = lhs.clone();
    union.union_with(&rhs);
    prop_assert_eq!(
        &composite,
        &union,
        "{} {}: the composite keys {:?} are not the union of the operand keys {:?}",
        what,
        op,
        composite,
        union,
    );
    Ok(())
}

/// The one-operand form: a unary node adds no key and drops none.
fn check_superset1(
    composite: Deps,
    operand: Deps,
    what: &'static str,
    op: &'static str,
) -> Result<(), TestCaseError> {
    prop_assert!(
        composite.is_superset_of(&operand),
        "{} {}: the composite reports {:?}, missing a key its operand reports ({:?})",
        what,
        op,
        composite,
        operand,
    );
    prop_assert_eq!(
        &composite,
        &operand,
        "{} {}: a one-operand node neither adds nor drops a key, but reports {:?} against {:?}",
        what,
        op,
        composite,
        operand,
    );
    Ok(())
}

/// Runs every binary arithmetic combinator over one pair of `Copy` operands.
macro_rules! assert_every_arithmetic_forwards {
    ($lhs:expr, $rhs:expr, $what:literal) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let left = lhs.dependencies();
        let right = rhs.dependencies();
        check_superset(
            (lhs + rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            "+",
        )?;
        check_superset(
            (lhs - rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            "-",
        )?;
        check_superset(
            (lhs * rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            "*",
        )?;
        check_superset(
            (lhs / rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            "/",
        )?;
        check_superset(
            (lhs % rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            "%",
        )?;
        check_superset(
            lhs.max(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".max(..)",
        )?;
        check_superset(
            lhs.min(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".min(..)",
        )?;
        check_superset(
            lhs.powf(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".powf(..)",
        )?;
        // clamp(expr, expr, expr) reads all three operands; the pair's union is the
        // answer here because both bound slots hold the same node.
        check_superset(
            lhs.clamp(rhs, rhs).dependencies(),
            left,
            right,
            $what,
            ".clamp(expr, expr, expr) with both bounds taken from the right operand",
        )?;
    }};
}

/// Runs every unary combinator over one `Copy` operand.
macro_rules! assert_every_unary_forwards {
    ($operand:expr, $what:literal) => {{
        let node = $operand;
        let keys = node.dependencies();
        check_superset1(node.sqrt().dependencies(), keys.clone(), $what, ".sqrt()")?;
        check_superset1(node.abs().dependencies(), keys.clone(), $what, ".abs()")?;
        check_superset1(node.acos().dependencies(), keys.clone(), $what, ".acos()")?;
        check_superset1((-node).dependencies(), keys.clone(), $what, "-(..)")?;
        check_superset1(
            node.cast::<i64>().dependencies(),
            keys.clone(),
            $what,
            ".cast::<i64>()",
        )?;
        check_superset1(
            node.clamp(0.0, 1.0).dependencies(),
            keys,
            $what,
            ".clamp(literal, literal) keeps only the value's keys",
        )?;
    }};
}

/// Runs every comparison over one pair of operands, then two of them against a
/// bare literal — the `AsExpression` path only the right operand takes.
macro_rules! assert_every_comparison_forwards {
    ($lhs:expr, $rhs:expr, $what:literal) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let left = lhs.dependencies();
        let right = rhs.dependencies();
        check_superset(
            lhs.lt(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".lt(..)",
        )?;
        check_superset(
            lhs.gt(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".gt(..)",
        )?;
        check_superset(
            lhs.le(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".le(..)",
        )?;
        check_superset(
            lhs.ge(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".ge(..)",
        )?;
        check_superset(
            lhs.eq(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".eq(..)",
        )?;
        check_superset(
            lhs.ne(rhs).dependencies(),
            left.clone(),
            right,
            $what,
            ".ne(..)",
        )?;
        let none = Deps::new();
        check_superset(
            lhs.lt(3.5).dependencies(),
            left.clone(),
            none.clone(),
            $what,
            ".lt(literal)",
        )?;
        check_superset(
            lhs.ge(3.5).dependencies(),
            left.clone(),
            none,
            $what,
            ".ge(literal)",
        )?;
        // `.not()` applied to a comparison: the key set of the comparison itself is
        // what the negation has to forward, both operands included.
        let compared = lhs.lt(rhs).dependencies();
        check_superset1(
            lhs.lt(rhs).not().dependencies(),
            compared,
            $what,
            ".not() over a comparison",
        )?;
    }};
}

/// Runs every logical combinator over one pair of boolean operands.
macro_rules! assert_every_logical_forwards {
    ($lhs:expr, $rhs:expr, $what:literal) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let left = lhs.dependencies();
        let right = rhs.dependencies();
        check_superset(
            lhs.and(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".and(..)",
        )?;
        check_superset(
            lhs.or(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".or(..)",
        )?;
        check_superset(
            lhs.xor(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".xor(..)",
        )?;
        check_superset(
            lhs.nand(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".nand(..)",
        )?;
        check_superset(
            lhs.nor(rhs).dependencies(),
            left.clone(),
            right.clone(),
            $what,
            ".nor(..)",
        )?;
        check_superset(
            lhs.nxor(rhs).dependencies(),
            left.clone(),
            right,
            $what,
            ".nxor(..)",
        )?;
        check_superset1(lhs.not().dependencies(), left.clone(), $what, ".not()")?;
        check_superset1((!lhs).dependencies(), left, $what, "!(..)")?;
    }};
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(300))]

    /// Property 1a — a tree built only from `Node::lit` depends on nothing.
    ///
    /// Catches a built-in leaf or a combinator reporting a key it should not: a
    /// dependency layer told that a constant reads an attribute would rebuild and
    /// re-evaluate trees that can never change, and `Deps::is_empty` — the check
    /// for "this tree is a constant" — would lie.
    #[test]
    fn literal_only_arithmetic_trees_depend_on_nothing(generated in leaves()) {
        let tree = build_f64(&generated, true);
        let deps = tree.dependencies();

        prop_assert!(deps.is_empty(), "a tree of literals reported {:?}", deps);
        prop_assert_eq!(deps.len(), 0);
        prop_assert_eq!(&deps, &Deps::new(), "the empty set, compared as a set");

        // The combinators the fold does not reach: unary nodes, a cast, and both
        // clamp shapes.
        prop_assert!(build_f64(&generated, true).sqrt().dependencies().is_empty());
        prop_assert!(build_f64(&generated, true).abs().dependencies().is_empty());
        prop_assert!((-build_f64(&generated, true)).dependencies().is_empty());
        prop_assert!(build_f64(&generated, true).cast::<i32>().dependencies().is_empty());
        prop_assert!(build_f64(&generated, true).clamp(0.0, 1.0).dependencies().is_empty());
        let clamped = build_f64(&generated, true)
            .clamp(build_f64(&generated, true), build_f64(&generated, true));
        prop_assert!(clamped.dependencies().is_empty());

        // Control, so the assertions above cannot be vacuous: the very same shape,
        // with keyed leaves instead of literals, does report keys.
        let keyed = build_f64(&generated, false);
        prop_assert!(!keyed.dependencies().is_empty(), "the control tree reported nothing, so \
            the literal assertions above could not have failed");
    }

    /// Property 1b — the same, for the comparison, logical, macro and cast nodes.
    ///
    /// Catches a boolean node turning its literal operand's absent key into a
    /// real one, and the nullary `all!`/`any!` forms acquiring a key.
    #[test]
    fn literal_only_boolean_and_cast_trees_depend_on_nothing(
        x in any::<f64>(),
        y in any::<f64>(),
        keep in any::<bool>(),
    ) {
        let lhs = Node::<f64, Ctx, _>::lit(x);
        let rhs = Node::<f64, Ctx, _>::lit(y);
        let yes = Node::<bool, Ctx, _>::lit(keep);
        let no = Node::<bool, Ctx, _>::lit(!keep);

        prop_assert!(lhs.lt(rhs).dependencies().is_empty());
        prop_assert!(lhs.gt(rhs).dependencies().is_empty());
        prop_assert!(lhs.le(rhs).dependencies().is_empty());
        prop_assert!(lhs.ge(rhs).dependencies().is_empty());
        prop_assert!(lhs.eq(rhs).dependencies().is_empty());
        prop_assert!(lhs.ne(rhs).dependencies().is_empty());
        prop_assert!(lhs.cast::<i8>().dependencies().is_empty());

        prop_assert!(yes.and(no).dependencies().is_empty());
        prop_assert!(yes.or(no).dependencies().is_empty());
        prop_assert!(yes.xor(no).dependencies().is_empty());
        prop_assert!(yes.nand(no).dependencies().is_empty());
        prop_assert!(yes.nor(no).dependencies().is_empty());
        prop_assert!(yes.nxor(no).dependencies().is_empty());
        prop_assert!(yes.not().dependencies().is_empty());
        prop_assert!((!yes).dependencies().is_empty());
        prop_assert!(all![yes, no, yes].dependencies().is_empty());
        prop_assert!(any![yes, no, yes].dependencies().is_empty());

        let empty_conjunction: Node<bool, Ctx, _> = all![];
        let empty_disjunction: Node<bool, Ctx, _> = any![];
        prop_assert!(empty_conjunction.dependencies().is_empty());
        prop_assert!(empty_disjunction.dependencies().is_empty());

        // Control: swap one literal for a named variable and a key appears.
        let named = Node::<f64, Ctx, _>::var("hp", read_hp);
        prop_assert_eq!(named.gt(rhs).dependencies().len(), 1);
        prop_assert_eq!((named * rhs).dependencies().len(), 1);
    }

    /// Property 2a — arithmetic monotonicity over randomly-shaped trees.
    ///
    /// For `+ - * / %`, `max`, `min`, `powf`, `sqrt`, `abs`, `-`, `acos`, `cast`
    /// and both `clamp` shapes, the composite's keys are exactly the union of its
    /// operands'. Catches a combinator that walks only one child: an invalidation
    /// layer built on `Node::dependencies` would then never hear that an
    /// attribute changed, and would keep a stale value forever.
    #[test]
    fn arithmetic_composites_are_supersets_of_their_operands(
        lhs_leaves in leaves(),
        rhs_leaves in leaves(),
        third_leaves in leaves(),
    ) {
        for bin in Bin::ALL {
            let lhs = build_f64(&lhs_leaves, false);
            let rhs = build_f64(&rhs_leaves, false);
            let left = lhs.dependencies();
            let right = rhs.dependencies();
            // The generator forces a variable into every tree, so neither side can
            // be the empty set — which is a subset of anything.
            prop_assert!(!left.is_empty() && !right.is_empty());
            let composite = combine_f64(lhs, rhs, bin).dependencies();
            check_superset(composite, left, right, "generated tree", bin.name())?;
        }

        for unary in Unary::ALL {
            let lhs = build_f64(&lhs_leaves, false);
            let left = lhs.dependencies();
            prop_assert!(!left.is_empty());
            check_superset1(
                deps_of_unary(lhs, unary),
                left,
                "generated tree",
                unary.name(),
            )?;
        }

        // clamp(expr, expr, expr): every one of the three operands is read.
        let value = build_f64(&third_leaves, false);
        let lo = build_f64(&lhs_leaves, false);
        let hi = build_f64(&rhs_leaves, false);
        let read = value.dependencies();
        let low = lo.dependencies();
        let high = hi.dependencies();
        let composite = value.clamp(lo, hi).dependencies();
        prop_assert!(
            composite.is_superset_of(&read)
                && composite.is_superset_of(&low)
                && composite.is_superset_of(&high),
            "clamp(expr, expr, expr) reports {:?}, missing one of {:?}, {:?}, {:?}",
            composite,
            read,
            low,
            high,
        );
        let mut all_three = read.clone();
        all_three.union_with(&low);
        all_three.union_with(&high);
        prop_assert_eq!(&composite, &all_three, "and nothing beyond the three operands");

        // clamp(expr, literal, literal): only the value being clamped is a key.
        let value = build_f64(&third_leaves, false);
        let read = value.dependencies();
        prop_assert_eq!(&value.clamp(0.0, 1.0).dependencies(), &read);
    }

    /// Property 2b — comparison monotonicity, over `Node::var` and `Node::new`
    /// operands on either side.
    ///
    /// Catches a comparison node that walks only its left operand — the shape most
    /// easily got wrong, since the right operand may also be a bare literal.
    #[test]
    fn comparison_composites_are_supersets_of_their_operands(
        lhs_named in any::<bool>(),
        rhs_named in any::<bool>(),
        lhs_name in 0..NAMES.len(),
        rhs_name in 0..NAMES.len(),
    ) {
        let lhs = f64_var(lhs_named, lhs_name);
        let rhs = f64_var(rhs_named, rhs_name);
        prop_assert!(!lhs.dependencies().is_empty() && !rhs.dependencies().is_empty());
        assert_every_comparison_forwards!(lhs, rhs, "comparison var/var");
        assert_every_unary_forwards!(lhs, "comparison var");
    }

    /// Property 2c — logical monotonicity, over named and type-keyed booleans.
    ///
    /// Catches a boolean node that skips a child — the very defect the may-read
    /// note on `Expr::get_dependencies` promises is *not* there: the walk visits
    /// both operands even when `eval` short-circuits past one of them.
    #[test]
    fn logical_composites_are_supersets_of_their_operands(
        lhs_named in any::<bool>(),
        rhs_named in any::<bool>(),
        lhs_flag in 0..FLAGS.len(),
        rhs_flag in 0..FLAGS.len(),
    ) {
        // One operand from each scheme per case. The name-keyed variable reports
        // `FLAGS[i]`; the type-keyed one reports its own type whatever it reads, so
        // for it the generated index simply selects the value.
        let lhs = bool_var(lhs_named, lhs_flag);
        let rhs = bool_var(rhs_named, rhs_flag);
        prop_assert!(!lhs.dependencies().is_empty() && !rhs.dependencies().is_empty());
        assert_every_logical_forwards!(lhs, rhs, "logical var/var");
    }

    /// Property 2d — the same, for operands whose keys come from different schemes.
    ///
    /// Catches a walk that stops early at a zero-sized `var_leaf!` leaf rather than
    /// a variable, i.e. a bug confined to one leaf shape.
    #[test]
    fn mixed_key_composites_are_supersets_of_their_operands(index in 0..NAMES.len()) {
        let leaf_a = Node::<f64, Ctx, _>::from_expr(ProbeA);
        let leaf_b = Node::<f64, Ctx, _>::from_expr(ProbeB);
        let named = named_f64(index);

        assert_every_arithmetic_forwards!(leaf_a, leaf_b, "leaf/leaf");
        assert_every_unary_forwards!(leaf_a, "leaf");
        assert_every_comparison_forwards!(leaf_a, leaf_b, "leaf/leaf");
        assert_every_arithmetic_forwards!(leaf_a, named, "leaf/var");
        assert_every_comparison_forwards!(leaf_a, named, "leaf/var");
        assert_every_arithmetic_forwards!(named, leaf_b, "var/leaf");
        assert_every_comparison_forwards!(named, leaf_b, "var/leaf");

        let leaf_flag = Node::<bool, Ctx, _>::from_expr(ProbeFlag);
        let named_flag = named_bool(3);
        let new_flag = typed_bool();
        assert_every_logical_forwards!(leaf_flag, named_flag, "logical leaf/var");
        assert_every_logical_forwards!(named_flag, leaf_flag, "logical var/leaf");
        assert_every_logical_forwards!(new_flag, leaf_flag, "logical new-var/leaf");
        assert_every_logical_forwards!(named_flag, new_flag, "logical named-var/new-var");
    }

    /// Property 3 — a `Node::var` variable is keyed by its exact name.
    ///
    /// One name is one key, two spellings are two keys, the key answers
    /// `contains_name`, and it is never a type key. Catches name normalisation (a
    /// trim, a case-fold) silently merging two attributes, and catches a named
    /// variable falling back to the shared `VarExpr<N, C>` key — the failure that
    /// would make `Node::var` indistinguishable from `Node::new`.
    #[test]
    fn named_variables_are_keyed_by_their_exact_name(
        lhs_index in 0..NAMES.len(),
        rhs_index in 0..NAMES.len(),
        absent in 0..ABSENT_NAMES.len(),
    ) {
        let lhs = named_f64(lhs_index);
        let rhs = named_f64(rhs_index);
        let lhs_name = NAMES[lhs_index];
        let rhs_name = NAMES[rhs_index];
        let lhs_deps = lhs.dependencies();
        let rhs_deps = rhs.dependencies();

        prop_assert_eq!(lhs_deps.len(), 1, "{:?} reports exactly one key", lhs_name);
        prop_assert!(lhs_deps.contains_name(lhs_name));
        prop_assert!(lhs_deps.contains(DepKey::name(lhs_name)),
            "the same check with the key spelled out");
        prop_assert!(!lhs_deps.contains_type_of::<VarExpr<f64, Ctx>>(),
            "a named variable reports no type key at all");
        prop_assert!(!lhs_deps.contains_name(ABSENT_NAMES[absent]),
            "and it does not answer for a name it never reads");

        let sum = (lhs + rhs).dependencies();
        if lhs_name == rhs_name {
            prop_assert_eq!(&lhs_deps, &rhs_deps,
                "one spelling is one key, however often it appears");
            prop_assert_eq!(sum.len(), 1, "and the composite holds that key once");
        } else {
            prop_assert_ne!(&lhs_deps, &rhs_deps, "{:?} and {:?} are two keys", lhs_name, rhs_name);
            prop_assert_eq!(sum.len(), 2, "including the near-miss spellings hp / HP / \"hp \"");
            prop_assert!(sum.contains_name(lhs_name) && sum.contains_name(rhs_name));
        }

        // The contrast: `Node::new` carries no name, so it reports the shared leaf key.
        let unkeyed = typed_f64(lhs_index);
        let unkeyed_deps = unkeyed.dependencies();
        prop_assert!(unkeyed_deps.contains_type_of::<VarExpr<f64, Ctx>>());
        prop_assert!(!unkeyed_deps.contains_name(lhs_name));
        prop_assert!(!unkeyed_deps.is_superset_of(&lhs_deps),
            "and it does not cover the named key");
    }

    /// Property 4 — the two keying schemes coexist in one `Deps`.
    ///
    /// A composite of a `Node::var` and a `var_leaf!` leaf reports exactly two
    /// keys, one `Name` and one `Type`. Catches the schemes colliding — a `Name`
    /// compared as a `Type`, say — which would make a leaf and a variable for one
    /// attribute invalidate each other.
    #[test]
    fn a_named_variable_and_a_typed_leaf_report_two_keys_of_two_kinds(index in 0..NAMES.len()) {
        let name = NAMES[index];
        let named = named_f64(index);
        let state = State {
            hp: 4.0,
            defense: 3.0,
            ..State::default()
        };

        let against_a = (named + Node::<f64, Ctx, _>::from_expr(ProbeA)).dependencies();
        let against_b = (named + Node::<f64, Ctx, _>::from_expr(ProbeB)).dependencies();

        for (keys, leaf_key, leaf_name) in [
            (&against_a, DepKey::type_of::<ProbeA>(), "ProbeA"),
            (&against_b, DepKey::type_of::<ProbeB>(), "ProbeB"),
        ] {
            prop_assert_eq!(keys.len(), 2, "one name key and one type key, against {}", leaf_name);
            prop_assert!(keys.contains_name(name), "the named variable, found by name");
            prop_assert!(keys.contains(leaf_key), "the leaf, found by type");
            prop_assert!(!keys.contains_type_of::<VarExpr<f64, Ctx>>(),
                "and the named variable contributes no type key");
            prop_assert!(!keys.contains_name(leaf_name), "a type key is never answerable by name");

            let by_name = keys.iter().filter(|key| key.as_name().is_some()).count();
            let by_type = keys.iter().filter(|key| key.as_type_id().is_some()).count();
            prop_assert_eq!(by_name, 1, "exactly one DepKey::Name");
            prop_assert_eq!(by_type, 1, "exactly one DepKey::Type");
        }

        // Evaluation is untouched by any of the keying: the name reads the attribute
        // it spells, whichever of the two keying schemes it reports.
        prop_assert_eq!(named.eval(&state), read_indexed(index, &state));
    }

    /// Property 5 — evaluating the same tree twice against the same context
    /// yields the same value, and the key walk repeats with it.
    ///
    /// The value-repeat assertions are smoke tests rather than properties:
    /// `Expr::eval` takes nothing but its context, and `src/` holds no interior
    /// mutability, static, or clock for a value to come from, so they can only
    /// fail if one of those stops being true. The dependency assertion at the end
    /// is the one that can bite, and it is checked against every subtree below
    /// the boolean tree, not just its left operand.
    #[test]
    fn evaluation_is_deterministic_for_a_fixed_context(
        lhs_leaves in leaves(),
        rhs_leaves in leaves(),
        state in states(),
    ) {
        let tree = build_f64(&lhs_leaves, false);
        let first = tree.eval(&state);
        let second = tree.eval(&state);
        // Compared as bits, so a NaN result compares equal to itself and a
        // wandering bit pattern cannot hide behind `NaN != NaN`.
        prop_assert_eq!(first.to_bits(), second.to_bits(), "the same node read twice");
        prop_assert_eq!(
            build_f64(&lhs_leaves, false).eval(&state).to_bits(),
            first.to_bits(),
            "and a freshly built copy of the same tree agrees",
        );
        prop_assert_eq!(tree.dependencies(), tree.dependencies(), "the key walk is deterministic too");

        // A boolean tree built from comparison and logical nodes as well.
        let condition = bnot(band(
            blt(build_f64(&lhs_leaves, false), build_f64(&rhs_leaves, false)),
            probe_flag(),
        ));
        prop_assert_eq!(condition.eval(&state), condition.eval(&state));
        prop_assert!(
            condition
                .dependencies()
                .is_superset_of(&build_f64(&lhs_leaves, false).dependencies()),
            "the boolean tree still reports the keys below it"
        );
        prop_assert!(
            condition
                .dependencies()
                .is_superset_of(&build_f64(&rhs_leaves, false).dependencies()),
            "including the operand on the right of the comparison"
        );
        prop_assert!(
            condition
                .dependencies()
                .is_superset_of(&probe_flag().dependencies()),
            "and the leaf folded in with `and`"
        );
    }

    /// Property 6 — the arithmetic identities, on finite inputs.
    ///
    /// Float inputs are filtered to finite values on purpose: `x - x`, `x + 0` and
    /// `sqrt(x * x)` are not identities once NaN is involved. That is float
    /// semantics rather than a bug. Catches a combinator wired to the wrong
    /// operation — `min` built on `max`, `abs` without the sign flip, the operands
    /// of a `-` swapped.
    #[test]
    fn arithmetic_identities_hold_on_finite_floats(
        x in finite_f64(),
        y in finite_f64(),
    ) {
        let state = State::default();
        let left = Node::<f64, Ctx, _>::lit(x);
        let right = Node::<f64, Ctx, _>::lit(y);

        prop_assert_eq!((left + 0.0).eval(&state), x, "x + 0 is x");
        prop_assert_eq!((left * 1.0).eval(&state), x, "x * 1 is x");

        let same = Node::<f64, Ctx, _>::lit(x);
        let also_same = Node::<f64, Ctx, _>::lit(x);
        prop_assert_eq!((same - also_same).eval(&state), 0.0, "x - x is 0");

        let node_max = left.max(right).eval(&state);
        let node_min = left.min(right).eval(&state);
        let low = x.min(y);
        let high = x.max(y);
        prop_assert!(
            node_min >= low && node_min <= high,
            "min({}, {}) returned {}, outside [{}, {}]",
            x, y, node_min, low, high,
        );
        prop_assert!(
            node_max >= low && node_max <= high,
            "max({}, {}) returned {}, outside [{}, {}]",
            x, y, node_max, low, high,
        );
        // `Float::max`/`Float::min` forward to the inherent methods, so the node
        // has to agree with them exactly, not merely stay in range.
        prop_assert_eq!(node_max, high, "max(..) is f64::max");
        prop_assert_eq!(node_min, low, "min(..) is f64::min");

        let absolute = Node::<f64, Ctx, _>::lit(x).abs();
        prop_assert!(absolute.eval(&state) >= 0.0, "abs(x) is never negative");
        prop_assert_eq!(absolute.eval(&state), x.abs(), "abs(x) is f64::abs");

        // sqrt(x * x) approximates |x|. The multiply and the root each cost about
        // half an ulp, hence the tolerance; the magnitude is folded into
        // [1e-150, 1e150] so the multiply can neither overflow to infinity nor
        // underflow to zero, which are float limits rather than node behaviour.
        let magnitude = x.abs().clamp(1e-150, 1e150);
        let squared = Node::<f64, Ctx, _>::lit(magnitude) * Node::<f64, Ctx, _>::lit(magnitude);
        let root = squared.sqrt().eval(&state);
        prop_assert!(
            (root - magnitude).abs() <= magnitude * 1e-12,
            "sqrt({} * {}) = {}, expected about {}",
            magnitude, magnitude, root, magnitude,
        );
    }

    /// Property 7 — `clamp` is bounded and idempotent.
    ///
    /// `lo <= hi` is a precondition rather than something the property tests,
    /// because `f32::clamp`/`f64::clamp` panic on `min > max` — and on a NaN bound
    /// — in both debug and release, which the crate documents as a panic. The
    /// bounds are sorted by [`ordered_bounds`] and every input is finite for the
    /// same reason. Catches clamp ignoring its bounds (a value escaping `[lo, hi]`)
    /// or applying its transformation twice.
    #[test]
    fn clamp_is_bounded_and_idempotent(
        value in finite_f64(),
        bounds in ordered_bounds(),
    ) {
        let (lo, hi) = bounds;
        let state = State::default();

        let once = Node::<f64, Ctx, _>::lit(value).clamp(lo, hi).eval(&state);
        prop_assert!(
            once >= lo && once <= hi,
            "clamp({}, {}, {}) escaped its bounds: {}",
            value, lo, hi, once,
        );

        let twice = Node::<f64, Ctx, _>::lit(once).clamp(lo, hi).eval(&state);
        prop_assert_eq!(twice, once, "clamping an already-clamped value changes nothing");

        // The same tree clamped twice over says the same — no second squeeze.
        let nested = Node::<f64, Ctx, _>::lit(value)
            .clamp(lo, hi)
            .clamp(lo, hi)
            .eval(&state);
        prop_assert_eq!(nested, once);

        // And bounds carried as expressions rather than literals agree, which is
        // the shape a caller writes when the range varies.
        let expression_bounded = Node::<f64, Ctx, _>::lit(value)
            .clamp(Node::<f64, Ctx, _>::lit(lo), Node::<f64, Ctx, _>::lit(hi))
            .eval(&state);
        prop_assert_eq!(expression_bounded, once, "clamp(expr, expr, expr) matches the literal form");
    }

    /// Property 8 — the boolean laws, over generated boolean expressions.
    ///
    /// De Morgan on plain variables and on composite operands, double negation,
    /// excluded middle and non-contradiction, the definitions of `xor`, `nand`,
    /// `nor` and `nxor`, commutativity, distribution, and `a < b` as the negation
    /// of `a >= b` for finite floats. The finite filter is required rather than
    /// defensive: for NaN both `a < b` and `a >= b` are false, so the negation law
    /// genuinely fails there. Catches a comparison or logical node built on the
    /// wrong operator — `ge` as `gt`, `nxor` missing its negation — which every
    /// condition in a caller's rules would then inherit.
    #[test]
    fn boolean_algebra_laws_hold(
        first in 0..FLAGS.len(),
        second in 0..FLAGS.len(),
        flags in prop::array::uniform8(any::<bool>()),
        x in finite_f64(),
        y in finite_f64(),
    ) {
        let state = State { flags, ..State::default() };
        // `a` and `b` are name-keyed and `c` is a type-keyed leaf, so the laws are
        // checked across both operand flavours. A type-keyed leaf reports its own
        // type as its key whatever it reads, so it needs no index here.
        let a = named_bool(first);
        let b = named_bool(second);
        let c = typed_bool();

        prop_assert!(a.and(b).not().eq(a.not().or(b.not())).eval(&state), "not(a && b) is !a || !b");
        prop_assert!(a.or(b).not().eq(a.not().and(b.not())).eval(&state), "not(a || b) is !a && !b");

        // De Morgan again, over composite operands.
        let left = a.and(c);
        let right = b.or(c);
        prop_assert!(left.and(right).not().eq(left.not().or(right.not())).eval(&state),
            "De Morgan over composites");

        prop_assert!(a.not().not().eq(a).eval(&state), "double negation");
        prop_assert!(a.or(a.not()).eval(&state), "excluded middle");
        prop_assert!(!a.and(a.not()).eval(&state), "non-contradiction");

        prop_assert!(a.xor(b).eq(a.ne(b)).eval(&state), "xor is != on bools");
        prop_assert!(a.nand(b).eq(a.and(b).not()).eval(&state), "nand is !(a && b)");
        prop_assert!(a.nor(b).eq(a.or(b).not()).eval(&state), "nor is !(a || b)");
        prop_assert!(a.nxor(b).eq(a.eq(b)).eval(&state), "nxor is a == b");
        prop_assert!(a.and(b).eq(b.and(a)).eval(&state), "and is commutative");
        prop_assert!(a.or(b.and(c)).eq(a.or(b).and(a.or(c))).eval(&state), "or distributes over and");

        // Comparisons: `lt` is the exact negation of `ge` on finite floats.
        let lhs = Node::<f64, Ctx, _>::lit(x);
        let rhs = Node::<f64, Ctx, _>::lit(y);
        prop_assert!(lhs.lt(rhs).eq(lhs.ge(rhs).not()).eval(&state), "{} < {} is !({} >= {})", x, y, x, y);
        prop_assert!(lhs.gt(rhs).eq(lhs.le(rhs).not()).eval(&state), "{} > {} is !({} <= {})", x, y, x, y);
        prop_assert!(lhs.eq(rhs).eq(lhs.ne(rhs).not()).eval(&state), "== is the negation of !=");
        prop_assert!(lhs.lt(rhs).and(lhs.gt(rhs)).not().eval(&state),
            "a finite value is never both below and above another");
    }

    /// Property 9 — `all!` and `any!` agree with the obvious semantics.
    ///
    /// Over a generated vector of up to eight boolean expressions, `all![..]`
    /// evaluates like `Iterator::all` and `any![..]` like `Iterator::any`. The
    /// zero-argument case is asserted from what the macros actually do — `true`
    /// and `false`, the identity element of each operator, which is also what
    /// `all`/`any` report for an empty slice — rather than assumed. Catches the
    /// fold being seeded with the wrong identity, which would flip every
    /// multi-condition gate.
    #[test]
    fn all_and_any_agree_with_vec_semantics(
        flags in prop::array::uniform8(any::<bool>()),
        arity in 0..=FLAGS.len(),
    ) {
        let state = State { flags, ..State::default() };
        let nodes = [
            named_bool(0),
            named_bool(1),
            named_bool(2),
            named_bool(3),
            named_bool(4),
            named_bool(5),
            named_bool(6),
            named_bool(7),
        ];

        let conjunct = all_of(&nodes, arity);
        let disjunct = any_of(&nodes, arity);
        let expected_all = flags[..arity].iter().all(|value| *value);
        let expected_any = flags[..arity].iter().any(|value| *value);

        prop_assert_eq!(conjunct.eval(&state), expected_all, "all![..] over {} variables", arity);
        prop_assert_eq!(disjunct.eval(&state), expected_any, "any![..] over {} variables", arity);
        prop_assert_eq!(conjunct.dependencies().len(), arity, "every operand of the fold stays a key");
        prop_assert_eq!(disjunct.dependencies().len(), arity);

        if arity == 0 {
            // Spelled out rather than inferred from the fold above: the nullary
            // forms are constants, and `Iterator::all`/`any` agree with them.
            let empty_conjunction: Node<bool, Ctx, _> = all![];
            let empty_disjunction: Node<bool, Ctx, _> = any![];
            prop_assert!(empty_conjunction.eval(&state), "all![] is true");
            prop_assert!(!empty_disjunction.eval(&state), "any![] is false");
        }
    }

    /// Property 10 — `cast` is total and reproduces Rust's `as`.
    ///
    /// Seven primitive pairs, covering all five the docs promise (`f32`→`i32`,
    /// `f64`→`u8`, `i32`→`u8`, `i64`→`i8`, `u32`→`i32`) plus int→float and
    /// float→float, over arbitrary bit patterns — NaN and both infinities
    /// included, since the documented promise is that NaN becomes `0` and
    /// out-of-range values saturate, never that a cast can panic. Catches `AsCast`
    /// drifting from `as` — a saturating pair written as a wrapping one, or a cast
    /// that panics — which is the one promise this node makes.
    #[test]
    fn cast_agrees_with_rust_as(
        f32_value in any::<f32>(),
        f64_value in any::<f64>(),
        i32_value in any::<i32>(),
        i64_value in any::<i64>(),
        u32_value in any::<u32>(),
    ) {
        let state = State::default();

        prop_assert_eq!(
            Node::<f32, Ctx, _>::lit(f32_value).cast::<i32>().eval(&state),
            f32_value as i32,
            "f32 -> i32 for {}", f32_value,
        );
        prop_assert_eq!(
            Node::<f64, Ctx, _>::lit(f64_value).cast::<u8>().eval(&state),
            f64_value as u8,
            "f64 -> u8 for {}", f64_value,
        );
        prop_assert_eq!(
            Node::<i32, Ctx, _>::lit(i32_value).cast::<u8>().eval(&state),
            i32_value as u8,
            "i32 -> u8");
        prop_assert_eq!(
            Node::<i64, Ctx, _>::lit(i64_value).cast::<i8>().eval(&state),
            i64_value as i8,
            "i64 -> i8");
        prop_assert_eq!(
            Node::<u32, Ctx, _>::lit(u32_value).cast::<i32>().eval(&state),
            u32_value as i32,
            "u32 -> i32");
        prop_assert_eq!(
            Node::<f32, Ctx, _>::lit(f32_value).cast::<u64>().eval(&state),
            f32_value as u64,
            "f32 -> u64 for {}", f32_value,
        );
        prop_assert_eq!(
            Node::<i32, Ctx, _>::lit(i32_value).cast::<f64>().eval(&state).to_bits(),
            (i32_value as f64).to_bits(),
            "i32 -> f64, compared as bits because the rounding is deterministic");

        // Float to float: NaN never compares equal to itself, so the NaN case is
        // checked as NaN and every other value exactly.
        let narrowed = Node::<f64, Ctx, _>::lit(f64_value).cast::<f32>().eval(&state);
        let as_narrowed = f64_value as f32;
        prop_assert!(
            narrowed == as_narrowed || (narrowed.is_nan() && as_narrowed.is_nan()),
            "f64 -> f32 for {}: {} vs {}", f64_value, narrowed, as_narrowed,
        );

        // A cast hanging off a variable is total too, and keeps the variable's key.
        let keyed = Node::<f64, Ctx, _>::var("hp", read_hp).cast::<u8>();
        prop_assert_eq!(keyed.eval(&state), state.hp as u8, "casting a variable reads the context");
        prop_assert!(keyed.dependencies().contains_name("hp"), "and the cast forwards the key");

        // The documented specials, so a change of rule cannot slip past the pairs.
        // Each float source gets its own, because saturation and the NaN rule are
        // per-pair conversions: `as` is written once per target type.
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::NAN).cast::<i32>().eval(&state), 0, "NaN becomes 0");
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::INFINITY).cast::<i32>().eval(&state), i32::MAX);
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::NEG_INFINITY).cast::<i32>().eval(&state), i32::MIN);
        prop_assert_eq!(Node::<f32, Ctx, _>::lit(f32::NAN).cast::<i32>().eval(&state), 0, "NaN becomes 0 from f32 too");
        prop_assert_eq!(Node::<f32, Ctx, _>::lit(f32::INFINITY).cast::<i32>().eval(&state), i32::MAX);
        prop_assert_eq!(Node::<f32, Ctx, _>::lit(f32::NEG_INFINITY).cast::<i32>().eval(&state), i32::MIN);
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::NAN).cast::<u8>().eval(&state), 0);
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::INFINITY).cast::<u8>().eval(&state), u8::MAX);
        prop_assert_eq!(Node::<f64, Ctx, _>::lit(f64::NEG_INFINITY).cast::<u8>().eval(&state), 0, "negative infinity saturates to the unsigned minimum");
        prop_assert_eq!(Node::<f32, Ctx, _>::lit(f32::NAN).cast::<u64>().eval(&state), 0);
        prop_assert_eq!(Node::<f32, Ctx, _>::lit(f32::INFINITY).cast::<u64>().eval(&state), u64::MAX);
    }

    /// Property 11 — a plan applies every step, or stops at the failure.
    ///
    /// Over a generated sequence of assignment steps on distinct fields, a run
    /// that succeeds leaves every field set, and a run whose step `k` fails leaves
    /// steps `0..k` applied and the rest untouched. That is the documented
    /// no-rollback behaviour of `Plan::run`, asserted exactly: a step after the
    /// failure writing its field, or a step before it being skipped, both fail
    /// this property.
    #[test]
    fn a_plan_applies_every_step_or_stops_at_the_failure(
        values in prop::collection::vec(any::<i32>(), 1..=5),
        first_field in 0..SETTERS.len(),
        fail_at in 0..8usize,
    ) {
        // Fields come from a rotation starting at `first_field`, so they are
        // distinct and no later step overwrites an earlier step's field.
        let steps: Vec<(usize, i32)> = values
            .iter()
            .enumerate()
            .map(|(index, value)| ((first_field + index) % SETTERS.len(), *value))
            .collect();
        // The failing step is a lookup of a key nobody wrote. A `fail_at` past the
        // end of the sequence means the plan runs to completion.
        let failure = fail_at.min(steps.len());

        let mut plan = Plan::<PlanCtx>::new();
        let mut expected = [SENTINEL; SETTERS.len()];
        for (index, (field, value)) in steps.iter().enumerate() {
            if index == failure {
                plan = plan.step(CacheLookupStep::<i32>::new("never_written"));
            }
            plan = plan.step(AssignmentStep::<i32, _, _>::new(
                SETTERS[*field],
                Node::lit(*value),
                None,
            ));
            // Only the steps ahead of the failing one are applied.
            if index < failure {
                expected[*field] = *value;
            }
        }

        let mut state = PlanState::sentinel();
        let result = plan.run(&mut state);
        let written = state.fields();

        if failure < steps.len() {
            prop_assert!(result.is_err(), "a lookup of a key nobody wrote must fail the run");
            if let Err(error) = result {
                prop_assert!(matches!(error, PlanError::CacheMiss { .. }),
                    "expected a cache miss, got {:?}", error);
            }
            prop_assert_eq!(written, expected,
                "the steps before the failure stay applied; the rest never ran");
        } else {
            prop_assert!(result.is_ok(), "a plan with no failing step must succeed: {:?}", result);
            prop_assert_eq!(written, expected, "a successful run leaves every field set");
        }
    }
}

// ---------------------------------------------------------------------------
// `all!` / `any!` at every arity
// ---------------------------------------------------------------------------

/// `all![nodes[0..arity]]`, erased to one node type.
///
/// One arm per arity, because the `and` chain the macro expands to has a distinct
/// Rust type for each length — which is why the arity cannot simply be a runtime
/// argument to the macro.
fn all_of(nodes: &[Var<bool, Ctx>], arity: usize) -> BTree {
    match arity {
        // Verified behaviour, not an assumption: with nothing to conjoin the
        // macro is the constant `true`, and the context type has to be supplied
        // because there is no argument left to infer it from.
        0 => {
            let empty: Node<bool, Ctx, _> = all![];
            erase(empty)
        }
        1 => erase(all![nodes[0]]),
        2 => erase(all![nodes[0], nodes[1]]),
        3 => erase(all![nodes[0], nodes[1], nodes[2]]),
        4 => erase(all![nodes[0], nodes[1], nodes[2], nodes[3]]),
        5 => erase(all![nodes[0], nodes[1], nodes[2], nodes[3], nodes[4]]),
        6 => erase(all![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5]
        ]),
        7 => erase(all![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5], nodes[6]
        ]),
        8 => erase(all![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5], nodes[6], nodes[7]
        ]),
        _ => unreachable!("arity is generated from 0..=FLAGS.len()"),
    }
}

/// `any![nodes[0..arity]]`, erased to one node type. See [`all_of`].
fn any_of(nodes: &[Var<bool, Ctx>], arity: usize) -> BTree {
    match arity {
        0 => {
            let empty: Node<bool, Ctx, _> = any![];
            erase(empty)
        }
        1 => erase(any![nodes[0]]),
        2 => erase(any![nodes[0], nodes[1]]),
        3 => erase(any![nodes[0], nodes[1], nodes[2]]),
        4 => erase(any![nodes[0], nodes[1], nodes[2], nodes[3]]),
        5 => erase(any![nodes[0], nodes[1], nodes[2], nodes[3], nodes[4]]),
        6 => erase(any![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5]
        ]),
        7 => erase(any![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5], nodes[6]
        ]),
        8 => erase(any![
            nodes[0], nodes[1], nodes[2], nodes[3], nodes[4], nodes[5], nodes[6], nodes[7]
        ]),
        _ => unreachable!("arity is generated from 0..=FLAGS.len()"),
    }
}

// ---------------------------------------------------------------------------
// Property 11 fixtures: a mutable context, one setter per field
// ---------------------------------------------------------------------------

/// The value every field starts at, so "this step never ran" is observable.
const SENTINEL: i32 = -999_999;

#[derive(Clone, Copy, Debug, PartialEq)]
struct PlanState {
    a: i32,
    b: i32,
    hp: i32,
    note: i32,
    side: i32,
}

impl PlanState {
    fn sentinel() -> Self {
        Self {
            a: SENTINEL,
            b: SENTINEL,
            hp: SENTINEL,
            note: SENTINEL,
            side: SENTINEL,
        }
    }

    fn fields(&self) -> [i32; SETTERS.len()] {
        [self.a, self.b, self.hp, self.note, self.side]
    }
}

struct PlanCtx;

impl Context for PlanCtx {
    type ContextItem<'w, 's> = PlanState;
}

impl ContextMut for PlanCtx {
    type ContextItemMut<'w, 's> = PlanState;

    fn as_readonly<'w, 's, 'a>(
        mut_item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's> {
        *mut_item
    }
}

fn set_a(state: &mut PlanState, value: i32) {
    state.a = value;
}

fn set_b(state: &mut PlanState, value: i32) {
    state.b = value;
}

fn set_hp(state: &mut PlanState, value: i32) {
    state.hp = value;
}

fn set_note(state: &mut PlanState, value: i32) {
    state.note = value;
}

fn set_side(state: &mut PlanState, value: i32) {
    state.side = value;
}

/// One setter per field, all of one `fn` type, so a generated index picks the
/// field at run time and one plan can hold a generated number of steps.
const SETTERS: [fn(&mut PlanState, i32); 5] = [set_a, set_b, set_hp, set_note, set_side];

/// `Node::var("")` is legal, and every empty-named variable in the crate shares
/// that single key — documented on `Node::var` and on `DepKey`, and pinned here
/// so it cannot change without someone noticing.
#[test]
fn an_empty_name_is_a_legal_key_shared_by_every_empty_named_variable() {
    let first = Node::<f64, Ctx, _>::var("", read_hp);
    let second = Node::<f64, Ctx, _>::var("", read_hp);
    let deps = (first + second).dependencies();
    assert_eq!(deps.len(), 1, "two empty-named variables are one key");
    assert!(deps.contains_name(""), "the empty string is a legal key");

    let named = Node::<f64, Ctx, _>::var("hp", read_hp).dependencies();
    assert!(named.contains_name("hp"));
    assert!(!named.contains_name(""), "and it is not the empty key");
}

/// The `&'static str` on name keys stops a runtime string becoming a key, but a
/// runtime string can still be tested against a set, which is the escape hatch
/// the `Deps::contains_name` doc promises.
#[test]
fn a_runtime_string_can_be_queried_against_a_name_key() {
    let deps = Node::<f64, Ctx, _>::var("hp", read_hp).dependencies();
    let hit = String::from("hp");
    let miss = String::from("attack");
    assert!(deps.iter().any(|k| k.as_name() == Some(hit.as_str())));
    assert!(!deps.iter().any(|k| k.as_name() == Some(miss.as_str())));
}
