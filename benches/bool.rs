//! Boolean evaluation: comparisons, the two short-circuiting combinators, and
//! `clamp`, the three-child node that replaced the old if/then/else select.
use criterion::{Criterion, criterion_group, criterion_main};
use express_it::expr::{Context, Expr};
use express_it::logic::{ExprCmpGt, ExprCmpLt, ExprLogicalAnd, ExprLogicalOr};
use express_it::nodes::{Node, VarExpr};
use express_it::ops::ExprClamp;
use express_it::{all, any};
use std::hint::black_box;

#[derive(Clone, Copy)]
struct State {
    atk: f32,
    def: f32,
    hp: f32,
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

fn atk() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.atk)
}

fn def() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.def)
}

fn hp() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.hp)
}

const STATE: State = State {
    atk: 12.0,
    def: 8.0,
    hp: 50.0,
};

fn bench_comparison(c: &mut Criterion) {
    let expr = atk().gt(def());

    c.bench_function("comparison_var_gt_var", |b| {
        b.iter(|| black_box(expr.eval(&STATE)));
    });
}

fn bench_short_circuit(c: &mut Criterion) {
    // Left operand false, so `and` never evaluates the rest of the tree.
    let short = atk().lt(0.0).and(hp().gt(0.0));
    // Every operand true, so `and` evaluates the whole tree.
    let full = all![atk().gt(def()), def().gt(0.0), hp().gt(0.0)];

    let mut group = c.benchmark_group("logical_and");
    group.bench_function("short_circuited", |b| {
        b.iter(|| black_box(short.eval(&STATE)));
    });
    group.bench_function("fully_evaluated", |b| {
        b.iter(|| black_box(full.eval(&STATE)));
    });
    group.finish();
}

fn bench_logic(c: &mut Criterion) {
    let conj = all![atk().gt(def()), def().gt(0.0)];
    // First operand true, so `or` skips the rest.
    let disj = any![atk().gt(def()), def().gt(hp()), hp().gt(0.0)];

    let mut group = c.benchmark_group("logic");
    group.bench_function("all_two_conditions", |b| {
        b.iter(|| black_box(conj.eval(&STATE)));
    });
    group.bench_function("any_first_true", |b| {
        b.iter(|| black_box(disj.eval(&STATE)));
    });
    group.finish();
}

fn bench_clamp(c: &mut Criterion) {
    // Three children, all of them read on every evaluation.
    let expr = hp().clamp(def(), atk());

    c.bench_function("trinary_clamp", |b| {
        b.iter(|| black_box(expr.eval(&STATE)));
    });
}

criterion_group!(
    bool_benches,
    bench_comparison,
    bench_short_circuit,
    bench_logic,
    bench_clamp
);
criterion_main!(bool_benches);
