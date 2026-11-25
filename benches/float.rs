//! Float evaluation: literals, variables, unary and binary nodes, a plain Rust
//! addition as the floor they are measured against, and the dependency walk.
use criterion::{Criterion, criterion_group, criterion_main};
use express_it::expr::{Context, Expr};
use express_it::nodes::{Node, VarExpr};
use express_it::ops::{ExprAbs, ExprSqrt};
use std::any::TypeId;
use std::collections::HashSet;
use std::hint::black_box;
use std::marker::PhantomData;

#[derive(Clone, Copy)]
struct State {
    atk: f32,
    def: f32,
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

const STATE: State = State {
    atk: 10.0,
    def: 3.0,
};

fn atk() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.atk)
}

fn def() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.def)
}

fn bench_literal(c: &mut Criterion) {
    let expr = Node::<f32, Ctx, _>::lit(42.0);

    c.bench_function("float_literal_eval", |b| {
        b.iter(|| black_box(expr.eval(&STATE)));
    });
}

fn bench_var(c: &mut Criterion) {
    let expr = atk();

    c.bench_function("float_var_eval", |b| {
        b.iter(|| black_box(expr.eval(&STATE)));
    });
}

fn bench_unary(c: &mut Criterion) {
    let neg = -Node::<f32, Ctx, _>::lit(9.0);
    let abs = Node::<f32, Ctx, _>::lit(-9.0).abs();
    let sqrt = Node::<f32, Ctx, _>::lit(9.0).sqrt();

    let mut group = c.benchmark_group("float_unary");
    group.bench_function("neg", |b| {
        b.iter(|| black_box(neg.eval(&STATE)));
    });
    group.bench_function("abs", |b| {
        b.iter(|| black_box(abs.eval(&STATE)));
    });
    group.bench_function("sqrt", |b| {
        b.iter(|| black_box(sqrt.eval(&STATE)));
    });
    group.finish();
}

fn bench_binary(c: &mut Criterion) {
    let expr = atk() + def();

    let mut group = c.benchmark_group("float_add");
    group.bench_function("var_plus_var", |b| {
        b.iter(|| black_box(expr.eval(&STATE)));
    });
    group.bench_function("plain_rust_add", |b| {
        b.iter(|| black_box(black_box(10.0_f32) + black_box(3.0_f32)));
    });
    group.finish();
}

/// A leaf that reports a key of its own, the shape a consumer uses when its
/// variables need per-attribute identity — what `express_it::var_leaf!` generates,
/// written out by hand.
struct Leaf<K>(PhantomData<K>);

impl<K: 'static> Clone for Leaf<K> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K: 'static> Copy for Leaf<K> {}

impl<K: 'static, C: Context> Expr<f32, C> for Leaf<K> {
    fn eval(&self, _ctx: &C::ContextItem<'_, '_>) -> f32 {
        1.0
    }

    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        deps.insert(TypeId::of::<K>());
    }
}

struct Attack;
struct Defense;
struct Haste;

fn leaf<K: 'static>() -> Node<f32, Ctx, Leaf<K>> {
    Node::from_expr(Leaf(PhantomData))
}

fn bench_dependency_walk(c: &mut Criterion) {
    let expr = (leaf::<Attack>() + leaf::<Defense>()) * leaf::<Haste>();

    let mut group = c.benchmark_group("get_dependencies");
    group.bench_function("three_leaves", |b| {
        b.iter(|| {
            let mut deps = HashSet::new();
            expr.get_dependencies(&mut deps);
            black_box(deps.len());
        });
    });
    group.bench_function("empty_sink_reuse", |b| {
        let mut deps = HashSet::new();
        b.iter(|| {
            deps.clear();
            expr.get_dependencies(&mut deps);
            black_box(deps.len());
        });
    });
    group.finish();
}

criterion_group!(
    float_benches,
    bench_literal,
    bench_var,
    bench_unary,
    bench_binary,
    bench_dependency_walk
);
criterion_main!(float_benches);
