//! Integer evaluation: a single variable, then a chain of integer operators.
use criterion::{Criterion, criterion_group, criterion_main};
use express_it::expr::{Context, Expr};
use express_it::nodes::Node;
use std::hint::black_box;

#[derive(Clone, Copy)]
struct State {
    hp: i32,
    damage: i32,
}

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = State;
}

fn bench_int_var(c: &mut Criterion) {
    let state = State {
        hp: 100,
        damage: 35,
    };
    let hp = Node::<i32, Ctx, _>::new(|ctx: &State| ctx.hp);

    c.bench_function("int_var_eval", |b| {
        b.iter(|| black_box(hp.eval(&state)));
    });
}

fn bench_int_chain(c: &mut Criterion) {
    let state = State {
        hp: 100,
        damage: 35,
    };
    let hp = Node::<i32, Ctx, _>::new(|ctx: &State| ctx.hp);
    let damage = Node::<i32, Ctx, _>::new(|ctx: &State| ctx.damage);
    let expr = ((hp - damage) * 2) / 3 + 1;

    c.bench_function("int_chain_eval", |b| {
        b.iter(|| black_box(expr.eval(&state)));
    });
}

criterion_group!(int_benches, bench_int_var, bench_int_chain);
criterion_main!(int_benches);
