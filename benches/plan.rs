//! Plan evaluation: a dependent expression on its own, then the same work as a
//! three-step plan that writes through to the context and the plan cache.
use criterion::{Criterion, criterion_group, criterion_main};
use express_it::expr::{Context, ContextMut, Expr};
use express_it::nodes::{Node, VarExpr};
use express_it::ops::ExprMax;
use express_it::plan::Plan;
use express_it::{save, save_cache};
use std::hint::black_box;

#[derive(Clone, Copy)]
struct State {
    hp: f32,
    atk: f32,
    def: f32,
    shield: f32,
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

const START: State = State {
    hp: 50.0,
    atk: 20.0,
    def: 5.0,
    shield: 0.0,
};

fn hp() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.hp)
}

fn atk() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.atk)
}

fn def() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.def)
}

fn shield() -> Node<f32, Ctx, VarExpr<f32, Ctx>> {
    Node::new(|ctx: &State| ctx.shield)
}

fn bench_dependent_expr(c: &mut Criterion) {
    let expr = (hp() - atk()).max(0.0);

    c.bench_function("dependent_expr_eval", |b| {
        b.iter(|| black_box(expr.eval(&START)));
    });
}

fn bench_plan_run(c: &mut Criterion) {
    let plan = Plan::<Ctx>::new()
        // Writes to the context and records the value under a cache key.
        .step(save!(
            State,
            shield,
            (atk() - def()).max(0.0),
            "shield_applied"
        ))
        // Reads the field the previous step wrote: steps run in order and each
        // one sees the writes of the steps before it.
        .step(save!(State, hp, hp() - shield()))
        // Cache-only step, no context write.
        .step(save_cache!(
            "hp_after",
            Node::<f32, Ctx, _>::new(|ctx: &State| ctx.hp)
        ));

    c.bench_function("plan_run_three_steps", |b| {
        let mut state = START;
        b.iter(|| {
            state = START;
            plan.run(&mut state).expect("plan should run to completion");
            black_box(state.hp);
        });
    });
}

criterion_group!(plan_benches, bench_dependent_expr, bench_plan_run);
criterion_main!(plan_benches);
