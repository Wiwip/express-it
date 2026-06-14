use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use express_it::frame::LazyPlan;
use express_it::test_utils::{scopes, Atk, Def, Hp, MapContext};

fn bench_dependency(c: &mut Criterion) {
    let expr = (Hp::get(scopes::SRC) - Atk::get(scopes::DST)).max(0.0);

    let mut ctx = MapContext::default();
    ctx.insert::<Hp>(scopes::SRC, 50.0);
    ctx.insert::<Atk>(scopes::DST, 20.0);

    c.bench_function("dependent_expr", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

fn bench_plan_commit(c: &mut Criterion) {
    let mut ctx = MapContext::default();
    ctx.insert::<Atk>(scopes::SRC, 10.0);
    ctx.insert::<Hp>(scopes::DST, 20.0);
    ctx.insert::<Def>(scopes::DST, 2.0);

    let plan = LazyPlan::new()
        .step(Hp::set(
            scopes::SRC,
            (Atk::get(scopes::SRC) - Def::get(scopes::DST)).max(0.0),
        ))
        .step(Hp::set(
            scopes::DST,
            Hp::get(scopes::SRC) - Hp::get(scopes::DST),
        ));

    c.bench_function("plan_commit", |b| {
        b.iter(|| plan.commit(&mut ctx).unwrap())
    });
}

criterion_group!(plan_benches, bench_dependency, bench_plan_commit);
criterion_main!(plan_benches);
