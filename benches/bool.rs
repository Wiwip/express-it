use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use express_it::logic::CompareExpr;
use express_it::test_utils::{Atk, Def, MapContext, scopes};

fn bench_trinary(c: &mut Criterion) {
    let mut ctx = MapContext::default();
    ctx.insert::<Atk>(scopes::SRC, 15.0);
    let expr = Atk::get(scopes::SRC)
        .gt(10.0_f32)
        .then(2.0_f32)
        .otherwise(3.0_f32);

    c.bench_function("trinary_if_then_else", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

fn bench_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("comparison");

    let cases = [
        ("gt", {
            let mut ctx = MapContext::default();
            ctx.insert::<Atk>(scopes::SRC, 12.0);
            ctx.insert::<Def>(scopes::SRC, 8.0);
            let expr = Atk::get(scopes::SRC).gt(Def::get(scopes::SRC));
            (expr, ctx)
        }),
        ("lt", {
            let mut ctx = MapContext::default();
            ctx.insert::<Atk>(scopes::SRC, 12.0);
            ctx.insert::<Def>(scopes::SRC, 8.0);
            let expr = Atk::get(scopes::SRC).lt(Def::get(scopes::SRC));
            (expr, ctx)
        }),
    ];

    for (name, (expr, ctx)) in cases {
        group.bench_with_input(criterion::BenchmarkId::from_parameter(name), &(), |b, _| {
            b.iter(|| black_box(expr.clone().eval(&ctx)).unwrap());
        });
    }

    group.finish();
}

fn bench_logic(c: &mut Criterion) {
    let mut ctx = MapContext::default();
    ctx.insert::<Atk>(scopes::SRC, 1.0);
    let expr = Atk::get(scopes::SRC).gt(0.0);

    c.bench_function("logic_eval", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

criterion_group!(bool_benches, bench_trinary, bench_comparison, bench_logic);
criterion_main!(bool_benches);
