use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use express_it::test_utils::{scopes, Atk, Def, MapContext, MapSchema};
use express_it::{expr::Expr, float::FloatExprNode};

fn bench_float_literal(c: &mut Criterion) {
    let ctx = MapContext::default();
    let expr: Expr<f32, MapSchema> =
        Expr::new(black_box(std::sync::Arc::new(FloatExprNode::Lit(42.0))));

    c.bench_function("float_literal_eval", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

fn bench_float_attribute(c: &mut Criterion) {
    let expr = Atk::get(scopes::SRC);
    let mut ctx = MapContext::default();
    ctx.insert::<Atk>(scopes::SRC, 10.0);

    c.bench_function("float_attribute_eval", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

fn bench_float_binary(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_binary");
    group.measurement_time(std::time::Duration::from_secs(10));

    let cases = [
        (
            "add",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC) + Def::get(scopes::SRC);
                (expr, ctx)
            },
        ),
        (
            "sub",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC) - Def::get(scopes::SRC);
                (expr, ctx)
            },
        ),
        (
            "mul",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC) * Def::get(scopes::SRC);
                (expr, ctx)
            },
        ),
        (
            "div",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC) / Def::get(scopes::SRC);
                (expr, ctx)
            },
        ),
        (
            "min",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC).min(Def::get(scopes::SRC));
                (expr, ctx)
            },
        ),
        (
            "max",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 10.0);
                ctx.insert::<Def>(scopes::SRC, 3.0);
                let expr = Atk::get(scopes::SRC).max(Def::get(scopes::SRC));
                (expr, ctx)
            },
        ),
    ];

    for (name, (expr, ctx)) in cases {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(name),
            &(),
            |b, _| {
                b.iter(|| black_box(expr.clone().eval(&ctx)).unwrap());
            },
        );
    }

    group.finish();
}

fn bench_float_unary(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_unary");

    let cases = [
        (
            "neg",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 9.0);
                let a = Atk::get(scopes::SRC);
                let expr = -a;
                (expr, ctx)
            },
        ),
        (
            "abs",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 9.0);
                let a = Atk::get(scopes::SRC);
                let expr = a.abs();
                (expr, ctx)
            },
        ),
        (
            "sin",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 9.0);
                let a = Atk::get(scopes::SRC);
                let expr = a.sin();
                (expr, ctx)
            },
        ),
        (
            "sqrt",
            {
                let mut ctx = MapContext::default();
                ctx.insert::<Atk>(scopes::SRC, 9.0);
                let a = Atk::get(scopes::SRC);
                let expr = a.sqrt();
                (expr, ctx)
            },
        ),
    ];

    for (name, (expr, ctx)) in cases {
        group.bench_with_input(
            criterion::BenchmarkId::from_parameter(name),
            &(),
            |b, _| {
                b.iter(|| black_box(expr.clone().eval(&ctx)).unwrap());
            },
        );
    }

    group.finish();
}

criterion_group!(
    float_benches,
    bench_float_literal,
    bench_float_attribute,
    bench_float_binary,
    bench_float_unary
);
criterion_main!(float_benches);
