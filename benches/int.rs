use criterion::{criterion_group, criterion_main, Criterion};
use express_it::test_utils::{scopes, IntHp, MapContext};
use std::hint::black_box;

fn bench_int_attribute(c: &mut Criterion) {
    let expr = IntHp::get(scopes::SRC);
    let mut ctx = MapContext::default();
    ctx.insert::<IntHp>(scopes::SRC, 10);

    c.bench_function("int_attribute_eval", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

criterion_group!(int_benches, bench_int_attribute);
criterion_main!(int_benches);
