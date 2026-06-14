use std::any::Any;
use std::hint::black_box;
use std::sync::Arc;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};

use express_it::context::ReadContext;
use express_it::float::{FloatBinaryOp, FloatExprNode, FloatUnaryOp};
use express_it::{
    context::Path,
    expr::{Expr, ExprSchema, ExpressionError},
};

struct BenchSchema;
impl ExprSchema for BenchSchema {
    type Context<'w, 's>
        = BenchContext
    where
        's: 'w;
}

#[derive(Default)]
struct BenchContext {
    atk: Option<Box<f32>>,
    def: Option<Box<f32>>,
}

impl ReadContext for BenchContext {
    fn get_any(&self, path: &Path) -> Result<&dyn Any, ExpressionError> {
        if path.as_str().contains("Atk") {
            self.atk
                .as_ref()
                .map(|v| v.as_ref() as &dyn Any)
                .ok_or(ExpressionError::MissingValue)
        } else {
            self.def
                .as_ref()
                .map(|v| v.as_ref() as &dyn Any)
                .ok_or(ExpressionError::MissingValue)
        }
    }
}

fn build_add_expr() -> Expr<f32, BenchSchema> {
    let lhs = Assignment {
        path: Path::new("src.Atk.val"),
        expr: Expr::new(Arc::new(FloatExprNode::Lit(10.0))),
    };
    let rhs = Assignment {
        path: Path::new("src.Def.val"),
        expr: Expr::new(Arc::new(FloatExprNode::Lit(3.0))),
    };

    Expr::new(Arc::new(FloatExprNode::BinaryOp {
        lhs_expr: lhs.expr,
        op: FloatBinaryOp::Add,
        rhs_expr: rhs.expr,
    }))
}

fn bench_float_binary_add(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_binary");
    group.measurement_time(std::time::Duration::from_secs(10));

    let expr = build_add_expr();
    let ctx = BenchContext {
        atk: Some(Box::new(10.0)),
        def: Some(Box::new(3.0)),
    };

    group.bench_function("add", |b| b.iter(|| black_box(expr.eval(&ctx)).unwrap()));

    group.finish();
}

use express_it::frame::Assignment;

fn bench_float_add_pure(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_add_pure");
    group.measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("add", |b| b.iter(|| black_box(10.0_f32 + 3.0_f32)));

    group.finish();
}

fn bench_float_lit(c: &mut Criterion) {
    let ctx = BenchContext::default();
    let expr: Expr<f32, BenchSchema> =
        Expr::new(black_box(std::sync::Arc::new(FloatExprNode::Lit(42.0))));

    c.bench_function("float_literal_eval", |b| {
        b.iter(|| black_box(expr.eval(&ctx)).unwrap())
    });
}

fn bench_float_unary(c: &mut Criterion) {
    let mut group = c.benchmark_group("float_unary");

    let cases = [
        ("lit_neg", {
            let expr: Expr<f32, BenchSchema> = Expr::new(Arc::new(FloatExprNode::UnaryOp {
                op: FloatUnaryOp::Neg,
                expr: Expr::new(Arc::new(FloatExprNode::Lit(9.0))),
            }));
            let ctx = BenchContext::default();
            (expr, ctx)
        }),
        ("lit_abs", {
            let expr: Expr<f32, BenchSchema> = Expr::new(Arc::new(FloatExprNode::UnaryOp {
                op: FloatUnaryOp::Abs,
                expr: Expr::new(Arc::new(FloatExprNode::Lit(9.0))),
            }));
            let ctx = BenchContext::default();
            (expr, ctx)
        }),
        ("lit_sqrt", {
            let expr: Expr<f32, BenchSchema> = Expr::new(Arc::new(FloatExprNode::UnaryOp {
                op: FloatUnaryOp::Sqrt,
                expr: Expr::new(Arc::new(FloatExprNode::Lit(9.0))),
            }));
            let ctx = BenchContext::default();
            (expr, ctx)
        }),
    ];

    for (name, (expr, ctx)) in cases {
        group.bench_with_input(BenchmarkId::from_parameter(name), &(), |b, _| {
            b.iter(|| black_box(expr.eval(&ctx)).unwrap())
        });
    }

    group.finish();
}

criterion_group!(
    float_benches,
    bench_float_add_pure,
    bench_float_lit,
    bench_float_unary,
    bench_float_binary_add,
);
criterion_main!(float_benches);
