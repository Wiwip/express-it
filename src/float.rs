use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::impl_float_binary_ops;
use crate::logic::{BoolExpr, BoolExprNode, Compare, CompareExpr, ComparisonOp};
use crate::num_cast::CastFrom;
use num_traits::Float;
use std::fmt::Debug;
use std::sync::Arc;

pub type FloatExpr<N> = Expr<N, FloatExprNode<N>>;

impl<N> CompareExpr for FloatExpr<N>
where
    N: Float + 'static,
{
    fn compare(self, op: ComparisonOp, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }
}

pub enum FloatExprNode<N> {
    Lit(N),
    Attribute(Path),
    Cast(Box<dyn ExprNode<N>>),
    UnaryOp {
        op: FloatUnaryOp,
        expr: FloatExpr<N>,
    },
    BinaryOp {
        lhs_expr: FloatExpr<N>,
        op: FloatBinaryOp,
        rhs_expr: FloatExpr<N>,
    },
    TrinaryOp {
        value_expr: FloatExpr<N>,
        op: FloatTrinaryOp,
        arg1_expr: FloatExpr<N>,
        arg2_expr: FloatExpr<N>,
    },
}

macro_rules! float_unary {
    ($($name:ident => $op:ident),* $(,)?) => {
        $(
            pub fn $name(self) -> Self {
                self.unary_expr(FloatUnaryOp::$op)
            }
        )*
    };
}

impl<N: Float + 'static> Expr<N, FloatExprNode<N>> {
    fn unary_expr(self, op: FloatUnaryOp) -> Self {
        Expr::new(Arc::new(FloatExprNode::UnaryOp { op, expr: self }))
    }

    float_unary! {
        neg => Neg,
        abs => Abs,
        acos => Acos,
        asin => Asin,
        cos => Cos,
        sin => Sin,
        tan => Tan,
        atan => Atan,
        floor => Floor,
        ceil => Ceil,
        exp => Exp,
        ln => Ln,
        log10 => Log10,
        log2 => Log2,
        sqrt => Sqrt,
        cbrt => Cbrt,
    }
}

impl<N: Float + 'static> Into<Expr<N, FloatExprNode<N>>> for FloatExprNode<N> {
    fn into(self) -> Expr<N, FloatExprNode<N>> {
        Expr::new(Arc::new(self))
    }
}

impl<N: Float + 'static> ExprNode<N> for FloatExprNode<N> {
    fn eval(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        match self {
            FloatExprNode::Lit(lit) => Ok(lit.clone()),
            FloatExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;

                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            FloatExprNode::Cast(cast) => Ok(cast.eval(ctx)?),
            FloatExprNode::UnaryOp { op, expr } => {
                let value = expr.inner.eval(ctx)?;
                op.eval(value)
            }
            FloatExprNode::BinaryOp {
                lhs_expr: lhs,
                op,
                rhs_expr: rhs,
            } => {
                let l = lhs.eval_dyn(ctx)?;
                let r = rhs.eval_dyn(ctx)?;
                op.eval(l, r)
            }
            FloatExprNode::TrinaryOp {
                value_expr,
                op,
                arg1_expr,
                arg2_expr,
            } => {
                let value = value_expr.eval_dyn(ctx)?;
                let arg1 = arg1_expr.eval_dyn(ctx)?;
                let arg2 = arg2_expr.eval_dyn(ctx)?;
                op.eval(value, arg1, arg2)
            }
        }
    }
}

impl<N> CastFrom<N> for FloatExprNode<N> {
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self {
        FloatExprNode::Cast(node)
    }
}

impl<N> std::ops::Neg for FloatExpr<N>
where
    N: Float + Send + Sync + 'static,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        Expr::new(Arc::new(FloatExprNode::UnaryOp {
            op: FloatUnaryOp::Neg,
            expr: self,
        }))
    }
}

impl_float_binary_ops!(
    FloatExprNode,
    BinaryOp,
    FloatBinaryOp,
    [
        Add => (add, Add),
        Sub => (sub, Sub),
        Mul => (mul, Mul),
        Div => (div, Div),
        Rem => (rem, Rem)
    ]
);

#[derive(Debug, Clone, Copy)]
pub enum FloatConditionOp {
    IsNan,
    IsInfinity,
}

#[derive(Debug, Clone, Copy)]
pub enum FloatUnaryOp {
    Neg,
    Abs,
    Acos,
    Asin,
    Cos,
    Sin,
    Tan,
    Atan,
    Floor,
    Ceil,
    Exp,
    Ln,
    Log10,
    Log2,
    Sqrt,
    Cbrt,
}

impl FloatUnaryOp {
    fn eval<N: Float>(&self, value: N) -> Result<N, ExpressionError> {
        let result = match self {
            FloatUnaryOp::Sin => value.sin(),
            FloatUnaryOp::Asin => value.asin(),
            FloatUnaryOp::Cos => value.cos(),
            FloatUnaryOp::Acos => value.acos(),
            FloatUnaryOp::Neg => value.neg(),
            FloatUnaryOp::Tan => value.tan(),
            FloatUnaryOp::Atan => value.atan(),
            FloatUnaryOp::Abs => value.abs(),
            FloatUnaryOp::Floor => value.floor(),
            FloatUnaryOp::Ceil => value.ceil(),
            FloatUnaryOp::Exp => value.exp(),
            FloatUnaryOp::Ln => value.ln(),
            FloatUnaryOp::Log10 => value.log10(),
            FloatUnaryOp::Log2 => value.log2(),
            FloatUnaryOp::Sqrt => value.sqrt(),
            FloatUnaryOp::Cbrt => value.cbrt(),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FloatBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Min,
    Max,
}

impl FloatBinaryOp {
    fn eval<N: Float>(&self, l: N, r: N) -> Result<N, ExpressionError> {
        let result = match self {
            FloatBinaryOp::Add => l + r,
            FloatBinaryOp::Sub => l - r,
            FloatBinaryOp::Mul => l * r,
            FloatBinaryOp::Div => l / r,
            FloatBinaryOp::Rem => l % r,
            FloatBinaryOp::Pow => l.powf(r),
            FloatBinaryOp::Min => l.min(r),
            FloatBinaryOp::Max => l.max(r),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FloatTrinaryOp {
    Clamp,
}

impl FloatTrinaryOp {
    fn eval<N: Float>(&self, val: N, arg1: N, arg2: N) -> Result<N, ExpressionError> {
        let result = match self {
            FloatTrinaryOp::Clamp => val.clamp(arg1, arg2),
        };
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::ExpressionError;
    use crate::test_utils::scopes::{DST, SRC};
    use crate::test_utils::{F32Attribute, I32Attribute, MapContext};
    use num_traits::Float;

    #[test]
    fn test_unary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<F32Attribute>(DST, 150.0);

        let expr = F32Attribute::dst().neg();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150.0);
    }

    #[test]
    fn test_binary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<F32Attribute>(DST, 150.0);

        // Source
        ctx.insert::<F32Attribute>(SRC, 50.0);

        let expr = F32Attribute::dst() - F32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 - 50.0);

        let expr = F32Attribute::dst() + F32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 + 50.0);

        let expr = F32Attribute::dst() * F32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 * 50.0);

        let expr = F32Attribute::dst() / F32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 / 50.0);
    }

    #[test]
    fn test_trig_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<F32Attribute>(DST, 150.0);

        // Source
        ctx.insert::<F32Attribute>(SRC, 50.0);

        let expr = F32Attribute::dst().sin() + F32Attribute::src().cos();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0.sin() + 50.0.cos());
    }

    #[test]
    fn test_cast_op() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<I32Attribute>(SRC, 49);
        ctx.insert::<F32Attribute>(SRC, 1500.0);

        let expr = F32Attribute::src() + I32Attribute::src().as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500.0 + 49.0);

        let expr = F32Attribute::src() - I32Attribute::src().as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500.0 - 49.0);
    }

    #[test]
    fn test_error_ops() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<I32Attribute>(SRC, 0);
        ctx.insert::<F32Attribute>(SRC, 1500.0);

        let expr = F32Attribute::src() / I32Attribute::src().as_();
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Ok(f32::INFINITY));

        let expr = F32Attribute::dst();
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::MissingAttribute));
    }
}
