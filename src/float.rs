use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, IfThenNode, SelectExprNodeImpl};
use crate::logic::BoolExpr;
use crate::num_cast::CastFrom;
use num_traits::Float;
use std::fmt::Debug;
use std::sync::Arc;

pub enum FloatExprNode<N: SelectExprNodeImpl<Property = N>> {
    Lit(N),
    Attribute(Path),
    Cast(Box<dyn ExprNode<N>>),
    UnaryOp {
        op: FloatUnaryOp,
        expr: Expr<N>,
    },
    BinaryOp {
        lhs_expr: Expr<N>,
        op: FloatBinaryOp,
        rhs_expr: Expr<N>,
    },
    TrinaryOp {
        value_expr: Expr<N>,
        op: FloatTrinaryOp,
        arg1_expr: Expr<N>,
        arg2_expr: Expr<N>,
    },
    IfThenOp {
        bool_expr: BoolExpr,
        arg1_expr: Expr<N>,
        arg2_expr: Expr<N>,
    },
    ErrorHandlingOp {
        expr: Expr<N>,
        or_expr: Expr<N>,
    },
}

impl<N> Into<Expr<N>> for FloatExprNode<N>
where
    N: Float + SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>> + Send + Sync + 'static,
{
    fn into(self) -> Expr<N> {
        Expr::new(Arc::new(self))
    }
}

impl<N> ExprNode<N> for FloatExprNode<N>
where
    N: Float + SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>> + Send + Sync + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        match self {
            FloatExprNode::Lit(lit) => Ok(*lit),
            FloatExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;

                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            FloatExprNode::Cast(cast) => cast.eval(ctx),
            FloatExprNode::UnaryOp { op, expr } => {
                let value = expr.eval_dyn(ctx)?;
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
            FloatExprNode::IfThenOp {
                bool_expr,
                arg1_expr,
                arg2_expr,
            } => {
                let bool_result = bool_expr.eval_dyn(ctx)?;
                if bool_result {
                    arg1_expr.eval_dyn(ctx)
                } else {
                    arg2_expr.eval_dyn(ctx)
                }
            }
            FloatExprNode::ErrorHandlingOp { expr, or_expr } => match expr.eval_dyn(ctx) {
                Ok(v) => Ok(v),
                Err(_) => or_expr.eval_dyn(ctx),
            },
        }
    }
}

impl<N> IfThenNode<N> for FloatExprNode<N>
where
    N: Float + SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>> + Send + Sync + 'static,
{
    fn if_then(bool_expr: BoolExpr, t: Expr<N>, f: Expr<N>) -> Self {
        FloatExprNode::IfThenOp {
            bool_expr,
            arg1_expr: t.into(),
            arg2_expr: f.into(),
        }
    }
}

impl<N> CastFrom<N> for FloatExprNode<N>
where
    N: SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>>,
{
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self {
        FloatExprNode::Cast(node)
    }
}

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

pub struct FloatSelector<N: SelectExprNodeImpl> {
    pub lhs: Expr<N>,
    pub op: BoolExpr,
    pub rhs: Expr<N>,
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::ExpressionError;
    use crate::test_utils::scopes::{DST, ERROR_SCOPE, SRC};
    use crate::test_utils::{Atk, Hp, MapContext};
    use std::ops::Neg;

    // Helper for float comparisons if needed
    fn assert_near(a: f32, b: f32) {
        assert!((a - b).abs() < 1e-6, "Left: {}, Right: {}", a, b);
    }

    #[test]
    fn test_float_unary_ops() {
        let mut ctx = MapContext::default();
        ctx.insert::<Hp>(SRC, 16.0);

        // Testing basic negation
        assert_eq!(Hp::get(SRC).neg().eval(&ctx).unwrap(), -16.0);

        // Testing Trig functions
        assert_near(Hp::get(SRC).sin().eval(&ctx).unwrap(), 16.0.sin());

        // Testing Math functions
        assert_eq!(Hp::get(SRC).sqrt().eval(&ctx).unwrap(), 4.0);
        assert_eq!(Hp::get(SRC).abs().eval(&ctx).unwrap(), 16.0);

        ctx.insert::<Atk>(SRC, 2.7);
        assert_eq!(Atk::get(SRC).floor().eval(&ctx).unwrap(), 2.0);
        assert_eq!(Atk::get(SRC).ceil().eval(&ctx).unwrap(), 3.0);
    }

    #[test]
    fn test_float_binary_ops_standard() {
        let mut ctx = MapContext::default();
        ctx.insert::<Hp>(DST, 150.0);
        ctx.insert::<Atk>(SRC, 50.0);

        // Basic operators (assuming operator overloading is implemented)
        assert_eq!((Hp::get(DST) + Atk::get(SRC)).eval(&ctx).unwrap(), 200.0);
        assert_eq!((Hp::get(DST) - Atk::get(SRC)).eval(&ctx).unwrap(), 100.0);
        assert_eq!((Hp::get(DST) * Atk::get(SRC)).eval(&ctx).unwrap(), 7500.0);
        assert_eq!((Hp::get(DST) / Atk::get(SRC)).eval(&ctx).unwrap(), 3.0);
        assert_eq!((Hp::get(DST) % Atk::get(SRC)).eval(&ctx).unwrap(), 0.0);
    }

    #[test]
    fn test_float_binary_ops_extended() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 2.0);
        ctx.insert::<Atk>(DST, 10.0);

        // Testing .pow() as a method on Expr<N>
        // 2.0 ^ 3.0 = 8.0
        assert_eq!(Atk::get(SRC).pow(3.0).eval(&ctx).unwrap(), 8.0);

        // Testing .min() and .max()
        assert_eq!(Atk::get(SRC).min(Atk::get(DST)).eval(&ctx).unwrap(), 2.0);
        assert_eq!(Atk::get(SRC).max(Atk::get(DST)).eval(&ctx).unwrap(), 10.0);

        // Fractional power (equivalent to sqrt)
        ctx.insert::<Hp>(SRC, 25.0);
        assert_near(Hp::get(SRC).pow(0.5).eval(&ctx).unwrap(), 5.0);
    }

    #[test]
    fn test_float_trinary_ops_clamp() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 150.0);
        ctx.insert::<Hp>(SRC, 50.0);

        // Hardcoded bounds
        assert_eq!(Atk::get(SRC).clamp(0.0, 100.0).eval(&ctx).unwrap(), 100.0);

        // Dynamic bounds using other expressions
        ctx.insert::<Atk>(DST, 20.0); // min
        ctx.insert::<Hp>(DST, 30.0);  // max
        // clamp 50 between 20 and 30 -> 30
        assert_eq!(Hp::get(SRC).clamp(Atk::get(DST), Hp::get(DST)).eval(&ctx).unwrap(), 30.0);
    }

    #[test]
    fn test_float_logic_and_errors() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 0.0);
        ctx.insert::<Atk>(DST, 10.0);

        // Division by zero in floats = Infinity
        let div_zero = Atk::get(DST) / Atk::get(SRC);
        assert_eq!(div_zero.eval(&ctx).unwrap(), f32::INFINITY);

        // Error Handling (Fallback)
        let expr = Atk::get(ERROR_SCOPE).unwrap_or(Atk::get(DST));
        assert_eq!(expr.eval(&ctx).unwrap(), 10.0);

        // Missing Attribute Error
        let missing = Atk::get(ERROR_SCOPE);
        assert_eq!(missing.eval(&ctx), Err(ExpressionError::MissingAttribute));
    }
}