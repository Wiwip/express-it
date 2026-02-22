use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, IfThenNode, SelectExprNodeImpl};
use crate::logic::{BoolExpr};
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
    use crate::expr::ExpressionError;
    use crate::test_utils::scopes::{DST, ERROR_SCOPE, SRC};
    use crate::test_utils::{Atk, Hp, MapContext};
    use num_traits::Float;
    use std::ops::Neg;

    #[test]
    fn test_unary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<Hp>(SRC, 150.0);

        let expr = Hp::get(SRC).neg();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150.0);
    }

    #[test]
    fn test_binary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<Hp>(DST, 150.0);

        // Source
        ctx.insert::<Atk>(SRC, 50.0);

        let expr = Hp::get(DST) - Atk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 - 50.0);

        let expr = Hp::get(DST) + Atk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 + 50.0);

        let expr = Hp::get(DST) * Atk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 * 50.0);

        let expr = Hp::get(DST) / Atk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0 / 50.0);
    }

    #[test]
    fn test_trig_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<Hp>(DST, 150.0);

        // Source
        ctx.insert::<Atk>(SRC, 50.0);

        let expr = Hp::get(DST).sin() + Atk::get(SRC).cos();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150.0.sin() + 50.0.cos());
    }

    #[test]
    fn test_cast_op() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<Hp>(DST, 49.0);
        ctx.insert::<Atk>(SRC, 1500.0);

        let expr = Hp::get(DST) + Atk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500.0 + 49.0);

        let expr = Atk::get(SRC) - Hp::get(DST);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500.0 - 49.0);
    }

    #[test]
    fn test_error_ops() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<Atk>(SRC, 0.0);
        ctx.insert::<Atk>(DST, 1500.0);

        let expr = Atk::get(DST) / Atk::get(SRC); // Div by 0
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Ok(f32::INFINITY));

        let expr = Atk::get(ERROR_SCOPE);
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::MissingAttribute));
    }
}
