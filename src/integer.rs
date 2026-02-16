use crate::context::{AttributeKey, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::impl_int_binary_ops;
use crate::logic::{BoolExpr, BoolExprNode, Compare, CompareExpr, ComparisonOp};
use crate::num_cast::CastFrom;
use num_traits::{AsPrimitive, CheckedNeg, PrimInt};
use std::sync::Arc;

pub type IntExpr<N> = Expr<N, IntExprNode<N>>;

impl<N> CompareExpr for IntExpr<N>
where
    N: PrimInt + CheckedNeg + 'static,
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

pub enum IntExprNode<N> {
    Lit(N),
    Attribute(AttributeKey),
    Cast(Box<dyn ExprNode<N>>),
    UnaryOp {
        op: IntUnaryOp,
        expr: IntExpr<N>,
    },
    BinaryOp {
        lhs_expr: IntExpr<N>,
        op: IntBinaryOp,
        rhs_expr: IntExpr<N>,
    },
    TrinaryOp {
        value_expr: IntExpr<N>,
        op: IntTrinaryOp,
        arg1_expr: IntExpr<N>,
        arg2_expr: IntExpr<N>,
    },
}

impl<N> ExprNode<N> for IntExprNode<N>
where
    N: PrimInt + CheckedNeg + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        match self {
            IntExprNode::Lit(lit) => Ok(lit.clone()),
            IntExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;

                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            IntExprNode::Cast(cast) => Ok(cast.eval(ctx)?),
            IntExprNode::UnaryOp { op, expr } => match op {
                IntUnaryOp::Neg => expr
                    .eval_dyn(ctx)?
                    .checked_neg()
                    .ok_or(ExpressionError::InvalidOperationNeg),
            },
            IntExprNode::BinaryOp {
                lhs_expr,
                op,
                rhs_expr,
            } => {
                let lhs = lhs_expr.eval_dyn(ctx)?;
                let rhs = rhs_expr.eval_dyn(ctx)?;
                op.eval(lhs, rhs)
            }
            IntExprNode::TrinaryOp {
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

impl<N: PrimInt + CheckedNeg + Send + Sync + 'static> Expr<N, IntExprNode<N>> {
    fn binary_expr(self, op: IntBinaryOp, rhs: Self) -> Self {
        Expr::new(Arc::new(IntExprNode::BinaryOp {
            lhs_expr: self,
            op,
            rhs_expr: rhs,
        }))
    }

    pub fn pow(self, rhs: Self) -> Self {
        self.binary_expr(IntBinaryOp::Pow, rhs)
    }

    pub fn clamp(self, min: impl Into<Self>, max: impl Into<Self>) -> Self {
        Expr::new(Arc::new(IntExprNode::TrinaryOp {
            value_expr: self,
            op: IntTrinaryOp::Clamp,
            arg1_expr: min.into(),
            arg2_expr: max.into(),
        }))
    }
}

impl<N> CastFrom<N> for IntExprNode<N> {
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self {
        IntExprNode::Cast(node)
    }
}

impl<N> std::ops::Neg for Expr<N, IntExprNode<N>>
where
    N: PrimInt + CheckedNeg + Send + Sync + 'static,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        Expr::new(Arc::new(IntExprNode::UnaryOp {
            op: IntUnaryOp::Neg,
            expr: self,
        }))
    }
}

impl_int_binary_ops!(
    IntExprNode,
    BinaryOp,
    IntBinaryOp,
    [
        Add => (add, Add),
        Sub => (sub, Sub),
        Mul => (mul, Mul),
        Div => (div, Div),
        Rem => (rem, Rem)
    ]
);

#[derive(Debug, Clone, Copy)]
pub enum IntUnaryOp {
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum IntBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl IntBinaryOp {
    fn eval<N: PrimInt + CheckedNeg>(&self, l: N, r: N) -> Result<N, ExpressionError> {
        let result = match self {
            IntBinaryOp::Add => l + r,
            IntBinaryOp::Sub => l - r,
            IntBinaryOp::Mul => l * r,
            IntBinaryOp::Div => l.checked_div(&r).ok_or(ExpressionError::DivisionByZero)?,
            IntBinaryOp::Rem => l % r,
            IntBinaryOp::Pow => l.pow(r.to_u32().ok_or(ExpressionError::InvalidTypes)?),
        };
        Ok(result)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IntTrinaryOp {
    Clamp,
}

impl IntTrinaryOp {
    fn eval<N: PrimInt + CheckedNeg>(
        &self,
        val: N,
        arg1: N,
        arg2: N,
    ) -> Result<N, ExpressionError> {
        let result = match self {
            IntTrinaryOp::Clamp => val.clamp(arg1, arg2),
        };
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::ExpressionError;
    use crate::test_utils::{F32Attribute, I32Attribute, MapContext};
    use std::ops::Neg;

    #[test]
    fn test_unary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert_dst::<I32Attribute>(150);

        let expr = I32Attribute::dst().neg();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150);
    }

    #[test]
    fn test_binary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert_dst::<I32Attribute>(150);
        // Source
        ctx.insert_src::<I32Attribute>(50);

        let expr = I32Attribute::dst() - I32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 - 50);

        let expr = I32Attribute::dst() + I32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 + 50);

        let expr = I32Attribute::dst() * I32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 * 50);

        let expr = I32Attribute::dst() / I32Attribute::src();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 / 50);
    }

    #[test]
    fn test_trinary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert_dst::<I32Attribute>(150);
        // Source
        ctx.insert_src::<I32Attribute>(50);

        let expr = I32Attribute::dst().clamp(0, 100);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 100);

        let expr = I32Attribute::src().clamp(0, 100);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 50);
    }

    #[test]
    fn test_cast_op() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert_src::<F32Attribute>(49.0);
        ctx.insert_src::<I32Attribute>(1500);

        let expr = I32Attribute::src() + F32Attribute::src().as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500 + 49);

        let expr = I32Attribute::src() - F32Attribute::src().as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500 - 49);
    }

    #[test]
    fn test_error_ops() {
        let mut ctx = MapContext::default();
        ctx.insert_src::<I32Attribute>(1500);
        ctx.insert_dst::<I32Attribute>(0);

        let expr = I32Attribute::src() / I32Attribute::dst();
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::DivisionByZero));

        let expr = F32Attribute::dst();
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::MissingAttribute));
    }
}
