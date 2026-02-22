use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, IfThenNode, SelectExprNodeImpl};
use crate::logic::BoolExpr;
use crate::num_cast::CastFrom;
use num_traits::{CheckedNeg, PrimInt};

pub enum IntExprNode<N: SelectExprNodeImpl> {
    Lit(N),
    Attribute(Path),
    Cast(Box<dyn ExprNode<N>>),
    UnaryOp {
        op: IntUnaryOp,
        expr: Expr<N>,
    },
    BinaryOp {
        lhs_expr: Expr<N>,
        op: IntBinaryOp,
        rhs_expr: Expr<N>,
    },
    TrinaryOp {
        value_expr: Expr<N>,
        op: IntTrinaryOp,
        arg1_expr: Expr<N>,
        arg2_expr: Expr<N>,
    },
    IfThenElseOp {
        bool_expr: BoolExpr,
        arg1_expr: Expr<N>,
        arg2_expr: Expr<N>,
    },
    ErrorHandlingOp {
        expr: Expr<N>,
        or_expr: Expr<N>,
    },
}

impl<N> ExprNode<N> for IntExprNode<N>
where
    N: PrimInt + CheckedNeg + SelectExprNodeImpl<Property = N> + Send + Sync + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        match self {
            IntExprNode::Lit(lit) => Ok(*lit),
            IntExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;

                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            IntExprNode::Cast(cast) => cast.eval(ctx),
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
            IntExprNode::IfThenElseOp {
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
            IntExprNode::ErrorHandlingOp { expr, or_expr } => match expr.eval_dyn(ctx) {
                Ok(v) => Ok(v),
                Err(_) => or_expr.eval_dyn(ctx),
            },
        }
    }
}

impl<N> IfThenNode<N> for IntExprNode<N>
where
    N: PrimInt
        + CheckedNeg
        + SelectExprNodeImpl<Property = N, Node = IntExprNode<N>>
        + Send
        + Sync
        + 'static,
{
    fn if_then(bool_expr: BoolExpr, t: Expr<N>, f: Expr<N>) -> Self {
        IntExprNode::IfThenElseOp {
            bool_expr,
            arg1_expr: t.into(),
            arg2_expr: f.into(),
        }
    }
}

impl<N> CastFrom<N> for IntExprNode<N>
where
    N: SelectExprNodeImpl<Property = N, Node = IntExprNode<N>>,
{
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self {
        IntExprNode::Cast(node)
    }
}

#[macro_export]
macro_rules! impl_std_binary_ops {
    (
        $target:ident,
        $node_variant:ident,
        $op_enum:ident,
        [$($trait:ident => ($method:ident, $variant:ident)),* $(,)?]
    ) => {
        $(
            impl<N> std::ops::$trait for Expr<N>
            where
                N: BinaryExprOps + SelectExprNodeImpl<Property = N, Node = $target<N>> + Send + Sync,
                $target<N>: crate::expr::ExprNode<N>,
            {
                type Output = Self;

                fn $method(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::$variant,
                        rhs_expr,
                    }))
                }
            }
        )*
    };
}

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
    Min,
    Max,
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
            IntBinaryOp::Min => l.min(r),
            IntBinaryOp::Max => l.max(r),
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
    use std::ops::Neg;
    use crate::test_utils::{IntAtk, IntDef, IntHp, MapContext};
    use crate::test_utils::scopes::{DST, ERROR_SCOPE, SRC};
    use super::*;

    #[test]
    fn test_unary_ops() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntDef>(SRC, 150);

        let expr = IntDef::get(SRC).neg();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150);

        // Edge Case: Testing checked_neg overflow.
        // Note: For standard i32/i64, negating MIN causes an overflow.
        ctx.insert::<IntDef>(DST, i32::MIN);
        let overflow_expr = IntDef::get(DST).neg();

        let err_result = overflow_expr.eval(&ctx);
        assert_eq!(err_result, Err(ExpressionError::InvalidOperationNeg));
    }

    #[test]
    fn test_binary_ops_standard() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntHp>(DST, 150);
        ctx.insert::<IntAtk>(SRC, 40);

        assert_eq!(
            (IntHp::get(DST) + IntAtk::get(SRC)).eval(&ctx).unwrap(),
            190
        );
        assert_eq!(
            (IntHp::get(DST) - IntAtk::get(SRC)).eval(&ctx).unwrap(),
            110
        );
        assert_eq!(
            (IntHp::get(DST) * IntAtk::get(SRC)).eval(&ctx).unwrap(),
            6000
        );
        assert_eq!((IntHp::get(DST) / IntAtk::get(SRC)).eval(&ctx).unwrap(), 3);
        assert_eq!((IntHp::get(DST) % IntAtk::get(SRC)).eval(&ctx).unwrap(), 30);
    }

    #[test]
    fn test_int_binary_ops_extended() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntAtk>(SRC, 10);
        ctx.insert::<IntAtk>(DST, 20);

        // Testing Min: min(10, 20) -> 10
        let min_expr = IntAtk::get(SRC).min(IntAtk::get(DST));
        assert_eq!(min_expr.eval(&ctx).unwrap(), 10);

        // Testing Max: max(10, 20) -> 20
        let max_expr = IntAtk::get(SRC).max(IntAtk::get(DST));
        assert_eq!(max_expr.eval(&ctx).unwrap(), 20);

        // Testing Pow: 2 ^ 3 -> 8
        // Note: If your .pow() takes a literal, it likely uses Into<Expr<N>>
        ctx.insert::<IntDef>(SRC, 2);
        let pow_expr = IntDef::get(SRC).pow(3);
        assert_eq!(pow_expr.eval(&ctx).unwrap(), 8);
    }

    #[test]
    fn test_trinary_ops_clamp() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntDef>(DST, 150); // Value to clamp

        // Clamp 150 between 0 and 100 -> Should be 100
        let expr = IntDef::get(DST).clamp(0, 100);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 100);

        ctx.insert::<IntDef>(SRC, -50);
        // Clamp -50 between 0 and 100 -> Should be 0
        let expr_low = IntDef::get(SRC).clamp(0, 100);
        assert_eq!(expr_low.eval(&ctx).unwrap(), 0);
    }
    #[test]
    fn test_error_handling_op() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntAtk>(SRC, 999); // Fallback value

        let expr = IntAtk::get(ERROR_SCOPE).unwrap_or(IntAtk::get(SRC));

        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 999);
    }

    #[test]
    fn test_evaluation_errors() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntAtk>(SRC, 0);
        ctx.insert::<IntAtk>(DST, 1500);

        // Division by Zero
        let expr_div = IntAtk::get(DST) / IntAtk::get(SRC);
        assert_eq!(expr_div.eval(&ctx), Err(ExpressionError::DivisionByZero));

        // Missing Attribute
        let expr_missing = IntAtk::get(ERROR_SCOPE);
        assert_eq!(
            expr_missing.eval(&ctx),
            Err(ExpressionError::MissingAttribute)
        );
    }
}
