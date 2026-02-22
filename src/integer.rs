use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, IfThenNode, SelectExprNodeImpl};
use crate::logic::BoolExpr;
use crate::num_cast::CastFrom;
use num_traits::{CheckedNeg, PrimInt};
use std::sync::Arc;

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

impl<
    N: PrimInt
        + CheckedNeg
        + SelectExprNodeImpl<Property = N, Node = IntExprNode<N>>
        + Send
        + Sync
        + 'static,
> Expr<N>
{
    pub fn clamp(self, min: impl Into<Self>, max: impl Into<Self>) -> Self {
        Expr::new(Arc::new(IntExprNode::TrinaryOp {
            value_expr: self,
            op: IntTrinaryOp::Clamp,
            arg1_expr: min.into(),
            arg2_expr: max.into(),
        }))
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
    use crate::expr::ExpressionError;
    use crate::test_utils::scopes::{DST, ERROR_SCOPE, SRC};
    use crate::test_utils::{IntAtk, IntDef, IntHp, MapContext};
    use std::ops::Neg;

    #[test]
    fn test_unary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<IntDef>(SRC, 150);

        let expr = IntDef::get(SRC).neg();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150);
    }

    #[test]
    fn test_binary_ops() {
        let mut ctx = MapContext::default();
        // Destination
        ctx.insert::<IntHp>(DST, 150);

        // Source
        ctx.insert::<IntAtk>(SRC, 50);

        let expr = IntHp::get(DST) - IntAtk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 - 50);

        let expr = IntHp::get(DST) + IntAtk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 + 50);

        let expr = IntHp::get(DST) * IntAtk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 * 50);

        let expr = IntHp::get(DST) / IntAtk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 150 / 50);
    }

    #[test]
    fn test_cast_op() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<IntHp>(DST, 49);
        ctx.insert::<IntAtk>(SRC, 1500);

        let expr = IntHp::get(DST) + IntAtk::get(SRC);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500 + 49);

        let expr = IntAtk::get(SRC) - IntHp::get(DST);
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 1500 - 49);
    }

    #[test]
    fn test_error_ops() {
        let mut ctx = MapContext::default();
        // Source
        ctx.insert::<IntAtk>(SRC, 0);
        ctx.insert::<IntAtk>(DST, 1500);

        let expr = IntAtk::get(DST) / IntAtk::get(SRC); // Div by 0
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::DivisionByZero));

        let expr = IntAtk::get(ERROR_SCOPE);
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::MissingAttribute));
    }
}
