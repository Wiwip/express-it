use crate::expr::ExpressionError;
use crate::numeric::{BinaryOpEval, NumExprKind, NumericExprNode, TrinaryOpEval, UnaryOpEval};
use num_traits::{CheckedNeg, PrimInt};

pub type IntExprNode<N, S> = NumericExprNode<N, S, IntKind>;

/// Selector that makes integer expressions use checked arithmetic operators.
pub struct IntKind;
impl<N: PrimInt + CheckedNeg + Send + Sync + 'static> NumExprKind<N> for IntKind {
    type UnaryOp = IntUnaryOp;
    type BinaryOp = IntBinaryOp;
    type TrinaryOp = IntTrinaryOp;
}

#[derive(Debug, Clone, Copy)]
pub enum IntUnaryOp {
    Neg,
}

impl<N: PrimInt + CheckedNeg> UnaryOpEval<N> for IntUnaryOp {
    fn eval(&self, value: N) -> Result<N, ExpressionError> {
        match self {
            IntUnaryOp::Neg => value
                .checked_neg()
                .ok_or(ExpressionError::InvalidOperationNeg),
        }
    }
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

impl<N: PrimInt + CheckedNeg> BinaryOpEval<N> for IntBinaryOp {
    fn eval(&self, l: N, r: N) -> Result<N, ExpressionError> {
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

impl<N: PrimInt + CheckedNeg> TrinaryOpEval<N> for IntTrinaryOp {
    fn eval(&self, val: N, arg1: N, arg2: N) -> Result<N, ExpressionError> {
        let result = match self {
            IntTrinaryOp::Clamp => val.clamp(arg1, arg2),
        };
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::scopes::{DST, SRC};
    use crate::test_utils::{IntAtk, IntDef, IntHp, MapContext};
    use std::ops::Neg;

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

        let expr = IntAtk::get("nothing").unwrap_or(IntAtk::get(SRC));

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

        // Missing value
        let expr_missing = IntAtk::get("nothing");
        assert_eq!(
            expr_missing.eval(&ctx),
            Err(ExpressionError::MissingValue)
        );
    }
}
