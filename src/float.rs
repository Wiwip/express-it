use crate::expr::{Expr, ExprSchema, ExpressionError, SelectExprNodeImpl};
use crate::logic::BoolExpr;
use crate::numeric::{BinaryOpEval, NumExprKind, NumericExprNode, TrinaryOpEval, UnaryOpEval};
use num_traits::Float;
use std::fmt::Debug;

pub type FloatExprNode<N, S> = NumericExprNode<N, S, FloatKind>;

/// Selector helpers for math functions on the float expression type.
pub struct FloatKind;
impl<N: Float + Send + Sync + 'static> NumExprKind<N> for FloatKind {
    type UnaryOp = FloatUnaryOp;
    type BinaryOp = FloatBinaryOp;
    type TrinaryOp = FloatTrinaryOp;
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

impl<N: Float> UnaryOpEval<N> for FloatUnaryOp {
    fn eval(&self, value: N) -> Result<N, ExpressionError> {
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

impl<N: Float> BinaryOpEval<N> for FloatBinaryOp {
    fn eval(&self, l: N, r: N) -> Result<N, ExpressionError> {
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

impl<N: Float> TrinaryOpEval<N> for FloatTrinaryOp {
    fn eval(&self, val: N, arg1: N, arg2: N) -> Result<N, ExpressionError> {
        let result = match self {
            FloatTrinaryOp::Clamp => val.clamp(arg1, arg2),
        };
        Ok(result)
    }
}

pub struct FloatSelector<N: SelectExprNodeImpl<S>, S: ExprSchema> {
    pub lhs: Expr<N, S>,
    pub op: BoolExpr<S>,
    pub rhs: Expr<N, S>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::ExpressionError;
    use crate::test_utils::scopes::{DST, SRC};
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
        ctx.insert::<Hp>(DST, 30.0); // max
        // clamp 50 between 20 and 30 -> 30
        assert_eq!(
            Hp::get(SRC)
                .clamp(Atk::get(DST), Hp::get(DST))
                .eval(&ctx)
                .unwrap(),
            30.0
        );
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
        let expr = Atk::get("nothing").unwrap_or(Atk::get(DST));
        assert_eq!(expr.eval(&ctx).unwrap(), 10.0);

        // Missing value error
        let missing = Atk::get("nothing");
        assert_eq!(missing.eval(&ctx), Err(ExpressionError::MissingValue));
    }
}
