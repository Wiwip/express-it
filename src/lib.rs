pub mod context;
pub mod expr;
pub mod float;
pub mod integer;
pub mod macros;
pub mod logic;

#[cfg(test)]
mod test_utils;

#[cfg(test)]
mod tests {
    use crate::context::{EvalContext, RetrieveAttribute};
    use crate::expr::{Expr, ExpressionError};
    use crate::float::{FloatExpr, FloatExprNode};
    use crate::integer::{IntExpr, IntExprNode};
    use num_traits::Num;
    use std::ops::Neg;
    use std::sync::Arc;

    #[derive(Copy, Clone)]
    struct DummyContext {
        lhs_value: f32,
        rhs_value: f32,
        signed_int_value: i32,
        unsigned_int_value: u32,
    }

    impl DummyContext {
        fn new(lhs: f32, rhs: f32, signed: i32, unsigned: u32) -> Self {
            Self {
                lhs_value: lhs,
                rhs_value: rhs,
                signed_int_value: signed,
                unsigned_int_value: unsigned,
            }
        }
    }

    impl EvalContext for DummyContext {
        fn get<N: Num>(&self, attribute: &Box<dyn RetrieveAttribute<N, Self>>) -> N {
            attribute.retrieve(self).unwrap()
        }
    }

    #[derive(Debug)]
    struct LhsTest;

    impl LhsTest {
        fn expr() -> Expr<f32, DummyContext, FloatExprNode<f32, DummyContext>> {
            Expr::new(Arc::new(FloatExprNode::Attribute(Box::new(LhsTest))))
        }
    }

    impl RetrieveAttribute<f32, DummyContext> for LhsTest {
        fn retrieve(&self, ctx: &DummyContext) -> Result<f32, ExpressionError> {
            Ok(ctx.lhs_value)
        }
    }

    #[derive(Debug)]
    struct RhsTest;

    impl RhsTest {
        fn expr() -> Expr<f32, DummyContext, FloatExprNode<f32, DummyContext>> {
            Expr::new(Arc::new(FloatExprNode::Attribute(Box::new(RhsTest))))
        }
    }

    impl RetrieveAttribute<f32, DummyContext> for RhsTest {
        fn retrieve(&self, ctx: &DummyContext) -> Result<f32, ExpressionError> {
            Ok(ctx.rhs_value)
        }
    }

    #[derive(Debug)]
    struct IIntTest;

    impl IIntTest {
        fn expr() -> Expr<i32, DummyContext, IntExprNode<i32, DummyContext>> {
            Expr::new(Arc::new(IntExprNode::Attribute(Box::new(IIntTest))))
        }
    }

    impl RetrieveAttribute<i32, DummyContext> for IIntTest {
        fn retrieve(&self, ctx: &DummyContext) -> Result<i32, ExpressionError> {
            Ok(ctx.signed_int_value)
        }
    }

    #[derive(Debug)]
    struct UIntTest;

    impl UIntTest {
        fn expr() -> Expr<u32, DummyContext, IntExprNode<u32, DummyContext>> {
            Expr::new(Arc::new(IntExprNode::Attribute(Box::new(UIntTest))))
        }
    }

    impl RetrieveAttribute<u32, DummyContext> for UIntTest {
        fn retrieve(&self, ctx: &DummyContext) -> Result<u32, ExpressionError> {
            Ok(ctx.unsigned_int_value)
        }
    }

    const LHS_FLOAT_VAL: f32 = 100.0;
    const RHS_FLOAT_VAL: f32 = 10.0;
    const IINT_VAL: i32 = 42;
    const UINT_VAL: u32 = 888;
    const LITERAL_CONSTANT: f32 = 1337.0;



    #[test]
    fn test_int_to_float_conversion() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        // Test conversion of an integer literal to float
        let int_expr: IntExpr<i32, DummyContext> = 42i32.into();
        let float_expr: FloatExpr<f32, DummyContext> = int_expr.as_();
        let result = float_expr.eval(&ctx).unwrap();
        assert_eq!(42.0f32, result);

        // Test conversion of an integer expression to float
        let int_lhs: IntExpr<i32, DummyContext> = 10i32.into();
        let int_rhs: IntExpr<i32, DummyContext> = 5i32.into();
        let int_sum = int_lhs + int_rhs;
        let float_sum: FloatExpr<f32, DummyContext> = int_sum.as_();
        let result = float_sum.eval(&ctx).unwrap();
        assert_eq!(15.0f32, result);

        // Test conversion with different integer types
        let int_i32: IntExpr<i32, DummyContext> = (-100i32).into();
        let float_from_i32: FloatExpr<f64, DummyContext> = int_i32.as_();
        let result = float_from_i32.eval(&ctx).unwrap();
        assert_eq!(-100.0f64, result);
    }

    #[test]
    fn test_attribute_getters() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        // Test LHS attribute retrieval
        let lhs_attr: Box<dyn RetrieveAttribute<f32, DummyContext>> = Box::new(LhsTest);
        let lhs_result = lhs_attr.retrieve(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL, lhs_result);

        // Test RHS attribute retrieval
        let rhs_attr: Box<dyn RetrieveAttribute<f32, DummyContext>> = Box::new(RhsTest);
        let rhs_result = rhs_attr.retrieve(&ctx).unwrap();
        assert_eq!(RHS_FLOAT_VAL, rhs_result);

        // Test attribute retrieval through context
        let lhs_via_ctx = ctx.get(&lhs_attr);
        assert_eq!(LHS_FLOAT_VAL, lhs_via_ctx);

        let rhs_via_ctx = ctx.get(&rhs_attr);
        assert_eq!(RHS_FLOAT_VAL, rhs_via_ctx);
    }

    #[test]
    fn test_attribute_math_operations() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        // Test addition with attributes
        let result = (LhsTest::expr() + RhsTest::expr()).eval(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL + RHS_FLOAT_VAL, result);

        // Test subtraction with attributes
        let result = (LhsTest::expr() - RhsTest::expr()).eval(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL - RHS_FLOAT_VAL, result);

        // Test multiplication with attributes
        let result = (LhsTest::expr() * RhsTest::expr()).eval(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL * RHS_FLOAT_VAL, result);

        // Test division with attributes
        let result = (LhsTest::expr() / RhsTest::expr()).eval(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL / RHS_FLOAT_VAL, result);

        // Test remainder with attributes
        let result = (LhsTest::expr() % RhsTest::expr()).eval(&ctx).unwrap();
        assert_eq!(LHS_FLOAT_VAL % RHS_FLOAT_VAL, result);
    }

    #[test]
    fn mixed_ops() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        // Test addition with attributes
        let result = (LhsTest::expr() + IIntTest::expr().as_() + LITERAL_CONSTANT)
            .eval(&ctx)
            .unwrap();
        assert_eq!(LHS_FLOAT_VAL + IINT_VAL as f32 + LITERAL_CONSTANT, result);
    }

    #[test]
    fn cast_ops() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        // Test addition with attributes
        let result = (LhsTest::expr() + RhsTest::expr() + IIntTest::expr().as_())
            .eval(&ctx)
            .unwrap();
        assert_eq!(LHS_FLOAT_VAL + RHS_FLOAT_VAL + IINT_VAL as f32, result);
    }

    #[test]
    fn neg_ops() {
        let ctx = DummyContext::new(LHS_FLOAT_VAL, RHS_FLOAT_VAL, IINT_VAL, UINT_VAL);

        let result = LhsTest::expr().neg().eval(&ctx).unwrap();
        assert_eq!(-LHS_FLOAT_VAL, result);

        let result = RhsTest::expr().neg().eval(&ctx).unwrap();
        assert_eq!(-RHS_FLOAT_VAL, result);

        let result = IIntTest::expr().neg().eval(&ctx).unwrap();
        assert_eq!(-IINT_VAL, result);

        let result = UIntTest::expr().neg().eval(&ctx);
        assert_eq!(Err(ExpressionError::InvalidOperationNeg), result);
    }
}
