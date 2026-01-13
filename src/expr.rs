use crate::context::EvalContext;
use crate::float::FloatExprNode;
use crate::integer::IntExprNode;
use num_traits::Num;
use std::error::Error;
use std::fmt::Debug;
use std::sync::Arc;

pub trait ExprNode<N: Num, Ctx: EvalContext>: Send + Sync {
    fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError>;
}

#[derive(Default, Debug)]
pub struct Expr<N: Num, Ctx: EvalContext, Nd: ExprNode<N, Ctx>> {
    pub inner: Arc<Nd>,
    phantom: std::marker::PhantomData<(N, Ctx)>,
}

impl<N: Num, Ctx: EvalContext, Nd: ExprNode<N, Ctx>> Expr<N, Ctx, Nd> {
    pub fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        self.inner.eval(ctx)
    }

    pub fn new(node: Arc<Nd>) -> Self {
        Expr {
            inner: node,
            phantom: Default::default(),
        }
    }
}

impl<N: Num, Ctx: EvalContext, Nd: ExprNode<N, Ctx>> Clone for Expr<N, Ctx, Nd> {
    fn clone(&self) -> Self {
        Expr::new(self.inner.clone())
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionError {
    AttributeNotFound,
    EmptyExpr,
    InvalidTypes,
    InvalidOperationNeg,
}

impl std::fmt::Display for ExpressionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionError::AttributeNotFound => {
                write!(
                    f,
                    "Attribute error: Failed to retrieve attribute from context"
                )
            }
            ExpressionError::EmptyExpr => {
                write!(f, "An Empty Expression was found.")
            }
            ExpressionError::InvalidTypes => {
                write!(f, "Invalid expression type.")
            }
            ExpressionError::InvalidOperationNeg => {
                write!(f, "Expression does not support negation.")
            }
        }
    }
}

impl Error for ExpressionError {}

#[macro_export]
macro_rules! impl_into_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $node:ident) => {
        impl<Ctx: EvalContext> From<$x> for Expr<$x, Ctx, $node<$x, Ctx>> {
            fn from(value: $x) -> Self {
                let value = Arc::new($node::Lit(value));
                Expr::new(value)
            }
        }
    };
    // Batch rule for multiple types mapping to the same Node
    ($node:ident: $($x:ty),+ $(,)?) => {
        $(
            $crate::impl_into_expr!(@impl $x, $node);
        )+
    };
}

impl_into_expr!(IntExprNode: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_into_expr!(FloatExprNode: f32, f64);

pub trait SelectExprNodeImpl {
    type Property: Num;
    type Node<Ctx: EvalContext>: ExprNode<Self::Property, Ctx>;
}

pub type SelectExprNode<T, Ctx> = <T as SelectExprNodeImpl>::Node<Ctx>;

#[macro_export]
macro_rules! impl_select_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $select:ident) => {
        impl SelectExprNodeImpl for $x {
            type Property = $x;
            type Node<Ctx: EvalContext> = $select<Self::Property, Ctx>;
        }
    };
    // Batch rule for multiple types mapping to the same Node
    ($select:ident: $($x:ty),+ $(,)?) => {
        $(
            $crate::impl_select_expr!(@impl $x, $select);
        )+
    };
}

impl_select_expr!(IntExprNode: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_select_expr!(FloatExprNode: f32, f64);

#[macro_export]
macro_rules! impl_math_expr_rhs_primitive {
    (@impl $x:ty, $select:ident) => {
        impl<Ctx: EvalContext> std::ops::Add<$x> for Expr<$x, Ctx, $select<$x, Ctx>> {
            type Output = Expr<$x, Ctx, $select<$x, Ctx>>;

            fn add(self, rhs: $x) -> Self::Output {
                self + Expr::new(Arc::new($select::Lit(rhs)))
            }
        }

        impl<Ctx: EvalContext> std::ops::Sub<$x> for Expr<$x, Ctx, $select<$x, Ctx>> {
            type Output = Expr<$x, Ctx, $select<$x, Ctx>>;

            fn sub(self, rhs: $x) -> Self::Output {
                self - Expr::new(Arc::new($select::Lit(rhs)))
            }
        }

        impl<Ctx: EvalContext> std::ops::Mul<$x> for Expr<$x, Ctx, $select<$x, Ctx>> {
            type Output = Expr<$x, Ctx, $select<$x, Ctx>>;

            fn mul(self, rhs: $x) -> Self::Output {
                self * Expr::new(Arc::new($select::Lit(rhs)))
            }
        }

        impl<Ctx: EvalContext> std::ops::Div<$x> for Expr<$x, Ctx, $select<$x, Ctx>> {
            type Output = Expr<$x, Ctx, $select<$x, Ctx>>;

            fn div(self, rhs: $x) -> Self::Output {
                self / Expr::new(Arc::new($select::Lit(rhs)))
            }
        }

        impl<Ctx: EvalContext> std::ops::Rem<$x> for Expr<$x, Ctx, $select<$x, Ctx>> {
            type Output = Expr<$x, Ctx, $select<$x, Ctx>>;

            fn rem(self, rhs: $x) -> Self::Output {
                self % Expr::new(Arc::new($select::Lit(rhs)))
            }
        }
    };
    ($select:ident: $($x:ty),+ $(,)?) => {
        $(
            $crate::impl_math_expr_rhs_primitive!(@impl $x, $select);
        )+
    };
}

impl_math_expr_rhs_primitive!(IntExprNode: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_math_expr_rhs_primitive!(FloatExprNode: f32, f64);

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::RetrieveAttribute;

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

    #[test]
    fn test_serialize() {
        let expr = LhsTest::expr();

        //let result = serde_json::to_string(&expr);
    }
}
