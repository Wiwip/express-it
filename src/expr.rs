use crate::context::EvalContext;
use crate::float::FloatExprNode;
use crate::integer::IntExprNode;
use crate::num_cast::{CastFrom, CastNumPrimitive};
use num_traits::{AsPrimitive, Num};
use std::error::Error;
use std::fmt::Debug;
use std::sync::Arc;

pub trait ExprNode<N> {
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError>;
}

#[derive(Default, Debug)]
pub struct Expr<N, Nd> {
    pub inner: Arc<Nd>,
    phantom: std::marker::PhantomData<N>,
}

impl<N, Nd: ExprNode<N>> Expr<N, Nd> {
    pub fn new(node: Arc<Nd>) -> Self {
        Expr {
            inner: node,
            phantom: Default::default(),
        }
    }
}

impl<N, Nd> Expr<N, Nd>
where
    N: Copy + Send + Sync + 'static,
    Nd: ExprNode<N> + 'static,
{
    pub fn as_<NOut, NdOut: ExprNode<NOut>>(&self) -> Expr<NOut, NdOut>
    where
        NOut: Num + Copy + Send + Sync + 'static,
        N: AsPrimitive<NOut>,
        NdOut: ExprNode<NOut> + CastFrom<NOut>,
    {
        let cast_node = CastNumPrimitive::new(self.clone());
        let expr_node = NdOut::cast_from(Box::new(cast_node));
        Expr::new(Arc::new(expr_node))
    }

    pub fn eval<Ctx: EvalContext>(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        self.inner.eval_node(ctx)
    }
    
    pub fn eval_dyn(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError> {
        self.inner.eval_node(ctx)
    }
}

impl<N, Nd: ExprNode<N>> Clone for Expr<N, Nd> {
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
    DivisionByZero,
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
                write!(f, "Unsigned expression do not support negation.")
            }
            ExpressionError::DivisionByZero => {
                write!(f, "Division by zero.")
            }
        }
    }
}

impl Error for ExpressionError {}

#[macro_export]
macro_rules! impl_into_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $node:ident) => {
        impl From<$x> for Expr<$x, $node<$x>> {
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
    type Node: ExprNode<Self::Property>;
}

pub type SelectExprNode<T> = <T as SelectExprNodeImpl>::Node;

#[macro_export]
macro_rules! impl_select_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $select:ident) => {
        impl SelectExprNodeImpl for $x {
            type Property = $x;
            type Node = $select<Self::Property>;
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

macro_rules! impl_math_ops {
        ($($trait:ident => $method:ident),*,) => {
            $(
                impl<N> std::ops::$trait<N> for Expr<N, SelectExprNode<N>>
                where
                    N: Num + SelectExprNodeImpl<Property = N>,
                    SelectExprNode<N>: ExprNode<N>,
                    Self: std::ops::$trait<Self, Output = Self> + From<N>,
                {
                    type Output = Self;

                    fn $method(self, rhs: N) -> Self::Output {
                        std::ops::$trait::$method(self, Self::from(rhs))
                    }
                }
            )*
        };
    }

impl_math_ops!(
    Add => add,
    Sub => sub,
    Mul => mul,
    Div => div,
    Rem => rem,
);
