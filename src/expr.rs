use crate::context::{Path, ReadContext, ScopeId};
use crate::float::{FloatBinaryOp, FloatExprNode, FloatUnaryOp};
use crate::frame::Assignment;
use crate::integer::IntExprNode;
use crate::integer::{IntBinaryOp, IntUnaryOp};
use crate::logic::{BoolExpr, BoolExprNode, Compare, CompareExpr, ComparisonOp};
use crate::num_cast::{CastFrom, CastNumPrimitive};
use num_traits::{AsPrimitive, Float, Num};
use std::error::Error;
use std::fmt::Debug;
use std::ops::Neg;
use std::sync::Arc;

pub trait ExprNode<N>: Send + Sync {
    fn eval(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError>;
}

pub trait IfThenNode<N>: ExprNode<N> + Sized
where
    N: SelectExprNodeImpl,
{
    fn if_then(bool_expr: BoolExpr, t: Expr<N>, f: Expr<N>) -> Self;
}

pub struct Expr<N: SelectExprNodeImpl> {
    pub inner: Arc<SelectExprNode<N>>,
    phantom: std::marker::PhantomData<N>,
}

impl<N: SelectExprNodeImpl> Expr<N> {
    pub fn new(node: Arc<SelectExprNode<N>>) -> Self {
        Expr {
            inner: node,
            phantom: Default::default(),
        }
    }
}

impl<N> Expr<N>
where
    N: SelectExprNodeImpl<Property = N> + Copy,
{
    pub fn as_<NOut>(&self) -> Expr<NOut>
    where
        NOut: SelectExprNodeImpl + Num + Copy + Send + Sync + 'static,
        NOut::Node: CastFrom<NOut>,
        N: AsPrimitive<NOut> + Send + Sync,
    {
        let cast_node = CastNumPrimitive::new(self.clone());

        let expr_node = NOut::Node::cast_from(Box::new(cast_node));
        Expr::new(Arc::new(expr_node))
    }

    pub fn eval<Ctx: ReadContext>(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        self.inner.eval(ctx)
    }

    pub fn eval_dyn(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        self.inner.eval(ctx)
    }

    pub fn alias(&self, scope: impl Into<ScopeId>, name: &str) -> Assignment<N> {
        Assignment {
            path: Path::from_name(scope, name),
            expr: Expr::new(self.inner.clone()),
        }
    }
}

impl<N> Expr<N>
where
    N: Float + SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>> + Send + Sync + 'static,
    Self: From<N>,
{
    pub fn powf(self, rhs: N) -> Self {
        Expr::new(Arc::new(FloatExprNode::BinaryOp {
            lhs_expr: self,
            op: FloatBinaryOp::Pow,
            rhs_expr: rhs.into(),
        }))
    }

    pub fn unwrap_or(self, rhs: Expr<N>) -> Self {
        let node = FloatExprNode::ErrorHandlingOp {
            expr: self,
            or_expr: rhs,
        };

        Expr::new(Arc::new(node))
    }
}

macro_rules! float_unary {
    ($($name:ident => $op:ident),* $(,)?) => {
        $(
            pub fn $name(self) -> Self {
                self.unary_expr(FloatUnaryOp::$op)
            }
        )*
    };
}

impl<N> Expr<N>
where
    N: Float + SelectExprNodeImpl<Property = N, Node = FloatExprNode<N>> + Send + Sync + 'static,
{
    fn unary_expr(self, op: FloatUnaryOp) -> Self {
        Expr::new(Arc::new(FloatExprNode::UnaryOp { op, expr: self }))
    }

    float_unary! {
        abs => Abs,
        acos => Acos,
        asin => Asin,
        cos => Cos,
        sin => Sin,
        tan => Tan,
        atan => Atan,
        floor => Floor,
        ceil => Ceil,
        exp => Exp,
        ln => Ln,
        log10 => Log10,
        log2 => Log2,
        sqrt => Sqrt,
        cbrt => Cbrt,
    }
}

impl<N: SelectExprNodeImpl<Property = N>> Clone for Expr<N> {
    fn clone(&self) -> Self {
        Expr::new(self.inner.clone())
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionError {
    MissingAttribute,
    InvalidTypes,
    InvalidPath,
    InvalidOperationNeg,
    DivisionByZero,
    DowncastError,
    FailedReflect(String),
}

impl std::fmt::Display for ExpressionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionError::MissingAttribute => {
                write!(f, "Failed to retrieve attribute from context.")
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
            ExpressionError::DowncastError => {
                write!(f, "Failed to downcast expression.")
            }
            ExpressionError::FailedReflect(msg) => {
                write!(f, "Failed to reflect expression. Error: {}", msg)
            }
            ExpressionError::InvalidPath => {
                write!(f, "Invalid path, no expression found at destination.")
            }
        }
    }
}

impl Error for ExpressionError {}

#[macro_export]
macro_rules! impl_into_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $node:ident) => {
        impl From<$x> for Expr<$x> {
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
impl_into_expr!(BoolExprNode: bool);

pub trait SelectExprNodeImpl {
    type Property;
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

impl SelectExprNodeImpl for bool {
    type Property = bool;
    type Node = BoolExprNode;
}

macro_rules! impl_math_ops {
        ($($trait:ident => $method:ident),*,) => {
            $(
                impl<N> std::ops::$trait<N> for Expr<N>
                where
                    N: Num + SelectExprNodeImpl<Property = N> ,
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

macro_rules! impl_neg_ops {
    (
        $node_enum_path:ident,      // Accepts FloatExprNode
        $node_variant_name:ident,   // Accepts UnaryOp
        $op_enum_path:ident,        // Accepts FloatUnaryOp
        $op_enum_variant:ident:     // Accepts Neg
        $($t:ty),*
    ) => {
        $(
            impl Neg for Expr<$t> {
                type Output = Self;

                fn neg(self) -> Self::Output {
                    let node = $node_enum_path::$node_variant_name {
                        op: $op_enum_path::$op_enum_variant, // The specific operator variant
                        expr: self, // The expression itself
                    };

                    Expr::new(Arc::new(node))
                }
            }
        )*
    };
}

// Floating Point types (f32, f64)
impl_neg_ops!(FloatExprNode,UnaryOp, FloatUnaryOp,Neg: f32, f64);
// Signed Integer types (i8, i32, i64, etc.)
impl_neg_ops!(IntExprNode,UnaryOp, IntUnaryOp,Neg: i8, i16, i32, i64, i128, isize);


impl<N> CompareExpr for Expr<N>
where
    N: Num + SelectExprNodeImpl<Property = N> + PartialOrd + Copy + Send + Sync + 'static,
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

macro_rules! impl_binary_std_ops {
    (
        $target:ident,
        $node_variant:ident,
        $op_enum:ident:
        $($t:ty),*
    ) => {
        $(
            impl std::ops::Add for Expr<$t>
            {
                type Output = Self;

                fn add(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::Add,
                        rhs_expr,
                    }))
                }
            }
            impl std::ops::Sub for Expr<$t>
            {
                type Output = Self;

                fn sub(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::Sub,
                        rhs_expr,
                    }))
                }
            }
            impl std::ops::Mul for Expr<$t>
            {
                type Output = Self;

                fn mul(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::Mul,
                        rhs_expr,
                    }))
                }
            }
            impl std::ops::Div for Expr<$t>
            {
                type Output = Self;

                fn div(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::Div,
                        rhs_expr,
                    }))
                }
            }
        )*
    };
}

impl_binary_std_ops!(IntExprNode, BinaryOp, IntBinaryOp: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_binary_std_ops!(FloatExprNode, BinaryOp, FloatBinaryOp:  f32, f64);

macro_rules! impl_binary_ops {
    (
        $node_enum_path:ident,
        $node_variant_name:ident,
        $op_enum_path:ident:
        $($t:ty),*
    ) => {
        $(
            impl Expr<$t>
            {
                fn binary_expr(self, op: $op_enum_path, rhs_expr: Self) -> Self {
                    Expr::new(Arc::new($node_enum_path::$node_variant_name {
                        lhs_expr: self,
                        op,
                        rhs_expr,
                    }))
                }

                pub fn min(self, rhs_expr: impl Into<Self>) -> Self {
                    self.binary_expr($op_enum_path::Min, rhs_expr.into())
                }

                pub fn max(self, rhs_expr: impl Into<Self>) -> Self {
                    self.binary_expr($op_enum_path::Max, rhs_expr.into())
                }
            }
        )*
    };
}

impl_binary_ops!(IntExprNode, BinaryOp, IntBinaryOp: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_binary_ops!(FloatExprNode, BinaryOp, FloatBinaryOp:  f32, f64);
