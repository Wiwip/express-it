use crate::context::{Path, ReadContext, ScopeId};
use crate::float::{FloatBinaryOp, FloatExprNode, FloatTrinaryOp, FloatUnaryOp};
use crate::integer::{IntBinaryOp, IntUnaryOp};
use crate::integer::{IntExprNode, IntTrinaryOp};
use crate::logic::{BoolExpr, BoolExprNode, Compare, CompareExpr, ComparisonOp};
use crate::num_cast::{CastFrom, CastNumPrimitive};
use num_traits::{AsPrimitive, Float, Num};
use std::error::Error;
use std::fmt::Debug;
use std::ops::Neg;
use std::sync::Arc;
use crate::frame::Assignment;

pub trait ExprNode<N, Ctx>: Send + Sync {
    fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError>;
    fn eval_dyn(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError>;
    fn get_dependencies(&self, deps: &mut std::collections::HashSet<Path>);
}

pub trait IfThenNode<N, Ctx>: ExprNode<N, Ctx> + Sized
where
    N: SelectExprNodeImpl<Ctx>,
    Ctx: ReadContext + 'static,
{
    fn if_then(bool_expr: BoolExpr<Ctx>, t: Expr<N, Ctx>, f: Expr<N, Ctx>) -> Self;
}

pub struct Expr<N: SelectExprNodeImpl<Ctx>, Ctx> {
    pub inner: Arc<SelectExprNode<N, Ctx>>,
    phantom: std::marker::PhantomData<(N, Ctx)>,
}

impl<N: SelectExprNodeImpl<Ctx>, Ctx> Expr<N, Ctx> {
    pub fn new(node: Arc<SelectExprNode<N, Ctx>>) -> Self {
        Expr {
            inner: node,
            phantom: Default::default(),
        }
    }
}

impl<N, Ctx: ReadContext  + 'static> Expr<N, Ctx>
where
    N: SelectExprNodeImpl<Ctx, Property = N> + Copy,
    Ctx: ReadContext,
{
    pub fn as_<NOut>(&self) -> Expr<NOut, Ctx>
    where
        NOut: SelectExprNodeImpl<Ctx> + Num + Copy + Send + Sync + 'static,
        NOut::Node: CastFrom<NOut, Ctx>,
        N: AsPrimitive<NOut> + Send + Sync,
    {
        let cast_node = CastNumPrimitive::<NOut, N, Ctx>::new(self.clone());

        let expr_node = NOut::Node::cast_from(Box::new(cast_node));
        Expr::new(Arc::new(expr_node))
    }

    pub fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        self.inner.eval(ctx)
    }

    pub fn alias(&self, scope: impl Into<ScopeId>, name: &str) -> Assignment<N, Ctx> {
        Assignment {
            path: Path::from_name(scope, name),
            expr: Expr::new(self.inner.clone()),
        }
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

impl<N, Ctx> Expr<N, Ctx>
where
    N: Float
        + SelectExprNodeImpl<Ctx, Property = N, Node = FloatExprNode<N, Ctx>>
        + Send
        + Sync
        + 'static,
    Ctx: ReadContext + 'static,
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

impl<N: SelectExprNodeImpl<Ctx>, Ctx: ReadContext  + 'static> Clone for Expr<N, Ctx> {
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
        impl<Ctx: ReadContext  + 'static> From<$x> for Expr<$x, Ctx> {
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

pub trait SelectExprNodeImpl<Ctx> {
    type Property: Send + Sync;
    type Node: ExprNode<Self::Property, Ctx>;
}

pub type SelectExprNode<T, Ctx> = <T as SelectExprNodeImpl<Ctx>>::Node;

#[macro_export]
macro_rules! impl_select_expr {
    // Inner rule for a single implementation
    (@impl $x:ty, $select:ident) => {
        impl<Ctx: ReadContext + 'static> SelectExprNodeImpl<Ctx> for $x {
            type Property = $x;
            type Node = $select<Self::Property, Ctx>;
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

impl<Ctx: Send + Sync + 'static> SelectExprNodeImpl<Ctx> for bool {
    type Property = bool;
    type Node = BoolExprNode<Ctx>;
}

macro_rules! impl_math_ops {
        ($($trait:ident => $method:ident),*,) => {
            $(
                impl<N, Ctx> std::ops::$trait<N> for Expr<N, Ctx>
                where
                    N: Num + SelectExprNodeImpl<Ctx, Property = N> ,
                    Self: std::ops::$trait<Self, Output = Self> + From<N>,
                    Ctx: ReadContext,
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
            impl<Ctx: ReadContext  + 'static> Neg for Expr<$t, Ctx> {
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

impl<N, Ctx> CompareExpr<Ctx> for Expr<N, Ctx>
where
    N: Num + SelectExprNodeImpl<Ctx, Property = N> + PartialOrd + Copy + Send + Sync + 'static,
    Ctx: ReadContext + 'static,
{
    fn compare(self, op: ComparisonOp, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
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
            impl<Ctx: ReadContext  + 'static> std::ops::Add for Expr<$t, Ctx>
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
            impl<Ctx: ReadContext  + 'static> std::ops::Sub for Expr<$t, Ctx>
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
            impl<Ctx: ReadContext  + 'static> std::ops::Mul for Expr<$t, Ctx>
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
            impl<Ctx: ReadContext  + 'static> std::ops::Div for Expr<$t, Ctx>
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
            impl<Ctx: ReadContext  + 'static> std::ops::Rem for Expr<$t, Ctx>
            {
                type Output = Self;

                fn rem(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::Rem,
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
            impl<Ctx: ReadContext  + 'static> Expr<$t, Ctx>
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

                pub fn pow(self, rhs_expr: impl Into<Self>) -> Self {
                    self.binary_expr($op_enum_path::Pow, rhs_expr.into())
                }

                pub fn unwrap_or(self, rhs: Self) -> Self {
                    let node = $node_enum_path::ErrorHandlingOp {
                        expr: self,
                        or_expr: rhs,
                    };
                    Expr::new(Arc::new(node))
                }
            }
        )*
    };
}

impl_binary_ops!(IntExprNode, BinaryOp, IntBinaryOp: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_binary_ops!(FloatExprNode, BinaryOp, FloatBinaryOp:  f32, f64);

macro_rules! impl_trinary_ops {
    (
        $node_enum_path:ident,
        $node_variant_name:ident,
        $op_enum_path:ident:
        $($t:ty),*
    ) => {
        $(
            impl<Ctx: ReadContext  + 'static> Expr<$t, Ctx>
            {
                fn trinary_expr(self, op: $op_enum_path, min_expr: Self, max_expr: Self) -> Self {
                    let node = $node_enum_path::$node_variant_name {
                        value_expr: self,
                        op,
                        arg1_expr: min_expr,
                        arg2_expr: max_expr,
                    };
                    Expr::new(Arc::new(node))
                }

                pub fn clamp(self, min_expr: impl Into<Self>, max_expr: impl Into<Self>) -> Self {
                    self.trinary_expr($op_enum_path::Clamp, min_expr.into(), max_expr.into())
                }
            }
        )*
    };
}

impl_trinary_ops!(IntExprNode, TrinaryOp, IntTrinaryOp: i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);
impl_trinary_ops!(FloatExprNode, TrinaryOp, FloatTrinaryOp:  f32, f64);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::scopes::{DST, SRC};
    use crate::test_utils::{Atk, Hp, IntAtk, IntHp};

    #[test]
    fn test_float_dependency_tracking() {
        let expr = Atk::get(SRC) + Hp::get(DST);
        let mut deps = std::collections::HashSet::new();
        expr.inner.get_dependencies(&mut deps);

        assert!(deps.contains(&Path::from_type_name::<Atk>(SRC)));
        assert!(deps.contains(&Path::from_type_name::<Hp>(DST)));
        assert_eq!(deps.len(), 2);
    }

    #[test]
    fn test_integer_dependency_tracking() {
        let expr = IntAtk::get(SRC) + IntHp::get(DST);
        let mut deps = std::collections::HashSet::new();
        expr.inner.get_dependencies(&mut deps);

        assert!(deps.contains(&Path::from_type_name::<IntAtk>(SRC)));
        assert!(deps.contains(&Path::from_type_name::<IntHp>(DST)));
        assert_eq!(deps.len(), 2);
    }
}
