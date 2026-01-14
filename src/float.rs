use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::impl_float_binary_ops;
use crate::integer::IntExprNode;
use num_traits::{AsPrimitive, CheckedNeg, Float, Num, PrimInt};
use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::sync::Arc;
use crate::logic::{BoolExpr, BoolExprNode, Compare, ComparisonOp};

pub type FloatExpr<N, Ctx> = Expr<N, Ctx, FloatExprNode<N, Ctx>>;

impl<N, Ctx> FloatExpr<N, Ctx>
where
    N: Float + Send + Sync + 'static,
    Ctx: EvalContext + 'static,
{
    pub fn gt(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Gt,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn ge(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Ge,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn lt(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Lt,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn le(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Le,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn eq(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Eq,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn ne(self, rhs: impl Into<Self>) -> BoolExpr<Ctx> {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Ne,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }
}

#[derive(Default)]
pub enum FloatExprNode<N: Float + Send + Sync, Ctx: EvalContext> {
    #[default]
    None,
    Lit(N),
    Attribute(Box<dyn RetrieveAttribute<N, Ctx>>),
    Cast(Box<dyn ExprNode<N, Ctx>>),
    UnaryOp {
        op: FloatUnaryOp,
        expr: FloatExpr<N, Ctx>,
    },
    BinaryOp {
        lhs: FloatExpr<N, Ctx>,
        op: FloatBinaryOp,
        rhs: FloatExpr<N, Ctx>,
    },

}

impl<N: Float + Send + Sync, Ctx: EvalContext> ExprNode<N, Ctx> for FloatExprNode<N, Ctx> {
    fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        match self {
            FloatExprNode::None => Err(ExpressionError::EmptyExpr),
            FloatExprNode::Lit(lit) => Ok(lit.clone()),
            FloatExprNode::Attribute(attribute) => Ok(ctx.get(attribute)),
            FloatExprNode::Cast(cast) => Ok(cast.eval(ctx)?),
            FloatExprNode::UnaryOp { op, expr } => {
                let value = expr.inner.eval(ctx)?;
                match op {
                    FloatUnaryOp::Sin => Ok(value.sin()),
                    FloatUnaryOp::Asin => Ok(value.asin()),
                    FloatUnaryOp::Cos => Ok(value.cos()),
                    FloatUnaryOp::Acos => Ok(value.acos()),
                    FloatUnaryOp::Neg => Ok(value.neg()),
                }
            }
            FloatExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval(ctx)?;
                let r = rhs.eval(ctx)?;
                match op {
                    FloatBinaryOp::Add => Ok(l + r),
                    FloatBinaryOp::Sub => Ok(l - r),
                    FloatBinaryOp::Mul => Ok(l * r),
                    FloatBinaryOp::Div => Ok(l / r),
                    FloatBinaryOp::Rem => Ok(l % r),
                    FloatBinaryOp::Pow => Ok(l.powf(r)),
                    //BinaryOp::Log => Ok(l.log(r)),
                }
            }
        }
    }
}

impl<N, Ctx> std::ops::Neg for Expr<N, Ctx, FloatExprNode<N, Ctx>>
where
    N: Float + Send + Sync + 'static,
    Ctx: EvalContext + 'static,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        Expr::new(Arc::new(FloatExprNode::UnaryOp {
            op: FloatUnaryOp::Neg,
            expr: self,
        }))
    }
}

impl_float_binary_ops!(
    FloatExprNode,
    BinaryOp,
    FloatBinaryOp,
    [
        Add => (add, Add),
        Sub => (sub, Sub),
        Mul => (mul, Mul),
        Div => (div, Div),
        Rem => (rem, Rem)
    ]
);

pub struct AsFloat<NOut, NIn: PrimInt + CheckedNeg + Send + Sync, Ctx: EvalContext> {
    inner: Expr<NIn, Ctx, IntExprNode<NIn, Ctx>>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn, Ctx: EvalContext> AsFloat<NOut, NIn, Ctx>
where
    NIn: PrimInt + CheckedNeg + Send + Sync,
{
    pub fn new(inner: Expr<NIn, Ctx, IntExprNode<NIn, Ctx>>) -> Self {
        Self {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn, Ctx> Debug for AsFloat<NOut, NIn, Ctx>
where
    NIn: PrimInt + CheckedNeg + Send + Sync,
    Ctx: EvalContext,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut, Ctx> ExprNode<NOut, Ctx> for AsFloat<NOut, NIn, Ctx>
where
    NIn: PrimInt + CheckedNeg + AsPrimitive<NOut> + Copy + Send + Sync + 'static,
    NOut: Num + Copy + Send + Sync + 'static,
    Ctx: EvalContext,
{
    fn eval(&self, ctx: &Ctx) -> Result<NOut, ExpressionError> {
        Ok(self.inner.eval(ctx)?.as_())
    }
}


#[derive(Debug, Clone, Copy)]
pub enum FloatUnaryOp {
    Neg,
    Acos,
    Asin,
    Cos,
    Sin,
}

#[derive(Debug, Clone, Copy)]
pub enum FloatBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}
