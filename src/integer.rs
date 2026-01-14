use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::float::{AsFloat, FloatExprNode};
use crate::impl_int_binary_ops;
use num_traits::{AsPrimitive, CheckedNeg, Float, PrimInt};
use std::sync::Arc;


pub type IntExpr<N, Ctx> = Expr<N, Ctx, IntExprNode<N, Ctx>>;

#[derive(Default)]
pub enum IntExprNode<N: PrimInt + CheckedNeg + Send + Sync, Ctx: EvalContext> {
    #[default]
    None,
    Lit(N),
    Attribute(Box<dyn RetrieveAttribute<N, Ctx>>),
    Cast(Box<dyn ExprNode<N, Ctx>>),
    UnaryOp {
        op: IntUnaryOp,
        expr: IntExpr<N, Ctx>,
    },
    BinaryOp {
        lhs: IntExpr<N, Ctx>,
        op: IntBinaryOp,
        rhs: IntExpr<N, Ctx>,
    },
}


impl<N: PrimInt + CheckedNeg + Send + Sync, Ctx: EvalContext> ExprNode<N, Ctx>
    for IntExprNode<N, Ctx>
{
    fn eval(&self, ctx: &Ctx) -> Result<N, ExpressionError> {
        match self {
            IntExprNode::None => Err(ExpressionError::EmptyExpr),
            IntExprNode::Lit(lit) => Ok(lit.clone()),
            IntExprNode::Attribute(attribute) => Ok(ctx.get(attribute)),
            IntExprNode::Cast(cast) => Ok(cast.eval(ctx)?),
            IntExprNode::UnaryOp { op, expr } => match op {
                IntUnaryOp::Neg => expr
                    .eval(ctx)?
                    .checked_neg()
                    .ok_or(ExpressionError::InvalidOperationNeg),
            },
            IntExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval(ctx)?;
                let r = rhs.eval(ctx)?;
                match op {
                    IntBinaryOp::Add => Ok(l + r),
                    IntBinaryOp::Sub => Ok(l - r),
                    IntBinaryOp::Mul => Ok(l * r),
                    IntBinaryOp::Div => Ok(l / r),
                    IntBinaryOp::Rem => Ok(l % r),
                    IntBinaryOp::Pow => Ok(l.pow(r.to_u32().ok_or(ExpressionError::InvalidTypes)?)),
                }
            }
        }
    }
}

impl<NIn, Ctx> Expr<NIn, Ctx, IntExprNode<NIn, Ctx>>
where
    NIn: PrimInt + CheckedNeg + Send + Sync + 'static,
    Ctx: EvalContext + 'static,
{
    pub fn as_<NOut>(&self) -> Expr<NOut, Ctx, FloatExprNode<NOut, Ctx>>
    where
        NOut: Float + Send + Sync + 'static,
        NIn: AsPrimitive<NOut>,
    {
        let inner = Expr::new(self.inner.clone());
        let as_float = FloatExprNode::Cast(Box::new(AsFloat::new(inner)));
        Expr::new(Arc::new(as_float))
    }
}

impl<N, Ctx> std::ops::Neg for Expr<N, Ctx, IntExprNode<N, Ctx>>
where
    N: PrimInt + CheckedNeg + Send + Sync + 'static,
    Ctx: EvalContext + 'static,
{
    type Output = Self;

    fn neg(self) -> Self::Output {
        Expr::new(Arc::new(IntExprNode::UnaryOp {
            op: IntUnaryOp::Neg,
            expr: self,
        }))
    }
}

impl_int_binary_ops!(
    IntExprNode,
    BinaryOp,
    IntBinaryOp,
    [
        Add => (add, Add),
        Sub => (sub, Sub),
        Mul => (mul, Mul),
        Div => (div, Div),
        Rem => (rem, Rem)
    ]
);

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
}