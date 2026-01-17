use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::impl_int_binary_ops;
use crate::logic::{BoolExpr, BoolExprNode, Compare, ComparisonOp};
use crate::num_cast::CastFrom;
use num_traits::{AsPrimitive, CheckedNeg, PrimInt};
use std::sync::Arc;

pub type IntExpr<N> = Expr<N, IntExprNode<N>>;

impl<N> IntExpr<N>
where
    N: PrimInt + CheckedNeg + Send + Sync + 'static,
{
    pub fn gt(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Gt,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn ge(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Ge,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn lt(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Lt,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn le(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Le,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn eq(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Eq,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }

    pub fn ne(self, rhs: impl Into<Self>) -> BoolExpr {
        let cmp = Compare {
            lhs: self,
            op: ComparisonOp::Ne,
            rhs: rhs.into(),
        };
        BoolExpr::new(Arc::new(BoolExprNode::Boxed(Box::new(cmp))))
    }
}

#[derive(Default)]
pub enum IntExprNode<N> {
    #[default]
    None,
    Lit(N),
    Attribute(Box<dyn RetrieveAttribute<N> + Send + Sync>),
    Cast(Box<dyn ExprNode<N> + Send + Sync>),
    UnaryOp {
        op: IntUnaryOp,
        expr: IntExpr<N>,
    },
    BinaryOp {
        lhs: IntExpr<N>,
        op: IntBinaryOp,
        rhs: IntExpr<N>,
    },
}

impl<N> ExprNode<N> for IntExprNode<N>
where
    N: PrimInt + CheckedNeg + Send + Sync + 'static,
{
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError> {
        match self {
            IntExprNode::None => Err(ExpressionError::EmptyExpr),
            IntExprNode::Lit(lit) => Ok(lit.clone()),
            IntExprNode::Attribute(attribute) => Ok(attribute.retrieve(ctx)?),
            IntExprNode::Cast(cast) => Ok(cast.eval_node(ctx)?),
            IntExprNode::UnaryOp { op, expr } => match op {
                IntUnaryOp::Neg => expr
                    .eval_dyn(ctx)?
                    .checked_neg()
                    .ok_or(ExpressionError::InvalidOperationNeg),
            },
            IntExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval_dyn(ctx)?;
                let r = rhs.eval_dyn(ctx)?;
                op.eval(l, r)
            }
        }
    }
}

impl<N> CastFrom<N> for IntExprNode<N> {
    fn cast_from(node: Box<dyn ExprNode<N> + Send + Sync>) -> Self {
        IntExprNode::Cast(node)
    }
}

impl<N> std::ops::Neg for Expr<N, IntExprNode<N>>
where
    N: PrimInt + CheckedNeg + Send + Sync + 'static,
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

impl IntBinaryOp {
    fn eval<N: PrimInt + CheckedNeg>(&self, l: N, r: N) -> Result<N, ExpressionError> {
        match self {
            IntBinaryOp::Add => Ok(l + r),
            IntBinaryOp::Sub => Ok(l - r),
            IntBinaryOp::Mul => Ok(l * r),
            IntBinaryOp::Div => Ok(l.checked_div(&r).ok_or(ExpressionError::DivisionByZero)?),
            IntBinaryOp::Rem => Ok(l % r),
            IntBinaryOp::Pow => Ok(l.pow(r.to_u32().ok_or(ExpressionError::InvalidTypes)?)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::ExpressionError;
    use crate::test_utils::{MapContext, StrAttr, Val};

    #[test]
    fn test_zero_div() {
        let mut ctx = MapContext::default();

        ctx.0.insert("zero".into(), Val::Int(0));
        ctx.0.insert("value".into(), Val::Int(100));

        let expr = StrAttr::i32("value") / StrAttr::i32("zero");
        let expr_result = expr.eval(&ctx);
        assert_eq!(expr_result, Err(ExpressionError::DivisionByZero));
    }

    #[test]
    fn test_int_binary_op() {}
}
