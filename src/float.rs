use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::impl_float_binary_ops;
use crate::logic::{BoolExpr, BoolExprNode, Compare, ComparisonOp};
use crate::num_cast::CastFrom;
use num_traits::Float;
use std::fmt::Debug;
use std::sync::Arc;

pub type FloatExpr<N> = Expr<N, FloatExprNode<N>>;

impl<N> FloatExpr<N>
where
    N: Float + Send + Sync + 'static,
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
pub enum FloatExprNode<N> {
    #[default]
    None,
    Lit(N),
    Attribute(Box<dyn RetrieveAttribute<N>>),
    Cast(Box<dyn ExprNode<N>>),
    UnaryOp {
        op: FloatUnaryOp,
        expr: FloatExpr<N>,
    },
    BinaryOp {
        lhs: FloatExpr<N>,
        op: FloatBinaryOp,
        rhs: FloatExpr<N>,
    },
}

impl<N: Float + Send + Sync + 'static> ExprNode<N> for FloatExprNode<N> {
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError> {
        match self {
            FloatExprNode::None => Err(ExpressionError::EmptyExpr),
            FloatExprNode::Lit(lit) => Ok(lit.clone()),
            FloatExprNode::Attribute(attribute) => Ok(attribute.retrieve(ctx)?),
            FloatExprNode::Cast(cast) => Ok(cast.eval_node(ctx)?),
            FloatExprNode::UnaryOp { op, expr } => {
                let value = expr.inner.eval_node(ctx)?;
                op.eval(value)
            }
            FloatExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval_dyn(ctx)?;
                let r = rhs.eval_dyn(ctx)?;
                op.eval(l, r)
            }
        }
    }
}

impl<N> CastFrom<N> for FloatExprNode<N> {
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self {
        FloatExprNode::Cast(node)
    }
}

impl<N> std::ops::Neg for FloatExpr<N>
where
    N: Float + Send + Sync + 'static,
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

#[derive(Debug, Clone, Copy)]
pub enum FloatUnaryOp {
    Neg,
    Acos,
    Asin,
    Cos,
    Sin,
}

impl FloatUnaryOp {
    fn eval<N: Float>(&self, value: N) -> Result<N, ExpressionError> {
        match self {
            FloatUnaryOp::Sin => Ok(value.sin()),
            FloatUnaryOp::Asin => Ok(value.asin()),
            FloatUnaryOp::Cos => Ok(value.cos()),
            FloatUnaryOp::Acos => Ok(value.acos()),
            FloatUnaryOp::Neg => Ok(value.neg()),
        }
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
}

impl FloatBinaryOp {
    fn eval<N: Float>(&self, l: N, r: N) -> Result<N, ExpressionError> {
        match self {
            FloatBinaryOp::Add => Ok(l + r),
            FloatBinaryOp::Sub => Ok(l - r),
            FloatBinaryOp::Mul => Ok(l * r),
            FloatBinaryOp::Div => Ok(l / r),
            FloatBinaryOp::Rem => Ok(l % r),
            FloatBinaryOp::Pow => Ok(l.powf(r)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{MapContext, StrAttr, Val};

    #[test]
    fn test_zero_div() {
        let mut ctx = MapContext::default();

        ctx.0.insert("zero".into(), Val::Float(0.0));
        ctx.0.insert("value".into(), Val::Float(100.0));

        let expr = StrAttr::f32("value") / StrAttr::f32("zero");
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, f32::INFINITY);
    }

    #[test]
    fn test_cast_op() {
        let mut ctx = MapContext::default();

        ctx.0.insert("ten".into(), Val::Float(10.0));
        ctx.0.insert("one_hundred".into(), Val::Int(100));

        let expr = StrAttr::f32("ten") + StrAttr::i32("one_hundred").as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 110.0);

        let expr = StrAttr::i32("one_hundred") + StrAttr::f32("ten").as_();
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 110);
    }
}
