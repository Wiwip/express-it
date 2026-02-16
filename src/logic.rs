use crate::context::ReadContext;
use crate::expr::{Expr, ExprNode, ExpressionError};

pub trait CompareExpr: Sized {
    fn compare(self, op: ComparisonOp, rhs: impl Into<Self>) -> BoolExpr;

    fn gt(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Gt, rhs)
    }
    fn ge(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Ge, rhs)
    }
    fn lt(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Lt, rhs)
    }
    fn le(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Le, rhs)
    }
    fn eq(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Eq, rhs)
    }
    fn ne(self, rhs: impl Into<Self>) -> BoolExpr {
        self.compare(ComparisonOp::Ne, rhs)
    }
}

pub type BoolExpr = Expr<bool, BoolExprNode>;

pub enum BoolExprNode {
    Lit(bool),
    Boxed(Box<dyn ExprNode<bool>>),
    UnaryOp {
        op: LogicUnaryOp,
        expr: BoolExpr,
    },
    BinaryOp {
        lhs: BoolExpr,
        op: LogicBinaryOp,
        rhs: BoolExpr,
    },
}

impl ExprNode<bool> for BoolExprNode {
    fn eval(&self, ctx: &dyn ReadContext) -> Result<bool, ExpressionError> {
        match self {
            BoolExprNode::Lit(lit) => Ok(lit.clone()),
            BoolExprNode::Boxed(value) => Ok(value.eval(ctx)?),
            BoolExprNode::UnaryOp { op, expr } => match op {
                LogicUnaryOp::Not => Ok(!expr.eval_dyn(ctx)?),
            },
            BoolExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval_dyn(ctx)?;
                let r = rhs.eval_dyn(ctx)?;
                match op {
                    LogicBinaryOp::And => Ok(l && r),
                    LogicBinaryOp::Or => Ok(l || r),
                    LogicBinaryOp::Xor => Ok(l ^ r),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ComparisonOp {
    Eq, // Equal
    Ne, // Not Equal
    Gt, // Greater Than
    Ge, // Greater or Equal Than
    Lt, // Less Than
    Le, // Less or Equal Than
}

impl ComparisonOp {
    pub fn compare<N: PartialOrd>(&self, lhs: &N, rhs: &N) -> bool {
        match self {
            ComparisonOp::Eq => lhs == rhs,
            ComparisonOp::Ne => lhs != rhs,
            ComparisonOp::Gt => lhs > rhs,
            ComparisonOp::Ge => lhs >= rhs,
            ComparisonOp::Lt => lhs < rhs,
            ComparisonOp::Le => lhs <= rhs,
        }
    }
}

pub struct Compare<N, Nd> {
    pub lhs: Expr<N, Nd>,
    pub op: ComparisonOp,
    pub rhs: Expr<N, Nd>,
}

impl<N, Nd> ExprNode<bool> for Compare<N, Nd>
where
    N: PartialOrd + Copy + 'static,
    Nd: ExprNode<N> + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<bool, ExpressionError> {
        let l = self.lhs.eval_dyn(ctx)?;
        let r = self.rhs.eval_dyn(ctx)?;
        Ok(self.op.compare(&l, &r))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LogicUnaryOp {
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum LogicBinaryOp {
    And,
    Or,
    Xor,
}

#[cfg(test)]
mod tests {
    use crate::logic::CompareExpr;
    use crate::test_utils::{F32Attribute, I32Attribute, MapContext};

    #[test]
    fn test_float_logic() {
        let mut ctx = MapContext::default();
        ctx.insert_src::<F32Attribute>(1500.0);
        ctx.insert_dst::<F32Attribute>(0.0);
        ctx.insert_dst::<I32Attribute>(0);

        let expr = F32Attribute::src().gt(F32Attribute::dst());
        let is_greater = expr.eval(&ctx).unwrap();
        assert_eq!(is_greater, 1500 > 0);

        let expr = F32Attribute::dst().lt(F32Attribute::src());
        let is_lower = expr.eval(&ctx).unwrap();
        assert_eq!(is_lower, 0 < 1500);

        let expr = F32Attribute::dst().eq(I32Attribute::dst().as_());
        let is_equal = expr.eval(&ctx).unwrap();
        assert_eq!(is_equal, true);
    }

    #[test]
    fn test_int_logic() {
        let mut ctx = MapContext::default();
        ctx.insert_src::<I32Attribute>(1500);
        ctx.insert_dst::<I32Attribute>(0);
        ctx.insert_dst::<F32Attribute>(0.0);

        let expr = I32Attribute::src().gt(I32Attribute::dst());
        let is_greater = expr.eval(&ctx).unwrap();
        assert_eq!(is_greater, 1500 > 0);

        let expr = I32Attribute::dst().lt(I32Attribute::src());
        let is_lower = expr.eval(&ctx).unwrap();
        assert_eq!(is_lower, 0 < 1500);

        let expr = I32Attribute::dst().eq(F32Attribute::dst().as_());
        let is_equal = expr.eval(&ctx).unwrap();
        assert_eq!(is_equal, true);
    }
}
