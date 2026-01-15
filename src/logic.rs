use crate::context::EvalContext;
use crate::expr::{Expr, ExprNode, ExpressionError};

pub type BoolExpr<Ctx> = Expr<bool, Ctx, BoolExprNode<Ctx>>;

#[derive(Default)]
pub enum BoolExprNode<Ctx> {
    #[default]
    None,
    Lit(bool),
    Boxed(Box<dyn ExprNode<bool, Ctx>>),
    UnaryOp {
        op: LogicUnaryOp,
        expr: BoolExpr<Ctx>,
    },
    BinaryOp {
        lhs: BoolExpr<Ctx>,
        op: LogicBinaryOp,
        rhs: BoolExpr<Ctx>,
    },
}

impl<Ctx: EvalContext> ExprNode<bool, Ctx> for BoolExprNode<Ctx> {
    fn eval(&self, ctx: &Ctx) -> Result<bool, ExpressionError> {
        match self {
            BoolExprNode::None => Err(ExpressionError::EmptyExpr),
            BoolExprNode::Lit(lit) => Ok(lit.clone()),
            //BoolExprNode::Attribute(attribute) => Ok(ctx.get(attribute)),
            BoolExprNode::UnaryOp { op, expr } => match op {
                LogicUnaryOp::Not => Ok(!expr.eval(ctx)?),
            },
            BoolExprNode::BinaryOp { lhs, op, rhs } => {
                let l = lhs.eval(ctx)?;
                let r = rhs.eval(ctx)?;
                match op {
                    LogicBinaryOp::And => Ok(l && r),
                    LogicBinaryOp::Or => Ok(l || r),
                    LogicBinaryOp::Xor => Ok(l ^ r),
                }
            }
            BoolExprNode::Boxed(value) => Ok(value.eval(ctx)?),
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

pub struct Compare<N, Ctx, Nd> {
    pub lhs: Expr<N, Ctx, Nd>,
    pub op: ComparisonOp,
    pub rhs: Expr<N, Ctx, Nd>,
}

impl<N: PartialOrd + Send + Sync, Ctx: EvalContext, Nd: ExprNode<N, Ctx>> ExprNode<bool, Ctx>
    for Compare<N, Ctx, Nd>
{
    fn eval(&self, ctx: &Ctx) -> Result<bool, ExpressionError> {
        let l = self.lhs.eval(ctx)?;
        let r = self.rhs.eval(ctx)?;
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
    use crate::test_utils::{MapContext, StrAttr, Val};

    #[test]
    fn test_float_logic() {
        let mut ctx = MapContext::default();

        ctx.0.insert("small_value".into(), Val::Float(0.1337));
        ctx.0.insert("large_value".into(), Val::Float(18_000.0));

        let expr = StrAttr::f32("small_value").gt(StrAttr::f32("large_value"));
        let is_greater = expr.eval(&ctx).unwrap();
        assert_eq!(is_greater, 0.1337 > 18_000.0);

        let expr = StrAttr::f32("small_value").lt(StrAttr::f32("large_value"));
        let is_less = expr.eval(&ctx).unwrap();
        assert_eq!(is_less, 0.1337 < 18_000.0);

        let expr = StrAttr::f32("small_value").eq(StrAttr::f32("small_value"));
        let is_equal = expr.eval(&ctx).unwrap();
        assert_eq!(is_equal, 0.1337 == 0.1337);
    }

    #[test]
    fn test_int_logic() {
        let mut ctx = MapContext::default();

        ctx.0.insert("zero".into(), Val::Int(0));
        ctx.0.insert("small_value".into(), Val::Int(42));
        ctx.0.insert("large_value".into(), Val::Int(1337));

        let expr = StrAttr::i32("small_value").gt(StrAttr::i32("large_value"));
        let is_greater = expr.eval(&ctx).unwrap();
        assert_eq!(is_greater, 0.1337 > 18_000.0);

        let expr = StrAttr::i32("small_value").lt(StrAttr::i32("large_value"));
        let is_less = expr.eval(&ctx).unwrap();
        assert_eq!(is_less, 0.1337 < 18_000.0);

        let expr = StrAttr::i32("zero").eq(StrAttr::i32("zero"));
        let is_equal = expr.eval(&ctx).unwrap();
        assert_eq!(is_equal, 0.1337 == 0.1337);
    }
}
