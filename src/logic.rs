use crate::context::{Path, ReadContext};
use crate::expr::{
    Expr, ExprNode, ExpressionError, IfThenNode, SelectExprNode, SelectExprNodeImpl,
};
use num_traits::Num;
use std::collections::HashSet;
use std::sync::Arc;

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

pub type BoolExpr = Expr<bool>;

impl BoolExpr {
    pub fn true_() -> BoolExpr {
        let node = BoolExprNode::Lit(true);
        BoolExpr::new(Arc::new(node))
    }

    pub fn false_() -> BoolExpr {
        let node = BoolExprNode::Lit(false);
        BoolExpr::new(Arc::new(node))
    }

    pub fn if_then_else<N>(
        self,
        if_true_expr: impl Into<Expr<N>>,
        if_false_expr: impl Into<Expr<N>>,
    ) -> Expr<N>
    where
        N: Num + SelectExprNodeImpl<Property = N> + Send + Sync + 'static,
        SelectExprNode<N>: IfThenNode<N>,
    {
        // Convert once (avoid move errors from multiple `.into()` calls)
        let bool_expr = self;
        let t: Expr<N> = if_true_expr.into();
        let f: Expr<N> = if_false_expr.into();

        // Build the correct node type (int or float) via the trait, not via FloatExprNode directly
        let node = <SelectExprNode<N> as IfThenNode<N>>::if_then(bool_expr, t, f);

        Expr::<N>::new(Arc::new(node))
    }

    pub fn then<N>(self, if_true_expr: impl Into<Expr<N>>) -> PartialConditional<N>
    where
        N: Num + SelectExprNodeImpl<Property = N> + Send + Sync + 'static,
        SelectExprNode<N>: IfThenNode<N>,
    {
        PartialConditional {
            bool_expr: self,
            if_true_expr: if_true_expr.into(),
        }
    }

    pub fn nand(self, other: BoolExpr) -> BoolExpr {
        !(self & other)
    }

    pub fn nor(self, other: BoolExpr) -> BoolExpr {
        !(self | other)
    }

    pub fn xnor(self, other: BoolExpr) -> BoolExpr {
        !(self ^ other)
    }

    pub fn any(self) {
        unimplemented!()
    }

    pub fn all() {
        unimplemented!()
    }
}

impl std::ops::Not for BoolExpr {
    type Output = BoolExpr;

    fn not(self) -> Self::Output {
        let node = BoolExprNode::UnaryOp {
            op: LogicUnaryOp::Not,
            expr: self,
        };
        Expr::new(Arc::new(node))
    }
}

impl std::ops::BitAnd for BoolExpr {
    type Output = BoolExpr;

    fn bitand(self, rhs: Self) -> Self::Output {
        let node = BoolExprNode::BinaryOp {
            lhs: self,
            op: LogicBinaryOp::And,
            rhs,
        };
        Expr::new(Arc::new(node))
    }
}

impl std::ops::BitOr for BoolExpr {
    type Output = BoolExpr;

    fn bitor(self, rhs: Self) -> Self::Output {
        let node = BoolExprNode::BinaryOp {
            lhs: self,
            op: LogicBinaryOp::Or,
            rhs,
        };
        Expr::new(Arc::new(node))
    }
}

impl std::ops::BitXor for BoolExpr {
    type Output = BoolExpr;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let node = BoolExprNode::BinaryOp {
            lhs: self,
            op: LogicBinaryOp::Xor,
            rhs,
        };
        Expr::new(Arc::new(node))
    }
}

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

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        match self {
            BoolExprNode::Lit(_) => {}
            BoolExprNode::Boxed(value) => {
                value.get_dependencies(deps);
            }
            BoolExprNode::UnaryOp { expr, .. } => {
                expr.inner.get_dependencies(deps);
            }
            BoolExprNode::BinaryOp { lhs, rhs, .. } => {
                lhs.inner.get_dependencies(deps);
                rhs.inner.get_dependencies(deps);
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

pub struct Compare<N: SelectExprNodeImpl> {
    pub lhs: Expr<N>,
    pub op: ComparisonOp,
    pub rhs: Expr<N>,
}

impl<N> ExprNode<bool> for Compare<N>
where
    N: SelectExprNodeImpl<Property = N> + PartialOrd + Send + Sync + Copy + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<bool, ExpressionError> {
        let l = self.lhs.eval_dyn(ctx)?;
        let r = self.rhs.eval_dyn(ctx)?;
        Ok(self.op.compare(&l, &r))
    }

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        self.rhs.inner.get_dependencies(deps);
        self.lhs.inner.get_dependencies(deps);
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

pub struct PartialConditional<N: SelectExprNodeImpl> {
    pub bool_expr: BoolExpr,
    pub if_true_expr: Expr<N>,
}

impl<N> PartialConditional<N>
where
    N: Num + Send + Sync + 'static + SelectExprNodeImpl<Property = N>,
    SelectExprNode<N>: IfThenNode<N>,
{
    pub fn otherwise(self, if_false_expr: impl Into<Expr<N>>) -> Expr<N> {
        let node = <SelectExprNode<N> as IfThenNode<N>>::if_then(
            self.bool_expr,
            self.if_true_expr,
            if_false_expr.into(),
        );

        Expr::<N>::new(Arc::new(node))
    }
}

#[cfg(test)]
mod tests {
    use crate::logic::{BoolExpr, CompareExpr};
    use crate::test_utils::scopes::{DST, SRC};
    use crate::test_utils::{Atk, Hp, IntDef, IntHp, MapContext};

    #[test]
    fn test_complex_comparison_operators() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntHp>(SRC, 100);
        ctx.insert::<IntHp>(DST, 200);

        assert!(IntHp::get(SRC).lt(IntHp::get(DST)).eval(&ctx).unwrap());
        assert!(IntHp::get(SRC).le(100).eval(&ctx).unwrap());
        assert!(IntHp::get(DST).gt(IntHp::get(SRC)).eval(&ctx).unwrap());
        assert!(IntHp::get(DST).ge(200).eval(&ctx).unwrap());
        assert!(IntHp::get(SRC).ne(IntHp::get(DST)).eval(&ctx).unwrap());
        assert!(IntHp::get(SRC).eq(100).eval(&ctx).unwrap());
    }

    #[test]
    fn test_nested_logic_expressions() {
        let ctx = MapContext::default();
        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        // Testing: !(True & False) | False  => !False | False => True | False => True
        let expr = (!(t.clone() & f.clone())) | f.clone();
        assert_eq!(expr.eval(&ctx).unwrap(), true);

        // Testing: (True ^ True) & True => False & True => False
        let expr2 = (t.clone() ^ t.clone()) & t.clone();
        assert_eq!(expr2.eval(&ctx).unwrap(), false);
    }

    #[test]
    fn test_conditional_logic_nesting() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 50.0);

        // Complex scenario:
        // If (Atk > 100) then 1.0
        // else if (Atk > 40) then 2.0
        // else 3.0
        let nested_if = Atk::get(SRC)
            .gt(100.0)
            .then(1.0)
            .otherwise(Atk::get(SRC).gt(40.0).then(2.0).otherwise(3.0));

        assert_eq!(nested_if.eval(&ctx).unwrap(), 2.0);

        // Update context to trigger the "else"
        ctx.insert::<Atk>(SRC, 10.0);
        assert_eq!(nested_if.eval(&ctx).unwrap(), 3.0);
    }

    #[test]
    fn test_logic_with_attribute_lookup() {
        let mut ctx = MapContext::default();
        // If not, we simulate it by comparing attributes
        ctx.insert::<IntHp>(SRC, 0);
        ctx.insert::<IntDef>(SRC, 10);

        // is_dead = HP <= 0
        let is_dead = IntHp::get(SRC).le(0);
        // has_armor = DEF > 0
        let has_armor = IntDef::get(SRC).gt(0);

        // Can move if (!is_dead & has_armor)
        let can_move = (!is_dead.clone()) & has_armor.clone();
        assert_eq!(can_move.eval(&ctx).unwrap(), false);

        // Revive
        ctx.insert::<IntHp>(SRC, 50);
        assert_eq!(can_move.eval(&ctx).unwrap(), true);
    }

    #[test]
    fn test_boolean_casting_and_if_then_cross_types() {
        let mut ctx = MapContext::default();
        ctx.insert::<IntHp>(SRC, 100);

        // Combine logic with numeric outputs
        // result = (HP == 100) ? 50.0 : 0.0
        let expr = IntHp::get(SRC).eq(100).if_then_else(50.0, 0.0);

        let res = expr.eval(&ctx).unwrap();
        // Check if it correctly returned a float 50.0
        assert_eq!(res, 50.0);
    }

    #[test]
    fn test_selector_logic() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 10.0);
        ctx.insert::<Hp>(SRC, 20.0);

        let select_atk_expr = Atk::get(SRC).gt(15.0);
        let select_def_expr = Hp::get(SRC).gt(15.0);

        let expr_result = select_atk_expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, false);
        // If Atk greater than 15.0, return 50.0 else return 100.0
        let expr_result = select_atk_expr
            .clone()
            .if_then_else(50.0, 100.0)
            .eval(&ctx)
            .unwrap();
        assert_eq!(expr_result, 100.0);
        let expr_result = select_atk_expr
            .then(50.0)
            .otherwise(100.0)
            .eval(&ctx)
            .unwrap();
        assert_eq!(expr_result, 100.0);

        let expr_result = select_def_expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, true);
        // If Def greater than 15.0, return 150.0 else return 550.0
        let expr_result = select_def_expr
            .clone()
            .if_then_else(150.0, 550.0)
            .eval(&ctx)
            .unwrap();
        assert_eq!(expr_result, 150.0);

        let expr_result = select_def_expr
            .then(150.0)
            .otherwise(550.0)
            .eval(&ctx)
            .unwrap();
        assert_eq!(expr_result, 150.0);
    }

    #[test]
    fn test_and() {
        let ctx = MapContext::default();

        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        // (A, B) → expected
        let cases = [
            (f.clone(), f.clone(), false),
            (f.clone(), t.clone(), false),
            (t.clone(), f.clone(), false),
            (t.clone(), t.clone(), true),
        ];

        for (a, b, expected) in cases {
            let result = (a & b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_or() {
        let ctx = MapContext::default();

        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        let cases = [
            (f.clone(), f.clone(), false),
            (f.clone(), t.clone(), true),
            (t.clone(), f.clone(), true),
            (t.clone(), t.clone(), true),
        ];

        for (a, b, expected) in cases {
            let result = (a | b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_xor() {
        let ctx = MapContext::default();

        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        let cases = [
            (f.clone(), f.clone(), false),
            (f.clone(), t.clone(), true),
            (t.clone(), f.clone(), true),
            (t.clone(), t.clone(), false),
        ];

        for (a, b, expected) in cases {
            let result = (a ^ b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_nand() {
        let ctx = MapContext::default();
        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        let cases = [
            (f.clone(), f.clone(), true),
            (f.clone(), t.clone(), true),
            (t.clone(), f.clone(), true),
            (t.clone(), t.clone(), false),
        ];

        for (a, b, expected) in cases {
            let result = a.nand(b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_nor() {
        let ctx = MapContext::default();
        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        let cases = [
            (f.clone(), f.clone(), true),
            (f.clone(), t.clone(), false),
            (t.clone(), f.clone(), false),
            (t.clone(), t.clone(), false),
        ];

        for (a, b, expected) in cases {
            let result = a.nor(b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn test_xnor() {
        let ctx = MapContext::default();
        let t = BoolExpr::true_();
        let f = BoolExpr::false_();

        let cases = [
            (f.clone(), f.clone(), true),
            (f.clone(), t.clone(), false),
            (t.clone(), f.clone(), false),
            (t.clone(), t.clone(), true),
        ];

        for (a, b, expected) in cases {
            let result = a.xnor(b).eval(&ctx).unwrap();
            assert_eq!(result, expected);
        }
    }
}
