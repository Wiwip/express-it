use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExprSchema, ExpressionError, IfThenNode, SelectExprNodeImpl};
use crate::logic::BoolExpr;
use crate::num_cast::CastFrom;
use std::collections::HashSet;
use std::sync::Arc;

// ---------------------------------------------------------------------------
// Kind trait: associates an `N` (number type) with its op enums.
// ---------------------------------------------------------------------------

pub trait NumExprKind<N: 'static>: 'static {
    type UnaryOp: Send + Sync + 'static;
    type BinaryOp: Send + Sync + 'static;
    type TrinaryOp: Send + Sync + 'static;
}

// ---------------------------------------------------------------------------
// Op-evaluation traits (one impl per op enum per N family).
// ---------------------------------------------------------------------------

pub trait UnaryOpEval<N> {
    fn eval(&self, value: N) -> Result<N, ExpressionError>;
}

pub trait BinaryOpEval<N> {
    fn eval(&self, l: N, r: N) -> Result<N, ExpressionError>;
}

pub trait TrinaryOpEval<N> {
    fn eval(&self, val: N, arg1: N, arg2: N) -> Result<N, ExpressionError>;
}

pub enum NumericExprNode<N, S, K>
where
    N: SelectExprNodeImpl<S, Property = N> + Send + Sync + 'static,
    S: ExprSchema,
    K: NumExprKind<N>,
{
    Lit(N),
    Attribute(Path),
    Cast(Box<dyn ExprNode<N, S>>),
    UnaryOp {
        op: K::UnaryOp,
        expr: Expr<N, S>,
    },
    BinaryOp {
        lhs_expr: Expr<N, S>,
        op: K::BinaryOp,
        rhs_expr: Expr<N, S>,
    },
    TrinaryOp {
        value_expr: Expr<N, S>,
        op: K::TrinaryOp,
        arg1_expr: Expr<N, S>,
        arg2_expr: Expr<N, S>,
    },
    IfThenElseOp {
        bool_expr: BoolExpr<S>,
        arg1_expr: Expr<N, S>,
        arg2_expr: Expr<N, S>,
    },
    ErrorHandlingOp {
        expr: Expr<N, S>,
        or_expr: Expr<N, S>,
    },
}

impl<N, S, K> From<NumericExprNode<N, S, K>> for Expr<N, S>
where
    N: SelectExprNodeImpl<S, Property = N, Node = NumericExprNode<N, S, K>> + Send + Sync + 'static,
    S: ExprSchema,
    K: NumExprKind<N>,
    K::UnaryOp: UnaryOpEval<N>,
    K::BinaryOp: BinaryOpEval<N>,
    K::TrinaryOp: TrinaryOpEval<N>,
{
    fn from(node: NumericExprNode<N, S, K>) -> Self {
        Expr::new(Arc::new(node))
    }
}

impl<N, S, K> ExprNode<N, S> for NumericExprNode<N, S, K>
where
    N: SelectExprNodeImpl<S, Property = N> + Copy + Send + Sync + 'static,
    S: ExprSchema,
    K: NumExprKind<N>,
    K::UnaryOp: UnaryOpEval<N>,
    K::BinaryOp: BinaryOpEval<N>,
    K::TrinaryOp: TrinaryOpEval<N>,
{
    fn eval<'w, 's>(&self, ctx: &S::Context<'w, 's>) -> Result<N, ExpressionError> {
        match self {
            NumericExprNode::Lit(lit) => Ok(*lit),
            NumericExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;
                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            NumericExprNode::Cast(cast) => cast.eval(ctx),
            NumericExprNode::UnaryOp { op, expr } => {
                let v = expr.eval(ctx)?;
                op.eval(v)
            }
            NumericExprNode::BinaryOp {
                lhs_expr,
                op,
                rhs_expr,
            } => {
                let l = lhs_expr.eval(ctx)?;
                let r = rhs_expr.eval(ctx)?;
                op.eval(l, r)
            }
            NumericExprNode::TrinaryOp {
                value_expr,
                op,
                arg1_expr,
                arg2_expr,
            } => {
                let v = value_expr.eval(ctx)?;
                let a1 = arg1_expr.eval(ctx)?;
                let a2 = arg2_expr.eval(ctx)?;
                op.eval(v, a1, a2)
            }
            NumericExprNode::IfThenElseOp {
                bool_expr,
                arg1_expr,
                arg2_expr,
            } => {
                if bool_expr.eval(ctx)? {
                    arg1_expr.eval(ctx)
                } else {
                    arg2_expr.eval(ctx)
                }
            }
            NumericExprNode::ErrorHandlingOp { expr, or_expr } => match expr.inner.eval(ctx) {
                Ok(v) => Ok(v),
                Err(_) => or_expr.eval(ctx),
            },
        }
    }

    fn eval_dyn(&self, ctx: &dyn ReadContext) -> Result<N, ExpressionError> {
        match self {
            NumericExprNode::Lit(lit) => Ok(*lit),
            NumericExprNode::Attribute(key) => {
                let value = ctx.get_any(key)?;
                if let Some(val) = value.downcast_ref::<N>() {
                    Ok(*val)
                } else {
                    Err(ExpressionError::InvalidTypes)
                }
            }
            NumericExprNode::Cast(cast) => cast.eval_dyn(ctx),
            NumericExprNode::UnaryOp { op, expr } => {
                let v = expr.inner.eval_dyn(ctx)?;
                op.eval(v)
            }
            NumericExprNode::BinaryOp {
                lhs_expr,
                op,
                rhs_expr,
            } => {
                let l = lhs_expr.inner.eval_dyn(ctx)?;
                let r = rhs_expr.inner.eval_dyn(ctx)?;
                op.eval(l, r)
            }
            NumericExprNode::TrinaryOp {
                value_expr,
                op,
                arg1_expr,
                arg2_expr,
            } => {
                let v = value_expr.inner.eval_dyn(ctx)?;
                let a1 = arg1_expr.inner.eval_dyn(ctx)?;
                let a2 = arg2_expr.inner.eval_dyn(ctx)?;
                op.eval(v, a1, a2)
            }
            NumericExprNode::IfThenElseOp {
                bool_expr,
                arg1_expr,
                arg2_expr,
            } => {
                if bool_expr.inner.eval_dyn(ctx)? {
                    arg1_expr.inner.eval_dyn(ctx)
                } else {
                    arg2_expr.inner.eval_dyn(ctx)
                }
            }
            NumericExprNode::ErrorHandlingOp { expr, or_expr } => match expr.inner.eval_dyn(ctx) {
                Ok(v) => Ok(v),
                Err(_) => or_expr.inner.eval_dyn(ctx),
            },
        }
    }

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        match self {
            NumericExprNode::Lit(_) => {}
            NumericExprNode::Attribute(path) => {
                deps.insert(path.clone());
            }
            NumericExprNode::Cast(cast) => cast.get_dependencies(deps),
            NumericExprNode::UnaryOp { expr, .. } => expr.inner.get_dependencies(deps),
            NumericExprNode::BinaryOp {
                lhs_expr, rhs_expr, ..
            } => {
                lhs_expr.inner.get_dependencies(deps);
                rhs_expr.inner.get_dependencies(deps);
            }
            NumericExprNode::TrinaryOp {
                value_expr,
                arg1_expr,
                arg2_expr,
                ..
            } => {
                value_expr.inner.get_dependencies(deps);
                arg1_expr.inner.get_dependencies(deps);
                arg2_expr.inner.get_dependencies(deps);
            }
            NumericExprNode::IfThenElseOp {
                bool_expr,
                arg1_expr,
                arg2_expr,
            } => {
                bool_expr.inner.get_dependencies(deps);
                arg1_expr.inner.get_dependencies(deps);
                arg2_expr.inner.get_dependencies(deps);
            }
            NumericExprNode::ErrorHandlingOp { expr, or_expr } => {
                expr.inner.get_dependencies(deps);
                or_expr.inner.get_dependencies(deps);
            }
        }
    }
}

impl<N, S, K> IfThenNode<N, S> for NumericExprNode<N, S, K>
where
    N: SelectExprNodeImpl<S, Property = N, Node = NumericExprNode<N, S, K>>
        + Copy
        + Send
        + Sync
        + 'static,
    S: ExprSchema,
    K: NumExprKind<N>,
    K::UnaryOp: UnaryOpEval<N>,
    K::BinaryOp: BinaryOpEval<N>,
    K::TrinaryOp: TrinaryOpEval<N>,
{
    fn if_then(bool_expr: BoolExpr<S>, t: Expr<N, S>, f: Expr<N, S>) -> Self {
        NumericExprNode::IfThenElseOp {
            bool_expr,
            arg1_expr: t,
            arg2_expr: f,
        }
    }
}

impl<N, S, K> CastFrom<N, S> for NumericExprNode<N, S, K>
where
    N: SelectExprNodeImpl<S, Property = N, Node = NumericExprNode<N, S, K>> + Send + Sync + 'static,
    S: ExprSchema,
    K: NumExprKind<N>,
{
    fn cast_from(node: Box<dyn ExprNode<N, S>>) -> Self {
        NumericExprNode::Cast(node)
    }
}
