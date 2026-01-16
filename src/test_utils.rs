use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use crate::integer::{IntExpr, IntExprNode};
use num_traits::AsPrimitive;
use std::any::Any;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
pub enum Val {
    Int(i128),
    Float(f64),
}

impl Val {
    /// Attempt to cast the value to a specific numeric type
    pub fn cast_to<N>(self) -> Result<N, ExpressionError>
    where
        f64: AsPrimitive<N>,
        i128: AsPrimitive<N>,
        N: 'static + Copy,
    {
        match self {
            Val::Float(f) => Ok(f.as_()),
            Val::Int(i) => Ok(i.as_()),
        }
    }
}

#[derive(Default)]
pub struct MapContext(pub HashMap<String, Val>);

impl MapContext {
    fn get_value(&self, key: &str) -> Result<Val, ExpressionError> {
        self.0
            .get(key)
            .copied()
            .ok_or(ExpressionError::AttributeNotFound)
    }
}

impl EvalContext for MapContext {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug, Clone)]
pub struct StrAttr(pub String);

impl StrAttr {
    pub fn f32(val: &str) -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Box::new(StrAttr(val.into())));
        Expr::new(Arc::new(expr))
    }

    pub fn i32(val: &str) -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Box::new(StrAttr(val.into())));
        Expr::new(Arc::new(expr))
    }
}

impl ExprNode<f32> for StrAttr {
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<f32, ExpressionError> {
        Ok(self.retrieve(ctx)?)
    }
}

impl ExprNode<i32> for StrAttr {
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<i32, ExpressionError> {
        Ok(self.retrieve(ctx)?)
    }
}

impl RetrieveAttribute<f32> for StrAttr {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<f32, ExpressionError> {
        let game_ctx = ctx.as_any()
            .downcast_ref::<MapContext>()
            .ok_or(ExpressionError::InvalidTypes)?;

        game_ctx.get_value(&self.0)?.cast_to::<f32>()
    }
}

impl RetrieveAttribute<i32> for StrAttr {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<i32, ExpressionError> {
        let game_ctx = ctx.as_any()
            .downcast_ref::<MapContext>()
            .ok_or(ExpressionError::InvalidTypes)?;

        game_ctx.get_value(&self.0)?.cast_to::<i32>()
    }
}
