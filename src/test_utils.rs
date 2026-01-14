use crate::context::{EvalContext, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use num_traits::{AsPrimitive, Num};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
pub enum Val {
    //Int(i128),
    //UInt(u128),
    Float(f64),
    //Bool(bool),
}

impl Val {
    /// Attempt to cast the value to a specific numeric type
    pub fn cast_to<N>(self) -> Result<N, ExpressionError>
    where
        f64: AsPrimitive<N>,
        N: 'static + Copy,
    {
        match self {
            Val::Float(f) => Ok(f.as_()),
            //Val::Bool(_) => Err(ExpressionError::InvalidTypes),
            //Val::Int(_) => Err(ExpressionError::InvalidTypes),
            //Val::UInt(_) => Err(ExpressionError::InvalidTypes),
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
    fn get<N: Num>(&self, attribute: &Box<dyn RetrieveAttribute<N, Self>>) -> N {
        attribute
            .retrieve(self)
            .expect("Attribute retrieval failed")
    }
}

#[derive(Debug, Clone)]
pub struct FloatAttr(pub String);

impl FloatAttr {
    pub fn f32(val: &str) -> FloatExpr<f32, MapContext> {
        let expr = FloatExprNode::Attribute(Box::new(FloatAttr(val.into())));
        Expr::new(Arc::new(expr))
    }
}

impl ExprNode<f32, MapContext> for FloatAttr {
    fn eval(&self, ctx: &MapContext) -> Result<f32, ExpressionError> {
        Ok(self.retrieve(ctx)?)
    }
}

impl RetrieveAttribute<f32, MapContext> for FloatAttr {
    fn retrieve(&self, ctx: &MapContext) -> Result<f32, ExpressionError> {
        ctx.get_value(&self.0)?.cast_to::<f32>()
    }
}
