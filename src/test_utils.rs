use crate::context::{EvalContext, Path, RetrieveAttribute};
use crate::expr::{Expr, ExprNode, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use crate::integer::{IntExpr, IntExprNode};
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;

pub trait ExprAttribute {
    type Property;
    type ExprType: ExprNode<Self::Property>;

    fn value(&self, ctx: &dyn EvalContext) -> Result<Self::Property, ExpressionError>;
}

#[derive(Default, Debug)]
pub struct MapContext(pub HashMap<Path, HashMap<TypeId, Box<dyn Any>>>);

impl MapContext {
    pub fn insert_src<T: ExprAttribute + 'static>(&mut self, value: T::Property) {
        self.0
            .entry(Path("src".into()).to_owned())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }

    pub fn insert_dst<T: ExprAttribute + 'static>(&mut self, value: T::Property) {
        self.0
            .entry(Path("dst".into()).to_owned())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }
}

impl EvalContext for MapContext {
    fn get_any(&self, path: &Path, type_id: TypeId) -> Result<&dyn Any, ExpressionError> {
        let val = self
            .0
            .get(path)
            .and_then(|m| m.get(&type_id))
            .ok_or(ExpressionError::MissingAttribute)?;
        Ok(val.as_ref())
    }
}

#[derive(Debug)]
pub struct F32Attribute(Path);

impl F32Attribute {
    pub fn src() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Box::new(F32Attribute("src".into())));
        Expr::new(Arc::new(expr))
    }

    pub fn dst() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Box::new(F32Attribute("dst".into())));
        Expr::new(Arc::new(expr))
    }
}

impl RetrieveAttribute<f32> for F32Attribute {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<f32, ExpressionError> {
        self.value(ctx)
    }
}

impl ExprAttribute for F32Attribute {
    type Property = f32;
    type ExprType = FloatExprNode<f32>;

    fn value(&self, ctx: &dyn EvalContext) -> Result<Self::Property, ExpressionError> {
        let any = ctx
            .get_any(&self.0, TypeId::of::<Self>())
            .or(Err(ExpressionError::MissingAttribute))?;

        let value = any
            .downcast_ref::<Self::Property>()
            .ok_or(ExpressionError::DowncastError);

        Ok(*value?)
    }
}

#[derive(Debug)]
pub struct I32Attribute(Path);

impl I32Attribute {
    #[allow(unused)]
    pub fn src() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Box::new(I32Attribute("src".into())));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Box::new(I32Attribute("dst".into())));
        Expr::new(Arc::new(expr))
    }
}

impl RetrieveAttribute<i32> for I32Attribute {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<i32, ExpressionError> {
        let any = ctx.get_any(&self.0, TypeId::of::<Self>())?;

        let value = any
            .downcast_ref::<i32>()
            .ok_or(ExpressionError::InvalidTypes)?;

        Ok(*value)
    }
}

impl ExprAttribute for I32Attribute {
    type Property = i32;
    type ExprType = IntExprNode<i32>;

    fn value(&self, ctx: &dyn EvalContext) -> Result<Self::Property, ExpressionError> {
        let any = ctx
            .get_any(&self.0, TypeId::of::<Self>())
            .or(Err(ExpressionError::MissingAttribute))?;

        let value = any
            .downcast_ref::<Self::Property>()
            .ok_or(ExpressionError::DowncastError);

        Ok(*value?)
    }
}


#[derive(Debug)]
pub struct U32Attribute(Path);

impl U32Attribute {
    #[allow(unused)]
    pub fn src() -> IntExpr<u32> {
        let expr = IntExprNode::Attribute(Box::new(U32Attribute("src".into())));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<u32> {
        let expr = IntExprNode::Attribute(Box::new(U32Attribute("dst".into())));
        Expr::new(Arc::new(expr))
    }
}

impl RetrieveAttribute<u32> for U32Attribute {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<u32, ExpressionError> {
        let any = ctx.get_any(&self.0, TypeId::of::<Self>())?;

        let value = any
            .downcast_ref::<u32>()
            .ok_or(ExpressionError::InvalidTypes)?;

        Ok(*value)
    }
}

impl ExprAttribute for U32Attribute {
    type Property = u32;
    type ExprType = IntExprNode<u32>;

    fn value(&self, ctx: &dyn EvalContext) -> Result<Self::Property, ExpressionError> {
        let any = ctx
            .get_any(&self.0, TypeId::of::<Self>())
            .or(Err(ExpressionError::MissingAttribute))?;

        let value = any
            .downcast_ref::<Self::Property>()
            .ok_or(ExpressionError::DowncastError);

        Ok(*value?)
    }
}
