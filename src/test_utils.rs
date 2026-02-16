use crate::context::{AttributeKey, ReadContext, WriteContext};
use crate::expr::{Expr, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use crate::frame::ExprAttribute;
use crate::integer::{IntExpr, IntExprNode};
use smol_str::ToSmolStr;
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default, Debug)]
pub struct MapContext(pub HashMap<smol_str::SmolStr, HashMap<TypeId, Box<dyn Any>>>);

impl MapContext {
    pub fn insert_src<T: ExprAttribute + 'static>(&mut self, value: T::Property)
    where
        T::Property: Send + Sync,
    {
        self.0
            .entry("src".into())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }

    pub fn insert_dst<T: ExprAttribute + 'static>(&mut self, value: T::Property)
    where
        T::Property: Send + Sync,
    {
        self.0
            .entry("dst".into())
            .or_insert(HashMap::new())
            .insert(TypeId::of::<T>(), Box::new(value));
    }
}

impl ReadContext for MapContext {
    fn get_any(&self, path: &AttributeKey) -> Result<&dyn Any, ExpressionError> {
        let val = self
            .0
            .get(path.path.as_str())
            .and_then(|m| m.get(&path.type_id))
            .ok_or(ExpressionError::MissingAttribute)?;
        Ok(val.as_ref())
    }

    fn get_any_component(
        &self,
        _path: &str,
        _type_id: TypeId,
    ) -> Result<&dyn Any, ExpressionError> {
        todo!()
    }
}

impl WriteContext for MapContext {
    fn write(&mut self, path: &AttributeKey, value: Box<dyn Any>) {
        self.0
            .entry(path.path.to_smolstr())
            .or_insert(HashMap::new())
            .insert(path.type_id, value);
    }
}

#[derive(Debug)]
pub struct F32Attribute;

impl F32Attribute {
    pub fn src() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(AttributeKey::new::<Self>("src"));
        Expr::new(Arc::new(expr))
    }

    pub fn dst() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(AttributeKey::new::<Self>("dst"));
        Expr::new(Arc::new(expr))
    }
}

impl ExprAttribute for F32Attribute {
    type Property = f32;
}

#[derive(Debug)]
pub struct I32Attribute;

impl I32Attribute {
    #[allow(unused)]
    pub fn src() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(AttributeKey::new::<Self>("src"));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(AttributeKey::new::<Self>("dst"));
        Expr::new(Arc::new(expr))
    }
}

impl ExprAttribute for I32Attribute {
    type Property = i32;
}

#[derive(Debug)]
pub struct U32Attribute;

impl U32Attribute {
    #[allow(unused)]
    pub fn src() -> IntExpr<u32> {
        let expr = IntExprNode::Attribute(AttributeKey::new::<Self>("src"));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<u32> {
        let expr = IntExprNode::Attribute(AttributeKey::new::<Self>("dst"));
        Expr::new(Arc::new(expr))
    }
}

impl ExprAttribute for U32Attribute {
    type Property = u32;
}
