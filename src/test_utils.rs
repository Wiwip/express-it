use crate::context::{Accessor, Path, ReadContext, ScopeId, WriteContext};
use crate::expr::{Expr, ExpressionError};
use crate::float::{FloatExpr, FloatExprNode};
use crate::frame::ExprAttribute;
use crate::integer::{IntExpr, IntExprNode};
use crate::test_utils::scopes::{DST, SRC};
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;

pub mod scopes {
    use super::ScopeId;
    pub const SRC: ScopeId = ScopeId(0);
    pub const DST: ScopeId = ScopeId(1);
}

#[derive(Default, Debug)]
pub struct MapContext(pub HashMap<(ScopeId, u64), Box<dyn Any + Send + Sync>>);

impl MapContext {
    pub fn insert<T: ExprAttribute + 'static>(
        &mut self,
        scope: impl Into<ScopeId>,
        value: T::Property,
    ) where
        T::Property: Send + Sync,
    {
        let path = Path::from_type::<T>(scope);

        self.0.insert((path.scope(), path.path()), Box::new(value));
    }
}

impl ReadContext for MapContext {
    fn get_any(&self, access: &dyn Accessor) -> Result<&dyn Any, ExpressionError> {
        let val = self
            .0
            .get(&(access.scope(), access.path()))
            .ok_or(ExpressionError::MissingAttribute)?;
        Ok(val.as_ref())
    }

    fn get_any_component(
        &self,
        _path: &str,
        _type_id: TypeId,
    ) -> Result<&dyn Any, ExpressionError> {
        unreachable!()
    }
}

impl WriteContext for MapContext {
    fn write(&mut self, access: &dyn Accessor, value: Box<dyn Any + Send + Sync>) {
        self.0.insert(access.key(), value);
    }
}

#[derive(Debug)]
pub struct F32Attribute;

impl F32Attribute {
    pub fn src() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Path::from_type::<Self>(SRC));
        Expr::new(Arc::new(expr))
    }

    pub fn dst() -> FloatExpr<f32> {
        let expr = FloatExprNode::Attribute(Path::from_type::<Self>(DST));
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
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(SRC));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<i32> {
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(DST));
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
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(SRC));
        Expr::new(Arc::new(expr))
    }

    #[allow(unused)]
    pub fn dst() -> IntExpr<u32> {
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(DST));
        Expr::new(Arc::new(expr))
    }
}

impl ExprAttribute for U32Attribute {
    type Property = u32;
}
