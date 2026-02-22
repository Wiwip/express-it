use crate::context::{Accessor, Path, ReadContext, ScopeId, WriteContext};
use crate::expr::{Expr, ExpressionError};
use crate::float::FloatExprNode;
use crate::frame::{Assignment, ExprAttribute};
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::sync::Arc;
use crate::integer::IntExprNode;

pub mod scopes {
    use super::ScopeId;
    pub const SRC: ScopeId = ScopeId(0);
    pub const DST: ScopeId = ScopeId(1);
    pub const ERROR_SCOPE: ScopeId = ScopeId(255);
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
        _path: ScopeId,
        _type_id: TypeId,
    ) -> Result<&dyn Any, ExpressionError> {
        unreachable!()
    }
}

impl WriteContext for MapContext {
    fn write(
        &mut self,
        access: &dyn Accessor,
        value: Box<dyn Any + Send + Sync>,
    ) -> Result<(), ExpressionError> {
        self.0.insert(access.key(), value);
        Ok(())
    }
}

pub struct Atk;

impl Atk {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<f32>,
    ) -> Assignment<f32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<f32> {
        let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Atk {
    type Property = f32;
}

pub struct Def;

impl Def {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<f32>,
    ) -> Assignment<f32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<f32> {
        let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Def {
    type Property = f32;
}

pub struct Hp;

impl Hp {
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<f32>,
    ) -> Assignment<f32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<f32> {
        let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Hp {
    type Property = f32;
}

pub struct IntAtk;

impl IntAtk {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<u32>,
    ) -> Assignment<u32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<u32> {
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntAtk {
    type Property = u32;
}

pub struct IntDef;

impl IntDef {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<i32>,
    ) -> Assignment<i32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<i32> {
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntDef {
    type Property = i32;
}

pub struct IntHp;

impl IntHp {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<ScopeId>,
        expr: Expr<u32>,
    ) -> Assignment<u32> {
        let path = Path::from_type::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<ScopeId>) -> Expr<u32> {
        let expr = IntExprNode::Attribute(Path::from_type::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntHp {
    type Property = u32;
}