use crate::context::{Path, ReadContext, SubjectId, WriteContext};
use crate::expr::{Expr, ExprSchema, ExpressionError};
use crate::float::FloatExprNode;
use crate::frame::Assignment;
use crate::integer::IntExprNode;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

pub mod scopes {
    use super::SubjectId;
    pub const SRC: SubjectId = SubjectId(0);
    pub const DST: SubjectId = SubjectId(1);
    pub const ERROR_SCOPE: SubjectId = SubjectId(255);
}

pub trait ExprAttribute {
    type Property: Debug;
}

#[derive(Debug, Clone, Copy)]
pub struct MapSchema;

impl ExprSchema for MapSchema {
    type Context<'w, 's> = MapContext where 's: 'w;
}

#[derive(Default, Debug)]
pub struct MapContext(pub HashMap<Path, Box<dyn Any + Send + Sync>>);

impl MapContext {
    pub fn insert<T: ExprAttribute + 'static>(
        &mut self,
        scope: impl Into<SubjectId>,
        value: T::Property,
    ) where
        T::Property: Send + Sync,
    {
        let path = Path::from_type_name::<T>(scope);

        self.0.insert(path, Box::new(value));
    }
}

impl ReadContext for MapContext {
    fn get_any(&self, path: &Path) -> Result<&dyn Any, ExpressionError> {
        let val = self
            .0
            .get(path)
            .ok_or(ExpressionError::MissingAttribute)?;
        Ok(val.as_ref())
    }
}

impl WriteContext for MapContext {
    fn write(
        &mut self,
        path: &Path,
        value: Box<dyn Any + Send + Sync>,
    ) -> Result<(), ExpressionError> {
        self.0.insert(path.clone(), value);
        Ok(())
    }
}

pub struct Atk;

impl Atk {
    #[allow(dead_code)]
    pub fn set(
        key: impl Into<SubjectId>,
        expr: Expr<f32, MapSchema>,
    ) -> Assignment<f32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<f32, MapSchema> {
        let expr = FloatExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Atk {
    type Property = f32;
}

pub struct Def;

impl Def {
    #[allow(dead_code)]
    pub fn set(key: impl Into<SubjectId>, expr: Expr<f32, MapSchema>) -> Assignment<f32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<f32, MapSchema> {
        let expr = FloatExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Def {
    type Property = f32;
}

pub struct Hp;

impl Hp {
    pub fn set(key: impl Into<SubjectId>, expr: Expr<f32, MapSchema>) -> Assignment<f32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<f32, MapSchema> {
        let expr = FloatExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for Hp {
    type Property = f32;
}

pub struct IntAtk;

impl IntAtk {
    #[allow(dead_code)]
    pub fn set(key: impl Into<SubjectId>, expr: Expr<u32, MapSchema>) -> Assignment<u32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<u32, MapSchema> {
        let expr = IntExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntAtk {
    type Property = u32;
}

pub struct IntDef;

impl IntDef {
    #[allow(dead_code)]
    pub fn set(key: impl Into<SubjectId>, expr: Expr<i32, MapSchema>) -> Assignment<i32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<i32, MapSchema> {
        let expr = IntExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntDef {
    type Property = i32;
}

pub struct IntHp;

impl IntHp {
    #[allow(dead_code)]
    pub fn set(key: impl Into<SubjectId>, expr: Expr<u32, MapSchema>) -> Assignment<u32, MapSchema> {
        let path = Path::from_type_name::<Self>(key.into());
        Assignment { path, expr }
    }
    pub fn get(scope: impl Into<SubjectId>) -> Expr<u32, MapSchema> {
        let expr = IntExprNode::Attribute(Path::from_type_name::<Self>(scope));
        Expr::new(Arc::new(expr))
    }
}
impl ExprAttribute for IntHp {
    type Property = u32;
}
