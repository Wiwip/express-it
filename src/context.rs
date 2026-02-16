use crate::expr::ExpressionError;
use smol_str::SmolStr;
use std::any::{Any, TypeId};
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AttributeKey {
    pub path: SmolStr,
    pub type_id: TypeId,
}

impl AttributeKey {
    pub fn new<T: 'static>(name: impl Into<SmolStr>) -> Self {
        Self {
            path: name.into(),
            type_id: TypeId::of::<T>(),
        }
    }
}

pub trait ReadContext {
    fn get_any(&self, path: &AttributeKey) -> Result<&dyn Any, ExpressionError>;
    fn get_any_component(&self, path: &str, type_id: TypeId) -> Result<&dyn Any, ExpressionError>;
}

pub trait WriteContext {
    fn write(&mut self, path: &AttributeKey, value: Box<dyn Any>);
}