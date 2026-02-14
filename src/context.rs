use crate::expr::ExpressionError;
use smol_str::SmolStr;
use std::any::{Any, TypeId};
use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AttributeKey {
    pub name: SmolStr,
    pub type_id: TypeId,
}

impl AttributeKey {
    pub fn new(name: impl Into<SmolStr>, type_id: TypeId) -> Self {
        Self {
            name: name.into(),
            type_id,
        }
    }
}

pub trait EvalContext {
    fn get_any(&self, path: &AttributeKey) -> Result<&dyn Any, ExpressionError>;
}
