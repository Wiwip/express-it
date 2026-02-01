use crate::expr::ExpressionError;
use std::any::{Any, TypeId};
use std::fmt::Debug;

pub trait EvalContext {
    fn get_any(
        &self,
        path: &Path,
        type_id: TypeId,
    ) -> Result<&dyn Any, ExpressionError>;
}

pub trait RetrieveAttribute<N>: Debug + Send + Sync {
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError>;
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Path(pub String);

impl From<&str> for Path {
    fn from(s: &str) -> Self {
        Path(s.into())
    }
}
