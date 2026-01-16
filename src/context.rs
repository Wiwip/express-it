use crate::expr::ExpressionError;
use std::fmt::Debug;

pub trait EvalContext: Send + Sync {
    //fn get<N: Num>(&self, attribute: &Box<dyn RetrieveAttribute<N>>) -> N;
    fn as_any(&self) -> &dyn std::any::Any;
}

pub trait RetrieveAttribute<N>: Debug + Send + Sync {
    //fn retrieve(&self) -> Result<N, ExpressionError>;
    fn retrieve(&self, ctx: &dyn EvalContext) -> Result<N, ExpressionError>;
}
