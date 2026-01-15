use crate::expr::ExpressionError;
use num_traits::Num;
use std::fmt::Debug;

pub trait EvalContext: Send + Sync {
    fn get<N: Num>(&self, attribute: &Box<dyn RetrieveAttribute<N, Self>>) -> N;
}

pub trait RetrieveAttribute<N, Ctx: EvalContext>: Debug + Send + Sync {
    fn retrieve(&self, ctx: &Ctx) -> Result<N, ExpressionError>;
}
