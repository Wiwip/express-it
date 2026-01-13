use std::fmt::Debug;
use num_traits::Num;
use crate::expr::ExpressionError;

pub trait EvalContext: Send + Sync {
    fn get<N: Num>(&self, attribute: &Box<dyn RetrieveAttribute<N, Self>>) -> N;
}

pub trait RetrieveAttribute<N: Num, Ctx: EvalContext>: Debug + Send + Sync {
    fn retrieve(&self, ctx: &Ctx) -> Result<N, ExpressionError>;
}