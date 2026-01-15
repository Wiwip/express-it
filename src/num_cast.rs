use crate::context::EvalContext;
use crate::expr::{Expr, ExprNode, ExpressionError};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub trait CastFrom<N, Ctx>: Sized {
    fn cast_from(node: Box<dyn ExprNode<N, Ctx>>) -> Self;
}

pub struct CastNumPrimitive<NOut, NIn, Ctx: EvalContext, Nd: ExprNode<NIn, Ctx>> {
    inner: Expr<NIn, Ctx, Nd>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn, Ctx, Nd> CastNumPrimitive<NOut, NIn, Ctx, Nd>
where
    NOut: Copy + Send + Sync + 'static,
    NIn: AsPrimitive<NOut> + Copy + Send + Sync,
    Ctx: EvalContext,
    Nd: ExprNode<NIn, Ctx>,
{
    pub fn new(inner: Expr<NIn, Ctx, Nd>) -> Self {
        Self {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn, Ctx, Nd> Debug for CastNumPrimitive<NOut, NIn, Ctx, Nd>
where
    Ctx: EvalContext,
    Nd: ExprNode<NIn, Ctx>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut, Ctx, Nd> ExprNode<NOut, Ctx> for CastNumPrimitive<NOut, NIn, Ctx, Nd>
where
    NIn: AsPrimitive<NOut> + Copy + Send + Sync + 'static,
    NOut: Num + Copy + Send + Sync + 'static,
    Ctx: EvalContext,
    Nd: ExprNode<NIn, Ctx>,
{
    fn eval(&self, ctx: &Ctx) -> Result<NOut, ExpressionError> {
        Ok(self.inner.eval(ctx)?.as_())
    }
}
