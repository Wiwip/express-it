use crate::context::EvalContext;
use crate::expr::{Expr, ExprNode, ExpressionError};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub trait CastFrom<N>: Sized {
    fn cast_from(node: Box<dyn ExprNode<N> + Send + Sync>) -> Self;
}

pub struct CastNumPrimitive<NOut, NIn, Nd: ExprNode<NIn>> {
    inner: Expr<NIn, Nd>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn, Nd> CastNumPrimitive<NOut, NIn, Nd>
where
    NOut: Copy + Send + Sync + 'static,
    NIn: AsPrimitive<NOut> + Copy + Send + Sync,
    Nd: ExprNode<NIn>,
{
    pub fn new(inner: Expr<NIn, Nd>) -> Self {
        Self {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn, Nd> Debug for CastNumPrimitive<NOut, NIn, Nd>
where
    Nd: ExprNode<NIn>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut, Nd> ExprNode<NOut> for CastNumPrimitive<NOut, NIn, Nd>
where
    NIn: AsPrimitive<NOut> + Copy + Send + Sync + 'static,
    NOut: Num + Copy + Send + Sync + 'static,
    Nd: ExprNode<NIn> + Send + Sync + 'static,
{
    fn eval_node(&self, ctx: &dyn EvalContext) -> Result<NOut, ExpressionError> {
        Ok(self.inner.eval_dyn(ctx)?.as_())
    }
}
