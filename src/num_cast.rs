use crate::context::ReadContext;
use crate::expr::{Expr, ExprNode, ExpressionError, SelectExprNodeImpl};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub trait CastFrom<N>: Sized {
    fn cast_from(node: Box<dyn ExprNode<N>>) -> Self;
}

pub struct CastNumPrimitive<NOut, NIn>
where
    NIn: SelectExprNodeImpl,
    NOut: SelectExprNodeImpl,
    //Nd: ExprNode<NIn>
{
    inner: Expr<NIn>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn> CastNumPrimitive<NOut, NIn>
where
    NOut: SelectExprNodeImpl + Copy + 'static,
    NIn: SelectExprNodeImpl + AsPrimitive<NOut> + Copy,
{
    pub fn new(inner: Expr<NIn>) -> Self {
        Self {
            inner,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn> Debug for CastNumPrimitive<NOut, NIn>
where
    NIn: SelectExprNodeImpl,
    NOut: SelectExprNodeImpl,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut> ExprNode<NOut> for CastNumPrimitive<NOut, NIn>
where
    NIn: SelectExprNodeImpl<Property = NIn> + AsPrimitive<NOut> + Send + Sync + Copy,
    NOut: SelectExprNodeImpl + Num + Send + Sync + Copy + 'static,
{
    fn eval(&self, ctx: &dyn ReadContext) -> Result<NOut, ExpressionError> {
        Ok(self.inner.eval_dyn(ctx)?.as_())
    }
}
