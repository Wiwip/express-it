use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, SelectExprNodeImpl};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::collections::HashSet;
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
    cast_expr: Expr<NIn>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn> CastNumPrimitive<NOut, NIn>
where
    NOut: SelectExprNodeImpl + Copy + 'static,
    NIn: SelectExprNodeImpl + AsPrimitive<NOut> + Copy,
{
    pub fn new(expr: Expr<NIn>) -> Self {
        Self {
            cast_expr: expr,
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
        Ok(self.cast_expr.eval_dyn(ctx)?.as_())
    }

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        self.cast_expr.inner.get_dependencies(deps);
    }
}
