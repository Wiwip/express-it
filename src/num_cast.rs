use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExprSchema, ExpressionError, SelectExprNodeImpl};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub trait CastFrom<N, S>: Sized {
    fn cast_from(node: Box<dyn ExprNode<N, S>>) -> Self;
}

pub struct CastNumPrimitive<NOut, NIn, S>
where
    NIn: SelectExprNodeImpl<S>,
    NOut: SelectExprNodeImpl<S>,
    S: ExprSchema,
{
    cast_expr: Expr<NIn, S>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn, S> CastNumPrimitive<NOut, NIn, S>
where
    NOut: SelectExprNodeImpl<S> + Copy + 'static,
    NIn: SelectExprNodeImpl<S> + AsPrimitive<NOut> + Copy,
    S: ExprSchema,
{
    pub fn new(expr: Expr<NIn, S>) -> Self {
        Self {
            cast_expr: expr,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn, S> Debug for CastNumPrimitive<NOut, NIn, S>
where
    NIn: SelectExprNodeImpl<S>,
    NOut: SelectExprNodeImpl<S>,
    S: ExprSchema,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut, S> ExprNode<NOut, S> for CastNumPrimitive<NOut, NIn, S>
where
    NIn: SelectExprNodeImpl<S, Property = NIn> + AsPrimitive<NOut> + Send + Sync + Copy,
    NOut: SelectExprNodeImpl<S> + Num + Send + Sync + Copy + 'static,
    S: ExprSchema,
{
    fn eval<'w, 's>(&self, ctx: &S::Context<'w, 's>) -> Result<NOut, ExpressionError> {
        Ok(self.cast_expr.inner.eval(ctx)?.as_())
    }

    fn eval_dyn(&self, ctx: &dyn ReadContext) -> Result<NOut, ExpressionError> {
        Ok(self.cast_expr.inner.eval_dyn(ctx)?.as_())
    }

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        self.cast_expr.inner.get_dependencies(deps);
    }
}
