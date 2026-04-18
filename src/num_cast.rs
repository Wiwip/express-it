use crate::context::{Path, ReadContext};
use crate::expr::{Expr, ExprNode, ExpressionError, SelectExprNodeImpl};
use num_traits::{AsPrimitive, Num};
use std::any::type_name;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

pub trait CastFrom<N, Ctx>: Sized {
    fn cast_from(node: Box<dyn ExprNode<N, Ctx>>) -> Self;
}

pub struct CastNumPrimitive<NOut, NIn, Ctx>
where
    NIn: SelectExprNodeImpl<Ctx>,
    NOut: SelectExprNodeImpl<Ctx>,
    Ctx: ReadContext,
{
    cast_expr: Expr<NIn, Ctx>,
    phantom: PhantomData<NOut>,
}

impl<NOut, NIn, Ctx> CastNumPrimitive<NOut, NIn, Ctx>
where
    NOut: SelectExprNodeImpl<Ctx> + Copy + 'static,
    NIn: SelectExprNodeImpl<Ctx> + AsPrimitive<NOut> + Copy,
    Ctx: ReadContext,
{
    pub fn new(expr: Expr<NIn, Ctx>) -> Self {
        Self {
            cast_expr: expr,
            phantom: Default::default(),
        }
    }
}

impl<NOut, NIn, Ctx> Debug for CastNumPrimitive<NOut, NIn, Ctx>
where
    NIn: SelectExprNodeImpl<Ctx>,
    NOut: SelectExprNodeImpl<Ctx>,
    Ctx: ReadContext,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AsFloat<{},{}>", type_name::<NOut>(), type_name::<NIn>())
    }
}

impl<NIn, NOut, Ctx: ReadContext> ExprNode<NOut, Ctx> for CastNumPrimitive<NOut, NIn, Ctx>
where
    NIn: SelectExprNodeImpl<Ctx, Property = NIn> + AsPrimitive<NOut> + Send + Sync + Copy,
    NOut: SelectExprNodeImpl<Ctx> + Num + Send + Sync + Copy + 'static,
    Ctx: ReadContext,
{
    fn eval(&self, ctx: &Ctx) -> Result<NOut, ExpressionError> {
        Ok(self.cast_expr.inner.eval(ctx)?.as_())
    }

    fn eval_dyn(&self, ctx: &dyn ReadContext) -> Result<NOut, ExpressionError> {
        Ok(self.cast_expr.inner.eval_dyn(ctx)?.as_())
    }

    fn get_dependencies(&self, deps: &mut HashSet<Path>) {
        self.cast_expr.inner.get_dependencies(deps);
    }
}
