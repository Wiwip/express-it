use crate::context::{Accessor, Path, ReadContext, ScopeId, WriteContext};
use crate::expr::{Expr, ExprNode, ExpressionError, SelectExprNodeImpl};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;

trait Context: ReadContext + WriteContext {}

pub trait ExprAttribute {
    type Property: Debug;
}

pub struct Assignment<N: SelectExprNodeImpl<Property = N>> {
    pub path: Path,
    pub expr: Expr<N>,
}

trait StepExecutor: Send + Sync {
    fn run(&self, read: &mut dyn Context);
}

impl<N: SelectExprNodeImpl<Property = N> + Send + Sync + 'static> StepExecutor for Assignment<N> {
    fn run(&self, ctx: &mut dyn Context) {
        let result = self.expr.inner.eval(ctx).unwrap_or_else(|_| {
            panic!(
                "Executor failed. {} value could not be found in context.",
                self.path
            )
        });
        let _handle_error = ctx.write(&self.path, Box::new(result));
    }
}

/// A context with the purpose of intercepting read-write calls and provide temporary values
/// Reads from the buffer first, and read from the real context if not present in the buffer.
/// Writes-back to the world only when commit is called.
pub struct CachedEvalContext<'a, R> {
    read_ctx: &'a R,
    shadow: HashMap<(ScopeId, u64), Box<dyn Any + Send + Sync>>,
}

impl<'a, Ctx> CachedEvalContext<'a, Ctx> {
    pub fn new(read_ctx: &'a Ctx) -> Self {
        Self {
            read_ctx,
            shadow: Default::default(),
        }
    }

    pub fn write() {}

    pub fn into_output(self) -> PlanResults {
        PlanResults {
            shadow: self.shadow,
        }
    }
}

impl<RW: ReadContext> Context for CachedEvalContext<'_, RW> {}

impl<R: ReadContext> ReadContext for CachedEvalContext<'_, R> {
    fn get_any(&self, access: &dyn Accessor) -> Result<&dyn Any, ExpressionError> {
        if let Some(value) = self.shadow.get(&access.key()) {
            Ok(value.as_ref())
        } else {
            self.read_ctx.get_any(access)
        }
    }

    fn get_any_component(
        &self,
        _path: ScopeId,
        _type_id: std::any::TypeId,
    ) -> Result<&dyn Any, ExpressionError> {
        unreachable!()
    }
}

impl<W> WriteContext for CachedEvalContext<'_, W> {
    fn write(
        &mut self,
        access: &dyn Accessor,
        value: Box<dyn Any + Send + Sync>,
    ) -> Result<(), ExpressionError> {
        self.shadow.insert(access.key(), value);
        Ok(())
    }
}

pub struct Step {
    exprs: Vec<Box<dyn StepExecutor>>,
}

impl<N> From<Assignment<N>> for Step
where
    N: SelectExprNodeImpl<Property = N> + Send + Sync + 'static,
    //Nd: ExprNode<N> + 'static,
{
    fn from(value: Assignment<N>) -> Self {
        Step {
            exprs: vec![Box::new(value)],
        }
    }
}

impl StepExecutor for Step {
    fn run(&self, ctx: &mut dyn Context) {
        for expr in &self.exprs {
            expr.run(ctx);
        }
    }
}

macro_rules! impl_step_from_tuple {
        ($($t:ident),+ $(,)?) => {
            impl<$($t),+> From<($($t,)+)> for Step
            where
                $($t: StepExecutor + 'static,)+
            {
                #[allow(non_snake_case)]
                fn from(value: ($($t,)+)) -> Step {
                    let ($($t,)+) = value;
                    Step {
                        exprs: vec![$(Box::new($t)),+],
                    }
                }
            }
        };
    }

impl_step_from_tuple!(T1, T2);
impl_step_from_tuple!(T1, T2, T3);
impl_step_from_tuple!(T1, T2, T3, T4);
impl_step_from_tuple!(T1, T2, T3, T4, T5);
impl_step_from_tuple!(T1, T2, T3, T4, T5, T6);
impl_step_from_tuple!(T1, T2, T3, T4, T5, T6, T7);
impl_step_from_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);

pub struct LazyPlan {
    plan: Vec<Step>,
}

impl LazyPlan {
    pub fn new() -> Self {
        Self { plan: vec![] }
    }

    pub fn step(mut self, step: impl Into<Step>) -> Self {
        self.plan.push(step.into());
        self
    }

    /// Phase 1: evaluate using only a read context, producing an owned output buffer.
    pub fn eval<R: ReadContext>(&self, read: &R) -> PlanResults {
        let mut shadow_eval = CachedEvalContext::new(read);

        for step in &self.plan {
            step.run(&mut shadow_eval);
        }

        shadow_eval.into_output()
    }

    /// Phase 2: flush buffered writes into a write context.
    pub fn flush<W: WriteContext>(&self, output: PlanResults, write: &mut W) {
        output.flush_into(write);
    }

    /// Commit the expression plan
    pub fn commit<RW: ReadContext + WriteContext>(&self, ctx: &mut RW) {
        let output = self.eval(ctx);
        self.flush(output, ctx);
    }
}

/// Owned result of evaluating a plan: a set of buffered writes.
/// This is intentionally detached from the original read context so we can
/// drop all read borrows before flushing into a write context.
pub struct PlanResults {
    shadow: HashMap<(ScopeId, u64), Box<dyn Any + Send + Sync>>,
}

impl PlanResults {
    pub fn flush_into<W: WriteContext>(mut self, write: &mut W) {
        for (key, value) in self.shadow.drain() {
            let path = Path {
                scope: key.0,
                id: key.1,
            };
            let _ = write.write(&path, value);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Path;
    use crate::expr::{Expr, ExprNode};
    use crate::float::FloatExprNode;
    use crate::frame::LazyPlan;
    use crate::test_utils::scopes::{DST, SRC};
    use crate::test_utils::{Atk, Def, Hp, MapContext};

    #[test]
    fn test_sequential_ops() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 10.0);

        ctx.insert::<Hp>(DST, 20.0);
        ctx.insert::<Def>(DST, 2.0);

        let dmg_taken_expr = Atk::get(SRC) - Def::get(DST);
        let new_hp_expr = Hp::get(DST) - dmg_taken_expr.clone().max(0.0);

        let lp = LazyPlan::new().step(Hp::set(DST, new_hp_expr));

        lp.commit(&mut ctx);

        let expr = FloatExprNode::Attribute(Path::from_type::<Hp>(DST));
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 12.0);
    }

    #[test]
    fn test_interim_ops() {
        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 10.0);

        ctx.insert::<Hp>(DST, 20.0);
        ctx.insert::<Def>(DST, 2.0);

        let dmg_taken_expr = Atk::get(SRC) - Def::get(DST);
        let get_dmg_taken: Expr<f32> =
            FloatExprNode::Attribute(Path::from_name(DST, "dmg_taken")).into();

        let lp = LazyPlan::new()
            .step(dmg_taken_expr.max(0.0).alias(DST, "dmg_taken"))
            .step(Hp::set(DST, Hp::get(DST) - get_dmg_taken));

        lp.commit(&mut ctx);

        let expr = FloatExprNode::<f32>::Attribute(Path::from_name(DST, "dmg_taken"));
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 8.0);

        let expr = FloatExprNode::Attribute(Path::from_type::<Hp>(DST));
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 12.0);
    }
}
