use crate::context::{Accessor, Path, ReadContext, ScopeId, WriteContext};
use crate::expr::{Expr, ExprNode, ExprSchema, ExpressionError, SelectExprNodeImpl};
use log::trace;
use num_traits::Num;
use std::any::Any;
use std::collections::HashMap;

trait Context: ReadContext + WriteContext {}

pub struct Assignment<N: SelectExprNodeImpl<S>, S: ExprSchema> {
    pub path: Path,
    pub expr: Expr<N, S>,
}

trait StepExecutor: Send + Sync + 'static {
    fn run(&self, read: &mut dyn Context) -> Result<(), ExpressionError>;
}

impl<N, S> StepExecutor for Assignment<N, S>
where
    N: SelectExprNodeImpl<S> + Send + Sync + 'static,
    S: ExprSchema,
{
    fn run(&self, ctx: &mut dyn Context) -> Result<(), ExpressionError> {
        trace!("Executing assignment for path: {:?}", self.path);
        let result = self.expr.inner.eval_dyn(ctx)?;
        trace!("Assignment completed for path: {:?}", self.path);
        let _handle_error = ctx.write(&self.path, Box::new(result));
        Ok(())
    }
}

/// A context with the purpose of intercepting read-write calls and provide temporary values
/// Reads from the buffer first, and read from the real context if not present in the buffer.
/// Writes-back to the world only when commit is called.
pub struct CachedEvalContext<'a, Ctx> {
    read_ctx: &'a Ctx,
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

impl<Ctx: ReadContext> Context for CachedEvalContext<'_, Ctx> {}

impl<Ctx: ReadContext> ReadContext for CachedEvalContext<'_, Ctx> {
    fn get_any(&self, access: &dyn Accessor) -> Result<&dyn Any, ExpressionError> {
        if let Some(value) = self.shadow.get(&access.key()) {
            trace!("Cache hit for path: {:?}", access.key());
            Ok(value.as_ref())
        } else {
            trace!(
                "Cache miss for path: {:?}, fetching from source",
                access.key()
            );
            self.read_ctx.get_any(access)
        }
    }

    fn get_any_component(&self, _path: &Path) -> Result<&dyn Any, ExpressionError> {
        unreachable!()
    }
}

impl<W> WriteContext for CachedEvalContext<'_, W> {
    fn write(
        &mut self,
        access: &dyn Accessor,
        value: Box<dyn Any + Send + Sync>,
    ) -> Result<(), ExpressionError> {
        trace!("Writing to shadow: {:?}", access.key());
        self.shadow.insert(access.key(), value);
        Ok(())
    }
}

pub struct Step {
    exprs: Vec<Box<dyn StepExecutor>>,
}

impl<N, S> From<Assignment<N, S>> for Step
where
    N: Num + SelectExprNodeImpl<S> + Send + Sync + 'static,
    S: ExprSchema,
{
    fn from(value: Assignment<N, S>) -> Self {
        Step {
            exprs: vec![Box::new(value)],
        }
    }
}

impl StepExecutor for Step {
    fn run(&self, ctx: &mut dyn Context) -> Result<(), ExpressionError> {
        for (i, expr) in self.exprs.iter().enumerate() {
            trace!("Running expr {}/{}", i + 1, self.exprs.len());
            expr.run(ctx)?;
        }
        Ok(())
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

impl_step_from_tuple!(T1);
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
    pub fn eval<R: ReadContext>(&self, read: &R) -> Result<PlanResults, ExpressionError> {
        trace!("Starting plan evaluation");
        let mut shadow_eval = CachedEvalContext::new(read);

        for (i, step) in self.plan.iter().enumerate() {
            trace!("Executing plan step {}/{}", i + 1, self.plan.len());
            step.run(&mut shadow_eval)?;
        }

        trace!("Plan evaluation complete");
        Ok(shadow_eval.into_output())
    }

    /// Phase 2: flush buffered writes into a write context.
    pub fn flush<W: WriteContext>(&self, output: PlanResults, write: &mut W) {
        trace!("Flushing plan results");
        output.flush_into(write);
    }

    /// Commit the expression plan
    pub fn commit<Ctx: ReadContext + WriteContext>(
        &self,
        ctx: &mut Ctx,
    ) -> Result<(), ExpressionError> {
        let output = self.eval(ctx)?;
        self.flush(output, ctx);
        trace!("Plan committed to context");
        Ok(())
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
            trace!("Flushing result for path: {:?}", path);
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
    use crate::test_utils::{Atk, Def, Hp, MapContext, MapSchema};

    fn init() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Trace) // Allow everything up to Trace
            .is_test(true)
            .try_init();
    }

    #[test]
    fn test_sequential_ops() {
        init();

        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 10.0);

        ctx.insert::<Hp>(DST, 20.0);
        ctx.insert::<Def>(DST, 2.0);

        let dmg_taken_expr = Atk::get(SRC) - Def::get(DST);
        let new_hp_expr = Hp::get(DST) - dmg_taken_expr.clone().max(0.0);

        let lp = LazyPlan::new().step(Hp::set(DST, new_hp_expr));

        lp.commit(&mut ctx).expect("Failed to commit");

        let expr = FloatExprNode::<f32, MapSchema>::Attribute(Path::from_type_name::<Hp>(DST));
        let expr_result = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 12.0);
    }

    #[test]
    fn test_interim_ops() {
        init();

        let mut ctx = MapContext::default();
        ctx.insert::<Atk>(SRC, 10.0);

        ctx.insert::<Hp>(DST, 20.0);
        ctx.insert::<Def>(DST, 2.0);

        let dmg_taken_expr = Atk::get(SRC) - Def::get(DST);
        let get_dmg_taken: Expr<f32, MapSchema> =
            FloatExprNode::Attribute(Path::from_name(DST, "dmg_taken")).into();

        let lp = LazyPlan::new()
            .step(dmg_taken_expr.max(0.0).alias(DST, "dmg_taken"))
            .step(Hp::set(DST, Hp::get(DST) - get_dmg_taken));

        lp.commit(&mut ctx).expect("Failed to commit");

        let expr = FloatExprNode::<f32, MapSchema>::Attribute(Path::from_name(DST, "dmg_taken"));
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 8.0);

        let expr = FloatExprNode::<f32, MapSchema>::Attribute(Path::from_type_name::<Hp>(DST));
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 12.0);
    }
}
