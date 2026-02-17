use crate::context::{Accessor, Path, ReadContext, ScopeId, WriteContext};
use crate::expr::{Expr, ExprNode, ExpressionError};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;

trait Context: ReadContext + WriteContext {
    fn commit(&mut self);
}

pub trait ExprAttribute {
    type Property: Debug;
}

pub struct Assignment<N, Nd: ExprNode<N>> {
    pub path: Path,
    pub expr: Expr<N, Nd>,
}

trait Executor: Send + Sync {
    fn run(&self, ctx: &mut dyn Context);
}

impl<N: Send + Sync + 'static, Nd: ExprNode<N>> Executor for Assignment<N, Nd> {
    fn run(&self, ctx: &mut dyn Context) {
        let result = self.expr.inner.eval(ctx).unwrap_or_else(|_| {
            panic!(
                "Executor failed. {} value could not be found in context.",
                self.path
            )
        });
        ctx.write(&self.path, Box::new(result));
    }
}

/// A context with the purpose of intercepting read-write calls and provide temporary values
/// Reads from the buffer first, and read from the real context if not present in the buffer.
/// Writes-back to the world only when commit is called.
pub struct ShadowContext<'a, Ctx> {
    ctx: &'a mut Ctx,
    shadow: HashMap<(ScopeId, u64), Box<dyn Any + Send + Sync>>,
}

impl<'a, Ctx> ShadowContext<'a, Ctx> {
    pub fn new(ctx: &'a mut Ctx) -> Self {
        Self {
            ctx: ctx,
            shadow: Default::default(),
        }
    }
}

impl<Ctx: ReadContext + WriteContext> Context for ShadowContext<'_, Ctx> {
    fn commit(&mut self) {
        for (key, value) in self.shadow.drain() {
            let path = Path {
                scope: key.0,
                id: key.1,
            };
            self.ctx.write(&path, value);
        }
    }
}

impl<Ctx: ReadContext> ReadContext for ShadowContext<'_, Ctx> {
    fn get_any(&self, access: &dyn Accessor) -> Result<&dyn Any, ExpressionError> {
        if let Some(value) = self.shadow.get(&access.key()) {
            Ok(value.as_ref())
        } else {
            self.ctx.get_any(access)
        }
    }

    fn get_any_component(
        &self,
        _path: &str,
        _type_id: std::any::TypeId,
    ) -> Result<&dyn Any, ExpressionError> {
        unreachable!()
    }
}

impl<Ctx: WriteContext> WriteContext for ShadowContext<'_, Ctx> {
    fn write(&mut self, access: &dyn Accessor, value: Box<dyn Any + Send + Sync>) {
        self.shadow.insert(access.key(), value);
    }
}

pub struct Step {
    exprs: Vec<Box<dyn Executor>>,
}

impl<N, Nd> From<Assignment<N, Nd>> for Step
where
    N: Send + Sync + 'static,
    Nd: ExprNode<N> + 'static,
{
    fn from(value: Assignment<N, Nd>) -> Self {
        Step {
            exprs: vec![Box::new(value)],
        }
    }
}

impl Executor for Step {
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
                $($t: Executor + 'static,)+
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

    /// Commit the expression plan
    pub fn commit<Ctx: ReadContext + WriteContext>(&self, ctx: &mut Ctx) {
        let mut shadow_view = ShadowContext::new(ctx);
        // Execute plan
        for step in &self.plan {
            step.run(&mut shadow_view);
        }
        // Commit key/values from temp buffer to destination
        shadow_view.commit();
    }
}

#[cfg(test)]
mod tests {
    use crate::context::{Path, ScopeId};
    use crate::expr::{Expr, ExprNode};
    use crate::float::{FloatExpr, FloatExprNode};
    use crate::frame::{Assignment, ExprAttribute, LazyPlan};
    use crate::test_utils::MapContext;
    use crate::test_utils::scopes::{DST, SRC};
    use std::sync::Arc;

    struct Atk;
    impl Atk {
        #[allow(dead_code)]
        pub fn set(
            key: impl Into<ScopeId>,
            expr: FloatExpr<f32>,
        ) -> Assignment<f32, FloatExprNode<f32>> {
            let path = Path::from_type::<Self>(key.into());
            Assignment { path, expr }
        }
        pub fn get(scope: impl Into<ScopeId>) -> FloatExpr<f32> {
            let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
            Expr::new(Arc::new(expr))
        }
    }
    impl ExprAttribute for Atk {
        type Property = f32;
    }

    struct Def;
    impl Def {
        #[allow(dead_code)]
        pub fn set(
            key: impl Into<ScopeId>,
            expr: FloatExpr<f32>,
        ) -> Assignment<f32, FloatExprNode<f32>> {
            let path = Path::from_type::<Self>(key.into());
            Assignment { path, expr }
        }
        pub fn get(scope: impl Into<ScopeId>) -> FloatExpr<f32> {
            let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
            Expr::new(Arc::new(expr))
        }
    }
    impl ExprAttribute for Def {
        type Property = f32;
    }
    struct Hp;
    impl Hp {
        pub fn set(
            key: impl Into<ScopeId>,
            expr: FloatExpr<f32>,
        ) -> Assignment<f32, FloatExprNode<f32>> {
            let path = Path::from_type::<Self>(key.into());
            Assignment { path, expr }
        }
        pub fn get(scope: impl Into<ScopeId>) -> FloatExpr<f32> {
            let expr = FloatExprNode::Attribute(Path::from_type::<Self>(scope));
            Expr::new(Arc::new(expr))
        }
    }
    impl ExprAttribute for Hp {
        type Property = f32;
    }

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
        let get_dmg_taken: FloatExpr<f32> =
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
