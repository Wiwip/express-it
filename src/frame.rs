use crate::context::{AttributeKey, ReadContext, WriteContext};
use crate::expr::{Expr, ExprNode, ExpressionError};
use std::any::Any;
use std::collections::HashMap;

pub trait Context: ReadContext + WriteContext {
    fn commit(&mut self);
}

pub trait ExprAttribute {
    type Property;
}

pub struct Assignment<N, Nd: ExprNode<N>> {
    pub key: AttributeKey,
    pub expr: Expr<N, Nd>,
}

pub trait Executor {
    fn run(&mut self, ctx: &mut dyn Context);
}

impl<N: 'static, Nd: ExprNode<N>> Executor for Assignment<N, Nd> {
    fn run(&mut self, ctx: &mut dyn Context) {
        let result = self.expr.inner.eval(ctx).unwrap();
        ctx.write(&self.key, Box::new(result));
    }
}

/// A context with the purpose of intercepting read-write calls and provide temporary values
/// Reads from the buffer first, and read from the real context if not present in the buffer.
/// Writes-back to the world only when commit is called.
pub struct ShadowContext<'a, Ctx> {
    ctx: &'a mut Ctx,
    shadow: &'a mut HashMap<AttributeKey, Box<dyn Any>>,
}

impl<Ctx: Context> Context for ShadowContext<'_, Ctx> {
    fn commit(&mut self) {
        for (key, value) in self.shadow.drain() {
            self.ctx.write(&key, value);
        }
    }
}

impl<Ctx: ReadContext> ReadContext for ShadowContext<'_, Ctx> {
    fn get_any(&self, path: &AttributeKey) -> Result<&dyn Any, ExpressionError> {
        if let Some(value) = self.shadow.get(path) {
            Ok(value.as_ref())
        } else {
            self.ctx.get_any(path)
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
    fn write(&mut self, path: &AttributeKey, value: Box<dyn Any>) {
        self.shadow.insert(path.clone(), value);
    }
}

pub struct Step {
    exprs: Vec<Box<dyn Executor>>,
}

impl<N, Nd> From<Assignment<N, Nd>> for Step
where
    N: 'static,
    Nd: ExprNode<N> + 'static,
{
    fn from(value: Assignment<N, Nd>) -> Self {
        Step {
            exprs: vec![Box::new(value)],
        }
    }
}

impl Executor for Step {
    fn run(&mut self, ctx: &mut dyn Context) {
        for expr in &mut self.exprs {
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
    shadow_buffer: HashMap<AttributeKey, Box<dyn Any>>,
    plan: Vec<Step>,
}

impl LazyPlan {
    pub fn new() -> Self {
        Self {
            shadow_buffer: Default::default(),
            plan: vec![],
        }
    }

    pub fn step(mut self, step: impl Into<Step>) -> Self {
        self.plan.push(step.into());
        self
    }

    /// Commit the expression plan
    pub fn commit<Ctx: Context>(mut self, ctx: &mut Ctx) {
        let mut shadow_view = ShadowContext {
            shadow: &mut self.shadow_buffer,
            ctx,
        };

        // Execute plan
        for step in &mut self.plan {
            step.run(&mut shadow_view);
        }

        // Commit key/values from temp buffer to destination
        shadow_view.commit()
    }
}

#[cfg(test)]
mod tests {
    use crate::context::AttributeKey;
    use crate::expr::{Expr, ExprNode};
    use crate::float::{FloatExpr, FloatExprNode};
    use crate::frame::{Assignment, LazyPlan};
    use crate::integer::IntExprNode;
    use crate::test_utils::{I32Attribute, MapContext};
    use std::ops::Neg;
    use std::sync::Arc;

    struct Atk;
    impl Atk {
        pub fn set(expr: FloatExpr<f32>) -> Assignment<f32, FloatExprNode<f32>> {
            Assignment {
                key: Self::key(),
                expr,
            }
        }
        pub fn key() -> AttributeKey {
            AttributeKey::new::<Self>("src.atk")
        }
    }

    struct Def;
    impl Def {
        pub fn set(expr: FloatExpr<f32>) -> Assignment<f32, FloatExprNode<f32>> {
            Assignment {
                key: Self::key(),
                expr,
            }
        }
        pub fn key() -> AttributeKey {
            AttributeKey::new::<Self>("src.atk")
        }
    }

    struct Source;

    #[test]
    fn test_sequential_ops() {
        let mut ctx = MapContext::default();
        ctx.insert_dst::<I32Attribute>(150);

        let atk_expr = I32Attribute::dst().neg();
        let double_atk_expr = I32Attribute::dst() * 2;
        let def_expr = I32Attribute::dst() + 100;

        let use_double_atk_expr = Expr::new(Arc::new(IntExprNode::<i32>::Attribute(
            AttributeKey::new::<Source>("double_attack"),
        )));

        let lp = LazyPlan::new()
            .step((
                Atk::set(atk_expr.as_()),
                double_atk_expr.alias::<Source>("double_attack"),
                Def::set(def_expr.as_()),
            ))
            .step(Atk::set(use_double_atk_expr.as_()));

        lp.commit(&mut ctx);

        println!("{:?}", ctx);
        println!("{:?}", Atk::key());

        let expr = FloatExprNode::Attribute(Atk::key());
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 300.0);
    }

    /*#[test]
    fn test_many_sequential_ops() {
        let mut ctx = MapContext::default();
        ctx.insert_dst::<I32Attribute>(150);

        let atk_expr = I32Attribute::dst().neg();
        let def_expr = I32Attribute::dst() + 100;

        let _lf = LazyExecutor::new(&mut ctx)
            //.step()
            .step((Atk::set(atk_expr.as_()), Def::set(def_expr.as_())))
            .step(Atk::set(atk_expr.as_()))
            .commit();

        let expr = FloatExprNode::Attribute(Atk::key());
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, -150.0);

        let expr = FloatExprNode::Attribute(Def::key());
        let expr_result: f32 = expr.eval(&ctx).unwrap();
        assert_eq!(expr_result, 250.0);
    }*/
}
