use crate::expr::{Context, Expr};
use crate::nodes::CacheSaveStep;
use std::any::Any;
use std::collections::HashMap;
use std::marker::PhantomData;

// ---------- PlanCache ----------

/// Typed, temporary key-value store shared across a single Plan run.
#[derive(Default)]
pub struct PlanCache {
    values: HashMap<String, Box<dyn Any>>,
}

impl PlanCache {
    #[inline]
    pub fn set<T: 'static>(&mut self, key: impl Into<String>, value: T) {
        self.values.insert(key.into(), Box::new(value));
    }

    #[inline]
    pub fn get<T: 'static>(&self, key: &str) -> Option<&T> {
        self.values.get(key).and_then(|v| v.downcast_ref::<T>())
    }

    #[inline]
    pub fn has<T: 'static>(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }
}

// ---------- StepExecutor ----------

pub trait StepExecutor<C: Context> {
    /// Read-only: evaluate against current context + shared cache.
    fn evaluate(&self, ctx: &C::ContextItem<'_, '_>, cache: &PlanCache) -> Box<dyn Any>;

    /// Mutating: apply the previously computed value, then optionally enrich cache.
    fn apply(&self, ctx: &mut C::ContextItem<'_, '_>, value: Box<dyn Any>, cache: &mut PlanCache);
}

// ---------- Plan ----------

pub struct Plan<C: Context> {
    steps: Vec<Box<dyn StepExecutor<C>>>,
    _marker: PhantomData<C>,
}

impl<C: Context> Plan<C> {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn step<S>(mut self, step: S) -> Self
    where
        S: StepExecutor<C> + 'static,
    {
        self.steps.push(Box::new(step));
        self
    }
}

impl<C: Context> Plan<C> {
    /// Sequential execute: evaluate → apply → mutate cache → next step.
    /// This lets step N read the writes + cache populated by step N-1.
    pub fn run<'w, 's>(&self, ctx: &mut C::ContextItem<'w, 's>) {
        let mut cache = PlanCache::default();
        for step in &self.steps {
            let value = step.evaluate(&*ctx, &cache);
            step.apply(ctx, value, &mut cache);
        }
    }
}

// ---------- AssignmentStep ----------

pub struct AssignmentStep<N: 'static, E, S> {
    pub setter_fn: S,
    pub expr: E,
    pub cache_key: Option<String>,
    pub _marker: std::marker::PhantomData<N>,
}

impl<N: 'static + Clone, C: Context, E, S: Fn(&mut C::ContextItem<'_, '_>, N) + 'static>
    StepExecutor<C> for AssignmentStep<N, E, S>
where
    E: Expr<N, C>,
{
    fn evaluate(&self, ctx: &C::ContextItem<'_, '_>, _cache: &PlanCache) -> Box<dyn Any> {
        Box::new(self.expr.eval(ctx))
    }

    fn apply(
        &self,
        write_ctx: &mut C::ContextItem<'_, '_>,
        value: Box<dyn Any>,
        cache: &mut PlanCache,
    ) {
        if let Ok(concrete_value) = value.downcast::<N>() {
            let v = *concrete_value;
            let cache_copy = v.clone();
            (self.setter_fn)(write_ctx, v);
            if let Some(ref key) = self.cache_key {
                cache.set(key.clone(), cache_copy);
            }
        }
    }
}

// ---------- CacheSaveStep ----------

// Definition lives in `nodes.rs` so `Node::alias()` can return it.

impl<N: 'static + Clone, C: Context, E: Expr<N, C>> StepExecutor<C> for CacheSaveStep<N, C, E> {
    fn evaluate(&self, ctx: &C::ContextItem<'_, '_>, _cache: &PlanCache) -> Box<dyn Any> {
        Box::new(self.expr.eval(ctx))
    }

    fn apply(&self, _ctx: &mut C::ContextItem<'_, '_>, value: Box<dyn Any>, cache: &mut PlanCache) {
        if let Ok(v) = value.downcast::<N>() {
            cache.set(self.key.to_string(), *v);
        }
    }
}

// Helper macros

/// Shorthand for creating an AssignmentStep.
///
/// 2 forms:
///   save!(ctx_ty, field, expr)                -> cache_key: None
///   save!(ctx_ty, field, expr, cache_key)     -> cache_key: Some(cache_key.to_string())
///
/// Examples:
///   plan.step(save!(CombatState, damage_dealt, Node::lit(35)))
///   plan.step(save!(CombatState, heal_received, heal_expr, "applied_heal"))
#[macro_export]
macro_rules! save {
    ($ctx_ty:ty, $field:ident, $expr:expr) => {{
        $crate::plan::AssignmentStep {
            setter_fn: |ctx: &mut $ctx_ty, val| ctx.$field = val,
            expr: $expr,
            cache_key: None,
            _marker: std::marker::PhantomData,
        }
    }};
    ($ctx_ty:ty, $field:ident, $expr:expr, $key:expr) => {{
        $crate::plan::AssignmentStep {
            setter_fn: |ctx: &mut $ctx_ty, val| ctx.$field = val,
            expr: $expr,
            cache_key: Some($key.to_string()),
            _marker: std::marker::PhantomData,
        }
    }};
}

/// Inline macro to create a cache-save step.
/// Usage: plan.step(save_cache!("my_key", Node::lit(42)))
#[macro_export]
macro_rules! save_cache {
    ($key:expr, $expr:expr) => {{
        $crate::nodes::CacheSaveStep {
            key: $key,
            expr: $expr,
        }
    }};
}

/// Inline macro to create a cache-lookup step.
/// Usage: plan.step(load_cache!(i32, "my_key"))
/// Or: `let s: CacheLookupStep<i32> = load_cache!("my_key");`
#[macro_export]
macro_rules! load_cache {
    ($ty:ty, $key:expr) => {{ $crate::plan::CacheLookupStep::<$ty>::new($key) }};
}

/// Pulls a previously cached value and emits it as the step's output.
/// Requires `V: Clone` because plan values are passed by-value through Box<dyn Any>.
pub struct CacheLookupStep<V: 'static> {
    pub key: String,
    _marker: PhantomData<V>,
}

impl<V> CacheLookupStep<V>
where
    V: Clone + 'static,
{
    pub fn new(key: impl Into<String>) -> Self {
        Self {
            key: key.into(),
            _marker: PhantomData,
        }
    }
}

impl<V, C: Context> StepExecutor<C> for CacheLookupStep<V>
where
    V: Clone + 'static,
{
    fn evaluate(&self, _ctx: &C::ContextItem<'_, '_>, cache: &PlanCache) -> Box<dyn Any> {
        Box::new(
            cache
                .get::<V>(&self.key)
                .cloned()
                .unwrap_or_else(|| panic!("PlanCache miss: key={}", self.key)),
        )
    }

    fn apply(
        &self,
        _ctx: &mut C::ContextItem<'_, '_>,
        _value: Box<dyn Any>,
        _cache: &mut PlanCache,
    ) {
        // no-op: lookup steps only produce values, they don't consume them
    }
}

macro_rules! impl_step_executor_for_tuple {
    ($($types:ident),+) => {
        #[allow(non_snake_case)]
        impl<C, $($types),+> StepExecutor<C> for ($($types,)+)
        where
            C: Context,
            $($types: StepExecutor<C>,)+
        {
            fn evaluate(
                &self,
                ctx: &C::ContextItem<'_, '_>,
                cache: &PlanCache,
            ) -> Box<dyn Any> {
                let ($($types,)+) = self;
                Box::new(vec![
                    $(
                        $types.evaluate(ctx, cache),
                    )+
                ])
            }

            fn apply(
                &self,
                ctx: &mut C::ContextItem<'_, '_>,
                value: Box<dyn Any>,
                cache: &mut PlanCache,
            ) {
                if let Ok(mut results) = value.downcast::<Vec<Box<dyn Any>>>() {
                    let ($($types,)+) = self;
                    $(
                        let value = results.pop().unwrap();
                        $types.apply(ctx, value, cache);
                    )+
                }
            }
        }
    };
}

// Specify tuple sizes to implement
impl_step_executor_for_tuple!(T1, T2);
impl_step_executor_for_tuple!(T1, T2, T3);
impl_step_executor_for_tuple!(T1, T2, T3, T4);
impl_step_executor_for_tuple!(T1, T2, T3, T4, T5);
impl_step_executor_for_tuple!(T1, T2, T3, T4, T5, T6);
impl_step_executor_for_tuple!(T1, T2, T3, T4, T5, T6, T7);
impl_step_executor_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
