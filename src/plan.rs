use crate::expr::{Context, ContextMut, Expr};
use crate::nodes::CacheSaveStep;
use std::any::{Any, type_name, type_name_of_val};
use std::collections::HashMap;
use std::fmt;
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

    /// Returns the entry under `key` only when it was stored as exactly `T`.
    /// A key holding another type reads as `None`.
    #[inline]
    pub fn get<T: 'static>(&self, key: &str) -> Option<&T> {
        self.values.get(key).and_then(|v| v.downcast_ref::<T>())
    }

    /// Type-strict presence check: true exactly when [`PlanCache::get::<T>`] would
    /// return a value, i.e. the key exists *and* holds a `T`.
    #[inline]
    pub fn has<T: 'static>(&self, key: &str) -> bool {
        self.get::<T>(key).is_some()
    }

    /// Presence regardless of type. Exists so callers can tell "no such key" apart
    /// from "key holds some other type", which [`PlanCache::has::<T>`] collapses.
    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.values.contains_key(key)
    }
}

// ---------- PlanError ----------

/// Why a [`Plan`] step could not be executed.
///
/// Every failure a step can hit travels through this type: no value-routing path
/// in this module panics. It is plain data, hence `Send + Sync`, so it crosses the
/// `dyn StepExecutor` boundary back to [`Plan::run`] without extra boxing.
///
/// The variants are constructed inside this crate only. Both the enum and each of
/// its struct variants are `#[non_exhaustive]`, so code outside this crate can
/// match on them (with a wildcard arm and `..`) but cannot build one: a custom
/// [`StepExecutor`] reports a failure by propagating a `PlanError` it received,
/// such as the one a delegated step's [`StepExecutor::evaluate`] returns, not by
/// constructing a new variant.
// `non_exhaustive` on the enum and on every struct variant, so a later release can
// add a variant or a field without a semver break; the downstream cost is spelled
// out in the docs above.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum PlanError {
    /// A cache lookup found no entry under this key.
    #[non_exhaustive]
    CacheMiss { key: String },
    /// An entry exists under `key`, but it was stored under a different type than
    /// the step requested. Kept apart from [`PlanError::CacheMiss`] because a
    /// wrong-type write and a missing write point at different bugs.
    #[non_exhaustive]
    CacheTypeMismatch {
        key: String,
        /// `std::any::type_name` of the type the step requested.
        expected: &'static str,
    },
    /// The value handed to `apply` is not the value that step's `evaluate`
    /// produces. Always a plan-construction bug or a hand-called `apply`; this
    /// case used to be swallowed, so the step silently wrote nothing.
    #[non_exhaustive]
    StepValueMismatch {
        /// `std::any::type_name` of the step that rejected the value.
        step: &'static str,
        /// What that `apply` needed, in words or as a type name.
        expected: &'static str,
    },
}

impl fmt::Display for PlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlanError::CacheMiss { key } => write!(f, "PlanCache miss: no entry for key {key:?}"),
            PlanError::CacheTypeMismatch { key, expected } => write!(
                f,
                "PlanCache key {key:?} holds a value of a different type than the requested {expected}"
            ),
            PlanError::StepValueMismatch { step, expected } => write!(
                f,
                "{step} received a value it cannot consume: expected {expected}"
            ),
        }
    }
}

impl std::error::Error for PlanError {}

// ---------- StepExecutor ----------

/// One unit of work in a [`Plan`]: produce a value, then write it out.
///
/// # Contract
///
/// * [`StepExecutor::evaluate`] is read-only. It reads the context and the shared
///   cache and returns the step's value boxed as `dyn Any`; that value is exactly
///   what [`StepExecutor::apply`] expects to receive.
/// * [`StepExecutor::apply`] performs the writes (context fields and/or cache
///   keys) and consumes the value produced by `evaluate`.
/// * Neither method panics on a value-routing path. A missing or wrong-typed cache
///   entry, and a boxed value that does not match the step, are reported as
///   [`PlanError`] — never swallowed and never panicked. `Plan::run` stops at the
///   first `Err` and hands it to the caller. An implementation outside this crate
///   can only propagate a `PlanError` it received: the variants cannot be
///   constructed downstream, see [`PlanError`].
pub trait StepExecutor<C: ContextMut>: Send + Sync {
    /// Read-only: evaluate against current context + shared cache.
    fn evaluate(
        &self,
        ctx: &C::ContextItem<'_, '_>,
        cache: &PlanCache,
    ) -> Result<Box<dyn Any>, PlanError>;

    /// Mutating: apply the previously computed value, then optionally enrich cache.
    fn apply(
        &self,
        ctx: &mut C::ContextItemMut<'_, '_>,
        value: Box<dyn Any>,
        cache: &mut PlanCache,
    ) -> Result<(), PlanError>;
}

// ---------- Plan ----------

pub struct Plan<C: Context> {
    steps: Vec<Box<dyn StepExecutor<C>>>,
    _marker: PhantomData<C>,
}

impl<C: ContextMut> Plan<C> {
    /// An empty plan. `#[must_use]`: a plan that is built and never run does nothing,
    /// so a discarded builder is always a mistake — see [`Plan::step`].
    #[must_use]
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Consumes the builder and returns it with `step` appended. `#[must_use]`: the
    /// step is added to the returned plan, not to the receiver, so calling this as a
    /// bare statement discards the step instead of adding it.
    #[must_use]
    pub fn step<S>(mut self, step: S) -> Self
    where
        S: StepExecutor<C> + 'static,
    {
        self.steps.push(Box::new(step));
        self
    }
}

impl<C: ContextMut> Default for Plan<C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<C: ContextMut> Plan<C> {
    /// Sequential execute: evaluate → apply → mutate cache → next step.
    /// This lets step N read the writes + cache populated by step N-1.
    ///
    /// Stops at the first step that reports a [`PlanError`]. The steps before it
    /// have already written to `ctx`; a failed run is not rolled back.
    pub fn run<'w, 's>(&self, ctx: &mut C::ContextItemMut<'w, 's>) -> Result<(), PlanError> {
        let mut cache = PlanCache::default();
        for step in &self.steps {
            let value = step.evaluate(&C::as_readonly(ctx), &cache)?;
            step.apply(ctx, value, &mut cache)?;
        }
        Ok(())
    }
}

// ---------- AssignmentStep ----------

pub struct AssignmentStep<N: 'static, E, S> {
    pub setter_fn: S,
    pub expr: E,
    pub cache_key: Option<String>,
    pub _marker: PhantomData<N>,
}

// Hand-written so the bounds land on the payload types only. There is no `Copy`
// twin: `cache_key` owns a `String`, so the step can never be `Copy`, whatever
// the bounds say.
impl<N: 'static, E: Clone, S: Clone> Clone for AssignmentStep<N, E, S> {
    fn clone(&self) -> Self {
        Self {
            setter_fn: self.setter_fn.clone(),
            expr: self.expr.clone(),
            cache_key: self.cache_key.clone(),
            _marker: PhantomData,
        }
    }
}

impl<N, C, E, S> StepExecutor<C> for AssignmentStep<N, E, S>
where
    E: Expr<N, C> + Send + Sync,
    N: 'static + Clone + Send + Sync,
    C: ContextMut,
    S: Fn(&mut C::ContextItemMut<'_, '_>, N) + 'static + Send + Sync,
{
    fn evaluate(
        &self,
        ctx: &C::ContextItem<'_, '_>,
        _cache: &PlanCache,
    ) -> Result<Box<dyn Any>, PlanError> {
        Ok(Box::new(self.expr.eval(ctx)))
    }

    fn apply(
        &self,
        write_ctx: &mut C::ContextItemMut<'_, '_>,
        value: Box<dyn Any>,
        cache: &mut PlanCache,
    ) -> Result<(), PlanError> {
        let concrete_value = value
            .downcast::<N>()
            .map_err(|_| PlanError::StepValueMismatch {
                step: type_name_of_val(self),
                expected: type_name::<N>(),
            })?;
        let v = *concrete_value;
        let cache_copy = v.clone();
        (self.setter_fn)(write_ctx, v);
        if let Some(ref key) = self.cache_key {
            cache.set(key.clone(), cache_copy);
        }
        Ok(())
    }
}

// ---------- CacheSaveStep ----------
impl<N, C, E> StepExecutor<C> for CacheSaveStep<N, C, E>
where
    N: 'static + Clone + Send + Sync,
    C: ContextMut,
    E: Expr<N, C> + Send + Sync,
{
    fn evaluate(
        &self,
        ctx: &C::ContextItem<'_, '_>,
        _cache: &PlanCache,
    ) -> Result<Box<dyn Any>, PlanError> {
        Ok(Box::new(self.expr.eval(ctx)))
    }

    fn apply(
        &self,
        _ctx: &mut C::ContextItemMut<'_, '_>,
        value: Box<dyn Any>,
        cache: &mut PlanCache,
    ) -> Result<(), PlanError> {
        let v = value
            .downcast::<N>()
            .map_err(|_| PlanError::StepValueMismatch {
                step: type_name_of_val(self),
                expected: type_name::<N>(),
            })?;
        cache.set(self.key.to_string(), *v);
        Ok(())
    }
}

// Helper macros

/// Shorthand for creating an [`AssignmentStep`]: writes the expression's value to
/// a context field, optionally also under a cache key.
///
/// 2 forms:
///   save!(ctx_ty, field, expr)                -> cache_key: None
///   save!(ctx_ty, field, expr, cache_key)     -> cache_key: Some(cache_key.to_string())
///
/// The result is a step, not a value: hand it to [`Plan::step`] and run the plan
/// with [`Plan::run`], which returns `Result<(), PlanError>`. An assignment step
/// always evaluates and always applies, so it is never the step that fails a run.
///
/// Examples:
///   let plan = plan.step(save!(CombatState, damage_dealt, Node::lit(35)));
///   let plan = plan.step(save!(CombatState, heal_received, heal_expr, "applied_heal"));
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

/// Inline macro to create a cache-save step: [`CacheSaveStep`], which writes the
/// expression's value into the run's [`PlanCache`] and writes nothing to the
/// context. Feed it to [`Plan::step`]; a cache-save step never fails a run.
///
/// Usage:
///   let plan = plan.step(save_cache!("my_key", Node::lit(42)));
#[macro_export]
macro_rules! save_cache {
    ($key:expr, $expr:expr) => {{
        $crate::nodes::CacheSaveStep {
            key: $key,
            expr: $expr,
        }
    }};
}

/// Inline macro to create a cache-lookup step: [`CacheLookupStep`], which reads a
/// key written earlier in the same [`Plan::run`] and emits it as that step's
/// output. The value type is the first argument, so both arguments are required:
///
///   let plan = plan.step(load_cache!(i32, "my_key"));
///   let step = load_cache!(i32, "my_key"); // a CacheLookupStep over i32
///
/// A lookup is the step that can fail a run: a missing key is
/// [`PlanError::CacheMiss`] and a key holding another type is
/// [`PlanError::CacheTypeMismatch`].
#[macro_export]
macro_rules! load_cache {
    ($ty:ty, $key:expr) => {{ $crate::plan::CacheLookupStep::<$ty>::new($key) }};
}

/// Pulls a previously cached value and emits it as the step's output.
/// Requires `V: Clone` because plan values are passed by-value through `Box<dyn Any>`.
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

impl<V, C: ContextMut> StepExecutor<C> for CacheLookupStep<V>
where
    V: Clone + 'static + Send + Sync,
{
    fn evaluate(
        &self,
        _ctx: &C::ContextItem<'_, '_>,
        cache: &PlanCache,
    ) -> Result<Box<dyn Any>, PlanError> {
        match cache.get::<V>(&self.key) {
            Some(v) => Ok(Box::new(v.clone())),
            // The key exists but holds another type: a wrong-type write, not a miss.
            None if cache.contains_key(&self.key) => Err(PlanError::CacheTypeMismatch {
                key: self.key.clone(),
                expected: type_name::<V>(),
            }),
            None => Err(PlanError::CacheMiss {
                key: self.key.clone(),
            }),
        }
    }

    fn apply(
        &self,
        _ctx: &mut C::ContextItemMut<'_, '_>,
        _value: Box<dyn Any>,
        _cache: &mut PlanCache,
    ) -> Result<(), PlanError> {
        // no-op: lookup steps only produce values, they don't consume them
        Ok(())
    }
}

macro_rules! impl_step_executor_for_tuple {
    ($($names:ident),+) => {
        #[allow(non_snake_case)]
        impl<C, $($names),+> StepExecutor<C> for ($($names,)+)
        where
            C: ContextMut,
            $($names: StepExecutor<C>,)+
        {
            fn evaluate(
                &self,
                ctx: &C::ContextItem<'_, '_>,
                cache: &PlanCache,
            ) -> Result<Box<dyn Any>, PlanError> {
                let ($($names,)+) = self;
                // Vec elements keep declaration order: element i is the value of
                // element i, which is what `apply` consumes below.
                Ok(Box::new(vec![
                    $(
                        $names.evaluate(ctx, cache)?,
                    )+
                ]))
            }

            fn apply(
                &self,
                ctx: &mut C::ContextItemMut<'_, '_>,
                value: Box<dyn Any>,
                cache: &mut PlanCache,
            ) -> Result<(), PlanError> {
                let results =
                    value.downcast::<Vec<Box<dyn Any>>>().map_err(|_| PlanError::StepValueMismatch {
                        step: type_name_of_val(self),
                        expected: "the Vec<Box<dyn Any>> produced by this tuple's evaluate",
                    })?;
                // Reject a value vector whose length does not match the tuple's arity
                // before writing anything, so a bad hand-call cannot leave the tuple
                // half-applied.
                let arity = [$(stringify!($names)),+].len();
                if results.len() != arity {
                    return Err(PlanError::StepValueMismatch {
                        step: type_name_of_val(self),
                        expected: "one value per tuple element",
                    });
                }
                let ($($names,)+) = self;
                // Consume front-to-back so element i receives the value element i
                // produced. `pop()` here would hand the last value to the first
                // element, i.e. swap every pair.
                let mut values = results.into_iter();
                $(
                    // Already guaranteed by the length check, but reported instead of
                    // unwrapped: a trait method must never panic.
                    let value = values.next().ok_or(PlanError::StepValueMismatch {
                        step: type_name_of_val(self),
                        expected: "one value per tuple element",
                    })?;
                    $names.apply(ctx, value, cache)?;
                )+
                Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::Node;

    #[derive(Debug, Default, Clone, Copy, PartialEq)]
    struct State {
        a: i32,
        b: i32,
        hp: i32,
        scale: f32,
        note: i32,
    }

    struct Ctx;

    impl Context for Ctx {
        type ContextItem<'w, 's> = State;
    }

    impl ContextMut for Ctx {
        type ContextItemMut<'w, 's> = State;

        fn as_readonly<'w, 's, 'a>(
            mut_item: &'a Self::ContextItemMut<'w, 's>,
        ) -> Self::ContextItem<'a, 's> {
            *mut_item
        }
    }

    #[test]
    fn tuple_step_assigns_values_in_declaration_order() {
        // Repro of the swapped-values defect: `apply` used `Vec::pop`, so `a` got
        // 2 and `b` got 1.
        let mut state = State::default();
        let plan =
            Plan::<Ctx>::new().step((save!(State, a, Node::lit(1)), save!(State, b, Node::lit(2))));

        plan.run(&mut state)
            .expect("two same-typed steps in one tuple must apply");

        assert_eq!((state.a, state.b), (1, 2));
    }

    #[test]
    fn tuple_step_with_mixed_value_types_writes_every_field() {
        // Same defect, mixed-type variant: the LIFO order handed an `f32` to the
        // `i32` step and vice versa, so both downcasts failed and neither field
        // was written at all (silently).
        let mut state = State::default();
        let plan = Plan::<Ctx>::new().step((
            save!(State, hp, Node::lit(7)),
            save!(State, scale, Node::lit(9.0f32)),
        ));

        plan.run(&mut state)
            .expect("a mixed-type tuple step must apply, not silently no-op");

        assert_eq!(state.hp, 7);
        assert_eq!(state.scale, 9.0);
    }

    #[test]
    fn three_tuple_step_keeps_declaration_order() {
        let mut state = State::default();
        let plan = Plan::<Ctx>::new().step((
            save!(State, a, Node::lit(1)),
            save!(State, b, Node::lit(2)),
            save!(State, note, Node::lit(3)),
        ));

        plan.run(&mut state).expect("3-tuple step must apply");

        assert_eq!((state.a, state.b, state.note), (1, 2, 3));
    }

    #[test]
    fn plan_cache_has_is_type_strict() {
        let mut cache = PlanCache::default();
        cache.set("k", 1.0f32);

        assert!(cache.has::<f32>("k"));
        assert!(
            !cache.has::<i32>("k"),
            "a key holding an f32 must not report an i32"
        );
        assert_eq!(cache.get::<i32>("k"), None);
        assert!(
            cache.contains_key("k"),
            "the untyped check still sees the f32 entry"
        );
        assert!(!cache.contains_key("absent"));
        assert!(!cache.has::<i32>("absent"));
    }

    #[test]
    fn cache_lookup_of_a_missing_key_reports_err() {
        let mut state = State::default();
        let plan = Plan::<Ctx>::new().step(load_cache!(i32, "never_written"));

        let err = plan
            .run(&mut state)
            .expect_err("reading a key nobody wrote must be an Err");

        assert_eq!(
            err,
            PlanError::CacheMiss {
                key: "never_written".to_string()
            }
        );
    }

    #[test]
    fn cache_lookup_of_a_wrongly_typed_key_reports_a_distinguishable_err() {
        let mut state = State::default();
        let plan = Plan::<Ctx>::new()
            .step(save_cache!("note", Node::lit(1.0f32)))
            .step(load_cache!(i32, "note"));

        let err = plan
            .run(&mut state)
            .expect_err("reading an f32 entry as i32 must be an Err");

        let PlanError::CacheTypeMismatch { key, expected } = err else {
            panic!("expected CacheTypeMismatch, got {err:?}");
        };
        assert_eq!(key, "note");
        assert_eq!(expected, "i32");
    }

    #[test]
    fn later_step_observes_the_previous_step_write() {
        let mut state = State::default();
        let plan = Plan::<Ctx>::new()
            .step(save!(State, a, Node::lit(5)))
            .step(save!(State, b, Node::new(|ctx: &State| ctx.a * 2)));

        plan.run(&mut state).expect("sequential plan must apply");

        assert_eq!((state.a, state.b), (5, 10), "step 2 read step 1's write");
    }

    #[test]
    fn later_step_observes_the_previous_step_cache_write() {
        let mut state = State::default();
        let plan = Plan::<Ctx>::new()
            .step(save!(State, hp, Node::lit(11), "hp_key"))
            // Would report CacheMiss if the shared cache were not threaded through.
            .step(load_cache!(i32, "hp_key"));

        plan.run(&mut state)
            .expect("the cache write of step 1 must be visible to step 2");

        assert_eq!(state.hp, 11);
    }

    #[test]
    fn assignment_step_with_cache_key_populates_context_and_cache() {
        let mut state = State::default();
        let mut cache = PlanCache::default();
        let step = save!(State, hp, Node::lit(4), "hp_key");

        let value = <_ as StepExecutor<Ctx>>::evaluate(&step, &state, &cache)
            .expect("an assignment step never fails to evaluate");
        <_ as StepExecutor<Ctx>>::apply(&step, &mut state, value, &mut cache)
            .expect("the value came from this step, so apply must accept it");

        assert_eq!(state.hp, 4);
        assert_eq!(cache.get::<i32>("hp_key"), Some(&4));
    }

    #[test]
    fn assignment_step_rejects_a_value_of_the_wrong_type() {
        let mut state = State::default();
        let mut cache = PlanCache::default();
        // The 4-argument form, so the step really does own a cache key: with the
        // 2-argument form `cache_key` is `None` and the cache assertion below could
        // never fail whatever `apply` did.
        let step = save!(State, hp, Node::lit(4), "hp_key");

        let err = <_ as StepExecutor<Ctx>>::apply(&step, &mut state, Box::new(4.0f32), &mut cache)
            .expect_err("a value the step cannot write must be an Err, not a silent no-op");

        assert!(matches!(err, PlanError::StepValueMismatch { .. }));
        assert_eq!(state.hp, 0, "a rejected value must not be half-applied");
        assert!(!cache.contains_key("hp_key"));
    }

    #[test]
    fn tuple_step_rejects_a_short_value_vector() {
        let mut state = State::default();
        let mut cache = PlanCache::default();
        let tuple = (save!(State, a, Node::lit(1)), save!(State, b, Node::lit(2)));
        let short: Box<dyn Any> = Box::new(vec![Box::new(1i32) as Box<dyn Any>]);

        let err = <_ as StepExecutor<Ctx>>::apply(&tuple, &mut state, short, &mut cache)
            .expect_err("one value per element is part of the tuple contract");

        assert!(matches!(err, PlanError::StepValueMismatch { .. }));
        assert_eq!((state.a, state.b), (0, 0));
    }

    #[test]
    fn tuple_step_rejects_a_value_that_is_not_a_value_vector() {
        let mut state = State::default();
        let mut cache = PlanCache::default();
        let tuple = (save!(State, a, Node::lit(1)), save!(State, b, Node::lit(2)));

        let err = <_ as StepExecutor<Ctx>>::apply(&tuple, &mut state, Box::new(1i32), &mut cache)
            .expect_err("a tuple step only consumes its own value vector");

        assert!(matches!(err, PlanError::StepValueMismatch { .. }));
        assert_eq!((state.a, state.b), (0, 0));
    }
}
