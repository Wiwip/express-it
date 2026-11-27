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
    /// Stores `value` under `key`, replacing whatever was there.
    ///
    /// Replacement is by key alone, so writing an `i32` over an `f32` already held
    /// under `"damage"` succeeds and leaves the cache holding an `i32`. The next
    /// `get::<f32>` answers `None`, and a `load_cache!` step for `f32` reports
    /// [`PlanError::CacheTypeMismatch`] — a wrong-type write is visible as that
    /// error at the first reader, never as a stale value.
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

    /// How many entries the cache holds, of any type.
    #[inline]
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Whether nothing has been cached yet.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// The keys currently held, in unspecified order — a `HashMap` has no sequence
    /// to promise, and a run has no reason to pay for one. Sort the collected keys
    /// if you need a stable listing. Keys carry no type, so pairing this with
    /// [`PlanCache::has::<T>`] is how you ask which of them hold a `T`.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &str> + '_ {
        self.values.keys().map(String::as_str)
    }

    /// Drops the entry under `key`; `false` when there was none. Whatever type it
    /// held is dropped with it, which is what makes this the only removal that
    /// works when the held type is not known here.
    #[inline]
    pub fn remove(&mut self, key: &str) -> bool {
        self.values.remove(key).is_some()
    }

    /// Drops every entry, keeping the allocation.
    ///
    /// [`Plan::run`] builds a fresh cache per run and drops it at the end, so this
    /// is reachable only from a custom [`StepExecutor::apply`], which is handed
    /// `&mut PlanCache` — the one place a step can decide that everything cached so
    /// far is stale.
    #[inline]
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

// ---------- PlanError ----------

/// Why a [`Plan`] step could not be executed.
///
/// Every failure a step can hit travels through this type: no value-routing path
/// in this module panics. It is plain data, hence `Send + Sync`, so it crosses the
/// `dyn StepExecutor` boundary back to [`Plan::run`] without extra boxing.
///
/// Both the enum and each of its struct variants are `#[non_exhaustive]`, so code
/// outside this crate can match on them (with a wildcard arm and `..`) but cannot
/// build one by naming a variant. A custom [`StepExecutor`] therefore reports a
/// failure by propagating a `PlanError` it received — such as the one a delegated
/// step's [`StepExecutor::evaluate`] returns — plus the one failure it detects
/// itself, built through [`PlanError::step_value_mismatch`], the only constructor
/// this enum offers.
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

impl PlanError {
    /// Builds the [`PlanError::StepValueMismatch`] case: `step`'s `apply` received a
    /// value that is not the one its own `evaluate` produces.
    ///
    /// This is a constructor rather than a public variant because the variants are
    /// `#[non_exhaustive]`: naming one from outside this crate is a compile error,
    /// and a downstream [`StepExecutor`] that hands `apply` a value it cannot write
    /// would otherwise have to swallow the fact. `step` is the name to blame — a
    /// type name, as `std::any::type_name_of_val` gives you, or any string that
    /// identifies the step — and `expected` says what that `apply` needed.
    #[must_use]
    pub fn step_value_mismatch(step: &'static str, expected: &'static str) -> Self {
        Self::StepValueMismatch { step, expected }
    }
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
/// * The two phases do not interleave. Within one step, `evaluate` finishes before
///   `apply` begins, so nothing a step writes can be read by that same step. For a
///   step that carries several elements — a tuple, see the [`StepExecutor`] impl on
///   tuples — this holds across the whole step: `evaluate` runs for every element
///   before `apply` runs for any of them, so one element cannot observe what another
///   element writes. A value written by an element becomes visible to the *next*
///   step, never to a later element of the same one.
/// * Neither method panics on a value-routing path. A missing or wrong-typed cache
///   entry, and a boxed value that does not match the step, are reported as
///   [`PlanError`] — never swallowed and never panicked — and `Plan::run` stops at
///   the first `Err` and hands it to the caller. That guarantee comes with the rule
///   of how a step reports one: the variants are `#[non_exhaustive]`, so an
///   implementation outside this crate propagates a `PlanError` it received (from a
///   step it delegates to, say) and constructs only the failure it can name, through
///   [`PlanError::step_value_mismatch`]. A downstream step that neither propagates
///   nor constructs an error is not honouring this contract; it is swallowing a
///   failure.
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

/// An ordered list of steps, executed once by [`Plan::run`].
///
/// `#[must_use]` on the type: a plan does nothing until it is run, so a plan built
/// and dropped as a statement is always a mistake. On the type rather than on the
/// constructors, so it covers every way to obtain one — [`Plan::new`],
/// [`Default::default`], [`Plan::step`], and any function returning a plan.
#[must_use = "a plan does nothing until it is run"]
pub struct Plan<C: Context> {
    steps: Vec<Box<dyn StepExecutor<C>>>,
    _marker: PhantomData<C>,
}

impl<C: ContextMut> Plan<C> {
    /// An empty plan.
    ///
    /// `Plan` is `#[must_use]`, because a plan that is built and never run does
    /// nothing: a discarded builder is always a mistake, here or at [`Plan::step`].
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            _marker: PhantomData,
        }
    }

    /// Consumes the builder and returns it with `step` appended.
    ///
    /// The step is added to the returned plan, not to the receiver, so calling this
    /// as a bare statement discards the step instead of adding it — one of the ways
    /// `Plan` being `#[must_use]` earns that attribute.
    pub fn step<S>(mut self, step: S) -> Self
    where
        S: StepExecutor<C> + 'static,
    {
        self.steps.push(Box::new(step));
        self
    }

    /// How many steps the plan holds. A tuple of steps counts as one step, because
    /// the tuple is one step: see [`StepExecutor`].
    #[inline]
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    /// Whether the plan holds no step at all, in which case [`Plan::run`] allocates
    /// a cache and returns `Ok(())` without touching the context.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
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
    // Carries `N`, the value type this step writes. It is not a field type — the
    // expression and the setter both mention it indirectly — so nothing but this
    // marker pins it, and [`AssignmentStep::new`] is the only way to name it.
    _marker: PhantomData<N>,
}

impl<N: 'static, E, S> AssignmentStep<N, E, S> {
    /// Builds an assignment step: `expr` is evaluated, handed to `setter_fn`, and
    /// cloned into the run cache under `cache_key` when there is one.
    ///
    /// [`save!`](crate::save) is the shorter spelling of the same step. Writing a
    /// `AssignmentStep { setter_fn, expr, cache_key, _marker }` literal is not an
    /// option any more — `_marker` became private, so this constructor is the
    /// route for a step built outside the crate.
    pub fn new(setter_fn: S, expr: E, cache_key: Option<String>) -> Self {
        Self {
            setter_fn,
            expr,
            cache_key,
            _marker: PhantomData,
        }
    }
}

// Hand-written so the bounds land on the payload types only. There is no `Copy`
// twin: `cache_key` owns a `String`, so the step can never be `Copy`, whatever
// the bounds say.
impl<N: 'static, E: Clone, S: Clone> Clone for AssignmentStep<N, E, S> {
    fn clone(&self) -> Self {
        Self::new(
            self.setter_fn.clone(),
            self.expr.clone(),
            self.cache_key.clone(),
        )
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
/// with [`Plan::run`], which returns `Result<(), PlanError>`. Under [`Plan::run`] an
/// assignment step always evaluates and always applies, because `run` feeds `apply`
/// the very value that step's own `evaluate` produced, so it is never the step that
/// fails a run. Hand-calling [`StepExecutor::apply`] with a value of another type is
/// the exception: it reports [`PlanError::StepValueMismatch`] and writes nothing.
///
/// Examples:
///   let plan = plan.step(save!(CombatState, damage_dealt, Node::lit(35)));
///   let plan = plan.step(save!(CombatState, heal_received, heal_expr, "applied_heal"));
#[macro_export]
macro_rules! save {
    ($ctx_ty:ty, $field:ident, $expr:expr) => {{
        let setter_fn = |ctx: &mut $ctx_ty, val| ctx.$field = val;
        $crate::plan::AssignmentStep::new(setter_fn, $expr, None)
    }};
    ($ctx_ty:ty, $field:ident, $expr:expr, $key:expr) => {{
        let setter_fn = |ctx: &mut $ctx_ty, val| ctx.$field = val;
        $crate::plan::AssignmentStep::new(setter_fn, $expr, Some($key.to_string()))
    }};
}

/// Inline macro to create a cache-save step: [`CacheSaveStep`], which writes the
/// expression's value into the run's [`PlanCache`] and writes nothing to the
/// context. Feed it to [`Plan::step`]; under [`Plan::run`] a cache-save step never
/// fails a run, for the same reason an assignment step does — `run` hands it its own
/// `evaluate` output. A hand-called [`StepExecutor::apply`] with a value of another
/// type does report [`PlanError::StepValueMismatch`].
///
/// Usage:
///   let plan = plan.step(save_cache!("my_key", Node::lit(42)));
///
/// The key is a `&'static str` — a literal, a `const`, or a leaked `String` —
/// unlike the owned key the four-argument `save!` accepts and `load_cache!`
/// looks up with. A key built at run time gives `E0597` here; the 0.3.0 release
/// notes list this as a known inconsistency.
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
///
/// The key is anything `Into<String>`, so an owned, run-time key works here even
/// though `save_cache!` and [`Node::alias`](crate::nodes::Node::alias) demand
/// `&'static str`.
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
        /// Several steps grouped into one [`Plan`] step.
        ///
        /// The tuple is a single step, so the two phases are batched over the whole
        /// tuple: `evaluate` runs for every element, front to back, before `apply`
        /// runs for any of them. Declaration order therefore decides which value goes
        /// to which element (element one gets value one), not whether one element's
        /// write lands before another element reads it — it does not. Both elements
        /// read the context and the cache as they stood when the step started.
        ///
        /// ```text
        /// // One step, two elements; the second reads the field the first writes.
        /// let mut state = State { a: 1, b: 0 };
        /// Plan::<Ctx>::new()
        ///     .step((
        ///         save!(State, a, Node::lit(7)),                 // writes a = 7
        ///         save!(State, b, Node::new(|ctx: &State| ctx.a)), // reads a: still 1
        ///     ))
        ///     .run(&mut state)
        ///     .expect("both elements apply");
        /// assert_eq!((state.a, state.b), (7, 1));
        ///
        /// // The same two elements as two steps: now the write is visible.
        /// let mut state = State { a: 1, b: 0 };
        /// Plan::<Ctx>::new()
        ///     .step(save!(State, a, Node::lit(7)))
        ///     .step(save!(State, b, Node::new(|ctx: &State| ctx.a)))
        ///     .run(&mut state)
        ///     .expect("both steps apply");
        /// assert_eq!((state.a, state.b), (7, 7));
        /// ```
        ///
        /// The run cache is batched the same way: `save_cache!("k", e)` grouped with
        /// `load_cache!(T, "k")` in one tuple is a [`PlanError::CacheMiss`], because
        /// the lookup evaluates before the save applies. Make them two steps.
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
    fn elements_of_one_tuple_step_read_the_state_before_the_tuple_ran() {
        // A tuple is ONE step: `evaluate` runs for every element before `apply` runs
        // for any of them, so element two reads `a` as it stood before the step and
        // never sees element one's write.
        let mut state = State {
            a: 3,
            ..State::default()
        };
        let plan = Plan::<Ctx>::new().step((
            save!(State, a, Node::lit(7)),
            save!(State, b, Node::new(|ctx: &State| ctx.a)),
        ));

        plan.run(&mut state)
            .expect("a stale read is not an error: both elements apply");

        assert_eq!(
            (state.a, state.b),
            (7, 3),
            "element two observed the pre-step `a`, not element one's write"
        );

        // The same two elements as two steps: the write of step one is visible to
        // step two, which is the only way to get the sequential reading.
        let mut state = State {
            a: 3,
            ..State::default()
        };
        let plan = Plan::<Ctx>::new()
            .step(save!(State, a, Node::lit(7)))
            .step(save!(State, b, Node::new(|ctx: &State| ctx.a)));

        plan.run(&mut state).expect("two steps must apply");

        assert_eq!((state.a, state.b), (7, 7), "step two read step one");
    }

    #[test]
    fn cache_write_and_lookup_of_one_key_in_one_tuple_report_a_cache_miss() {
        // One step: the tuple evaluates the lookup before the save's `apply` has
        // written the key, so the key is genuinely absent at lookup time.
        let mut state = State::default();
        let plan =
            Plan::<Ctx>::new().step((save_cache!("note", Node::lit(9)), load_cache!(i32, "note")));

        let err = plan
            .run(&mut state)
            .expect_err("a lookup cannot see a write from its own step");

        assert_eq!(
            err,
            PlanError::CacheMiss {
                key: "note".to_string()
            }
        );

        // The very same two steps, one step each: the write lands first.
        let mut state = State::default();
        Plan::<Ctx>::new()
            .step(save_cache!("note", Node::lit(9)))
            .step(load_cache!(i32, "note"))
            .run(&mut state)
            .expect("as separate steps the write is visible to the lookup");
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

    #[test]
    fn cache_len_keys_remove_and_clear_track_the_entries() {
        let mut cache = PlanCache::default();
        assert!(cache.is_empty());

        cache.set("hp", 10i32);
        cache.set("scale", 2.0f32);
        assert_eq!(cache.len(), 2);
        assert!(!cache.is_empty());

        let mut keys: Vec<&str> = cache.keys().collect();
        keys.sort_unstable();
        assert_eq!(keys, ["hp", "scale"], "keys lists entries of every type");

        // Removal is by key alone: the caller does not have to know the entry
        // holds an i32, which `get::<T>` and `has::<T>` both insist on.
        assert!(cache.remove("hp"));
        assert!(
            !cache.remove("hp"),
            "a second removal has nothing to remove"
        );
        assert!(!cache.contains_key("hp"));
        assert!(cache.has::<f32>("scale"), "the other entry is untouched");

        cache.clear();
        assert!(
            cache.is_empty(),
            "clear is the one invalidation a custom step can apply to everything"
        );
    }

    #[test]
    fn a_replaced_key_holds_the_new_type_and_not_the_old_one() {
        let mut cache = PlanCache::default();
        cache.set("damage", 35i32);
        cache.set("damage", 2.5f32);

        assert_eq!(
            cache.len(),
            1,
            "a write over a key replaces, it does not add"
        );
        assert!(cache.get::<i32>("damage").is_none());
        assert_eq!(cache.get::<f32>("damage"), Some(&2.5));
        assert!(
            cache.contains_key("damage"),
            "the key is still there, holding another type — the distinction \
             `has::<T>` collapses and a reader turns into CacheTypeMismatch"
        );
    }

    #[test]
    fn plan_reports_its_step_count_and_a_tuple_counts_once() {
        let empty = Plan::<Ctx>::new();
        assert!(empty.is_empty());
        assert_eq!(empty.len(), 0);

        let mut state = State::default();
        empty
            .run(&mut state)
            .expect("an empty plan runs and changes nothing");
        assert_eq!(state, State::default());

        let one = Plan::<Ctx>::new().step(save!(State, hp, Node::lit(1)));
        assert_eq!(one.len(), 1);
        assert!(!one.is_empty());

        let grouped = Plan::<Ctx>::new().step((
            save!(State, hp, Node::lit(1)),
            save!(State, note, Node::lit(2)),
        ));
        assert_eq!(grouped.len(), 1, "the tuple is one step, not two");
    }
}
