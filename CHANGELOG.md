# Changelog

Behaviour changes that matter to a caller of `express-it`: what started working,
stopped working, or changed meaning. The current API is documented in `README.md`
and in `cargo doc --open`; this file only records what moved between releases.

## Unreleased

All additive except where noted.

* `Node::from_expr(expr)` builds a node from a leaf the caller wrote. It takes no
  `Expr` bound, since storing a leaf asks for none. Writing
  `Node { expr, _marker: PhantomData }` still compiles — `Node`'s `_marker` field
  is now `#[doc(hidden)]` but stays `pub`, and `tests/public_marker.rs` pins that
  from a separate crate — but it is no longer the documented way in.
* `var_leaf!(Name, Value, Ctx, |ctx| field)` declares one attribute as its own
  zero-sized leaf, with its own dependency key and its own `Display` name. This is
  the sixth exported macro and it is re-exported from the prelude. It accepts
  attributes inside the invocation, as `var_leaf!(#[doc = "…"], …)`, so a crate
  under `#![deny(missing_docs)]` can document the generated item.
* A variable built through `Node::new` still shares one dependency key with every
  other variable of the same value type and context; that behaviour is unchanged
  and still pinned. Two `var_leaf!` attributes report two keys, so a composite
  expression can now say which two attributes it depends on.
* `PlanError::step_value_mismatch(step, expected)` constructs the variant that
  `#[non_exhaustive]` makes unwritable outside the crate, so a hand-written
  `StepExecutor` can report a mistyped value instead of swallowing it.
* `Plan` carries `#[must_use]` on the type, so a discarded plan is diagnosed
  whichever way it was built; `Plan::default();` was silent before. The
  method-level attributes on `new` and `step` are gone, since clippy reports
  `double_must_use` once the type has one.

## 0.2.0

* `Plan::run` now returns `Result<(), PlanError>` instead of panicking: a missing
  cache key, a key holding the wrong type, and a value handed to the wrong step all
  come back as an error. The steps before the failure stay applied.
* Steps grouped in a tuple are now applied in declaration order — element one gets
  value one. They were applied reversed, so a tuple step wrote the wrong fields.
  This fixes which element receives which value; it does not make the elements of a
  tuple sequential. A tuple is still one step, so every element evaluates before any
  element applies, and no element can read another element's write.
* `Node::cast::<T>()` is total: it performs Rust's `as` conversion instead of
  panicking on a cast that `NumCast` could not represent.
* The cast bound narrowed. `Node::cast` now requires `AsCast`, which this crate
  implements for the primitive numeric types; a `Num` type of your own needs its own
  `AsCast` impl to stay castable.
* `PlanCache::has::<T>` is now type-strict — the only cache change here that neither
  fails to compile nor reports an error at run time. A key that holds some other type
  now simply answers `false` where it used to answer `true`. (The tuple ordering fix
  and the total `cast` above are silent in their own way too: one quietly starts
  writing the right fields, the other quietly stops panicking.)
* The criterion benches in `benches/` now actually execute; they were declared
  without being wired up, so `cargo bench` ran nothing.
* `PlanError` is `#[non_exhaustive]`, on the enum and on every variant, so adding a
  variant or a field later is not a semver break. The cost: code outside the crate
  can no longer construct those variants by naming one, so a `StepExecutor` you write
  yourself reports a failure by propagating one it received — say from a step it
  delegates to — and builds the one failure it detects itself through
  `PlanError::step_value_mismatch`, the public constructor for that case.
* `Plan` is `#[must_use]`, so a plan that is built and never run is a warning rather
  than silence.
* The bevy example terminates: it drives the schedule once and returns instead of
  looping forever.
