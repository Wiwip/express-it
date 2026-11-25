# Changelog

Behaviour changes that matter to a caller of `express-it`: what started working,
stopped working, or changed meaning. The current API is documented in `README.md`
and in `cargo doc --open`; this file only records what moved between releases.

## 0.3.0

A breaking release. Three intentional breaks, listed first with their
migrations; everything else is additive or documentation.

### Breaking

**1. No public type exposes a marker field.** `Node`, the generated comparison
nodes and `AssignmentStep` each had a `pub _marker: PhantomData<..>` that had to
be spelled out to build the type. Every one is private now, and each type has a
constructor, so the marker was never the only way in.

```text
0.2  let n = Node { expr: leaf, _marker: PhantomData };
0.3  let n = Node::<f32, Ctx, _>::from_expr(leaf);

0.2  GreaterThanNode { lhs: a, rhs: b, _marker: PhantomData }
0.3  GreaterThanNode::new(a, b)

0.2  AssignmentStep { setter_fn, expr, cache_key: None, _marker: PhantomData }
0.3  AssignmentStep::new(setter_fn, expr, None)
```

`Node::lit`, `Node::new`, `Node::var`, every operator method and the `save!`,
`save_cache!` and `load_cache!` macros are unchanged, so this only reaches code
that wrote those literals by hand. `Node::expr` stays public for reading. A 0.2
literal now fails with `E0451: field \`_marker\` of struct \`Node\` is private`.
`CastNode`, `Plan`, `CacheLookupStep` and `CacheSaveStep` were already private
and are unchanged.

**2. Dependencies are keyed by name as well as by type.** `Expr::get_dependencies`
collected a `HashSet<TypeId>`, which forces a type to stand in for an identity:
every variable built through `Node::new` with one value type and context reported
one key, and telling two attributes apart required writing a leaf type each. The
sink is now `Deps`, a set of `DepKey`, where a key is either `DepKey::Type` (what
`var_leaf!` and hand-written leaves report) or `DepKey::Name` (what `Node::var`
reports).

```text
0.2  fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
         deps.insert(TypeId::of::<Self>());
     }
0.3  fn get_dependencies(&self, deps: &mut Deps) {
         deps.insert(DepKey::type_of::<Self>());
     }
```

Per-attribute identity now has two routes, and neither needs a leaf type:

```text
let hp = Node::<f32, Ctx, _>::var("health", |ctx| ctx.health);   // DepKey::Name
express_it::var_leaf!(pub Health, f32, Ctx, |ctx| ctx.health);   // DepKey::Type
```

`Node::dependencies()` returns a `Deps` outright, so callers no longer build a
sink, and `Deps::is_superset_of` answers the containment question a watcher or a
plan asks. `Node::new` is deliberately unchanged: it still reports one shared
key, which is correct when dependencies are not tracked, and its pinning test
still passes. `Deps` is `PartialEq` but not `Ord`, because a hash set has no
meaningful order and an ordering over iteration would be a lie. `contains_name`
takes a `&'static str`.

**3. `var_leaf!` takes a visibility token.** `var_leaf!(pub Health, f32, Ctx,
|ctx| ctx.health)`. A call with no token still compiles and now generates a
*private* leaf — the default follows `bitflags!` and `thread_local!` — so a 0.2
call site that relied on the implicit `pub` fails at its first cross-module use
with `E0603`, not at the macro. Attributes still go before the visibility,
because rustc discards documentation written ahead of a macro call.

### Added

* `Node::from_expr(expr)` builds a node from a caller-supplied leaf; `var_leaf!`
  declares an attribute as its own zero-sized leaf with its own dependency key
  and `Display` name, and accepts attributes inside the invocation so a crate
  under `#![deny(missing_docs)]` can document the generated item.
* `PlanError::step_value_mismatch(step, expected)` constructs the variant that
  `#[non_exhaustive]` makes unwritable outside the crate, so a hand-written
  `StepExecutor` can report a mistyped value instead of swallowing it.
* `#[must_use]` on `Plan`: a discarded plan is diagnosed however it was built,
  and `Plan::default();` used to be silent. The method-level attributes on `new`
  and `step` are gone, since clippy reports `double_must_use` once the type has
  one.
* Dual MIT OR Apache-2.0 licensing, which also unblocks `cargo publish`.

### Documented, not changed

Steps grouped in a tuple form ONE step: every element is evaluated before any
element is applied, so elements never observe each other's writes, and a
`save_cache!` tupleed with a `load_cache!` of the same key is a `CacheMiss`.
That behaviour predates this release and is unchanged; what was missing was the
saying, since "applied in declaration order" invited the opposite reading.

### Verification

`cargo test` on 0.3.0 runs unit, integration, doc and property tests. The
property suite checks dependency monotonicity across every combinator family, the
coexistence of the two key kinds, plan prefix-application on failure, and that
`cast` agrees with Rust's `as` for every pair it claims to cover, including NaN
and infinities. Continuous integration builds on stable and on 1.85.0, the
advertised `rust-version`, across Linux, macOS and Windows, and runs
`cargo publish --dry-run`, which is the check that a missing licence field or a
broken `readme` path cannot ship.

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
