# Changelog

Behaviour changes that matter to a caller of `express-it`: what started working,
stopped working, or changed meaning. The current API is documented in `README.md`
and in `cargo doc --open`; this file only records what moved between releases.

## Unreleased

Work toward 0.4.0. Two entries under *Breaking* change what an existing program
must write — cache keys are owned, and a named variable prints its name. Everything
else here is additive, a CI guarantee, or the removal of a development-only
dependency.

### Breaking

- Cache keys are owned on both sides. `CacheSaveStep.key` is a `String` where it
  was a `&'static str`, and `Node::alias` and `save_cache!` take any
  `Into<String>`, which is what `load_cache!`, `PlanCache::set` and `save!`'s
  fourth argument already did. A key computed at run time — from an entity id, the
  case that prompted this — now compiles on the write side. This closes the first
  entry of the 0.3.0 *Known limitations*.

  Two consequences. `CacheSaveStep` no longer implements `Copy` (it did for
  `E: Copy`), because a key that owns its string is not `Copy`; `Clone` is
  unaffected, and `AssignmentStep` never had a `Copy` twin for the same reason. And
  a hand-written `CacheSaveStep { key, expr }` literal still compiles but must now
  hand over an owned key — `CacheSaveStep::new`, `Node::alias` or `save_cache!` sidestep
  it. Dependency keys are untouched: `DepKey::Name` still borrows a `&'static str`,
  so `Node::var` names remain `'static` by design.
- A name-keyed variable prints its own name. `Node::var("hp", …)` renders as
  `var:hp` where it used to render `var`, matching how `DepKey` renders a name key.
  A tree of several variables used to print `var` once per variable with nothing to
  tell them apart, which is the opposite of what printing a tree is for.
  Anonymous `Node::new` variables still print `var`, and `var_leaf!` leaves still
  print their own type's `Display`, so only `Node::var` output moves. Anything that
  parses rendered trees has one string to fix.

### Added

- `Deps` grew the set algebra a dependency watcher needs — `intersect_with`,
  `difference_with`, `is_subset_of` — plus `names()`, `types()` and
  `to_sorted_vec()`, which sorts rather than exposing hash order. `iter` still
  walks the set in unspecified order and still allocates nothing.
- `DepKey` is now `Ord`: type keys before name keys, in the order the variants are
  declared, which is what `to_sorted_vec` sorts by. The type half is ordered by
  `TypeId`, whose own ordering the standard library derives from a hash — see the
  method docs for what that does and does not promise.
- `PlanCache` grew `len`, `is_empty`, `keys`, `remove` and `clear`. A custom
  `StepExecutor` could write and read cache entries but could not enumerate them,
  drop one, or invalidate the lot.
- `Plan` grew `len` and `is_empty`, counting a tuple of steps as the single step it
  is.
- `[package.metadata.docs.rs]` with `all-features = true`, so docs.rs builds what a
  consumer reads. The crate declares no features today, so this changes nothing
  about what gets built.
- `save_with!`, the seventh exported macro and the setter-taking twin of `save!`
  (re-exported by the prelude). `save!` generates `|ctx, val| ctx.field = val`, so
  it needs the write view to *be* the struct; a view that is a handle —
  `Rc<RefCell<State>>`, the usual single-threaded shape — has no field to name and
  had no macro at all. Hand-writing `AssignmentStep::new` worked and still does;
  this is the same step in one line.
- `AssignmentStep` had no documentation of its own (nor did its three fields). It
  now states the setter's bound, and a `compile_fail` example pins the one way to
  get it wrong: a setter that captures an `Rc` is `E0277`, because a step is
  `Send + Sync`. A context doc example shows the shape that does work — the
  `Rc<RefCell<_>>` as the *view*, with a `Send + Sync` marker context — and runs.

### Changed

- Every cargo invocation in CI now passes `--locked`; `cargo fmt` takes no such
  flag, so it is the one exception. A plain `cargo bench --no-run` had been
  rewriting `Cargo.lock` (blake3 1.8.7 to 1.8.8) while every check stayed green,
  which means CI could test a graph nobody had run locally.

### Removed

- The `bevy` dev-dependency, and with it the bevy-backed example. The library never
depended on bevy; the example existed to show that an engine's entity reference
fits `Context::ContextItem<'w, 's>`, two lifetimes and all. `examples/expr_demo.rs`
now shows the same shape over a hand-written component store in about fifty lines,
prints the same numbers, and adds the one case the engine version could not show
without a second plugin: a view that refuses a component by name. A `cargo test` on
a clone no longer builds an ECS, and the highest `rust-version` among the
dev-dependencies is `criterion`'s 1.86 instead of 1.97.1.

### Corrected

- The 0.3.0 *Known limitations* entry for `Context: Send + Sync` said a
  single-threaded host "must use `Arc<Mutex<_>>` or `Arc<RwLock<_>>`". It does not.
  Measured: with a `Send + Sync` context marker, `type ContextItem<'w, 's> =
  Rc<RefCell<State>>` compiles, evaluates, and runs plans; `E0277` appears only when
  the context type itself holds the `Rc`, and a context carries no data. The
  original wording is corrected in place in that section, since leaving a false
  limitation published is worse than editing a released note.

Library `missing_docs` warnings went from 149 to 144 across these commits:
`PlanCache::set` had none, `AssignmentStep` and its three fields had none, and
everything added here ships with its own documentation.

## 0.3.0

A breaking release. Whether 0.2.0 ever reached a registry cannot be established
from this repository — there is no tag, and `cargo publish` answers that question
only against the index — so the migrations below are written for a caller building
from `main` at or before the 0.2.0 commits rather than for somebody upgrading a
registry dependency. What *is* checkable here is that `var_leaf!` and
`Node::from_expr` were added during this cycle and are in no state of the branch
before it, which is why they appear under *Added* and not as breaks: no earlier
form of either existed to migrate from.

### Breaking

**1. No public type exposes a marker field.** `Node`, the generated comparison
nodes and `AssignmentStep` each had a `pub _marker: PhantomData<..>` that had to
be spelled out to build the type. Every one is private now, and each type has a
constructor, so the marker was never the only way in.

```text
0.2  let n = Node { expr: leaf, _marker: PhantomData };
0.3  let n = Node::<f32, Ctx, _>::from_expr(leaf);

0.2  GreaterThanNode { lhs: a, rhs: b, _marker: PhantomData }
0.3  GreaterThanNode::<f32, _, _>::new(a, b)

0.2  AssignmentStep { setter_fn, expr, cache_key: None, _marker: PhantomData }
0.3  AssignmentStep::<f32, _, _>::new(setter_fn, expr, None)
```

Both constructors take their value type from the marker rather than from an
argument, so the type parameter has to come from somewhere: written as a bare
statement, `GreaterThanNode::new(a, b)` is `E0282: type annotations needed`, and
so is `AssignmentStep::new(setter, expr, None)`. Inside `save!` or `.step()` the
surrounding context fixes it and no turbofish is needed.

`Node::lit`, `Node::new`, every operator method and the `save!`,
`save_cache!` and `load_cache!` macros are unchanged, so this only reaches code
that wrote those literals by hand. `Node::expr` stays public for reading. A 0.2
literal now fails with `E0451: field \`_marker\` of struct \`Node\` is private`,
once the node's type is written down; with nothing to infer it from the compiler
stops earlier, at `E0282: type annotations needed`.
The marker fields of `CastNode`, `Plan` and `CacheLookupStep` were already
private, so nothing changes there; `CacheSaveStep` has no marker field and both
of its fields stay public, because `save_cache!` expands a
`CacheSaveStep { key, expr }` literal at the *call site* and only compiles while
they are.

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

`Deps` and `DepKey` are re-exported from the prelude, since implementing a leaf
names both in this signature; they also stay at `express_it::expr::{Deps, DepKey}`.

Per-attribute identity now has two routes, and neither needs a leaf type written
by hand:

```text
let hp = Node::<f32, Ctx, _>::var("health", |ctx| ctx.health);   // DepKey::Name
express_it::var_leaf!(pub Health, f32, Ctx, |ctx| ctx.health);   // DepKey::Type
```

`Node::dependencies()` returns a `Deps` outright, so callers no longer build a
sink, and `Deps::is_superset_of` answers the containment question a watcher or a
plan asks. `Node::new` is deliberately unchanged: it still reports one shared
key, which is correct when dependencies are not tracked, and its pinning test
still passes. Three limits on name keys are documented on the methods and worth
repeating here, because they are the kind that surprise: a name must be
`&'static`, so a name produced at runtime cannot become a key (it can still be
queried with `deps.iter().any(|k| k.as_name() == Some(s.as_str()))`); a name is
not scoped to a value type or a context, so `"hp"` means one attribute across the
whole crate, including the empty string, which is a legal key shared by every
empty-named variable; and a set answers "which keys does this tree report", not
"which attributes exist", because two unnamed `Node::new` variables of one type
share one key.

`Deps` is `PartialEq` but deliberately not `Ord`: it is a hash set, and an
ordering over unspecified iteration order is a promise nobody should build on.

The same change added a field to a public struct: `VarExpr` is now
`{ pub fetch_fn, pub key: Option<&'static str> }`, where the `key` field is new.
`VarExpr` is re-exported from the prelude, so a literal written as
`VarExpr { fetch_fn }` — the shape this crate itself used — now fails with
`E0063: missing field \`key\``. Add `key: None`, or use the new `VarExpr::new`
and `VarExpr::with_key` constructors, which cannot drift like this again.

### Added

* `Node::from_expr(expr)` builds a node from a caller-supplied leaf; `var_leaf!`
  declares an attribute as its own zero-sized leaf with its own dependency key
  and `Display` name, and accepts attributes inside the invocation so a crate
  under `#![deny(missing_docs)]` can document the generated item. The leaf's
  visibility is written by the caller — `var_leaf!(pub Health, f32, Ctx, |ctx|
  ctx.health)` — and the macro inserts none of its own, so a call with no token
  generates a leaf private to its module. That default follows `bitflags!` and
  `thread_local!`; using such a leaf outside its module is `E0603`, as for any
  private item. Attributes go before the visibility, because rustc discards
  documentation written ahead of a macro call.
* `PlanError::step_value_mismatch(step, expected)` constructs the variant that
  `#[non_exhaustive]` makes unwritable outside the crate, so a hand-written
  `StepExecutor` can report a mistyped value instead of swallowing it.
* `#[must_use]` on `Plan`: a discarded plan is diagnosed however it was built,
  and `Plan::default();` used to be silent. The attribute sits on the type
  rather than on `new` and `step`, which is what covers `Default`, and clippy
  reports `double_must_use` if both are ever present again.
* `Node::var(name, fetch)` builds a variable whose dependency key is its name,
  and `Node::dependencies()` hands back a `Deps` so a caller never builds a sink
  by hand. Both are new in this cycle, which is also why `Node::new` needed no
  change and still reports one shared key.
* Dual MIT OR Apache-2.0 licensing, with the canonical texts. Cargo only *warns*
  about a manifest with no `license` and packages anyway — measured, exit 0 — so
  this is a licensing decision rather than a packaging fix, and the CI job
  asserts the field and both texts because cargo itself will not.
* The centre of the API now has documentation. `Context` and its `ContextItem`,
  `ContextMut` with its `ContextItemMut` and `as_readonly`, `Expr::eval`,
  `AsExpression` and its `Target`, all five public modules, `Node::lit`,
  `Node::alias`, `Node::expr` and both `CacheSaveStep` fields were bare
  declarations before — eighteen items, which is exactly the 167-to-149 drop in
  `missing_docs` warnings. `as_readonly` is the one signature a user has to get
  exactly right: three named lifetimes, with `'a` and `'s` — not `'w` — in the
  return type. Writing it with `'_` instead gives `E0106` plus `E0195` and points
  nowhere useful, so the trait page now carries a working implementation and a
  `compile_fail` example of the elided form. A write view that borrows must put
  its borrow in the read view's *first* lifetime parameter, which the declaration
  only implies, so that case is shown as a compiled example too. Two further
  `compile_fail` examples pin the `Send + Sync` bound on `Context` (an
  `Rc<RefCell<_>>` inside a context is `E0277`) and the run-time cache key below.

### Documented, not changed

Steps grouped in a tuple form ONE step: every element is evaluated before any
element is applied, so elements never observe each other's writes, and a
`save_cache!` tupleed with a `load_cache!` of the same key is a `CacheMiss`.
That behaviour predates this release and is unchanged; what was missing was the
saying, since "applied in declaration order" invited the opposite reading.

### Known limitations

Recorded here because a caller meets all three, and none of them is fixed in this
release — they are documented, and each has a note at the item that raises it.

* **Cache keys came in two lifetimes.** `save_cache!` and `Node::alias` took
  `&'static str`; the four-argument `save!`, `load_cache!` and `PlanCache` all
  took an owned key. A key built at run time therefore compiled for the lookup and
  failed for the store with `E0597` — the borrow does not live long enough — which is
  the obvious thing to try when a key is derived from an entity id. The split
  traced to the first commit of the current design, and was left here as an API
  inconsistency for a release that may break. That release closed it: both sides
  now take `Into<String>`, so this limitation no longer applies.
* **`Context: Send + Sync`.** A context *type* holding `Rc<RefCell<_>>` is rejected
  with `E0277`. The bound stops at the context type and does not reach the state,
  so a single-threaded host keeps `Rc<RefCell<State>>` as its read and write view;
  `save_with!` (added since 0.3.0) writes through such a handle, and `Arc<Mutex<_>>`
  or `Arc<RwLock<_>>` is not required. This bullet originally claimed a
  single-threaded host "must use" those — see the correction note in *Unreleased*.
  The bound arrived one commit into the 0.2.0 window — `8bf50ab`, with `Cargo.toml`
  already reading 0.2.0 — and was stated nowhere until now, so anyone pinning an
  older checkout finds it unannounced in the source and unmentioned here.
* **A boxed sub-expression cannot be displayed.** `Display` for `Node` asks its
  payload to be `Display`, and `Box<dyn Expr>` is not, so a tree deepened through
  `erase()` evaluates and reports dependencies but will not print. Documented on
  both the helper and `StoredExpr`.

The crate also does not yet enforce its own documentation: `RUSTFLAGS="-W
missing_docs" cargo check` reports 149 warnings, down from 167 in this cycle, and
almost all of them are the macro-generated node structs and operator traits rather
than hand-written API. Turning the lint on in `src/lib.rs` would fail CI at once,
so it is left off; 149 is the number to drive down before it goes on.

### Verification

`cargo test` on 0.3.0 runs 184 tests: 121 unit, 7 macro hygiene, 6 plan, 12
prelude, 18 property, 3 stored-expression and 17 doctests. Three of the doctests
are `compile_fail`, pinning the errors the documentation promises rather than the
values it promises. The
property suite checks dependency monotonicity across every combinator family (a
composite reports its operands' keys and nothing more), the coexistence of the
two key kinds, plan prefix-application on failure, and that `cast` agrees with
Rust's `as` on 8 representative pairs — float to signed and unsigned integers,
integer wrap, widening, narrowing — plus the documented specials written out per
source type: NaN to `0`, `+∞` saturating to the target's maximum, `-∞` to its
minimum, including from `f32` as well as `f64`.

Continuous integration is configured to build on stable across Linux, macOS and
Windows, and to check the library alone against 1.85.0, the advertised
`rust-version`. Nothing here has run that workflow yet, so it is a configuration
and not a result — and it is deliberately library-only, because the dev targets
pull in `criterion` (which requires 1.86) and `bevy` (1.97.1), so a full test run
cannot happen on the version the library itself supports. The `1.85.0` claim is
the one promise in this release that no tool available here can check: no 1.85
toolchain is installed and none can be fetched offline. `cargo publish --dry-run`
is likewise the last CI step and not a local one, since it needs the registry
index; locally the equivalent is `cargo package`, which builds the packed crate
standalone against `num-traits` alone.

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
* The bevy example terminates: it drives the schedule once and returns instead of
  looping forever.
