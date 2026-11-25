# express-it

A typed expression-tree builder and a sequential plan runner for game logic.

You describe a computation once as a tree of `Node`s — variables read out of your
own context type, literals, arithmetic, comparisons, boolean combinators, `clamp`,
casts — and evaluate that tree against a value whenever you like. Nothing is
interpreted, parsed, or boxed on the way: the tree is plain generic Rust types, and
`eval` walks it.

On top of the trees sits `Plan`: an ordered list of steps that each evaluate an
expression and then write the result somewhere — into a field of your context, into
a per-run typed cache, or both. Steps run in order, so step *N* sees what step
*N-1* wrote.

```text
express_it::expr    Context / ContextMut, Expr, AsExpression, StoredExpr
express_it::nodes   Node, Var, VarExpr, LiteralNode, CastNode, CacheSaveStep
express_it::ops     + - * / %, unary - and !, powf, min, max, abs, acos, sqrt, clamp
express_it::logic   == != < <= > >=, not, and or xor nand nor nxor, all!, any!
express_it::plan    Plan, PlanCache, PlanError, StepExecutor, AssignmentStep,
                    CacheLookupStep, save!, save_cache!, load_cache!
express_it::prelude everything above that a caller has to import, in one glob
```

## Adding the crate

```toml
[dependencies]
express-it = "0.2"
```

## Building an expression

`Node<N, C, E>` is the tree: `N` is the value type, `C` is your context (where
variables read from), `E` is the node's own shape. You build trees through the
constructors and operators and never name `E` yourself.

```rust
use express_it::prelude::*;

struct Stats {
    health: f32,
    max_health: f32,
    attack: f32,
    poisoned: bool,
}

struct GameCtx;

impl Context for GameCtx {
    type ContextItem<'w, 's> = Stats;
}

fn main() {
    let health = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.health);
    let max_health = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.max_health);
    let attack = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.attack);
    let poisoned = Node::<bool, GameCtx, _>::new(|ctx: &Stats| ctx.poisoned);

    // At least 1 damage, never more than a quarter of max health. The bounds of
    // `clamp` may themselves be expressions.
    let damage = attack
        .max(1.0)
        .clamp(Node::lit(0.0), max_health * 0.25);

    // `all!` and `any!` fold a list of conditions into one boolean tree.
    let wounded_and_poisoned =
        all![health.gt(0.0), health.lt(max_health), poisoned];

    let player = Stats {
        health: 60.0,
        max_health: 100.0,
        attack: 24.0,
        poisoned: true,
    };

    assert_eq!(damage.eval(&player), 24.0);
    assert!(wounded_and_poisoned.eval(&player));

    // Trees print themselves, which is handy when a rule needs explaining.
    assert_eq!(
        damage.to_string(),
        "clamp(max(var, 1), 0, (var * 0.25))"
    );
}
```

The same tree is reused, recomposed, and re-evaluated against different contexts —
every `Node` is `Copy` when its payload is, so a node stays usable after you use it.

## Running a plan

A plan is a list of steps. `save!` writes to a context field (and optionally also
under a cache key), `save_cache!` writes only to the per-run cache, and
`load_cache!` reads a key back — which is the step that can fail.

```rust
use express_it::prelude::*;

#[derive(Debug, Clone, Copy)]
struct CombatState {
    health: i32,
    damage_dealt: i32,
}

struct CombatCtx;

impl Context for CombatCtx {
    type ContextItem<'w, 's> = CombatState;
}

impl ContextMut for CombatCtx {
    type ContextItemMut<'w, 's> = CombatState;

    fn as_readonly<'w, 's, 'a>(
        item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's> {
        *item
    }
}

fn main() {
    let mut state = CombatState {
        health: 100,
        damage_dealt: 0,
    };

    let plan = Plan::<CombatCtx>::new()
        // Write a field and remember the value under a cache key.
        .step(save!(CombatState, damage_dealt, Node::lit(35), "applied_damage"))
        // Read the field the previous step just wrote.
        .step(save!(
            CombatState,
            health,
            Node::new(|ctx: &CombatState| ctx.health - ctx.damage_dealt)
        ))
        // Cache-only step: no context write.
        .step(save_cache!(
            "remaining",
            Node::new(|ctx: &CombatState| ctx.health)
        ))
        // Reading a key that this run wrote is fine; reading one nobody wrote
        // would stop the run with PlanError::CacheMiss.
        .step(load_cache!(i32, "applied_damage"));

    // `run` returns `Result<(), PlanError>`: a bad cache read is an error value,
    // not a panic. Steps before the failure stay applied — a failed run is not
    // rolled back.
    plan.run(&mut state).expect("every key this plan reads exists");

    assert_eq!(state.damage_dealt, 35);
    assert_eq!(state.health, 65);
}
```

## Panics and failure semantics

The plan layer never panics on value routing: a missing cache key, a key holding
the wrong type, and a value handed to the wrong step all come back as
`PlanError`, and `Plan::run` stops at the first one.

Expression evaluation is a different story, because it evaluates Rust primitives.
These still panic:

* **Integer division and remainder by zero** — `a / b` and `a % b` panic when `b`
  is zero, in debug and in release alike.
* **Integer overflow** — `a + b`, `a - b`, `a * b` and `-a` panic in a debug build
  and wrap in a release build, exactly like Rust's own overflow checks (so the same
  tree can give different answers per profile). Turn overflow checks on for release
  — `overflow-checks = true` under `[profile.release]`, or
  `RUSTFLAGS="-C overflow-checks=on"` — if you need the panic in both.
* **`clamp`** — panics in **both** profiles when `min > max`, or when either bound
  is NaN. Bounds may be expressions themselves, so a tree can reach this at run
  time. A NaN *value* between valid bounds is not a panic: it comes back as NaN.
  (`0.5.clamp(NaN, 1.0)` panics; `NaN.clamp(0.0, 1.0)` returns NaN.)

`cast` is the deliberate exception: `Node::cast::<T>()` is total and never panics.
It performs Rust's `as` conversion, so `NaN` becomes `0`, out-of-range floats
saturate to the target integer's min/max, and int-to-int casts wrap on two's
complement (`(-1i32).cast::<u32>()` is `u32::MAX`).

A panic inside an expression is not turned into a `PlanError`: `Plan::run` does not
catch unwinding, so a `1 / 0` in a step's tree takes the process down with it just
as it would outside a plan.

## Operators come from traits

`node + 1`, `node * 2.0`, and `-node` need no import: they are `std::ops` impls.
Everything spelled as a method — `.max()`, `.lt()`, `.and()`, `.eval()` — is a trait
of this crate, and a trait has to be in scope for its method to resolve. So
`use express_it::ops::ExprMax;` for one method, or one line covers the lot:

```rust
use express_it::prelude::*;
```

The prelude is purely additive; every existing path, including
`express_it::ops::ExprMax` and `express_it::plan::StepExecutor`, keeps working.

The prelude also carries the names a caller has to write down rather than call:
`StepExecutor` and `PlanCache`, the three step types (`AssignmentStep`,
`CacheSaveStep`, `CacheLookupStep`), and `AsCast` for a generic cast bound —
enough to implement a custom step or name a cast bound from the one glob.

## Known limitation: variables share one dependency key

`Expr::get_dependencies` collects the `TypeId` keys of the leaves a tree reads, so a
caller can register interest before the first evaluation. A variable built with
`Node::new` reports the key of `VarExpr<N, C>` rather than its own identity, so all
variables of one value type and one context collapse to a single key. Two different
variables are therefore indistinguishable to a dependency-driven invalidation layer.

If you need per-attribute identity, write your own leaf: implement `Expr<N, C>` for
your own type, insert your own key in `get_dependencies`, and use it as the `E`
parameter of `Node`. Everything else in the crate forwards dependencies downward,
so your keys arrive intact.

## What changed in 0.2

* `Plan::run` now returns `Result<(), PlanError>` instead of panicking: a missing
  cache key, a key holding the wrong type, and a value handed to the wrong step all
  come back as an error. The steps before the failure stay applied.
* Steps grouped in a tuple are now applied in declaration order — element one gets
  value one. They were applied reversed, so a tuple step wrote the wrong fields.
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
  can no longer construct those variants, so a `StepExecutor` you write yourself
  reports a failure by propagating one it received — say from a step it delegates
  to — rather than building a new error.
* The bevy example terminates: it drives the schedule once and returns instead of
  looping forever.

## Examples and benches

`cargo run --example expr_demo` (bevy-backed variables), `--example conditions`,
`--example display`, `--example plan`; `cargo bench` runs the criterion suites in
`benches/`.

## License

Undecided. There is no license grant yet, so no permission to copy, distribute, or
modify is granted — the crate is not publishable until the owner settles this.
