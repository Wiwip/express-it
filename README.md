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
express_it::expr    Context / ContextMut, Expr, AsExpression, StoredExpr, DepKey, Deps
express_it::nodes   Node, Var, VarExpr, CacheSaveStep; LiteralNode and CastNode,
                    the tree shapes, live here and only here — see the last line
express_it::ops     + - * / %, unary - and !, powf, min, max, abs, acos, sqrt, clamp
express_it::logic   == != < <= > >=, not, and or xor nand nor nxor
express_it::plan    Plan, PlanCache, PlanError, StepExecutor, AssignmentStep,
                    CacheLookupStep
express_it          the six macros, each `#[macro_export]`ed, so this is their only
                    home: all!, any!, save!, save_cache!, load_cache!, var_leaf!
                    (`express_it::logic::all` and `express_it::plan::save` do not
                    resolve — E0432)
express_it::prelude every trait whose method you call, every step type you have to
                    name, and all six macros, in one glob — the two tree shapes are
                    the deliberate exception
```

## Adding the crate

```toml
[dependencies]
express-it = "0.2"
```

## Building an expression

`Node<N, C, E>` is the tree: `N` is the value type, `C` is your context (where
variables read from), `E` is the node's own shape. You build trees through the four
constructors — `Node::lit` for a constant, `Node::new` for a variable, `Node::var`
for a variable that is its own dependency key, and `Node::from_expr` for a leaf of
your own — and the operators, and you never name `E` yourself.

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

Steps grouped into a tuple — `(save!(..), save!(..))` handed to one `.step(..)` —
run as a *single* step: every element is evaluated before any element is applied.
Declaration order then decides which value goes to which element, not what each
element reads. Two elements of one tuple never see each other's writes, and
`save_cache!("k", e)` tupleed together with `load_cache!(T, "k")` is a
`PlanError::CacheMiss`, because the lookup evaluates before the save writes. Make
them two steps whenever one has to feed the other.

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

## Dependency keys: names and types

`Expr::get_dependencies` collects the keys of the leaves a tree reads, so a caller
can register interest before the first evaluation. Ask a node for its keys with
`node.dependencies()`: it hands back a `Deps` set. Two caveats travel with it. The
answer is a **may-read superset** — `a && b` reports both operands even though a
live `eval` short-circuits — and its iteration order is unspecified, because it is
a hash set; `Deps` is comparable for equality but has no ordering.

A key is a `DepKey`, and there are two kinds:

* `DepKey::Name` — a variable's **name**, reported by `Node::var`. Two attributes
  of one value type and one context are two keys, with no leaf type written. Names
  compare by exact string equality, so `"hp"` and `"HP"` are two keys.
* `DepKey::Type` — a leaf **type**'s identity, reported by `var_leaf!` and by a
  hand-written leaf. `Node::new` reports this kind too: the key of `VarExpr<N, C>`,
  so every variable built that way with one value type and one context shares a
  single key.

```rust
use express_it::prelude::*;

struct Stats {
    health: f32,
    attack: f32,
}

struct GameCtx;

impl Context for GameCtx {
    type ContextItem<'w, 's> = Stats;
}

// Named: one key per attribute, and you can ask a set for one attribute by name.
let hp = Node::<f32, GameCtx, _>::var("health", |ctx: &Stats| ctx.health);
let attack = Node::<f32, GameCtx, _>::var("attack", |ctx: &Stats| ctx.attack);

let deps = (hp + attack).dependencies();
assert_eq!(deps.len(), 2);
assert!(deps.contains_name("health"));
assert!(deps.contains_name("attack"));
assert!(!deps.contains_name("defense"), "a name the tree never reads");
assert!(deps.is_superset_of(&hp.dependencies()));

// The same two attributes as `Node::new` variables: one key between them, so a
// dependency layer cannot tell them apart.
let hp_var = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.health);
let attack_var = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.attack);
assert_eq!(hp_var.dependencies(), attack_var.dependencies());
assert_eq!((hp_var + attack_var).dependencies().len(), 1);

// Keys are a collection like any other: iterate, union, or test membership.
let mut both = hp.dependencies();
both.union_with(&attack.dependencies());
assert_eq!(both.iter().filter(|key| key.as_name().is_some()).count(), 2);

// A tree of literals reads nothing at all.
assert!(Node::<f32, GameCtx, _>::lit(1.0).dependencies().is_empty());
```

Give each attribute its own leaf type instead of a name and the keys separate the
same way — a leaf's key is then its own type. `var_leaf!` declares such a leaf: a
zero-sized `Expr` implementor whose `get_dependencies` inserts
`DepKey::type_of::<Self>()` and whose `Display` prints its own name, and
`Node::from_expr` makes a node out of it. The name is written with the visibility
the leaf should have (`pub` here, `pub(crate)`, `pub(super)`, or nothing for a leaf
private to its module); the macro inserts no visibility of its own, so a call that
names none now generates a private leaf where 0.2 generated a `pub` one. The last
argument is the fetch, `|ctx| ..`, with `ctx` bound to a reference to your context
item:

```rust
use express_it::prelude::*;

struct Stats {
    health: f32,
    attack: f32,
}

struct GameCtx;

impl Context for GameCtx {
    type ContextItem<'w, 's> = Stats;
}

var_leaf!(pub Health, f32, GameCtx, |ctx| ctx.health);
var_leaf!(pub Attack, f32, GameCtx, |ctx| ctx.attack);

let health = Node::<f32, GameCtx, _>::from_expr(Health);
let attack = Node::<f32, GameCtx, _>::from_expr(Attack);

// Two attributes, two type keys, and the tree reads by attribute name.
assert_ne!(health.dependencies(), attack.dependencies());
assert!(health.dependencies().contains_type_of::<Health>());
assert_eq!((health + attack).to_string(), "(Health + Attack)");

// The two keying schemes coexist: one name key and one type key in the same set,
// and neither is found by the other's check.
let shield = Node::<f32, GameCtx, _>::var("shield", |ctx: &Stats| ctx.health * 2.0);
let mixed = (health + shield).dependencies();
assert_eq!(mixed.len(), 2);
assert!(mixed.contains_type_of::<Health>());
assert!(mixed.contains_name("shield"));
```

Choose by what the identity is: a name via `Node::var` when the attribute is what
you want to key on and nothing else, a type via `var_leaf!` when the leaf itself is
worth having — a `Display` name for the tree, a fetch compiled into `eval`, a leaf
several nodes share.

Writing the leaf by hand works just as well: implement `Expr<N, C>` for your own
type, insert `DepKey::type_of::<Self>()` in `get_dependencies`, and hand it to
`Node::from_expr` — the `Expr` trait doc (`express_it::expr::Expr`) shows that impl
in full. `var_leaf!` is that shape spelled shorter. Everything else in the crate
forwards dependencies downward, so a leaf's keys arrive intact however deep it sits
in the tree.

## Version history

Release-by-release behaviour changes live in
[CHANGELOG.md](CHANGELOG.md) at the repository root. This page describes the
current API and does not carry release notes.

## Examples and benches

The examples in `examples/` and the criterion suites in `benches/` are development
targets of this crate: they belong to a clone of the repository, not to depending
on the crate, because they need its dev-dependencies (`bevy`, `criterion`). From a
clone of the repository:

```sh
cargo run --example expr_demo   # bevy-backed variables
cargo run --example conditions
cargo run --example display
cargo run --example plan
cargo bench                     # the four criterion suites
```

## License

Dual-licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  <https://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <https://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
