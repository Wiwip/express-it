use crate::expr::{Context, DepKey, Deps, Expr};
use num_traits::Num;
use std::any::type_name;
use std::fmt;
use std::marker::PhantomData;

/// An expression tree: a leaf `E` plus the value type `N` and context `C` it is
/// read in.
///
/// Build one with [`Node::lit`], [`Node::new`], [`Node::var`] or
/// [`Node::from_expr`], then compose it with the operators and the method nodes.
///
/// Everything the crate does over a tree is recursive — `eval`, the dependency
/// walk behind [`Node::dependencies`], `Display`, and `Drop` each descend one
/// frame per level — so a tree that is thousands of nodes deep along a single
/// branch needs a stack that can hold it. `ops::tests` builds a 4,000-deep chain
/// and evaluates it as the margin check; widening it is the way to find the limit
/// on a given thread.
pub struct Node<N, C, E> {
    /// The leaf this node wraps.
    ///
    /// Public so a caller can keep or reuse the leaf itself; nothing about
    /// building or evaluating a tree needs it. The marker beside it is private —
    /// a node carrying a leaf of your own comes from [`Node::from_expr`].
    pub expr: E,
    // Carries the `N` and `C` parameters, which the struct otherwise would not
    // mention. Private as of 0.3: a caller supplies a leaf through
    // [`Node::from_expr`] rather than by naming this field, and that
    // constructor's doc comment carries the whole route.
    _marker: PhantomData<(N, C)>,
}

impl<N: 'static + Clone, C: Context, E: Expr<N, C>> Node<N, C, E> {
    /// Turns this node into a step that stores its value in the
    /// [`PlanCache`](crate::plan::PlanCache) under `key` — the method form of
    /// [`save_cache!`](crate::save_cache!).
    ///
    /// The key is `&'static str`, so it must be a literal, a `const`, or
    /// otherwise outlive the whole program. A key built at run time will not
    /// compile here — `E0597`, the borrow does not live long enough — even though
    /// neighbouring four-argument [`save!`](crate::save!) and
    /// [`load_cache!`](crate::load_cache!) both accept an owned key:
    ///
    /// ```compile_fail
    /// # use express_it::prelude::*;
    /// # struct State { v: f64 }
    /// # struct Ctx;
    /// # impl Context for Ctx { type ContextItem<'w, 's> = State; }
    /// let key = String::from("per-run-slot");
    /// let step = Node::<f64, Ctx, _>::lit(1.0).alias(key.as_str());
    /// # let _ = step;
    /// ```
    ///
    /// That asymmetry is a known wart of this API, not a design claim; see the
    /// 0.3.0 release notes. Until it is closed, pass a literal, or leak the
    /// string with `Box::leak` if the key really is computed and the program
    /// lives as long as the plan.
    pub fn alias(self, key: &'static str) -> CacheSaveStep<N, C, E> {
        CacheSaveStep { key, expr: self }
    }
}

impl<N, C, E> Node<N, C, E> {
    /// Builds a node from a leaf supplied by the caller.
    ///
    /// `E` is the node's own shape, so a leaf of your own becomes a node here.
    /// The reason to write one is its dependency key: a leaf reports a key of its
    /// own, which a variable built through [`Node::new`] cannot (it reports the key
    /// of `VarExpr<N, C>`). For per-attribute identity without writing a leaf, use
    /// [`Node::var`], which keys a variable by name; [`var_leaf!`](crate::var_leaf)
    /// generates a type-keyed leaf for one attribute.
    ///
    /// This is the only way to build a node from a leaf you wrote. `Node`'s
    /// `_marker` field is private, so the `Node { expr, _marker: PhantomData }`
    /// literal that 0.2 accepted no longer compiles — the migration is to write
    /// `Node::from_expr(leaf)` and drop the marker from the call. The built-in
    /// leaves keep their own constructors, so a leaf never has to be named by
    /// hand at all: [`Node::lit`] for a constant, [`Node::new`] for a variable.
    ///
    /// No bounds on `N`, `C`, or `E`: the constructor only stores the leaf, and
    /// every way of using the node — `eval`, the operators, `Display` — asks for
    /// the bound it needs itself. That is what makes this the constructor the
    /// crate can always reach: `impl Clone for Node` has no `C: Context` in
    /// scope, and builds through here.
    #[inline(always)]
    pub fn from_expr(expr: E) -> Self {
        Self {
            expr,
            _marker: PhantomData,
        }
    }
}

impl<N, C: Context> Node<N, C, LiteralNode<N>> {
    /// A node holding a constant.
    ///
    /// The value's own type fixes the tree's value type, so `Node::lit(1.0f64)`
    /// and `Node::lit(1.0f32)` are different trees. A primitive written directly
    /// as an operand (`health * 2.0`) is turned into this same leaf by the
    /// operator's own impl for primitives; the comparison and logic combinators
    /// reach the same leaf through
    /// [`AsExpression`](crate::expr::AsExpression).
    #[inline(always)]
    pub fn lit(value: N) -> Self {
        Self::from_expr(LiteralNode { value })
    }
}

impl<N, C, E: Clone> Clone for Node<N, C, E> {
    fn clone(&self) -> Self {
        // `from_expr` rather than a literal: this impl has no `C: Context` in
        // scope, and the constructor asks for no bounds.
        Self::from_expr(self.expr.clone())
    }
}

impl<N, C, E: Copy> Copy for Node<N, C, E> {}

impl<N: Num, C: Context, E: Expr<N, C>> Node<N, C, E> {
    /// Reinterprets the node's value as `To` with Rust's `as` conversion rules.
    ///
    /// The conversion is total: it never panics and never returns `None`. It is
    /// literally `value as To` (see [`AsCast`]), so NaN converts to `0`, floats
    /// outside the target integer range saturate to that type's min/max, and
    /// int-to-int casts wrap on two's complement (`-1i32` becomes `u32::MAX`).
    ///
    /// The `AsCast` bound covers the primitive numeric types, so a third-party
    /// `Num` type needs its own `AsCast` impl to be castable.
    pub fn cast<To: Num>(self) -> Node<To, C, CastNode<N, To, C, E>>
    where
        N: AsCast<To>,
    {
        // Qualified rather than `Self::` — `cast` returns a node of a different
        // `N` and `E` than the impl's `Self`.
        Node::<To, C, _>::from_expr(CastNode::new(self.expr))
    }
}

impl<N, C, E> Expr<N, C> for Node<N, C, E>
where
    C: Context,
    E: Expr<N, C>,
{
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        self.expr.eval(ctx)
    }

    #[inline(always)]
    fn get_dependencies(&self, deps: &mut Deps) {
        self.expr.get_dependencies(deps);
    }
}

impl<N, C: Context, E: Expr<N, C>> Node<N, C, E> {
    /// The dependency keys of every leaf below this node.
    ///
    /// The ordinary entry point: it builds the [`Deps`] sink, walks the tree
    /// through [`Expr::get_dependencies`], and hands back the keys — no sink to
    /// declare and no trait method to name.
    ///
    /// The answer is the same **may-read superset** documented on
    /// [`Expr::get_dependencies`], and iteration order is equally unspecified; see
    /// [`Deps`]. A tree of literals has no keys at all.
    ///
    /// ```
    /// use express_it::prelude::*;
    ///
    /// struct Stats {
    ///     hp: f32,
    /// }
    ///
    /// struct GameCtx;
    ///
    /// impl Context for GameCtx {
    ///     type ContextItem<'w, 's> = Stats;
    /// }
    ///
    /// var_leaf!(pub Max, f32, GameCtx, |_ctx| 100.0f32);
    ///
    /// let hp = Node::<f32, GameCtx, _>::var("hp", |ctx: &Stats| ctx.hp);
    /// let max = Node::<f32, GameCtx, _>::from_expr(Max);
    /// let ratio = (hp / max).clamp(0.0, 1.0);
    ///
    /// let deps = ratio.dependencies();
    /// assert_eq!(deps.len(), 2, "the named variable and the leaf");
    /// assert!(deps.contains_name("hp"));
    /// assert!(deps.contains_type_of::<Max>());
    /// assert!(Node::<f32, GameCtx, _>::lit(1.0).dependencies().is_empty());
    /// ```
    #[inline]
    pub fn dependencies(&self) -> Deps {
        let mut deps = Deps::new();
        self.expr.get_dependencies(&mut deps);
        deps
    }
}

impl<N: fmt::Display, C: Context, E: Expr<N, C> + fmt::Display> fmt::Display for Node<N, C, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

/// Evaluates `expr` and stores the result in the PlanCache under `key`.
/// Does NOT write to the context state — pure cache-only step.
pub struct CacheSaveStep<N: 'static, C: Context, E> {
    /// The cache slot to write. `&'static str`, so a literal or a leaked string:
    /// a key built at run time does not compile here, unlike the owned key the
    /// four-argument `save!` and `load_cache!` accept.
    pub key: &'static str,
    /// The expression evaluated and stored; the context is never written.
    pub expr: Node<N, C, E>,
}

// Hand-written, like the `Node` impls: `C` is a marker parameter here, so a
// derived impl would demand `C: Clone`/`C: Copy` and leave the plain unit-struct
// contexts used across the examples unable to clone their own steps.
impl<N: 'static, C: Context, E: Clone> Clone for CacheSaveStep<N, C, E> {
    fn clone(&self) -> Self {
        Self {
            key: self.key,
            expr: self.expr.clone(),
        }
    }
}

impl<N: 'static, C: Context, E: Copy> Copy for CacheSaveStep<N, C, E> {}

pub type Var<N, C> = Node<N, C, VarExpr<N, C>>;

/// The leaf behind a variable: how to read the value, and how to name the
/// dependency.
///
/// `key` is the whole difference between the two ways to build this node:
///
/// * [`Node::new`] leaves it `None`, so the variable reports the key of
///   `VarExpr<N, C>` — one key shared by every variable of one value type and one
///   context.
/// * [`Node::var`] fills it in, so the variable reports a
///   [`DepKey::Name`] and each attribute is its own key
///   with no leaf type written.
///
/// The fields are public because the struct is, and a `VarExpr` written by hand
/// can set them; [`VarExpr::new`] and [`VarExpr::with_key`] cover what a caller
/// normally wants.
pub struct VarExpr<N, C: Context> {
    pub fetch_fn: for<'w, 's> fn(&C::ContextItem<'w, 's>) -> N,
    /// The variable's own name, or `None` to be keyed by this leaf's type.
    ///
    /// Compared by exact string equality once it reaches a
    /// [`Deps`] set, so one attribute has to be spelled one way
    /// everywhere.
    pub key: Option<&'static str>,
}

impl<N, C: Context> VarExpr<N, C> {
    /// A variable keyed by this leaf's own type.
    ///
    /// This is what [`Node::new`] builds, and it means every unnamed variable of
    /// one value type and one context reports a single dependency key. Reach for
    /// [`VarExpr::with_key`] when two attributes have to be told apart.
    pub fn new(fetch_fn: for<'w, 's> fn(&C::ContextItem<'w, 's>) -> N) -> Self {
        Self {
            fetch_fn,
            key: None,
        }
    }

    /// A variable keyed by `name`, so two attributes of one type are two keys.
    ///
    /// The same three limits as [`Node::var`] apply: `name` must be `'static`,
    /// it is compared by exact string equality, and it is global to the crate
    /// rather than scoped to this value type or context.
    pub fn with_key(
        name: &'static str,
        fetch_fn: for<'w, 's> fn(&C::ContextItem<'w, 's>) -> N,
    ) -> Self {
        Self {
            fetch_fn,
            key: Some(name),
        }
    }
}

impl<N, C: Context> Clone for VarExpr<N, C> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N, C: Context> Node<N, C, VarExpr<N, C>> {
    /// Constructs a variable node pre-wrapped for math operations.
    ///
    /// The variable reports the key of `VarExpr<N, C>`, so **every variable built
    /// through `Node::new` with one value type and one context shares a single
    /// dependency key** — `hp` and `defense` are the same key to anything watching
    /// [`Node::dependencies`]. Use it when that is what you want: a tree that only
    /// ever needs "some `f32` of this context changed", with one leaf type per
    /// value type to invalidate. When sharing is wrong — when the two attributes
    /// have to be told apart — [`Node::var`] keys by name and `var_leaf!` keys by
    /// leaf type, and either gives per-attribute identity.
    #[inline(always)]
    pub fn new(fetch_fn: fn(&C::ContextItem<'_, '_>) -> N) -> Self {
        Self::from_expr(VarExpr {
            fetch_fn,
            key: None,
        })
    }

    /// Constructs a variable node whose dependency key is `key`, its name.
    ///
    /// This is per-attribute dependency identity without a leaf type: the key goes
    /// into the [`Deps`] set as
    /// [`DepKey::Name`], so two variables of one value
    /// type and one context are two keys as long as their names differ. [`Node::new`]
    /// cannot express that — its two variables of one value type collapse to one key
    /// — and it is exactly what a dependency-driven invalidation layer needs.
    ///
    /// Names are compared by exact string equality: `"hp"` and `"HP"` are two
    /// different keys. A name key never collides with a type key either, so a named
    /// variable and a [`var_leaf!`](crate::var_leaf) leaf sit in one `Deps` set
    /// side by side. The name is also what the leaf prints as, so a rendered tree
    /// says which variable is which — `var:hp` — while a [`Node::new`] variable has
    /// no name to print and shows a bare `var`.
    ///
    /// Three limits worth knowing before you pick names.
    ///
    /// * The name must be `'static`. A name that only exists at runtime — read
    ///   from config, deserialised, or produced by formatting — cannot become a
    ///   key; you can still *query* such a name against a set with
    ///   `deps.iter().any(|k| k.as_name() == Some(runtime.as_str()))`, but you
    ///   cannot insert one.
    /// * A name key says nothing about the value type or the context: `var("hp", ..)`
    ///   on an `f32` tree and on an `i64` tree of a different context produce the
    ///   same key. Names are one flat namespace per crate.
    /// * `""` is a legal name, and so every empty-named variable in the crate
    ///   shares one key. Give variables real attribute names.
    ///
    /// ```
    /// use express_it::prelude::*;
    ///
    /// struct Stats {
    ///     hp: f32,
    ///     defense: f32,
    /// }
    ///
    /// struct GameCtx;
    ///
    /// impl Context for GameCtx {
    ///     type ContextItem<'w, 's> = Stats;
    /// }
    ///
    /// // The same two attributes built with `Node::new`: one key between them,
    /// // because both report the key of `VarExpr<f32, GameCtx>`.
    /// let hp = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.hp);
    /// let defense = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.defense);
    /// assert_eq!(hp.dependencies(), defense.dependencies());
    /// assert_eq!((hp + defense).dependencies().len(), 1);
    ///
    /// // Named: two keys, each answerable by name.
    /// let hp = Node::<f32, GameCtx, _>::var("hp", |ctx: &Stats| ctx.hp);
    /// let defense = Node::<f32, GameCtx, _>::var("defense", |ctx: &Stats| ctx.defense);
    /// let deps = (hp + defense).dependencies();
    /// assert_eq!(deps.len(), 2);
    /// assert!(deps.contains_name("hp"));
    /// assert!(deps.contains_name("defense"));
    /// assert!(!deps.contains_name("attack"), "a name it never reads");
    ///
    /// // Evaluation is untouched: only key collection changed.
    /// let stats = Stats { hp: 60.0, defense: 12.0 };
    /// assert_eq!((hp + defense).eval(&stats), 72.0);
    /// ```
    #[inline(always)]
    pub fn var(key: &'static str, fetch_fn: fn(&C::ContextItem<'_, '_>) -> N) -> Self {
        Self::from_expr(VarExpr {
            fetch_fn,
            key: Some(key),
        })
    }
}

impl<N: 'static, C: Context + 'static> Expr<N, C> for VarExpr<N, C> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        (self.fetch_fn)(ctx)
    }

    /// Reports this variable's key: its own name when it has one, and otherwise
    /// the identity of `VarExpr<N, C>`.
    ///
    /// So a [`Node::var`] variable is a key of its own, while every [`Node::new`]
    /// variable of one value type and one context shares a single key — the
    /// documented behaviour of that constructor, and what
    /// [`var_leaf!`](crate::var_leaf) exists to get around when a type is preferred
    /// over a name.
    fn get_dependencies(&self, deps: &mut Deps) {
        let key = match self.key {
            Some(name) => DepKey::name(name),
            None => DepKey::type_of::<Self>(),
        };
        deps.insert(key);
    }
}

impl<N, C: Context> Copy for VarExpr<N, C> {}

impl<N: fmt::Display, C: Context> fmt::Display for VarExpr<N, C> {
    /// A named variable prints `var:<name>`, the same rendering [`DepKey`] gives a
    /// name key; an anonymous [`Node::new`] variable has no name and prints `var`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.key {
            Some(key) => write!(f, "var:{key}"),
            None => write!(f, "var"),
        }
    }
}

/// Declares a zero-sized leaf for one attribute: a type implementing
/// [`Expr`](crate::expr::Expr) whose dependency key is its own type
/// ([`DepKey::Type`]) and whose
/// [`Display`](std::fmt::Display) name is its own.
///
/// Read as: make a leaf called `$name`, visible as `$vis`, whose value is a
/// `$value`, read out of `$ctx`, by running the body of `|$bind| ..` against the
/// context item. Hand the leaf to [`Node::from_expr`] to get a node.
///
/// **A name instead of a type:** [`Node::var`] keys a variable by
/// [`DepKey::Name`] and writes no leaf at all, so
/// per-attribute dependency identity does not require this macro. Use `Node::var`
/// when all you need is for two attributes to be two keys; use this macro when the
/// leaf type itself is worth having, because it also buys a `Display` name for the
/// tree and a fetch compiled into `eval`. The two schemes coexist: one `Deps` set
/// holds a name key and a type key side by side.
///
/// Why a type per attribute rather than a closure: a variable built with
/// [`Node::new`] reports the key of `VarExpr<N, C>`, so every variable of one
/// value type and one context shares a single key and cannot be told apart. What
/// [`Expr::get_dependencies`](crate::expr::Expr::get_dependencies) needs is a key
/// that distinguishes the leaf, and a leaf can get one either way: a per-attribute
/// type reports `DepKey::type_of::<Self>()`, and a single generic leaf parameterized
/// per attribute reports the key of its type parameter instead — the shape this
/// crate's own test leaf `dep_probe::Probe<A>` uses, and the one
/// `benches/float.rs` writes by hand as `Leaf<K>`. The macro chooses the first
/// shape because it costs the caller nothing: one call produces the item, the
/// `Expr` impl, and the `Display` name, where writing them out is three items.
/// That is the whole advantage. A hand-written leaf of either shape behaves
/// identically once written, carries the same keys, and goes into
/// [`Node::from_expr`] the same way.
///
/// # Attributes on the generated item
///
/// A slot for attributes sits ahead of the visibility and the name, inside the
/// parentheses, and what is written there is attached to the generated struct.
/// That is how a crate under `#![deny(missing_docs)]` documents an item it never
/// wrote by hand — without an attribute such a crate fails on the generated
/// struct with `missing documentation for a struct`:
///
/// ```
/// # use express_it::prelude::*;
/// # struct Stats { hp: f32 }
/// # struct GameCtx;
/// # impl Context for GameCtx { type ContextItem<'w, 's> = Stats; }
/// var_leaf!(
///     #[doc = "The target's current hit points."]
///     pub Health,
///     f32,
///     GameCtx,
///     |ctx| ctx.hp
/// );
///
/// let node = Node::<f32, GameCtx, _>::from_expr(Health);
/// assert_eq!(node.eval(&Stats { hp: 61.0 }), 61.0);
/// ```
///
/// The attributes have to be inside the parentheses, and they come first — ahead
/// of the visibility. Written in front of the whole call —
/// `#[doc = ".."] var_leaf!(..);` — they belong to the macro invocation and not to
/// what it expands to, so rustc warns `unused doc comment` and `missing_docs`
/// still fires. The `///` sugar only exists in item position, so the unsugared
/// `#[doc = ".."]` is what goes in the slot; `#[allow(..)]` and friends go there
/// the same way.
///
/// # Visibility is part of the call
///
/// `var_leaf!` takes a visibility. As of 0.3 every call writes one between the
/// attributes and the name, and the generated struct carries exactly what is
/// written there — the macro inserts none of its own. `pub` for an outward-facing
/// leaf, `pub(crate)` for one the crate keeps to itself, `pub(super)` for the
/// parent module. An empty visibility, written as no token at all, makes the leaf
/// private to the module the call sits in.
///
/// ```
/// # use express_it::prelude::*;
/// # struct Stats { hp: f32 }
/// # struct GameCtx;
/// # impl Context for GameCtx { type ContextItem<'w, 's> = Stats; }
/// var_leaf!(pub Health, f32, GameCtx, |ctx| ctx.hp);
/// var_leaf!(pub(crate) Regen, f32, GameCtx, |ctx| ctx.hp / 2.0);
/// var_leaf!(Shadow, f32, GameCtx, |ctx| ctx.hp * 2.0);
///
/// let stats = Stats { hp: 3.0 };
/// assert_eq!(Node::<f32, GameCtx, _>::from_expr(Health).eval(&stats), 3.0);
/// assert_eq!(Node::<f32, GameCtx, _>::from_expr(Regen).eval(&stats), 1.5);
/// assert_eq!(Node::<f32, GameCtx, _>::from_expr(Shadow).eval(&stats), 6.0);
/// ```
///
/// The visibility is written by the caller and nothing is inserted on their
/// behalf: `var_leaf!(pub Health, ..)` generates a public leaf, `pub(crate)` a
/// crate-private one, and a call with no token a leaf private to the module it
/// sits in — which is what a private helper or an integration test wants. Using
/// such a leaf from outside its own module fails with `E0603` (*struct `Health`
/// is private*), exactly as for any other private item. Defaulting to private
/// follows `bitflags!` and `thread_local!`, which behave the same way.
///
/// The macro is new in 0.3.0 and has never inserted a visibility of its own, so
/// there is no earlier call shape to migrate from.
///
/// # The generated fetch, and what it cannot reach
///
/// The fetch body is compiled directly into `eval`, not stored as a closure or a
/// function pointer. The reason is the context item: it is a generic associated
/// type (`C::ContextItem<'w, 's>`), and a `move |ctx| ..` closure cannot be
/// higher-ranked over those two lifetimes, so a stored closure would not
/// typecheck. That describes the shape of the generated code, not a reason to
/// prefer the macro — a hand-written leaf puts its fetch in `eval` for the very
/// same reason. `$bind` is bound to a reference to the incoming context item
/// (`&ContextItem`), so `|ctx| ctx.hp` reads exactly as it reads here. A body that
/// ignores the binding should name it with a leading underscore (`|_ctx| ..`), as
/// with any unused binding.
///
/// Because the body is compiled into a generated item rather than run where it is
/// written, it can reach neither of these (both verified against this crate):
///
/// * The type parameters of a generic item the call sits in. Invoking inside
///   `fn make<T: Default>() { .. }` with `T` as the value type and `T::default()`
///   as the body reports `E0401`, *can't use generic parameters from outer item*
///   — a nested item is independent of its parent.
/// * A local of the invocation scope. `let base = 1.0f32;` and then a body of
///   `|_ctx| base` reports `E0434`, *can't capture dynamic environment in a fn
///   item*.
///
/// Invoke at module level, and let the leaf read what it needs out of the context.
///
/// `$value` has to be owned, or name its lifetimes. A bare `&str` is rejected:
/// lifetime elision hands the `&` in the generated `eval` the `&self` borrow, so
/// the body cannot borrow from the context parameter, and the call reports "impl
/// item signature doesn't match trait item signature" together with "lifetime may
/// not live long enough" — not `E0106`, which the elision sidesteps. A type whose
/// lifetimes are spelled out, such as `&'static str`, is accepted, but then the
/// body has to supply a value of that lifetime: a `&ContextItem` borrow cannot do
/// that, so take a `&'static str` from a `const` or `static` and clone what comes
/// out of the context.
///
/// ```
/// use express_it::prelude::*;
///
/// struct Stats {
///     hp: f32,
///     attack: f32,
/// }
///
/// struct GameCtx;
///
/// impl Context for GameCtx {
///     type ContextItem<'w, 's> = Stats;
/// }
///
/// var_leaf!(pub Health, f32, GameCtx, |ctx| ctx.hp);
/// var_leaf!(pub Attack, f32, GameCtx, |ctx| ctx.attack);
///
/// let health = Node::<f32, GameCtx, _>::from_expr(Health);
/// let attack = Node::<f32, GameCtx, _>::from_expr(Attack);
/// let stats = Stats { hp: 60.0, attack: 24.0 };
///
/// assert_eq!(health.eval(&stats), 60.0);
/// assert_eq!((health + attack).eval(&stats), 84.0);
/// assert_eq!(health.to_string(), "Health", "and the leaf prints its own name");
///
/// // The point of the macro: two attributes, two type keys.
/// assert_ne!(health.dependencies(), attack.dependencies());
/// assert!(health.dependencies().contains_type_of::<Health>());
///
/// // And a tree of the two reports both, not one merged key.
/// assert_eq!((health + attack).dependencies().len(), 2);
///
/// // The same pair built through `Node::new` collapses to one key.
/// let hp_var = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.hp);
/// let atk_var = Node::<f32, GameCtx, _>::new(|ctx: &Stats| ctx.attack);
/// assert_eq!(hp_var.dependencies(), atk_var.dependencies());
/// assert_eq!(hp_var.dependencies().len(), 1);
///
/// // `Node::var` separates them again with no leaf type, by name instead — and a
/// // name key is not a type key, so the two schemes never overlap.
/// let hp_named = Node::<f32, GameCtx, _>::var("hp", |ctx: &Stats| ctx.hp);
/// let atk_named = Node::<f32, GameCtx, _>::var("attack", |ctx: &Stats| ctx.attack);
/// let named = (hp_named + atk_named).dependencies();
/// assert_eq!(named.len(), 2);
/// assert!(named.contains_name("hp"));
/// assert!(named.contains_name("attack"));
/// assert!(!named.contains_type_of::<Health>());
/// ```
#[macro_export]
macro_rules! var_leaf {
    ($(#[$meta:meta])* $vis:vis $name:ident, $value:ty, $ctx:ty, |$bind:ident| $body:expr) => {
        $(#[$meta])*
        #[derive(Clone, Copy)]
        $vis struct $name;

        impl $crate::expr::Expr<$value, $ctx> for $name {
            // The body goes here rather than into a stored closure: the context
            // item is a GAT, so the fetch cannot be a `for<'w, 's> fn(..)` value.
            #[inline(always)]
            fn eval(&self, ctx: &<$ctx as $crate::expr::Context>::ContextItem<'_, '_>) -> $value {
                let $bind = ctx;
                $body
            }

            fn get_dependencies(&self, deps: &mut $crate::expr::Deps) {
                deps.insert($crate::expr::DepKey::type_of::<Self>());
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                write!(f, stringify!($name))
            }
        }
    };
}

impl<From: Num, To: Num, C: Context, E: Expr<From, C>> fmt::Display for CastNode<From, To, C, E>
where
    From: fmt::Display,
    E: fmt::Display,
{
    // `type_name` needs no bound on `To` (it is `fn type_name<T: ?Sized>()`), so
    // the target type is printed without widening this impl.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cast::<{}>({})", type_name::<To>(), self.inner)
    }
}

#[derive(Clone, Copy)]
pub struct LiteralNode<N> {
    pub value: N,
}

impl<N: Copy, C: Context> Expr<N, C> for LiteralNode<N> {
    #[inline(always)]
    fn eval(&self, _ctx: &C::ContextItem<'_, '_>) -> N {
        self.value
    }
}

impl<N: fmt::Display> fmt::Display for LiteralNode<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// Numeric conversion that reproduces Rust's `as` operator for every primitive
/// pair, so `Node::cast` can be total instead of panicking.
///
/// Each result is what `value as To` yields:
///
/// * float → float: only precision changes; NaN stays NaN, infinities stay infinite.
/// * float → int: the fractional part is dropped toward zero, `NaN` becomes `0`,
///   and values beyond the target range saturate to that type's min/max
///   (`1e30f32 as i32 == i32::MAX`, `-1.0f32 as u32 == 0`).
/// * int → int: in-range values are unchanged; otherwise the bits are truncated,
///   i.e. two's-complement wrapping (`-1i32 as u32 == u32::MAX`,
///   `i64::MAX as i8 == -1`).
/// * int → float: rounded to the nearest representable value.
///
/// Implemented for the primitive numeric types only (`f32`, `f64`, the signed and
/// unsigned integers): implement it yourself for your own `Num` type to cast to or
/// from it.
pub trait AsCast<To> {
    /// Converts `self` to `To` with `as` semantics. Total: never panics.
    ///
    /// Takes `self` by value because that is what `as` does, and every implementor
    /// is a `Copy` primitive, so there is nothing worth borrowing. Same reasoning
    /// (and the same allow) as [`crate::expr::AsExpression::as_expr`].
    #[allow(clippy::wrong_self_convention)]
    fn as_cast(self) -> To;
}

macro_rules! impl_as_cast {
    ($from:ty, [$($to:ty),+]) => {
        $(
            impl AsCast<$to> for $from {
                #[inline(always)]
                fn as_cast(self) -> $to {
                    self as $to
                }
            }
        )+
    };
}

/// `as` is defined for every primitive pair, so the matrix covers every
/// `Num`-compatible source/target combination the crate can build a cast for.
macro_rules! impl_as_cast_matrix {
    ($($from:ty),+ $(,)?) => {
        $(
            impl_as_cast!(
                $from,
                [f32, f64, i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize]
            );
        )+
    };
}

impl_as_cast_matrix!(
    f32, f64, // Floats
    i8, i16, i32, i64, i128, isize, // Signed Ints
    u8, u16, u32, u64, u128, usize // Unsigned Ints
);

/// Converts the inner expression's value from `From` to `To` at evaluation time.
///
/// The conversion is [`AsCast::as_cast`], i.e. Rust's `as` operator: total, never
/// panicking, saturating for float→int (NaN becomes `0`), wrapping for int→int.
pub struct CastNode<From: Num, To: Num, C: Context, E: Expr<From, C>> {
    inner: E,
    _marker: PhantomData<(From, To, C)>,
}

impl<From: Num, To: Num, C: Context, T: Expr<From, C>> CastNode<From, To, C, T> {
    /// Wraps `inner`, converting its value from `From` to `To` at evaluation
    /// time through [`AsCast::as_cast`].
    ///
    /// `Node::cast` is the usual route; this is the same node for a caller
    /// assembling a tree by hand.
    pub fn new(inner: T) -> Self {
        Self {
            inner,
            _marker: PhantomData,
        }
    }
}

impl<From: Num, To: Num, C: Context, T: Expr<From, C> + Copy> Copy for CastNode<From, To, C, T> {}

impl<From, To, C: Context, T> Expr<To, C> for CastNode<From, To, C, T>
where
    From: Num + AsCast<To>,
    To: Num,
    T: Expr<From, C>,
{
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> To {
        self.inner.eval(ctx).as_cast()
    }

    fn get_dependencies(&self, deps: &mut Deps) {
        self.inner.get_dependencies(deps);
    }
}

impl<From: Num, To: Num, C: Context, E: Expr<From, C> + Clone> Clone for CastNode<From, To, C, E> {
    fn clone(&self) -> Self {
        CastNode::new(self.inner.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{Context, Expr};

    pub struct Ctx;

    impl Context for Ctx {
        type ContextItem<'w, 's> = ();
    }

    #[test]
    fn test_literal_node_is_copy() {
        // Assigning `a` to `b` and then using `a` again is the Copy check: a move
        // would not compile. Both halves are then wrapped in a node and evaluated,
        // so what was copied is a working node rather than a value that merely
        // exists.
        fn assert_copy<T: Copy>() {}

        let a = LiteralNode { value: 1i32 };
        assert_copy::<LiteralNode<i32>>();
        let b = a;

        let left: Node<i32, Ctx, _> = Node::from_expr(a);
        let right: Node<i32, Ctx, _> = Node::from_expr(b);
        assert_eq!(left.eval(&()), 1);
        assert_eq!(right.eval(&()), 1);
        assert_eq!(left.eval(&()), right.eval(&()));
        assert_eq!(left.to_string(), right.to_string());
    }

    #[test]
    fn test_variable_node_is_copy() {
        fn assert_copy<T: Copy>() {}

        let a: Node<i32, Ctx, _> = Node::new(|()| 4i32);
        assert_copy::<VarExpr<i32, Ctx>>();
        assert_copy::<Node<i32, Ctx, VarExpr<i32, Ctx>>>();
        let b = a;

        assert_eq!(a.eval(&()), 4);
        assert_eq!(b.eval(&()), 4);
        // The copy is the same variable: same value, same key, same rendering.
        assert_eq!(a.eval(&()), b.eval(&()));
        assert_eq!(a.dependencies(), b.dependencies());
        assert_eq!(a.to_string(), b.to_string());
    }

    #[test]
    fn test_cast_nan_f64_to_f32_preserves_nan() {
        let inner = LiteralNode { value: f64::NAN };
        let node = CastNode::<f64, f32, Ctx, _>::new(inner);
        assert!(node.eval(&()).is_nan());
    }

    #[test]
    fn test_cast_node_is_copy() {
        fn assert_copy<T: Copy>() {}

        let inner = LiteralNode { value: 3i32 };
        let node = CastNode::<i32, f32, Ctx, _>::new(inner);
        assert_copy::<CastNode<i32, f32, Ctx, LiteralNode<i32>>>();
        let copied = node;

        assert_eq!(node.eval(&()), 3.0f32);
        assert_eq!(copied.eval(&()), 3.0f32);
        // Bits, so the comparison holds for a cast whose result is NaN.
        assert_eq!(node.eval(&()).to_bits(), copied.eval(&()).to_bits());
        assert_eq!(node.to_string(), copied.to_string());
    }

    #[test]
    fn cast_display_names_the_target_type() {
        // `.cast::<i32>()` and `.cast::<u8>()` must not print the same string.
        let as_i32 = Node::<f32, Ctx, _>::lit(3.7f32).cast::<i32>();
        let as_u8 = Node::<f32, Ctx, _>::lit(3.7f32).cast::<u8>();

        // Built from `type_name` rather than hardcoded, because the exact
        // rendering of a type name is not specified by Rust; what has to hold is
        // that the target type appears and that two targets print differently.
        let want_i32 = format!("cast::<{}>(3.7)", std::any::type_name::<i32>());
        let want_u8 = format!("cast::<{}>(3.7)", std::any::type_name::<u8>());

        assert_eq!(as_i32.to_string(), want_i32);
        assert_eq!(as_u8.to_string(), want_u8);
        assert_ne!(as_i32.to_string(), as_u8.to_string());
    }

    #[test]
    fn cache_save_step_clones_without_a_clone_context() {
        // `Ctx` is a plain unit struct: a derived impl would have demanded
        // `C: Clone`, which it does not satisfy, so this would not compile.
        // Generic so that clippy's `clone_on_copy` does not fire on a payload that
        // happens to be `Copy`.
        fn cloned<T: Clone>(value: T) -> T {
            value.clone()
        }

        let step = Node::<i32, Ctx, _>::lit(7).alias("seven");
        let _cloned = cloned(step);
    }

    #[test]
    fn test_literal_eval_via_expr_trait() {
        let lit = LiteralNode { value: 7i32 };
        assert_eq!(Expr::<i32, Ctx>::eval(&lit, &()), 7);
    }

    #[test]
    fn test_variable_node_from_fn() {
        let node: Node<i32, Ctx, _> = Node::new(|()| 4i32);
        assert_eq!(node.eval(&()), 4);
    }

    #[test]
    fn test_cast_node_int_to_float() {
        let inner = LiteralNode { value: 3i32 };
        let node = CastNode::<i32, f32, Ctx, _>::new(inner);
        assert_eq!(node.eval(&()), 3.0f32);
    }

    #[test]
    fn test_cast_node_float_to_int_truncates() {
        let inner = LiteralNode { value: 3.7f32 };
        let node = CastNode::<f32, i32, Ctx, _>::new(inner);
        assert_eq!(node.eval(&()), 3i32);
    }

    #[test]
    fn test_cast_out_of_range_int_wraps_like_as() {
        // `i64::MAX as i8` keeps the low byte: -1. The cast is total, so the
        // out-of-range value that used to panic now returns like `as` does.
        let inner = LiteralNode { value: i64::MAX };
        let node = CastNode::<i64, i8, Ctx, _>::new(inner);
        assert_eq!(node.eval(&()), -1i8);
    }

    #[test]
    fn test_cast_negative_int_to_unsigned_wraps_like_as() {
        let inner = LiteralNode { value: -1i32 };
        let node = CastNode::<i32, u32, Ctx, _>::new(inner);
        assert_eq!(node.eval(&()), u32::MAX);
    }

    #[test]
    fn node_wrapper_forwards_dependencies_to_inner_expr() {
        use crate::dep_probe::{Attack, Defense, Haste, attr, key, keys, set};

        assert_eq!(
            keys(&(attr::<Attack>() + attr::<Defense>())),
            set([key::<Attack>(), key::<Defense>()])
        );
        assert_eq!(keys(&attr::<Haste>()), set([key::<Haste>()]));
    }

    #[test]
    fn vars_built_through_node_new_share_one_key() {
        // Documented limitation: a VarExpr with no name reports the identity of
        // VarExpr<N, C>, not of the variable, so two different variables of one
        // value type and one context collapse to a single key. Pinned so that
        // giving variables their own key has to change this test on purpose — and
        // `Node::var` is the key it was given instead, which leaves this
        // behaviour of `Node::new` exactly as it was.
        let attack: Node<f32, Ctx, _> = Node::new(|()| 10.0f32);
        let defense: Node<f32, Ctx, _> = Node::new(|()| 2.0f32);

        let mut deps = Deps::new();
        attack.get_dependencies(&mut deps);
        defense.get_dependencies(&mut deps);

        assert_eq!(deps.len(), 1, "both variables report the same VarExpr key");
        // The count alone would also pass if the shared key became something
        // else, so pin which key it is.
        assert!(
            deps.contains_type_of::<VarExpr<f32, Ctx>>(),
            "and the one key is the VarExpr type, not something else"
        );
    }

    #[test]
    fn named_variables_of_one_value_type_get_their_own_keys() {
        // The case `Node::new` cannot express, and the point of `Node::var`: same
        // value type, same context, two attributes, two keys.
        let hp: Node<f32, Ctx, _> = Node::var("hp", |()| 60.0f32);
        let defense: Node<f32, Ctx, _> = Node::var("defense", |()| 12.0f32);

        assert_ne!(
            hp.dependencies(),
            defense.dependencies(),
            "two names are two keys"
        );
        assert_eq!(hp.dependencies().len(), 1);
        assert!(hp.dependencies().contains_name("hp"));
        assert!(defense.dependencies().contains_name("defense"));

        let both = (hp + defense).dependencies();
        assert_eq!(both.len(), 2, "and the composite reports both");
        assert!(both.contains_name("hp") && both.contains_name("defense"));

        // Exact string equality, so one attribute spelled two ways is two keys.
        assert_ne!(DepKey::name("hp"), DepKey::name("HP"));

        // Evaluation is untouched by any of this.
        assert_eq!((hp + defense).eval(&()), 72.0);
    }

    #[test]
    fn a_named_variable_and_a_var_leaf_leaf_report_two_keys_of_two_kinds() {
        // The two keying schemes coexist in one set: one `Name` key, one `Type`
        // key, and neither is mistaken for the other.
        let hp = Node::<f32, Ctx, _>::var("hp", |()| 60.0f32);
        let attack = Node::<f32, Ctx, _>::from_expr(ProbeAttack);
        let tree = hp + attack;

        let deps = tree.dependencies();
        assert_eq!(deps.len(), 2, "exactly two keys");
        assert!(deps.contains_name("hp"), "the named variable");
        assert!(deps.contains_type_of::<ProbeAttack>(), "the leaf type");
        assert!(!deps.contains_name("ProbeAttack"));
        assert!(!deps.contains_type_of::<VarExpr<f32, Ctx>>());

        let kinds: Vec<Option<&'static str>> = deps.iter().map(DepKey::as_name).collect();
        assert_eq!(kinds.iter().filter(|n| n.is_some()).count(), 1);
        assert_eq!(kinds.iter().filter(|n| n.is_none()).count(), 1);
        assert_eq!(tree.eval(&()), 70.0);
    }

    #[test]
    fn a_composite_is_a_superset_of_each_operand_and_of_nothing_more() {
        let hp = Node::<f32, Ctx, _>::var("hp", |()| 60.0f32);
        let attack = Node::<f32, Ctx, _>::from_expr(ProbeAttack);
        let tree = hp + attack;

        let composite = tree.dependencies();
        assert!(composite.is_superset_of(&hp.dependencies()));
        assert!(
            composite.is_superset_of(&attack.dependencies()),
            "the composite covers each operand's key set"
        );
        assert!(composite.is_superset_of(&composite));
        assert!(
            !hp.dependencies().is_superset_of(&attack.dependencies()),
            "and one operand does not cover the other"
        );

        let unrelated: Deps = [DepKey::name("speed")].into_iter().collect();
        assert!(
            !composite.is_superset_of(&unrelated),
            "a key the tree never reports breaks the superset"
        );
        assert!(
            composite.is_superset_of(&Deps::new()),
            "every set is a superset of the empty set"
        );
    }

    #[test]
    fn contains_name_and_contains_type_of_answer_for_the_right_kind() {
        let hp = Node::<f32, Ctx, _>::var("hp", |()| 60.0f32);
        let attack = Node::<f32, Ctx, _>::from_expr(ProbeAttack);
        let deps = (hp + attack).dependencies();

        assert!(deps.contains_name("hp"), "finds the name it holds");
        assert!(deps.contains_type_of::<ProbeAttack>(), "finds the type");
        assert!(
            !deps.contains_name("attack"),
            "rejects a name it does not hold"
        );
        assert!(
            !deps.contains_type_of::<ProbeDefense>(),
            "rejects a type it does not hold"
        );
        assert!(
            !deps.contains_type_of::<VarExpr<f32, Ctx>>(),
            "a named variable reports no type key at all"
        );
        assert!(
            !deps.contains_name("ProbeAttack"),
            "a type key is never found by name"
        );
        assert!(
            deps.contains(DepKey::name("hp")),
            "the same checks spelled with an explicit key"
        );
    }

    #[test]
    fn a_tree_of_literals_has_no_dependencies() {
        // Nothing reachable reports a key, so the default `get_dependencies` body
        // is what runs all the way down.
        let tree = (Node::<f32, Ctx, _>::lit(2.0) + Node::<f32, Ctx, _>::lit(3.0)).cast::<i32>();

        assert_eq!(tree.eval(&()), 5);
        assert!(tree.dependencies().is_empty());
        assert_eq!(tree.dependencies(), Deps::new());
    }

    /// A leaf written by hand, the way a consumer would write one: the shape
    /// `from_expr` takes.
    #[derive(Clone, Copy)]
    struct Hp;

    impl Expr<f32, Ctx> for Hp {
        fn eval(&self, _ctx: &()) -> f32 {
            7.5
        }

        fn get_dependencies(&self, deps: &mut Deps) {
            deps.insert(DepKey::type_of::<Self>());
        }
    }

    impl fmt::Display for Hp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Hp")
        }
    }

    #[test]
    fn from_expr_builds_a_node_that_evaluates_composes_and_aliases() {
        let hp = Node::<f32, Ctx, _>::from_expr(Hp);

        assert_eq!(hp.eval(&()), 7.5, "the node evaluates through the leaf");
        assert_eq!(hp.to_string(), "Hp", "and prints through it");

        // Operators: `Node` operand on both sides, and a bare literal.
        let doubled = (hp + hp) * 2.0;
        assert_eq!(doubled.eval(&()), 30.0);

        // `.alias(..)`: the cache-save step carries the same leaf, so the tree it
        // saves is the tree that was built.
        let step = hp.alias("hp");
        assert_eq!(step.key, "hp");
        assert_eq!(step.expr.eval(&()), 7.5);
    }

    // The same two leaves through the macro, so the generated impl is exercised
    // in-crate as well as from the integration tests. No visibility modifier: each
    // leaf is named only by the test below, in this same module.
    var_leaf!(ProbeAttack, f32, Ctx, |_ctx| 10.0f32);
    var_leaf!(ProbeDefense, f32, Ctx, |_ctx| 2.0f32);

    #[test]
    fn var_leaf_leaves_have_their_own_key_while_node_new_variables_share_one() {
        // The contrast the macro exists for.
        let attack = Node::<f32, Ctx, _>::from_expr(ProbeAttack);
        let defense = Node::<f32, Ctx, _>::from_expr(ProbeDefense);

        assert_eq!(attack.eval(&()), 10.0);
        assert_eq!(defense.eval(&()), 2.0);
        assert_eq!(
            (attack + defense).to_string(),
            "(ProbeAttack + ProbeDefense)",
            "Display comes from the leaf, so a tree of leaves reads by attribute"
        );

        let mut leaf_keys = Deps::new();
        attack.get_dependencies(&mut leaf_keys);
        defense.get_dependencies(&mut leaf_keys);
        assert_eq!(leaf_keys.len(), 2, "one key per leaf type");
        assert!(leaf_keys.contains_type_of::<ProbeAttack>());
        assert!(leaf_keys.contains_type_of::<ProbeDefense>());

        // The claim the macro exists for, asserted on the composite itself rather
        // than on two merged single-leaf sets: a tree of two macro leaves reports
        // two dependency keys.
        let mut composite_keys = Deps::new();
        (attack + defense).get_dependencies(&mut composite_keys);
        assert_eq!(
            composite_keys.len(),
            2,
            "a composite of two var_leaf! attributes reports two keys"
        );
        assert_eq!(
            composite_keys, leaf_keys,
            "and they are the very keys the two leaves report alone"
        );

        let mut var_keys = Deps::new();
        Node::<f32, Ctx, _>::new(|()| 10.0f32).get_dependencies(&mut var_keys);
        Node::<f32, Ctx, _>::new(|()| 2.0f32).get_dependencies(&mut var_keys);
        assert_eq!(
            var_keys.len(),
            1,
            "the pinned VarExpr behaviour is untouched by the macro"
        );
    }

    #[test]
    fn a_named_variable_prints_its_name_and_an_anonymous_one_prints_var() {
        let named = Node::<f32, Ctx, _>::var("hp", |()| 30.0f32);
        let anonymous = Node::<f32, Ctx, _>::new(|()| 30.0f32);

        assert_eq!(named.to_string(), "var:hp");
        assert_eq!(anonymous.to_string(), "var");
        assert_eq!(
            (named * 2.0).to_string(),
            "(var:hp * 2)",
            "the name reaches through the operator nodes, which is the whole point"
        );
    }
}
