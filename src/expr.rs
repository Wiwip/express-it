use crate::nodes::{LiteralNode, Node, VarExpr};
use std::any::TypeId;
use std::collections::HashSet;
use std::fmt;

pub trait Context: Send + Sync {
    type ContextItem<'w, 's>;
}

pub trait ContextMut: Context {
    type ContextItemMut<'w, 's>;

    fn as_readonly<'w, 's, 'a>(
        mut_item: &'a Self::ContextItemMut<'w, 's>,
    ) -> Self::ContextItem<'a, 's>;
}

/// What a tree evaluates, and which leaves it may read.
///
/// `N` is the value type, `C` the context a variable reads from. Every node in
/// the crate implements it, and a leaf of your own implements it too — that is
/// the whole contract a custom leaf has to satisfy, and it is two methods:
/// `eval`, and [`get_dependencies`](Expr::get_dependencies) reporting the keys of
/// the leaves below this node into a [`Deps`] sink.
///
/// A leaf that wants its own key reports one; a node that merely holds other
/// expressions forwards the walk to its children. `var_leaf!` writes both halves
/// of a leaf, [`Node::var`](crate::nodes::Node::var) needs no leaf at all, and
/// neither is reachable from the other: the first gives a leaf its identity from
/// its type, the second from a name.
///
/// Two things that are not two methods, but bite anyway. Composing a node with
/// another needs `Copy` — the comparison and logical combinator impls require
/// `L: Expr<N, C> + Copy` — so a leaf that cannot be `Copy` can be held behind
/// [`Box`], whose `Expr` impl forwards, which keeps it a usable tree payload and
/// composable with the arithmetic operators; a box is never `Copy`, though, so it
/// still cannot take `.lt()` or `.and()` — see [`StoredExpr`]. And
/// `get_dependencies` has a **default body that adds nothing**: correct for a
/// literal, silently wrong for a composite, which then reports a strict *subset*
/// of what it reads. That is the one direction the may-read contract below does
/// not cover, and a watcher built from such a node is never told the tree is
/// stale.
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
/// // The shape a downstream implementor copies: a zero-sized leaf, one key.
/// #[derive(Clone, Copy)]
/// struct Hp;
///
/// impl Expr<f32, GameCtx> for Hp {
///     fn eval(&self, ctx: &Stats) -> f32 {
///         ctx.hp
///     }
///
///     fn get_dependencies(&self, deps: &mut Deps) {
///         deps.insert(DepKey::type_of::<Self>());
///     }
/// }
///
/// let hp = Node::<f32, GameCtx, _>::from_expr(Hp);
///
/// assert_eq!(hp.eval(&Stats { hp: 61.0 }), 61.0);
///
/// // `dependencies` is the caller-side accessor; `get_dependencies` above is the
/// // method a leaf implements.
/// let deps = hp.dependencies();
/// assert_eq!(deps.len(), 1);
/// assert!(deps.contains(DepKey::type_of::<Hp>()));
/// assert!(deps.contains_type_of::<Hp>());
/// assert!(!deps.contains_name("hp"), "a type key is not a name key");
/// ```
pub trait Expr<N, C: Context> {
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N;

    /// Collects the dependency keys of every leaf reachable from this node.
    ///
    /// The result is a **may-read superset**, not the set of attributes a given
    /// evaluation actually touches: the walk visits every child of every node,
    /// while `eval` may skip some of them. `&&` and `||` are the case that
    /// matters — `a && b` reports both operands even though a live `eval`
    /// short-circuits and never reads `b` once `a` is false. Arithmetic is exact
    /// about which *leaves the walk reaches*, by contrast: every arithmetic node
    /// (`+ - * / %`, `powf`, `min`, `max`, `abs`, `sqrt`, `acos`, `clamp(..)`, casts)
    /// evaluates all of its children. It is not exact about which *attributes*
    /// exist, because two variables built through
    /// [`Node::new`](crate::nodes::Node::new) of one value type report one shared
    /// key, so a set can name fewer attributes than the tree really reads.
    /// Watching a leaf that a particular evaluation skipped only costs a
    /// redundant re-evaluation, so the superset is a documented property rather
    /// than a defect, and callers get a conservative answer they can register
    /// edges from ahead of the first evaluation.
    ///
    /// Leaf identity is up to the leaf: the default implementation contributes
    /// nothing, which is correct for nodes that hold no variables (a literal)
    /// and silently wrong for a composite that forwards to nothing, which reports
    /// fewer keys than it reads.
    /// Order is unspecified — the sink is a hash set of [`DepKey`]s, so a caller
    /// that depends on a particular order must sort the collected keys.
    ///
    /// Callers ask for the keys of a node through
    /// [`Node::dependencies`](crate::nodes::Node::dependencies), which builds the
    /// sink and calls this; implementing it is for a leaf of your own, and the
    /// [`Expr`] doc shows that impl.
    fn get_dependencies(&self, _deps: &mut Deps) {}
}

/// One leaf's identity, as reported through [`Expr::get_dependencies`].
///
/// Two flavours, deliberately:
///
/// * [`DepKey::Type`] is the identity of a leaf *type*. [`var_leaf!`](crate::var_leaf)
///   reports it, and so does a hand-written leaf through [`DepKey::type_of`]. Two
///   leaves of one type are one key, which is what makes a type per attribute.
/// * [`DepKey::Name`] is a variable *name*, reported by
///   [`Node::var`](crate::nodes::Node::var). Two attributes of one value type and
///   one context get two keys from their names alone, with no leaf type written.
///
/// A `Name` key is compared by **exact string equality**: `"hp"` and `"HP"` are two
/// keys, and so are `"hp"` and `"hp "`. Nothing case-folds, trims, normalises, or
/// parses a name, so one attribute spelled two ways is two keys to a dependency
/// layer. A `Name` and a `Type` never collide either — they are different variants,
/// so a name and a leaf type share nothing.
///
/// A name is not scoped to a value type or a context: `"hp"` on an `f32` tree and
/// `"hp"` on an `i64` tree of some other context are the same key, because a name
/// key carries nothing but its string. One spelling therefore means one attribute
/// across the whole crate — and `""`, being a legal string, is a legal key that
/// every empty-named variable shares.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum DepKey {
    /// Identity of a leaf type: what `var_leaf!` and hand-written leaves report.
    Type(TypeId),
    /// A named variable, keyed by its name rather than by its type.
    Name(&'static str),
}

impl DepKey {
    /// The key of the leaf type `T`: the `Type` variant.
    ///
    /// Use it from a leaf's `get_dependencies` when the leaf's own type is its
    /// identity, as a `var_leaf!` leaf does.
    #[inline(always)]
    pub fn type_of<T: 'static>() -> Self {
        DepKey::Type(TypeId::of::<T>())
    }

    /// The key of the variable named `key`: the `Name` variant.
    ///
    /// Exact string equality decides whether two of these are the same key, so
    /// spell one attribute one way everywhere. The name has to be `'static`, so a
    /// name produced at runtime cannot be inserted as a key; and `""` is a legal
    /// name, shared by every empty-named key.
    #[inline(always)]
    pub fn name(key: &'static str) -> Self {
        DepKey::Name(key)
    }

    /// The `TypeId` inside a [`DepKey::Type`] key, `None` for a name key.
    #[inline(always)]
    pub fn as_type_id(self) -> Option<TypeId> {
        match self {
            DepKey::Type(id) => Some(id),
            DepKey::Name(_) => None,
        }
    }

    /// The name inside a [`DepKey::Name`] key, `None` for a type key.
    #[inline(always)]
    pub fn as_name(self) -> Option<&'static str> {
        match self {
            DepKey::Name(name) => Some(name),
            DepKey::Type(_) => None,
        }
    }
}

/// Renders the variant's prefix and what the variant actually holds: `var:<name>`
/// for a name, `type:<Debug of the TypeId>` for a type.
///
/// There is no nicer rendering for the type half to print. A `TypeId` carries no
/// name string — `Debug` is as close as it gets to naming the type, and on some
/// builds it is only a hash — so printing anything else would be inventing a type
/// name the runtime does not have.
impl fmt::Display for DepKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DepKey::Name(name) => write!(f, "var:{name}"),
            DepKey::Type(id) => write!(f, "type:{id:?}"),
        }
    }
}

/// The keys a tree reports, collected by [`Expr::get_dependencies`] and handed
/// back by [`Node::dependencies`](crate::nodes::Node::dependencies).
///
/// A set of [`DepKey`]s: a leaf reached by two paths through the tree contributes
/// one key, and a name key and a type key sit side by side in the same collection.
///
/// **Iteration order is unspecified**, and deliberately so — the backing store is
/// a hash set. `Deps` is `PartialEq` (two sinks holding the same keys are equal,
/// whatever order they went in) but is *not* `Ord` or `PartialOrd`: an ordering
/// would claim a sequence the hash set does not have. A caller that wants keys in
/// a stable order has to sort them itself — collect `DepKey::to_string` for each
/// key and sort those. Sorting the `Option<&str>` from [`DepKey::as_name`] orders
/// only the name keys, since every type key maps to `None`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Deps {
    keys: HashSet<DepKey>,
}

impl Deps {
    /// An empty sink.
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            keys: HashSet::new(),
        }
    }

    /// Adds `key`; `false` when it was already there.
    #[inline(always)]
    pub fn insert(&mut self, key: DepKey) -> bool {
        self.keys.insert(key)
    }

    /// Empties the sink, keeping its allocation.
    ///
    /// Exists so a caller can reuse one sink across many walks instead of
    /// allocating a set per walk.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.keys.clear();
    }

    /// Whether `key` is in the set.
    #[inline(always)]
    pub fn contains(&self, key: DepKey) -> bool {
        self.keys.contains(&key)
    }

    /// Whether the type key of `T` is in the set — the check for a leaf keyed by
    /// its type (`var_leaf!`, a hand-written leaf, a [`Node::new`] variable).
    ///
    /// A leaf reports whatever key it chooses, which for a generic leaf is often
    /// its type parameter rather than itself: `Probe<Attack>` reports `Attack`, so
    /// this answers for `Attack` and not for `Probe<Attack>`.
    #[inline(always)]
    pub fn contains_type_of<T: 'static>(&self) -> bool {
        self.keys.contains(&DepKey::type_of::<T>())
    }

    /// Whether the name key `name` is in the set — the check for a
    /// [`Node::var`] variable. Exact string equality, as on [`DepKey::name`].
    ///
    /// The `&'static str` follows from the key itself borrowing a `&'static str`.
    /// A name that only exists at runtime cannot become a key, but it can still
    /// be tested against a set:
    /// `deps.iter().any(|k| k.as_name() == Some(runtime.as_str()))`.
    #[inline(always)]
    pub fn contains_name(&self, name: &'static str) -> bool {
        self.keys.contains(&DepKey::name(name))
    }

    /// How many keys the set holds.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.keys.len()
    }

    /// Whether the set holds no key at all — true for a tree of literals.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// Iterates the keys, in unspecified order.
    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = DepKey> + '_ {
        self.keys.iter().copied()
    }

    /// Whether every key of `other` is also in `self`.
    ///
    /// The containment test a dependency layer wants: a composite's keys are a
    /// superset of each operand's, so "does this tree read at least what that
    /// tree reads?" is one call. The empty set is a subset of everything, so
    /// `is_superset_of` against an empty `other` is always true — and because
    /// unnamed `Node::new` variables share one key, a true answer can still be
    /// resting on a key that stands for two attributes.
    #[inline(always)]
    pub fn is_superset_of(&self, other: &Deps) -> bool {
        self.keys.is_superset(&other.keys)
    }

    /// Adds every key of `other` to `self` — the keys of two subtrees, say.
    #[inline(always)]
    pub fn union_with(&mut self, other: &Deps) {
        self.keys.extend(other.keys.iter().copied());
    }
}

impl FromIterator<DepKey> for Deps {
    #[inline]
    fn from_iter<I: IntoIterator<Item = DepKey>>(iter: I) -> Self {
        Self {
            keys: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Deps {
    type Item = DepKey;
    type IntoIter = std::collections::hash_set::IntoIter<DepKey>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.keys.into_iter()
    }
}

impl<'a> IntoIterator for &'a Deps {
    type Item = DepKey;
    type IntoIter = std::iter::Copied<std::collections::hash_set::Iter<'a, DepKey>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.keys.iter().copied()
    }
}

pub type BoolExpr<C> = Node<bool, C, VarExpr<bool, C>>;

/// A type-erased expression: the shape a tree holds when a sub-expression is
/// chosen at runtime (or stored in a heterogeneous collection) instead of being
/// named in the type.
///
/// One limitation follows from the box itself: it is never `Copy`, so a
/// `StoredExpr` composes with the arithmetic operators and the method nodes
/// (`max`, `clamp`, `sqrt`, …) but not with the comparison (`.ge()`, `.le()`, …)
/// or logical (`.and()`, `.or()`, …) combinators, whose impls require
/// `L: Expr<N, C> + Copy`.
pub type StoredExpr<N, C> = Box<dyn Expr<N, C> + Send + Sync>;

// Forwards through `Box` so the alias above composes into a tree: without it a
// `StoredExpr` implements nothing and cannot be a `Node` payload, an operand of
// `+ - * /`, or the expression of a plan step.
impl<N, C: Context, E: Expr<N, C> + ?Sized> Expr<N, C> for Box<E> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        (**self).eval(ctx)
    }

    #[inline(always)]
    fn get_dependencies(&self, deps: &mut Deps) {
        (**self).get_dependencies(deps);
    }
}

pub trait AsExpression<N, C: Context> {
    type Target: Expr<N, C> + Send + Sync + 'static;

    /// Consumes the wrapper and hands back the expression it holds.
    ///
    /// Named `as_expr` rather than `into_expr` because it takes `self` by value:
    /// `Node` is a zero-cost builder wrapper, so stripping it is a free cast, and
    /// every operator already hands back a fresh `Node` that cannot be borrowed
    /// past this point. Not an `as_` reference cast, hence the allow.
    #[allow(clippy::wrong_self_convention)]
    fn as_expr(self) -> Self::Target;
}

impl<N, C: Context, E> AsExpression<N, C> for Node<N, C, E>
where
    E: Expr<N, C> + Clone + Send + Sync + 'static,
{
    type Target = E;

    #[inline(always)]
    fn as_expr(self) -> Self::Target {
        self.expr
    }
}

macro_rules! impl_as_expression_for_primitives {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<C: Context> AsExpression<$ty, C> for $ty {
                type Target = LiteralNode<$ty>;

                #[inline(always)]
                fn as_expr(self) -> Self::Target {
                    LiteralNode { value: self }
                }
            }
        )*
    };
}

impl_as_expression_for_primitives!(
    f32, f64, i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, bool, char,
);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::{LiteralNode, Node};

    pub struct Ctx;

    impl Context for Ctx {
        type ContextItem<'w, 's> = ();
    }

    #[test]
    fn test_as_expression_node_unwrap() {
        let wrapped: Node<i32, Ctx, _> = Node::lit(7);
        let expr: LiteralNode<i32> = wrapped.as_expr();
        assert_eq!(expr.value, 7);
    }

    #[test]
    fn test_as_expression_primitives() {
        let f: LiteralNode<f32> = AsExpression::<f32, Ctx>::as_expr(2.75f32);
        assert_eq!(f.value, 2.75);

        let i: LiteralNode<i32> = AsExpression::<i32, Ctx>::as_expr(-42i32);
        assert_eq!(i.value, -42);

        let u: LiteralNode<u32> = AsExpression::<u32, Ctx>::as_expr(99u32);
        assert_eq!(u.value, 99);

        let b: LiteralNode<bool> = AsExpression::<bool, Ctx>::as_expr(true);
        assert!(b.value);

        let c: LiteralNode<char> = AsExpression::<char, Ctx>::as_expr('x');
        assert_eq!(c.value, 'x');
    }

    #[test]
    fn stored_expr_composes_into_a_tree() {
        use crate::dep_probe::{Attack, Defense, ProbeCtx, attr, key, keys, set};

        // A boxed expression as the payload of a node, then an operand of a
        // binary node: exactly the composition the exported `StoredExpr` alias
        // could not express before there was an `Expr` impl forwarding `Box`.
        let boxed: StoredExpr<f32, ProbeCtx> = Box::new(attr::<Attack>().expr);
        let boxed_node: Node<f32, ProbeCtx, _> = Node::from_expr(boxed);

        let tree = boxed_node + attr::<Defense>();

        assert_eq!(tree.eval(&()), 12.0, "the boxed operand still evaluates");
        assert_eq!(
            keys(&tree),
            set([key::<Attack>(), key::<Defense>()]),
            "get_dependencies forwards through the box"
        );
    }

    #[test]
    fn deps_equality_ignores_insertion_order_and_insert_is_idempotent() {
        // Equality is set equality: a hash set has no order to compare, and two
        // sinks holding the same keys are the same answer.
        let mut forwards = Deps::new();
        assert!(
            forwards.insert(DepKey::name("hp")),
            "the first insert reports a new key"
        );
        assert!(forwards.insert(DepKey::name("defense")));

        let mut backwards: Deps = [DepKey::name("defense"), DepKey::name("hp")]
            .into_iter()
            .collect();
        assert_eq!(
            forwards, backwards,
            "same keys in another order are still equal"
        );

        assert!(
            !forwards.insert(DepKey::name("hp")),
            "re-inserting a key reports false and changes nothing"
        );
        assert_eq!(forwards.len(), 2, "and the set stays two keys");
        assert_eq!(
            forwards, backwards,
            "so an idempotent insert preserves equality"
        );

        // A name and a type never compare equal, even for the same attribute.
        assert_ne!(
            DepKey::name("hp"),
            DepKey::type_of::<crate::nodes::VarExpr<f32, crate::dep_probe::ProbeCtx>>(),
            "the two keying schemes do not overlap"
        );

        backwards.insert(DepKey::name("haste"));
        assert_ne!(forwards, backwards, "and one extra key makes them unequal");
    }

    #[test]
    fn dep_key_accessors_split_the_two_variants() {
        let named = DepKey::name("hp");
        let typed = DepKey::type_of::<u32>();

        assert_eq!(named.as_name(), Some("hp"));
        assert_eq!(named.as_type_id(), None);
        assert_eq!(typed.as_type_id(), Some(TypeId::of::<u32>()));
        assert_eq!(typed.as_name(), None);

        // `type_of` is `TypeId::of` under another name, and distinct types are
        // distinct keys.
        assert_eq!(typed, DepKey::Type(TypeId::of::<u32>()));
        assert_ne!(typed, DepKey::type_of::<i32>());
    }

    #[test]
    fn dep_key_display_renders_both_variants_with_their_prefixes() {
        assert_eq!(DepKey::name("defense").to_string(), "var:defense");
        assert_eq!(
            DepKey::type_of::<u32>().to_string(),
            format!("type:{:?}", TypeId::of::<u32>()),
            "the type half prints what a TypeId actually offers: its Debug, prefixed"
        );
    }

    #[test]
    fn deps_iterate_into_keys_and_union_merges_them() {
        let hp: Deps = [DepKey::name("hp")].into_iter().collect();
        let defense: Deps = [DepKey::name("defense")].into_iter().collect();

        let mut merged = hp.clone();
        merged.union_with(&defense);

        assert_eq!(merged.len(), 2);
        assert!(merged.is_superset_of(&hp));
        assert!(merged.is_superset_of(&defense));
        assert!(!hp.is_superset_of(&defense));

        // `iter`, `for` over a reference, and `for` over the value all yield
        // `DepKey` itself, not a reference to one.
        let mut printed: Vec<String> = merged.iter().map(|key| key.to_string()).collect();
        printed.sort();
        assert_eq!(printed, ["var:defense", "var:hp"]);

        let mut by_ref: Vec<DepKey> = (&merged).into_iter().collect();
        by_ref.sort_by_key(|key| key.to_string());
        assert_eq!(by_ref, [DepKey::name("defense"), DepKey::name("hp")]);

        assert!(!merged.is_empty());
        let by_value: Vec<DepKey> = merged.into_iter().collect();
        assert_eq!(by_value.len(), 2, "and consuming the set yields each key");

        let mut emptied = Deps::new();
        assert!(emptied.is_empty());
        emptied.insert(DepKey::name("hp"));
        emptied.clear();
        assert!(emptied.is_empty(), "clear empties the sink for reuse");
    }
}
