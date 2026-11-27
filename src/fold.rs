//! Flat n-ary reductions: sum and product over a list of same-typed children.
//!
//! The reason this module exists is stack depth. A tree normally nests as deep as
//! your source code nests, and the compiler caps that long before it matters. The
//! one way to build a *deep* tree is to accumulate in a loop through a type-erased
//! sub-expression —
//!
//! ```ignore
//! let mut acc = Node::lit(0.0f32);
//! for row in rows {
//!     acc = Node::from_expr(Box::new(acc + weight(row)) as StoredExpr<..>) as ..;
//! }
//! ```
//!
//! which builds a linked chain one `eval` frame per row. Measured on rustc 1.99.1,
//! release, x86-64 Linux: 4,000 levels run anywhere, 20,000 needs a 1 MiB stack,
//! 60,000 overflows 1 MiB and 100,000 overflows the 8 MiB a main thread gets by
//! default — an abort, not an error you could match. `StoredExpr` carries the same
//! numbers and is the measurement of record.
//!
//! [`Node::sum`](crate::nodes::Node::sum) replaces that pattern with a vector:
//! accumulate the *children* into a `Vec`, then build one node. Evaluation folds in
//! a loop, printing loops, dropping loops, and the stack cost is one frame however
//! many rows go in.

use core::marker::PhantomData;

use num_traits::Num;

use crate::expr::{Context, Deps, Expr};
use crate::nodes::Node;

/// How a [`FoldNode`] combines two values. Sealed: the two markers here are the
/// whole set, because a reduction that is not associative cannot be flattened.
pub trait Fold<N>: Copy + sealed::Sealed {
    /// Combine the accumulator with the next child.
    fn fold(acc: N, next: N) -> N;

    /// The value of no children at all.
    fn identity() -> N;

    /// The token between children when a fold prints: `"+"` or `"*"`.
    fn token() -> &'static str;

    /// How the identity prints when there are no children: `"0"` or `"1"`. A
    /// numeral rather than the rendered value, so that printing a fold needs no
    /// `Display` on `N`.
    fn identity_text() -> &'static str;
}

mod sealed {
    pub trait Sealed {}
    impl Sealed for super::Sum {}
    impl Sealed for super::Product {}
}

/// Folds with `+`, empty is `N::zero()`. See [`Node::sum`](crate::nodes::Node::sum).
#[derive(Clone, Copy, Debug, Default)]
pub struct Sum;

/// Folds with `*`, empty is `N::one()`. See [`Node::product`](crate::nodes::Node::product).
#[derive(Clone, Copy, Debug, Default)]
pub struct Product;

impl<N: Num> Fold<N> for Sum {
    #[inline]
    fn fold(acc: N, next: N) -> N {
        acc + next
    }

    fn identity() -> N {
        N::zero()
    }

    fn token() -> &'static str {
        "+"
    }

    fn identity_text() -> &'static str {
        "0"
    }
}

impl<N: Num> Fold<N> for Product {
    #[inline]
    fn fold(acc: N, next: N) -> N {
        acc * next
    }

    fn identity() -> N {
        N::one()
    }

    fn token() -> &'static str {
        "*"
    }

    fn identity_text() -> &'static str {
        "1"
    }
}

/// A reduction over a list of children held flat, in a `Vec`.
///
/// `E` is the child expression type, and the node inherits its manners from it:
/// with `E = Node<..>` of a `Clone` payload the fold is `Clone`, and with
/// `E = StoredExpr<..>` it holds mixed leaf types and gives `Clone` up, the same
/// trade every erased sub-expression makes. No fold is ever `Copy` — the children
/// live in a `Vec`, and a `Vec` is not `Copy` whatever it holds — so composing a
/// node that holds one moves it.
///
/// Built through [`Node::sum`](crate::nodes::Node::sum) or
/// [`Node::product`](crate::nodes::Node::product) rather than by hand.
pub struct FoldNode<N, C, Op, E> {
    children: Vec<E>,
    _fold: PhantomData<(N, Op)>,
    _ctx: PhantomData<C>,
}

impl<N, C, Op, E> FoldNode<N, C, Op, E> {
    /// The children, in fold order.
    pub fn children(&self) -> &[E] {
        &self.children
    }

    pub(crate) fn new(children: Vec<E>) -> Self {
        Self {
            children,
            _fold: PhantomData,
            _ctx: PhantomData,
        }
    }
}

impl<N: Num, C: Context, Op: Fold<N>, E: Expr<N, C>> Expr<N, C> for FoldNode<N, C, Op, E> {
    #[inline]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        // A loop, not a recursion: this is the whole point of the node.
        //
        // The accumulator starts at the *first child*, not at the identity. Seeding
        // with `N::zero()` is arithmetically innocent and still wrong: `0.0 + (-0.0)`
        // is `+0.0`, so a zero-seeded fold turns a lone `-0.0` into `+0.0` where the
        // hand-nested expression keeps it, and a fold is meant to be the same
        // arithmetic written differently. Starting at the first child makes the fold
        // evaluate exactly `((a op b) op c)`, bit for bit, and leaves the identity
        // for the empty vector alone.
        let mut children = self.children.iter();
        let Some(first) = children.next() else {
            return Op::identity();
        };
        let mut acc = first.eval(ctx);
        for child in children {
            acc = Op::fold(acc, child.eval(ctx));
        }
        acc
    }

    fn get_dependencies(&self, deps: &mut Deps) {
        for child in &self.children {
            child.get_dependencies(deps);
        }
    }

    fn render(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self.children.first() {
            // A fold of nothing is the identity, and prints as it evaluates.
            None => f.write_str(Op::identity_text()),
            Some(first) => {
                write!(f, "(")?;
                first.render(f)?;
                for child in &self.children[1..] {
                    write!(f, " {} ", Op::token())?;
                    child.render(f)?;
                }
                write!(f, ")")
            }
        }
    }
}

// Written by hand rather than derived: `#[derive(Clone)]` would ask `N`, `C` and
// `Op` to be `Clone` too, and none of them takes part in what is stored.
impl<N, C, Op, E: Clone> Clone for FoldNode<N, C, Op, E> {
    fn clone(&self) -> Self {
        Self {
            children: self.children.clone(),
            _fold: PhantomData,
            _ctx: PhantomData,
        }
    }
}

// The two constructors sit on `Node`s whose operator is already fixed. Written as
// one generic impl over `Op`, `Node::sum` would leave `Op` unconstrained and every
// call would be a `type annotations needed` error, which is a strange failure for a
// function that says `sum` in its name.
impl<N: Num, C: Context, E> Node<N, C, FoldNode<N, C, Sum, E>> {
    /// One node over many children, folded with `+`, evaluated in a loop.
    ///
    /// This is how a list of records becomes one expression without a chain: push
    /// the per-row expressions into a `Vec` and call this once, instead of building
    /// a tree in the loop. Depth stays at one level no matter how many children go
    /// in, so the stack cost does not grow with the row count.
    ///
    /// ```ignore
    /// let terms: Vec<StoredExpr<f32, Ctx>> = rows
    ///     .iter()
    ///     .map(|r| Box::new(weight(r) * r.bonus) as StoredExpr<f32, Ctx>)
    ///     .collect();
    /// let total = Node::sum(terms);
    /// ```
    ///
    /// The first child seeds the accumulator, so a fold evaluates exactly the
    /// left-nested `((a + b) + c)` bit for bit — including a `-0.0`, which a
    /// zero-seeded fold would turn into `+0.0`. An empty list is `N::zero()` and
    /// prints as `0`.
    ///
    /// **A `Vec` holds one type.** Children of a single node type — `Vec<Node<f32,
    /// Ctx, Hp>>`, or `Vec<Node<f32, Ctx, LiteralNode<f32>>>` — go in directly.
    /// Children of *different* leaf types (an `Hp`, a `Bonus`, a literal) share one
    /// type only once erased, so collect them as `Vec<StoredExpr<f32, Ctx>>`; the
    /// fold then holds mixed leaves, gives up `Clone` like any erased tree, and
    /// prints and folds exactly the same.
    pub fn sum(children: Vec<E>) -> Self {
        Self::from_expr(FoldNode::new(children))
    }
}

impl<N: Num, C: Context, E> Node<N, C, FoldNode<N, C, Product, E>> {
    /// One node over many children, folded with `*`, evaluated in a loop. An empty
    /// list is `N::one()` and prints as `1`, and as with [`Node::sum`] the first
    /// child seeds the accumulator, so the bits match the left-nested product.
    ///
    /// Note the ordering: a fold multiplies left to right, and floating-point
    /// multiplication is not associative, so this and a hand-nested `((a * b) * c)`
    /// can differ in the last bits just as `(a + b) + c` can. The vector holds one
    /// type, as for [`Node::sum`].
    pub fn product(children: Vec<E>) -> Self {
        Self::from_expr(FoldNode::new(children))
    }
}
