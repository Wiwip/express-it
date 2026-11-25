//! `Node`'s `_marker` field is `pub`, `#[doc(hidden)]`, and stays that way on
//! purpose: a `Node { expr, _marker }` literal written before [`Node::from_expr`]
//! existed must keep compiling. Nothing inside the crate writes such a literal
//! any more, so only a test in a SEPARATE CRATE can pin the field's visibility —
//! an in-crate unit test would compile against a private field just as happily.
//! Integration tests are their own crate, which is what this file is.
//!
//! If someone privatizes the field, the literal below stops with `E0616` ("field
//! `_marker` of struct `Node` is private") and this test stops compiling; the
//! rationale lives in `from_expr`'s doc comment, where `cargo doc` can show it.

use express_it::expr::{Context, Expr};
use express_it::nodes::Node;
use std::any::TypeId;
use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;

struct Ctx;

impl Context for Ctx {
    type ContextItem<'w, 's> = ();
}

/// A leaf written by hand, the shape `from_expr` takes.
#[derive(Clone, Copy)]
struct Hp;

impl Expr<f32, Ctx> for Hp {
    fn eval(&self, _ctx: &()) -> f32 {
        7.5
    }

    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        deps.insert(TypeId::of::<Self>());
    }
}

impl fmt::Display for Hp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hp")
    }
}

#[test]
fn a_node_literal_written_outside_the_crate_still_compiles() {
    // The pre-`from_expr` spelling, from another crate: this is the line that
    // requires `_marker` to be `pub`, and the only thing that says so.
    let written_by_hand: Node<f32, Ctx, Hp> = Node {
        expr: Hp,
        _marker: PhantomData,
    };

    // The documented route builds the same node.
    let built = Node::<f32, Ctx, _>::from_expr(Hp);

    assert_eq!(written_by_hand.eval(&()), 7.5);
    assert_eq!(built.eval(&()), 7.5);
    assert_eq!(written_by_hand.to_string(), built.to_string(), "Hp");

    let mut deps = HashSet::new();
    written_by_hand.get_dependencies(&mut deps);
    assert_eq!(deps.len(), 1, "and the literal carries the leaf's own key");
}
