use crate::expr::{Context, Expr};
use num_traits::Num;
use std::fmt;
use std::marker::PhantomData;

pub struct Node<N, C, E> {
    pub expr: E,
    pub _marker: PhantomData<(N, C)>,
}

impl<N: 'static + Clone, C: Context, E: Expr<N, C>> Node<N, C, E> {
    pub fn alias(self, key: &'static str) -> CacheSaveStep<N, C, E> {
        CacheSaveStep { key, expr: self }
    }
}

impl<N, C: Context> Node<N, C, LiteralNode<N>> {
    #[inline(always)]
    pub fn lit(value: N) -> Self {
        Self {
            expr: LiteralNode { value },
            _marker: PhantomData,
        }
    }
}

impl<N, C, E: Clone> Clone for Node<N, C, E> {
    fn clone(&self) -> Self {
        Node {
            expr: self.expr.clone(),
            _marker: PhantomData,
        }
    }
}

impl<N, C, E: Copy> Copy for Node<N, C, E> {}

impl<N: Num, C: Context, E: Expr<N, C>> Node<N, C, E> {
    pub fn cast<To: Num>(self) -> Node<To, C, CastNode<N, To, C, E>> {
        Node {
            expr: CastNode::new(self.expr),
            _marker: PhantomData,
        }
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
}

impl<N: fmt::Display, C: Context, E: Expr<N, C> + fmt::Display> fmt::Display
    for Node<N, C, E>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

/// Evaluates `expr` and stores the result in the PlanCache under `key`.
/// Does NOT write to the context state — pure cache-only step.
#[derive(Clone, Copy)]
pub struct CacheSaveStep<N: 'static, C: Context, E> {
    pub key: &'static str,
    pub expr: Node<N, C, E>,
}

pub type Var<N, C> = Node<N, C, VarNode<N, C>>;

pub struct VarNode<N, C: Context> {
    pub fetch_fn: for<'w, 's> fn(&C::ContextItem<'w, 's>) -> N,
}

impl<N, C: Context> Clone for VarNode<N, C> {
    fn clone(&self) -> Self {
        Self {
            fetch_fn: self.fetch_fn,
        }
    }
}

impl<N, C: Context> Node<N, C, VarNode<N, C>> {
    /// Constructs a variable node pre-wrapped for math operations
    pub fn new(fetch_fn: fn(&C::ContextItem<'_, '_>) -> N) -> Self {
        Node {
            expr: VarNode { fetch_fn },
            _marker: PhantomData,
        }
    }
}

impl<N, C: Context> Expr<N, C> for VarNode<N, C> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        (self.fetch_fn)(ctx)
    }
}

impl<N, C: Context> Copy for VarNode<N, C> {}

impl<N: fmt::Display, C: Context> fmt::Display for VarNode<N, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var")
    }
}

impl<From: Num, To: Num, C: Context, E: Expr<From, C>> fmt::Display for CastNode<From, To, C, E>
where
    From: fmt::Display,
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "cast({})", self.inner)
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

pub struct CastNode<From: Num, To: Num, C: Context, E: Expr<From, C>> {
    inner: E,
    _marker: PhantomData<(From, To, C)>,
}

impl<From: Num, To: Num, C: Context, T: Expr<From, C>> CastNode<From, To, C, T> {
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
    From: Num + num_traits::ToPrimitive + num_traits::NumCast,
    To: Num + num_traits::NumCast,
    T: Expr<From, C>,
{
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> To {
        let val = self.inner.eval(ctx);
        num_traits::cast(val).expect("Failed to cast numeric value")
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
        let a = LiteralNode { value: 1i32 };
        let _ = a;
    }

    #[test]
    fn test_variable_node_is_copy() {
        let a: Node<i32, Ctx, _> = Node::new(|()| 4i32);
        let _ = a;
    }

    #[test]
    fn test_cast_nan_f64_to_f32_preserves_nan() {
        let inner = LiteralNode { value: f64::NAN };
        let node = CastNode::<f64, f32, Ctx, _>::new(inner);
        assert!(node.eval(&()).is_nan());
    }

    #[test]
    fn test_cast_node_is_copy() {
        let inner = LiteralNode { value: 3i32 };
        let node = CastNode::<i32, f32, Ctx, _>::new(inner);
        let _ = node;
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
    fn test_cast_failure_panics() {
        let inner = LiteralNode { value: i64::MAX };
        let node = CastNode::<i64, i8, Ctx, _>::new(inner);
        let result = std::panic::catch_unwind(|| node.eval(&()));
        assert!(result.is_err());
    }
}
