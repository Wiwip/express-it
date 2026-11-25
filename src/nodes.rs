use crate::expr::{Context, Expr};
use num_traits::Num;
use std::any::{TypeId, type_name};
use std::collections::HashSet;
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

    #[inline(always)]
    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        self.expr.get_dependencies(deps);
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
    pub key: &'static str,
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

pub struct VarExpr<N, C: Context> {
    pub fetch_fn: for<'w, 's> fn(&C::ContextItem<'w, 's>) -> N,
}

impl<N, C: Context> Clone for VarExpr<N, C> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N, C: Context> Node<N, C, VarExpr<N, C>> {
    /// Constructs a variable node pre-wrapped for math operations
    pub fn new(fetch_fn: fn(&C::ContextItem<'_, '_>) -> N) -> Self {
        Node {
            expr: VarExpr { fetch_fn },
            _marker: PhantomData,
        }
    }
}

impl<N: 'static, C: Context + 'static> Expr<N, C> for VarExpr<N, C> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
        (self.fetch_fn)(ctx)
    }

    /// Reports the key of this variable. Note that the key is the identity of
    /// `VarExpr<N, C>` itself, so every variable built through [`Node::new`] with
    /// the same value type and the same context shares a single key. A leaf that
    /// needs per-variable identity carries its own key instead, which is what a
    /// custom `Expr` leaf does when it wraps `Node` as its `E` parameter.
    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
        deps.insert(TypeId::of::<Self>());
    }
}

impl<N, C: Context> Copy for VarExpr<N, C> {}

impl<N: fmt::Display, C: Context> fmt::Display for VarExpr<N, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var")
    }
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

    fn get_dependencies(&self, deps: &mut HashSet<TypeId>) {
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
    fn cast_display_names_the_target_type() {
        // `.cast::<i32>()` and `.cast::<u8>()` must not print the same string.
        let as_i32 = Node::<f32, Ctx, _>::lit(3.7f32).cast::<i32>();
        let as_u8 = Node::<f32, Ctx, _>::lit(3.7f32).cast::<u8>();

        assert_eq!(as_i32.to_string(), "cast::<i32>(3.7)");
        assert_eq!(as_u8.to_string(), "cast::<u8>(3.7)");
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
        // Documented limitation: VarExpr reports the identity of VarExpr<N, C>,
        // not of the variable, so two different variables of one value type and
        // one context collapse to a single key. Pinned so that giving variables
        // their own key has to change this test on purpose.
        let attack: Node<f32, Ctx, _> = Node::new(|()| 10.0f32);
        let defense: Node<f32, Ctx, _> = Node::new(|()| 2.0f32);

        let mut deps = HashSet::new();
        attack.get_dependencies(&mut deps);
        defense.get_dependencies(&mut deps);

        assert_eq!(deps.len(), 1, "both variables report the same VarExpr key");
    }
}
