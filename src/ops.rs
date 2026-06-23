use crate::expr::{Context, Expr};
use crate::nodes::LiteralNode;
use crate::nodes::Node;
use num_traits::{Float, Num};
use std::fmt;
use std::marker::PhantomData;

macro_rules! impl_binary_op {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $op:tt) => {
        #[derive(Clone, Copy)]
        pub struct $node_name<L, R> {
            pub lhs: L,
            pub rhs: R,
        }

        impl<N: Num, C: Context, L, R> Expr<N, C> for $node_name<L, R>
        where
            L: Expr<N, C>,
            R: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
                self.lhs.eval(ctx) $op self.rhs.eval(ctx)
            }
        }

        impl<L: fmt::Display, R: fmt::Display> fmt::Display for $node_name<L, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({} {} {})", self.lhs, stringify!($op), self.rhs)
            }
        }

        // Node + Node
        impl<N, C, L, R> std::ops::$trait_name<Node<N, C, R>> for Node<N, C, L>
        where
            N: Num + Copy,
            C: Context,
            L: Expr<N, C>,
            R: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<L, R>>;

            #[inline(always)]
            fn $method_name(self, rhs: Node<N, C, R>) -> Self::Output {
                Node {
                    expr: $node_name { lhs: self.expr, rhs: rhs.expr },
                    _marker: PhantomData,
                }
            }
        }

        // Node + Literal Primitive (e.g. expr + 10.0)
        impl<N, C, L> std::ops::$trait_name<N> for Node<N, C, L>
        where
            N: Num + Copy,
            C: Context,
            L: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<L, LiteralNode<N>>>;

            #[inline(always)]
            fn $method_name(self, rhs: N) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: LiteralNode { value: rhs },
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// Trait, Method, Generation Node Target, Operational Token
impl_binary_op!(Add, add, AddNode, +);
impl_binary_op!(Sub, sub, SubNode, -);
impl_binary_op!(Mul, mul, MulNode, *);
impl_binary_op!(Div, div, DivNode, /);
impl_binary_op!(Rem, rem, RemNode, %);

macro_rules! impl_left_literal {
    ($prim:ty, $trait_name:ident, $method_name:ident, $node_name:ident) => {
        // Primitive + Node value
        impl<C, L> std::ops::$trait_name<Node<$prim, C, L>> for $prim
        where
            C: Context,
            L: Expr<$prim, C>,
        {
            type Output = Node<$prim, C, $node_name<LiteralNode<$prim>, L>>;

            #[inline(always)]
            fn $method_name(self, rhs: Node<$prim, C, L>) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: LiteralNode { value: self },
                        rhs: rhs.expr,
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

macro_rules! bulk_impl_left_literals {
    ($($prim:ty),*) => {
        $(
            impl_left_literal!($prim, Add, add, AddNode);
            impl_left_literal!($prim, Sub, sub, SubNode);
            impl_left_literal!($prim, Mul, mul, MulNode);
            impl_left_literal!($prim, Div, div, DivNode);
        )*
    };
}

bulk_impl_left_literals!(
    f32, f64, // Floats
    i8, i16, i32, i64, i128, isize, // Signed Ints
    u8, u16, u32, u64, u128, usize // Unsigned Ints
);

macro_rules! impl_unary_op {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $op:tt) => {
        #[derive(Clone, Copy)]
        pub struct $node_name<E> {
            pub expr: E,
        }

        impl<N, C: Context, E> Expr<N, C> for $node_name<E>
        where
            N: std::ops::$trait_name<Output = N>,
            E: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
                $op self.expr.eval(ctx)
            }
        }

        impl<E: fmt::Display> fmt::Display for $node_name<E> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({}{})", stringify!($op), self.expr)
            }
        }

        // Value-by-Value Operator Overload (e.g., -node)
        impl<N, C, E> std::ops::$trait_name for Node<N, C, E>
        where
            N: std::ops::$trait_name<Output = N> + Copy,
            C: Context,
            E: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<E>>;

            #[inline(always)]
            fn $method_name(self) -> Self::Output {
                Node {
                    expr: $node_name { expr: self.expr },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// Trait, Method, Generation Node Target, Operational Token
impl_unary_op!(Neg, neg, NegNode, -); // For numeric negation (-x)
impl_unary_op!(Not, not, NotNode, !); // For logical/bitwise inversion (!x)

macro_rules! impl_binary_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $bound:path) => {
        #[derive(Clone, Copy)]
        pub struct $node_name<L, R> {
            pub lhs: L,
            pub rhs: R,
        }

        impl<N: $bound, C: Context, L, R> Expr<N, C> for $node_name<L, R>
        where
            L: Expr<N, C>,
            R: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
                self.lhs.eval(ctx).$method_name(self.rhs.eval(ctx))
            }
        }

        impl<L: fmt::Display, R: fmt::Display> fmt::Display for $node_name<L, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({}, {})", stringify!($method_name), self.lhs, self.rhs)
            }
        }

        pub trait $trait_name<RHS> {
            type Output;
            fn $method_name(self, rhs: RHS) -> Self::Output;
        }

        impl<N, C, L, R> $trait_name<Node<N, C, R>> for Node<N, C, L>
        where
            N: $bound + Copy,
            C: Context,
            L: Expr<N, C>,
            R: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<L, R>>;

            #[inline(always)]
            fn $method_name(self, rhs: Node<N, C, R>) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: rhs.expr,
                    },
                    _marker: PhantomData,
                }
            }
        }

        impl<N, C, L> $trait_name<N> for Node<N, C, L>
        where
            N: $bound + Copy,
            C: Context,
            L: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<L, LiteralNode<N>>>;

            #[inline(always)]
            fn $method_name(self, rhs: N) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: LiteralNode { value: rhs },
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// Extension Trait, Method Name, Node Target, Trait Bound Required
impl_binary_method!(ExprPow, powf, PowNode, Float);
impl_binary_method!(ExprMin, min, MinNode, Float);
impl_binary_method!(ExprMax, max, MaxNode, Float);

macro_rules! impl_unary_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $bound:path) => {
        #[derive(Clone, Copy)]
        pub struct $node_name<E> {
            pub expr: E,
        }

        impl<N: $bound, C: Context, E> Expr<N, C> for $node_name<E>
        where
            E: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
                self.expr.eval(ctx).$method_name()
            }
        }

        impl<E: fmt::Display> fmt::Display for $node_name<E> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}({})", stringify!($method_name), self.expr)
            }
        }

        pub trait $trait_name {
            type Output;
            fn $method_name(self) -> Self::Output;
        }

        impl<N, C, E> $trait_name for Node<N, C, E>
        where
            N: $bound + Copy,
            C: Context,
            E: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<E>>;

            #[inline(always)]
            fn $method_name(self) -> Self::Output {
                Node {
                    expr: $node_name { expr: self.expr },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// Extension Trait, Method Name, Node Target, Trait Bound Required
impl_unary_method!(ExprAbs, abs, AbsNode, num_traits::Signed);
impl_unary_method!(ExprAcos, acos, AcosNode, Float);
impl_unary_method!(ExprSqrt, sqrt, SqrtNode, Float);

macro_rules! impl_trinary_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $bound:path) => {
        #[derive(Clone, Copy)]
        pub struct $node_name<E, A, B> {
            pub expr: E,
            pub arg1: A,
            pub arg2: B,
        }

        impl<N: $bound, C: Context, E, A, B> Expr<N, C> for $node_name<E, A, B>
        where
            E: Expr<N, C>,
            A: Expr<N, C>,
            B: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> N {
                self.expr
                    .eval(ctx)
                    .$method_name(self.arg1.eval(ctx), self.arg2.eval(ctx))
            }
        }

        impl<E: fmt::Display, A: fmt::Display, B: fmt::Display> fmt::Display
            for $node_name<E, A, B>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    "{}({}, {}, {})",
                    stringify!($method_name),
                    self.expr, self.arg1, self.arg2
                )
            }
        }

        pub trait $trait_name<A, B> {
            type Output;
            fn $method_name(self, arg1: A, arg2: B) -> Self::Output;
        }

        impl<N, C, E, A, B> $trait_name<Node<N, C, A>, Node<N, C, B>> for Node<N, C, E>
        where
            N: $bound + Copy,
            C: Context,
            E: Expr<N, C>,
            A: Expr<N, C>,
            B: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<E, A, B>>;

            #[inline(always)]
            fn $method_name(self, arg1: Node<N, C, A>, arg2: Node<N, C, B>) -> Self::Output {
                Node {
                    expr: $node_name {
                        expr: self.expr,
                        arg1: arg1.expr,
                        arg2: arg2.expr,
                    },
                    _marker: PhantomData,
                }
            }
        }

        // Node . clamp(Literal, Literal)
        impl<N, C, E> $trait_name<N, N> for Node<N, C, E>
        where
            N: $bound + Copy,
            C: Context,
            E: Expr<N, C>,
        {
            type Output = Node<N, C, $node_name<E, LiteralNode<N>, LiteralNode<N>>>;

            #[inline(always)]
            fn $method_name(self, arg1: N, arg2: N) -> Self::Output {
                Node {
                    expr: $node_name {
                        expr: self.expr,
                        arg1: LiteralNode { value: arg1 },
                        arg2: LiteralNode { value: arg2 },
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// Extension Trait, Method Name, Node Target, Trait Bound Required
impl_trinary_method!(ExprClamp, clamp, FloatClampNode, Float);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::{Context, Expr};
    use crate::logic::{ExprCmpGe, ExprLogicalAnd, ExprLogicalOr};
    use crate::nodes::Node;

    pub struct Ctx;

    impl Context for Ctx {
        type ContextItem<'w, 's> = ();
    }

    #[test]
    fn test_binary_add_node_node() {
        let a = Node::<i32, Ctx, _>::lit(10);
        let b = Node::<i32, Ctx, _>::lit(7);
        assert_eq!((a + b).eval(&()), 17);
    }

    #[test]
    fn test_binary_add_node_literal() {
        let a = Node::<i32, Ctx, _>::lit(10);
        assert_eq!((a + 5).eval(&()), 15);
    }

    #[test]
    fn test_left_literal_add() {
        let a = Node::<i32, Ctx, _>::lit(10);
        assert_eq!((5 + a).eval(&()), 15);
    }

    #[test]
    fn test_binary_sub() {
        let a = Node::<i32, Ctx, _>::lit(10);
        assert_eq!((a - 3).eval(&()), 7);
    }

    #[test]
    fn test_binary_mul() {
        let a = Node::<i32, Ctx, _>::lit(6);
        assert_eq!((a * 4).eval(&()), 24);
    }

    #[test]
    fn test_binary_div() {
        let a = Node::<i32, Ctx, _>::lit(20);
        assert_eq!((a / 4).eval(&()), 5);
    }

    #[test]
    fn test_binary_rem() {
        let a = Node::<i32, Ctx, _>::lit(23);
        assert_eq!((a % 5).eval(&()), 3);
    }

    #[test]
    fn test_unary_neg() {
        let a = Node::<i32, Ctx, _>::lit(42);
        assert_eq!((-a).eval(&()), -42);
    }

    #[test]
    fn test_unary_not_bool_true() {
        let t = Node::<bool, Ctx, _>::lit(true);
        assert_eq!((!t).eval(&()), false);
    }

    #[test]
    fn test_unary_not_bool_false() {
        let f = Node::<bool, Ctx, _>::lit(false);
        assert_eq!((!f).eval(&()), true);
    }

    #[test]
    fn test_binary_method_powf_float() {
        let a = Node::<f32, Ctx, _>::lit(2.0);
        assert_eq!(a.powf(3.0).eval(&()), 8.0);
    }

    #[test]
    fn test_binary_method_min_float() {
        let a = Node::<f32, Ctx, _>::lit(3.5);
        assert_eq!(a.min(2.5).eval(&()), 2.5);
    }

    #[test]
    fn test_binary_method_max_float() {
        let a = Node::<f32, Ctx, _>::lit(3.5);
        assert_eq!(a.max(4.0).eval(&()), 4.0);
    }

    #[test]
    fn test_unary_method_abs_float() {
        let a = Node::<f32, Ctx, _>::lit(-6.25);
        assert_eq!(a.abs().eval(&()), 6.25);
    }

    #[test]
    fn test_unary_method_sqrt_float() {
        let a = Node::<f32, Ctx, _>::lit(9.0);
        assert_eq!(a.sqrt().eval(&()), 3.0);
    }

    #[test]
    fn test_unary_method_acos_float() {
        let a = Node::<f32, Ctx, _>::lit(1.0);
        assert!((a.acos().eval(&()) - 0.0).abs() < 1e-6);
    }

    #[test]
    fn test_trinary_method_clamp_float() {
        let a = Node::<f32, Ctx, _>::lit(15.0);
        assert_eq!(a.clamp(10.0, 20.0).eval(&()), 15.0);
        assert_eq!(a.clamp(20.0, 30.0).eval(&()), 20.0);
        assert_eq!(a.clamp(1.0, 5.0).eval(&()), 5.0);
    }

    #[test]
    fn test_fp_add_nan_propagates() {
        let a = Node::<f32, Ctx, _>::lit(1.0);
        let b = Node::<f32, Ctx, _>::lit(f32::NAN);
        assert!((a + b).eval(&()).is_nan());
    }

    #[test]
    fn test_fp_div_zero_gives_inf() {
        let a = Node::<f32, Ctx, _>::lit(1.0);
        assert_eq!((a / 0.0).eval(&()), f32::INFINITY);
    }

    #[test]
    fn test_fp_div_neg_zero_gives_neg_inf() {
        let a = Node::<f32, Ctx, _>::lit(1.0);
        assert_eq!((a / -0.0).eval(&()), f32::NEG_INFINITY);
    }

    #[test]
    fn test_logical_ops_short_circuit_style() {
        let t = Node::<bool, Ctx, _>::lit(true);
        let f = Node::<bool, Ctx, _>::lit(false);
        assert_eq!(t.and(f).eval(&()), false);
        assert_eq!(t.and(t).eval(&()), true);
        assert_eq!(f.or(t).eval(&()), true);
        assert_eq!(f.or(f).eval(&()), false);
    }

    #[test]
    #[should_panic]
    fn test_int_overflow_panics_in_debug() {
        let a = Node::<i32, Ctx, _>::lit(i32::MAX);
        let _ = (a + 1).eval(&());
    }

    #[test]
    #[should_panic]
    fn test_int_div_by_zero_panics() {
        let a = Node::<i32, Ctx, _>::lit(1);
        let _ = (a / 0).eval(&());
    }

    #[test]
    #[should_panic]
    fn test_int_mod_by_zero_panics() {
        let a = Node::<i32, Ctx, _>::lit(1);
        let _ = (a % 0).eval(&());
    }

    #[test]
    fn test_fp_mod_by_zero_gives_nan() {
        let a = Node::<f32, Ctx, _>::lit(1.0);
        assert!((a % 0.0).eval(&()).is_nan());
    }

    #[test]
    fn test_cast_roundtrip_f32_to_i32_and_back() {
        let inner: Node<f32, Ctx, _> = Node::lit(3.7f32);
        let to_int = inner.cast::<i32>();
        let back = to_int.cast::<f32>();
        assert_eq!(back.eval(&()), 3.0);
    }

    #[test]
    #[should_panic]
    fn test_cast_nan_to_int_panics() {
        let _ = Node::<f32, Ctx, _>::lit(f32::NAN).cast::<i32>().eval(&());
    }

    #[test]
    #[should_panic]
    fn test_cast_inf_to_int_panics() {
        let _ = Node::<f32, Ctx, _>::lit(f32::INFINITY)
            .cast::<i32>()
            .eval(&());
    }

    #[test]
    fn test_cast_through_variable_node() {
        let var: Node<i32, Ctx, _> = Node::new(|()| 7i32);
        let casted = var.cast::<f32>();
        assert_eq!(casted.eval(&()), 7.0);
    }

    #[test]
    fn test_cast_variable_via_new_then_via_cast_ends() {
        let inner: Node<i32, Ctx, _> = Node::new(|()| 11i32);
        let casted = inner.cast::<f32>();
        assert_eq!(casted.eval(&()), 11.0);
    }

    #[test]
    fn test_f64_sqrt_node() {
        let a = Node::<f64, Ctx, _>::lit(16.0);
        assert_eq!(a.sqrt().eval(&()), 4.0);
    }

    #[test]
    fn test_f64_powf_node() {
        let a = Node::<f64, Ctx, _>::lit(2.0);
        assert_eq!(a.powf(10.0).eval(&()), 1024.0);
    }

    #[test]
    fn test_acos_domain_error_yields_nan() {
        let a = Node::<f32, Ctx, _>::lit(2.0);
        assert!(a.acos().eval(&()).is_nan());
    }

    #[test]
    fn test_deeply_nested_arithmetic() {
        let a = Node::<i32, Ctx, _>::lit(10);
        let b = Node::<i32, Ctx, _>::lit(3);
        let c = Node::<i32, Ctx, _>::lit(2);
        let d = Node::<i32, Ctx, _>::lit(5);
        let result = ((a + b) * c - d) / 3;
        assert_eq!(result.eval(&()), 7);
    }

    #[test]
    fn test_mixed_variable_literal_deep_tree() {
        let base: Node<i32, Ctx, _> = Node::new(|()| 4);
        let lit = Node::<i32, Ctx, _>::lit(6);
        let tree = (base + lit) * 2 - 4;
        assert_eq!(tree.eval(&()), 16);
    }

    #[test]
    fn test_float_zero_identity_add() {
        let a = Node::<f32, Ctx, _>::lit(7.5);
        assert_eq!((a + 0.0).eval(&()), 7.5);
    }

    #[test]
    fn test_float_neg_zero_equals_zero() {
        use crate::logic::ExprCmpEq;
        let a = Node::<f32, Ctx, _>::lit(0.0);
        let b = Node::<f32, Ctx, _>::lit(-0.0);
        assert!(a.eq(b).eval(&()));
    }

    #[test]
    fn test_many_node_tree_no_stack_overflow() {
        let cur: Node<i32, Ctx, _> = Node::lit(1);
        assert_eq!(cur.eval(&()), 1);
    }

    #[test]
    fn test_clamp_rounds_down_at_floor() {
        let a = Node::<f32, Ctx, _>::lit(3.3);
        assert_eq!(a.clamp(5.0, 10.0).eval(&()), 5.0);
    }

    #[test]
    fn test_clamp_rounds_up_at_ceiling() {
        let a = Node::<f32, Ctx, _>::lit(12.7);
        assert_eq!(a.clamp(5.0, 10.0).eval(&()), 10.0);
    }

    #[test]
    fn test_left_literal_sub() {
        let a = Node::<i32, Ctx, _>::lit(10);
        assert_eq!((5 - a).eval(&()), -5);
    }

    #[test]
    fn test_left_literal_mul() {
        let a = Node::<i32, Ctx, _>::lit(4);
        assert_eq!((3 * a).eval(&()), 12);
    }

    #[test]
    fn test_left_literal_div() {
        let a = Node::<i32, Ctx, _>::lit(2);
        assert_eq!((10 / a).eval(&()), 5);
    }

    #[test]
    fn test_rem_with_negative_dividend() {
        let a = Node::<i32, Ctx, _>::lit(-10);
        assert_eq!((a % 3).eval(&()), -1);
    }

    #[test]
    fn test_composed_game_style_rule() {
        let hp = Node::<i32, Ctx, _>::new(|()| 3);
        let shield = Node::<i32, Ctx, _>::new(|()| 2);
        let burst = Node::<i32, Ctx, _>::lit(9);

        let effective_hp = hp + shield;
        let overkill = burst - effective_hp;
        let threshold = Node::<i32, Ctx, _>::lit(3);
        let is_lethal = overkill.ge(threshold);

        assert!(is_lethal.eval(&()));
        assert_eq!(effective_hp.eval(&()), 5);
        assert_eq!(overkill.eval(&()), 4);
    }

    #[test]
    fn test_shared_subexpression_reuse() {
        let base = Node::<f32, Ctx, _>::new(|()| 2.0);
        let scale = Node::<f32, Ctx, _>::lit(3.0);

        let projected = base * scale;
        let bonus = projected + 5.0;
        let penalty = projected - 1.0;

        assert_eq!(projected.eval(&()), 6.0);
        assert_eq!(bonus.eval(&()), 11.0);
        assert_eq!(penalty.eval(&()), 5.0);
    }

    #[test]
    fn test_cast_then_compare_chain() {
        let raw = Node::<f32, Ctx, _>::new(|()| 9.7);
        let as_int = raw.cast::<i32>();
        let threshold = Node::<i32, Ctx, _>::lit(9);

        assert!(as_int.ge(threshold).eval(&()));
        assert_eq!(as_int.eval(&()), 9);
    }

    #[test]
    fn test_mixed_float_int_boundary_chain() {
        let damage = Node::<f32, Ctx, _>::lit(9.5);
        let divisor = Node::<f32, Ctx, _>::lit(2.0);
        let result = (damage / divisor).cast::<i32>();
        assert_eq!(result.eval(&()), 4);
    }

    #[test]
    fn test_clamp_inside_float_chain() {
        let raw = Node::<f32, Ctx, _>::new(|()| 14.0);
        let damped = raw * 0.5;
        let clamped = damped.clamp(5.0, 8.0);

        assert_eq!(damped.eval(&()), 7.0);
        assert_eq!(clamped.eval(&()), 7.0);
    }

    #[test]
    fn test_min_max_in_float_chain() {
        let base = Node::<f32, Ctx, _>::new(|()| 50.0);
        let pool = base / 10.0;
        let floor = 2.0;
        let ceiling = 7.0;

        let step1 = pool.min(floor);
        let final_val = step1.max(ceiling);

        assert_eq!(pool.eval(&()), 5.0);
        assert_eq!(step1.eval(&()), 2.0);
        assert_eq!(final_val.eval(&()), 7.0);
    }

    #[test]
    fn test_powf_in_nested_arithmetic() {
        let base = Node::<f32, Ctx, _>::lit(2.0);
        let power = Node::<f32, Ctx, _>::lit(3.0);
        let squared = base.powf(power);
        let total = squared + 1.0;

        assert_eq!(squared.eval(&()), 8.0);
        assert_eq!(total.eval(&()), 9.0);
    }

    #[test]
    fn test_three_way_logical_branch() {
        let a = Node::<bool, Ctx, _>::lit(true);
        let b = Node::<bool, Ctx, _>::lit(false);
        let c = Node::<bool, Ctx, _>::lit(true);

        let branch1 = a.and(b).or(c);
        let branch2 = a.or(b).and(c);
        let branch3 = a.or(b).or(c);

        assert!(branch1.eval(&()));
        assert!(branch2.eval(&()));
        assert!(branch3.eval(&()));
    }
}
