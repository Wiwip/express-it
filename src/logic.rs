use crate::expr::{AsExpression, Context, Expr};
use crate::nodes::Node;
use std::fmt;
use std::marker::PhantomData;

#[derive(Copy, Clone)]
pub struct LogicalNotNode<E> {
    pub inner: E,
}

impl<C: Context, E: Expr<bool, C>> Expr<bool, C> for LogicalNotNode<E> {
    #[inline(always)]
    fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> bool {
        !self.inner.eval(ctx)
    }
}

impl<E: fmt::Display> fmt::Display for LogicalNotNode<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(not {})", self.inner)
    }
}

pub trait ExprLogicNot {
    type Output;
    fn not(self) -> Self::Output;
}

impl<C: Context, L: Expr<bool, C> + Copy> ExprLogicNot for Node<bool, C, L> {
    type Output = Node<bool, C, LogicalNotNode<L>>;

    #[inline(always)]
    fn not(self) -> Self::Output {
        Node {
            expr: LogicalNotNode { inner: self.expr },
            _marker: std::marker::PhantomData,
        }
    }
}

macro_rules! impl_cmp_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $bound:path, $operator:tt) => {
        #[derive(Copy, Clone)]
        pub struct $node_name<N, L, R> {
            pub lhs: L,
            pub rhs: R,
            pub _marker: std::marker::PhantomData<N>,
        }

        impl<N: $bound, C: Context, L, R> Expr<bool, C> for $node_name<N, L, R>
        where
            L: Expr<N, C>,
            R: Expr<N, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> bool {
                self.lhs.eval(ctx) $operator self.rhs.eval(ctx)
            }
        }

        impl<N: fmt::Display, L: fmt::Display, R: fmt::Display> fmt::Display for $node_name<N, L, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({} {} {})", self.lhs, stringify!($operator), self.rhs)
            }
        }

        pub trait $trait_name<R> {
            type Output;
            fn $method_name(self, rhs: R) -> Self::Output;
        }

        impl<N, C, L, R> $trait_name<R> for Node<N, C, L>
        where
            N: $bound + Copy,
            C: Context,
            L: Expr<N, C> + Copy,
            R: AsExpression<N, C>,
            R::Target: Copy,
        {
            type Output = Node<bool, C, $node_name<N, L, R::Target>>;

            #[inline(always)]
            fn $method_name(self, rhs: R) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: rhs.as_expr(),
                        _marker: std::marker::PhantomData,
                    },
                    _marker: std::marker::PhantomData,
                }
            }
        }
    };
}

impl_cmp_method!(ExprCmpGt, gt, GreaterThanNode,  std::cmp::PartialOrd, >);
impl_cmp_method!(ExprCmpLt, lt, LessThanNode,     std::cmp::PartialOrd, <);
impl_cmp_method!(ExprCmpEq, eq, EqualsNode,       std::cmp::PartialEq, ==);
impl_cmp_method!(ExprCmpNotEq, ne, NotEqualsNode, std::cmp::PartialEq, !=);
impl_cmp_method!(ExprCmpGe, ge, GreaterEqualNode, std::cmp::PartialOrd, >=);
impl_cmp_method!(ExprCmpLe, le, LessEqualNode,    std::cmp::PartialOrd, <=);

macro_rules! impl_logical_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $operator:tt) => {
        #[derive(Copy, Clone)]
        pub struct $node_name<L, R> {
            pub lhs: L,
            pub rhs: R,
        }

        impl<C: Context, L, R> Expr<bool, C> for $node_name<L, R>
        where
            L: Expr<bool, C>,
            R: Expr<bool, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> bool {
                self.lhs.eval(ctx) $operator self.rhs.eval(ctx)
            }
        }

        impl<L: fmt::Display, R: fmt::Display> fmt::Display for $node_name<L, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "({} {} {})", self.lhs, stringify!($operator), self.rhs)
            }
        }

        pub trait $trait_name<R> {
            type Output;
            fn $method_name(self, rhs: R) -> Self::Output;
        }

        impl<C, L, R> $trait_name<R> for Node<bool, C, L>
        where
            C: Context,
            L: Expr<bool, C> + Copy,
            R: AsExpression<bool, C>,
            R::Target: Copy,
        {
            type Output = Node<bool, C, $node_name<L, R::Target>>;

            #[inline(always)]
            fn $method_name(self, rhs: R) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: rhs.as_expr(),
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

// all/any
impl_logical_method!(ExprLogicalAnd, and, LogicalAndNode, &&);
impl_logical_method!(ExprLogicalOr,  or,  LogicalOrNode,  ||);
impl_logical_method!(ExprLogicalXor, xor, LogicalXorNode,  ^);

macro_rules! impl_neg_logical_method {
    ($trait_name:ident, $method_name:ident, $node_name:ident, $operator:tt) => {
        #[derive(Copy, Clone)]
        pub struct $node_name<L, R> {
            pub lhs: L,
            pub rhs: R,
        }

        impl<C: Context, L, R> Expr<bool, C> for $node_name<L, R>
        where
            L: Expr<bool, C>,
            R: Expr<bool, C>,
        {
            #[inline(always)]
            fn eval(&self, ctx: &C::ContextItem<'_, '_>) -> bool {
                !(self.lhs.eval(ctx) $operator self.rhs.eval(ctx))
            }
        }

        impl<L: fmt::Display, R: fmt::Display> fmt::Display for $node_name<L, R> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "!({} {} {})", self.lhs, stringify!($operator), self.rhs)
            }
        }

        pub trait $trait_name<R> {
            type Output;
            fn $method_name(self, rhs: R) -> Self::Output;
        }

        impl<C, L, R> $trait_name<R> for Node<bool, C, L>
        where
            C: Context,
            L: Expr<bool, C> + Copy,
            R: AsExpression<bool, C>,
            R::Target: Copy,
        {
            type Output = Node<bool, C, $node_name<L, R::Target>>;

            #[inline(always)]
            fn $method_name(self, rhs: R) -> Self::Output {
                Node {
                    expr: $node_name {
                        lhs: self.expr,
                        rhs: rhs.as_expr(),
                    },
                    _marker: PhantomData,
                }
            }
        }
    };
}

impl_neg_logical_method!(ExprLogicalNand, nand, LogicalNandNode, &&);
impl_neg_logical_method!(ExprLogicalNor,  nor,  LogicalNorNode,  ||);
impl_neg_logical_method!(ExprLogicalNxor, nxor, LogicalNxorNode,  ^);

#[macro_export]
macro_rules! all {
    () => {{ Node::lit(true) }};
    ($head:expr $(, $tail:expr)* $(,)?) => {{
        $head
        $(
            .and($tail)
        )*
    }};
}

#[macro_export]
macro_rules! any {
    () => {{ Node::lit(false) }};
    ($head:expr $(, $tail:expr)* $(,)?) => {{
        $head
        $(
            .or($tail)
        )*
    }};
}

impl<C: Context> Expr<bool, C> for bool {
    #[inline(always)]
    fn eval(&self, _ctx: &C::ContextItem<'_, '_>) -> bool {
        *self
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{Context, Expr};
    use crate::logic::*;
    use crate::nodes::{Node, VarNode};
    use std::marker::PhantomData;

    // 1. Setup a Mock Context for the Engine to Evaluate Against
    pub struct MockEngine;

    pub struct MockPlayerData {
        pub damage: f32,
        pub level: i32,
        pub is_poisoned: bool,
    }

    impl Context for MockEngine {
        // Using a reference to our player data struct as the runtime context item
        type ContextItem<'w, 's> = &'w MockPlayerData;
    }

    // Helper to create a variable node cleanly in tests
    fn var<N>(
        f: for<'a, 'w> fn(&'a &'w MockPlayerData) -> N,
    ) -> Node<N, MockEngine, VarNode<N, MockEngine>> {
        Node {
            expr: VarNode { fetch_fn: f },
            _marker: PhantomData,
        }
    }

    #[test]
    fn test_primitive_as_expression_literals() {
        let ctx_data = MockPlayerData {
            damage: 100.0,
            level: 1,
            is_poisoned: false,
        };

        // Ensure literal creation and AsExpression are functional
        let lit_true = true;
        let lit_false = false;

        assert!(Expr::<bool, MockEngine>::eval(&lit_true, &&ctx_data));
        assert!(!Expr::<bool, MockEngine>::eval(&lit_false, &&ctx_data));
    }

    #[test]
    fn test_basic_comparisons() {
        let ctx_data = MockPlayerData {
            damage: 150.0,
            level: 10,
            is_poisoned: false,
        };

        let damage_node = Node::<f32, MockEngine, _>::lit(150.0);

        // Test Greater Than (gt) and Less Than (lt) using automatic primitive conversions
        let is_gt = damage_node.gt(100.0);
        let is_lt = damage_node.lt(50.0);
        let is_eq = damage_node.eq(150.0);

        assert!(is_gt.eval(&&ctx_data), "150.0 should be greater than 100.0");
        assert!(
            !is_lt.eval(&&ctx_data),
            "150.0 should not be less than 50.0"
        );
        assert!(is_eq.eval(&&ctx_data), "150.0 should equal 150.0");
    }

    #[test]
    fn test_logical_and_or_chaining() {
        let ctx_data = MockPlayerData {
            damage: 150.0,
            level: 10,
            is_poisoned: false,
        };

        let t = Node::<bool, MockEngine, _>::lit(true);
        let f = Node::<bool, MockEngine, _>::lit(false);

        // Test combination scenarios
        assert!(t.and(true).eval(&&ctx_data));
        assert!(!t.and(false).eval(&&ctx_data));
        assert!(t.or(false).eval(&&ctx_data));
        assert!(!f.or(false).eval(&&ctx_data));

        // Complex structural chain: (true && false) || true => true
        let complex_chain = t.and(f).or(t);
        assert!(complex_chain.eval(&&ctx_data));
    }

    #[test]
    fn test_dynamic_variable_evaluation() {
        // Setup two different engine states to ensure the expressions are evaluated lazily
        let low_state = MockPlayerData {
            damage: 45.0,
            level: 5,
            is_poisoned: false,
        };
        let high_state = MockPlayerData {
            damage: 120.0,
            level: 60,
            is_poisoned: true,
        };

        // Construct dynamic rule expressions using variable captures
        let dynamic_damage = var(|ctx| ctx.damage);
        let dynamic_poison = var(|ctx| ctx.is_poisoned);
        let dynamic_level = var(|ctx| ctx.level);

        // Rule: (Damage > 100.0 AND level >= 50) OR target is poisoned
        let boss_mechanic_trigger = dynamic_damage
            .gt(100.0)
            .and(dynamic_level.ge(50))
            .or(dynamic_poison);

        // Verify state 1 (Low state matches none of the criteria)
        assert!(
            !boss_mechanic_trigger.eval(&&low_state),
            "Low state should not trigger mechanic"
        );

        // Verify state 2 (High state matches all criteria)
        assert!(
            boss_mechanic_trigger.eval(&&high_state),
            "High state should successfully trigger mechanic"
        );
    }

    #[test]
    fn test_node_is_copy() {
        let ctx_data = MockPlayerData {
            damage: 150.0,
            level: 10,
            is_poisoned: false,
        };
        let damage_node = Node::<f32, MockEngine, _>::lit(150.0);

        // If Node is Copy, we can reuse `damage_node` across multiple calls
        // without compiling into a "use of moved value" panic or compile error.
        let check_one = damage_node.gt(100.0);
        let check_two = damage_node.lt(200.0); // Reusing safely!

        assert!(check_one.eval(&&ctx_data));
        assert!(check_two.eval(&&ctx_data));
    }

    #[test]
    fn test_mixed_variable_to_variable_comparisons() {
        let ctx_data = MockPlayerData {
            damage: 150.0,
            level: 10,
            is_poisoned: false,
        };

        let damage_node = var(|ctx| ctx.damage);
        let level_node = var(|ctx| ctx.level as f32);

        let comparison = damage_node.gt(level_node);

        assert!(comparison.eval(&&ctx_data));
    }

    #[test]
    fn test_all_macro_true() {
        let ctx_data = MockPlayerData {
            damage: 50.0,
            level: 10,
            is_poisoned: false,
        };

        let cond1 = Node::<f32, MockEngine, _>::new(|ctx| ctx.damage).gt(0.0);
        let cond2 = Node::<i32, MockEngine, _>::new(|ctx| ctx.level).gt(0);
        let cond3 = Node::<bool, MockEngine, _>::new(|ctx| ctx.is_poisoned).not();

        let all_true = all![cond1, cond2, cond3];
        assert!(all_true.eval(&&ctx_data));
    }

    #[test]
    fn test_all_macro_false() {
        let ctx_data = MockPlayerData {
            damage: 50.0,
            level: 10,
            is_poisoned: false,
        };

        let cond1 = Node::<f32, MockEngine, _>::new(|ctx| ctx.damage).gt(0.0);
        let cond2 = Node::<bool, MockEngine, _>::new(|ctx| ctx.is_poisoned);
        let cond3 = Node::<i32, MockEngine, _>::new(|ctx| ctx.level).gt(0);

        let all_check = all![cond1, cond2, cond3];
        assert!(!all_check.eval(&&ctx_data));
    }

    #[test]
    fn test_any_macro_true_first() {
        let ctx_data = MockPlayerData {
            damage: 0.0,
            level: 0,
            is_poisoned: true,
        };

        let cond1 = Node::<f32, MockEngine, _>::new(|ctx| ctx.damage).gt(10.0);
        let cond2 = Node::<bool, MockEngine, _>::new(|ctx| ctx.is_poisoned);

        let any_check = any![cond1, cond2];
        assert!(any_check.eval(&&ctx_data));
    }

    #[test]
    fn test_any_macro_true_last() {
        let ctx_data = MockPlayerData {
            damage: 5.0,
            level: 1,
            is_poisoned: false,
        };

        let cond1 = Node::<f32, MockEngine, _>::new(|ctx| ctx.damage).gt(10.0);
        let cond2 = Node::<i32, MockEngine, _>::new(|ctx| ctx.level).ge(2);
        let cond3 = Node::<bool, MockEngine, _>::new(|ctx| ctx.is_poisoned).not();

        let any_check = any![cond1, cond2, cond3];
        assert!(any_check.eval(&&ctx_data));
    }

    #[test]
    fn test_any_macro_all_false() {
        let ctx_data = MockPlayerData {
            damage: 1.0,
            level: 1,
            is_poisoned: false,
        };

        let cond1 = Node::<f32, MockEngine, _>::new(|ctx| ctx.damage).gt(10.0);
        let cond2 = Node::<i32, MockEngine, _>::new(|ctx| ctx.level).ge(2);
        let cond3 = Node::<bool, MockEngine, _>::new(|ctx| ctx.is_poisoned);

        let any_check = any![cond1, cond2, cond3];
        assert!(!any_check.eval(&&ctx_data));
    }
}
