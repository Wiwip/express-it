#[macro_export]
macro_rules! impl_float_binary_ops {
    (
        $target:ident,
        $node_variant:ident,
        $op_enum:ident,
        [$($trait:ident => ($method:ident, $variant:ident)),* $(,)?]
    ) => {
        $(
            impl<N> std::ops::$trait for Expr<N, $target<N>>
            where
                N: Float + Send + Sync,
                $target<N>: crate::expr::ExprNode<N>,
            {
                type Output = Self;

                fn $method(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::$variant,
                        rhs_expr,
                    }))
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! impl_int_binary_ops {
    (
        $target:ident,
        $node_variant:ident,
        $op_enum:ident,
        [$($trait:ident => ($method:ident, $variant:ident)),* $(,)?]
    ) => {
        $(
            impl<N> std::ops::$trait for Expr<N, $target<N>>
            where
                N: PrimInt + CheckedNeg + AsPrimitive<u32> + Send + Sync,
                $target<N>: crate::expr::ExprNode<N>,
            {
                type Output = Self;

                fn $method(self, rhs_expr: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs_expr: self,
                        op: $op_enum::$variant,
                        rhs_expr,
                    }))
                }
            }
        )*
    };
}
