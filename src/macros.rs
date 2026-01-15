#[macro_export]
macro_rules! impl_float_binary_ops {
    (
        $target:ident,
        $node_variant:ident,
        $op_enum:ident,
        [$($trait:ident => ($method:ident, $variant:ident)),* $(,)?]
    ) => {
        $(
            impl<N, Ctx> std::ops::$trait for Expr<N, Ctx, $target<N, Ctx>>
            where
                N: Float + Send + Sync,
                Ctx: crate::context::EvalContext,
                $target<N, Ctx>: crate::expr::ExprNode<N, Ctx>,
            {
                type Output = Self;

                fn $method(self, rhs: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs: self,
                        op: $op_enum::$variant,
                        rhs,
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
            impl<N, Ctx> std::ops::$trait for Expr<N, Ctx, $target<N, Ctx>>
            where
                N: PrimInt + CheckedNeg + AsPrimitive<u32> + Send + Sync,
                Ctx: crate::context::EvalContext,
                $target<N, Ctx>: crate::expr::ExprNode<N, Ctx>,
            {
                type Output = Self;

                fn $method(self, rhs: Self) -> Self::Output {
                    Expr::new(std::sync::Arc::new($target::$node_variant {
                        lhs: self,
                        op: $op_enum::$variant,
                        rhs,
                    }))
                }
            }
        )*
    };
}
