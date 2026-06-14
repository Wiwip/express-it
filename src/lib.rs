/// Core expression types and operator overloads for float, int, and bool expressions.
pub mod expr;

/// Path-based context lookups that the expression system reads from.
pub mod context;

/// Shared numeric node type used by both float and integer expressions.
pub mod numeric;

/// Float-specific operator definitions: trig, rounding, and basic arithmetic.
pub mod float;

/// Plan execution and write-back caching.
pub mod frame;

/// Integer-specific operators: checked arithmetic and negation.
pub mod integer;

/// Boolean logic and comparisons.
pub mod logic;

/// Numeric casting between expression types.
pub mod num_cast;

/// Test scaffolding built on top of `MapContext`.
pub mod test_utils;
