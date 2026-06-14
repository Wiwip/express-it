use crate::expr::ExpressionError;
use smol_str::SmolStr;
use std::any::Any;
use std::fmt::Debug;
use std::hash::Hash;

/// The human-readable path used by your user library to build expressions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Path {
    inner: SmolStr,
}

impl Path {
    pub fn new(path: impl Into<SmolStr>) -> Self {
        Self {
            inner: path.into(),
        }
    }

    /// Automatically uses the Rust type name as the root
    pub fn from_type_name<T: 'static>(
        prefix: impl Into<SmolStr>,
        suffix: impl Into<SmolStr>,
    ) -> Self {
        let prefix = prefix.into();
        let suffix = suffix.into();
        let name = Self::get_short_name::<T>();

        let path = format!("{}.{}.{}", prefix, name, suffix);
        Self::from_name(path)
    }

    /// Creates a new Path from a specific name
    pub fn from_name(name: impl Into<SmolStr>) -> Self {
        Self {
            inner: name.into(),
        }
    }

    pub fn from_segments(segments: &[&str]) -> Self {
        Self::from_name(segments.join("."))
    }

    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn hash_value(&self) -> u64 {
        fnv1a64(self.as_str())
    }

    fn get_short_name<T>() -> &'static str {
        let full_name = std::any::type_name::<T>();
        full_name.split("::").last().unwrap_or(full_name)
    }
}

pub trait ReadContext: Send + Sync {
    fn get_any(&self, path: &Path) -> Result<&dyn Any, ExpressionError>;
}

pub trait WriteContext {
    fn write(
        &mut self,
        access: &Path,
        value: Box<dyn Any + Send + Sync>,
    ) -> Result<(), ExpressionError>;
}

pub const fn fnv1a64(s: &str) -> u64 {
    let bytes = s.as_bytes();
    let mut hash = 0xcbf29ce484222325;
    let mut i = 0;
    while i < bytes.len() {
        hash ^= bytes[i] as u64;
        hash = hash.wrapping_mul(0x100000001b3);
        i += 1;
    }
    hash
}

#[cfg(test)]
mod tests {
    use super::fnv1a64;

    #[test]
    fn test_const_hash_known_vectors() {
        // FNV-1a 64-bit official test vectors
        assert_eq!(fnv1a64(""), 0xcbf29ce484222325);
        assert_eq!(fnv1a64("a"), 0xaf63dc4c8601ec8c);
        assert_eq!(fnv1a64("hello"), 0xa430d84680aabd0b);
        assert_eq!(fnv1a64("foobar"), 0x85944171f73967e8);
    }

    #[test]
    fn test_const_hash_compile_time() {
        const H: u64 = fnv1a64("The quick brown fox jumps over the lazy dog");
        assert_eq!(H, 0xf3f9b7f5e7e47110);
    }
}
