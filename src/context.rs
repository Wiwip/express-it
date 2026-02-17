use crate::expr::ExpressionError;
use std::any::{Any, TypeId, type_name};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub scope: ScopeId,
    pub id: u64,
}

impl Path {
    pub fn from_name(scope_id: impl Into<ScopeId>, name: &str) -> Self {
        Self {
            scope: scope_id.into(),
            id: fnv1a64(name),
        }
    }

    pub fn from_type<T: 'static>(scope_id: impl Into<ScopeId>) -> Self {
        Self {
            scope: scope_id.into(),
            id: fnv1a64(type_name::<T>()),
        }
    }

    pub fn from_id(scope_id: impl Into<ScopeId>, id: u64) -> Self {
        Self {
            scope: scope_id.into(),
            id,
        }
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Path(scope: {}, path: {})", self.scope.0, self.id)
    }
}

impl Accessor for Path {
    fn scope(&self) -> ScopeId {
        self.scope
    }
    fn path(&self) -> u64 {
        self.id
    }
    fn key(&self) -> (ScopeId, u64) {
        (self.scope, self.id)
    }
}

pub trait ReadContext {
    fn get_any(& self, access: &dyn Accessor) -> Result<&dyn Any, ExpressionError>;
    fn get_any_component(&self, path: ScopeId, type_id: TypeId) -> Result<&dyn Any, ExpressionError>;
}

pub trait WriteContext {
    fn write(&mut self, access: &dyn Accessor, value: Box<dyn Any + Send + Sync>) -> Result<(), ExpressionError>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u8);

pub trait Accessor {
    fn scope(&self) -> ScopeId;
    fn path(&self) -> u64;
    fn key(&self) -> (ScopeId, u64);
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
