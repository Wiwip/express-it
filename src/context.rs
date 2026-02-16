use crate::expr::ExpressionError;
use std::any::{Any, TypeId, type_name};
use std::fmt::{Debug, Formatter};
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub scope: ScopeId,
    pub path: u64,
}

impl Path {
    pub fn from_name(scope_id: impl Into<ScopeId>, name: &str) -> Self {
        Self {
            scope: scope_id.into(),
            path: hash_string(name),
        }
    }

    pub fn from_type<T: 'static>(scope_id: impl Into<ScopeId>) -> Self {
        Self {
            scope: scope_id.into(),
            path: hash_string(type_name::<T>()),
        }
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Path(scope: {}, path: {})", self.scope.0, self.path)
    }
}

impl Accessor for Path {
    fn scope(&self) -> ScopeId {
        self.scope
    }
    fn path(&self) -> u64 {
        self.path
    }
    fn key(&self) -> (ScopeId, u64) {
        (self.scope, self.path)
    }
}

pub trait ReadContext {
    fn get_any(&self, path: &dyn Accessor) -> Result<&dyn Any, ExpressionError>;
    fn get_any_component(&self, path: &str, type_id: TypeId) -> Result<&dyn Any, ExpressionError>;
}

pub trait WriteContext {
    fn write(&mut self, path: &dyn Accessor, value: Box<dyn Any>);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u8);

pub trait Accessor {
    fn scope(&self) -> ScopeId;
    fn path(&self) -> u64;
    fn key(&self) -> (ScopeId, u64);
}

fn hash_string(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
