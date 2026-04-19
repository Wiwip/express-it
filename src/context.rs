use crate::expr::ExpressionError;
use core::fmt;
use std::any::Any;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

/// The human-readable, logical segment used when building ASTs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathSegment {
    /// Access a struct field (e.g., `.current_value`)
    Field(Cow<'static, str>),
    /// Access a map/dictionary key (e.g., `["fire_damage"]`)
    Key(Cow<'static, str>),
    /// Access a tuple/list index (e.g., `[0]`)
    Index(usize),
}

impl PathSegment {
    /// Helper for zero-allocation static field names
    pub fn field(name: &'static str) -> Self {
        PathSegment::Field(Cow::Borrowed(name))
    }

    /// Helper for zero-allocation static keys
    pub fn key(name: &'static str) -> Self {
        PathSegment::Key(Cow::Borrowed(name))
    }
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Struct fields get a dot prefix: .current_value
            PathSegment::Field(name) => write!(f, ".{}", name),
            // Map keys get bracket and quote wrapping: ["fire_damage"]
            PathSegment::Key(key) => write!(f, "[\"{}\"]", key),
            // Indices get brackets: [0]
            PathSegment::Index(idx) => write!(f, "[{}]", idx),
        }
    }
}

/// The human-readable path used by your user library to build expressions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub subject: SubjectId,
    pub root: Cow<'static, str>,
    pub segments: Vec<PathSegment>,
}

impl Path {
    /// Helper to create a new Path with just a root
    pub fn new(subject: SubjectId, root: impl Into<Cow<'static, str>>) -> Self {
        Self {
            subject,
            root: root.into(),
            segments: Vec::new(),
        }
    }

    /// Builder pattern helper to add a field segment
    pub fn with_field(mut self, name: &'static str) -> Self {
        self.segments.push(PathSegment::field(name));
        self
    }

    /// Builder pattern helper to add a key segment
    pub fn with_key(mut self, name: &'static str) -> Self {
        self.segments.push(PathSegment::key(name));
        self
    }

    /// Automatically uses the Rust type name as the root of the path
    pub fn from_type_name<T: 'static>(subject: impl Into<SubjectId>) -> Self {
        Self {
            subject: subject.into(),
            root: Cow::Borrowed(std::any::type_name::<T>()),
            segments: Vec::new(),
        }
    }

    /// Creates a new Path from a subject and a specific string name
    pub fn from_name(subject: impl Into<SubjectId>, name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            subject: subject.into(),
            root: name.into(),
            segments: Vec::new(),
        }
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Path(subject: {}, path: {}", self.subject.0, self.root)?;
        for segment in &self.segments {
            // PathSegment's Display impl handles the dots and brackets
            write!(f, "{}", segment)?;
        }
        write!(f, ")")
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SubjectId(pub u8);

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
