use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::{
    file::FilePosition,
    ty::generics::Generics,
    util::{self},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LangPathPart(pub String, pub Option<Generics>);

impl LangPathPart {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn generics(&self) -> &Option<Generics> {
        &self.1
    }
}

impl Display for LangPathPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(generics) = &self.1 {
            write!(f, "{}", util::to_generic_name(&self.0, generics))
        } else {
            write!(f, "{}", &self.0)
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub struct LangPath {
    /// Contains all parts of the path in order.
    parts: Vec<LangPathPart>,

    /// Indicates if this path is resolved or not. This means that the path is
    /// a "full" path that for example isn't a partial path that needs to be
    /// merged with a "use" path to make it correct.
    ///
    /// For static methods in a struct, the path might contain the struct just
    /// after the path has been parsed. This is incorrect and the struct will
    /// be moved/removed from the path at a later stage. As long as this might
    /// be the case, this flag will be set to false.
    ///
    /// When a path first is created, this is set to false no matter if the path
    /// is full or not.
    resolved: bool,

    file_pos: Option<FilePosition>,
}

impl LangPath {
    const SEP: &'static str = "::";

    pub fn new(parts: Vec<LangPathPart>, file_pos: Option<FilePosition>) -> Self {
        Self {
            parts,
            resolved: false,
            file_pos,
        }
    }

    pub fn set_resolved(&mut self, resolved: bool) {
        self.resolved = resolved;
    }

    pub fn is_resolved(&self) -> bool {
        self.resolved
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    /// Removes the last "part" of the path from this LangPath and returns it.
    pub fn pop(&mut self) -> Option<LangPathPart> {
        self.parts.pop()
    }

    pub fn push(&mut self, part: LangPathPart) {
        self.parts.push(part);
    }

    pub fn first(&self) -> Option<&LangPathPart> {
        self.parts.first()
    }

    pub fn last(&self) -> Option<&LangPathPart> {
        self.parts.last()
    }

    pub fn count(&self) -> usize {
        self.parts.len()
    }

    /// Clones `self` and returns a new LangPath where the `name`/`generics`
    /// have been appended as a part.
    pub fn clone_push(&self, name: &str, generics: Option<&Generics>) -> LangPath {
        let mut new_path = self.clone();
        let new_part = LangPathPart(name.into(), generics.cloned());
        new_path.parts.push(new_part);
        new_path
    }

    /// Returns the "full name" of this path. This is all path parts seperated
    /// by double colons (::).
    pub fn full_name(&self) -> String {
        self.parts
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(LangPath::SEP)
    }

    pub fn file_pos(&self) -> Option<&FilePosition> {
        self.file_pos.as_ref()
    }

    pub fn file_pos_mut(&mut self) -> Option<&mut FilePosition> {
        self.file_pos.as_mut()
    }

    /// Creates a new LangPath by appending `other` to the end of the `self` path.
    /// The new path will be set to `resolved`.
    pub fn join(&self, other: &LangPath, file_pos: Option<FilePosition>) -> LangPath {
        let mut parts = self.parts.clone();
        parts.extend(other.parts.iter().cloned());

        let mut new_path = LangPath::new(parts, file_pos);
        new_path.set_resolved(true);
        new_path
    }
}

impl PartialEq for LangPath {
    fn eq(&self, other: &Self) -> bool {
        self.full_name() == other.full_name()
    }
}

impl Hash for LangPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.full_name().hash(state);
    }
}

impl Default for LangPath {
    fn default() -> Self {
        LangPath::new(Vec::default(), None)
    }
}

impl Display for LangPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.full_name())
    }
}

impl From<LangPathPart> for LangPath {
    fn from(part: LangPathPart) -> Self {
        LangPath::new(vec![part], None)
    }
}

impl From<Vec<LangPathPart>> for LangPath {
    fn from(parts: Vec<LangPathPart>) -> Self {
        LangPath::new(parts, None)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct LangPathBuilder {
    parts: Vec<LangPathPart>,
    file_pos: Option<FilePosition>,
}

impl LangPathBuilder {
    pub fn new() -> Self {
        LangPathBuilder::default()
    }

    /// Checks if this path builder are empty. This means that no path parts have
    /// been added to this builder.
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn count(&self) -> usize {
        self.parts.len()
    }

    pub fn add_path(&mut self, s: &str) -> &mut Self {
        self.parts.push(LangPathPart(s.into(), None));
        self
    }

    pub fn add_path_gen(&mut self, s: &str, generics: &Generics) -> &mut Self {
        self.parts
            .push(LangPathPart(s.into(), Some(generics.clone())));
        self
    }

    pub fn file_pos(&mut self, file_pos: FilePosition) -> &mut Self {
        self.file_pos = Some(file_pos);
        self
    }

    pub fn build(self) -> LangPath {
        LangPath::new(self.parts, self.file_pos)
    }
}
