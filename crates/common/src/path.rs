use std::{fmt::Debug, hash::Hash};

use crate::{
    error::LangResult,
    file::FilePosition,
    hash::{DerefType, TyEnvHash},
    ty::{generics::Generics, ty_env::TyEnv},
    BlockId,
};

#[derive(Debug, Clone)]
pub struct LangPath {
    /// Contains all parts of the path in order.
    pub(crate) parts: Vec<LangPathPart>,
    pub file_pos: Option<FilePosition>,
}

impl LangPath {
    pub fn new(parts: Vec<LangPathPart>, file_pos: Option<FilePosition>) -> Self {
        Self { parts, file_pos }
    }

    pub fn empty() -> Self {
        Self {
            parts: Vec::with_capacity(0),
            file_pos: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn without_gens(&self) -> LangPath {
        self.with_gens_opt(None)
    }

    pub fn with_gens(&self, generics: Generics) -> LangPath {
        self.with_gens_opt(Some(generics))
    }

    pub fn with_gens_opt(&self, generics: Option<Generics>) -> LangPath {
        let mut path = self.clone();
        let last_part = path.pop().unwrap();
        path.push(LangPathPart(last_part.0, generics));
        path
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

    pub fn last_mut(&mut self) -> Option<&mut LangPathPart> {
        self.parts.last_mut()
    }

    pub fn count(&self) -> usize {
        self.parts.len()
    }

    /// Clones `self` and returns a new LangPath where the `name`/`generics`
    /// have been appended as a part.
    pub fn clone_push(
        &self,
        name: &str,
        generics: Option<&Generics>,
        file_pos: Option<FilePosition>,
    ) -> LangPath {
        let mut new_path = self.clone();
        new_path.file_pos = file_pos;
        new_path
            .parts
            .push(LangPathPart(name.into(), generics.cloned()));
        new_path
    }

    pub fn file_pos(&self) -> Option<&FilePosition> {
        self.file_pos.as_ref()
    }

    pub fn file_pos_mut(&mut self) -> Option<&mut FilePosition> {
        self.file_pos.as_mut()
    }

    pub fn gens(&self) -> Option<&Generics> {
        self.last().map(|part| part.generics().as_ref()).flatten()
    }

    pub fn gens_mut(&mut self) -> Option<&mut Generics> {
        self.last_mut().map(|part| part.1.as_mut()).flatten()
    }

    /// Creates a new LangPath by appending `other` to the end of the `self` path.
    /// The new path will be set to `resolved`.
    pub fn join(&self, other: &LangPath, file_pos: Option<FilePosition>) -> LangPath {
        let mut parts = self.parts.clone();
        parts.extend(other.parts.iter().cloned());
        LangPath::new(parts, file_pos)
    }
}

impl Default for LangPath {
    fn default() -> Self {
        LangPath::new(Vec::default(), None)
    }
}

impl From<&str> for LangPath {
    fn from(part: &str) -> Self {
        LangPath::new(vec![part.into()], None)
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

impl<const N: usize> From<[LangPathPart; N]> for LangPath {
    fn from(parts: [LangPathPart; N]) -> Self {
        LangPath::new(parts.to_vec(), None)
    }
}

impl TyEnvHash for LangPath {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        for (i, part) in self.parts.iter().enumerate() {
            i.hash(state);
            part.hash_with_state(ty_env, deref_type, state)?;
        }
        Ok(())
    }
}

impl TyEnvHash for &LangPath {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        for (i, part) in self.parts.iter().enumerate() {
            i.hash(state);
            part.hash_with_state(ty_env, deref_type, state)?;
        }
        Ok(())
    }
}

impl TyEnvHash for (LangPath, BlockId) {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        self.0.hash_with_state(ty_env, deref_type, state)?;
        self.1.hash(state);
        Ok(())
    }
}

impl TyEnvHash for (LangPath, Option<String>) {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        self.0.hash_with_state(ty_env, deref_type, state)?;
        self.1.hash(state);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct LangPathPart(pub String, pub Option<Generics>);

impl LangPathPart {
    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn generics(&self) -> &Option<Generics> {
        &self.1
    }
}

impl From<&str> for LangPathPart {
    fn from(name: &str) -> Self {
        LangPathPart(name.into(), None)
    }
}

impl From<&String> for LangPathPart {
    fn from(name: &String) -> Self {
        LangPathPart(name.into(), None)
    }
}

impl From<String> for LangPathPart {
    fn from(name: String) -> Self {
        LangPathPart(name, None)
    }
}

impl TyEnvHash for LangPathPart {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        self.0.hash(state);
        // A empty list of generics and no generics at all is considered equals.
        if let Some(gens) = self.1.as_ref() {
            if !gens.is_empty_types() {
                gens.hash_with_state(ty_env, deref_type, state)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
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

    pub fn add_path_with_gen(&mut self, s: &str, gens: Option<&Generics>) -> &mut Self {
        self.parts.push(LangPathPart(s.into(), gens.cloned()));
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
