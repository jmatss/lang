use std::{collections::HashMap, hash::Hash};

use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    TypeId,
};

use super::ty_env::TyEnv;

/// Used to indicate what kind this "Generics" is.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericsKind {
    /// Indicates that this "Generics" struct is a declaration of generics.
    /// Examples are declarations of generics on structs etc.
    ///   (<K, V>)
    Decl,

    /// Indicates that this "Generics" struct is a implementation of generics.
    /// This is the real types that will replace the declared generics.
    ///   (<&str, i32>)
    Impl,

    /// Is not supposed to have any generics so this struct should be empty.
    Empty,
}

#[derive(Debug, Clone, Default)]
pub struct Generics {
    pub names: Vec<String>,
    pub types: Vec<TypeId>,

    /// A map used to do fast lookups. The key is the name of the generic and
    /// the value is the index of that generic in both `names` and `types`.
    lookup: HashMap<String, usize>,
}

impl Generics {
    pub fn new() -> Self {
        Self {
            names: Vec::default(),
            types: Vec::default(),
            lookup: HashMap::default(),
        }
    }

    pub fn empty() -> Self {
        Self {
            names: Vec::with_capacity(0),
            types: Vec::with_capacity(0),
            lookup: HashMap::with_capacity(0),
        }
    }

    pub fn len(&self) -> usize {
        if self.names.is_empty() {
            self.types.len()
        } else if self.types.is_empty() {
            self.names.len()
        } else {
            (self.names.len() + self.types.len()) / 2
        }
    }

    pub fn len_names(&self) -> usize {
        self.names.len()
    }

    pub fn len_types(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty() && self.types.is_empty()
    }

    pub fn is_empty_names(&self) -> bool {
        self.names.is_empty()
    }

    pub fn is_empty_types(&self) -> bool {
        self.types.is_empty()
    }

    pub fn get(&self, name: &str) -> Option<TypeId> {
        if let Some(idx) = self.lookup.get(name) {
            self.types.get(*idx).copied()
        } else {
            None
        }
    }

    pub fn get_name(&self, idx: usize) -> Option<String> {
        self.names.get(idx).cloned()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.names.contains(&name.into())
    }

    /// Inserts a new generic type into this Generics. If the generic with the
    /// name `name` already has been set, this will change the type of that
    /// generic instead of creating a new entry.
    pub fn insert(&mut self, name: String, id: TypeId) {
        if let Some(idx) = self.lookup.get(&name) {
            self.types[*idx] = id;
        } else {
            let idx = self.names.len();
            self.lookup.insert(name.clone(), idx);

            self.names.push(name);
            self.types.push(id);
        }
    }

    pub fn insert_lookup(&mut self, name: String, idx: usize) {
        self.lookup.insert(name, idx);
    }

    pub fn insert_name(&mut self, name: String) {
        if !self.names.contains(&name) {
            self.names.push(name);
        }
    }

    pub fn insert_type(&mut self, id: TypeId) {
        self.types.push(id);
    }

    pub fn iter_names(&self) -> std::slice::Iter<String> {
        self.names.iter()
    }

    pub fn iter_names_mut(&mut self) -> std::slice::IterMut<String> {
        self.names.iter_mut()
    }

    pub fn iter_types(&self) -> std::slice::Iter<TypeId> {
        self.types.iter()
    }

    pub fn iter_types_mut(&mut self) -> std::slice::IterMut<TypeId> {
        self.types.iter_mut()
    }
}

impl TyEnvHash for Generics {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        if !matches!(deref_type, DerefType::None) {
            if !self.types.is_empty() {
                for gen_type_id in self.types.iter() {
                    gen_type_id.hash_with_state(ty_env, deref_type, state)?;
                }
            } else if !self.names.is_empty() {
                self.names.hash(state);
            }
        }
        Ok(())
    }
}
