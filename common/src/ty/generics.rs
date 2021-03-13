use std::{collections::HashMap, fmt::Display, hash::Hash};

use super::ty::Ty;

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

#[derive(Debug, Clone, Eq, Default)]
pub struct Generics {
    names: Vec<String>,
    types: Vec<Ty>,

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

    pub fn is_empty_types(&self) -> bool {
        self.types.is_empty()
    }

    pub fn get(&self, name: &str) -> Option<&Ty> {
        if let Some(idx) = self.lookup.get(name) {
            self.types.get(*idx)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Ty> {
        if let Some(idx) = self.lookup.get(name) {
            self.types.get_mut(*idx)
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
    pub fn insert(&mut self, name: String, ty: Ty) {
        if let Some(idx) = self.lookup.get(&name) {
            self.types[*idx] = ty;
        } else {
            let idx = self.names.len();
            self.lookup.insert(name.clone(), idx);

            self.names.push(name);
            self.types.push(ty);
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

    pub fn insert_type(&mut self, ty: Ty) {
        if !self.types.contains(&ty) {
            self.types.push(ty);
        }
    }

    pub fn iter_names(&self) -> std::slice::Iter<String> {
        self.names.iter()
    }

    pub fn iter_names_mut(&mut self) -> std::slice::IterMut<String> {
        self.names.iter_mut()
    }

    pub fn iter_types(&self) -> std::slice::Iter<Ty> {
        self.types.iter()
    }

    pub fn iter_types_mut(&mut self) -> std::slice::IterMut<Ty> {
        self.types.iter_mut()
    }

    pub fn is_solved(&self) -> bool {
        let mut solved = true;

        for ty in &self.types {
            if ty.contains_unknown_any() || ty.contains_generic() {
                solved = false;
                break;
            }
        }

        solved
    }
}

impl Hash for Generics {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for name in &self.names {
            name.hash(state);
        }
        for ty in &self.types {
            ty.hash(state);
        }
    }
}

impl PartialEq for Generics {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }

        if self
            .iter_names()
            .zip(other.iter_names())
            .any(|(a, b)| a != b)
        {
            return false;
        }

        if self
            .iter_types()
            .zip(other.iter_types())
            .any(|(a, b)| a != b)
        {
            return false;
        }

        true
    }
}

impl Display for Generics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = self
            .types
            .iter()
            .map(|ty| ty.to_string())
            .collect::<Vec<_>>()
            .join(",");

        write!(f, "<{}>", result)
    }
}
