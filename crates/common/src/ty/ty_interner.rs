use std::collections::{HashMap, HashSet};

use crate::{hash_map::TyEnvHashMap, TypeId};

use super::ty::Ty;

/// A struct containing all types. All code outside of this interner will only
/// store references to types (TypeId). To access the actual type, they will have
/// to fetch it from this interner.
///
/// Handling all types in this interner allows for easy updating of types during
/// type inference. The same type can be used in multiple places in the AST and
/// they all have to be updated/synchronized when the type is inferred to a new
/// type. This interner handles that synchronization and the AST only contains
/// references to the type in this interner, ensuring that the type only needs to
/// be updated in one place and is always up-to-date.
#[derive(Debug)]
pub struct TyInterner {
    pub(super) id_to_ty: HashMap<TypeId, Ty>,
    pub(super) ty_to_id: TyEnvHashMap<Ty, TypeId>,

    /// Contains a map of type IDs that should be "forwarded" to some other ID.
    /// This is use for example with GenericInstance's where multiple type IDs
    /// should map to the exact same instance of a type.
    /// The key is the type ID that should map to something else and the value
    /// is to which type it should map.
    pub(super) forwards: HashMap<TypeId, TypeId>,

    /// A counter that is used to create new `TypeId`s. It will be increment
    /// for every new type created. The type ID 0 is used for special purposes,
    /// so this value will always start counting from 1.
    pub(super) type_id: u64,

    /// A counter used to make "generic" unique IDs. This will be used for
    /// unknown and generic unique IDs.
    pub(super) unique_id: u64,
}

impl Default for TyInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TyInterner {
    pub fn new() -> Self {
        Self {
            id_to_ty: HashMap::default(),
            ty_to_id: TyEnvHashMap::default(),
            forwards: HashMap::default(),
            type_id: 1,
            unique_id: 1,
        }
    }

    pub fn all_types(&self) -> HashSet<TypeId> {
        self.id_to_ty.keys().cloned().collect::<HashSet<_>>()
    }
}
