use std::{collections::hash_map::DefaultHasher, hash::Hasher};

use crate::{error::LangResult, ty::ty_env::TyEnv};

/// Trait to be implemented for items to be use in a `TyEnvHashMap` or `TyEnvHashSet`
pub trait TyEnvHash {
    fn hash(&self, ty_env: &TyEnv, deref_type: DerefType) -> LangResult<u64> {
        let mut state = DefaultHasher::new();
        self.hash_with_state(ty_env, deref_type, &mut state)?;
        Ok(state.finish())
    }

    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()>;
}

/// Used to specify how to handle nested `Ty`/`TypeId`s inside other types.
/// Ex. if they should be inferred to their inferred type before use or if the
/// "original" type should be used instead.
#[derive(Debug, Clone, Copy)]
pub enum DerefType {
    /// A deep hash type will use forwarded and inferred types when hashing any
    /// found `Ty`s or `TypeId`s.
    Deep,

    /// A deep hash type will NOT use forwarded and inferred types when hashing
    /// any found `Ty`s or `TypeId`s.
    Shallow,

    /// Used when it doesn't matter. This should only be used when it is known
    /// that the "thing" doesn't contain any nested types.
    None,
}
