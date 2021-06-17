use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
};

use super::ty_env::TyEnv;

/// A unique ID used to reference specific types. All cmp/eq function only looks
/// at the unique ID, the scope information is ignored.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct TypeId(pub u64);

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TyEnvHash for TypeId {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        let type_id = match deref_type {
            DerefType::Deep => {
                let fwd_type_id = ty_env.forwarded(*self);
                ty_env.inferred_type(fwd_type_id)?
            }
            DerefType::Shallow => *self,
            DerefType::None => unreachable!("type_id: {}", self),
        };

        let ty = ty_env.ty(type_id)?;
        ty.hash_with_state(ty_env, deref_type, state)
    }
}

impl TyEnvHash for &[TypeId] {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        for type_id in self.iter() {
            type_id.hash_with_state(ty_env, deref_type, state)?;
        }
        Ok(())
    }
}
