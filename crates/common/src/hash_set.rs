use std::collections::{hash_map::Values, HashMap};

use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    ty::ty_env::TyEnv,
};

/// A custom HashSet that looks up types in a `TyEnv` before performing the
/// hash operations.
#[derive(Clone)]
pub struct TyEnvHashSet<V: TyEnvHash + Clone> {
    map: HashMap<u64, V>,
}

impl<V: TyEnvHash + Clone> Default for TyEnvHashSet<V> {
    fn default() -> Self {
        TyEnvHashSet::new()
    }
}

impl<V: TyEnvHash + Clone> TyEnvHashSet<V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::default(),
        }
    }

    pub fn contains(&self, ty_env: &TyEnv, deref_type: DerefType, v: &V) -> LangResult<bool> {
        let v_hash = v.hash(ty_env, deref_type)?;
        Ok(self.map.contains_key(&v_hash))
    }

    // TODO: Make this take an iterator as `other`.
    pub fn extend(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        other: &TyEnvHashSet<V>,
    ) -> LangResult<()> {
        for value in other.values() {
            self.insert(ty_env, deref_type, value.clone())?;
        }
        Ok(())
    }

    pub fn get(&self, ty_env: &TyEnv, deref_type: DerefType, v: &V) -> LangResult<Option<&V>> {
        let v_hash = v.hash(ty_env, deref_type)?;
        Ok(self.map.get(&v_hash))
    }

    pub fn insert(&mut self, ty_env: &TyEnv, deref_type: DerefType, v: V) -> LangResult<Option<V>> {
        let v_hash = v.hash(ty_env, deref_type)?;
        Ok(self.map.insert(v_hash, v))
    }

    pub fn values(&self) -> Values<'_, u64, V> {
        self.map.values()
    }

    pub fn remove(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        v: &V,
    ) -> LangResult<Option<V>> {
        let v_hash = v.hash(ty_env, deref_type)?;
        Ok(self.map.remove(&v_hash))
    }
}

impl<V> std::fmt::Debug for TyEnvHashSet<V>
where
    V: std::fmt::Debug + TyEnvHash + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "# set map: {:#?}", self.map)
    }
}
