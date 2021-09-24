use std::collections::{
    hash_map::{Keys, Values, ValuesMut},
    HashMap,
};

use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    ty::ty_env::TyEnv,
};

/// A custom HashMap that looks up types in a `TyEnv` before performing the
/// hash operations.
pub struct TyEnvHashMap<K: TyEnvHash, V> {
    map: HashMap<u64, V>,
    key_hashes: HashMap<u64, K>,
}

impl<K: TyEnvHash, V> Default for TyEnvHashMap<K, V> {
    fn default() -> Self {
        TyEnvHashMap::new()
    }
}

impl<K: TyEnvHash, V> TyEnvHashMap<K, V> {
    pub fn new() -> Self {
        Self {
            map: HashMap::default(),
            key_hashes: HashMap::default(),
        }
    }

    pub fn contains_key(&self, ty_env: &TyEnv, deref_type: DerefType, k: &K) -> LangResult<bool> {
        let k_hash = k.hash(ty_env, deref_type)?;
        Ok(self.map.contains_key(&k_hash))
    }

    // TODO: Make this take an iterator as `other`.
    pub fn extend(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        other: &TyEnvHashMap<K, V>,
    ) -> LangResult<()> {
        todo!()
    }

    // TODO: Implement "entry" function. It would simplify/shorten a lot of code
    //       in the analyze/ty generic related logic.

    /// Removes all items in this map that does NOT match the predicate.
    pub fn retain<F>(&mut self, predicate: F)
    where
        F: Fn(&K, &V) -> bool,
    {
        if self.map.len() != self.key_hashes.len() {
            panic!(
                "TyEnvHashMap different sizes. map#: {}, key_hashes#: {}",
                self.map.len(),
                self.key_hashes.len()
            );
        }

        let hashes = self.key_hashes.keys().cloned().collect::<Vec<_>>();
        for hash in hashes {
            let key = self.key_hashes.get(&hash).unwrap();
            let value = self.map.get(&hash).unwrap();

            if !predicate(key, value) {
                self.map.remove(&hash);
                self.key_hashes.remove(&hash);
            }
        }
    }

    pub fn get(&self, ty_env: &TyEnv, deref_type: DerefType, k: &K) -> LangResult<Option<&V>> {
        let k_hash = k.hash(ty_env, deref_type)?;
        Ok(self.map.get(&k_hash))
    }

    pub fn get_mut(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        k: &K,
    ) -> LangResult<Option<&mut V>> {
        let k_hash = k.hash(ty_env, deref_type)?;
        Ok(self.map.get_mut(&k_hash))
    }

    pub fn keys(&self) -> Values<'_, u64, K> {
        self.key_hashes.values()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn values(&self) -> Values<'_, u64, V> {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_, u64, V> {
        self.map.values_mut()
    }

    pub fn insert(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        k: K,
        v: V,
    ) -> LangResult<Option<V>> {
        let k_hash = k.hash(ty_env, deref_type)?;
        self.key_hashes.insert(k_hash, k);
        Ok(self.map.insert(k_hash, v))
    }

    pub fn remove(
        &mut self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        k: &K,
    ) -> LangResult<Option<V>> {
        let k_hash = k.hash(ty_env, deref_type)?;
        self.key_hashes.remove(&k_hash);
        Ok(self.map.remove(&k_hash))
    }

    pub fn iter(&self) -> TyEnvHashMapIter<'_, K, V> {
        TyEnvHashMapIter {
            ty_env_hash_map: self,
            key_iter: self.key_hashes.keys(),
        }
    }
}

/// An iterator to iterate over the key-value pairs of a `TyEnvHashMap`.
pub struct TyEnvHashMapIter<'it, K: TyEnvHash, V> {
    ty_env_hash_map: &'it TyEnvHashMap<K, V>,
    key_iter: Keys<'it, u64, K>,
}

impl<'it, K: TyEnvHash, V> Iterator for TyEnvHashMapIter<'it, K, V> {
    type Item = (&'it K, &'it V);

    fn next(&mut self) -> Option<Self::Item> {
        let hash = self.key_iter.next()?;
        let key = self.ty_env_hash_map.key_hashes.get(hash)?;
        let value = self.ty_env_hash_map.map.get(hash)?;
        Some((key, value))
    }
}

impl<K, V> std::fmt::Debug for TyEnvHashMap<K, V>
where
    K: std::fmt::Debug + TyEnvHash,
    V: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "TyEnvHashMap {{")?;
        for (key_hash, key) in &self.key_hashes {
            if let Some(value) = self.map.get(key_hash) {
                writeln!(f, "key: {:#?}, value: {:#?}", key, value)?;
            } else {
                writeln!(f, "key: {:#?}, value: #INVALID#", key)?;
            }
        }
        write!(f, "}}")
    }
}
