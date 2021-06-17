use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    path::LangPath,
    ty::{generics::Generics, ty_env::TyEnv},
};

/// A helper function to compare two `LangPath`s.
///
/// The `TypeId`s will be used to lookup the corresponding `Ty`s in the TyInterner
/// and those `Ty`s will then be compared. This ensures that two different `TypeId`s
/// mapping to the same `Ty` will be seen as equal.
pub fn path_eq(
    ty_env: &TyEnv,
    path_a: &LangPath,
    path_b: &LangPath,
    deref_type: DerefType,
) -> LangResult<bool> {
    let hash_a = path_a.hash(ty_env, deref_type)?;
    let hash_b = path_b.hash(ty_env, deref_type)?;
    Ok(hash_a == hash_b)
}

/// A helper function to compare two `Generics`s.
///
/// The `TypeId`s will be used to lookup the corresponding `Ty`s in the TyInterner
/// and those `Ty`s will then be compared. This ensures that two different `TypeId`s
/// mapping to the same `Ty` will be seen as equal.
pub fn generics_eq(
    ty_env: &TyEnv,
    gens_a: &Generics,
    gens_b: &Generics,
    deref_type: DerefType,
) -> LangResult<bool> {
    let hash_a = gens_a.hash(ty_env, deref_type)?;
    let hash_b = gens_b.hash(ty_env, deref_type)?;
    Ok(hash_a == hash_b)
}
