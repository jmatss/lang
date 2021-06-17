use std::sync::Mutex;

use crate::{
    ctx::ast_ctx::AstCtx,
    eq::path_eq,
    error::LangResult,
    hash::DerefType,
    path::LangPath,
    ty::{
        solve::{inferred_type, solve},
        ty::Ty,
    },
    TypeId,
};

use super::{generics::Generics, inner_ty::InnerTy, ty_env::TyEnv};

/// Recursively replaces any generic identifiers from "UnknownIdent" wrapped
/// inside a "CompoundType" into "Generic"s.
pub fn replace_gens(ty_env: &mut TyEnv, id: TypeId, generics: &Generics) -> LangResult<()> {
    match ty_env.ty(id)?.clone() {
        Ty::CompoundType(InnerTy::UnknownIdent(path, ..), gens, type_info) => {
            for gen_type_id in gens.iter_types() {
                replace_gens(ty_env, *gen_type_id, generics)?;
            }

            if path.count() == 1 {
                let possible_gen_name = path.first().unwrap().name();
                for gen_name in generics.iter_names() {
                    if gen_name == possible_gen_name {
                        let new_ty = Ty::Generic(
                            possible_gen_name.into(),
                            ty_env.new_unique_id(),
                            type_info.clone(),
                        );
                        ty_env.update(id, new_ty)?;
                    }
                }
            }
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownMethodArgument(type_id, ..)
        | Ty::UnknownMethodGeneric(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            replace_gens(ty_env, type_id, generics)?;
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                replace_gens(ty_env, ret_type_id, generics)?;
            }
            for gen_type_id in gens.iter() {
                replace_gens(ty_env, *gen_type_id, generics)?;
            }
            for param_type_id in params.iter() {
                replace_gens(ty_env, *param_type_id, generics)?;
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                replace_gens(ty_env, type_id, generics)?;
            }
        }

        _ => (),
    }

    Ok(())
}

/// Recursively replaces any "Generic" types with the actual implementation
/// type. I.e. any "Generic" type that has a ident that is a key in the
/// `generics_impl` will be replaced with the value in the map.
///
/// The returned value indicates if the given type stored in `type_id` have
/// been updated. If that is the case, a new type ID representing the new
/// type will be returned. If the type haven't been updated/modified, None
/// is returned.
///
/// This function will recursively create new type IDs for all the types that
/// contains nested generics. This is needed to not modify the already existing
/// types in the type environment when replace the generics.
/// When a new type is inserted/created, the type will be solved and then
/// inferred before being returned.
pub fn replace_gen_impls(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    generics_impl: &Generics,
) -> LangResult<Option<TypeId>> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "replace_gen_impls -- type_id: {}, ty: {:#?} generics_impl: {:#?}",
        type_id, ty_clone, generics_impl
    );

    let ty_was_updated = match &mut ty_clone {
        Ty::Generic(ident, ..) => {
            if let Some(impl_type_id) = generics_impl.get(ident) {
                ty_clone = ty_env.lock().unwrap().ty(impl_type_id)?.clone();
                true
            } else {
                false
            }
        }

        Ty::GenericInstance(ident, unique_id, ..) => {
            if let Some(impl_type_id) = generics_impl.get(ident) {
                let ty_env_lock = ty_env.lock().unwrap();
                match ty_env_lock.ty(impl_type_id)?.clone() {
                    Ty::Generic(..) => false,
                    Ty::GenericInstance(_, impl_unique_id, ..) if impl_unique_id == *unique_id => {
                        false
                    }
                    _ => {
                        ty_clone = ty_env_lock.ty(impl_type_id)?.clone();
                        true
                    }
                }
            } else {
                false
            }
        }

        // Need to update generics both in the type `gens_clone` and the
        // generics declared inside a potential LangPath of the InnerTy.
        Ty::CompoundType(inner_ty, gens_clone, ..) => {
            let mut was_updated = false;

            for gen_type_id in gens_clone.iter_types_mut() {
                if let Some(new_gen_type_id) =
                    replace_gen_impls(ty_env, ast_ctx, *gen_type_id, generics_impl)?
                {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }

            if let Some(path) = inner_ty.get_ident_mut() {
                if let Some(inner_gens_clune) =
                    path.last_mut().map(|part| part.1.as_mut()).flatten()
                {
                    for gen_type_id in inner_gens_clune.iter_types_mut() {
                        if let Some(new_gen_type_id) =
                            replace_gen_impls(ty_env, ast_ctx, *gen_type_id, generics_impl)?
                        {
                            *gen_type_id = new_gen_type_id;
                            was_updated = true;
                        }
                    }
                }
            }

            was_updated
        }

        Ty::Pointer(type_id_i, ..)
        | Ty::Array(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownMethodArgument(type_id_i, ..)
        | Ty::UnknownMethodGeneric(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            if let Some(new_type_id_i) =
                replace_gen_impls(ty_env, ast_ctx, *type_id_i, generics_impl)?
            {
                *type_id_i = new_type_id_i;
                true
            } else {
                false
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            let mut was_updated = false;
            if let Some(ret_type_id) = ret_type_id_opt {
                if let Some(new_ret_type_id) =
                    replace_gen_impls(ty_env, ast_ctx, *ret_type_id, generics_impl)?
                {
                    *ret_type_id = new_ret_type_id;
                    was_updated = true;
                }
            }
            for gen_type_id in gens.iter_mut() {
                if let Some(new_gen_type_id) =
                    replace_gen_impls(ty_env, ast_ctx, *gen_type_id, generics_impl)?
                {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }
            for param_type_id in params.iter_mut() {
                if let Some(new_param_type_id) =
                    replace_gen_impls(ty_env, ast_ctx, *param_type_id, generics_impl)?
                {
                    *param_type_id = new_param_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type_mut() {
                if let Some(new_type_id_i) =
                    replace_gen_impls(ty_env, ast_ctx, *type_id_i, generics_impl)?
                {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }

        Ty::Any(..) => false,
    };

    // If any nested type was updated, the `ty_clone` will have been updated
    // and represents the new type for the current `type_id`. Insert it into
    // the type environment (if it is a new unique type) and return the new
    // type ID to indicate to the caller that the type have changed.
    Ok(if ty_was_updated {
        let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
        solve(ty_env, ast_ctx, new_type_id)?;
        Some(inferred_type(&ty_env.lock().unwrap(), new_type_id)?)
    } else {
        None
    })
}

/// Recursively replaces any structure types with idents that matches the
/// old structure name. These will be replaced with the new type with the
/// generics "replaced"/"implemented".
///
/// The returned value indicates if the given type stored in `type_id` have
/// been updated. If that is the case, a new type ID representing the new
/// type will be returned. If the type haven't been updated/modified, None
/// is returned.
///
/// This function will recursively create new type IDs for all the types that
/// contains nested generics. This is needed to not modify the already existing
/// types in the type environment when replace the generics.
pub fn replace_self(
    ty_env: &mut TyEnv,
    type_id: TypeId,
    old_path: &LangPath,
    new_self_type_id: TypeId,
) -> LangResult<Option<TypeId>> {
    debug!(
        "replace_self -- type_id: {}, old_path: {:?}, new_self_type_id: {}",
        type_id, old_path, new_self_type_id
    );

    let mut ty_clone = ty_env.ty(type_id)?.clone();

    let ty_was_updated = match &mut ty_clone {
        Ty::CompoundType(inner_ty, ..) => match inner_ty {
            InnerTy::Struct(path)
            | InnerTy::Enum(path)
            | InnerTy::Union(path)
            | InnerTy::Trait(path)
            | InnerTy::UnknownIdent(path, ..) => {
                if path_eq(ty_env, path, old_path, DerefType::Deep)? {
                    ty_clone = ty_env.ty(new_self_type_id)?.clone();
                    true
                } else {
                    false
                }
            }
            _ => false,
        },

        Ty::Pointer(type_id_i, ..)
        | Ty::Array(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownMethodArgument(type_id_i, ..)
        | Ty::UnknownMethodGeneric(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            if let Some(new_type_id) = replace_self(ty_env, *type_id_i, old_path, new_self_type_id)?
            {
                *type_id_i = new_type_id;
                true
            } else {
                false
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            let mut was_updated = false;
            if let Some(ret_type_id) = ret_type_id_opt {
                if let Some(new_type_id) =
                    replace_self(ty_env, *ret_type_id, old_path, new_self_type_id)?
                {
                    *ret_type_id = new_type_id;
                    was_updated = true;
                }
            }
            for gen_type_id in gens.iter_mut() {
                if let Some(new_type_id) =
                    replace_self(ty_env, *gen_type_id, old_path, new_self_type_id)?
                {
                    *gen_type_id = new_type_id;
                    was_updated = true;
                }
            }
            for param_type_id in params.iter_mut() {
                if let Some(new_type_id) =
                    replace_self(ty_env, *param_type_id, old_path, new_self_type_id)?
                {
                    *param_type_id = new_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type_mut() {
                if let Some(new_type_id) =
                    replace_self(ty_env, *type_id_i, old_path, new_self_type_id)?
                {
                    *type_id_i = new_type_id;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }

        _ => false,
    };

    // If any nested type was updated, the `ty_clone` will have been updated
    // and represents the new type for the current `type_id`. Insert it into
    // the type environment (if it is a new unique type) and return the new
    // type ID to indicate to the caller that the type have changed.
    Ok(if ty_was_updated {
        Some(ty_env.id(&ty_clone)?)
    } else {
        None
    })
}

/// Recursively replaces any "Generic" types with newly created "GenericInstance"s.
///
/// This is done to prevent any "Generic"s to leak out of function parameters
/// or return values. The "Generic" will be shared while the "GenericInstance"s
/// created will be unique.
///
/// The returned value indicates if the given type stored in `type_id` have
/// been updated. If that is the case, a new type ID representing the new
/// type will be returned. If the type haven't been updated/modified, None
/// is returned.
///
/// The given `unique_id` is the value that will be assigned to the newly
/// created "GenericInstance"s to make them "unique". To ensure that
/// duplicates aren't created every time a sepcific type is solved, the
/// unique ID will be given as a argument so that the uniqueness can be
/// decided by the caller instead of a new generic instance being created for
/// every call to this function.
pub fn replace_gens_with_gen_instances(
    ty_env: &mut TyEnv,
    type_id: TypeId,
    unique_id: u64,
) -> LangResult<Option<TypeId>> {
    debug!(
        "replace_gens_with_gen_instances -- type_id: {}, unique_id: {}",
        type_id, unique_id
    );

    let mut ty_clone = ty_env.ty(type_id)?.clone();

    let ty_was_updated = match &mut ty_clone {
        Ty::Generic(ident, ..) => {
            let new_gen_inst = ty_env.id(&Ty::GenericInstance(
                ident.clone(),
                unique_id,
                ty_clone.type_info().clone(),
            ))?;

            ty_clone = ty_env.ty(new_gen_inst)?.clone();
            true
        }

        Ty::CompoundType(_, gens_clone, ..) => {
            let mut was_updated = false;
            for gen_type_id in gens_clone.iter_types_mut() {
                if let Some(new_gen_type_id) =
                    replace_gens_with_gen_instances(ty_env, *gen_type_id, unique_id)?
                {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Pointer(type_id_i, ..)
        | Ty::Array(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownMethodArgument(type_id_i, ..)
        | Ty::UnknownMethodGeneric(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            if let Some(new_type_id_i) =
                replace_gens_with_gen_instances(ty_env, *type_id_i, unique_id)?
            {
                *type_id_i = new_type_id_i;
                true
            } else {
                false
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            let mut was_updated = false;
            if let Some(ret_type_id) = ret_type_id_opt {
                if let Some(new_ret_type_id) =
                    replace_gens_with_gen_instances(ty_env, *ret_type_id, unique_id)?
                {
                    *ret_type_id = new_ret_type_id;
                    was_updated = true;
                }
            }
            for gen_type_id in gens.iter_mut() {
                if let Some(new_gen_type_id) =
                    replace_gens_with_gen_instances(ty_env, *gen_type_id, unique_id)?
                {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }
            for param_type_id in params.iter_mut() {
                if let Some(new_param_type_id) =
                    replace_gens_with_gen_instances(ty_env, *param_type_id, unique_id)?
                {
                    *param_type_id = new_param_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type_mut() {
                if let Some(new_type_id_i) =
                    replace_gens_with_gen_instances(ty_env, *type_id_i, unique_id)?
                {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }

        Ty::GenericInstance(..) | Ty::Any(..) => false,
    };

    // If any nested type was updated, the `ty_clone` will have been updated
    // and represents the new type for the current `type_id`. Insert it into
    // the type environment (if it is a new unique type) and return the new
    // type ID to indicate to the caller that the type have changed.
    Ok(if ty_was_updated {
        Some(ty_env.id(&ty_clone)?)
    } else {
        None
    })
}

/// Recursively replaces any found unique IDs in the given type.
///
/// This can be done when creating a "copy" of the type so that the two types
/// doesn't have types mapping to eachother because they contain the same
/// unique ID.
pub fn replace_unique_ids(ty_env: &mut TyEnv, type_id: TypeId) -> LangResult<Option<TypeId>> {
    debug!("replace_unique_ids -- type_id: {},", type_id);

    let mut ty_clone = ty_env.ty(type_id)?.clone();

    let ty_was_updated = match &mut ty_clone {
        Ty::GenericInstance(_, unique_id, ..)
        | Ty::Any(unique_id, ..)
        | Ty::Generic(_, unique_id, ..) => {
            *unique_id = ty_env.new_unique_id();
            true
        }

        Ty::UnknownAdtMember(type_id_i, _, unique_id, ..)
        | Ty::UnknownAdtMethod(type_id_i, _, _, unique_id, ..)
        | Ty::UnknownMethodArgument(type_id_i, _, _, _, unique_id, ..)
        | Ty::UnknownMethodGeneric(type_id_i, _, _, unique_id, ..)
        | Ty::UnknownArrayMember(type_id_i, unique_id, ..) => {
            if let Some(new_type_id_i) = replace_unique_ids(ty_env, *type_id_i)? {
                *type_id_i = new_type_id_i;
            }

            *unique_id = ty_env.new_unique_id();
            true
        }

        Ty::CompoundType(_, gens_clone, ..) => {
            let mut was_updated = false;
            for gen_type_id in gens_clone.iter_types_mut() {
                if let Some(new_gen_type_id) = replace_unique_ids(ty_env, *gen_type_id)? {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
            if let Some(new_type_id_i) = replace_unique_ids(ty_env, *type_id_i)? {
                *type_id_i = new_type_id_i;
                true
            } else {
                false
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            let mut was_updated = false;
            if let Some(ret_type_id) = ret_type_id_opt {
                if let Some(new_ret_type_id) = replace_unique_ids(ty_env, *ret_type_id)? {
                    *ret_type_id = new_ret_type_id;
                    was_updated = true;
                }
            }
            for gen_type_id in gens.iter_mut() {
                if let Some(new_gen_type_id) = replace_unique_ids(ty_env, *gen_type_id)? {
                    *gen_type_id = new_gen_type_id;
                    was_updated = true;
                }
            }
            for param_type_id in params.iter_mut() {
                if let Some(new_param_type_id) = replace_unique_ids(ty_env, *param_type_id)? {
                    *param_type_id = new_param_type_id;
                    was_updated = true;
                }
            }
            was_updated
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type_mut() {
                if let Some(new_type_id_i) = replace_unique_ids(ty_env, *type_id_i)? {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
    };

    // If any nested type was updated, the `ty_clone` will have been updated
    // and represents the new type for the current `type_id`. Insert it into
    // the type environment (if it is a new unique type) and return the new
    // type ID to indicate to the caller that the type have changed.
    Ok(if ty_was_updated {
        Some(ty_env.id(&ty_clone)?)
    } else {
        None
    })
}

/// Iterates through all types in the type environment. For any type that
/// is the inferred type of its substitution set and is a type that currently
/// isn't known but can be turned into a default type, convert it to the
/// default type.
pub fn convert_defaults(ty_env: &Mutex<TyEnv>) -> LangResult<()> {
    let all_types = ty_env.lock().unwrap().interner.all_types();
    for type_id in all_types {
        let inf_type_id = inferred_type(&ty_env.lock().unwrap(), type_id)?;
        if type_id == inf_type_id {
            convert_default(ty_env, type_id)?;
        }
    }
    Ok(())
}

/// Converts any unknown values to their corresponding "default" values
/// if possible. This includes ints and floats that are converted to i32
/// and f32 respectively.
pub fn convert_default(ty_env: &Mutex<TyEnv>, type_id: TypeId) -> LangResult<()> {
    let inf_type_id = inferred_type(&ty_env.lock().unwrap(), type_id)?;

    let ty_clone = ty_env.lock().unwrap().ty_clone(inf_type_id)?;
    match ty_clone {
        Ty::CompoundType(inner_ty, generics, ..) => {
            if inner_ty.is_unknown_int() || inner_ty.is_unknown_float() {
                replace_default(&mut ty_env.lock().unwrap(), inf_type_id)?;
            }

            for gen_type_id in generics.iter_types() {
                convert_default(ty_env, *gen_type_id)?;
            }
        }

        Ty::Array(type_id_i, expr_opt, ..) => {
            convert_default(ty_env, type_id_i)?;

            if let Some(expr) = expr_opt {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    convert_default(ty_env, expr_type_id)?;
                }
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                convert_default(ty_env, ret_type_id)?;
            }
            for gen_type_id in gens {
                convert_default(ty_env, gen_type_id)?;
            }
            for param_type_id in params {
                convert_default(ty_env, param_type_id)?;
            }
        }

        Ty::Expr(expr, _) => {
            if let Ok(expr_type_id) = expr.get_expr_type() {
                convert_default(ty_env, expr_type_id)?;
            }
        }

        Ty::Pointer(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownMethodArgument(type_id_i, ..)
        | Ty::UnknownMethodGeneric(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            convert_default(ty_env, type_id_i)?;
        }

        Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
    }

    Ok(())
}

/// When this function is called, the type with ID `id` is known to be a
/// compound type containing either a int or float unknown.
fn replace_default(ty_env: &mut TyEnv, id: TypeId) -> LangResult<()> {
    if let Ty::CompoundType(inner_ty, ..) = ty_env.ty_mut(id)? {
        if inner_ty.is_unknown_int() {
            *inner_ty = InnerTy::default_int();
            debug!("Replaced type ID {} with default int.", id);
        } else if inner_ty.is_unknown_float() {
            *inner_ty = InnerTy::default_float();
            debug!("Replaced type ID {} with default float.", id);
        }
    }
    Ok(())
}
