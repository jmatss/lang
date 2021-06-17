use either::Either;

use crate::{error::LangResult, path::LangPath, TypeId};

use super::{inner_ty::InnerTy, ty::Ty, ty_env::TyEnv, type_info::TypeInfo};

/// Checks if the type with ID `type_id` contains generics with any of the
/// names found in `gen_names`. The function will check both Generic's and
/// GenericInstance's.
pub fn contains_generic_with_name(
    ty_env: &TyEnv,
    type_id: TypeId,
    gen_names: &[String],
) -> LangResult<bool> {
    Ok(match ty_env.ty(type_id)? {
        Ty::Generic(gen_name, ..) | Ty::GenericInstance(gen_name, ..) => {
            gen_names.contains(gen_name)
        }

        Ty::CompoundType(_, gens, ..) => {
            for type_id in gens.iter_types() {
                if contains_generic_with_name(ty_env, *type_id, gen_names)? {
                    return Ok(true);
                }
            }
            false
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownMethodArgument(type_id, ..)
        | Ty::UnknownMethodGeneric(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            contains_generic_with_name(ty_env, *type_id, gen_names)?
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                contains_generic_with_name(ty_env, type_id, gen_names)?
            } else {
                false
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            for type_id_i in gens.iter().chain(params) {
                if contains_generic_with_name(ty_env, *type_id_i, gen_names)? {
                    return Ok(true);
                }
            }

            if let Some(ret_type_id) = ret_type_id_opt {
                if contains_generic_with_name(ty_env, *ret_type_id, gen_names)? {
                    return Ok(true);
                }
            }

            false
        }

        Ty::Any(..) => false,
    })
}

/// Checks if the type `child_ty` can be found in type with ID `parent_id`.
/// This is a "shallow" check i.e. it doesn't check the contents of the type,
/// only that the "type" of the type is correct.
///
/// # Example
///
/// If this function is called with:
///
///  `parent_id` = CompoundType("abc")
///   &
///  `child_id` = CompoundType("xyz")
///
/// the function would return true even though "abc" != "xyz".
pub fn contains_ty_shallow(ty_env: &TyEnv, parent_id: TypeId, child_ty: &Ty) -> LangResult<bool> {
    let parent_ty = ty_env.ty(parent_id)?;

    match (parent_ty, child_ty) {
        (Ty::CompoundType(..), Ty::CompoundType(..))
        | (Ty::Pointer(..), Ty::Pointer(..))
        | (Ty::Array(..), Ty::Array(..))
        | (Ty::Fn(..), Ty::Fn(..))
        | (Ty::Expr(..), Ty::Expr(..))
        | (Ty::Generic(..), Ty::Generic(..))
        | (Ty::Any(..), Ty::Any(..))
        | (Ty::GenericInstance(..), Ty::GenericInstance(..))
        | (Ty::UnknownAdtMember(..), Ty::UnknownAdtMember(..))
        | (Ty::UnknownAdtMethod(..), Ty::UnknownAdtMethod(..))
        | (Ty::UnknownMethodArgument(..), Ty::UnknownMethodArgument(..))
        | (Ty::UnknownMethodGeneric(..), Ty::UnknownMethodGeneric(..))
        | (Ty::UnknownArrayMember(..), Ty::UnknownArrayMember(..)) => return Ok(true),
        _ => (),
    }

    Ok(match parent_ty {
        Ty::CompoundType(_, gens, _) => {
            let mut contains = false;
            for type_id in gens.iter_types() {
                if contains_ty_shallow(ty_env, *type_id, child_ty)? {
                    contains = true;
                    break;
                }
            }
            contains
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownMethodGeneric(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => contains_ty_shallow(ty_env, *type_id, child_ty)?,

        Ty::UnknownAdtMethod(type_id, _, gen_type_ids, ..)
        | Ty::UnknownMethodArgument(type_id, _, gen_type_ids, ..) => {
            let mut contains_ty = contains_ty_shallow(ty_env, *type_id, child_ty)?;
            for gen_type_id in gen_type_ids {
                if contains_ty_shallow(ty_env, *gen_type_id, child_ty)? {
                    contains_ty = true;
                }
            }
            contains_ty
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                contains_ty_shallow(ty_env, type_id, child_ty)?
            } else {
                false
            }
        }

        _ => false,
    })
}

fn contains_inner_ty_shallow(ty_env: &TyEnv, id: TypeId, inner_ty: &InnerTy) -> LangResult<bool> {
    match ty_env.ty(id)? {
        Ty::CompoundType(cur_inner_ty, gens, _) => {
            if cur_inner_ty.contains_inner_ty_shallow(inner_ty) {
                return Ok(true);
            }

            for gen_type_id in gens.iter_types() {
                if contains_inner_ty_shallow(ty_env, *gen_type_id, inner_ty)? {
                    return Ok(true);
                }
            }

            Ok(false)
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownMethodArgument(type_id, ..)
        | Ty::UnknownMethodGeneric(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            contains_inner_ty_shallow(ty_env, *type_id, inner_ty)
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                if contains_inner_ty_shallow(ty_env, *ret_type_id, inner_ty)? {
                    return Ok(true);
                }
            }
            for gen_type_id in gens {
                if contains_inner_ty_shallow(ty_env, *gen_type_id, inner_ty)? {
                    return Ok(true);
                }
            }
            for param_type_id in params {
                if contains_inner_ty_shallow(ty_env, *param_type_id, inner_ty)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                contains_inner_ty_shallow(ty_env, type_id, inner_ty)
            } else {
                Ok(false)
            }
        }

        Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(false),
    }
}

pub fn contains_generic_decl_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let gen_ty = Ty::Generic("".into(), 0, TypeInfo::None);
    contains_ty_shallow(ty_env, id, &gen_ty)
}

pub fn contains_generic_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let gen_ty = Ty::Generic("".into(), 0, TypeInfo::None);
    let gen_inst_ty = Ty::GenericInstance("".into(), 0, TypeInfo::None);
    Ok(contains_ty_shallow(ty_env, id, &gen_ty)? || contains_ty_shallow(ty_env, id, &gen_inst_ty)?)
}

pub fn contains_any_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let any_ty = Ty::Any(0, TypeInfo::None);
    contains_ty_shallow(ty_env, id, &any_ty)
}

pub fn contains_unknown_any_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let unknown_adt_member = Ty::UnknownAdtMember(TypeId(0), "".into(), 0, TypeInfo::None);
    let unknown_adt_method = Ty::UnknownAdtMethod(
        TypeId(0),
        "".into(),
        Vec::with_capacity(0),
        0,
        TypeInfo::None,
    );
    let unknown_method_argument = Ty::UnknownMethodArgument(
        TypeId(0),
        "".into(),
        Vec::with_capacity(0),
        Either::Right(0),
        0,
        TypeInfo::None,
    );
    let unknown_method_generic =
        Ty::UnknownMethodGeneric(TypeId(0), "".into(), Either::Left(0), 0, TypeInfo::None);
    let unknown_array_member = Ty::UnknownArrayMember(TypeId(0), 0, TypeInfo::None);

    Ok(contains_unknown_inner_shallow(ty_env, id)?
        || contains_unknown_ident_shallow(ty_env, id)?
        || contains_unknown_int_shallow(ty_env, id)?
        || contains_unknown_float_shallow(ty_env, id)?
        || contains_ty_shallow(ty_env, id, &unknown_adt_member)?
        || contains_ty_shallow(ty_env, id, &unknown_adt_method)?
        || contains_ty_shallow(ty_env, id, &unknown_method_argument)?
        || contains_ty_shallow(ty_env, id, &unknown_method_generic)?
        || contains_ty_shallow(ty_env, id, &unknown_array_member)?)
}

pub fn contains_unknown_inner_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    contains_inner_ty_shallow(ty_env, id, &InnerTy::Unknown(0))
}

pub fn contains_unknown_int_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    contains_inner_ty_shallow(ty_env, id, &InnerTy::UnknownInt(0, 0))
}

pub fn contains_unknown_float_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    contains_inner_ty_shallow(ty_env, id, &InnerTy::UnknownFloat(0))
}

pub fn contains_unknown_ident_shallow(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    contains_inner_ty_shallow(ty_env, id, &InnerTy::UnknownIdent(LangPath::empty(), 0))
}
