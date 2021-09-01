use std::collections::HashSet;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_set::TyEnvHashSet,
    path::LangPath,
    token::expr::Expr,
    TypeId,
};

use super::{
    generics::Generics,
    inner_ty::InnerTy,
    ty::{SolveCond, Ty},
    ty_env::TyEnv,
    type_info::TypeInfo,
};

pub fn get_inner(ty_env: &TyEnv, id: TypeId) -> LangResult<&InnerTy> {
    if let Ty::CompoundType(inner_ty, ..) = ty_env.ty(id)? {
        Ok(inner_ty)
    } else {
        Err(LangError::new(
            format!("Type with ID {} not a CompoundType.", id),
            LangErrorKind::GeneralError,
            None,
        ))
    }
}

pub fn get_inner_mut(ty_env: &mut TyEnv, id: TypeId) -> LangResult<&mut InnerTy> {
    if let Ty::CompoundType(inner_ty, ..) = ty_env.ty_mut(id)? {
        Ok(inner_ty)
    } else {
        Err(LangError::new(
            format!("Type with ID {} not a CompoundType.", id),
            LangErrorKind::GeneralError,
            None,
        ))
    }
}

pub fn get_generic_ident(ty_env: &TyEnv, id: TypeId) -> LangResult<&str> {
    match ty_env.ty(id)? {
        Ty::Generic(ident, ..) | Ty::GenericInstance(ident, ..) => Ok(ident),
        _ => Err(LangError::new(
            format!(
                "Type with ID {} not a Generic or GenericInstance: {:#?}",
                id,
                ty_env.ty(id)?
            ),
            LangErrorKind::GeneralError,
            None,
        )),
    }
}

/// Returns the identifier if this type represents a ADT.
/// If this type isn't a ADT, None is returned.
pub fn get_ident(ty_env: &TyEnv, id: TypeId) -> LangResult<Option<LangPath>> {
    let ty = ty_env.ty(id)?;
    match ty {
        Ty::CompoundType(inner_ty, ..) => Ok(inner_ty.get_ident()),
        _ => Err(LangError::new(
            format!("Type with ID {} not a CompoundType: {:#?}", id, ty),
            LangErrorKind::GeneralError,
            None,
        )),
    }
}

/// Returns the generics if this type represents an ADT with generics.
/// If this type isn't an ADT with generics, None is returned.
pub fn get_gens(ty_env: &TyEnv, id: TypeId) -> LangResult<Option<&Generics>> {
    match ty_env.ty(id)? {
        Ty::CompoundType(inner_ty, ..) => Ok(inner_ty.gens()),
        _ => Ok(None),
    }
}

pub fn get_type_info(ty_env: &TyEnv, id: TypeId) -> Option<&TypeInfo> {
    Some(ty_env.ty(id).ok()?.type_info())
}

pub fn get_type_info_mut(ty_env: &mut TyEnv, id: TypeId) -> Option<&mut TypeInfo> {
    Some(ty_env.ty_mut(id).ok()?.type_info_mut())
}

pub fn get_file_pos(ty_env: &TyEnv, id: TypeId) -> Option<&FilePosition> {
    ty_env.ty(id).ok()?.file_pos()
}

pub fn get_file_pos_mut(ty_env: &mut TyEnv, id: TypeId) -> Option<&mut FilePosition> {
    ty_env.ty_mut(id).ok()?.file_pos_mut()
}

// TODO: This only fetched a expression if it is the outer most type.
//       How should expression in ex. generics be handled? Should this
//       return a iterator or a list?
// TODO: Rewrite this is a safe way. Temporary hack to get something to work.
pub fn get_exprs_mut<'a, 'b>(ty_env: &'a mut TyEnv, id: TypeId) -> LangResult<Vec<&'b mut Expr>> {
    let mut exprs = Vec::default();

    match ty_env.ty(id)?.clone() {
        Ty::Expr(..) => {
            exprs.push(get_exprs_mut_priv(ty_env, id)?);
        }

        Ty::Array(type_id, dim_expr_opt, ..) => {
            let inner_exprs = get_exprs_mut(ty_env, type_id)?;
            for inner_expr in inner_exprs {
                exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
            }

            if dim_expr_opt.is_some() {
                exprs.push(get_exprs_mut_priv(ty_env, id)?);
            }
        }

        Ty::CompoundType(inner_ty, ..) => {
            if let Some(gens) = inner_ty.gens() {
                for gen_type_id in gens.iter_types() {
                    let inner_exprs = get_exprs_mut(ty_env, *gen_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }
            }
        }

        Ty::Pointer(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownFnArgument(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            let inner_exprs = get_exprs_mut(ty_env, type_id)?;
            for inner_expr in inner_exprs {
                exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
            }
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                let inner_exprs = get_exprs_mut(ty_env, ret_type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }
            }

            for gen_type_id in gens.iter() {
                let inner_exprs = get_exprs_mut(ty_env, *gen_type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }
            }

            for param_type_id in params.iter() {
                let inner_exprs = get_exprs_mut(ty_env, *param_type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }
            }
        }

        Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
    }

    Ok(exprs)
}

/// This function is called when the type with ID `id` is known to contain
/// a expression. This is true for: Ty::Expr & Ty::Array.
fn get_exprs_mut_priv<'a, 'b>(ty_env: &'a mut TyEnv, id: TypeId) -> LangResult<&'b mut Expr> {
    match ty_env.ty_mut(id)? {
        Ty::Expr(expr, ..) | Ty::Array(_, Some(expr), ..) => unsafe {
            Ok((expr.as_mut() as *mut Expr).as_mut().unwrap())
        },
        _ => Err(LangError::new(
            format!(
                "Unable to get mutable exprs from type: {:#?}",
                ty_env.ty_mut(id)?
            ),
            LangErrorKind::AnalyzeError,
            None,
        )),
    }
}

/// Gets a vector of all "Generic" types that is contained in the given
/// type `self`.
pub fn get_generics(ty_env: &TyEnv, id: TypeId) -> LangResult<Vec<TypeId>> {
    let mut generics = Vec::default();

    match ty_env.ty(id)? {
        Ty::Generic(..) => {
            generics.push(id);
        }

        Ty::CompoundType(inner_ty, ..) => {
            if let Some(gens) = inner_ty.gens() {
                for gen_type_id in gens.iter_types() {
                    let mut inner_generics = get_generics(ty_env, *gen_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownFnArgument(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            let mut inner_generics = get_generics(ty_env, *type_id)?;
            generics.append(&mut inner_generics);
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                let mut inner_generics = get_generics(ty_env, *ret_type_id)?;
                generics.append(&mut inner_generics);
            }
            for gen_type_id in gens {
                let mut inner_generics = get_generics(ty_env, *gen_type_id)?;
                generics.append(&mut inner_generics);
            }
            for param_type_id in params {
                let mut inner_generics = get_generics(ty_env, *param_type_id)?;
                generics.append(&mut inner_generics);
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(expr_type_id) = expr.get_expr_type() {
                let mut inner_generics = get_generics(ty_env, expr_type_id)?;
                generics.append(&mut inner_generics);
            }
        }

        _ => (),
    }

    Ok(generics)
}

/// Gets a set of all unsolvable types that is contained in the given type `self`.
/// Which types are counted as unsolvable depends on the given `solve_cond`.
/// `Any` types are always excluded.
pub fn get_unsolvable(
    ty_env: &TyEnv,
    type_id: TypeId,
    solve_cond: SolveCond,
) -> LangResult<HashSet<TypeId>> {
    let mut unsolvable = HashSet::default();

    match ty_env.ty(type_id)? {
        Ty::CompoundType(inner_ty, ..) => {
            if !inner_ty.is_solved(solve_cond) {
                unsolvable.insert(type_id);
            }
            if let Some(gens) = inner_ty.gens() {
                for gen_type_id in gens.iter_types() {
                    unsolvable.extend(get_unsolvable(ty_env, *gen_type_id, solve_cond)?);
                }
            }
        }

        Ty::Pointer(type_id_i, ..)
        | Ty::Array(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownFnArgument(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            unsolvable.extend(get_unsolvable(ty_env, *type_id_i, solve_cond)?);
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                unsolvable.extend(get_unsolvable(ty_env, *ret_type_id, solve_cond)?);
            }
            for gen_type_id in gens {
                unsolvable.extend(get_unsolvable(ty_env, *gen_type_id, solve_cond)?);
            }
            for param_type_id in params {
                unsolvable.extend(get_unsolvable(ty_env, *param_type_id, solve_cond)?);
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type() {
                unsolvable.extend(get_unsolvable(ty_env, type_id_i, solve_cond)?);
            }
        }

        Ty::Generic(..) => {
            if !solve_cond.can_solve_gen() {
                unsolvable.insert(type_id);
            }
        }
        Ty::GenericInstance(..) => {
            if !solve_cond.can_solve_gen_inst() {
                unsolvable.insert(type_id);
            }
        }

        Ty::Any(..) => (),
    }

    Ok(unsolvable)
}

/// Gets a set of the paths of all ADTs/traits that is contained in the type.
/// Set `full_paths` to true to fetch the names as full names, set to false
/// to just return the names without generics.
pub fn get_adt_and_trait_paths(
    ty_env: &TyEnv,
    id: TypeId,
    full_paths: bool,
) -> LangResult<TyEnvHashSet<LangPath>> {
    let mut paths = TyEnvHashSet::default();

    // TODO: Is this correct? Can we assume that when one wants to include the
    //       generics in the path (i.e. `full_paths` set to true), the generics
    //       should be fetched with `DerefType::Deep`?
    let deref_type = if full_paths {
        DerefType::Deep
    } else {
        DerefType::Shallow
    };

    match ty_env.ty(id)? {
        Ty::CompoundType(inner_ty, ..) => {
            if let Some(gens) = inner_ty.gens() {
                for gen_type_id in gens.iter_types() {
                    let inner_paths = get_adt_and_trait_paths(ty_env, *gen_type_id, full_paths)?;
                    paths.extend(ty_env, deref_type, &inner_paths)?;
                }
            }

            match inner_ty {
                InnerTy::Struct(path)
                | InnerTy::Enum(path)
                | InnerTy::Union(path)
                | InnerTy::Trait(path)
                | InnerTy::UnknownIdent(path, ..) => {
                    let path_clone = if full_paths {
                        path.clone()
                    } else {
                        path.without_gens()
                    };
                    paths.insert(ty_env, deref_type, path_clone)?;
                }
                _ => (),
            }
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownFnArgument(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            let inner_paths = get_adt_and_trait_paths(ty_env, *type_id, full_paths)?;
            paths.extend(ty_env, deref_type, &inner_paths)?;
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                let inner_paths = get_adt_and_trait_paths(ty_env, *ret_type_id, full_paths)?;
                paths.extend(ty_env, deref_type, &inner_paths)?;
            }
            for gen_type_id in gens {
                let inner_paths = get_adt_and_trait_paths(ty_env, *gen_type_id, full_paths)?;
                paths.extend(ty_env, deref_type, &inner_paths)?;
            }
            for param_type_id in params {
                let inner_paths = get_adt_and_trait_paths(ty_env, *param_type_id, full_paths)?;
                paths.extend(ty_env, deref_type, &inner_paths)?;
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                let inner_paths = get_adt_and_trait_paths(ty_env, type_id, full_paths)?;
                paths.extend(ty_env, deref_type, &inner_paths)?;
            }
        }

        _ => (),
    }

    Ok(paths)
}

/// Given a type ID `type_id`, returns all "nested" type IDs that it contains.
///
/// The set of all nested type IDs are given as a parameter instead of as a
/// return value to prevent infinite recursion. Those situations can happen
/// when ex. solving an array member which will check nested types for both
/// the member AND the "wrapping parent" array.
///
/// If `incl_inf` is set to true, this will include nested type IDs for any
/// inferred types as well.
pub fn get_nested_type_ids(
    ty_env: &TyEnv,
    all_nested_type_ids: &mut HashSet<TypeId>,
    type_id: TypeId,
    incl_inf: bool,
) -> LangResult<()> {
    if all_nested_type_ids.contains(&type_id) {
        return Ok(());
    } else {
        all_nested_type_ids.insert(type_id);
    }

    match ty_env.ty(type_id)? {
        Ty::CompoundType(inner_ty, ..) => {
            if let Some(gens) = inner_ty.gens() {
                for gen_type_id in gens.iter_types() {
                    get_nested_type_ids(ty_env, all_nested_type_ids, *gen_type_id, incl_inf)?;
                }
            }
        }

        Ty::Pointer(type_id_i, ..)
        | Ty::Array(type_id_i, ..)
        | Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownAdtMethod(type_id_i, ..)
        | Ty::UnknownFnArgument(type_id_i, ..)
        | Ty::UnknownArrayMember(type_id_i, ..) => {
            get_nested_type_ids(ty_env, all_nested_type_ids, *type_id_i, incl_inf)?;
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                get_nested_type_ids(ty_env, all_nested_type_ids, *ret_type_id, incl_inf)?;
            }
            for type_id_i in gens.iter().chain(params.iter()) {
                get_nested_type_ids(ty_env, all_nested_type_ids, *type_id_i, incl_inf)?;
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type() {
                get_nested_type_ids(ty_env, all_nested_type_ids, type_id_i, incl_inf)?;
            }
        }

        Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => (),
    }

    if incl_inf {
        let inf_type_id = ty_env.inferred_type(type_id)?;
        if type_id != inf_type_id {
            get_nested_type_ids(ty_env, all_nested_type_ids, inf_type_id, incl_inf)?;
        }
    }

    Ok(())
}
