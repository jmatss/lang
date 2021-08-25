use crate::{
    error::{LangError, LangErrorKind, LangResult},
    path::LangPath,
    ty::{inner_ty::InnerTy, ty::Ty},
    TypeId,
};

use super::{generics::Generics, get::get_inner, ty::SolveCond, ty_env::TyEnv};

/// Checks if the type referenced by the type ID `id` only contains solved types.
///
/// What counts as solved depends on the given `solve_cond` enum, see the
/// description of the `SolveCond` enum for more information about what/how
/// types are solvable and the different variants.
///
/// It first checks if the given `type_id` is solved. If that is not the case
/// and `check_inf` is set to true, it checks its inferred type for solvability
/// as well (unless `type_id` is the preferred type in its set).
/// The `check_inf` flag is only used for the first call to this function.
/// Any recursive calls inside tvhis funmction will set it to true.
///
/// Any "Unknown..." types that have their ADTs/arrays will be considered
/// solvable if `incl_unknowns` is set to true. This is needed since they
/// might not be possible to solve until all generics have been resolved
/// which is done at a later stage than the "normal" solving of the types.
pub fn is_solved(
    ty_env: &TyEnv,
    type_id: TypeId,
    check_inf: bool,
    solve_cond: SolveCond,
) -> LangResult<bool> {
    debug!(
        "is_solved -- type_id: {}, check_inf: {}, solve_cond: {:?}",
        type_id, check_inf, solve_cond
    );

    let is_solved_bool = match ty_env.ty(type_id)? {
        Ty::CompoundType(inner_ty, ..) => {
            let inner_solved = inner_ty.is_solved(solve_cond);

            let mut gens_solved = true;
            if let Some(gens) = inner_ty.gens() {
                for type_id in gens.iter_types() {
                    if !is_solved(ty_env, *type_id, check_inf, solve_cond)? {
                        gens_solved = false;
                    }
                }
            }

            inner_solved && gens_solved
        }

        Ty::Pointer(type_id_i, ..) => is_solved(ty_env, *type_id_i, true, solve_cond)?,

        Ty::Array(type_id_i, expr_opt, ..) => {
            let ty_solved = is_solved(ty_env, *type_id_i, check_inf, solve_cond)?;
            let expr_ty_solved = if let Some(expr_type_id) = expr_opt
                .as_ref()
                .map(|expr| expr.get_expr_type().ok())
                .flatten()
            {
                is_solved(ty_env, expr_type_id, true, solve_cond)?
            } else {
                true
            };
            ty_solved && expr_ty_solved
        }

        Ty::Fn(gens, params, ret_ty_id, ..) => {
            let ty_solved = if let Some(ret_ty_id) = ret_ty_id {
                is_solved(ty_env, *ret_ty_id, true, solve_cond)?
            } else {
                true
            };
            let mut gens_solved = true;
            for gen_type_id in gens {
                if !is_solved(ty_env, *gen_type_id, true, solve_cond)? {
                    gens_solved = false;
                }
            }
            let mut params_solved = true;
            for param_type_id in params {
                if !is_solved(ty_env, *param_type_id, true, solve_cond)? {
                    params_solved = false;
                }
            }
            ty_solved && gens_solved && params_solved
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type() {
                is_solved(ty_env, type_id_i, true, solve_cond)?
            } else {
                true
            }
        }

        Ty::GenericInstance(..) => solve_cond.can_solve_gen_inst(),
        Ty::Generic(..) => solve_cond.can_solve_gen(),
        Ty::Any(..) => true,

        Ty::UnknownAdtMember(type_id_i, ..)
        | Ty::UnknownFnArgument(Some(type_id_i), ..)
        | Ty::UnknownFnGeneric(Some(type_id_i), ..)
        | Ty::UnknownArrayMember(type_id_i, ..)
            if solve_cond.can_solve_unknown() =>
        {
            is_solved(ty_env, *type_id_i, true, solve_cond)?
        }

        Ty::UnknownAdtMethod(type_id_i, method_path, ..) if solve_cond.can_solve_unknown() => {
            let mut is_solved_bool = is_solved(ty_env, *type_id_i, true, solve_cond)?;
            if let Some(gens) = method_path.last().map(|part| part.generics()).unwrap() {
                for gen_type_id in gens.iter_types() {
                    if !is_solved(ty_env, *gen_type_id, true, solve_cond)? {
                        is_solved_bool = false;
                    }
                }
            }
            is_solved_bool
        }

        Ty::UnknownAdtMember(..)
        | Ty::UnknownFnArgument(..)
        | Ty::UnknownFnGeneric(..)
        | Ty::UnknownArrayMember(..)
        | Ty::UnknownAdtMethod(..) => false,
    };

    let inf_type_id = ty_env.inferred_type(type_id)?;
    if !check_inf || is_solved_bool || type_id == inf_type_id {
        Ok(is_solved_bool)
    } else {
        is_solved(ty_env, inf_type_id, true, solve_cond)
    }
}

pub fn is_adt(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_adt())
        .unwrap_or(false))
}

pub fn is_int(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_int())
        .unwrap_or(false))
}

pub fn is_float(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_float())
        .unwrap_or(false))
}

pub fn is_bool(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_bool())
        .unwrap_or(false))
}

pub fn is_char(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_char())
        .unwrap_or(false))
}

pub fn is_string(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_string())
        .unwrap_or(false))
}

pub fn is_primitive(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_primitive())
        .unwrap_or(false))
}

pub fn is_unknown(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_unknown())
        .unwrap_or(false))
}

pub fn is_unknown_ident(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_unknown_ident())
        .unwrap_or(false))
}

pub fn is_unknown_int(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_unknown_int())
        .unwrap_or(false))
}

pub fn is_unknown_float(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    Ok(get_inner(ty_env, id)
        .map(|inner_ty| inner_ty.is_unknown_float())
        .unwrap_or(false))
}

pub fn is_aggregate(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::CompoundType(..)))
}

pub fn is_pointer(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Pointer(..)))
}

pub fn is_array(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Array(..)))
}

pub fn is_fn(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Fn(..)))
}

pub fn is_expr(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Expr(..)))
}

pub fn is_any(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Any(..)))
}

pub fn is_generic(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::Generic(..) | Ty::GenericInstance(..)))
}

pub fn is_unknown_adt_member(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::UnknownAdtMember(..)))
}

pub fn is_unknown_adt_method(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::UnknownAdtMethod(..)))
}

pub fn is_unknown_method_argument(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::UnknownFnArgument(..)))
}

pub fn is_unknown_method_generic(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::UnknownFnGeneric(..)))
}

pub fn is_unknown_array_member(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::UnknownArrayMember(..)))
}

pub fn is_unknown_any(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(match ty {
        Ty::UnknownAdtMember(..)
        | Ty::UnknownAdtMethod(..)
        | Ty::UnknownFnArgument(..)
        | Ty::UnknownFnGeneric(..)
        | Ty::UnknownArrayMember(..) => true,
        Ty::CompoundType(inner_ty, ..) => {
            inner_ty.is_unknown()
                || inner_ty.is_unknown_ident()
                || inner_ty.is_unknown_int()
                || inner_ty.is_unknown_float()
        }
        _ => false,
    })
}

/// Checks if the given type is signed. This returns true if this is a signed
/// integer, returns false for every other type (includingn non-int types).
pub fn is_signed(ty_env: &TyEnv, id: TypeId) -> LangResult<bool> {
    let ty = ty_env.ty(id)?;
    Ok(matches!(ty, Ty::CompoundType(inner_ty, ..) if inner_ty.is_signed()))
}

pub fn assert_compatible(ty_env: &TyEnv, first_id: TypeId, second_id: TypeId) -> LangResult<()> {
    if is_compatible(ty_env, first_id, second_id)? {
        Ok(())
    } else {
        let first_ty = ty_env.ty(first_id)?;
        let second_ty = ty_env.ty(second_id)?;
        Err(LangError::new(
            format!(
                "Tried to map incompatible types.\nFirst:\n{:#?}\nSecond:\n{:#?}",
                first_ty, second_ty
            ),
            LangErrorKind::AnalyzeError,
            None,
        ))
    }
}

pub fn is_compatible(ty_env: &TyEnv, first_id: TypeId, second_id: TypeId) -> LangResult<bool> {
    debug!(
        "is_compatible -- first_id: {}, second_id: {}",
        first_id, second_id
    );

    let first_id = ty_env.forwarded(first_id);
    let second_id = ty_env.forwarded(second_id);

    // Handles all cases with "Unknown" types.
    if is_generic(ty_env, first_id)?
        || is_generic(ty_env, second_id)?
        || is_any(ty_env, first_id)?
        || is_any(ty_env, second_id)?
    {
        return Ok(true);
    } else if is_unknown_int(ty_env, first_id)? {
        if is_unknown_float(ty_env, second_id)? || is_unknown_ident(ty_env, second_id)? {
            return Ok(false);
        } else if is_int(ty_env, second_id)? || is_unknown_any(ty_env, second_id)? {
            return Ok(true);
        }
    } else if is_unknown_int(ty_env, second_id)? {
        if is_unknown_float(ty_env, first_id)? || is_unknown_ident(ty_env, first_id)? {
            return Ok(false);
        } else if is_int(ty_env, first_id)? || is_unknown_any(ty_env, first_id)? {
            return Ok(true);
        }
    } else if is_unknown_float(ty_env, first_id)? {
        if is_unknown_int(ty_env, second_id)? || is_unknown_ident(ty_env, second_id)? {
            return Ok(false);
        } else if is_float(ty_env, second_id)? || is_unknown_any(ty_env, second_id)? {
            return Ok(true);
        }
    } else if is_unknown_float(ty_env, second_id)? {
        if is_unknown_int(ty_env, first_id)? || is_unknown_ident(ty_env, first_id)? {
            return Ok(false);
        } else if is_float(ty_env, first_id)? || is_unknown_any(ty_env, first_id)? {
            return Ok(true);
        }
    } else if is_unknown_any(ty_env, first_id)? || is_unknown_any(ty_env, second_id)? {
        return Ok(true);
    } else if is_string(ty_env, first_id)? || is_string(ty_env, second_id)? {
        // Add support for compatibility with string and {u8}. This is what
        // the string will be compiled down to for now, but should be changed
        // to a custom String struct later.
        // This allows for easy interop with C string during development.
        match (ty_env.ty(first_id)?, ty_env.ty(second_id)?) {
            (Ty::Pointer(ptr_type_id, ..), _) | (_, Ty::Pointer(ptr_type_id, ..)) => {
                if let Ty::CompoundType(InnerTy::U8, ..) = ty_env.ty(*ptr_type_id)? {
                    return Ok(true);
                }
            }
            _ => (),
        }
    }

    // Handles all cases regarding types that isn't "Unknown" or generic.
    match (ty_env.ty(first_id)?, ty_env.ty(second_id)?) {
        (Ty::Pointer(inner_a, ..), Ty::Pointer(inner_b, ..))
        | (Ty::Array(inner_a, ..), Ty::Array(inner_b, ..)) => {
            is_compatible(ty_env, *inner_a, *inner_b)
        }

        (Ty::CompoundType(comp_a, ..), Ty::CompoundType(comp_b, ..)) => {
            is_compatible_inner_ty(ty_env, comp_a, comp_b)
        }

        (
            Ty::Fn(gens_a, params_a, ret_type_id_opt_a, ..),
            Ty::Fn(gens_b, params_b, ret_type_id_opt_b, ..),
        ) => {
            match (ret_type_id_opt_a, ret_type_id_opt_b) {
                (Some(ret_type_id_a), Some(ret_type_id_b)) => {
                    return is_compatible(ty_env, *ret_type_id_a, *ret_type_id_b);
                }
                (None, Some(_)) => return Ok(false),
                (Some(_), None) => return Ok(false),
                (None, None) => (),
            }

            if gens_a.len() != gens_b.len() {
                return Ok(false);
            }
            for (gen_type_id_a, gen_type_id_b) in gens_a.iter().zip(gens_b) {
                if !is_compatible(ty_env, *gen_type_id_a, *gen_type_id_b)? {
                    return Ok(false);
                }
            }

            if params_a.len() != params_b.len() {
                return Ok(false);
            }
            for (param_type_id_a, param_type_id_b) in params_a.iter().zip(params_b) {
                if !is_compatible(ty_env, *param_type_id_a, *param_type_id_b)? {
                    return Ok(false);
                }
            }

            Ok(true)
        }

        (Ty::Expr(expr_a, ..), Ty::Expr(expr_b, ..)) => {
            let type_id_a = expr_a.get_expr_type()?;
            let type_id_b = expr_b.get_expr_type()?;
            is_compatible(ty_env, type_id_a, type_id_b)
        }
        (Ty::Expr(expr, ..), other_ty) | (other_ty, Ty::Expr(expr, ..)) => {
            let expr_type_id = expr.get_expr_type()?;
            let other_type_id = ty_env.id_try(other_ty)?;
            is_compatible(ty_env, expr_type_id, other_type_id)
        }

        _ => Ok(false),
    }
}

#[allow(clippy::suspicious_operation_groupings)]
pub fn is_compatible_inner_ty(
    ty_env: &TyEnv,
    first_inner_ty: &InnerTy,
    second_inner_ty: &InnerTy,
) -> LangResult<bool> {
    if (first_inner_ty.is_unknown() || second_inner_ty.is_unknown())
        || (first_inner_ty.is_unknown_int() && second_inner_ty.is_int())
        || (second_inner_ty.is_unknown_int() && first_inner_ty.is_int())
        || (first_inner_ty.is_unknown_float() && second_inner_ty.is_float())
        || (second_inner_ty.is_unknown_float() && first_inner_ty.is_float())
    {
        return Ok(true);
    } else if (
                (first_inner_ty.is_adt() || first_inner_ty.is_trait() || first_inner_ty.is_unknown_ident())
                &&
                !(second_inner_ty.is_adt() || second_inner_ty.is_trait() || second_inner_ty.is_unknown_ident())
            )
            ||  // (this comment disables auto-formatting)
            (
                (second_inner_ty.is_adt() || second_inner_ty.is_trait() || second_inner_ty.is_unknown_ident())
                &&
                !(first_inner_ty.is_adt() || first_inner_ty.is_trait() || first_inner_ty.is_unknown_ident())
            )
    {
        return Ok(false);
    }

    match (first_inner_ty, second_inner_ty) {
        (InnerTy::Struct(path_a), InnerTy::Struct(path_b))
        | (InnerTy::Struct(path_a), InnerTy::UnknownIdent(path_b, ..))
        | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Struct(path_b, ..))
        | (InnerTy::Enum(path_a), InnerTy::Enum(path_b))
        | (InnerTy::Enum(path_a), InnerTy::UnknownIdent(path_b, ..))
        | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Enum(path_b, ..))
        | (InnerTy::Union(path_a), InnerTy::Union(path_b))
        | (InnerTy::Union(path_a), InnerTy::UnknownIdent(path_b, ..))
        | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Union(path_b, ..))
        | (InnerTy::Trait(path_a), InnerTy::Trait(path_b))
        | (InnerTy::Trait(path_a), InnerTy::UnknownIdent(path_b, ..))
        | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Trait(path_b, ..)) => {
            is_compatible_path(ty_env, path_a, path_b)
        }

        (InnerTy::Void, InnerTy::Void)
        | (InnerTy::Character, InnerTy::Character)
        | (InnerTy::String, InnerTy::String)
        | (InnerTy::Boolean, InnerTy::Boolean)
        | (InnerTy::I8, InnerTy::I8)
        | (InnerTy::U8, InnerTy::U8)
        | (InnerTy::I16, InnerTy::I16)
        | (InnerTy::U16, InnerTy::U16)
        | (InnerTy::I32, InnerTy::I32)
        | (InnerTy::U32, InnerTy::U32)
        | (InnerTy::F32, InnerTy::F32)
        | (InnerTy::I64, InnerTy::I64)
        | (InnerTy::U64, InnerTy::U64)
        | (InnerTy::F64, InnerTy::F64)
        | (InnerTy::I128, InnerTy::I128)
        | (InnerTy::U128, InnerTy::U128) => Ok(true),

        _ => Ok(false),
    }
}

pub fn is_compatible_path(
    ty_env: &TyEnv,
    first_path: &LangPath,
    second_path: &LangPath,
) -> LangResult<bool> {
    // There is a possiblity that one of the paths is prepended with the
    // module path while the other isn't. Do the comparison back to front so
    // that if one of the paths contains the module, that part will be skipped.
    for (first_part, second_part) in first_path
        .parts
        .iter()
        .rev()
        .zip(second_path.parts.iter().rev())
    {
        if first_part.name() != second_part.name() {
            return Ok(false);
        }

        // TODO: Is there a possiblity that one of the paths doesn't have the
        //       generics specified while the other one does at this point?
        match (first_part.generics(), second_part.generics()) {
            (Some(first_gens), Some(second_gens)) => {
                if !is_compatible_gens(ty_env, first_gens, second_gens)? {
                    return Ok(false);
                }
            }
            (Some(_), None) | (None, Some(_)) => return Ok(false),
            (None, None) => (),
        }
    }

    // The only path out of this function that returns `true`. Any earlier
    // return from this function will be `false`.
    Ok(true)
}

pub fn is_compatible_gens(
    ty_env: &TyEnv,
    first_gens: &Generics,
    second_gens: &Generics,
) -> LangResult<bool> {
    if first_gens.len_types() != second_gens.len_types() {
        return Ok(false);
    }

    for (first_name, second_name) in first_gens.iter_names().zip(second_gens.iter_names()) {
        if first_name != second_name {
            return Ok(false);
        }
    }

    for (first_id, second_id) in first_gens.iter_types().zip(second_gens.iter_types()) {
        if !is_compatible(ty_env, *first_id, *second_id)? {
            return Ok(false);
        }
    }

    // The only path out of this function that returns `true`. Any earlier
    // return from this function will be `false`.
    Ok(true)
}
