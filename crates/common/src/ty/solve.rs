use std::{collections::HashSet, sync::Mutex};

use either::Either;

use crate::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    error::LangResult,
    path::LangPath,
    token::block::Fn,
    ty::{
        generics::Generics,
        get::get_file_pos,
        inner_ty::InnerTy,
        is::{is_any, is_generic, is_solved},
        replace::{replace_gen_impls, replace_gens_with_gen_instances},
        substitution_sets::{promote, union},
        ty::SolveCond,
        type_info::TypeInfo,
    },
    TypeId,
};

use super::{ty::Ty, ty_env::TyEnv};

/// A enum representing the status/progress of solving a ADT type.
enum AdtSolveStatus {
    /// The given ADT type was solved. Return the path (possibly containing
    /// generics) of the solved ADT.
    Solved(LangPath),

    /// Some progress was made, but the ADT wasn't solved. Return the progressed
    /// TypeId (not the original).
    Progress(TypeId),

    /// The given ADT type is a GenericInstance. The TypeId is the representation
    /// of the GenericInstance.
    GenericInstance(TypeId),

    /// No progress made.
    NoProgress,
}

/// Returns the type contained in the root node of the substitution set that
/// contains the type `type_id`. This will be the type with the highest
/// precedence in the set containing the block with ID `block_id`.
///
/// If the given type doesn't belong to a set, returns the type itself as the
/// inferred type. If the given `type_id` has no root block ID set, a error
/// will be returned.
pub fn inferred_type(ty_env: &TyEnv, type_id: TypeId) -> LangResult<TypeId> {
    let type_id = ty_env.forwarded(type_id);
    ty_env.inferred_type(type_id)
}

/// Inserts a new constraint between two types.
/// If the type IDs are the equal, no new constraint will be created.
pub fn insert_constraint(
    ty_env: &Mutex<TyEnv>,
    type_id_a: TypeId,
    type_id_b: TypeId,
) -> LangResult<()> {
    let (type_id_a, is_any_a, type_id_b, is_any_b) = {
        let ty_env_guard = ty_env.lock().unwrap();

        let type_id_a = ty_env_guard.forwarded(type_id_a);
        let type_id_b = ty_env_guard.forwarded(type_id_b);

        if type_id_a == type_id_b {
            return Ok(());
        }

        debug!(
            "insert_constraint -- type_id_a: {}, ty_a: {:#?}\ntype_id_b: {}, ty_b: {:#?}",
            type_id_a,
            ty_env_guard.ty(type_id_a)?,
            type_id_b,
            ty_env_guard.ty(type_id_b)?,
        );

        let is_any_a = is_any(&ty_env_guard, type_id_a)?;
        let is_any_b = is_any(&ty_env_guard, type_id_b)?;

        (type_id_a, is_any_a, type_id_b, is_any_b)
    };

    if !is_any_a && !is_any_b {
        unify(ty_env, type_id_a, type_id_b)?;
    }

    Ok(())
}

/// "Unifies" the two types `type_id_a` and `type_id_b`.
/// This function also inserts new constraints for any inner types.
fn unify(ty_env: &Mutex<TyEnv>, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<TypeId> {
    let inf_type_id = union(ty_env, type_id_a, type_id_b)?;

    debug!(
        "After union, type_id_a: {}, type_id_b: {}, inf_type_id: {}",
        type_id_a, type_id_b, inf_type_id
    );

    insert_constraint_inner(ty_env, type_id_a, inf_type_id)?;
    insert_constraint_inner(ty_env, type_id_b, inf_type_id)?;

    Ok(inf_type_id)
}

/// Inserts constraints for potential "inner types" of types `ty` and `inferred_ty`.
fn insert_constraint_inner(
    ty_env: &Mutex<TyEnv>,
    type_id: TypeId,
    inf_type_id: TypeId,
) -> LangResult<()> {
    let (ty, inf_ty) = {
        let ty_env_guard = ty_env.lock().unwrap();

        let type_id = ty_env_guard.forwarded(type_id);
        let inf_type_id = ty_env_guard.forwarded(inf_type_id);

        if type_id == inf_type_id {
            return Ok(());
        }

        let ty = ty_env_guard.ty(type_id)?.clone();
        let inf_ty = ty_env_guard.ty(inf_type_id)?.clone();

        (ty, inf_ty)
    };

    match (ty, inf_ty) {
        (Ty::CompoundType(inner_a, ..), Ty::CompoundType(inner_b, ..)) => {
            if inner_a.gens().is_none() && inner_b.gens().is_none() {
                return Ok(());
            }

            let empty_gens = Generics::empty();
            let gens_a = inner_a.gens().unwrap_or(&empty_gens);
            let gens_b = inner_b.gens().unwrap_or(&empty_gens);

            for (ty_a_gen, ty_b_gen) in gens_a.iter_types().zip(gens_b.iter_types()) {
                insert_constraint_inner(ty_env, *ty_a_gen, *ty_b_gen)?;
                insert_constraint(ty_env, *ty_a_gen, *ty_b_gen)?;
            }
            Ok(())
        }

        (Ty::Pointer(ty_a_inner, ..), Ty::Pointer(ty_b_inner, ..))
        | (Ty::Array(ty_a_inner, ..), Ty::Array(ty_b_inner, ..))
        | (Ty::UnknownArrayMember(ty_a_inner, ..), Ty::UnknownArrayMember(ty_b_inner, ..)) => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        (
            Ty::UnknownAdtMethod(ty_a_inner, a_name, ..),
            Ty::UnknownAdtMethod(ty_b_inner, b_name, ..),
        )
        | (
            Ty::UnknownAdtMember(ty_a_inner, a_name, ..),
            Ty::UnknownAdtMember(ty_b_inner, b_name, ..),
        ) if a_name == b_name => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        (
            Ty::UnknownMethodArgument(ty_a_inner, a_name, a_idx_or_name, ..),
            Ty::UnknownMethodArgument(ty_b_inner, b_name, b_idx_or_name, ..),
        ) if a_name == b_name && a_idx_or_name == b_idx_or_name => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        (
            Ty::UnknownMethodGeneric(ty_a_inner, a_name, a_idx, ..),
            Ty::UnknownMethodGeneric(ty_b_inner, b_name, b_idx, ..),
        ) if a_name == b_name && a_idx == b_idx => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        _ => Ok(()),
    }
}

/// Iterates through all types found in the TypeEnvironment and tries to
/// solve them.
pub fn solve_all_solvable(ty_env: &Mutex<TyEnv>, ast_ctx: &AstCtx) -> LangResult<()> {
    let all_types = ty_env.lock().unwrap().interner.all_types();
    for type_id in all_types {
        solve(ty_env, ast_ctx, type_id)?;
    }
    Ok(())
}

/// Given a type `type_id`, tries to solve it recursively by looking at structure/
/// function declarations. This is needed for types that can't be figured out
/// directly from looking at the "use site" in the source code, some information
/// needs to be fetched from somewhere else to deduce the correct type.
///
/// This function should be the entry call when solving a new type. If this
/// is a recursive solve call inside a solve, the `priv_solve()` should be
/// used which keeps track of types seen to prevent infinite recursion.
///
/// OBS! This function does NOT return the final solved type ID, it might
///      return a partially solved type in some cases. If you want the type
///      ID that are "closest" to the correctly solved type ID, use the
///      `inferred_type()` function.
pub fn solve(ty_env: &Mutex<TyEnv>, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<TypeId> {
    let mut seen_type_ids = HashSet::default();
    solve_priv(ty_env, ast_ctx, type_id, &mut seen_type_ids)
}

/// Tries to solve the given `type_id` type and also keeps track of type IDs
/// seen during the solving to prevent infinite loops (in `seen_type_ids`).
///
/// The solving process first starts by looking at the substitition sets to
/// see if the given type is solved already. If that is the case, that solved
/// type will be returned. If it is not solved already, a "manual" solve will
/// be done. This will recursively traverse through the type and its nested
/// types to try and solve them.
///
/// This two step logic will be applied to the given `type_id` first and if
/// that is not solvable, the same logic will be applied to its inferred type.
fn solve_priv(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let fwd_type_id = ty_env.lock().unwrap().forwarded(type_id);

    debug!(
        "solve_priv -- type_id: {}, fwd_type_id: {}, fwd_ty: {:#?}",
        type_id,
        fwd_type_id,
        ty_env.lock().unwrap().ty(fwd_type_id)
    );

    if seen_type_ids.contains(&fwd_type_id) {
        return ty_env.lock().unwrap().inferred_type(fwd_type_id);
    } else {
        seen_type_ids.insert(fwd_type_id);
    }

    let inf_type_id = inferred_type(&ty_env.lock().unwrap(), fwd_type_id)?;

    let check_inf = false;
    let solve_cond = SolveCond::new()
        .excl_default()
        .excl_unknown()
        .excl_gen()
        .excl_gen_inst();
    let is_solved_res = is_solved(&ty_env.lock().unwrap(), fwd_type_id, check_inf, solve_cond)?;
    if is_solved_res {
        let root_type_id = promote(ty_env, fwd_type_id)?;
        insert_constraint(ty_env, fwd_type_id, inf_type_id)?;
        return Ok(root_type_id);
    }

    let solved_type_id = solve_manual(ty_env, ast_ctx, fwd_type_id, seen_type_ids)?;

    let check_inf = false;
    let solve_cond = SolveCond::new()
        .excl_default()
        .excl_unknown()
        .excl_gen()
        .excl_gen_inst();
    let is_solved_res = is_solved(
        &ty_env.lock().unwrap(),
        solved_type_id,
        check_inf,
        solve_cond,
    )?;
    if is_solved_res {
        return Ok(solved_type_id);
    }

    let inf_type_id = inferred_type(&ty_env.lock().unwrap(), fwd_type_id)?;

    let check_inf = true;
    let solve_cond = SolveCond::new()
        .excl_default()
        .excl_unknown()
        .excl_gen()
        .excl_gen_inst();
    let is_solved_res = is_solved(&ty_env.lock().unwrap(), inf_type_id, check_inf, solve_cond)?;
    if is_solved_res {
        insert_constraint(ty_env, fwd_type_id, inf_type_id)?;
        return Ok(inf_type_id);
    }

    if seen_type_ids.contains(&inf_type_id) {
        return inferred_type(&ty_env.lock().unwrap(), inf_type_id);
    } else {
        seen_type_ids.insert(inf_type_id);
    }

    let inf_solved_type_id = solve_manual(ty_env, ast_ctx, inf_type_id, seen_type_ids)?;
    insert_constraint(ty_env, inf_type_id, inf_solved_type_id)?;

    Ok(inf_solved_type_id)
}

/// Given a type `type_id`, tries to solve it. This function does NOT look
/// in either the `substition_sets` or its inferred type to solve this
/// specific `type_id` (use `solve()` in that case). This is only true for
/// the first "layer" of the solve, any inner types will be solved recursively
/// with using lookups etc.
fn solve_manual(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_manual -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    match ty_clone {
        Ty::CompoundType(..) => solve_compound(ty_env, ast_ctx, type_id, seen_type_ids),
        Ty::Pointer(..) | Ty::Array(..) => solve_aggregate(ty_env, ast_ctx, type_id, seen_type_ids),

        Ty::Expr(..) => solve_expr(ty_env, ast_ctx, type_id, seen_type_ids),
        Ty::Fn(..) => solve_fn(ty_env, ast_ctx, type_id, seen_type_ids),

        Ty::UnknownAdtMember(..) => {
            solve_unknown_adt_member(ty_env, ast_ctx, type_id, seen_type_ids)
        }
        Ty::UnknownAdtMethod(..) => {
            solve_unknown_adt_method(ty_env, ast_ctx, type_id, seen_type_ids)
        }
        Ty::UnknownMethodArgument(..) => {
            solve_unknown_method_argument(ty_env, ast_ctx, type_id, seen_type_ids)
        }
        Ty::UnknownMethodGeneric(..) => {
            solve_unknown_method_generic(ty_env, ast_ctx, type_id, seen_type_ids)
        }
        Ty::UnknownArrayMember(..) => {
            solve_unknown_array_member(ty_env, ast_ctx, type_id, seen_type_ids)
        }

        Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(type_id),
    }
}

/// Solves compound types (i.e. types that might contain generics).
fn solve_compound(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_compound -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let mut was_updated = false;

    let inner_ty = if let Ty::CompoundType(inner_ty, ..) = &mut ty_clone {
        inner_ty
    } else {
        unreachable!();
    };

    let mut new_nested_seen_type_ids: HashSet<TypeId> = HashSet::new();
    if let Some(gens) = inner_ty.gens_mut() {
        for gen_type_id in gens.iter_types_mut() {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            solve_priv(ty_env, ast_ctx, *gen_type_id, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));

            let inf_gen_type_id = inferred_type(&ty_env.lock().unwrap(), *gen_type_id)?;
            if *gen_type_id != inf_gen_type_id {
                *gen_type_id = inf_gen_type_id;
                was_updated = true;
            }
        }
    }
    seen_type_ids.extend(new_nested_seen_type_ids);

    // Solve the inner structure type.
    if let InnerTy::UnknownIdent(path, block_id) = inner_ty {
        let ty_env_guard = ty_env.lock().unwrap();
        let gens = path.gens();

        let full_path_opt = if let Ok(full_path) =
            ast_ctx.calculate_adt_full_path(&ty_env_guard, &path.without_gens(), *block_id)
        {
            Some(full_path)
        } else if let Ok(full_path) =
            ast_ctx.calculate_trait_full_path(&ty_env_guard, &path.without_gens(), *block_id)
        {
            Some(full_path)
        } else {
            None
        };

        if let Some(full_path) = full_path_opt {
            let full_path_with_gens = full_path.with_gens_opt(gens.cloned());

            let new_inner_type_id = if ast_ctx.is_struct(&ty_env_guard, &full_path) {
                Some(InnerTy::Struct(full_path_with_gens))
            } else if ast_ctx.is_enum(&ty_env_guard, &full_path) {
                Some(InnerTy::Enum(full_path_with_gens))
            } else if ast_ctx.is_union(&ty_env_guard, &full_path) {
                Some(InnerTy::Union(full_path_with_gens))
            } else if ast_ctx.is_trait(&ty_env_guard, &full_path) {
                Some(InnerTy::Trait(full_path_with_gens))
            } else {
                None
            };

            if let Some(new_inner_type_id) = new_inner_type_id {
                *inner_ty = new_inner_type_id;
                was_updated = true;
            }
        }
    }

    let inf_type_id = if was_updated {
        let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
        insert_constraint(ty_env, type_id, new_type_id)?;
        new_type_id
    } else {
        type_id
    };

    Ok(inf_type_id)
}

/// Solves aggregate types (array or pointer).
fn solve_aggregate(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_aggregate -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    // TODO: Probably need to implement solving of the array dimension expr.
    let new_type_id = match &mut ty {
        Ty::Pointer(aggr_type_id, ..) | Ty::Array(aggr_type_id, ..) => {
            solve_priv(ty_env, ast_ctx, *aggr_type_id, seen_type_ids)?;
            let inf_type_id = inferred_type(&ty_env.lock().unwrap(), *aggr_type_id)?;

            if &inf_type_id != aggr_type_id {
                *aggr_type_id = inf_type_id;

                let new_type_id = ty_env.lock().unwrap().id(&ty)?;
                insert_constraint(ty_env, type_id, new_type_id)?;

                new_type_id
            } else {
                type_id
            }
        }
        _ => unreachable!(),
    };

    let inf_type_id = inferred_type(&ty_env.lock().unwrap(), type_id)?;
    insert_constraint_inner(ty_env, type_id, inf_type_id)?;

    // TODO: Should this return `inf_type_id`? The `new_type_id` is not 100%
    //       the most optimaly solved type, but might need to return so that
    //       the caller can work on that type.
    Ok(new_type_id)
}

fn solve_expr(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_expr -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    let inf_type_id = if let Ty::Expr(expr, ..) = ty {
        let expr_type_id = expr.get_expr_type()?;
        solve_priv(ty_env, ast_ctx, expr_type_id, seen_type_ids)?
    } else {
        type_id
    };

    Ok(inf_type_id)
}

fn solve_fn(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_fn -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    if let Ty::Fn(gens, params, ret_type_id_opt, ..) = ty {
        let mut new_nested_seen_type_ids: HashSet<TypeId> = HashSet::new();
        for type_id_i in gens.iter().chain(params.iter()) {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            solve_priv(ty_env, ast_ctx, *type_id_i, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));
        }

        if let Some(ret_type_id) = ret_type_id_opt {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            solve_priv(ty_env, ast_ctx, ret_type_id, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));
        }
        seen_type_ids.extend(new_nested_seen_type_ids);
    } else {
        unreachable!();
    }

    // TODO: Does this need to be returned?
    Ok(type_id)
}

fn solve_unknown_adt_member(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_unknown_adt_member -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, member_name) =
        if let Ty::UnknownAdtMember(adt_type_id, member_name, ..) = &mut ty_clone {
            (adt_type_id, member_name)
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => {
            return Ok(type_id);
        }
    };

    let mut new_type_id = {
        let ty_env_guard = ty_env.lock().unwrap();
        let file_pos = get_file_pos(&ty_env_guard, *adt_type_id).cloned();
        ast_ctx
            .get_adt_member(
                &ty_env_guard,
                &adt_path.without_gens(),
                &member_name,
                file_pos,
            )?
            .read()
            .unwrap()
            .ty
            .unwrap()
    };

    // TODO: Is this needed? Does this do anything atm?
    // Replace potential generics with impls from the solved ADT type.
    if let Some(adt_gens) = adt_path.gens() {
        if let Some(new_new_type_id) = replace_gen_impls(ty_env, ast_ctx, new_type_id, adt_gens)? {
            new_type_id = new_new_type_id;
        }
    }

    // Start from max of u64 to prevent clashing with "normal" unique IDs.
    let unique_id = u64::MAX - type_id.0;
    let replace_gens_res =
        replace_gens_with_gen_instances(&mut ty_env.lock().unwrap(), new_type_id, unique_id)?;
    if let Some(new_new_type_id) = replace_gens_res {
        insert_constraint(ty_env, new_type_id, new_new_type_id)?;
        new_type_id = new_new_type_id;
    }

    solve_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
    let inf_new_type_id = inferred_type(&ty_env.lock().unwrap(), new_type_id)?;

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_adt_method(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_unknown_adt_method -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, method_name, fn_call_gens_vec, type_info) = if let Ty::UnknownAdtMethod(
        ty,
        method_name,
        fn_call_gens_vec,
        _,
        type_info,
    ) = &mut ty_clone
    {
        (ty, method_name, fn_call_gens_vec, type_info)
    } else {
        unreachable!()
    };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
    };

    let method = ast_ctx.get_method(
        &ty_env.lock().unwrap(),
        &adt_path.without_gens(),
        &method_name,
    )?;
    let method = method.as_ref().read().unwrap();
    let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

    // Create/get the generics for the function call and replace potential
    // generics in the return type of "this function call".
    let fn_call_gens = if fn_call_gens_vec.is_empty() {
        new_method_generics(&mut ty_env.lock().unwrap(), &method, &type_info)?
            .unwrap_or_else(Generics::empty)
    } else {
        let mut fn_call_gens = Generics::new();
        for (gen_name, gen_unknown_ty) in fn_gens.iter_names().zip(fn_call_gens_vec) {
            fn_call_gens.insert(gen_name.clone(), *gen_unknown_ty);
        }
        fn_call_gens
    };

    debug!(
        "solve_unknown_adt_method -- fn_call_gens: {:#?}, adt_path: {:#?}",
        fn_call_gens, adt_path
    );

    let inf_new_type_id = if let Some(mut new_type_id) = method.ret_type {
        if let Some(new_new_type_id) =
            replace_gen_impls(ty_env, ast_ctx, new_type_id, &fn_call_gens)?
        {
            new_type_id = new_new_type_id;
        }
        if let Some(adt_gens) = adt_path.gens() {
            if let Some(new_new_type_id) =
                replace_gen_impls(ty_env, ast_ctx, new_type_id, adt_gens)?
            {
                new_type_id = new_new_type_id;
            }
        }

        // Start from max of u64 to prevent clashing with "normal" unique IDs.
        let unique_id = u64::MAX - type_id.0;
        let replace_gens_res =
            replace_gens_with_gen_instances(&mut ty_env.lock().unwrap(), new_type_id, unique_id)?;
        if let Some(new_new_type_id) = replace_gens_res {
            insert_constraint(ty_env, new_type_id, new_new_type_id)?;
            new_type_id = new_new_type_id;
        }

        solve_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
        inferred_type(&ty_env.lock().unwrap(), new_type_id)?
    } else {
        // The return type of the method is None == Void.
        ty_env
            .lock()
            .unwrap()
            .id(&Ty::CompoundType(InnerTy::Void, TypeInfo::None))?
    };

    debug!(
        "solve_unknown_adt_method -- inf_new_type_id: {},  ty: {:#?}",
        inf_new_type_id,
        ty_env.lock().unwrap().ty(inf_new_type_id)
    );

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_method_argument(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_unknown_method_argument -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, method_name, fn_call_gens_vec, name_or_idx, type_info) =
        if let Ty::UnknownMethodArgument(
            type_id,
            method_name,
            fn_call_gens_vec,
            name_or_idx,
            _,
            type_info,
        ) = &mut ty_clone
        {
            (
                type_id,
                method_name,
                fn_call_gens_vec,
                name_or_idx,
                type_info,
            )
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
    };

    let actual_idx = match name_or_idx {
        Either::Left(arg_name) => ast_ctx.get_method_param_idx(
            &ty_env.lock().unwrap(),
            &adt_path.without_gens(),
            &method_name,
            &arg_name,
        )?,
        Either::Right(idx) => *idx,
    };

    let mut new_type_id = ast_ctx.get_method_param_type(
        &ty_env.lock().unwrap(),
        &adt_path.without_gens(),
        &method_name,
        actual_idx,
    )?;

    let method = ast_ctx.get_method(
        &ty_env.lock().unwrap(),
        &adt_path.without_gens(),
        &method_name,
    )?;
    let method = method.as_ref().read().unwrap();
    let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

    // Create/get the generics for the function call and replace potential
    // generics in the return type of "this function call".
    let fn_call_gens = if fn_call_gens_vec.is_empty() {
        if let Some(method_gens) =
            new_method_generics(&mut ty_env.lock().unwrap(), &method, &type_info)?
        {
            method_gens
        } else {
            Generics::empty()
        }
    } else {
        let mut fn_call_gens = Generics::new();
        for (gen_name, gen_unknown_ty) in fn_gens.iter_names().zip(fn_call_gens_vec) {
            fn_call_gens.insert(gen_name.clone(), *gen_unknown_ty);
        }
        fn_call_gens
    };

    if let Some(new_new_type_id) = replace_gen_impls(ty_env, ast_ctx, new_type_id, &fn_call_gens)? {
        new_type_id = new_new_type_id;
    }
    if let Some(adt_gens) = adt_path.gens() {
        if let Some(new_new_type_id) = replace_gen_impls(ty_env, ast_ctx, new_type_id, adt_gens)? {
            new_type_id = new_new_type_id;
        }
    }

    // Start from max of u64 to prevent clashing with "normal" unique IDs.
    let unique_id = u64::MAX - type_id.0;
    let replace_gens_res =
        replace_gens_with_gen_instances(&mut ty_env.lock().unwrap(), new_type_id, unique_id)?;
    if let Some(new_new_type_id) = replace_gens_res {
        insert_constraint(ty_env, new_type_id, new_new_type_id)?;
        new_type_id = new_new_type_id;
    }

    solve_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
    let inf_new_type_id = inferred_type(&ty_env.lock().unwrap(), new_type_id)?;

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_method_generic(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_unknown_method_generic -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, method_name, gen_idx_or_name, type_info) =
        if let Ty::UnknownMethodGeneric(adt_type_id, method_name, gen_idx_or_name, _, type_info) =
            &mut ty_clone
        {
            (adt_type_id, method_name, gen_idx_or_name, type_info)
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.lock().unwrap().id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
    };

    let method = ast_ctx.get_method(
        &ty_env.lock().unwrap(),
        &adt_path.without_gens(),
        &method_name,
    )?;
    let method = method.as_ref().read().unwrap();

    let gen_name = match gen_idx_or_name {
        Either::Left(idx) => {
            if let Some(generic_name) = method
                .generics
                .as_ref()
                .map(|gens| gens.get_name(*idx))
                .flatten()
            {
                generic_name
            } else {
                return Err(ast_ctx.err(format!(
                    "Method call specified generic at index {}. \
                        Method declaration for \"{}\" has no generic at that index.",
                    gen_idx_or_name, method_name
                )));
            }
        }
        Either::Right(gen_name) => gen_name.clone(),
    };

    let unique_id = ty_env.lock().unwrap().new_unique_id();
    let mut new_type_id =
        ty_env
            .lock()
            .unwrap()
            .id(&Ty::GenericInstance(gen_name, unique_id, type_info.clone()))?;

    if let Some(adt_gens) = adt_path.gens() {
        if let Some(new_new_type_id) = replace_gen_impls(ty_env, ast_ctx, new_type_id, adt_gens)? {
            new_type_id = new_new_type_id;
        }
    }

    let new_method_gens_res =
        new_method_generics(&mut ty_env.lock().unwrap(), &method, &type_info)?;
    if let Some(method_generics) = &new_method_gens_res {
        if let Some(new_new_type_id) =
            replace_gen_impls(ty_env, ast_ctx, new_type_id, &method_generics)?
        {
            new_type_id = new_new_type_id;
        }
    }

    solve_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
    let inf_new_type_id = inferred_type(&ty_env.lock().unwrap(), new_type_id)?;

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_array_member(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.lock().unwrap().ty_clone(type_id)?;

    debug!(
        "solve_unknown_array_member -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    let arr_type_id = if let Ty::UnknownArrayMember(arr_type_id, ..) = ty {
        arr_type_id
    } else {
        unreachable!()
    };

    solve_priv(ty_env, ast_ctx, arr_type_id, seen_type_ids)?;
    let new_arr_type_id = inferred_type(&ty_env.lock().unwrap(), arr_type_id)?;

    if arr_type_id == new_arr_type_id {
        // No progress made.
        return Ok(type_id);
    }

    insert_constraint(ty_env, arr_type_id, new_arr_type_id)?;

    let new_arr_ty = ty_env.lock().unwrap().ty_clone(new_arr_type_id)?;
    if let Ty::Array(new_member_type_id, ..) = new_arr_ty {
        solve_priv(ty_env, ast_ctx, new_member_type_id, seen_type_ids)?;
        let inf_new_member_type_id = inferred_type(&ty_env.lock().unwrap(), new_member_type_id)?;

        insert_constraint(ty_env, inf_new_member_type_id, type_id)?;
        Ok(new_member_type_id)
    } else {
        Ok(type_id)
    }
}

/// Given a potential ADT type `adt_type_id`, tries to solve it.
/// Returns a `AdtSolveStatus` enum indicating if any progress was made
/// solving the ADT type.
fn solve_adt_type(
    ty_env: &Mutex<TyEnv>,
    ast_ctx: &AstCtx,
    adt_type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<AdtSolveStatus> {
    debug!(
        "solve_adt_type -- adt_type_id: {}, seen_type_ids: {:?}",
        adt_type_id, seen_type_ids,
    );

    solve_priv(ty_env, ast_ctx, adt_type_id, seen_type_ids)?;
    let inf_adt_type_id = inferred_type(&ty_env.lock().unwrap(), adt_type_id)?;

    let is_generic_res = is_generic(&ty_env.lock().unwrap(), inf_adt_type_id)?;
    let check_inf = true;
    let solve_cond = SolveCond::new().excl_unknown();
    let is_solved_res = is_solved(
        &ty_env.lock().unwrap(),
        inf_adt_type_id,
        check_inf,
        solve_cond,
    )?;

    if is_generic_res && adt_type_id != inf_adt_type_id {
        Ok(AdtSolveStatus::GenericInstance(inf_adt_type_id))
    } else if is_generic_res {
        Ok(AdtSolveStatus::NoProgress)
    } else if is_solved_res {
        if adt_type_id != inf_adt_type_id {
            set_generic_names(&mut ty_env.lock().unwrap(), ast_ctx, inf_adt_type_id)?;
        }

        let inf_adt_ty = ty_env.lock().unwrap().ty_clone(inf_adt_type_id)?;
        let inner_ty = match inf_adt_ty {
            Ty::CompoundType(inner_ty, ..) => inner_ty,

            // TODO: Fix this edge case. This might be a pointer to ADT to
            //       represent a "{this}" function. Remove the need for this
            //       logic in the future.
            Ty::Pointer(type_id_i, type_info) => {
                solve_priv(ty_env, ast_ctx, type_id_i, seen_type_ids)?;
                let inf_type_id_i = inferred_type(&ty_env.lock().unwrap(), type_id_i)?;

                let is_generic_res = is_generic(&ty_env.lock().unwrap(), inf_type_id_i)?;
                if is_generic_res && adt_type_id != inf_adt_type_id {
                    let new_type_id = ty_env
                        .lock()
                        .unwrap()
                        .id(&Ty::Pointer(inf_type_id_i, type_info))?;
                    return Ok(AdtSolveStatus::GenericInstance(new_type_id));
                } else if is_generic_res {
                    return Ok(AdtSolveStatus::NoProgress);
                }

                if adt_type_id != inf_adt_type_id {
                    set_generic_names(&mut ty_env.lock().unwrap(), ast_ctx, inf_type_id_i)?;
                }

                let inf_ty_i = ty_env.lock().unwrap().ty_clone(inf_type_id_i)?;
                if let Ty::CompoundType(inner_ty, ..) = inf_ty_i {
                    inner_ty
                } else {
                    unreachable!(
                        "ADT type not pointer to compound, inf_adt_type_id: {}",
                        inf_adt_type_id
                    );
                }
            }

            _ => unreachable!("ADT type not valid: {:#?}", inf_adt_ty),
        };

        if let Some(adt_path) = inner_ty.get_ident() {
            Ok(AdtSolveStatus::Solved(adt_path))
        } else {
            Err(ast_ctx.err(format!(
                "ADT CompoundType isn't generic or identifier, is: {:?}\nADT type ID: {}",
                inner_ty, adt_type_id
            )))
        }
    } else if adt_type_id == inf_adt_type_id {
        Ok(AdtSolveStatus::NoProgress)
    } else {
        Ok(AdtSolveStatus::Progress(inf_adt_type_id))
    }
}

/// Creates a new `Generic` where the types will be new `GenericInstance`s.
/// This `Generic` can be used to replace `Ty::Generic` with new
/// `Ty::GenericInstance`s found in this returned value.
///
/// This is needed to ensure that no "raw" `Ty::Generic`s are leaked outside
/// the function body itself. I.e. this can be used to replace arguments and
/// return values of function calls.
pub fn new_method_generics(
    ty_env: &mut TyEnv,
    method: &Fn,
    type_info: &TypeInfo,
) -> LangResult<Option<Generics>> {
    // TODO: This should be done somewhere else. This feels like a really
    //       random place to do it.
    if let Some(method_generics) = &method.generics {
        let mut new_method_generics = Generics::new();

        for generic_name in method_generics.iter_names() {
            let unique_id = ty_env.new_unique_id();
            let type_id = ty_env.id(&Ty::GenericInstance(
                generic_name.clone(),
                unique_id,
                type_info.clone(),
            ))?;

            new_method_generics.insert(generic_name.clone(), type_id);
        }

        Ok(Some(new_method_generics))
    } else {
        Ok(None)
    }
}

/// If the given type `ty` contains generics that don't have their "names"
/// set, this function will fetch the structure and set the names if possible.
pub fn set_generic_names(ty_env: &mut TyEnv, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<()> {
    let ty_clone = ty_env.ty_clone(type_id)?;

    let inner_ty = match ty_clone {
        Ty::CompoundType(inner_ty, ..) => inner_ty,
        Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
            return set_generic_names(ty_env, ast_ctx, type_id_i);
        }
        _ => return Ok(()),
    };

    match inner_ty.gens() {
        Some(gens) if !gens.is_empty_types() && gens.is_empty_names() => (),
        _ => return Ok(()),
    }

    // From this point on, we know that the given type have generic types set
    // but those generics doesn't have their names set. Get names from the ADT
    // in the AST and set those in the current generics.

    let path_without_gens = inner_ty.get_ident().unwrap().without_gens();
    let full_path_without_gens = ast_ctx
        .calculate_adt_full_path(ty_env, &path_without_gens, BlockCtx::DEFAULT_BLOCK_ID)?
        .without_gens();

    let adt = match ast_ctx.get_adt(ty_env, &full_path_without_gens) {
        Ok(adt) => adt,
        Err(err) => return Err(err),
    };
    let adt = adt.as_ref().read().unwrap();

    if let Some(adt_gens) = adt.generics.clone() {
        let ty_mut = ty_env.ty_mut(type_id)?;

        let gens = match ty_mut {
            Ty::CompoundType(inner_ty, ..) => inner_ty.gens_mut().unwrap(),
            _ => unreachable!(),
        };

        for (idx, gen_name) in adt_gens.iter_names().enumerate() {
            gens.insert_lookup(gen_name.clone(), idx);
            gens.insert_name(gen_name.clone());
        }
    }

    Ok(())
}
