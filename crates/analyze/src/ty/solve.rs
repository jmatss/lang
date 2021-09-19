use std::collections::{HashMap, HashSet, VecDeque};

use either::Either;
use log::debug;

use common::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    eq::path_eq,
    error::LangResult,
    hash::DerefType,
    path::LangPath,
    token::block::Fn,
    traverse::traverse_ctx::TraverseCtx,
    ty::{
        generics::Generics,
        get::{get_file_pos, get_generics, get_unsolvable},
        inner_ty::InnerTy,
        is::{is_any, is_generic, is_solved, is_tuple},
        replace::{convert_defaults, replace_gen_impls, replace_gens_with_gen_instances},
        substitution_sets::{promote, union},
        to_string::{to_string_path, to_string_type_id},
        ty::{SolveCond, Ty},
        ty_env::TyEnv,
        type_id::TypeId,
        type_info::TypeInfo,
    },
};

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

pub fn solve_type_system(ctx: &mut TraverseCtx) -> LangResult<()> {
    let mut all_types = ctx.ty_env.lock().interner.all_types();

    // Remove all generic types declared in trait functions. These are types that
    // are used in the trait "blue-print" functions and will never be solvable.
    let trait_gens = collect_trait_gens(&ctx.ty_env.lock(), ctx.ast_ctx)?;
    all_types.retain(|type_id| !trait_gens.contains(type_id));

    solve_types_ids(ctx, all_types)?;
    convert_defaults(&mut ctx.ty_env.lock())?;

    Ok(())
}

/// Tries to solves all types given in `all_type_ids`.
fn solve_types_ids(ctx: &mut TraverseCtx, all_type_ids: HashSet<TypeId>) -> LangResult<()> {
    if all_type_ids.is_empty() {
        return Ok(());
    }

    // Contains a list of all type IDs that aren't solved yet.
    //
    // During the solving stage type IDs will be popped from the front an solved
    // in that order. Any new types created during the solving process will be
    // inserted at the back of the queue.
    let mut unsolved = VecDeque::default();
    for type_id in all_type_ids.iter() {
        unsolved.push_back(*type_id);
    }

    // Keeps a count of how many types have been traversed without solving a
    // single one. If this counts up to `unsolved.len()`, we have traversed the
    // whole list without solving any.
    // The type solving have either failed, or we need to change the `SolveCond`
    // and try solving the types again.
    let mut iter_count = 0;

    // All unsolvable types will be iterated through for every SolveCond found
    // in `solve_conds`. It starts using the SolveCond from the start of the array
    // which should be the "strictest" solve condition and will then, if unable
    // to solve all types with this condition, continue using the next SolveCond
    // in the vector. This continues until either all types are solved or until
    // all SolveCond's have been tried and the type system isn't solvable.
    let solve_conds = [SolveCond::new().excl_gen_inst(), SolveCond::new()];

    let mut ty_env_guard = ctx.ty_env.lock();

    ty_env_guard.solve_mode = true;

    for solve_cond in solve_conds {
        while let Some(type_id) = unsolved.pop_front() {
            iter_count += 1;

            debug!(
                "solving solver -- type_id: {}, iter_count: {}",
                type_id, iter_count
            );

            solve_type_id(&mut ty_env_guard, ctx.ast_ctx, type_id)?;
            let inf_type_id = ty_env_guard.inferred_type(type_id)?;

            // During the `solve()`, new types might potentially be created and
            // inserted into the `new_type_ids`. They might have to be solved.
            for new_type_id in ty_env_guard.new_type_ids.drain() {
                unsolved.push_back(new_type_id);
            }

            let check_inf = true;
            if is_solved(&ty_env_guard, inf_type_id, check_inf, solve_cond)?
                && nested_is_solved(&ty_env_guard, type_id, check_inf, solve_cond)?
            {
                iter_count = 0;
            } else {
                unsolved.push_back(type_id);
            }

            if iter_count >= unsolved.len() {
                break;
            }
        }
    }

    if unsolved.is_empty() {
        Ok(())
    } else {
        let mut err_msg = "Unable to solve type system.".to_string();

        for type_id in unsolved {
            let inf_type_id = ty_env_guard.inferred_type(type_id)?;

            err_msg.push_str(&format!(
                "\nUnable to solve type {} ({}). Got back unsolved: {} ({}). ty:\n{:#?}",
                type_id,
                to_string_type_id(&ty_env_guard, type_id)?,
                inf_type_id,
                to_string_type_id(&ty_env_guard, inf_type_id)?,
                ty_env_guard.ty(inf_type_id)
            ));
        }

        Err(ctx.ast_ctx.err(err_msg))
    }
}

/// Inserts a new constraint between two types.
/// If the type IDs are the equal, no new constraint will be created.
pub fn insert_constraint(
    ty_env: &mut TyEnv,
    type_id_a: TypeId,
    type_id_b: TypeId,
) -> LangResult<()> {
    let (type_id_a, is_any_a, type_id_b, is_any_b) = {
        let type_id_a = ty_env.forwarded(type_id_a);
        let type_id_b = ty_env.forwarded(type_id_b);

        if type_id_a == type_id_b {
            return Ok(());
        }

        debug!(
            "insert_constraint -- type_id_a: {}, ty_a: {:#?}\ntype_id_b: {}, ty_b: {:#?}",
            type_id_a,
            ty_env.ty(type_id_a)?,
            type_id_b,
            ty_env.ty(type_id_b)?,
        );

        let is_any_a = is_any(ty_env, type_id_a)?;
        let is_any_b = is_any(ty_env, type_id_b)?;

        (type_id_a, is_any_a, type_id_b, is_any_b)
    };

    if !is_any_a || !is_any_b {
        unify(ty_env, type_id_a, type_id_b)?;
    }

    Ok(())
}

/// "Unifies" the two types `type_id_a` and `type_id_b`.
/// This function also inserts new constraints for any inner types.
fn unify(ty_env: &mut TyEnv, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<TypeId> {
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
    ty_env: &mut TyEnv,
    type_id: TypeId,
    inf_type_id: TypeId,
) -> LangResult<()> {
    let (ty, inf_ty) = {
        let type_id = ty_env.forwarded(type_id);
        let inf_type_id = ty_env.forwarded(inf_type_id);

        if type_id == inf_type_id {
            return Ok(());
        }

        let ty = ty_env.ty(type_id)?.clone();
        let inf_ty = ty_env.ty(inf_type_id)?.clone();

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
            Ty::UnknownAdtMethod(ty_a_inner, a_path, ..),
            Ty::UnknownAdtMethod(ty_b_inner, b_path, ..),
        ) if path_eq(ty_env, &a_path, &b_path, DerefType::Deep)? => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        (
            Ty::UnknownAdtMember(ty_a_inner, a_name, ..),
            Ty::UnknownAdtMember(ty_b_inner, b_name, ..),
        ) if a_name == b_name => {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        (
            Ty::UnknownFnArgument(ty_a_inner, a_path, a_idx_or_name, ..),
            Ty::UnknownFnArgument(ty_b_inner, b_path, b_idx_or_name, ..),
        ) if path_eq(ty_env, &a_path, &b_path, DerefType::Deep)?
            && a_idx_or_name == b_idx_or_name =>
        {
            insert_constraint_inner(ty_env, ty_a_inner, ty_b_inner)?;
            insert_constraint(ty_env, ty_a_inner, ty_b_inner)
        }

        _ => Ok(()),
    }
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
fn solve_type_id(ty_env: &mut TyEnv, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<TypeId> {
    let mut seen_type_ids = HashSet::default();
    solve_type_id_priv(ty_env, ast_ctx, type_id, &mut seen_type_ids)
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
fn solve_type_id_priv(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let fwd_type_id = ty_env.forwarded(type_id);

    debug!(
        "solve_type_id_priv -- type_id: {}, fwd_type_id: {}, fwd_ty: {:#?}",
        type_id,
        fwd_type_id,
        ty_env.ty(fwd_type_id)
    );

    if seen_type_ids.contains(&fwd_type_id) {
        return ty_env.inferred_type(fwd_type_id);
    } else {
        seen_type_ids.insert(fwd_type_id);
    }

    let inf_type_id = ty_env.inferred_type(fwd_type_id)?;

    let check_inf = false;
    let solve_cond = SolveCond::new()
        .excl_default()
        .excl_unknown()
        .excl_gen()
        .excl_gen_inst();
    let is_solved_res = is_solved(ty_env, fwd_type_id, check_inf, solve_cond)?;
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
    let is_solved_res = is_solved(ty_env, solved_type_id, check_inf, solve_cond)?;
    if is_solved_res {
        return Ok(solved_type_id);
    }

    let inf_type_id = ty_env.inferred_type(fwd_type_id)?;

    let check_inf = true;
    let solve_cond = SolveCond::new()
        .excl_default()
        .excl_unknown()
        .excl_gen()
        .excl_gen_inst();
    let is_solved_res = is_solved(ty_env, inf_type_id, check_inf, solve_cond)?;
    if is_solved_res {
        insert_constraint(ty_env, fwd_type_id, inf_type_id)?;
        return Ok(inf_type_id);
    }

    if seen_type_ids.contains(&inf_type_id) {
        return ty_env.inferred_type(inf_type_id);
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
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty_clone = ty_env.ty_clone(type_id)?;

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
        Ty::UnknownFnArgument(..) => {
            solve_unknown_method_argument(ty_env, ast_ctx, type_id, seen_type_ids)
        }
        Ty::UnknownArrayMember(..) => {
            solve_unknown_array_member(ty_env, ast_ctx, type_id, seen_type_ids)
        }

        Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(type_id),
    }
}

/// Solves compound types (i.e. types that might contain generics).
fn solve_compound(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.ty_clone(type_id)?;

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
            solve_type_id_priv(ty_env, ast_ctx, *gen_type_id, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));

            let inf_gen_type_id = ty_env.inferred_type(*gen_type_id)?;
            if *gen_type_id != inf_gen_type_id {
                *gen_type_id = inf_gen_type_id;
                was_updated = true;
            }
        }
    }
    seen_type_ids.extend(new_nested_seen_type_ids);

    // Solve the inner structure type.
    if let InnerTy::UnknownIdent(path, block_id) = inner_ty {
        let gens = path.gens();

        let full_path_opt = if let Ok(full_path) =
            ast_ctx.calculate_adt_full_path(ty_env, &path.without_gens(), *block_id)
        {
            Some(full_path)
        } else if let Ok(full_path) =
            ast_ctx.calculate_trait_full_path(ty_env, &path.without_gens(), *block_id)
        {
            Some(full_path)
        } else {
            None
        };

        if let Some(full_path) = full_path_opt {
            let full_path_with_gens = full_path.with_gens_opt(gens.cloned());

            let new_inner_type_id = if ast_ctx.is_struct(ty_env, &full_path) {
                Some(InnerTy::Struct(full_path_with_gens))
            } else if ast_ctx.is_enum(ty_env, &full_path) {
                Some(InnerTy::Enum(full_path_with_gens))
            } else if ast_ctx.is_union(ty_env, &full_path) {
                Some(InnerTy::Union(full_path_with_gens))
            } else if ast_ctx.is_trait(ty_env, &full_path) {
                Some(InnerTy::Trait(full_path_with_gens))
            } else if ast_ctx.is_tuple(ty_env, &full_path) {
                Some(InnerTy::Tuple(full_path_with_gens))
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
        let new_type_id = ty_env.id(&ty_clone)?;
        insert_constraint(ty_env, type_id, new_type_id)?;
        new_type_id
    } else {
        type_id
    };

    Ok(inf_type_id)
}

/// Solves aggregate types (array or pointer).
fn solve_aggregate(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_aggregate -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    // TODO: Probably need to implement solving of the array dimension expr.
    let new_type_id = match &mut ty {
        Ty::Pointer(aggr_type_id, ..) | Ty::Array(aggr_type_id, ..) => {
            solve_type_id_priv(ty_env, ast_ctx, *aggr_type_id, seen_type_ids)?;
            let inf_type_id = ty_env.inferred_type(*aggr_type_id)?;

            if &inf_type_id != aggr_type_id {
                *aggr_type_id = inf_type_id;

                let new_type_id = ty_env.id(&ty)?;
                insert_constraint(ty_env, type_id, new_type_id)?;

                new_type_id
            } else {
                type_id
            }
        }
        _ => unreachable!(),
    };

    let inf_type_id = ty_env.inferred_type(type_id)?;
    insert_constraint_inner(ty_env, type_id, inf_type_id)?;

    // TODO: Should this return `inf_type_id`? The `new_type_id` is not 100%
    //       the most optimaly solved type, but might need to return so that
    //       the caller can work on that type.
    Ok(new_type_id)
}

fn solve_expr(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_expr -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    let inf_type_id = if let Ty::Expr(expr, ..) = ty {
        let expr_type_id = expr.get_expr_type()?;
        solve_type_id_priv(ty_env, ast_ctx, expr_type_id, seen_type_ids)?
    } else {
        type_id
    };

    Ok(inf_type_id)
}

fn solve_fn(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_fn -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    if let Ty::Fn(gens, params, ret_type_id_opt, ..) = ty {
        let mut new_nested_seen_type_ids: HashSet<TypeId> = HashSet::new();
        for type_id_i in gens.iter().chain(params.iter()) {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            solve_type_id_priv(ty_env, ast_ctx, *type_id_i, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));
        }

        if let Some(ret_type_id) = ret_type_id_opt {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            solve_type_id_priv(ty_env, ast_ctx, ret_type_id, &mut seen_type_ids_snapshot)?;
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
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_unknown_adt_member -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, name_or_idx) =
        if let Ty::UnknownAdtMember(adt_type_id, name_or_idx, ..) = &mut ty_clone {
            (adt_type_id, name_or_idx)
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => {
            return Ok(type_id);
        }
    };

    let mut new_type_id = {
        let file_pos = get_file_pos(ty_env, *adt_type_id).cloned();
        match name_or_idx {
            Either::Left(member_name) => ast_ctx
                .get_adt_member(ty_env, &adt_path.without_gens(), member_name, file_pos)?
                .read()
                .ty
                .unwrap(),
            Either::Right(idx) => {
                if adt_path.count() == 1 && adt_path.last().unwrap().name() == "Tuple" {
                    *adt_path.gens().unwrap().types.get(*idx).unwrap()
                } else {
                    ast_ctx
                        .get_adt_member_with_index(
                            ty_env,
                            &adt_path.without_gens(),
                            *idx,
                            file_pos,
                        )?
                        .read()
                        .ty
                        .unwrap()
                }
            }
        }
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
    let replace_gens_res = replace_gens_with_gen_instances(ty_env, new_type_id, unique_id)?;
    if let Some(new_new_type_id) = replace_gens_res {
        insert_constraint(ty_env, new_type_id, new_new_type_id)?;
        new_type_id = new_new_type_id;
    }

    solve_type_id_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
    let inf_new_type_id = ty_env.inferred_type(new_type_id)?;

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_adt_method(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_unknown_adt_method -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, method_path, method_arg_tys) =
        if let Ty::UnknownAdtMethod(ty, method_path, method_arg_tys, ..) = &mut ty_clone {
            (ty, method_path, method_arg_tys)
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
    };

    let (method_name, method_call_gens) = if let Some(part) = method_path.last_mut() {
        (part.0.clone(), part.1.clone())
    } else {
        unreachable!("{:#?}", ty_clone);
    };

    let method = ast_ctx.get_method(ty_env, &adt_path.without_gens(), &method_name)?;
    let method = method.read();
    let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

    let fn_call_gens = if fn_gens.is_empty() {
        Generics::empty()
    } else if let Some(method_call_gens) = method_call_gens {
        if method_call_gens.is_empty_names() {
            let mut new_method_call_gens = Generics::new();
            for (gen_name, gen_type_id) in fn_gens.iter_names().zip(method_call_gens.iter_types()) {
                new_method_call_gens.insert(gen_name.clone(), *gen_type_id);
            }
        }
        method_call_gens
    } else {
        // At this point, it is the first time that we have solved the ADT of the
        // `UnknownAdtMethod` and we have been able to see that the method have
        // generics declared and that the call-site didn't specify any generics.
        infer_gens_from_args(ty_env, ast_ctx, &method, method_arg_tys, Some(type_id))?
    };

    if let Some(last_part) = method_path.last_mut() {
        last_part.1 = Some(fn_call_gens.clone());
    }
    ty_env.update(type_id, ty_clone.clone())?;

    let inf_new_type_id = if let Some(mut new_type_id) = method.ret_type {
        // Create/get the generics for the function call and replace potential
        // generics in the return type of "this function call".
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
        let replace_gens_res = replace_gens_with_gen_instances(ty_env, new_type_id, unique_id)?;
        if let Some(new_new_type_id) = replace_gens_res {
            insert_constraint(ty_env, new_type_id, new_new_type_id)?;
            new_type_id = new_new_type_id;
        }

        solve_type_id_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
        ty_env.inferred_type(new_type_id)?
    } else {
        // The return type of the method is None == Void.
        ty_env.id(&Ty::CompoundType(InnerTy::Void, TypeInfo::None))?
    };

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_method_argument(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let mut ty_clone = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_unknown_method_argument -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
        type_id, seen_type_ids, ty_clone
    );

    let (adt_type_id, method_path, name_or_idx, ..) =
        if let Ty::UnknownFnArgument(type_id, method_path, name_or_idx, ..) = &mut ty_clone {
            (type_id, method_path, name_or_idx)
        } else {
            unreachable!()
        };

    let adt_path = match solve_adt_type(ty_env, ast_ctx, *adt_type_id, seen_type_ids)? {
        AdtSolveStatus::Solved(adt_path) => adt_path,
        AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
            *adt_type_id = gen_adt_type_id;
            let new_type_id = ty_env.id(&ty_clone)?;
            insert_constraint(ty_env, type_id, new_type_id)?;
            return Ok(new_type_id);
        }
        AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
    };

    let (method_name, method_call_gens) = if let Some(part) = method_path.last_mut() {
        (part.0.clone(), part.1.clone())
    } else {
        unreachable!("{:#?}", ty_clone);
    };

    let actual_idx = match name_or_idx {
        Either::Left(arg_name) => ast_ctx.get_method_param_idx(
            ty_env,
            &adt_path.without_gens(),
            &method_name,
            arg_name,
        )?,
        Either::Right(idx) => *idx,
    };

    let mut new_type_id = ast_ctx.get_method_param_type(
        ty_env,
        &adt_path.without_gens(),
        &method_name,
        actual_idx,
    )?;

    let method = ast_ctx.get_method(ty_env, &adt_path.without_gens(), &method_name)?;
    let method = method.read();
    let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

    // Create/get the generics for the function call and replace potential
    // generics in the return type of "this function call".
    let fn_call_gens = if fn_gens.is_empty() {
        Generics::empty()
    } else if let Some(method_call_gens) = method_call_gens {
        if method_call_gens.is_empty_names() {
            let mut new_method_call_gens = Generics::new();
            for (gen_name, gen_type_id) in fn_gens.iter_names().zip(method_call_gens.iter_types()) {
                new_method_call_gens.insert(gen_name.clone(), *gen_type_id);
            }
        }
        method_call_gens
    } else {
        Generics::empty()
    };

    if let Some(last_part) = method_path.last_mut() {
        last_part.1 = Some(fn_call_gens.clone());
    }
    ty_env.update(type_id, ty_clone.clone())?;

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
    let replace_gens_res = replace_gens_with_gen_instances(ty_env, new_type_id, unique_id)?;
    if let Some(new_new_type_id) = replace_gens_res {
        insert_constraint(ty_env, new_type_id, new_new_type_id)?;
        new_type_id = new_new_type_id;
    }

    solve_type_id_priv(ty_env, ast_ctx, new_type_id, seen_type_ids)?;
    let inf_new_type_id = ty_env.inferred_type(new_type_id)?;

    insert_constraint(ty_env, type_id, inf_new_type_id)?;
    Ok(inf_new_type_id)
}

fn solve_unknown_array_member(
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<TypeId> {
    let ty = ty_env.ty_clone(type_id)?;

    debug!(
        "solve_unknown_array_member -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
        type_id, seen_type_ids, ty
    );

    let arr_type_id = if let Ty::UnknownArrayMember(arr_type_id, ..) = ty {
        arr_type_id
    } else {
        unreachable!()
    };

    solve_type_id_priv(ty_env, ast_ctx, arr_type_id, seen_type_ids)?;
    let new_arr_type_id = ty_env.inferred_type(arr_type_id)?;

    if arr_type_id == new_arr_type_id {
        // No progress made.
        return Ok(type_id);
    }

    insert_constraint(ty_env, arr_type_id, new_arr_type_id)?;

    let new_arr_ty = ty_env.ty_clone(new_arr_type_id)?;
    if let Ty::Array(new_member_type_id, ..) = new_arr_ty {
        solve_type_id_priv(ty_env, ast_ctx, new_member_type_id, seen_type_ids)?;
        let inf_new_member_type_id = ty_env.inferred_type(new_member_type_id)?;

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
    ty_env: &mut TyEnv,
    ast_ctx: &AstCtx,
    adt_type_id: TypeId,
    seen_type_ids: &mut HashSet<TypeId>,
) -> LangResult<AdtSolveStatus> {
    debug!(
        "solve_adt_type -- adt_type_id: {}, seen_type_ids: {:?}",
        adt_type_id, seen_type_ids,
    );

    solve_type_id_priv(ty_env, ast_ctx, adt_type_id, seen_type_ids)?;
    let inf_adt_type_id = ty_env.inferred_type(adt_type_id)?;

    let is_generic_res = is_generic(ty_env, inf_adt_type_id)?;
    let check_inf = true;
    let solve_cond = SolveCond::new().excl_unknown().excl_any();
    let is_solved_res = is_solved(ty_env, inf_adt_type_id, check_inf, solve_cond)?;

    if is_generic_res && adt_type_id != inf_adt_type_id {
        Ok(AdtSolveStatus::GenericInstance(inf_adt_type_id))
    } else if is_generic_res {
        Ok(AdtSolveStatus::NoProgress)
    } else if is_solved_res {
        if adt_type_id != inf_adt_type_id {
            set_generic_names(ty_env, ast_ctx, inf_adt_type_id)?;
        }

        let inf_adt_ty = ty_env.ty_clone(inf_adt_type_id)?;
        let inner_ty = match inf_adt_ty {
            Ty::CompoundType(inner_ty, ..) if inner_ty.is_primitive() => {
                InnerTy::Struct(inner_ty.get_primitive_ident().into())
            }

            Ty::CompoundType(inner_ty, ..) => inner_ty,

            // TODO: Fix this edge case. This might be a pointer to ADT to
            //       represent a "{this}" function. Remove the need for this
            //       logic in the future.
            Ty::Pointer(type_id_i, type_info) => {
                solve_type_id_priv(ty_env, ast_ctx, type_id_i, seen_type_ids)?;
                let inf_type_id_i = ty_env.inferred_type(type_id_i)?;

                let is_generic_res = is_generic(ty_env, inf_type_id_i)?;
                if is_generic_res && adt_type_id != inf_adt_type_id {
                    let new_type_id = ty_env.id(&Ty::Pointer(inf_type_id_i, type_info))?;
                    return Ok(AdtSolveStatus::GenericInstance(new_type_id));
                } else if is_generic_res {
                    return Ok(AdtSolveStatus::NoProgress);
                }

                if adt_type_id != inf_adt_type_id {
                    set_generic_names(ty_env, ast_ctx, inf_type_id_i)?;
                }

                let inf_ty_i = ty_env.ty_clone(inf_type_id_i)?;
                if let Ty::CompoundType(inner_ty, ..) = inf_ty_i {
                    inner_ty
                } else {
                    unreachable!(
                        "ADT type not pointer to compound, inf_adt_type:: {} ({})",
                        to_string_type_id(ty_env, inf_adt_type_id)?,
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

/// Checks if all nested types of `type_id` is solved.
/// This check is needed since the inferred type of `type_id` might be ex.
/// a `DefaultInt` which is considered solved, but the current `type_id` might
/// be a `UnknownMethodArgument` which contains an unsolved ADT type.
/// In this case we wouldn't want to consider this `type_id` solved since we
/// might find a better type than `DefaultInt` once the ADT type is solved and
/// we can figure out the type of the unknown argument.
fn nested_is_solved(
    ty_env: &TyEnv,
    type_id: TypeId,
    check_inf: bool,
    solve_cond: SolveCond,
) -> LangResult<bool> {
    for nested_type_id in get_unsolvable(ty_env, type_id, solve_cond)? {
        let inf_type_id = ty_env.inferred_type(nested_type_id)?;
        if !is_solved(ty_env, inf_type_id, check_inf, solve_cond)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Iterates through all traits and collects the `Generic` types.
/// The types will be collected from the traits parameters and return type.
///
/// Types in traits will never be solved because traits are used as "blueprints"
/// only. We therefore have to treat these generics specially and exclude them
/// from some logic. They need to be excluded from the type solving since they
/// are unsolvable.
fn collect_trait_gens(ty_env: &TyEnv, ctx: &AstCtx) -> LangResult<HashSet<TypeId>> {
    let mut gen_type_ids = HashSet::default();

    for trait_ in ctx.traits.values() {
        let trait_ = trait_.read();
        for method in &trait_.methods {
            if let Some(params) = &method.parameters {
                for param in params {
                    if let Some(param_type_id) = param.read().ty {
                        for gen_type_id in get_generics(ty_env, param_type_id)? {
                            gen_type_ids.insert(gen_type_id);
                        }
                    }
                }
            }

            if let Some(ret_type_id) = method.ret_type {
                for gen_type_id in get_generics(ty_env, ret_type_id)? {
                    gen_type_ids.insert(gen_type_id);
                }
            }
        }
    }

    Ok(gen_type_ids)
}

/// Given a function declaration `fn_decl`, and a call to this function `fn_call`,
/// tries to infer the types of the generics for the function call.
///
/// One should not call this function if the function declaration doesn't contain
/// any generics or if the function call have specified the types of the generics
/// manually in the code.
///
/// This function will "zip" the types of the parameters/arguments and return
/// types. If a generic is found in the `fn_decl` type, the corresponding type
/// in the `fn_call` arg/return type will become the inferred type for the generic.
pub fn infer_gens_from_args(
    ty_env: &TyEnv,
    ast_ctx: &AstCtx,
    fn_decl: &Fn,
    fn_call_arg_tys: &[TypeId],
    fn_call_ret_ty: Option<TypeId>,
) -> LangResult<Generics> {
    let decl_gens = if let Some(gens) = &fn_decl.generics {
        gens
    } else {
        return Ok(Generics::empty());
    };

    let mut gen_name_to_impl = HashMap::default();

    if let Some(params) = &fn_decl.parameters {
        assert!(params.len() == fn_call_arg_tys.len());

        for (param, arg_type_id) in params.iter().zip(fn_call_arg_tys) {
            let param = param.read();
            collect_gen_type_id(
                ty_env,
                &mut gen_name_to_impl,
                param.ty.unwrap(),
                *arg_type_id,
            )?;
        }
    }

    if let (Some(decl_ret), Some(impl_ret)) = (fn_decl.ret_type, fn_call_ret_ty) {
        collect_gen_type_id(ty_env, &mut gen_name_to_impl, decl_ret, impl_ret)?;
    }

    let mut new_gens = Generics::new();
    for gen_name in decl_gens.iter_names() {
        if let Some(type_id) = gen_name_to_impl.get(gen_name) {
            new_gens.insert(gen_name.clone(), *type_id);
        } else {
            let err_msg_start = if let Some(adt_type_id) = fn_decl.method_adt {
                format!(
                    "Method {} in ADT {}",
                    &fn_decl.name,
                    to_string_type_id(ty_env, adt_type_id)?
                )
            } else {
                let fn_path =
                    fn_decl
                        .module
                        .clone_push(&fn_decl.name, None, Some(fn_decl.file_pos));
                format!("Function {}", to_string_path(ty_env, &fn_path))
            };
            return Err(ast_ctx.err(format!(
                "{} have declared a generic with name \"{}\". \
                The compiler was unable to infer the type of this generic for the function call. \
                Please manually specify the desired type for the generic at the function call.",
                err_msg_start, gen_name
            )));
        }
    }

    Ok(new_gens)
}

// TODO: Are there any more types needed to be matched and traversed in this
//       function when collecting type ids?
fn collect_gen_type_id(
    ty_env: &TyEnv,
    gen_name_to_impl: &mut HashMap<String, TypeId>,
    decl_type_id: TypeId,
    impl_type_id: TypeId,
) -> LangResult<()> {
    let decl_ty = ty_env.ty(decl_type_id)?;
    let impl_ty = ty_env.ty(impl_type_id)?;
    match (decl_ty, impl_ty) {
        (Ty::GenericInstance(gen_name, ..), _) | (Ty::Generic(gen_name, ..), _) => {
            gen_name_to_impl.insert(gen_name.clone(), impl_type_id);
        }

        (Ty::CompoundType(decl_inner_ty, _), Ty::CompoundType(impl_inner_ty, _)) => {
            if let (Some(decl_gens), Some(impl_gens)) = (decl_inner_ty.gens(), impl_inner_ty.gens())
            {
                for (decl_id, impl_id) in decl_gens.iter_types().zip(impl_gens.iter_types()) {
                    collect_gen_type_id(ty_env, gen_name_to_impl, *decl_id, *impl_id)?;
                }
            }
        }

        (Ty::Pointer(decl_id, _), Ty::Pointer(impl_id, _))
        | (Ty::Array(decl_id, ..), Ty::Array(impl_id, ..)) => {
            collect_gen_type_id(ty_env, gen_name_to_impl, *decl_id, *impl_id)?;
        }

        (
            Ty::Fn(decl_gens, decl_params, decl_ret, _),
            Ty::Fn(impl_gens, impl_params, impl_ret, _),
        ) => {
            for (decl_id, impl_id) in decl_gens.iter().zip(impl_gens) {
                collect_gen_type_id(ty_env, gen_name_to_impl, *decl_id, *impl_id)?;
            }
            for (decl_id, impl_id) in decl_params.iter().zip(impl_params) {
                collect_gen_type_id(ty_env, gen_name_to_impl, *decl_id, *impl_id)?;
            }
            if let (Some(decl_id), Some(impl_id)) = (decl_ret, impl_ret) {
                collect_gen_type_id(ty_env, gen_name_to_impl, *decl_id, *impl_id)?;
            }
        }

        _ => (),
    }
    Ok(())
}

/// If the given type `ty` contains generics that don't have their "names"
/// set, this function will fetch the structure and set the names if possible.
pub fn set_generic_names(ty_env: &mut TyEnv, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<()> {
    if is_tuple(ty_env, type_id)? {
        return Ok(());
    }

    let inner_ty = match ty_env.ty_clone(type_id)? {
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
    let adt = adt.read();

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
