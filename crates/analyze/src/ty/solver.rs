use std::collections::{HashMap, HashSet};

use log::debug;

use common::{
    error::{LangError, LangResult},
    path::LangPathPart,
    token::op::UnOperator,
    token::{block::Block, expr::FnCall, op::UnOp},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::get_unsolvable,
        is::is_solved,
        replace::convert_defaults,
        solve::{inferred_type, solve, solve_all_solvable},
        substitution_sets::sub_sets_debug_print,
        to_string::to_string_type_id,
        ty::{SolveCond, Ty},
        type_id::TypeId,
    },
};

/// Tries to solve all types in the type system. If unable to solve all types,
/// a error will be returned.
///
/// Types containing "Generic" types are NOT solved by this function, they are
/// counted as solved at this stage. All logic related to generic types are
/// done after this step.
pub(crate) fn solve_all(ctx: &mut TraverseCtx) -> LangResult<()> {
    let unsolvables = solve_solvable(ctx, SolveCond::new().excl_gen_inst())?;

    if let Some(unsolvables) = solve_unsolvable(ctx, unsolvables)? {
        let ty_env_guard = ctx.ty_env.lock().unwrap();

        let mut err_msg = "Unable to solve type system.".to_string();

        for (type_id, child_type_ids) in unsolvables {
            let inf_type_id = inferred_type(&ty_env_guard, type_id)?;

            err_msg.push_str(&format!(
                "\nUnable to solve type {} ({}). Got back unsolved: {} ({}). ty:\n{:#?}",
                type_id,
                to_string_type_id(&ty_env_guard, type_id)?,
                inf_type_id,
                to_string_type_id(&ty_env_guard, inf_type_id)?,
                ty_env_guard.ty(inf_type_id)
            ));

            if !child_type_ids.is_empty() {
                let dependant_str = child_type_ids
                    .iter()
                    .map(|id| id.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                err_msg.push_str(&format!("\n  (dependant on type IDs: {})", dependant_str))
            }
        }

        Err(ctx.ast_ctx.err(err_msg))
    } else {
        Ok(())
    }
}

/// This function handles the initial solving of the types. It goes through
/// all types in the substitution sets and tries to solve them.
///
/// This function creates and returns a map of all types in the type environment.
/// The keys of the map are the type IDs and the values are all unsolvable types
/// that are contained in the specific key type ID.
///
/// Since the "parent" type might be needed to solve the children correctly,
/// this map will be used to ensure that the parent continues being considered
/// unsolved until all its chilren are solved.
fn solve_solvable(
    ctx: &mut TraverseCtx,
    solve_cond: SolveCond,
) -> LangResult<HashMap<TypeId, HashSet<TypeId>>> {
    solve_all_solvable(ctx.ty_env, &ctx.ast_ctx)?;

    let mut unsolvables = HashMap::default();

    let ty_env_guard = ctx.ty_env.lock().unwrap();
    for type_id in ty_env_guard.interner.all_types() {
        let mut nested_unsolvables = get_unsolvable(&ty_env_guard, type_id, solve_cond)?;
        if nested_unsolvables.contains(&type_id) {
            nested_unsolvables.remove(&type_id);
            unsolvables.insert(type_id, nested_unsolvables);
        } else if !nested_unsolvables.is_empty() {
            unsolvables.insert(type_id, nested_unsolvables);
        }
    }

    Ok(unsolvables)
}

/// This function will do a more thorough solve of the types given in `unsolvables`.
/// It will loop over them (potentially) multiple types until on of two points
/// are reached:
///   1. Success. All of the types in `unsolvables` have been solved.
/// or
///   2. Failure. No progress was made looping over all unsolved types. This
///      means the type system doesn't contain enough information to solve all
///      types. Returns a map of all unsolvable types and their children.
fn solve_unsolvable(
    ctx: &mut TraverseCtx,
    mut unsolvables: HashMap<TypeId, HashSet<TypeId>>,
) -> LangResult<Option<HashMap<TypeId, HashSet<TypeId>>>> {
    if unsolvables.is_empty() {
        return Ok(None);
    }

    // All unsolvable types will be iterated through for every SolveCond found
    // in `solve_conds`. It starts using the SolveCond from the start of the array
    // which should be the "weakest" solve condition and will then, if unable to
    // solve all types with this condition, continue using the next SolveCond
    // in the vector. This continues until either all types are solved or until
    // all SolveCond's have been tried and the type system isn't solvable.
    let solve_conds = [SolveCond::new().excl_gen_inst(), SolveCond::new()];

    let mut fully_solved = false;

    for solve_cond in solve_conds.iter() {
        // For every "solve condition", loop over the types multiple times if
        // needed until no progression was made in a iteration. At that point,
        // break this inner `loop` and continue with the next `solve_cond`.
        loop {
            let mut solved_this_iteration = HashSet::new();
            let start_len = unsolvables.len();

            debug!(
                "start unsolvable -- start len: {}, solve_cond: {:?}",
                start_len, solve_cond
            );

            for (type_id, child_type_ids) in unsolvables.iter() {
                debug!("solve unsolvable -- type_id: {}", type_id);

                solve(&ctx.ty_env, ctx.ast_ctx, *type_id)?;
                let inf_type_id = inferred_type(&ctx.ty_env.lock().unwrap(), *type_id)?;

                let check_inf = true;
                let is_solved_res = is_solved(
                    &ctx.ty_env.lock().unwrap(),
                    inf_type_id,
                    check_inf,
                    *solve_cond,
                )?;
                if is_solved_res {
                    let mut all_children_solved = true;
                    for child_type_id in child_type_ids {
                        let child_is_solved = is_solved(
                            &ctx.ty_env.lock().unwrap(),
                            *child_type_id,
                            check_inf,
                            *solve_cond,
                        )?;

                        if !child_is_solved {
                            all_children_solved = false;
                            break;
                        }
                    }

                    debug!(
                        "solve unsolvable -- type_id: {}, all_children_solved: {}",
                        type_id, all_children_solved
                    );

                    if all_children_solved {
                        solved_this_iteration.insert(*type_id);
                    }
                }
            }

            for solved_type_id in &solved_this_iteration {
                unsolvables.remove(solved_type_id);
            }

            if unsolvables.is_empty() {
                // No unsolved types left, all of them have been solved, SUCCESS!
                fully_solved = true;
                break;
            } else if solved_this_iteration.is_empty() {
                // No types was solved this iteration meaning that no more progress
                // can be made at this `solve_cond`. Break and start with the
                // next `solve_cond`.
                break;
            }
        }

        if fully_solved {
            break;
        }
    }

    if unsolvables.is_empty() {
        Ok(None)
    } else {
        Ok(Some(unsolvables))
    }
}

/// Iterates through all types in the token and replaces them with their correctly
/// solved types.
pub struct TypeSolver {
    errors: Vec<LangError>,
}

impl TypeSolver {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn end_debug_print(&self, ctx: &mut TraverseCtx) {
        let mut all_type_ids = ctx
            .ty_env
            .lock()
            .unwrap()
            .interner
            .all_types()
            .into_iter()
            .collect::<Vec<_>>();
        all_type_ids.sort_unstable();

        let mut all_types_string = String::new();
        for type_id in all_type_ids {
            all_types_string.push_str(&format!(
                "\ntype_id: {} - {:?}",
                type_id,
                to_string_type_id(&ctx.ty_env.lock().unwrap(), type_id)
            ));
        }

        debug!(
            "Type solving done.\nforwards: {:#?}\nall types: {}\nsubs:",
            ctx.ty_env.lock().unwrap().forwards(),
            all_types_string
        );
        sub_sets_debug_print(ctx.ty_env)
    }
}

impl Visitor for TypeSolver {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        debug!("before solving -- AST: {:#?}", &block);

        if let Err(err) = solve_all(ctx) {
            self.errors.push(err);
            self.end_debug_print(ctx);

            ctx.stop = true;
            return;
        }

        if let Err(err) = convert_defaults(ctx.ty_env) {
            self.errors.push(err);
            self.end_debug_print(ctx);

            ctx.stop = true;
            return;
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        self.end_debug_print(ctx);
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        let inf_type_id = match inferred_type(&ctx.ty_env.lock().unwrap(), *type_id) {
            Ok(inf_type_id) => inf_type_id,
            Err(err) => {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
                return;
            }
        };

        if *type_id != inf_type_id {
            match ctx.ty_env.lock().unwrap().forward(*type_id, inf_type_id) {
                Ok(_) => *type_id = inf_type_id,
                Err(err) => {
                    if !self.errors.contains(&err) {
                        self.errors.push(err);
                    }
                }
            }
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Some(adt_type_id) = &mut fn_call.method_adt {
            let adt_ty = match ctx.ty_env.lock().unwrap().ty_clone(*adt_type_id) {
                Ok(adt_ty) => adt_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Fix this, seems very random to fix this here.
            // The `method_adt` might possible be a pointer to the ADT.
            // In that case we need to "dereference" the pointer and get the
            // actual ADT type.
            if let Ty::Pointer(actual_adt_type_id, ..) = adt_ty {
                match inferred_type(&ctx.ty_env.lock().unwrap(), actual_adt_type_id) {
                    Ok(inf_adt_type_id) => *adt_type_id = inf_adt_type_id,
                    Err(err) => self.errors.push(err),
                }
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {
        // TODO: Move this logic to somewhere else so that this whole function
        //       `visit_un_op()` can be removed. It doesn't feel like it should
        //       be in this file.

        // Edge case logic for ADT access. Need to figure out the index of the
        // member that is being accessed.
        if let UnOperator::AdtAccess(member_name, member_idx) = &mut un_op.operator {
            let type_id = match un_op.value.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let inf_type_id = match inferred_type(&ctx.ty_env.lock().unwrap(), type_id) {
                Ok(inf_type_id) => inf_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let inf_ty = match ctx.ty_env.lock().unwrap().ty_clone(inf_type_id) {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            match inf_ty {
                Ty::CompoundType(inner_ty, ..) if inner_ty.is_adt() => {
                    let mut path_excluding_gens = inner_ty.get_ident().unwrap();
                    let last_part = path_excluding_gens.pop().unwrap();
                    path_excluding_gens.push(LangPathPart(last_part.0, None));

                    let idx = match ctx.ast_ctx.get_adt_member_index(
                        &ctx.ty_env.lock().unwrap(),
                        &path_excluding_gens,
                        member_name,
                    ) {
                        Ok(idx) => idx,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    *member_idx = Some(idx as u64);
                }

                _ => {
                    let err = ctx.ast_ctx.err(format!(
                        "Expression that was ADT accessed wasn't ADT or compound, was: {:#?}. \
                        Value inferred type ID: {}, inferred ty: {:#?}",
                        un_op.value, inf_type_id, inf_ty
                    ));
                    self.errors.push(err);
                }
            }
        }
    }
}
