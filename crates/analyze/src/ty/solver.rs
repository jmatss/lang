use std::collections::{HashSet, VecDeque};

use log::{debug, log_enabled, Level};

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
        solve::{inferred_type, solve},
        substitution_sets::sub_sets_debug_print,
        to_string::to_string_type_id,
        ty::{SolveCond, Ty},
        ty_env::TyEnv,
        type_id::TypeId,
    },
};

/// Tries to solves all types given in `all_type_ids`.
pub fn solve_all(ctx: &mut TraverseCtx, all_type_ids: HashSet<TypeId>) -> LangResult<()> {
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

    // A set used for fast-lookups to see if a specific type ID is solved or not.
    //
    // The items in this set should be an exact copy of the items in the
    // `unsolved` queue.
    let mut unsolved_lookup = all_type_ids;

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

    ctx.ty_env.lock().unwrap().solve_mode = true;

    for solve_cond in solve_conds {
        while let Some(type_id) = unsolved.pop_front() {
            iter_count += 1;

            debug!(
                "solving solver -- type_id: {}, iter_count: {}",
                type_id, iter_count
            );

            solve(&ctx.ty_env, ctx.ast_ctx, type_id)?;
            let inf_type_id = inferred_type(&ctx.ty_env.lock().unwrap(), type_id)?;

            let mut ty_env_guard = ctx.ty_env.lock().unwrap();

            // During the `solve()`, new types might potentially be created and
            // inserted into the `new_type_ids`. They might have to be solved.
            for new_type_id in ty_env_guard.new_type_ids.drain() {
                unsolved.push_back(new_type_id);
                unsolved_lookup.insert(new_type_id);
            }

            let check_inf = true;
            if is_solved(&ty_env_guard, inf_type_id, check_inf, solve_cond)?
                && nested_is_solved(&ty_env_guard, type_id, check_inf, solve_cond)?
            {
                unsolved_lookup.remove(&type_id);
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
        let ty_env_guard = ctx.ty_env.lock().unwrap();
        let mut err_msg = "Unable to solve type system.".to_string();

        for type_id in unsolved {
            let inf_type_id = inferred_type(&ty_env_guard, type_id)?;

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
        let inf_type_id = inferred_type(ty_env, nested_type_id)?;
        if !is_solved(ty_env, inf_type_id, check_inf, solve_cond)? {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Iterates through all types in the AST and replaces them with their correctly
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

        sub_sets_debug_print(&ctx.ty_env.lock().unwrap());
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

        let all_types = ctx.ty_env.lock().unwrap().interner.all_types();
        if let Err(err) = solve_all(ctx, all_types) {
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
        if log_enabled!(Level::Debug) {
            self.end_debug_print(ctx);
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        let inf_type_id = match inferred_type(&ty_env_guard, *type_id) {
            Ok(inf_type_id) => inf_type_id,
            Err(err) => {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
                return;
            }
        };

        if *type_id != inf_type_id {
            match ty_env_guard.forward(*type_id, inf_type_id) {
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
            let ty_env_guard = ctx.ty_env.lock().unwrap();

            let adt_ty = match ty_env_guard.ty_clone(*adt_type_id) {
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
                match inferred_type(&ty_env_guard, actual_adt_type_id) {
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
