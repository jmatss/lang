use super::context::TypeContext;
use common::{
    error::{LangError, LangResult},
    token::op::UnOperator,
    token::{ast::AstToken, expr::FnCall, op::UnOp},
    traverser::TraverseContext,
    ty::ty::Ty,
    visitor::Visitor,
    BlockId, TypeId,
};
use log::{debug, warn};

/// Tried to solve all types in the type system. If unable to solve all types,
/// a error will be returned.
///
/// Types containing "Generic" types are NOT solved by this function, they are
/// counted as solved at this stage. All logic related to generic types are
/// done after this step.
pub(crate) fn solve_all(type_context: &mut TypeContext) -> Result<(), Vec<LangError>> {
    let all_unsolvables = solve_solvable(type_context).map_err(|e| vec![e])?;
    warn!("All unsolvable: {:#?}", all_unsolvables);
    solve_unsolvable(type_context, all_unsolvables)
}

/// This function handles the initial solving of the types. It goes through
/// all types in the substitution sets and tries to solve them. Any found
/// unsolved types will be returned in the resulting vector.
fn solve_solvable(type_context: &mut TypeContext) -> LangResult<Vec<(BlockId, TypeId)>> {
    let mut all_unsolvables = type_context.solve_all()?;

    let mut all_nested_unsolvables = Vec::default();
    for (root_id, unsolvable_type_id) in all_unsolvables.iter() {
        let nested_unsolvables = type_context
            .analyze_context
            .ty_env
            .get_unsolvable(*unsolvable_type_id)?;

        let mut nested_unsolvables = nested_unsolvables
            .into_iter()
            .map(|ty| (*root_id, ty))
            .collect::<Vec<_>>();

        all_nested_unsolvables.append(&mut nested_unsolvables);
    }

    all_unsolvables.append(&mut all_nested_unsolvables);
    Ok(all_unsolvables)
}

/// This function will do a more thorough solve of the unsolved types looping
/// over them (potentially) multiple types until on of two points are reached:
///  1. Success. The given `unsolved_tys` is empty, all of them have been solved.
/// or
///  2. Failure. No progress was made looping over all unsolved types. This
///     means the type system doesn't contain enough information to be solved.
fn solve_unsolvable(
    type_context: &mut TypeContext,
    mut all_unsolvables: Vec<(BlockId, TypeId)>,
) -> Result<(), Vec<LangError>> {
    let mut errors = Vec::default();

    if all_unsolvables.is_empty() {
        return Ok(());
    }

    loop {
        let mut i = 0;
        let start_len = all_unsolvables.len();

        warn!("start unsolvable -- start len: {}", start_len);

        while i < all_unsolvables.len() {
            let (root_id, type_id) = all_unsolvables.get(i).unwrap();

            warn!("++before unsolvable solve ++ type_id: {}", type_id);
            let inf_type_id = match type_context.solve(*type_id, *root_id) {
                Ok(inf_type_id) => inf_type_id,
                Err(err) => {
                    errors.push(err);
                    i += 1;
                    continue;
                }
            };

            let sub_sets = type_context.substitutions.get(root_id);

            warn!(
                "++before unsolvable is_solve ++ type_id: {}, inf_type_id: {}",
                type_id, inf_type_id
            );
            let is_solved = match type_context
                .analyze_context
                .ty_env
                .is_solved(sub_sets, inf_type_id)
            {
                Ok(res) => res,
                Err(err) => {
                    errors.push(err);
                    false
                }
            };
            warn!("++after unsolvable is_solve ++");
            let contains_generic = match type_context
                .analyze_context
                .ty_env
                .contains_generic_shallow(inf_type_id)
            {
                Ok(res) => res,
                Err(err) => {
                    errors.push(err);
                    false
                }
            };
            let contains_any = match type_context
                .analyze_context
                .ty_env
                .contains_any_shallow(inf_type_id)
            {
                Ok(res) => res,
                Err(err) => {
                    errors.push(err);
                    false
                }
            };

            warn!("unsolvable -- type_id: {}, inf_type_id: {}, is_solved: {}, contains_generic: {}, contains any: {}",
            type_id, inf_type_id, is_solved, contains_generic, contains_any);

            if is_solved || contains_generic || contains_any {
                all_unsolvables.swap_remove(i);
            } else {
                i += 1;
            }
        }

        warn!("end unsolvable -- end len: {}", all_unsolvables.len());

        // No unsolved type was solved after a whole traversal of all unsolved
        // types. There is no way to solve these types, report error and break.
        if start_len == all_unsolvables.len() {
            let mut err_msg = "Unable to solve type system.".to_string();

            for (root_id, unsolved_type_id) in all_unsolvables {
                let inf_type_id = match type_context.inferred_type(unsolved_type_id, root_id) {
                    Ok(inf_type_id) => inf_type_id,
                    Err(err) => {
                        errors.push(err);
                        continue;
                    }
                };

                err_msg.push_str(&format!(
                    "\n(block ID {}) Unable to resolve type {:#?}\nGot back unsolved: {:#?}.",
                    root_id, unsolved_type_id, inf_type_id
                ));
            }

            let err = type_context.analyze_context.err(err_msg);
            errors.push(err);
            break;
        }

        // No unsolved types left, all of them have been solved, success!
        if all_unsolvables.is_empty() {
            break;
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Iterates through all types in the token and replaces them with their correctly
/// solved types.
pub struct TypeSolver<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,
    errors: Vec<LangError>,
}

impl<'a, 'tctx> TypeSolver<'a, 'tctx> {
    pub fn new(type_context: &'a mut TypeContext<'tctx>) -> Self {
        Self {
            type_context,
            errors: Vec::default(),
        }
    }

    /// Replaced the type `ty` with the preferred inferred type.
    fn subtitute_type(&mut self, type_id: &mut TypeId, block_id: BlockId) -> LangResult<()> {
        warn!("START SUBSTITUTE");
        let root_id = self.type_context.analyze_context.get_root_id(block_id)?;
        let inf_type_id = self.type_context.solve(*type_id, root_id)?;

        // Converts any "UnknownInt" to i32 and "UnknownFloat" to f32.
        self.type_context
            .analyze_context
            .ty_env
            .convert_defaults(inf_type_id)?;

        let sub_sets = self.type_context.substitutions.get(&root_id);
        if let Some(sub_sets) = sub_sets {
            sub_sets.debug_print(&self.type_context.analyze_context.ty_env);
        } else {
            warn!("cur sub_sets None.");
        }

        let is_solved = self
            .type_context
            .analyze_context
            .ty_env
            .is_solved(sub_sets, inf_type_id)?;

        let contains_generic = self
            .type_context
            .analyze_context
            .ty_env
            .contains_generic_shallow(inf_type_id)?;

        let contains_any = self
            .type_context
            .analyze_context
            .ty_env
            .contains_any_shallow(inf_type_id)?;

        if !(is_solved || contains_generic || contains_any) {
            let ty = self.type_context.analyze_context.ty_env.ty(*type_id)?;

            let ty_str = self
                .type_context
                .analyze_context
                .ty_env
                .to_string_debug(*type_id)?;

            let inferred_ty = self.type_context.analyze_context.ty_env.ty(inf_type_id)?;

            let inferred_ty_str = self
                .type_context
                .analyze_context
                .ty_env
                .to_string_debug(inf_type_id)?;

            let file_pos = self.type_context.analyze_context.ty_env.file_pos(*type_id);

            let inferred_file_pos = self
                .type_context
                .analyze_context
                .ty_env
                .file_pos(inf_type_id);

            warn!(
                "Unable to solve type:\n  {:?} ({} - {:?}).\nEnded up with inferred type:\n{:?} ({} - {:?})\n-- solved_type_id: {} --\nis_solved: {}, contains_generic: {}, contains_any: {}",
                ty,
                type_id,
                file_pos,
                inferred_ty_str,
                inf_type_id,
                inferred_file_pos,
                inf_type_id,
                is_solved,
                contains_generic,
                contains_any
            );

            return Err(self.type_context.analyze_context.err(format!(
                "Unable to solve type:\n  {:?} ({} - {:?}).\nEnded up with inferred type:\n{:?} ({} - {:?})\n--\nis_solved: {}, contains_generic: {}, contains_any: {}",
                inferred_ty,
                type_id,
                file_pos,
                inferred_ty_str,
                inf_type_id,
                inferred_file_pos,
                is_solved,
                contains_generic,
                contains_any
            )));
        }

        let inf_ty = self
            .type_context
            .analyze_context
            .ty_env
            .ty(inf_type_id)?
            .clone();

        self.type_context
            .analyze_context
            .ty_env
            .update(*type_id, inf_ty)?;

        warn!(
            "solve -- old_type_id: {}, new_type_id: {}",
            type_id, inf_type_id
        );

        //*type_id = inferred_type_id;
        Ok(())
    }
}

impl<'a, 'tctx> Visitor for TypeSolver<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseContext) {
        debug!("AST: {:#?}", &ast_token);
        if self.errors.is_empty() {
            if let Err(err) = self.type_context.promote_all() {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
                ctx.stop = true;
                return;
            }
        }

        if let Err(errs) = solve_all(self.type_context) {
            for err in errs {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }
            self.type_context.pretty_print_subs();
            let mut s = String::new();
            for type_id in self.type_context.analyze_context.ty_env.all_types() {
                s.push_str(&format!(
                    "\nType ID: {}\nTy: {:#?}",
                    type_id,
                    self.type_context.analyze_context.ty_env.ty(type_id)
                ));
            }
            warn!("abc123{}", s);
            ctx.stop = true;
        }

        if self.errors.is_empty() {
            if let Err(err) = self.type_context.promote_all() {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
                ctx.stop = true;
                return;
            }
        }
    }

    fn visit_end(&mut self, _ctx: &mut TraverseContext) {
        debug!("Type solving done.\nSubs:",);
        self.type_context.pretty_print_subs();
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseContext) {
        if let Err(err) = self.subtitute_type(type_id, ctx.block_id) {
            if !self.errors.contains(&err) {
                self.errors.push(err);
            }
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, _ctx: &mut TraverseContext) {
        if let Some(adt_type_id) = &mut fn_call.method_adt {
            let adt_ty = match self.type_context.analyze_context.ty_env.ty(*adt_type_id) {
                Ok(adt_ty) => adt_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Fix this, seems very random to fix this here.
            // The `method_structure` might possible be a pointer to the
            // structure, need to get the actual structure type in that case.
            if let Ty::Pointer(actual_adt_type_id, ..) = adt_ty {
                *adt_type_id = *actual_adt_type_id;
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseContext) {
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

            let inferred_type_id = match self.type_context.inferred_type(type_id, ctx.block_id) {
                Ok(inferred_type_id) => inferred_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let inferred_ty = match self
                .type_context
                .analyze_context
                .ty_env
                .ty(inferred_type_id)
            {
                Ok(ty) => ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            match inferred_ty {
                Ty::CompoundType(inner_ty, ..) if inner_ty.is_adt() => {
                    let old_name = inner_ty.get_ident().unwrap();

                    let idx = match self.type_context.analyze_context.get_adt_member_index(
                        &old_name,
                        member_name,
                        ctx.block_id,
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
                    let err = self.type_context.analyze_context.err(format!(
                        "Expression that was ADT accessed wasn't ADT or compound, was: {:#?}. \
                        Value inferred type ID: {}, inferred ty: {:#?}",
                        un_op.value, inferred_type_id, inferred_ty
                    ));
                    self.errors.push(err);
                }
            }
        }
    }
}
