use common::{
    error::LangError,
    path::LangPathPart,
    token::op::UnOperator,
    token::{expr::FnCall, op::UnOp},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{ty::Ty, type_id::TypeId},
};

/// Iterates through all types in the AST and replaces them with their correctly
/// solved types.
pub struct TypeUpdater {
    errors: Vec<LangError>,
}

impl TypeUpdater {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for TypeUpdater {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        let inf_type_id = match ty_env_guard.inferred_type(*type_id) {
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
                match ty_env_guard.inferred_type(actual_adt_type_id) {
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

            let inf_type_id = match ctx.ty_env.lock().unwrap().inferred_type(type_id) {
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
