use super::context::TypeContext;
use common::{
    error::LangError,
    token::op::UnOperator,
    token::{ast::AstToken, expr::FnCall, op::UnOp},
    traverser::TraverseContext,
    ty::ty::Ty,
    visitor::Visitor,
    BlockId,
};
use log::debug;

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

    /// Replaced the type `ty` with the preferred inferred type. If it was solvable
    /// true if return, otherwise false is returned.
    fn subtitute_type(&mut self, ty: &mut Ty, block_id: BlockId) {
        if let Err(err) = self.type_context.final_solve(ty, block_id) {
            if !self.errors.contains(&err) {
                self.errors.push(err);
            }
            return;
        }

        let mut inferred_ty = match self.type_context.inferred_type(ty, block_id) {
            Ok(inferred_ty) => inferred_ty,
            Err(err) => {
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
                return;
            }
        };

        // Converts any "UnknownInt" to i32 and "UnknownFloat" to f32.
        inferred_ty.convert_defaults();

        if inferred_ty.is_solved() || inferred_ty.contains_generic() || inferred_ty.contains_any() {
            *ty = inferred_ty;
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to resolve type {:#?} in block ID {}. Got back unsolved: {:#?}.",
                ty, block_id, inferred_ty
            ));
            self.errors.push(err);
        }
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

    fn visit_default_block(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {
        debug!("Running deep solve.");
        if let Err(err) = self.type_context.deep_solve() {
            if !self.errors.contains(&err) {
                self.errors.push(err);
            }
        }
        debug!("Done with deep solve.");
    }

    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        self.subtitute_type(ty, ctx.block_id);
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, _ctx: &TraverseContext) {
        if let Some(structure_ty) = &mut fn_call.method_adt {
            // TODO: Fix this, seems very random to fix this here.
            // The `method_structure` might possible be a pointer to the
            // structure, need to get the actual structure type in that case.
            if let Ty::Pointer(ty, ..) = structure_ty {
                *structure_ty = *ty.clone();
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {
        // TODO: Move this logic to somewhere else so that this whole function
        //       `visit_un_op()` can be removed. It doesn't feel like it should
        //       be in this file.

        // Edge case logic for ADT access. Need to figure out the index of the
        // member that is being accessed.
        if let UnOperator::AdtAccess(member_name, member_idx) = &mut un_op.operator {
            match un_op.value.get_expr_type() {
                Ok(Ty::CompoundType(inner_ty, ..)) if inner_ty.is_adt() => {
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

                Err(err) => {
                    self.errors.push(err);
                }

                _ => {
                    let err = self.type_context.analyze_context.err(format!(
                        "Expression that was ADT accessed wasn't ADT or compound, was: {:#?}",
                        un_op.value
                    ));
                    self.errors.push(err);
                }
            }
        }
    }
}
