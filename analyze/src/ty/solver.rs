use super::context::{SubResult, TypeContext};
use common::{
    error::LangError,
    token::op::UnOperator,
    token::{expr::FuncCall, op::UnOp},
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
    BlockId,
};

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

    fn subtitute_type(&mut self, ty: &mut Ty, block_id: BlockId) {
        match self.type_context.solve_substitution(ty, true, block_id) {
            SubResult::Solved(solved_ty) => {
                *ty = solved_ty;
            }

            // TODO: There might be other unsolved types other than generics
            //       inside the `unsolved_ty` which is missed when doing this
            //       check. Can this be a problem?
            // Need to allow for types that contains generics. This is because
            // they might not be solved until the instances of structs/methods
            // are created that implementes the generics.
            SubResult::UnSolved(unsolved_ty) if unsolved_ty.contains_generic() => {
                *ty = unsolved_ty;
            }

            SubResult::UnSolved(unsolved_ty) => {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to resolve type {:#?} in block ID {}. Got back unsolved: {:#?}.",
                    ty, block_id, unsolved_ty
                ));
                self.errors.push(err);
            }

            SubResult::Err(err) => {
                self.errors.push(err);
            }
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

    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        self.subtitute_type(ty, ctx.block_id);
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        if let Some(structure_ty) = &mut func_call.method_structure {
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

        // Edge case logic for struct access. Need to figure out the index
        // of the member that is being accessed.
        if let UnOperator::StructAccess(member_name, member_idx) = &mut un_op.operator {
            match un_op.value.get_expr_type() {
                // TODO: Implement for enum and interface as well.
                Ok(Ty::CompoundType(InnerTy::Struct(ref old_name), ..)) => {
                    let idx = match self.type_context.analyze_context.get_struct_member_index(
                        old_name,
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

                // TODO:
                Err(err) => {
                    self.errors.push(err);
                }

                _ => {
                    let err = self.type_context.analyze_context.err(format!(
                        "Expression that was struct accessed wasn't struct or compound, was: {:#?}",
                        un_op.value
                    ));
                    self.errors.push(err);
                }
            }
        }
    }
}
