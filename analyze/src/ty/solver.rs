use crate::block::BlockInfo;

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
use std::collections::{hash_map::Entry, HashMap};

pub struct TypeSolver<'a> {
    type_context: &'a mut TypeContext<'a>,

    /// Will contain all types containing generics. This will then be used to
    /// create new generic structures with the specific generic implementations.
    /// The String key is the name of the structure/type. The Ty values are the
    /// actual types containing generic implentations/instances.
    pub generic_structures: HashMap<String, Vec<Ty>>,

    errors: Vec<LangError>,
}

impl<'a> TypeSolver<'a> {
    pub fn new(type_context: &'a mut TypeContext<'a>) -> Self {
        Self {
            type_context,
            generic_structures: HashMap::default(),
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

    /// If the given type represents a type that contains generics, this function
    /// will insert those into `self.generic_structures`. This map will in a later
    /// stage be used to create all the structures containing the different
    /// generic types.
    /// This function also adds the names for the generics if they aren't already
    /// set and that information is attainable.
    fn create_generic_struct(&mut self, ty: &mut Ty) {
        // Do not create a "copy" of the actual structure type that contains the
        // generic declarations, should only create "copies" for the structures
        // that "implements" the generics.
        if ty.contains_generic() {
            return;
        }

        let ident = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if !generics.is_empty() {
                    inner_ty.to_string()
                } else {
                    return;
                }
            }

            Ty::Pointer(ty_box, ..) | Ty::Array(ty_box, ..) => {
                self.create_generic_struct(ty_box);
                return;
            }

            _ => return,
        };

        // TODO: Don't hardcode default id.
        let id = BlockInfo::DEFAULT_BLOCK_ID;

        // Set names of generics if they aren't set already.
        if let Err(err) = self.type_context.set_generic_names(ty, id) {
            self.errors.push(err);
            return;
        }

        match self.generic_structures.entry(ident) {
            Entry::Occupied(mut o) => {
                if !o.get().contains(ty) {
                    o.get_mut().push(ty.clone());
                }
            }
            Entry::Vacant(v) => {
                v.insert(vec![ty.clone()]);
            }
        }
    }
}

impl<'a> Visitor for TypeSolver<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        self.subtitute_type(ty, ctx.block_id);
        self.create_generic_struct(ty);
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
