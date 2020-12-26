use std::collections::{hash_map::Entry, HashMap};

use common::{
    error::LangError,
    token::op::UnOperator,
    token::{expr::FuncCall, op::UnOp},
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
};

use crate::BlockInfo;

use super::context::{SubResult, TypeContext};

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

    fn subtitute_type(&mut self, ty: &mut Ty, finalize: bool) {
        match self.type_context.solve_substitution(ty, finalize) {
            SubResult::Solved(sub_ty) => {
                *ty = sub_ty;
            }

            SubResult::UnSolved(un_sub_ty) if finalize => {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to resolve type {:?}. Got back unsolved: {:?}.",
                    ty, un_sub_ty
                ));
                self.errors.push(err);
            }

            SubResult::UnSolved(un_sub_ty) => *ty = un_sub_ty,

            SubResult::Err(err) => {
                self.errors.push(err);
            }
        }
    }

    /// If the given type represents a type that contains generics, this function
    /// will insert those into `self.generic_structures`. This map will in a later
    /// stage be used to create all the structures containing the different
    /// generic types.
    fn create_generic_struct(&mut self, ty: &mut Ty) {
        let (ident, inner_ty, generics) = match ty {
            Ty::CompoundType(inner_ty, generics) => {
                if !generics.is_empty() {
                    (inner_ty.to_string(), inner_ty, generics)
                } else {
                    return;
                }
            }

            Ty::Pointer(ty_box) | Ty::Array(ty_box, ..) => {
                self.create_generic_struct(ty_box);
                return;
            }

            _ => return,
        };

        // TODO: Don't hardcode default id.
        let id = BlockInfo::DEFAULT_BLOCK_ID;

        // At this point it is known that `generics` contains generics, and this
        // if-statement sees that the names are empty. Get the structure and
        // insert the names for the generics.
        if generics.len_names() == 0 {
            match inner_ty {
                InnerTy::Struct(ident) => {
                    match self.type_context.analyze_context.get_struct(ident, id) {
                        Ok(struct_) => {
                            if let Some(generic_names) = &struct_.borrow().generic_params {
                                for (idx, gen_name) in generic_names.iter().enumerate() {
                                    generics.insert_lookup(gen_name.clone(), idx);
                                    generics.insert_name(gen_name.clone());
                                }
                            }
                        }

                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                }

                _ => panic!("TODO: Implement for more types: {:#?}", ty),
            }
        }

        match self.generic_structures.entry(ident) {
            Entry::Occupied(mut o) => {
                if !o.get_mut().contains(ty) {
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

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.subtitute_type(ty, true);
        self.create_generic_struct(ty);
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        if let Some(structure_ty) = &mut func_call.method_structure {
            // TODO: Fix this, seems very random to fix this here.
            // The `method_structure` might possible be a pointer to the
            // structure, need to get the actual structure type in that case.
            if let Ty::Pointer(ty) = structure_ty {
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
