use super::substitution_sets::SubstitutionSets;
use crate::context::AnalyzeContext;
use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{block::Function, expr::Expr},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
    BlockId,
};
use either::Either;
use log::debug;
use std::collections::HashMap;

pub struct TypeContext<'a> {
    /// Needed to look up ex. struct members.
    pub analyze_context: &'a mut AnalyzeContext,

    /// The key is the "root id" of the block in which the substitutions are done
    /// in. This will be in the scope of either a function or a structure.
    ///
    /// The sets will be updated continuously during this step and should at the
    /// end of this step be used to infer the correct types. The
    substitutions: HashMap<BlockId, SubstitutionSets>,
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            substitutions: HashMap::default(),
        }
    }

    pub fn pretty_print_subs(&self) -> String {
        format!("substitutions sets: {:#?}", self.substitutions)
    }

    /// Returns the type contained in the root node of the substitution set that
    /// contains the type `ty`. This will be the type with the highest precedence
    /// in the set containing the block with ID `block_id`.
    ///
    /// If the given type doesn't belong to a set, returns the type itself as the
    /// inferred type.
    pub fn inferred_type(&mut self, ty: &Ty, block_id: BlockId) -> LangResult<Ty> {
        let root_id = self.analyze_context.get_root_id(block_id)?;
        if let Some(sub_sets) = self.substitutions.get(&root_id) {
            let inferred_ty = sub_sets.inferred_type(ty)?;
            self.solve(&inferred_ty, root_id)
        } else {
            self.solve(ty, root_id)
        }
    }

    /// Inserts a new constraint between two types.
    /// If the types are the equal, this function will do a early Ok return.
    ///
    /// The `block_id` is used to decide which scope this contraint should be
    /// added to. It will be inseted into the "first" root block which contains
    /// the given `block_id` (this might be the `block_id` itself).
    pub fn insert_constraint(&mut self, ty_a: &Ty, ty_b: &Ty, block_id: BlockId) -> LangResult<()> {
        if ty_a == ty_b {
            return Ok(());
        }

        let root_id = self.analyze_context.get_root_id(block_id)?;

        debug!(
            "insert_constraint, root ID {} -- ty_a: {:#?}, ty_b: {:#?}",
            root_id, ty_a, ty_b
        );

        if !ty_a.is_any() && !ty_b.is_any() {
            self.unify(ty_a, ty_b, root_id)?;
        }

        Ok(())
    }

    /// "Unifies" the two types `ty_a` and `ty_b` in the scope of `root_id`.
    /// If the resulting unified type is unsolved, a `solve` is applied to try
    /// and solved it if possible.
    /// This function also inserts new constraints for any inner types.
    fn unify(&mut self, ty_a: &Ty, ty_b: &Ty, root_id: BlockId) -> LangResult<Ty> {
        let inferred_ty = self
            .substitutions
            .entry(root_id)
            .or_insert_with(SubstitutionSets::new)
            .union(ty_a, ty_b)?;

        debug!(
            "After union, ty_a: {:#?}, ty_b: {:#?}, inferred: {:#?}, root_id: {}",
            ty_a, ty_b, inferred_ty, root_id
        );

        self.insert_constraint_inner(ty_a, &inferred_ty, root_id)?;
        self.insert_constraint_inner(ty_b, &inferred_ty, root_id)?;

        self.solve(&inferred_ty, root_id)
    }

    /// Inserts constraints for potential "inner types" of types `ty_a` and `ty_b`.
    fn insert_constraint_inner(
        &mut self,
        ty_a: &Ty,
        ty_b: &Ty,
        root_id: BlockId,
    ) -> LangResult<()> {
        match (ty_a, ty_b) {
            (Ty::CompoundType(_, ty_a_gens, ..), Ty::CompoundType(_, ty_b_gens, ..)) => {
                for (ty_a_gen, ty_b_gen) in ty_a_gens.iter_types().zip(ty_b_gens.iter_types()) {
                    self.insert_constraint(ty_a_gen, ty_b_gen, root_id)?;
                }
                Ok(())
            }

            (Ty::Pointer(ty_a_inner, ..), Ty::Pointer(ty_b_inner, ..))
            | (Ty::Array(ty_a_inner, ..), Ty::Array(ty_b_inner, ..))
            | (Ty::UnknownAdtMember(ty_a_inner, ..), Ty::UnknownAdtMember(ty_b_inner, ..))
            | (Ty::UnknownAdtMethod(ty_a_inner, ..), Ty::UnknownAdtMethod(ty_b_inner, ..))
            | (
                Ty::UnknownMethodArgument(ty_a_inner, ..),
                Ty::UnknownMethodArgument(ty_b_inner, ..),
            )
            | (Ty::UnknownArrayMember(ty_a_inner, ..), Ty::UnknownArrayMember(ty_b_inner, ..)) => {
                self.insert_constraint(ty_a_inner, ty_b_inner, root_id)
            }

            _ => Ok(()),
        }
    }

    /// Given a type `ty`, tries to solve it recursively by looking at structure/
    /// function declarations. This is needed for types that can't be figured out
    /// directly from looking at the "use site" in the source code, some information
    /// needs to be fetched from somewhere else to deduce the correct type.
    ///
    /// If the given type `ty` is solved when this function is call, a early
    /// Ok is returned.
    pub fn solve(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        if ty.is_solved() {
            return Ok(ty.clone());
        }

        match ty {
            Ty::CompoundType(..) => self.solve_compound(ty, root_id),
            Ty::Pointer(..) | Ty::Array(..) => self.solve_aggregate(ty, root_id),

            Ty::Expr(..) => self.solve_expr(ty, root_id),
            Ty::Fn(..) => self.solve_fn(ty, root_id),

            Ty::UnknownAdtMember(..) => self.solve_unknown_adt_member(ty, root_id),
            Ty::UnknownAdtMethod(..) => self.solve_unknown_adt_method(ty, root_id),
            Ty::UnknownMethodArgument(..) => self.solve_unknown_method_argument(ty, root_id),
            Ty::UnknownMethodGeneric(..) => self.solve_unknown_method_generic(ty, root_id),
            Ty::UnknownArrayMember(..) => self.solve_unknown_array_member(ty, root_id),

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(ty.clone()),
        }
    }

    /// Solves compound types (i.e. types that might contain generics).
    fn solve_compound(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        let mut new_ty = ty.clone();

        let (inner_ty, generics) = if let Ty::CompoundType(inner_ty, generics, ..) = &mut new_ty {
            (inner_ty, generics)
        } else {
            unreachable!();
        };

        // Solve the generic parameters.
        for gen_ty in generics.iter_types_mut() {
            self.solve(gen_ty, root_id)?;
            *gen_ty = self.inferred_type(gen_ty, root_id)?;
        }

        // Solve the inner structure type.
        if let InnerTy::UnknownIdent(ident, id) = inner_ty {
            if self.analyze_context.is_struct(ident, *id) {
                *inner_ty = InnerTy::Struct(ident.clone());
            } else if self.analyze_context.is_enum(ident, *id) {
                *inner_ty = InnerTy::Enum(ident.clone());
            } else if self.analyze_context.is_union(ident, *id) {
                *inner_ty = InnerTy::Union(ident.clone());
            } else if self.analyze_context.is_trait(ident, *id) {
                *inner_ty = InnerTy::Trait(ident.clone());
            }
        }

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    /// Solves aggregate types (array or pointer).
    fn solve_aggregate(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        let mut new_ty = ty.clone();

        // TODO: Probably need to implement solving of the array dimension expr.
        match &mut new_ty {
            Ty::Pointer(ty_aggr, ..) | Ty::Array(ty_aggr, ..) => {
                self.solve(ty_aggr, root_id)?;
                *ty_aggr = Box::new(self.inferred_type(ty_aggr, root_id)?);
            }
            _ => unreachable!(),
        }

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    fn solve_expr(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        let mut new_ty = ty.clone();

        if let Ty::Expr(expr, ..) = &mut new_ty {
            let expr_ty = expr.get_expr_type()?;
            self.solve(&expr_ty, root_id)?;
            new_ty = self.inferred_type(&expr_ty, root_id)?;
        }

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    /// Since this function should return something, the return type of the
    /// fn that is being solved will be returned. If the return type is None,
    /// a new void type will be returned.
    fn solve_fn(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_fn: {:#?}", ty);

        if let Ty::Fn(gens, args, ret_ty, ..) = ty {
            for gen_ty in gens {
                let mut new_gen_ty = gen_ty.clone();

                self.solve(&new_gen_ty, root_id)?;
                new_gen_ty = self.inferred_type(&new_gen_ty, root_id)?;

                self.insert_constraint(&new_gen_ty, gen_ty, root_id)?;
            }

            for arg_ty in args {
                let mut new_arg_ty = arg_ty.clone();

                self.solve(&new_arg_ty, root_id)?;
                new_arg_ty = self.inferred_type(&new_arg_ty, root_id)?;

                self.insert_constraint(&new_arg_ty, arg_ty, root_id)?;
            }

            let new_ret_ty = if let Some(ret_ty) = ret_ty {
                let mut new_ret_ty = *ret_ty.clone();

                self.solve(&new_ret_ty, root_id)?;
                new_ret_ty = self.inferred_type(&new_ret_ty, root_id)?;
                self.insert_constraint(&new_ret_ty, ret_ty, root_id)?;

                new_ret_ty
            } else {
                Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None)
            };

            Ok(new_ret_ty)
        } else {
            unreachable!();
        }
    }

    fn solve_unknown_adt_member(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_unknown_adt_member: {:#?}", ty);

        let (structure_ty, member_name) = if let Ty::UnknownAdtMember(ty, member_name, ..) = &ty {
            (ty, member_name)
        } else {
            unreachable!()
        };

        let adt_ty_info = self.solve_adt_type(structure_ty, root_id)?;
        let (inner_ty, generics) = if let Some(info) = adt_ty_info {
            info
        } else {
            return self.solve_partial_adt_type(ty, structure_ty, root_id);
        };

        let mut new_ty = if let Some(adt_name) = inner_ty.get_ident() {
            self.analyze_context
                .get_adt_member(&adt_name, member_name, root_id)?
                .borrow()
                .ty
                .clone()
                .unwrap()
        } else {
            return Ok(ty.clone());
        };

        // Since this fetched the actual ADt "template" that is used by all, the
        // generics will still be the "Generics". Need to replace them with the
        // actual type for this specific use of the AST.
        new_ty.replace_generics_impl(&generics);

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    fn solve_unknown_adt_method(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_unknown_adt_method: {:#?}", ty);

        let (adt_ty, method_name, type_info) =
            if let Ty::UnknownAdtMethod(ty, method_name, _, type_info) = &ty {
                (ty, method_name, type_info)
            } else {
                unreachable!()
            };

        let adt_ty_info = self.solve_adt_type(adt_ty, root_id)?;
        let (inner_ty, generics) = if let Some(info) = adt_ty_info {
            info
        } else {
            return self.solve_partial_adt_type(ty, adt_ty, root_id);
        };

        let method = if let Some(adt_name) = inner_ty.get_ident() {
            self.analyze_context
                .get_adt_method(&adt_name, method_name, root_id)?
        } else {
            return Ok(ty.clone());
        };
        let method = method.borrow();

        let new_ty = if let Some(mut new_ty) = method.ret_type.clone() {
            new_ty.replace_generics_impl(&generics);
            if let Some(method_generics) = &TypeContext::new_method_generics(&method, type_info) {
                new_ty.replace_generics_impl(method_generics);
            }

            new_ty
        } else {
            // The return type of the method is None == Void.
            Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None)
        };

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    fn solve_unknown_method_argument(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_unknown_method_argument: {:#?}", ty);

        let (adt_ty, method_name, name_or_idx, type_info) =
            if let Ty::UnknownMethodArgument(ty, method_name, name_or_idx, _, type_info) = &ty {
                (ty, method_name, name_or_idx, type_info)
            } else {
                unreachable!()
            };

        let adt_ty_info = self.solve_adt_type(adt_ty, root_id)?;
        let (inner_ty, generics) = if let Some(info) = adt_ty_info {
            info
        } else {
            return self.solve_partial_adt_type(ty, adt_ty, root_id);
        };

        let adt_name = inner_ty.get_ident().unwrap();

        // If this is a named argument, use that name to identify the parameter
        // type and then get the index for the parameter. Otherwise use the
        // index of the argument directly.
        let actual_idx = match name_or_idx {
            Either::Left(arg_name) => self.analyze_context.get_method_param_idx(
                &adt_name,
                &method_name,
                &arg_name,
                root_id,
            )?,
            Either::Right(idx) => *idx,
        };

        let mut new_ty = self.analyze_context.get_method_param_type(
            &adt_name,
            &method_name,
            actual_idx,
            root_id,
        )?;

        let method = self
            .analyze_context
            .get_adt_method(&adt_name, method_name, root_id)?;
        let method = method.borrow();

        new_ty.replace_generics_impl(&generics);
        if let Some(method_generics) = &TypeContext::new_method_generics(&method, type_info) {
            new_ty.replace_generics_impl(method_generics);
        }

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    fn solve_unknown_method_generic(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_unknown_method_generic: {:#?}", ty);

        let (adt_ty, method_name, idx, type_info) =
            if let Ty::UnknownMethodGeneric(ty, method_name, idx, _, type_info) = &ty {
                (ty, method_name, idx, type_info)
            } else {
                unreachable!()
            };

        let adt_ty_info = self.solve_adt_type(adt_ty, root_id)?;
        let (inner_ty, generics) = if let Some(info) = adt_ty_info {
            info
        } else {
            return self.solve_partial_adt_type(ty, adt_ty, root_id);
        };

        let method = if let Some(adt_name) = inner_ty.get_ident() {
            self.analyze_context
                .get_adt_method(&adt_name, method_name, root_id)?
        } else {
            return Ok(ty.clone());
        };
        let method = method.borrow();

        let generic_name = if let Some(generic_name) = method
            .generic_names
            .as_ref()
            .map(|gens| gens.get(*idx))
            .flatten()
        {
            generic_name.clone()
        } else {
            let err = self.analyze_context.err(format!(
                "Method call specified generic at index {}. Method declaration for \"{}\" has no generic at that index.",
                idx, method_name
            ));
            return Err(err);
        };

        // Get some arbitrary data from the file_pos to create a unique ID.
        let file_pos = type_info.file_pos().unwrap();
        let id = format!(
            "R:{}-Cs:{}-Ce:{}",
            file_pos.line_start, file_pos.column_start, file_pos.column_end
        );
        let mut new_ty = Ty::GenericInstance(generic_name, id, type_info.clone());

        new_ty.replace_generics_impl(&generics);
        if let Some(method_generics) = &TypeContext::new_method_generics(&method, type_info) {
            new_ty.replace_generics_impl(method_generics);
        }

        self.insert_constraint(&new_ty, ty, root_id)?;
        Ok(new_ty)
    }

    fn solve_unknown_array_member(&mut self, ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        debug!("solve_unknown_array_member: {:#?}", ty);

        let array_ty = if let Ty::UnknownArrayMember(array_ty, ..) = &ty {
            array_ty
        } else {
            unreachable!()
        };

        self.solve(array_ty, root_id)?;

        let new_array_ty = self.inferred_type(array_ty, root_id)?;
        self.insert_constraint(&new_array_ty, array_ty, root_id)?;

        if let Ty::Array(new_member_ty, ..) = new_array_ty {
            self.insert_constraint(&new_member_ty, ty, root_id)?;
            Ok(*new_member_ty)
        } else {
            Ok(ty.clone())
        }
    }

    /// Given a potential ADT type `adt_ty`, tries to solve it. If any progress
    /// is made in solving the ADT type, it will be inserted as a new constraint.
    ///
    /// If the ADT type is completly solvable, copies of the inner type and the
    /// generics will be returned for the solvable type.
    fn solve_adt_type(
        &mut self,
        adt_ty: &Ty,
        root_id: BlockId,
    ) -> LangResult<Option<(InnerTy, Generics)>> {
        let mut new_adt_ty = adt_ty.clone();

        self.solve(&new_adt_ty, root_id)?;

        let inferred_adt_ty = self.inferred_type(&new_adt_ty, root_id)?;
        if inferred_adt_ty.is_solved() {
            new_adt_ty = inferred_adt_ty;
        } else {
            return Ok(None);
        }

        self.set_generic_names(&mut new_adt_ty, root_id)?;
        self.insert_constraint(&new_adt_ty, adt_ty, root_id)?;

        match &new_adt_ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                Ok(Some((inner_ty.clone(), generics.clone())))
            }

            // TODO: Fix this edge case. This might be a pointer to ADT since a
            //       "{this}" will cause problems. Fix the problem and remove this
            //       logic in the future.
            Ty::Pointer(ty_box, ..) => {
                let opt = if let Ty::CompoundType(inner_ty, generics, ..) = ty_box.as_ref() {
                    Some((inner_ty.clone(), generics.clone()))
                } else {
                    None
                };

                Ok(opt)
            }

            _ => Ok(None),
        }
    }

    /// A `adt_ty` might have been "half" solved, ex. it might be solved to a generic.
    /// This function can be use to partially solve the wrapping "Unknown..."
    /// types that "wraps" the `adt_ty` (ex. function call, args etc.).
    fn solve_partial_adt_type(&mut self, ty: &Ty, adt_ty: &Ty, root_id: BlockId) -> LangResult<Ty> {
        let mut new_ty = ty.clone();

        let old_adt_ty = match &mut new_ty {
            Ty::UnknownAdtMember(old_adt_ty, ..)
            | Ty::UnknownAdtMethod(old_adt_ty, ..)
            | Ty::UnknownMethodArgument(old_adt_ty, ..)
            | Ty::UnknownMethodGeneric(old_adt_ty, ..) => old_adt_ty,

            _ => return Ok(ty.clone()),
        };

        let inferred_adt_ty = self.inferred_type(adt_ty, root_id)?;

        if *old_adt_ty.as_ref() != inferred_adt_ty {
            *old_adt_ty = Box::new(inferred_adt_ty);
            self.insert_constraint(&new_ty, ty, root_id)?;

            Ok(new_ty)
        } else {
            Ok(ty.clone())
        }
    }

    /// Creates new a new `Generic` where the types will be new `GenericInstance`s.
    /// This `Generic` can be used to replace `Ty::Generic` with new
    /// `Ty::GenericInstance`s found in this returned value.
    ///
    /// This is needed to ensure that no "raw" `Ty::Generic`s are leaked outside
    /// the function body itself. I.e. this can be used to replace arguments and
    /// return values of function calls.
    pub fn new_method_generics(method: &Function, type_info: &TypeInfo) -> Option<Generics> {
        // TODO: This should be done somewhere else. This feels like a really
        //       random place to do it.
        if let Some(method_generic_names) = &method.generic_names {
            let mut method_generics = Generics::new();

            for generic_name in method_generic_names.iter() {
                // Get some arbitrary data from the file_pos to create a unique ID.
                let file_pos = type_info.file_pos().unwrap();
                let id = format!(
                    "R:{}-Cs:{}-Ce:{}",
                    file_pos.line_start, file_pos.column_start, file_pos.column_end
                );
                let ty = Ty::GenericInstance(generic_name.clone(), id, type_info.clone());

                method_generics.insert(generic_name.clone(), ty);
            }

            Some(method_generics)
        } else {
            None
        }
    }

    /// If the given type `ty` contains generics that don't have their "names"
    /// set, this function will fetch the structure and set the names if possible.
    pub fn set_generic_names(&self, ty: &mut Ty, block_id: BlockId) -> LangResult<()> {
        let (inner_ty, generics) = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

            Ty::Pointer(ty_box, ..) | Ty::Array(ty_box, ..) => {
                return self.set_generic_names(ty_box, block_id);
            }

            _ => return Ok(()),
        };

        if !generics.is_empty() && generics.len_names() == 0 && inner_ty.is_adt() {
            let ident = inner_ty.get_ident().unwrap();

            let adt = match self.analyze_context.get_adt(&ident, block_id) {
                Ok(adt) => adt,
                Err(err) => return Err(err),
            };
            let adt = adt.borrow();

            if let Some(generic_names) = &adt.generics {
                for (idx, gen_name) in generic_names.iter().enumerate() {
                    generics.insert_lookup(gen_name.clone(), idx);
                    generics.insert_name(gen_name.clone());
                }
            }
        }

        Ok(())
    }

    // TODO: Is it possible to move this function to "Expr" in some way?
    pub fn get_expr_type(&self, expr_opt: Option<&Expr>) -> LangResult<Ty> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type()
        } else {
            Err(LangError::new(
                "expr opt set to None.".into(),
                LangErrorKind::AnalyzeError,
                None,
            ))
        }
    }
}
