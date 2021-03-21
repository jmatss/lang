use crate::context::AnalyzeContext;
use backtrace::Backtrace;
use common::{
    error::{LangError, LangErrorKind, LangResult},
    path::{LangPath, LangPathPart},
    token::{block::Fn, expr::Expr},
    ty::{generics::Generics, inner_ty::InnerTy, substitution_sets::SubstitutionSets, ty::Ty},
    type_info::TypeInfo,
    BlockId, TypeId,
};
use either::Either;
use log::{debug, info, warn};
use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
};

/// A enum representing the status/progress of solving a ADT type.
enum AdtSolveStatus {
    /// The given ADT type was solved. Return the path and generics of the solved ADT.
    Solved(LangPath, Generics),

    /// Some progress was made, but the ADT wasn't solved. Return the progressed
    /// TypeId (not the original).
    Progress(TypeId),

    /// No progress made.
    NoProgress,
}

pub struct TypeContext<'a> {
    /// Needed to look up ex. struct members.
    pub analyze_context: &'a mut AnalyzeContext,

    /// The key is the "root id" of the block in which the substitutions are done
    /// in. This will be in the scope of either a function or a structure.
    ///
    /// The sets will be updated continuously during this step and should at the
    /// end of this step be used to infer the correct types. The
    pub(crate) substitutions: HashMap<BlockId, SubstitutionSets>,
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            substitutions: HashMap::default(),
        }
    }

    pub fn pretty_print_subs(&self) {
        for sets in self.substitutions.values() {
            sets.debug_print(&self.analyze_context.ty_env);
        }
    }

    /// Returns all root block IDs for which the type with ID `type_id` belongs
    /// to in the substitution sets. If the given type doesn't exist in any set,
    /// None is returned.
    pub fn root_id(&mut self, type_id: TypeId) -> Option<HashSet<BlockId>> {
        let mut root_ids = HashSet::default();
        for (root_id, sets) in &mut self.substitutions {
            if sets.contains_ty(type_id) {
                root_ids.insert(*root_id);
            }
        }

        if !root_ids.is_empty() {
            Some(root_ids)
        } else {
            None
        }
    }

    /// Returns the type contained in the root node of the substitution set that
    /// contains the type `type_id`. This will be the type with the highest
    /// precedence in the set containing the block with ID `block_id`.
    ///
    /// If the given type doesn't belong to a set, returns the type itself as the
    /// inferred type.
    pub fn inferred_type(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<TypeId> {
        let root_id = self.analyze_context.get_root_id(block_id)?;
        if let Some(sub_sets) = self.substitutions.get(&root_id) {
            let inf_type_id = sub_sets.inferred_type(type_id)?;
            warn!("inf_type_id: {}", inf_type_id);
            self.solve(inf_type_id, root_id)
        } else {
            self.solve(type_id, root_id)
        }
    }

    /// Inserts a new constraint between two types.
    /// If the types are the equal, this function will do a early Ok return.
    ///
    /// The `block_id` is used to decide which scope this contraint should be
    /// added to. It will be inseted into the "first" root block which contains
    /// the given `block_id` (this might be the `block_id` itself).
    pub fn insert_constraint(
        &mut self,
        type_id_a: TypeId,
        type_id_b: TypeId,
        block_id: BlockId,
    ) -> LangResult<()> {
        if type_id_a == type_id_b {
            return Ok(());
        }

        let root_id = self.analyze_context.get_root_id(block_id)?;

        debug!(
            "insert_constraint, root ID {} -- type_id_a: {}, ty_a: {:#?}\ntype_id_b: {}, ty_b: {:#?}",
            root_id,
            type_id_a,
            self.analyze_context.ty_env.ty(type_id_a)?,
            type_id_b,
            self.analyze_context.ty_env.ty(type_id_b)?,
        );

        if !self.analyze_context.ty_env.is_any(type_id_a)?
            && !self.analyze_context.ty_env.is_any(type_id_b)?
        {
            self.unify(type_id_a, type_id_b, root_id)?;
        }

        Ok(())
    }

    /// "Unifies" the two types `type_id_a` and `type_id_b` in the scope of `root_id`.
    /// If the resulting unified type is unsolved, a `solve` is applied to try
    /// and solved it if possible.
    /// This function also inserts new constraints for any inner types.
    fn unify(
        &mut self,
        type_id_a: TypeId,
        type_id_b: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let inferred_type_id = self
            .substitutions
            .entry(root_id)
            .or_insert_with(SubstitutionSets::new)
            .union(&self.analyze_context.ty_env, type_id_a, type_id_b)?;

        debug!(
            "After union, type_id_a: {}, type_id_b: {}, inferred: {}, root_id: {}",
            type_id_a, type_id_b, inferred_type_id, root_id
        );

        self.insert_constraint_inner(type_id_a, inferred_type_id, root_id)?;
        self.insert_constraint_inner(type_id_b, inferred_type_id, root_id)?;

        Ok(inferred_type_id)
    }

    /// Inserts constraints for potential "inner types" of types `ty` and `inferred_ty`.
    fn insert_constraint_inner(
        &mut self,
        type_id: TypeId,
        inferred_type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<()> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();
        let inferred_ty = self.analyze_context.ty_env.ty(inferred_type_id)?.clone();

        match (ty, inferred_ty) {
            (Ty::CompoundType(_, ty_a_gens, ..), Ty::CompoundType(_, ty_b_gens, ..)) => {
                for (ty_a_gen, ty_b_gen) in ty_a_gens.iter_types().zip(ty_b_gens.iter_types()) {
                    self.insert_constraint_inner(*ty_a_gen, *ty_b_gen, root_id)?;
                    self.insert_constraint(*ty_a_gen, *ty_b_gen, root_id)?;
                }
                Ok(())
            }

            (Ty::Pointer(ty_a_inner, ..), Ty::Pointer(ty_b_inner, ..))
            | (Ty::Array(ty_a_inner, ..), Ty::Array(ty_b_inner, ..))
            | (Ty::UnknownArrayMember(ty_a_inner, ..), Ty::UnknownArrayMember(ty_b_inner, ..)) => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner, root_id)?;
                self.insert_constraint(ty_a_inner, ty_b_inner, root_id)
            }

            (
                Ty::UnknownAdtMethod(ty_a_inner, a_name, ..),
                Ty::UnknownAdtMethod(ty_b_inner, b_name, ..),
            )
            | (
                Ty::UnknownAdtMember(ty_a_inner, a_name, ..),
                Ty::UnknownAdtMember(ty_b_inner, b_name, ..),
            ) if a_name == b_name => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner, root_id)?;
                self.insert_constraint(ty_a_inner, ty_b_inner, root_id)
            }

            (
                Ty::UnknownMethodArgument(ty_a_inner, a_name, a_idx_or_name, ..),
                Ty::UnknownMethodArgument(ty_b_inner, b_name, b_idx_or_name, ..),
            ) if a_name == b_name && a_idx_or_name == b_idx_or_name => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner, root_id)?;
                self.insert_constraint(ty_a_inner, ty_b_inner, root_id)
            }

            (
                Ty::UnknownMethodGeneric(ty_a_inner, a_name, a_idx, ..),
                Ty::UnknownMethodGeneric(ty_b_inner, b_name, b_idx, ..),
            ) if a_name == b_name && a_idx == b_idx => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner, root_id)?;
                self.insert_constraint(ty_a_inner, ty_b_inner, root_id)
            }

            _ => Ok(()),
        }
    }

    /// Iterates through all types found in the TypeEnvironment and tries to
    /// solve them. Any types that wasn't solvable will be returned.
    pub fn solve_all(&mut self) -> LangResult<Vec<(BlockId, TypeId)>> {
        let mut unsolvables = Vec::default();

        for type_id in self.analyze_context.ty_env.all_types() {
            if let Some(root_ids) = self.root_id(type_id) {
                for root_id in root_ids {
                    let inf_type_id = self.solve(type_id, root_id)?;
                    let sub_sets = self.substitutions.get(&root_id);

                    let is_solved = self
                        .analyze_context
                        .ty_env
                        .is_solved(sub_sets, inf_type_id)?;

                    let contains_generic = self
                        .analyze_context
                        .ty_env
                        .contains_generic_shallow(inf_type_id)?;

                    let contains_any = self
                        .analyze_context
                        .ty_env
                        .contains_any_shallow(inf_type_id)?;

                    warn!(
                        "solve_all -- type_id: {}, inf_type_id: {}, is_solved: {}, contains_generic: {}, contains_any: {}",
                        type_id, inf_type_id, is_solved, contains_generic, contains_any
                    );

                    if !(is_solved || contains_generic || contains_any) {
                        unsolvables.push((root_id, type_id));
                    }
                }
            }
        }

        Ok(unsolvables)
    }

    /// Iterates through all types found in the TypeEnvironment and tries to
    /// promote them in their respective substitution sets.
    /// This process check that if a given type is solved, it will be promoted
    /// to the root of its set if the current root isn't solved.
    pub fn promote_all(&mut self) -> LangResult<()> {
        for type_id in self.analyze_context.ty_env.all_types() {
            if let Some(root_ids) = self.root_id(type_id) {
                for root_id in root_ids {
                    let sets = self.substitutions.get_mut(&root_id).unwrap();
                    warn!("promote: {}", type_id);
                    sets.promote(&self.analyze_context.ty_env, type_id)?;
                }
            }
        }
        Ok(())
    }

    /// Iterates through all types in the substitution sets and solves them.
    /// Also tries to solve any nested types and map them to the correct types.
    pub fn deep_solve(&mut self) -> LangResult<()> {
        for (root_id, sets) in self.substitutions.clone() {
            for type_id in sets.all_types() {
                self.solve(type_id, root_id)?;
                let inferred_type_id = self.inferred_type(type_id, root_id)?;
                self.insert_constraint_inner(type_id, inferred_type_id, root_id)?;
            }
        }
        Ok(())
    }

    pub fn all_types(&self) -> HashMap<BlockId, HashSet<TypeId>> {
        let mut type_ids = HashMap::default();
        for (id, sets) in &self.substitutions {
            type_ids.insert(*id, sets.all_types());
        }
        type_ids
    }

    pub fn solve_deep(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<TypeId> {
        let root_id = self.analyze_context.get_root_id(block_id)?;
        if let Some(sets) = self.substitutions.get_mut(&root_id) {
            for type_id in sets.types_traversed_to_root(type_id).iter() {
                self.solve(*type_id, root_id)?;
            }
        }
        self.solve(type_id, root_id)
    }

    // TODO: This is slow but currently needed in some cases. Find a better way.
    pub fn final_solve(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<TypeId> {
        let root_id = self.analyze_context.get_root_id(block_id)?;
        if let Some(sets) = self.substitutions.get_mut(&root_id) {
            // A lot faster but not good enough, doesn't solve all test files.
            for type_id in sets.types_traversed_to_root(type_id).iter() {
                self.solve(*type_id, root_id)?;
            }

            // for ty in sets.all_types() {
            //     self.solve(&ty, root_id)?;
            // }
        }
        self.solve(type_id, root_id)
    }

    /// Given a type `type_id`, tries to solve it recursively by looking at structure/
    /// function declarations. This is needed for types that can't be figured out
    /// directly from looking at the "use site" in the source code, some information
    /// needs to be fetched from somewhere else to deduce the correct type.
    pub fn solve(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        if type_id != TypeId(6) {
            warn!("solve_ty: {}", type_id);
        } else {
            warn!("solve_ty: {}, backtrace: {:#?}", type_id, Backtrace::new());
        }

        if let Some(sets) = self.substitutions.get_mut(&root_id) {
            //sets.promote(&self.analyze_context.ty_env, type_id)?;

            warn!(
                "A solve_ty: {}, contains_ty: {}, is_root: {}",
                type_id,
                sets.contains_ty(type_id),
                sets.is_root(type_id)
            );
            if sets.contains_ty(type_id) && !sets.is_root(type_id) {
                // The given `type_id` is NOT the root of its set, look at the
                // root instead and try to solve it recursively. This will "solve"
                // the current `type_id` implicitly.
                let inf_type_id = sets.inferred_type(type_id)?;
                warn!("B solve_ty: {}, inf_ty: {}", type_id, inf_type_id);
                let solved_type_id = self.solve(inf_type_id, root_id)?;
                self.insert_constraint_inner(type_id, solved_type_id, root_id)?;
                warn!(
                    "after B solved_ty: {}, inf_ty: {}",
                    solved_type_id, inf_type_id
                );
                return Ok(solved_type_id);
            } else if self.analyze_context.ty_env.is_solved(Some(sets), type_id)? {
                warn!("C solve_ty: {}", type_id);
                // The given `type_id` is the root of its set and is solved.
                return Ok(type_id);
            }
        }

        warn!("D solve_ty: {}", type_id);

        // The given `type_id` is the root of its set and isn't solved (from
        // looking in the sets). Try to solve it "manually" in the logic below.
        self.solve_manual(type_id, root_id)
    }

    /// Given a type `type_id`, tries to solve it. This function does NOT look
    /// in either the `substition_sets` or its inferred type to solve this
    /// specific `type_id` (use `solve()` in that case). This is only true for
    /// the first "layer" of the solve, any inner types will be solved recursively
    /// with using lookups etc.
    fn solve_manual(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?;
        match ty {
            Ty::CompoundType(..) => self.solve_compound(type_id, root_id),
            Ty::Pointer(..) | Ty::Array(..) => self.solve_aggregate(type_id, root_id),

            Ty::Expr(..) => self.solve_expr(type_id, root_id),
            Ty::Fn(..) => self.solve_fn(type_id, root_id),

            Ty::UnknownAdtMember(..) => self.solve_unknown_adt_member(type_id, root_id),
            Ty::UnknownAdtMethod(..) => self.solve_unknown_adt_method(type_id, root_id),
            Ty::UnknownMethodArgument(..) => self.solve_unknown_method_argument(type_id, root_id),
            Ty::UnknownMethodGeneric(..) => self.solve_unknown_method_generic(type_id, root_id),
            Ty::UnknownArrayMember(..) => self.solve_unknown_array_member(type_id, root_id),

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(type_id),
        }
    }

    /// Solves compound types (i.e. types that might contain generics).
    fn solve_compound(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        let (inner_ty, generics, type_info) =
            if let Ty::CompoundType(inner_ty, generics, type_info) = ty {
                (inner_ty, generics, type_info)
            } else {
                unreachable!();
            };

        // Solve the generic parameters.
        for gen_type_id in generics.iter_types() {
            self.solve(*gen_type_id, root_id)?;
        }

        // Solve the inner structure type.
        let inf_type_id = if let InnerTy::UnknownIdent(path, block_id) = inner_ty {
            warn!("solve unknown_ident, type_id: {}, path: {}", type_id, path);

            let full_path_opt = if let Ok(full_path) = self
                .analyze_context
                .calculate_adt_full_path(&path, block_id)
            {
                Some(full_path)
            } else if let Ok(full_path) = self
                .analyze_context
                .calculate_trait_full_path(&path, block_id)
            {
                Some(full_path)
            } else {
                None
            };

            warn!("solve unknown_ident, full_path_opt: {:#?}", full_path_opt);

            if let Some(mut full_path) = full_path_opt {
                // Add the already (possible) existing generics to the full path.
                let last_part = full_path.pop().unwrap();
                full_path.push(LangPathPart(last_part.0, Some(generics.clone())));

                let new_inner_type_id = if self.analyze_context.is_struct(&full_path, block_id) {
                    Some(InnerTy::Struct(full_path))
                } else if self.analyze_context.is_enum(&full_path, block_id) {
                    Some(InnerTy::Enum(full_path))
                } else if self.analyze_context.is_union(&full_path, block_id) {
                    Some(InnerTy::Union(full_path))
                } else if self.analyze_context.is_trait(&full_path, block_id) {
                    Some(InnerTy::Trait(full_path))
                } else {
                    None
                };

                warn!(
                    "solve unknown_ident, new_inner_type_id: {:#?}",
                    new_inner_type_id
                );

                if let Some(new_inner_type_id) = new_inner_type_id {
                    let new_type_id = self.analyze_context.ty_env.id(&Ty::CompoundType(
                        new_inner_type_id,
                        generics,
                        type_info,
                    ))?;
                    self.insert_constraint(type_id, new_type_id, root_id)?;

                    new_type_id
                } else {
                    type_id
                }
            } else {
                type_id
            }
        } else {
            type_id
        };

        Ok(inf_type_id)
    }

    /// Solves aggregate types (array or pointer).
    fn solve_aggregate(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        let mut ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        // TODO: Probably need to implement solving of the array dimension expr.
        let inf_type_id = match &mut ty {
            Ty::Pointer(aggr_type_id, ..) | Ty::Array(aggr_type_id, ..) => {
                self.solve(*aggr_type_id, root_id)?;
                let inferred_type_id = self.inferred_type(*aggr_type_id, root_id)?;

                if &inferred_type_id != aggr_type_id {
                    *aggr_type_id = inferred_type_id;

                    let new_type_id = self.analyze_context.ty_env.id(&ty)?;
                    self.insert_constraint(type_id, new_type_id, root_id)?;

                    new_type_id
                } else {
                    type_id
                }
            }
            _ => unreachable!(),
        };

        Ok(inf_type_id)
    }

    fn solve_expr(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty_mut(type_id)?;

        let inf_type_id = if let Ty::Expr(expr, ..) = ty {
            let expr_type_id = expr.get_expr_type()?;
            self.solve(expr_type_id, root_id)?
        } else {
            type_id
        };

        Ok(inf_type_id)
    }

    fn solve_fn(&mut self, type_id: TypeId, root_id: BlockId) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        if let Ty::Fn(gens, params, ret_type_id_opt, ..) = ty {
            for type_id_i in gens.iter().chain(params.iter()) {
                self.solve(*type_id_i, root_id)?;
            }

            if let Some(ret_type_id) = ret_type_id_opt {
                self.solve(ret_type_id, root_id)?;
            }
        } else {
            unreachable!();
        }

        // TODO: Does this need to be returned?
        Ok(type_id)
    }

    fn solve_unknown_adt_member(
        &mut self,
        type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        debug!("solve_unknown_adt_member: {:#?}", ty);
        warn!("solve_unknown_adt_member: {:#?}", ty);

        let (adt_type_id, member_name) =
            if let Ty::UnknownAdtMember(adt_type_id, member_name, ..) = ty {
                (adt_type_id, member_name)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) = match self.solve_adt_type(adt_type_id, root_id)? {
            AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),

            // TODO: Should `Progress` return a `unknown_adt_member` containing
            //       the new `inf_adt_type_id` instead of the old one?
            AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
        };

        let new_type_id = self
            .analyze_context
            .get_adt_member(
                &adt_path,
                &member_name,
                root_id,
                self.analyze_context.ty_env.file_pos(adt_type_id).cloned(),
            )?
            .borrow()
            .ty
            .clone()
            .unwrap();

        // TODO: Is this needed? Does this do anything atm?
        // Replace potential generics with impls from the solved ADT type.
        self.analyze_context
            .ty_env
            .replace_generics_impl(new_type_id, &adt_gens)?;

        warn!(
            "solve_unknown_adt_member -- inserting constraint. type_id: {}, new_type_id: {}",
            type_id, new_type_id
        );

        self.insert_constraint(type_id, new_type_id, root_id)?;
        Ok(new_type_id)
    }

    fn solve_unknown_adt_method(
        &mut self,
        type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        debug!("solve_unknown_adt_method: {:#?}", ty);

        let (adt_type_id, method_name, fn_call_gens_vec, type_info) =
            if let Ty::UnknownAdtMethod(ty, method_name, fn_call_gens_vec, _, type_info) = ty {
                (ty, method_name, fn_call_gens_vec, type_info)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) = match self.solve_adt_type(adt_type_id, root_id)? {
            AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),

            // TODO: Should `Progress` return a `unknown_adt_member` containing
            //       the new `inf_adt_type_id` instead of the old one?
            AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
        };

        let method = self
            .analyze_context
            .get_method(&adt_path, &method_name, root_id)?;
        let method = method.borrow();
        let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

        // Create/get the generics for the function call and replace potential
        // generics in the return type of "this function call".
        let fn_call_gens = if fn_call_gens_vec.is_empty() {
            if let Some(method_generics) = self.new_method_generics(&method, &type_info)? {
                method_generics
            } else {
                Generics::empty()
            }
        } else {
            let mut fn_call_gens = Generics::new();
            for (gen_name, gen_unknown_ty) in fn_gens.iter_names().zip(fn_call_gens_vec) {
                fn_call_gens.insert(gen_name.clone(), gen_unknown_ty);
            }
            fn_call_gens
        };

        let new_type_id = if let Some(new_type_id) = method.ret_type {
            self.analyze_context
                .ty_env
                .replace_generics_impl(new_type_id, &fn_call_gens)?;
            self.analyze_context
                .ty_env
                .replace_generics_impl(new_type_id, &adt_gens)?;
            new_type_id
        } else {
            // The return type of the method is None == Void.
            self.analyze_context
                .borrow_mut()
                .ty_env
                .id(&Ty::CompoundType(
                    InnerTy::Void,
                    Generics::empty(),
                    TypeInfo::None,
                ))?
        };

        warn!(
            "solve_unknown_adt_method -- inserting constraint. type_id: {}, new_type_id: {}",
            type_id, new_type_id
        );

        self.insert_constraint(type_id, new_type_id, root_id)?;
        Ok(new_type_id)
    }

    fn solve_unknown_method_argument(
        &mut self,
        type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        debug!("solve_unknown_method_argument: {:#?}", ty);

        let (adt_type_id, method_name, fn_call_gens_vec, name_or_idx, type_info) =
            if let Ty::UnknownMethodArgument(
                type_id,
                method_name,
                fn_call_gens_vec,
                name_or_idx,
                _,
                type_info,
            ) = ty
            {
                (
                    type_id,
                    method_name,
                    fn_call_gens_vec,
                    name_or_idx,
                    type_info,
                )
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) = match self.solve_adt_type(adt_type_id, root_id)? {
            AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),

            // TODO: Should `Progress` return a `unknown_adt_member` containing
            //       the new `inf_adt_type_id` instead of the old one?
            AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
        };

        let actual_idx = match name_or_idx {
            Either::Left(arg_name) => self.analyze_context.get_method_param_idx(
                &adt_path,
                &method_name,
                &arg_name,
                root_id,
            )?,
            Either::Right(idx) => idx,
        };

        let new_type_id = self.analyze_context.get_method_param_type(
            &adt_path,
            &method_name,
            actual_idx,
            root_id,
        )?;

        let method = self
            .analyze_context
            .get_method(&adt_path, &method_name, root_id)?;
        let method = method.borrow();
        let fn_gens = method.generics.clone().unwrap_or_else(Generics::empty);

        // Create/get the generics for the function call and replace potential
        // generics in the return type of "this function call".
        let fn_call_gens = if fn_call_gens_vec.is_empty() {
            if let Some(method_gens) = self.new_method_generics(&method, &type_info)? {
                method_gens
            } else {
                Generics::empty()
            }
        } else {
            let mut fn_call_gens = Generics::new();
            for (gen_name, gen_unknown_ty) in fn_gens.iter_names().zip(fn_call_gens_vec) {
                fn_call_gens.insert(gen_name.clone(), gen_unknown_ty);
            }
            fn_call_gens
        };

        self.analyze_context
            .ty_env
            .replace_generics_impl(new_type_id, &fn_call_gens)?;
        self.analyze_context
            .ty_env
            .replace_generics_impl(new_type_id, &adt_gens)?;

        self.insert_constraint(type_id, new_type_id, root_id)?;
        Ok(new_type_id)
    }

    fn solve_unknown_method_generic(
        &mut self,
        type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        debug!("solve_unknown_method_generic: {:#?}", ty);

        let (adt_type_id, method_name, gen_idx_or_name, type_info) =
            if let Ty::UnknownMethodGeneric(
                adt_type_id,
                method_name,
                gen_idx_or_name,
                _,
                type_info,
            ) = ty
            {
                (adt_type_id, method_name, gen_idx_or_name, type_info)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) = match self.solve_adt_type(adt_type_id, root_id)? {
            AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),

            // TODO: Should `Progress` return a `unknown_adt_member` containing
            //       the new `inf_adt_type_id` instead of the old one?
            AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
        };

        let method = self
            .analyze_context
            .get_method(&adt_path, &method_name, root_id)?;
        let method = method.borrow();

        let gen_name = match gen_idx_or_name {
            Either::Left(idx) => {
                if let Some(generic_name) = method
                    .generics
                    .as_ref()
                    .map(|gens| gens.get_name(idx))
                    .flatten()
                {
                    generic_name
                } else {
                    let err = self.analyze_context.err(format!(
                        "Method call specified generic at index {}. \
                        Method declaration for \"{}\" has no generic at that index.",
                        gen_idx_or_name, method_name
                    ));
                    return Err(err);
                }
            }
            Either::Right(gen_name) => gen_name,
        };

        // Get some arbitrary data from the file_pos to create a unique ID.
        let file_pos = type_info.file_pos().unwrap();
        let id = format!(
            "R:{}-Cs:{}-Ce:{}",
            file_pos.line_start, file_pos.column_start, file_pos.column_end
        );

        let new_type_id = self.analyze_context.ty_env.id(&Ty::GenericInstance(
            gen_name,
            id,
            type_info.clone(),
        ))?;

        self.analyze_context
            .ty_env
            .replace_generics_impl(new_type_id, &adt_gens)?;
        if let Some(method_generics) = &self.new_method_generics(&method, &type_info)? {
            self.analyze_context
                .ty_env
                .replace_generics_impl(new_type_id, method_generics)?;
        }

        self.insert_constraint(type_id, new_type_id, root_id)?;
        Ok(new_type_id)
    }

    fn solve_unknown_array_member(
        &mut self,
        member_type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let ty = self.analyze_context.ty_env.ty(member_type_id)?.clone();

        debug!("solve_unknown_array_member: {:#?}", ty);

        let arr_type_id = if let Ty::UnknownArrayMember(arr_type_id, ..) = ty {
            arr_type_id
        } else {
            unreachable!()
        };

        let new_arr_type_id = self.solve(arr_type_id, root_id)?;
        if arr_type_id == new_arr_type_id {
            // No progress made.
            return Ok(member_type_id);
        }

        self.insert_constraint(arr_type_id, new_arr_type_id, root_id)?;

        let new_arr_ty = self.analyze_context.ty_env.ty(new_arr_type_id)?.clone();
        if let Ty::Array(new_member_type_id, ..) = new_arr_ty {
            self.insert_constraint(new_member_type_id, member_type_id, root_id)?;
            Ok(new_member_type_id)
        } else {
            Ok(member_type_id)
        }
    }

    /// Given a potential ADT type `adt_type_id`, tries to solve it.
    /// Returns a `AdtSolveStatus` enum indicating if any progress was made
    /// solving the ADT type.
    fn solve_adt_type(
        &mut self,
        adt_type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<AdtSolveStatus> {
        let inf_adt_type_id = self.solve(adt_type_id, root_id)?;
        let sub_sets = self.substitutions.get(&root_id);

        if inf_adt_type_id == adt_type_id {
            Ok(AdtSolveStatus::NoProgress)
        } else if self
            .analyze_context
            .ty_env
            .is_solved(sub_sets, inf_adt_type_id)?
        {
            self.set_generic_names(inf_adt_type_id, root_id)?;

            let inf_adt_ty = self.analyze_context.ty_env.ty(inf_adt_type_id)?.clone();

            info!(
                "inf_adt_type_id: {}, inf_adt_ty: {:#?}",
                inf_adt_type_id, inf_adt_ty
            );

            let (inner_ty, adt_generics) = match inf_adt_ty {
                Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

                // TODO: Fix this edge case. This might be a pointer to ADT since a
                //       "{this}" will cause problems. Fix the problem and remove this
                //       logic in the future.
                Ty::Pointer(type_id_i, ..) => {
                    let inf_type_id_i = self.inferred_type(type_id_i, root_id)?;
                    let inf_ty_i = self.analyze_context.ty_env.ty(inf_type_id_i)?.clone();
                    if let Ty::CompoundType(inner_ty, generics, ..) = inf_ty_i {
                        (inner_ty, generics)
                    } else {
                        unreachable!("ADT type not pointer to compound: {:#?}", inf_adt_ty);
                    }
                }

                _ => unreachable!("ADT type not valid: {:#?}", inf_adt_ty),
            };
            let adt_path = inner_ty.get_ident().unwrap();

            Ok(AdtSolveStatus::Solved(adt_path, adt_generics))
        } else {
            Ok(AdtSolveStatus::Progress(inf_adt_type_id))
        }
    }

    /// A `adt_type_id` might have been "half" solved, ex. it might be solved to
    /// a generic. This function can be use to partially solve the wrapping
    /// "Unknown..." types that "wraps" the `adt_type_id` (ex. function call,
    /// args etc.).
    fn solve_partial_adt_type(
        &mut self,
        type_id: TypeId,
        adt_type_id: TypeId,
        root_id: BlockId,
    ) -> LangResult<TypeId> {
        let new_adt_type_id = self.solve(adt_type_id, root_id)?;
        if adt_type_id != new_adt_type_id {
            self.insert_constraint(adt_type_id, new_adt_type_id, root_id)?;
            Ok(new_adt_type_id)
        } else {
            Ok(type_id)
        }
    }

    /// Creates new a new `Generic` where the types will be new `GenericInstance`s.
    /// This `Generic` can be used to replace `Ty::Generic` with new
    /// `Ty::GenericInstance`s found in this returned value.
    ///
    /// This is needed to ensure that no "raw" `Ty::Generic`s are leaked outside
    /// the function body itself. I.e. this can be used to replace arguments and
    /// return values of function calls.
    pub fn new_method_generics(
        &mut self,
        method: &Fn,
        type_info: &TypeInfo,
    ) -> LangResult<Option<Generics>> {
        // TODO: This should be done somewhere else. This feels like a really
        //       random place to do it.
        if let Some(method_generics) = &method.generics {
            let mut new_method_generics = Generics::new();

            for generic_name in method_generics.iter_names() {
                // Get some arbitrary data from the file_pos to create a unique ID.
                let file_pos = type_info.file_pos().unwrap();
                let id = format!(
                    "R:{}-Cs:{}-Ce:{}",
                    file_pos.line_start, file_pos.column_start, file_pos.column_end
                );
                let type_id = self.analyze_context.ty_env.id(&Ty::GenericInstance(
                    generic_name.clone(),
                    id,
                    type_info.clone(),
                ))?;

                new_method_generics.insert(generic_name.clone(), type_id);
            }

            Ok(Some(new_method_generics))
        } else {
            Ok(None)
        }
    }

    /// If the given type `ty` contains generics that don't have their "names"
    /// set, this function will fetch the structure and set the names if possible.
    pub fn set_generic_names(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<()> {
        let ty = self.analyze_context.ty_env.ty(type_id)?.clone();

        let (inner_ty, generics) = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                return self.set_generic_names(type_id_i, block_id);
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

            if let Some(adt_gens) = adt.generics.clone() {
                let ty = self.analyze_context.ty_env.ty_mut(type_id)?;

                let generics = match ty {
                    Ty::CompoundType(_, generics, ..) => generics,
                    _ => unreachable!("type_id: {:?}, generics: {:#?}", type_id, generics),
                };

                for (idx, gen_name) in adt_gens.iter_names().enumerate() {
                    generics.insert_lookup(gen_name.clone(), idx);
                    generics.insert_name(gen_name.clone());
                }
            }
        }

        Ok(())
    }

    // TODO: Is it possible to move this function to "Expr" in some way?
    pub fn get_expr_type(&self, expr_opt: Option<&Expr>) -> LangResult<TypeId> {
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
