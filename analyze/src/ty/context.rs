use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    file::FilePosition,
    token::expr::Expr,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
};
use either::Either;
use log::debug;
use std::collections::{hash_map, HashMap};

use crate::{AnalyzeContext, BlockInfo};

// TODO: Remove `analyze_context` from here, makes no sense to have it nested
//       in this TypeContext. The TypeCOntexct currently needs to look up
//       struct declaration when solving substitutions.

/// "Stand-alone" struct that will contain all the important state/context
/// after the type inference have been ran. This struct will then be used by
/// the "TypeSolver" to make sure that everything has gone as expected and
/// it will also replace all temporary "unknown" types with real ones.
pub struct TypeContext<'a> {
    /// Needed to look up ex. struct members.
    pub analyze_context: &'a mut AnalyzeContext,

    /// The key is a "unknown" type that is to be infered and the value
    /// is the type that the "unknown" type infers to. This map will be updated
    /// continuously during this step but should contain all substitutions after
    /// this analyzer is done.
    ///
    /// If a "unknown" type doesn't have a substitution in this map at the end
    /// of this step, the program didn't have enough information to infer the type
    /// and the error should be reported.
    pub substitutions: HashMap<Ty, Ty>,

    /// This contains constraints/equalities that, from looking at the actual source
    /// code, needs to be true. Ex. "var x = y" would create a constraint that
    /// x and y needs to be equal. These constraints will continuous added and
    /// also tried to be solved during this analyzing step.
    ///
    /// If a constraint can be solved, it will "converted" to a substitution that
    /// will be moved to into `substitutions`. Ex. "var x: i32 = y" is a solvable
    /// constraint that would lead to a substitution "typeof(y) => i32".
    ///
    /// If a constraint is added that can never be true ex. "i32 == u32",
    /// that should be reported as a error.
    pub constraints: Vec<(Ty, Ty)>,

    /// Is the current type when solving substitutions.
    cur_ty: Ty,
}

#[derive(Debug, Clone)]
pub enum SubResult {
    Solved(Ty),
    UnSolved(Ty),
    Err(LangError),
}

impl SubResult {
    /// Returns a copy of the inner value of the SubResult.
    pub fn inner(&self) -> Result<Ty, LangError> {
        match self {
            SubResult::Solved(ty) | SubResult::UnSolved(ty) => Ok(ty.clone()),
            SubResult::Err(err) => Err(err.clone()),
        }
    }

    pub fn is_solved(&self) -> bool {
        matches!(self, SubResult::Solved(_))
    }

    pub fn is_unsolved(&self) -> bool {
        matches!(self, SubResult::UnSolved(_))
    }
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        let tmp_ty = Ty::CompoundType(InnerTy::Void, Generics::new());

        Self {
            analyze_context,
            substitutions: HashMap::default(),
            constraints: Vec::default(),
            cur_ty: tmp_ty,
        }
    }

    // TODO: Will trying to solve the constraints all the time cause the function
    //       to be to slow?
    /// Inserts a constraint.
    pub fn insert_constraint(&mut self, lhs: Ty, rhs: Ty) {
        if !self.constraints.contains(&(lhs.clone(), rhs.clone())) {
            debug!("Insert constraint -- lhs: {:?}, rhs: {:?}", &lhs, &rhs);

            // Create constraints for the "inner" types if possible.
            match (&lhs, &rhs) {
                (Ty::CompoundType(_, lhs_gens), Ty::CompoundType(_, rhs_gens)) => {
                    for (lhs_gen, rhs_gen) in lhs_gens.iter_types().zip(rhs_gens.iter_types()) {
                        self.insert_constraint(lhs_gen.clone(), rhs_gen.clone());
                    }
                }

                (Ty::Pointer(lhs_inner), Ty::Pointer(rhs_inner))
                | (Ty::Array(lhs_inner, _), Ty::Array(rhs_inner, _))
                | (
                    Ty::UnknownStructureMember(lhs_inner, _),
                    Ty::UnknownStructureMember(rhs_inner, _),
                )
                | (
                    Ty::UnknownStructureMethod(lhs_inner, _),
                    Ty::UnknownStructureMethod(rhs_inner, _),
                )
                | (
                    Ty::UnknownMethodArgument(lhs_inner, _, _),
                    Ty::UnknownMethodArgument(rhs_inner, _, _),
                )
                | (Ty::UnknownArrayMember(lhs_inner), Ty::UnknownArrayMember(rhs_inner)) => {
                    self.insert_constraint(*lhs_inner.clone(), *rhs_inner.clone());
                }

                _ => (),
            }

            self.constraints.push((lhs, rhs));
        } else {
            debug!(
                "Constraint already exists -- lhs: {:?}, rhs: {:?}",
                &lhs, &rhs
            );
        }
    }

    pub fn solve_constraints(&mut self, finalize: bool) -> Result<(), Vec<LangError>> {
        // If `finalize` is set, first solve the constraints without finalizing
        // just to make sure that all possible types that can be solved without
        // being "forced" are solved. After that run it again to "force" solve
        // the types when there are no other options left.
        if finalize {
            // Run twice. The first time two completly unsolvable types are NOT
            // mapped. The second run they will be "forced" to map even though
            // they might not be solved.
            self.solve_constraints_priv(false, false)?;
            self.solve_constraints_priv(false, true)?;
        }

        self.solve_constraints_priv(finalize, true)
    }

    // TODO: Solve infinite recursive solves when generics get implemented.
    // TODO: Should the constraint be removed when a new substitution is added?
    //       Will that constraint every by used again?
    /// Solves the constraints. If `finalize` is set to true, all constraints
    /// must be solved and inserted as substitutions, otherwise `Error` will be
    /// returned. If `finalize` is set to false, it doesn't matter that all
    /// types wasn't solvable, `Ok` will be returned.
    ///
    /// See the comment for the function `solve_constraint` for an explanation
    /// of the parameter `map_unsolvables`.
    fn solve_constraints_priv(
        &mut self,
        finalize: bool,
        map_unsolvables: bool,
    ) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        // Loops over the constraints multiple times if needed until all solvable
        // constraints have been solved.
        loop {
            let start_constraint_len = self.constraints.len();

            let mut i = 0;
            while i < self.constraints.len() {
                let (lhs, rhs) = self.constraints.get(i).cloned().unwrap();

                match self.solve_constraint(lhs, rhs, finalize, map_unsolvables) {
                    Ok(true) => {
                        self.constraints.swap_remove(i);
                    }
                    Ok(false) => {
                        i += 1;
                    }
                    Err(mut errs) => {
                        errors.append(&mut errs);
                        self.constraints.swap_remove(i);
                    }
                }
            }

            // If all constraints have been solved (indicated by a empty constraint
            // vector) or if the current amount of constraints are the same as the
            // start amount (before the while loop above ran), no more constraints
            // can be solved; break out of the outer loop.
            if self.constraints.is_empty() || start_constraint_len == self.constraints.len() {
                break;
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// The `map_unsolvables` indicates what should happen when both lhs and rhs
    /// are unsolvable. If `map_unsolvables` is set to true, a new substitution
    /// will be created; if it is set to false, the constraint will be "skipped"
    /// and no new substitution will be created.
    ///
    /// This is done, similar to `finalize`, to let types that are 100% solvable
    /// be solved before starting to force types that aren't 100% solvable to be
    /// solved.
    fn solve_constraint(
        &mut self,
        lhs: Ty,
        rhs: Ty,
        finalize: bool,
        map_unsolvables: bool,
    ) -> Result<bool, Vec<LangError>> {
        let mut errors = Vec::default();

        let lhs_sub = self.solve_substitution(&lhs, finalize);
        let rhs_sub = self.solve_substitution(&rhs, finalize);

        debug!(
            "Solving constraint -- finalize: {}, map_unsolvables: {}\nlhs: {:#?}\nlhs_sub: {:#?}\nrhs: {:#?}\nrhs_sub: {:#?}",
            finalize, map_unsolvables, &lhs, &lhs_sub, &rhs, &rhs_sub
        );

        // Get the inner types of the sub results. If there are any errors,
        // save the errors and continue with the next iteration of
        // constraint solving.
        let (lhs_sub_inner, rhs_sub_inner) = match (lhs_sub.inner(), rhs_sub.inner()) {
            (Ok(l), Ok(r)) => (l, r),

            (Err(err_a), Err(err_b)) => {
                errors.push(err_a);
                errors.push(err_b);
                return Err(errors);
            }

            (Err(err), _) | (_, Err(err)) => {
                errors.push(err);
                return Err(errors);
            }
        };

        // If the types aren't compatible, report a error and remove the
        // constraint. This will allow the inference to continue to do as
        // much work as possible even though it will never be 100% solved.
        if !lhs_sub_inner.is_compatible(&rhs_sub_inner) {
            let err = self.analyze_context.err(format!(
                "Unsolvable type constraint. Lhs: {:#?}, rhs: {:#?}. \
                Lhs_sub_inner: {:#?}, rhs_sub_inner: {:#?}",
                lhs, rhs, lhs_sub_inner, rhs_sub_inner
            ));
            errors.push(err);
            return Err(errors);
        }

        // The reason this matching is done down here instead of in the
        // "inner" chech match above is just to allow for the compatible
        // check inbetween.
        let is_solved = match (lhs_sub, rhs_sub) {
            (SubResult::Solved(to), SubResult::UnSolved(from))
            | (SubResult::UnSolved(from), SubResult::Solved(to)) => {
                if let Err(err) = self.insert_substitution(from, to) {
                    errors.push(err);
                    return Err(errors);
                } else {
                    true
                }
            }

            (SubResult::UnSolved(first), SubResult::UnSolved(second)) if map_unsolvables => {
                match self.get_mapping_direction(first, second) {
                    Ok((from, to)) => {
                        if let Err(err) = self.insert_substitution(from, to) {
                            errors.push(err);
                            return Err(errors);
                        } else {
                            true
                        }
                    }
                    Err(err) => {
                        errors.push(err);
                        return Err(errors);
                    }
                }
            }

            (SubResult::UnSolved(_), SubResult::UnSolved(_)) if !map_unsolvables => false,

            // Nothing to do if both are solved, the substitution have already
            // been completed. Any errors have already been reported in the logic
            // above, so no need to report them again.
            _ => true,
        };

        Ok(is_solved)
    }

    /// Takes in two types and sorts them in the correct "mapping order" i.e.
    /// which type should map to which in the substitution map.
    /// The left item in the returned tuple should be mapped to the right.
    /// Returns Err if something goes wrong ex. if unable to find a valid
    /// mapping for the types.
    pub fn get_mapping_direction(&mut self, lhs: Ty, rhs: Ty) -> CustomResult<(Ty, Ty)> {
        // TODO: Can mapping unknowns (any/int/float) to each other cause infinite
        //       recursion. Probably; how can it be prevented?

        debug!(
            "Getting mapping direction -- lhs: {:?}, rhs: {:?}",
            lhs, rhs
        );

        let lhs_sub = match self.solve_substitution(&lhs, false) {
            SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => sub_ty,
            SubResult::Err(err) => return Err(err),
        };
        let rhs_sub = match self.solve_substitution(&rhs, false) {
            SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => sub_ty,
            SubResult::Err(err) => return Err(err),
        };

        if !lhs_sub.is_compatible(&rhs_sub) {
            return Err(self.analyze_context.err(format!(
                "Tried to map incompatible types. Lhs_sub: {:?}, rhs_sub: {:?}",
                lhs_sub, rhs_sub
            )));
        }

        if lhs_sub.precedence(&rhs_sub) {
            Ok((rhs_sub, lhs_sub))
        } else {
            Ok((lhs_sub, rhs_sub))
        }
    }

    /// Checks if adding a substitution from `from` to `to` creates a loop.
    fn causes_loop(&self, from: &Ty, to: &Ty) -> bool {
        if let Some(new_to) = self.substitutions.get(to) {
            if from == new_to {
                true
            } else {
                self.causes_loop(from, new_to)
            }
        } else {
            false
        }
    }

    /// Inserts a new substitution, mapping `from` to `to`.
    pub fn insert_substitution(&mut self, from: Ty, to: Ty) -> CustomResult<()> {
        debug!("Insert substitution -- from: {:?}, to: {:?}", &from, &to);

        // Can't map to itself (infinite recursion) and shouldn't cause any kind
        // of loop.
        if from == to || self.causes_loop(&from, &to) {
            debug!(
                "Substitution to self or causes loop -- from: {:?}, to: {:?}",
                &from, &to
            );
            return Ok(());
        }

        // Insert substitutions for the inner types of outer types that have
        // been "solved" and contains other "Ty"s.
        match (&from, &to) {
            (Ty::CompoundType(_, gens_from), Ty::CompoundType(_, gens_to)) => {
                for (gen_from, gen_ty) in gens_from.iter_types().zip(gens_to.iter_types()) {
                    self.insert_substitution(gen_from.clone(), gen_ty.clone())?;
                }
            }

            (Ty::Pointer(inner_from), Ty::Pointer(inner_to))
            | (Ty::Array(inner_from, _), Ty::Array(inner_to, _)) => {
                self.insert_substitution(*inner_from.clone(), *inner_to.clone())?;
            }

            _ => (),
        }

        // If the `from` doesn't already have a mapping, just insert the
        // new mapping to `to` and return. Otherwise add a new substitution
        // between the old and the new `to`.
        let old_to = match self.substitutions.entry(from) {
            hash_map::Entry::Occupied(ref mut o) => o.get().clone(),
            hash_map::Entry::Vacant(v) => {
                v.insert(to);
                return Ok(());
            }
        };

        match self.get_mapping_direction(to, old_to) {
            Ok((new_from, new_to)) => {
                if !self.causes_loop(&new_from, &new_to) {
                    self.insert_substitution(new_from, new_to)
                } else {
                    Ok(())
                }
            }
            Err(err) => Err(err),
        }
    }

    /// Substitutes the given type recursively until no more substitution can
    /// be found. If the type is already known, this function will make no
    /// substitution and will not report any errors.
    ///
    /// If `finalize` is set to true, any "UnknownInt" or "UnknownFloat" will be
    /// converted to its default types (i32/f32). After that point the type is
    /// assumed to be set to the correct type and will never change again.
    ///
    /// If `finalize` is set to false, any "UnknownInt" or "UnknownFloat" will be
    /// kept since the type inference isn't finished yet and they might be set
    /// to something different than the default i32/f32.
    pub fn solve_substitution(&mut self, ty: &Ty, finalize: bool) -> SubResult {
        static MAX_LOOP_ITERATIONS: i32 = 50;

        self.cur_ty = ty.clone();
        let mut i = 0;

        while i < MAX_LOOP_ITERATIONS {
            debug!("Substituting, i: {} -- cur_ty: {:?}", i, self.cur_ty);
            i += 1;

            if let Ty::Generic(..) = self.cur_ty {
                return self.solve_generic(finalize);
            }

            if self.cur_ty.is_generic() {
                return self.solve_generic(finalize);
            } else if self.substitutions.contains_key(&self.cur_ty) {
                // Set the substitute type as the current type and and try to
                // solve the type iteratively.
                self.cur_ty = self.substitutions.get(&self.cur_ty).unwrap().clone();
            } else {
                match &self.cur_ty {
                    Ty::CompoundType(..) => {
                        return self.solve_compound_type(finalize);
                    }
                    Ty::Pointer(..) | Ty::Array(..) => {
                        return self.solve_aggregate(finalize);
                    }
                    Ty::UnknownStructureMember(..) => {
                        return self.solve_unknown_structure_member(finalize);
                    }
                    Ty::UnknownStructureMethod(..) => {
                        return self.solve_unknown_structure_method(finalize);
                    }
                    Ty::UnknownMethodArgument(..) => {
                        return self.solve_unknown_method_argument(finalize);
                    }
                    Ty::UnknownArrayMember(..) => {
                        return self.solve_unknown_array_member(finalize);
                    }
                    Ty::Any => {
                        return self.solve_any(finalize);
                    }
                    Ty::Generic(..) | Ty::GenericInstance(..) => {
                        unreachable!("Type was Generic or GenericImpl.");
                    }
                }
            }
        }

        // Unable to find any substitution after `MAX_LOOP_ITERATIONS` iterations.
        // Assume that something the high number of interations is a indication
        // of a infinite loop and report as error.
        SubResult::Err(
            self.analyze_context
                .err(format!("Recursion error, #iterations: {}", i)),
        )
    }

    // TODO: Does the "InnerTy" need to be solved? Currently it isn't and just
    //       the outer "Ty" is solved and the assumption is that the InnerTy
    //       will be solved "automatically".
    /// Solves compound types (i.e. types that might contain generics).
    fn solve_compound_type(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();
        let mut is_solved;

        if let Ty::CompoundType(inner_ty, generics) = &mut cur_ty_modified {
            is_solved = true;

            // All inner types needs to be solved. If any of them can't be solved,
            // this whole aggregate type will count as unsolvable.
            for gen_ty in generics.iter_types_mut() {
                let result = self.solve_substitution(gen_ty, finalize);
                match &result {
                    SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => {
                        *gen_ty = sub_ty.clone();
                        if result.is_unsolved() {
                            is_solved = false;
                        }
                    }
                    err => return err.clone(),
                }
            }

            // Make a check to see if the inner type is solved as well.
            // If this is to be finalized, solve the unknown ints and floats.
            match inner_ty {
                InnerTy::UnknownInt(_, _) if finalize => {
                    *inner_ty = InnerTy::default_int();
                }
                InnerTy::UnknownFloat(_) if finalize => {
                    *inner_ty = InnerTy::default_float();
                }

                InnerTy::UnknownIdent(ident, id) => {
                    // Check if this unknown structure can be found and then
                    // replaced the inner type with the correct structure.
                    if self.analyze_context.get_struct(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Struct(ident.clone());
                    } else if self.analyze_context.get_enum(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Enum(ident.clone());
                    } else if self.analyze_context.get_interface(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Interface(ident.clone());
                    } else {
                        is_solved = false;
                    }
                }

                InnerTy::Unknown(_) | InnerTy::UnknownInt(_, _) | InnerTy::UnknownFloat(_) => {
                    is_solved = false
                }

                _ => (),
            }

            self.cur_ty = cur_ty_backup;
            if is_solved {
                SubResult::Solved(cur_ty_modified)
            } else {
                SubResult::UnSolved(cur_ty_modified)
            }
        } else {
            unreachable!("This function will only be called when it is a compound type.");
        }
    }

    /// Solves aggregate types (array or pointer). This does NOT solve
    /// compound types.
    fn solve_aggregate(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        match &mut cur_ty_modified {
            Ty::Pointer(ty) | Ty::Array(ty, _) => {
                let result = self.solve_substitution(ty.as_ref(), finalize);
                match &result {
                    SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => {
                        if ty.contains_unknown_any() {
                            if let Err(err) = self.insert_substitution(*ty.clone(), sub_ty.clone())
                            {
                                return SubResult::Err(err);
                            }
                        }

                        *ty = Box::new(sub_ty.clone());

                        self.cur_ty = cur_ty_backup;
                        if result.is_solved() {
                            SubResult::Solved(cur_ty_modified)
                        } else {
                            SubResult::UnSolved(cur_ty_modified)
                        }
                    }

                    err => err.clone(),
                }
            }

            _ => {
                unreachable!("This function will only be called when it is a pointer/array type.");
            }
        }
    }

    /// Solves generic types.
    fn solve_generic(&mut self, finalize: bool) -> SubResult {
        let cur_ty_backup = self.cur_ty.clone();

        match &self.cur_ty {
            Ty::Generic(..) => {
                // TODO: IS this OK? The "Generic" types should only be a part of structs
                //       and their impl methods that are to be removed. New structs/impl
                //       have been created with the generics replaced. Need to make sure
                //       that it is actually the case somewhere else later in the code.
                //       These structs/impls will be removed in the `converter` stage
                //       which is after the current `solver` stage.
                if finalize {
                    SubResult::Solved(cur_ty_backup)
                } else {
                    SubResult::UnSolved(cur_ty_backup)
                }
            }

            Ty::GenericInstance(..) => {
                if let Some(ty) = self.substitutions.get(&self.cur_ty).cloned() {
                    self.solve_substitution(&ty, finalize)
                } else {
                    SubResult::UnSolved(cur_ty_backup)
                }
            }

            _ => unreachable!(),
        }
    }

    /// Solves "Any" types. If a Any type has a mapping in the substitution map,
    /// this function won't be called.
    fn solve_any(&mut self, finalize: bool) -> SubResult {
        if finalize {
            SubResult::Err(
                self.analyze_context
                    .err(format!("Unable to solve \"Any\" type: {:#?}", self.cur_ty)),
            )
        } else {
            SubResult::UnSolved(self.cur_ty.clone())
        }
    }

    fn solve_unknown_structure_member(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownStructureMember(ty, member_name) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let member_name = member_name.clone();

            // Get any possible substitution.
            let sub_ty = match self.solve_substitution(ty, finalize) {
                SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => {
                    *ty = Box::new(sub_ty.clone());
                    sub_ty
                }
                err => return err,
            };

            let (inner_ty, generics) = match &sub_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics) => (inner_ty, generics),

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownStructureMember: {:#?}",
                        sub_ty
                    )))
                }
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            let var = match inner_ty {
                InnerTy::Struct(ident) => {
                    match self
                        .analyze_context
                        .get_struct_member(ident, &member_name, id)
                    {
                        Ok(var) => var,
                        Err(err) => return SubResult::Err(err),
                    }
                }

                InnerTy::Enum(ident) => {
                    match self
                        .analyze_context
                        .get_enum_member(ident, &member_name, id)
                    {
                        Ok(var) => var,
                        Err(err) => return SubResult::Err(err),
                    }
                }

                InnerTy::Interface(ident) => {
                    panic!("TODO: Interface");
                }

                _ => {
                    self.cur_ty = cur_ty_backup;

                    return if finalize {
                        SubResult::Err(self.analyze_context.err(format!(
                            "Invalid inner type when solving UnknownStructureMember: {:#?}",
                            inner_ty
                        )))
                    } else {
                        SubResult::UnSolved(cur_ty_modified)
                    };
                }
            };

            let var = var.borrow();
            if let Some(mut new_ty) = var.ty.clone() {
                // Since this fetched the actual structue "template" that is used
                // by all, the generics will still be the "Generics". Need to
                // replace them with the actual type for this specific use of
                // the structure.
                new_ty.replace_generics_impl(generics);

                // Add a substitution from the "UnknownStructureMember" to the
                // actual type fetched from the member of the structure.
                if let Err(err) = self.insert_substitution(cur_ty_backup, new_ty.clone()) {
                    return SubResult::Err(err);
                }

                self.solve_substitution(&new_ty, finalize)
            } else {
                SubResult::Err(self.analyze_context.err(format!(
                    "Type not set for member \"{}\" in structure \"{}\"",
                    member_name, inner_ty
                )))
            }
        } else {
            unreachable!(
                "This function will only be called when it is a UnknownStructureMember type."
            );
        }
    }

    fn solve_unknown_structure_method(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownStructureMethod(ty, method_name) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let method_name = method_name.clone();

            let sub_ty = match self.solve_substitution(ty, finalize) {
                SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => {
                    *ty = Box::new(sub_ty.clone());
                    sub_ty
                }
                err => return err,
            };

            let (inner_ty, generics) = match &sub_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics) => (inner_ty, generics),

                // TODO: Is this valid? Solve this in a better way. There are currently
                //       two points in the code where this fix is needed.
                // The type given to the `UnknownStructureMethod` might possibly
                // have been a pointer to the structure instead of the structure
                // type itself.
                Ty::Pointer(ty_box) => {
                    if let Ty::CompoundType(inner_ty, generics) = ty_box.as_ref() {
                        (inner_ty, generics)
                    } else {
                        self.cur_ty = cur_ty_backup;
                        return SubResult::UnSolved(cur_ty_modified);
                    }
                }

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownStructureMethod: {:#?}",
                        sub_ty
                    )))
                }
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            let method = match inner_ty {
                InnerTy::Struct(struct_name) => {
                    match self
                        .analyze_context
                        .get_method(struct_name, &method_name, id)
                    {
                        Ok(method) => method,
                        Err(err) => return SubResult::Err(err),
                    }
                }

                InnerTy::Enum(_) => {
                    panic!("TODO: Enum, should it have methods?")
                }

                InnerTy::Interface(_) => {
                    panic!("TODO: Interface, should it have methods?");
                }

                _ => {
                    return if finalize {
                        SubResult::Err(self.analyze_context.err(format!(
                            "Invalid inner type when solving UnknownStructureMethod: {:#?}",
                            inner_ty
                        )))
                    } else {
                        self.cur_ty = cur_ty_backup;
                        SubResult::UnSolved(cur_ty_modified)
                    };
                }
            };

            let method = method.borrow();
            if let Some(mut new_ty) = method.ret_type.clone() {
                new_ty.replace_generics_impl(generics);

                // Add a substitution from the "UnknownStructureMethod" to the
                // actual type fetched from the method of the structure.
                if let Err(err) = self.insert_substitution(cur_ty_backup, new_ty.clone()) {
                    return SubResult::Err(err);
                }

                self.solve_substitution(&new_ty, finalize)
            } else {
                // The return type of the method is None == Void.
                let ty = Ty::CompoundType(InnerTy::Void, Generics::new());
                if let Err(err) = self.insert_substitution(cur_ty_backup, ty.clone()) {
                    SubResult::Err(err)
                } else {
                    SubResult::Solved(ty)
                }
            }
        } else {
            unreachable!(
                "This function will only be called when it is a UnknownStructureMethod type."
            );
        }
    }

    fn solve_unknown_method_argument(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownMethodArgument(ty, method_name, name_or_idx) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let method_name = method_name.clone();
            let name_or_idx = name_or_idx.clone();

            // Get any possible substitution and replace the current type with
            // the new substituted type. The returned value might be the same as
            // the current, but doesn't hurt to replace with itself.
            let sub_ty = match self.solve_substitution(ty, finalize) {
                SubResult::Solved(sub_ty) | SubResult::UnSolved(sub_ty) => {
                    *ty = Box::new(sub_ty.clone());
                    sub_ty
                }
                err => return err,
            };

            let (inner_ty, generics) = match &sub_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics) => (inner_ty, generics),

                // TODO: Solve nested ty in better way.
                Ty::Pointer(ty_box) => {
                    if let Ty::CompoundType(inner_ty, generics) = ty_box.as_ref() {
                        (inner_ty, generics)
                    } else {
                        self.cur_ty = cur_ty_backup;
                        return SubResult::UnSolved(cur_ty_modified);
                    }
                }

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownMethodArgument: {:#?}",
                        sub_ty
                    )))
                }
            };

            let struct_name = match inner_ty {
                InnerTy::Struct(struct_name) => struct_name,

                InnerTy::Enum(_) => {
                    panic!("TODO: Enum, should it have methods?")
                }

                InnerTy::Interface(_) => {
                    panic!("TODO: Interface, should it have methods?");
                }

                _ => {
                    return if finalize {
                        SubResult::Err(self.analyze_context.err(format!(
                            "Invalid inner type when solving UnknownStructureMethod: {:#?}",
                            inner_ty
                        )))
                    } else {
                        self.cur_ty = cur_ty_backup;
                        SubResult::UnSolved(cur_ty_modified)
                    };
                }
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            // If this is a named argument, use that name to identify the parameter
            // type and then get the index for the parameter. Otherwise use the
            // index of the argument directly.
            let actual_idx = match name_or_idx {
                Either::Left(arg_name) => {
                    match self.analyze_context.get_method_param_idx(
                        struct_name,
                        &method_name,
                        &arg_name,
                        id,
                    ) {
                        Ok(idx) => idx,
                        Err(err) => return SubResult::Err(err),
                    }
                }
                Either::Right(idx) => idx,
            };

            let mut arg_ty = match self.analyze_context.get_method_param_type(
                struct_name,
                &method_name,
                actual_idx,
                BlockInfo::DEFAULT_BLOCK_ID,
            ) {
                Ok(ty) => ty,
                Err(err) => {
                    return SubResult::Err(err);
                }
            };

            arg_ty.replace_generics_impl(generics);

            // Add a substitution from the "UnknownMethodArgument" to the
            // actual type fetched from the method of the structure.
            if let Err(err) = self.insert_substitution(cur_ty_backup, arg_ty.clone()) {
                return SubResult::Err(err);
            }

            self.solve_substitution(&arg_ty, finalize)
        } else {
            unreachable!(
                "This function will only be called when it is a UnknownMethodArgument type."
            );
        }
    }

    fn solve_unknown_array_member(&mut self, finalize: bool) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownArrayMember(ty) = &mut cur_ty_modified {
            // Add a substitution from the "UnknownArrayMember" to the actual
            // type of the array if it has been found.
            match self.solve_substitution(&ty, finalize) {
                SubResult::Solved(sub_ty) => {
                    if let Ty::Array(real_ty, _) = sub_ty {
                        if let Err(err) =
                            self.insert_substitution(cur_ty_backup.clone(), *real_ty.clone())
                        {
                            SubResult::Err(err)
                        } else {
                            self.cur_ty = cur_ty_backup;
                            SubResult::Solved(*real_ty)
                        }
                    } else {
                        SubResult::Err(self.analyze_context.err(format!(
                            "Epected type to be array. unknown: {:#?}, solved: {:#?}",
                            ty, sub_ty
                        )))
                    }
                }

                SubResult::UnSolved(un_sub_ty) => {
                    *ty = Box::new(un_sub_ty);

                    self.cur_ty = cur_ty_backup;
                    SubResult::UnSolved(cur_ty_modified)
                }

                res => res,
            }
        } else {
            unreachable!("This function will only be called when it is a UnknownArrayMember type.");
        }
    }

    // TODO: Is it possible to move this function to "Expr" in some way?
    pub fn get_expr_type(&self, expr_opt: Option<&Expr>) -> CustomResult<Ty> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type()
        } else {
            Err(LangError::new(
                "expr opt set to None.".into(),
                AnalyzeError {
                    file_pos: FilePosition::default(),
                },
            ))
        }
    }
}
