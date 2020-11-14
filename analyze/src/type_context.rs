use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::expr::Expr,
    types::Type,
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
    pub substitutions: HashMap<Type, Type>,

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
    pub constraints: Vec<(Type, Type)>,

    /// Is the current type when solving substitutions.
    cur_ty: Type,
}

#[derive(Debug, Clone)]
pub enum SubResult {
    Solved(Type),
    UnSolved(Type),
    Err(LangError),
}

impl SubResult {
    /// Returns a copy of the inner value of the SubResult.
    pub fn inner(&self) -> Result<Type, LangError> {
        match self {
            SubResult::Solved(ty) | SubResult::UnSolved(ty) => Ok(ty.clone()),
            SubResult::Err(err) => Err(err.clone()),
        }
    }
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            substitutions: HashMap::default(),
            constraints: Vec::default(),
            cur_ty: Type::Void,
        }
    }

    // TODO: Will trying to solve the constraints all the time cause the function
    //       to be to slow?
    /// Inserts a constraint. After a new constraint is inserted, it will try
    /// to solve all constraints.
    pub fn insert_constraint(&mut self, lhs: Type, rhs: Type) {
        if !self.constraints.contains(&(lhs.clone(), rhs.clone())) {
            debug!("Insert constraint -- lhs: {:?}, rhs: {:?}", &lhs, &rhs);
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
            self.solve_constraints_priv(false)?;
        }
        self.solve_constraints_priv(finalize)
    }

    // TODO: Solve infinite recursive solves when generics get implemented.
    // TODO: Should the constraint be removed when a new substitution is added?
    //       Will that constraint every by used again?
    /// Solves the constraints. If `finalize` is set to true, all constraints
    /// must be solved and inserted as substitutions, otherwise `Error` will be
    /// returned. If `finalize` is set to false, it doesn't matter that all
    /// types wasn't solvable, `Ok` will be returned.
    fn solve_constraints_priv(&mut self, finalize: bool) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        // Takes the first constraint in the list, removes it and then tries
        // to solve it. Repeats until there are no more constarints.
        while !self.constraints.is_empty() {
            let (lhs, rhs) = self.constraints.swap_remove(0);

            let mut result = self.solve_constraint(lhs, rhs, finalize);
            if let Err(errs) = &mut result {
                errors.append(errs);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn solve_constraint(
        &mut self,
        lhs: Type,
        rhs: Type,
        finalize: bool,
    ) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        let lhs_sub = self.solve_substitution(&lhs, finalize);
        let rhs_sub = self.solve_substitution(&rhs, finalize);

        debug!(
            "Solving constraint -- finalize: {}\nlhs: {:#?}\nlhs_sub: {:#?}\nrhs: {:#?}\nrhs_sub: {:#?}",
            finalize, &lhs, &lhs_sub, &rhs, &rhs_sub
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
        match (lhs_sub, rhs_sub) {
            (SubResult::Solved(to), SubResult::UnSolved(from))
            | (SubResult::UnSolved(from), SubResult::Solved(to)) => {
                if let Err(err) = self.insert_substitution(from, to) {
                    errors.push(err)
                }
            }

            (SubResult::UnSolved(first), SubResult::UnSolved(second)) => {
                match self.get_mapping_direction(first, second) {
                    Ok((from, to)) => {
                        if let Err(err) = self.insert_substitution(from, to) {
                            errors.push(err)
                        }
                    }
                    Err(err) => errors.push(err),
                };
            }

            // Nothing to do if both are solved, the substitution have already
            // been completed. Any errors have already been reported in the logic
            // above, so no need to report them again.
            _ => (),
        }

        Ok(())
    }

    /// Takes in two types and sorts them in the correct "mapping order" i.e.
    /// which type should map to which in the substitution map.
    /// The left item in the returned tuple should be mapped to the right.
    /// Returns Err if something goes wrong ex. if unable to find a valid
    /// mapping for the types.
    pub fn get_mapping_direction(&mut self, lhs: Type, rhs: Type) -> CustomResult<(Type, Type)> {
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

        Ok(
            if lhs_sub == rhs_sub || lhs_sub.is_unknown() || lhs_sub.is_generic() {
                (lhs, rhs)
            } else if rhs_sub.is_unknown() || rhs_sub.is_generic() {
                (rhs, lhs)
            } else if !lhs_sub.is_unknown_any() && !rhs_sub.is_unknown_any() {
                // True if both are known, but they aren't equal and they are still
                // compatible. This means that both are aggregates of the same type.
                // Structs will have been filtered earlier in this function.
                match (lhs_sub.clone(), rhs_sub.clone()) {
                    (Type::Pointer(inner_lhs), Type::Pointer(inner_rhs)) => {
                        self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                    }
                    (Type::Array(inner_lhs, _), Type::Array(inner_rhs, _)) => {
                        self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                    }
                    _ => unreachable!(format!("lhs_sub: {:?}, rhs_sub: {:?}", lhs_sub, rhs_sub)),
                }
            } else if !lhs_sub.is_unknown_any() {
                // Only lhs known.
                (rhs, lhs)
            } else if !rhs_sub.is_unknown_any() {
                // Only rhs known.
                (lhs, rhs)
            } else if lhs_sub.is_unknown_struct_member()
                || lhs_sub.is_unknown_struct_method()
                || lhs_sub.is_unknown_method_argument()
            {
                // Prefer struct member/method unknowns over int/float/array
                // unknowns since the types will always be set for the structs
                // and their methods.
                (rhs, lhs)
            } else if rhs_sub.is_unknown_struct_member()
                || rhs_sub.is_unknown_struct_method()
                || rhs_sub.is_unknown_method_argument()
            {
                (lhs, rhs)
            } else if lhs_sub.is_unknown_int() || lhs_sub.is_unknown_float() {
                // Prefer int/float unknowns over array member unknowns.
                (rhs, lhs)
            } else if rhs.is_unknown_int() || rhs_sub.is_unknown_float() {
                (lhs, rhs)
            } else {
                // Both are array member unknowns, direction doesn't matter.
                (rhs, lhs)
            },
        )
    }

    /// Checks if adding a substitution from `from` to `to` creates a loop.
    fn causes_loop(&self, from: &Type, to: &Type) -> bool {
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
    pub fn insert_substitution(&mut self, from: Type, to: Type) -> CustomResult<()> {
        debug!("Insert substitution -- from: {:?}, to: {:?}", &from, &to);

        // Can't map to itself (infinite recursion) and shouldn't cause any kind
        // of loop.
        if from == to || self.causes_loop(&from, &to) {
            debug!(
                "Substitution to self or causes loop -- from unsolved: {:?}, to solved: {:?}",
                &from, &to
            );
            return Ok(());
        }

        // Insert substitutions for the inner types of aggregate types.
        match (&from, &to) {
            // TODO: Does "CompoundType" also need to be matched on?
            //Type::CompoundType(..) => {}
            (Type::Pointer(inner_from), Type::Pointer(inner_to))
            | (Type::Array(inner_from, _), Type::Array(inner_to, _)) => {
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
    /// If `finalize` is set to false, the UnknownInt" or "UnknownFloat" will be
    /// kept since the type inference isn't finished yet and they might be set
    /// to something different than the default i32/f32.
    pub fn solve_substitution(&mut self, ty: &Type, finalize: bool) -> SubResult {
        static MAX_LOOP_ITERATIONS: i32 = 50;

        self.cur_ty = ty.clone();
        let mut i = 0;

        while i < MAX_LOOP_ITERATIONS {
            debug!("Substituting, i: {} -- cur_ty: {:?}", i, self.cur_ty);
            i += 1;

            if self.cur_ty.is_primitive() {
                return SubResult::Solved(self.cur_ty.clone());
            } else if self.substitutions.contains_key(&self.cur_ty) {
                let sub_ty = self.substitutions.get(&self.cur_ty).unwrap().clone();

                // TODO: Clean up logic in here. Shouldn't need to have special
                //       edge case for aggregate types (pointer and array) in
                //       here. Should move this logic into the if-case that
                //       is below (self.cur_ty.is_aggregated()).

                // If this is a aggregate type (pointer or array), insert a
                // subtitution for the inner types as well.
                match (self.cur_ty.clone(), sub_ty.clone()) {
                    (Type::Pointer(l), Type::Pointer(r))
                    | (Type::Array(l, _), Type::Array(r, _)) => {
                        match self.get_mapping_direction(*l, *r) {
                            Ok((from, to)) => {
                                debug!(
                                    "Outer aggregate solved -- inner from: {:#?}\nto: {:#?}",
                                    from, to
                                );

                                if let Err(err) = self.insert_substitution(from, to) {
                                    return SubResult::Err(err);
                                }
                            }
                            Err(err) => return SubResult::Err(err),
                        }
                    }
                    _ => (),
                }

                // Set the substitute type as the current type and and try to
                // solve the type iteratively.
                self.cur_ty = sub_ty;
            } else if self.cur_ty.is_aggregated() {
                return match self.solve_aggregate(finalize) {
                    Ok(true) => SubResult::Solved(self.cur_ty.clone()),
                    Ok(false) => SubResult::UnSolved(self.cur_ty.clone()),
                    Err(err) => SubResult::Err(err),
                };
            } else {
                match self.cur_ty.clone() {
                    // True if finalize is either true or false.
                    Type::Unknown(_) => return SubResult::UnSolved(self.cur_ty.clone()),

                    // Set default type for int/float if this is to be finalized.
                    // Don't set default int/float if this isn't to be finalized.
                    Type::UnknownInt(..) if finalize => {
                        return SubResult::Solved(Type::I32);
                    }
                    Type::UnknownFloat(_) if finalize => {
                        return SubResult::Solved(Type::F32);
                    }
                    Type::UnknownInt(..) | Type::UnknownFloat(_) if !finalize => {
                        return SubResult::UnSolved(self.cur_ty.clone())
                    }

                    // If a generic matches a substition, it will be done in the
                    // else-if above. Ending up here is makes it "unsolvable".
                    Type::Generic(_) => return SubResult::UnSolved(self.cur_ty.clone()),

                    Type::UnknownStructMember(ref struct_ty, ref member_name) => {
                        return self.solve_unknown_struct_member(struct_ty, member_name, finalize);
                    }

                    Type::UnknownStructMethod(ref struct_ty, ref method_name) => {
                        return self.solve_unknown_struct_method(struct_ty, method_name, finalize);
                    }

                    Type::UnknownMethodArgument(ref struct_ty, ref method_name, ref position) => {
                        return self.solve_unknown_method_argument(
                            struct_ty,
                            method_name,
                            position,
                            finalize,
                        );
                    }

                    Type::UnknownArrayMember(ref array_ty) => {
                        return self.solve_unknown_array_member(array_ty, finalize);
                    }

                    _ => unreachable!("cur_ty: {:#?}", self.cur_ty),
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

    /// Solves aggregate types (i.e. types that might contain other types).
    /// Aggregate types can be:
    ///   * Struct compound type (containing possible generic parameters)
    ///   * Pointer
    ///   * Array
    fn solve_aggregate(&mut self, finalize: bool) -> CustomResult<bool> {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let mut local_cur_ty = self.cur_ty.clone();
        let mut is_solved;

        // Need to clone `self.cur_ty` because problems with mutable borrow
        // of self/self.cur_ty. Will update the real `self.cur_ty` inside the
        // match statement with information updated in the cloned `cur_ty`.
        match local_cur_ty {
            Type::CompoundType(_, ref mut generics) => {
                is_solved = true;

                // All inner types needs to be solved. If any of them can't be solved,
                // this whole aggregate type will count as unsolvable.
                for inner_ty in generics.values_mut() {
                    match self.solve_inner_type(inner_ty, finalize) {
                        Ok(false) => is_solved = false,
                        Err(err) => return Err(err),
                        _ => (),
                    }
                }
            }

            Type::Pointer(ref mut inner_ty) | Type::Array(ref mut inner_ty, _) => {
                is_solved = self.solve_inner_type(inner_ty.as_mut(), finalize)?;
            }

            _ => unreachable!("Expected aggregate, was: {:?}", self.cur_ty),
        }

        self.cur_ty = local_cur_ty;
        Ok(is_solved)
    }

    /// Solves the inner type of aggregate types.
    fn solve_inner_type(&mut self, inner_ty: &mut Type, finalize: bool) -> CustomResult<bool> {
        let is_solved;

        match self.solve_substitution(&inner_ty, finalize) {
            SubResult::Solved(sub_ty) => {
                debug!(
                    "Inner type solved -- inner: {:#?}\nsub_ty: {:#?}",
                    inner_ty, sub_ty
                );

                // Add a new substitution for the inner types if they are solvable.
                // The `insert_substitution` function will make sure that no
                // weird mappings/loops are caused, so it should always be safe
                // to call here.
                self.insert_substitution(inner_ty.clone(), sub_ty.clone())?;

                *inner_ty = sub_ty;
                is_solved = true;
            }
            SubResult::UnSolved(un_sub_ty) => {
                debug!(
                    "Inner type unsolved -- inner: {:#?}\nun_sub_ty: {:#?}",
                    inner_ty, un_sub_ty
                );

                // Add a new constraint for the inner types if they are unsolvable.
                self.insert_constraint(inner_ty.clone(), un_sub_ty.clone());

                *inner_ty = un_sub_ty;
                is_solved = false;
            }
            SubResult::Err(err) => return Err(err),
        }

        Ok(is_solved)
    }

    fn solve_unknown_struct_member(
        &mut self,
        struct_ty: &Type,
        member_name: &str,
        finalize: bool,
    ) -> SubResult {
        // Get any possible substitution and replace the current type with
        // the new substituted type. The returned value might be the same as the
        // current, but doesn't hurt to replace with itself.
        let sub_struct_ty = match self.solve_substitution(struct_ty, finalize) {
            SubResult::UnSolved(sub_struct_ty) | SubResult::Solved(sub_struct_ty) => {
                self.cur_ty =
                    Type::UnknownStructMember(Box::new(sub_struct_ty.clone()), member_name.into());
                sub_struct_ty
            }

            err => return err,
        };

        match &sub_struct_ty {
            Type::CompoundType(struct_name, generics) => {
                // TODO: Fix this. Move `analyze_context` out
                //       of `type_context` and don't hardcode
                //       the default block ID.
                let var = match self.analyze_context.get_struct_member(
                    struct_name,
                    member_name,
                    BlockInfo::DEFAULT_BLOCK_ID,
                ) {
                    Ok(var) => var.clone(),
                    Err(err) => {
                        return SubResult::Err(err);
                    }
                };

                if let Some(ty) = &var.ret_type {
                    // Since this fetched the actual struct "template"
                    // that is used by all, the generics will still
                    // be the "Generics". Need to replace them with
                    // the actual type for this specific use of the
                    // struct.
                    let mut new_ty = ty.clone();
                    new_ty.replace_generics_impl(generics);

                    match self.solve_substitution(&new_ty, finalize) {
                        SubResult::Solved(sub_ty) => SubResult::Solved(sub_ty),
                        SubResult::UnSolved(un_sub_ty) => SubResult::UnSolved(un_sub_ty),
                        err => err,
                    }
                } else {
                    SubResult::Err(self.analyze_context.err(format!(
                        "Type not set for member \"{}\" in struct \"{}\"",
                        member_name, struct_name
                    )))
                }
            }

            _ => {
                if finalize {
                    SubResult::Err(self.analyze_context.err(format!(
                        "Struct access (member name \"{}\") on unknown struct type: {:?}",
                        member_name, sub_struct_ty
                    )))
                } else {
                    SubResult::UnSolved(self.cur_ty.clone())
                }
            }
        }
    }

    fn solve_unknown_struct_method(
        &mut self,
        struct_ty: &Type,
        method_name: &str,
        finalize: bool,
    ) -> SubResult {
        let sub_struct_ty = match self.solve_substitution(struct_ty, finalize) {
            SubResult::UnSolved(sub_struct_ty) | SubResult::Solved(sub_struct_ty) => {
                self.cur_ty =
                    Type::UnknownStructMethod(Box::new(sub_struct_ty.clone()), method_name.into());
                sub_struct_ty
            }

            err => return err,
        };

        match &sub_struct_ty {
            Type::CompoundType(struct_name, ..) => {
                // TODO: Fix this. Move `analyze_context` out
                //       of `type_context` and don't hardcode
                //       the default block ID.
                let method = match self.analyze_context.get_method(
                    struct_name,
                    method_name,
                    BlockInfo::DEFAULT_BLOCK_ID,
                ) {
                    Ok(method) => method.clone(),
                    Err(err) => {
                        return SubResult::Err(err);
                    }
                };

                if let Some(ret_type) = &method.ret_type {
                    match self.solve_substitution(ret_type, finalize) {
                        SubResult::Solved(sub_ty) => SubResult::Solved(sub_ty),
                        SubResult::UnSolved(un_sub_ty) => SubResult::UnSolved(un_sub_ty),
                        err => err,
                    }
                } else {
                    SubResult::Solved(Type::Void)
                }
            }

            _ => {
                if finalize {
                    SubResult::Err(self.analyze_context.err(format!(
                        "Method call (method name \"{}\") on non struct type: {:?}",
                        method_name, self.cur_ty
                    )))
                } else {
                    SubResult::UnSolved(self.cur_ty.clone())
                }
            }
        }
    }

    fn solve_unknown_method_argument(
        &mut self,
        struct_ty: &Type,
        method_name: &str,
        position: &Either<String, u64>,
        finalize: bool,
    ) -> SubResult {
        let sub_struct_ty = match self.solve_substitution(&struct_ty, finalize) {
            SubResult::UnSolved(sub_struct_ty) | SubResult::Solved(sub_struct_ty) => {
                self.cur_ty = Type::UnknownMethodArgument(
                    Box::new(sub_struct_ty.clone()),
                    method_name.into(),
                    position.clone(),
                );
                sub_struct_ty
            }

            err => return err,
        };

        match &sub_struct_ty {
            Type::CompoundType(struct_name, ..) => {
                // If this is a named argument, use that name to
                // identify the parameter type and then get the
                // index for the parameter. Otherwise use the
                // index of the argument directly.
                let actual_idx = match position {
                    Either::Left(arg_name) => {
                        // TODO: Fix this. Move `analyze_context` out
                        //       of `type_context` and don't hardcode
                        //       the default block ID.
                        match self.analyze_context.get_method_param_idx(
                            struct_name,
                            method_name,
                            arg_name,
                            BlockInfo::DEFAULT_BLOCK_ID,
                        ) {
                            Ok(idx) => idx,
                            Err(err) => return SubResult::Err(err),
                        }
                    }
                    Either::Right(idx) => *idx,
                };

                let arg_ty = match self.analyze_context.get_method_param_type(
                    struct_name,
                    method_name,
                    actual_idx,
                    BlockInfo::DEFAULT_BLOCK_ID,
                ) {
                    Ok(ty) => ty,
                    Err(err) => {
                        return SubResult::Err(err);
                    }
                };

                SubResult::Solved(arg_ty)
            }

            _ => {
                if finalize {
                    SubResult::Err(self.analyze_context.err(format!(
                        "Method call (method name \"{}\") argument used on non struct type(???): {:?}",
                        method_name, self.cur_ty
                    )))
                } else {
                    SubResult::UnSolved(self.cur_ty.clone())
                }
            }
        }
    }

    fn solve_unknown_array_member(&mut self, array_ty: &Type, finalize: bool) -> SubResult {
        let sub_array_ty = match self.solve_substitution(&array_ty, finalize) {
            SubResult::Solved(sub_ty) => {
                self.cur_ty = Type::UnknownArrayMember(Box::new(sub_ty.clone()));
                sub_ty
            }

            SubResult::UnSolved(un_sub_ty) => {
                // If it is known that this is an array but the
                // inner type is unknown, add a constrain between
                // the inner type and the return type of the
                // current "UnknownArrayMember".
                // This constraint is impossible to figure
                // out before this point, so this is a ugly fix
                // to ensure that nested arrays are solvable.
                if let Type::Array(inner_ty, _) = &un_sub_ty {
                    self.insert_constraint(self.cur_ty.clone(), *inner_ty.clone());
                }

                self.cur_ty = Type::UnknownArrayMember(Box::new(un_sub_ty.clone()));
                un_sub_ty
            }

            err => return err,
        };

        match &sub_array_ty {
            Type::Array(inner_ty, _) => match self.solve_substitution(inner_ty, finalize) {
                SubResult::Solved(sub_ty) => SubResult::Solved(sub_ty),
                SubResult::UnSolved(un_sub_ty) => SubResult::UnSolved(un_sub_ty),
                err => err,
            },

            _ => {
                if finalize {
                    SubResult::Err(
                        self.analyze_context
                            .err(format!("Array access on non Array type: {:?}", self.cur_ty)),
                    )
                } else {
                    SubResult::UnSolved(self.cur_ty.clone())
                }
            }
        }
    }

    // TODO: Is it possible to move this function to "Expr" in some way?
    pub fn get_expr_type(&self, expr_opt: Option<&Expr>) -> CustomResult<Type> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type()
        } else {
            Err(LangError::new(
                "expr opt set to None.".into(),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }
}
