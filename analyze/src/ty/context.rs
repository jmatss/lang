use super::constraint_sets::ConstraintSets;
use crate::{block::BlockInfo, AnalyzeContext};
use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::expr::Expr,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
    BlockId,
};
use core::panic;
use either::Either;
use log::debug;
use std::collections::{hash_map::Entry, HashMap};

// TODO: Remove `analyze_context` from here, makes no sense to have it nested
//       in this TypeContext. The TypeCOntexct currently needs to look up
//       struct declaration when solving substitutions.

// TODO: Add `set_generic_names()` calls when solving stuff. It is currently
//       used in `solve_unknown_structure_method()` & `solve_unknown_structure_method()`,
//       but would probably be best to be put into a more "generic" solve method,
//       ex. when solving all compound/array/pointer.

/// "Stand-alone" struct that will contain all the important state/context
/// after the type inference have been ran. This struct will then be used by
/// the "TypeSolver" to make sure that everything has gone as expected and
/// it will also replace all temporary "unknown" types with real ones.
pub struct TypeContext<'a> {
    /// Needed to look up ex. struct members.
    pub analyze_context: &'a mut AnalyzeContext,

    /// The key is a "unknown" type that is to be inferred and the value
    /// is the type that the "unknown" type infers to. This map will be updated
    /// continuously during this step but should contain all substitutions after
    /// this analyzer is done.
    ///
    /// If a "unknown" type doesn't have a substitution in this map at the end
    /// of this step, the program didn't have enough information to infer the type
    /// and the error should be reported.
    pub(super) substitutions: HashMap<BlockId, HashMap<Ty, Ty>>,

    /// This contains constraints/equalities that, from looking at the actual source
    /// code, needs to be true. Ex. "var x = y" would create a constraint that
    /// x and y needs to be equal. These constraints will be continuously added
    /// and also tried to be solved during this analyzing step.
    ///
    /// If a constraint can be solved, it will be "converted" to a substitution that
    /// will be moved to into `substitutions`. Ex. "var x: i32 = y" is a solvable
    /// constraint that would lead to a substitution "typeof(y) => i32".
    ///
    /// If a constraint is added that can never be true ex. "i32 == u32",
    /// that should be reported as a error.
    pub(super) constraints: HashMap<BlockId, ConstraintSets>,

    /// Is the current type when solving substitutions.
    cur_ty: Ty,
}

#[derive(Debug, Clone)]
pub enum SubResult {
    Solved(Ty),
    UnSolved(Ty),
    Err(LangError),
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        let tmp_ty = Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None);

        Self {
            analyze_context,
            substitutions: HashMap::default(),
            constraints: HashMap::default(),
            cur_ty: tmp_ty,
        }
    }

    fn substitutions(&mut self, root_id: BlockId) -> &mut HashMap<Ty, Ty> {
        self.substitutions.entry(root_id).or_default()
    }

    fn constraints(&mut self, root_id: BlockId) -> &mut ConstraintSets {
        self.constraints
            .entry(root_id)
            .or_insert_with(|| ConstraintSets::new(root_id))
    }

    /// Inserts a constraint. The `block_id` is used to decide which scope this
    /// contraint should be added to. It will be inseted into the "first" root block
    /// which contains the given `block_id` (this might be the `block_id` itself).
    pub fn insert_constraint(&mut self, lhs: Ty, rhs: Ty, block_id: BlockId) -> CustomResult<()> {
        let root_id = self.analyze_context.get_root_id(block_id)?;

        debug!(
            "insert_constraint, scope ID {} -- lhs: {:?}, rhs: {:?}",
            root_id, &lhs, &rhs
        );

        // Create constraints for the "inner" types if possible.
        match (&lhs, &rhs) {
            (Ty::CompoundType(_, lhs_gens, ..), Ty::CompoundType(_, rhs_gens, ..)) => {
                for (lhs_gen, rhs_gen) in lhs_gens.iter_types().zip(rhs_gens.iter_types()) {
                    self.insert_constraint(lhs_gen.clone(), rhs_gen.clone(), root_id)?;
                }
            }

            (Ty::Pointer(lhs_inner, ..), Ty::Pointer(rhs_inner, ..))
            | (Ty::Array(lhs_inner, ..), Ty::Array(rhs_inner, ..))
            | (
                Ty::UnknownStructureMember(lhs_inner, ..),
                Ty::UnknownStructureMember(rhs_inner, ..),
            )
            | (
                Ty::UnknownStructureMethod(lhs_inner, ..),
                Ty::UnknownStructureMethod(rhs_inner, ..),
            )
            | (
                Ty::UnknownMethodArgument(lhs_inner, ..),
                Ty::UnknownMethodArgument(rhs_inner, ..),
            )
            | (Ty::UnknownArrayMember(lhs_inner, ..), Ty::UnknownArrayMember(rhs_inner, ..)) => {
                self.insert_constraint(*lhs_inner.clone(), *rhs_inner.clone(), root_id)?;
            }

            _ => (),
        }

        if !lhs.is_any() && !rhs.is_any() {
            self.constraints(root_id).union(&lhs, &rhs);
        }

        Ok(())
    }

    pub fn solve_constraints(&mut self, block_id: BlockId) -> Result<(), Vec<LangError>> {
        let root_id = self
            .analyze_context
            .get_root_id(block_id)
            .map_err(|err| vec![err])?;

        if !self.constraints.contains_key(&root_id) {
            // TODO: Might be good to do a check to see that this is a valid
            //       block id. Currently it is just assumed that it is a valid
            //       block id with no constraints.
            // If the given scope is empty, return Ok, nothing to do here.
            return Ok(());
        }

        let mut constraint_sets = self.constraints(root_id).constraint_sets();

        debug!(
            "solve_constraints root_id {}, constraint sets:\n{:#?}",
            root_id, constraint_sets
        );

        // If `finalize` is set, first solve the constraints without finalizing
        // just to make sure that all possible types that can be solved without
        // being "forced" are solved. After that run it again to "force" solve
        // the types when there are no other options left.
        //
        // The second bool `req_all_solved` ("requires all solved") is used to
        // indicate if one has to solve ALL constraints in the set for it to count
        // as solving the set. If it is set to `false`, the set is counted as solved
        // if atleast one type is solved in the set.
        let options = [(false, true), (false, false), (true, false)];
        for (finalize, req_all_solved) in options.iter() {
            self.solve_constraints_priv(&mut constraint_sets, *finalize, *req_all_solved, root_id)?;
        }

        Ok(())
    }

    // TODO: Solve infinite recursive solves when generics get implemented.
    /// Solves the constraints. If `finalize` is set to true, all constraints
    /// must be solved and inserted as substitutions, otherwise `Error` will be
    /// returned. If `finalize` is set to false, it doesn't matter that all
    /// types wasn't solvable, `Ok` will be returned.
    fn solve_constraints_priv(
        &mut self,
        constraint_sets: &mut Vec<Vec<Ty>>,
        finalize: bool,
        requires_all_solved: bool,
        root_id: BlockId,
    ) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        let mut idx;
        let mut prev_end_set_count = constraint_sets.len() + 1;

        loop {
            idx = 0;

            while idx < constraint_sets.len() {
                match self.solve_constraint(
                    constraint_sets.get(idx).unwrap(),
                    finalize,
                    requires_all_solved,
                    root_id,
                ) {
                    Ok(true) => {
                        // Remove this `constraint_set` from the `constraint_sets`;
                        // only non-solved constraints should be stored in the list.
                        constraint_sets.swap_remove(idx);
                    }

                    Ok(false) => {
                        idx += 1;
                    }

                    Err(mut errs) => {
                        errors.append(&mut errs);
                        idx += 1;
                    }
                }
            }

            if constraint_sets.is_empty() {
                // All constraints solved, break out of the loop and return from function.
                break;
            } else if constraint_sets.len() == prev_end_set_count && finalize {
                // No set in the `constraint_sets` was solvable this time in the
                // "while" loop above even though `finalize` was set to true.
                // Types not solvable, report error.
                // TODO: Better error message.
                let err = self.analyze_context.err(format!(
                    "Unable to solve all constraint sets:\n{:#?}",
                    constraint_sets
                ));
                errors.push(err);
                break;
            } else if constraint_sets.len() == prev_end_set_count && !finalize {
                // No set in `constraint_sets` solvable, but finalize was set to
                // false. "break" and return from the function.
                break;
            } else {
                // At least one set in `constraint_sets` was solved in the logic
                // above. Continue with the rest of the constraint sets.
                prev_end_set_count = constraint_sets.len();
            }
        }

        debug!("Substitutions\n{:#?}", self.substitutions);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Solves the given `constraint_set`. If the constraint set was solvable,
    /// this function inserts the mapping into the substitutions and a `true` is
    /// returned. If unable to solve the constraint set, `false` is returned.
    /// If the constraint set can be proven to be not solvable, error(s) will be
    /// returned.
    fn solve_constraint(
        &mut self,
        constraint_set: &[Ty],
        finalize: bool,
        requires_all_solved: bool,
        root_id: BlockId,
    ) -> Result<bool, Vec<LangError>> {
        let mut errors = Vec::default();
        let mut solved_tys = Vec::default();

        for ty in constraint_set {
            match self.solve_substitution(ty, finalize, root_id) {
                SubResult::Solved(solved_ty) => solved_tys.push(solved_ty),
                SubResult::UnSolved(_) => (),
                SubResult::Err(err) => {
                    errors.push(err);
                    break;
                }
            }
        }

        // Solved if either 1. All types in the constraint set are solved,
        //               or 2. Atleast one of the types is solved and
        //                     `requires_all_solved` is NOT set indicating that
        //                     solving atleast one type solves the whole set.
        let is_all_solved = solved_tys.len() == constraint_set.len();
        let is_some_solved = !solved_tys.is_empty() && !requires_all_solved;
        let is_solved = is_some_solved || is_all_solved;

        if is_solved {
            if let Err(mut err) = self.create_substitution(&solved_tys, &constraint_set, root_id) {
                errors.append(&mut err);
            };
        }

        if errors.is_empty() {
            Ok(is_solved)
        } else {
            Err(errors)
        }
    }

    fn create_substitution(
        &mut self,
        solved_types: &[Ty],
        constraint_set: &[Ty],
        root_id: BlockId,
    ) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        // `Any` has the lowest precedence of all types, so start with the
        // variable set to it and assume that it will be overwritten.
        let mut preferred_ty = Ty::Any(TypeInfo::None);

        // Figure out the type with the highest precedence from the constraint
        // set. This will become the type that all other types in the set
        // should be converted to.
        for ty in solved_types.iter() {
            if ty.precedence(&preferred_ty) {
                preferred_ty = ty.clone();
            }
        }

        debug!("preferred_ty: {:#?}", preferred_ty);

        // The "preferred_ty" have been found. Add substitutions from all
        // the other types in the set (unsolved+solved) to this type.
        for ty in constraint_set.iter().chain(solved_types) {
            if let Err(err) = ty.assert_compatible(&preferred_ty) {
                errors.push(err);
                continue;
            }

            if let Err(err) = self.insert_substitution(ty.clone(), preferred_ty.clone(), root_id) {
                errors.push(err);
                continue;
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Inserts a new substitution, mapping `from` to `to`, in the scope with
    /// block ID `root_id`.
    pub fn insert_substitution(&mut self, from: Ty, to: Ty, root_id: BlockId) -> CustomResult<()> {
        if from == to || from.is_any() || to.is_any() {
            // Don't map a type to itself and don't map "Any" types.
            return Ok(());
        } else if self.causes_loop(&from, &to, root_id) {
            // TODO: Can this check be removed? Is it possible to end up here?
            //       This loop checking is very inefficient, would be good to
            //       remove if possible.
            unreachable!(
                "Substitution causes loop in scope ID {} -- from: {:#?}, to: {:#?}",
                root_id, &from, &to
            );
        }

        debug!(
            "Inserting substitution in scope ID {}, from:\n{:#?}\nto:\n{:#?}",
            root_id, from, to
        );

        // If the `from` doesn't already have a mapping, just insert the
        // new mapping to `to` and return. Otherwise if it already has a mapping,
        // use that mapping but make sure it is compatible with the new `to`.
        match self.substitutions(root_id).entry(from.clone()) {
            Entry::Vacant(v) => {
                v.insert(to.clone());
            }
            Entry::Occupied(o) => {
                o.get().assert_compatible(&to)?;
            }
        }

        // Insert substitutions for the "inner" types if possible.
        match (&from, &to) {
            (Ty::CompoundType(_, lhs_gens, ..), Ty::CompoundType(_, rhs_gens, ..)) => {
                for (lhs_gen, rhs_gen) in lhs_gens.iter_types().zip(rhs_gens.iter_types()) {
                    self.insert_substitution(lhs_gen.clone(), rhs_gen.clone(), root_id)?;
                }
            }

            (Ty::Pointer(lhs_inner, ..), Ty::Pointer(rhs_inner, ..))
            | (Ty::Array(lhs_inner, ..), Ty::Array(rhs_inner, ..))
            | (
                Ty::UnknownStructureMember(lhs_inner, ..),
                Ty::UnknownStructureMember(rhs_inner, ..),
            )
            | (
                Ty::UnknownStructureMethod(lhs_inner, ..),
                Ty::UnknownStructureMethod(rhs_inner, ..),
            )
            | (
                Ty::UnknownMethodArgument(lhs_inner, ..),
                Ty::UnknownMethodArgument(rhs_inner, ..),
            )
            | (Ty::UnknownArrayMember(lhs_inner, ..), Ty::UnknownArrayMember(rhs_inner, ..)) => {
                self.insert_substitution(*lhs_inner.clone(), *rhs_inner.clone(), root_id)?;
            }

            _ => (),
        }

        Ok(())
    }

    /// Checks if adding a substitution from `from` to `to` creates a loop.
    fn causes_loop(&self, from: &Ty, to: &Ty, root_id: BlockId) -> bool {
        if let Some(new_to) = self
            .substitutions
            .get(&root_id)
            .map(|subs| subs.get(to))
            .flatten()
        {
            if from == new_to {
                true
            } else {
                self.causes_loop(from, new_to, root_id)
            }
        } else {
            false
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
    pub fn solve_substitution(&mut self, ty: &Ty, finalize: bool, block_id: BlockId) -> SubResult {
        let root_id = match self.analyze_context.get_root_id(block_id) {
            Ok(root_id) => root_id,
            Err(err) => return SubResult::Err(err),
        };

        static MAX_LOOP_ITERATIONS: i32 = 50;
        self.cur_ty = ty.clone();

        for i in 0..MAX_LOOP_ITERATIONS {
            debug!(
                "Substituting in scope ID {}, i: {} -- cur_ty: {:?}",
                root_id, i, self.cur_ty
            );

            let local_cur_ty = self.cur_ty.clone();

            if self.substitutions(root_id).contains_key(&local_cur_ty) {
                // Set the substitute type as the current type and and try to
                // solve the type iteratively.
                self.cur_ty = self
                    .substitutions(root_id)
                    .get(&local_cur_ty)
                    .unwrap()
                    .clone();
            } else {
                match &self.cur_ty {
                    Ty::CompoundType(..) => {
                        return self.solve_compound_type(finalize, root_id);
                    }
                    Ty::Pointer(..) | Ty::Array(..) => {
                        return self.solve_aggregate(finalize, root_id);
                    }
                    Ty::Expr(expr, ..) => {
                        if let Ok(ty) = expr.get_expr_type() {
                            return self.solve_substitution(&ty, finalize, root_id);
                        } else {
                            unreachable!("No type for expr: {:#?}", expr);
                        }
                    }
                    Ty::UnknownStructureMember(..) => {
                        return self.solve_unknown_structure_member(finalize, root_id);
                    }
                    Ty::UnknownStructureMethod(..) => {
                        return self.solve_unknown_structure_method(finalize, root_id);
                    }
                    Ty::UnknownMethodArgument(..) => {
                        return self.solve_unknown_method_argument(finalize, root_id);
                    }
                    Ty::UnknownArrayMember(..) => {
                        return self.solve_unknown_array_member(finalize, root_id);
                    }
                    Ty::Any(..) => {
                        return self.solve_any(finalize);
                    }
                    Ty::Generic(..) | Ty::GenericInstance(..) => {
                        return self.solve_generic(finalize);
                    }
                }
            }
        }

        // Unable to find any substitution after `MAX_LOOP_ITERATIONS` iterations.
        // Assume that the high number of interations is a indication of a
        // infinite loop and report as error.
        SubResult::Err(self.analyze_context.err(format!(
            "Unable to solve substitution after {} iterations, last cur_ty: {:#?}",
            MAX_LOOP_ITERATIONS, self.cur_ty
        )))
    }

    /// Solves compound types (i.e. types that might contain generics).
    fn solve_compound_type(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();
        let mut is_solved;

        if let Ty::CompoundType(inner_ty, generics, ..) = &mut cur_ty_modified {
            is_solved = true;

            // All inner types needs to be solved. If any of them can't be solved,
            // this whole aggregate type will count as unsolvable.
            for gen_ty in generics.iter_types_mut() {
                match self.solve_substitution(gen_ty, finalize, root_id) {
                    SubResult::Solved(solved_ty) => {
                        *gen_ty = solved_ty.clone();
                    }

                    SubResult::UnSolved(..) => {
                        is_solved = false;
                    }

                    err => return err,
                }
            }

            // Make a check to see if the inner type is solved as well.
            // If this is to be finalized, solve the unknown ints and floats.
            match inner_ty {
                InnerTy::UnknownInt(..) if finalize => {
                    *inner_ty = InnerTy::default_int();
                }
                InnerTy::UnknownFloat(..) if finalize => {
                    *inner_ty = InnerTy::default_float();
                }
                InnerTy::UnknownInt(..) | InnerTy::UnknownFloat(..) | InnerTy::Unknown(..) => {
                    is_solved = false
                }

                InnerTy::UnknownIdent(ident, id) => {
                    // Check if this unknown structure can be found and then
                    // replaced the inner type with the correct structure.
                    if self.analyze_context.get_struct(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Struct(ident.clone());
                    } else if self.analyze_context.get_enum(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Enum(ident.clone());
                    } else if self.analyze_context.get_trait(ident, *id).is_ok() {
                        *inner_ty = InnerTy::Trait(ident.clone());
                    } else {
                        is_solved = false;
                    }
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

    /// Solves aggregate types (array or pointer).
    fn solve_aggregate(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        match &mut cur_ty_modified {
            Ty::Pointer(ty, ..) | Ty::Array(ty, ..) => {
                match self.solve_substitution(ty.as_ref(), finalize, root_id) {
                    SubResult::Solved(solved_ty) => {
                        self.cur_ty = cur_ty_backup;

                        *ty = Box::new(solved_ty);
                        SubResult::Solved(cur_ty_modified)
                    }

                    SubResult::UnSolved(..) => {
                        self.cur_ty = cur_ty_backup;

                        SubResult::UnSolved(cur_ty_modified)
                    }

                    err => err,
                }
            }

            _ => {
                unreachable!("This function will only be called when it is a pointer/array type.");
            }
        }
    }

    /// Solves generic types.
    fn solve_generic(&mut self, finalize: bool) -> SubResult {
        match self.cur_ty {
            Ty::Generic(..) if finalize => SubResult::Solved(self.cur_ty.clone()),
            Ty::Generic(..) if !finalize => SubResult::UnSolved(self.cur_ty.clone()),
            Ty::GenericInstance(..) => SubResult::Solved(self.cur_ty.clone()),
            _ => unreachable!(),
        }
    }

    /// Solves "Any" types.
    fn solve_any(&mut self, finalize: bool) -> SubResult {
        if finalize {
            SubResult::Solved(self.cur_ty.clone())
        } else {
            SubResult::UnSolved(self.cur_ty.clone())
        }
    }

    fn solve_unknown_structure_member(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownStructureMember(ty, member_name, ..) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let member_name = member_name.clone();

            debug!("solve_unknown_structure_member: {:#?}", cur_ty_backup);

            // Get any possible substitution.
            let mut solved_ty = match self.solve_substitution(ty, finalize, root_id) {
                SubResult::Solved(solved_ty) => {
                    *ty = Box::new(solved_ty.clone());
                    solved_ty
                }

                SubResult::UnSolved(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                err => return err,
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            if let Err(err) = self.set_generic_names(&mut solved_ty, id) {
                return SubResult::Err(err);
            }

            let (inner_ty, generics) = match &solved_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..)
                | Ty::Generic(..)
                | Ty::GenericInstance(..)
                | Ty::Any(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

                Ty::Pointer(ty_box, ..) => {
                    if let Ty::CompoundType(inner_ty, generics, ..) = ty_box.as_ref() {
                        (inner_ty, generics)
                    } else {
                        self.cur_ty = cur_ty_backup;
                        return SubResult::UnSolved(cur_ty_modified);
                    }
                }

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownStructureMember: {:#?}",
                        solved_ty
                    )))
                }
            };

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

                InnerTy::Trait(ident) => {
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
                if let Err(err) = self.insert_substitution(cur_ty_backup, new_ty.clone(), root_id) {
                    return SubResult::Err(err);
                }

                self.solve_substitution(&new_ty, finalize, root_id)
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

    fn solve_unknown_structure_method(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownStructureMethod(ty, method_name, ..) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let method_name = method_name.clone();

            let mut solved_ty = match self.solve_substitution(ty, finalize, root_id) {
                SubResult::Solved(solved_ty) => {
                    *ty = Box::new(solved_ty.clone());
                    solved_ty
                }

                SubResult::UnSolved(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                err => return err,
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            if let Err(err) = self.set_generic_names(&mut solved_ty, id) {
                return SubResult::Err(err);
            }

            let (inner_ty, generics) = match &solved_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..)
                | Ty::Generic(..)
                | Ty::GenericInstance(..)
                | Ty::Any(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

                // TODO: Is this valid? Solve this in a better way. There are currently
                //       two points in the code where this fix is needed.
                // The type given to the `UnknownStructureMethod` might possibly
                // have been a pointer to the structure instead of the structure
                // type itself.
                Ty::Pointer(ty_box, ..) => {
                    if let Ty::CompoundType(inner_ty, generics, ..) = ty_box.as_ref() {
                        (inner_ty, generics)
                    } else {
                        self.cur_ty = cur_ty_backup;
                        return SubResult::UnSolved(cur_ty_modified);
                    }
                }

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownStructureMethod: {:#?}",
                        solved_ty
                    )))
                }
            };

            let method = match inner_ty {
                InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Trait(ident) => {
                    match self.analyze_context.get_method(ident, &method_name, id) {
                        Ok(method) => method,
                        Err(err) => return SubResult::Err(err),
                    }
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
                if let Err(err) = self.insert_substitution(cur_ty_backup, new_ty.clone(), root_id) {
                    return SubResult::Err(err);
                }

                self.solve_substitution(&new_ty, finalize, root_id)
            } else {
                // The return type of the method is None == Void.
                let ty = Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None);
                if let Err(err) = self.insert_substitution(cur_ty_backup, ty.clone(), root_id) {
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

    fn solve_unknown_method_argument(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownMethodArgument(ty, method_name, name_or_idx, ..) = &mut cur_ty_modified {
            // Work around to make borrow checker happy (so that one can use
            // `local_cur_ty` further down in this block).
            let method_name = method_name.clone();
            let name_or_idx = name_or_idx.clone();

            // Get any possible substitution and replace the current type with
            // the new substituted type. The returned value might be the same as
            // the current, but doesn't hurt to replace with itself.
            let mut solved_ty = match self.solve_substitution(ty, finalize, root_id) {
                SubResult::Solved(solved_ty) => {
                    *ty = Box::new(solved_ty.clone());
                    solved_ty
                }

                SubResult::UnSolved(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                err => return err,
            };

            // TODO: Don't hardcode default id.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            if let Err(err) = self.set_generic_names(&mut solved_ty, id) {
                return SubResult::Err(err);
            }

            let (inner_ty, generics) = match &solved_ty {
                Ty::CompoundType(InnerTy::UnknownIdent(..), ..)
                | Ty::UnknownStructureMember(..)
                | Ty::UnknownStructureMethod(..)
                | Ty::UnknownMethodArgument(..)
                | Ty::Generic(..)
                | Ty::GenericInstance(..)
                | Ty::Any(..) => {
                    self.cur_ty = cur_ty_backup;
                    return SubResult::UnSolved(cur_ty_modified);
                }

                Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

                // TODO: Solve nested ty in better way.
                Ty::Pointer(ty_box, ..) => {
                    if let Ty::CompoundType(inner_ty, generics, ..) = ty_box.as_ref() {
                        (inner_ty, generics)
                    } else {
                        self.cur_ty = cur_ty_backup;
                        return SubResult::UnSolved(cur_ty_modified);
                    }
                }

                _ => {
                    return SubResult::Err(self.analyze_context.err(format!(
                        "Invalid struct type of UnknownMethodArgument: {:#?}",
                        solved_ty
                    )))
                }
            };

            let structure_name = match inner_ty {
                InnerTy::Struct(structure_name)
                | InnerTy::Enum(structure_name)
                | InnerTy::Trait(structure_name) => structure_name,

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

            // If this is a named argument, use that name to identify the parameter
            // type and then get the index for the parameter. Otherwise use the
            // index of the argument directly.
            let actual_idx = match name_or_idx {
                Either::Left(arg_name) => {
                    match self.analyze_context.get_method_param_idx(
                        structure_name,
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
                structure_name,
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
            if let Err(err) = self.insert_substitution(cur_ty_backup, arg_ty.clone(), root_id) {
                return SubResult::Err(err);
            }

            self.solve_substitution(&arg_ty, finalize, root_id)
        } else {
            unreachable!(
                "This function will only be called when it is a UnknownMethodArgument type."
            );
        }
    }

    fn solve_unknown_array_member(&mut self, finalize: bool, root_id: BlockId) -> SubResult {
        // Since this function will call other functions that updates `cur_ty`
        // recursively, need to save a local copy that will be restored before
        // this function returns.
        let cur_ty_backup = self.cur_ty.clone();
        let mut cur_ty_modified = self.cur_ty.clone();

        if let Ty::UnknownArrayMember(ty, ..) = &mut cur_ty_modified {
            // Add a substitution from the "UnknownArrayMember" to the actual
            // type of the array if it has been found.
            match self.solve_substitution(&ty, finalize, root_id) {
                SubResult::Solved(solved_ty) => {
                    if let Ty::Array(real_ty, ..) = solved_ty {
                        if let Err(err) = self.insert_substitution(
                            cur_ty_backup.clone(),
                            *real_ty.clone(),
                            root_id,
                        ) {
                            SubResult::Err(err)
                        } else {
                            self.cur_ty = cur_ty_backup;
                            SubResult::Solved(*real_ty)
                        }
                    } else {
                        SubResult::Err(self.analyze_context.err(format!(
                            "Epected type to be array. unknown: {:#?}, solved: {:#?}",
                            ty, solved_ty
                        )))
                    }
                }

                SubResult::UnSolved(..) => {
                    self.cur_ty = cur_ty_backup;
                    SubResult::UnSolved(cur_ty_modified)
                }

                res => res,
            }
        } else {
            unreachable!("This function will only be called when it is a UnknownArrayMember type.");
        }
    }

    /// If the given type `ty` contains generics that don't have their "names"
    /// set, this function will fetch the structure and set the names if possible.
    pub fn set_generic_names(&self, ty: &mut Ty, block_id: BlockId) -> CustomResult<()> {
        let (inner_ty, generics) = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

            Ty::Pointer(ty_box, ..) | Ty::Array(ty_box, ..) => {
                return self.set_generic_names(ty_box, block_id);
            }

            _ => return Ok(()),
        };

        if !generics.is_empty() && generics.len_names() == 0 {
            match inner_ty {
                InnerTy::Struct(ident) => match self.analyze_context.get_struct(ident, block_id) {
                    Ok(struct_) => {
                        if let Some(generic_names) = &struct_.borrow().generics {
                            for (idx, gen_name) in generic_names.iter().enumerate() {
                                generics.insert_lookup(gen_name.clone(), idx);
                                generics.insert_name(gen_name.clone());
                            }
                        }
                    }

                    Err(err) => return Err(err),
                },

                _ => panic!(
                    "TODO: Implement for more types. generics: {:#?}, inner_ty: {:#?}",
                    generics, inner_ty
                ),
            }
        }

        Ok(())
    }

    // TODO: Is it possible to move this function to "Expr" in some way?
    pub fn get_expr_type(&self, expr_opt: Option<&Expr>) -> CustomResult<Ty> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type()
        } else {
            Err(LangError::new(
                "expr opt set to None.".into(),
                AnalyzeError,
                None,
            ))
        }
    }
}
