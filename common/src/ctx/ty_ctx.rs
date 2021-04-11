use std::collections::HashSet;

use either::Either;

use crate::{
    ctx::ty_env::SolveCond,
    error::{LangError, LangErrorKind, LangResult},
    path::{LangPath, LangPathPart},
    token::{block::Fn, expr::Expr},
    ty::{
        generics::Generics, inner_ty::InnerTy, substitution_sets::SubstitutionSets, ty::Ty,
        type_info::TypeInfo,
    },
    TypeId,
};

use super::{ast_ctx::AstCtx, ty_env::TyEnv};

/// A enum representing the status/progress of solving a ADT type.
enum AdtSolveStatus {
    /// The given ADT type was solved. Return the path and generics of the solved ADT.
    Solved(LangPath, Generics),

    /// Some progress was made, but the ADT wasn't solved. Return the progressed
    /// TypeId (not the original).
    Progress(TypeId),

    /// The given ADT type is a GenericInstance. The TypeId is the representation
    /// of the GenericInstance.
    GenericInstance(TypeId),

    /// No progress made.
    NoProgress,
}

#[derive(Debug)]
pub struct TyCtx {
    /// Contains information about all types in the type system and keeps a
    /// mapping from TypeId's to Ty's.
    pub ty_env: TyEnv,

    /// Contains all substitution sets in the type system. This stores information
    /// about all types that "interact" with eachother in the type system.
    /// This is used to ensure that the "interactions" are valid (type checking)
    /// and is also used to infer types (type inference).
    pub sub_sets: SubstitutionSets,
}

impl<'a> TyCtx {
    pub fn new(ty_env: TyEnv) -> Self {
        Self {
            ty_env,
            sub_sets: SubstitutionSets::default(),
        }
    }

    pub fn pretty_print_subs(&self) {
        self.sub_sets.debug_print(&self.ty_env)
    }

    /// Returns the type contained in the root node of the substitution set that
    /// contains the type `type_id`. This will be the type with the highest
    /// precedence in the set containing the block with ID `block_id`.
    ///
    /// If the given type doesn't belong to a set, returns the type itself as the
    /// inferred type. If the given `type_id` has no root block ID set, a error
    /// will be returned.
    pub fn inferred_type(&self, type_id: TypeId) -> LangResult<TypeId> {
        let type_id = self.ty_env.get_forwarded(type_id);
        self.sub_sets.inferred_type(type_id)
    }

    /// Inserts a new constraint between two types.
    /// If the types are the equal, this function will do a early Ok return.
    pub fn insert_constraint(&mut self, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<()> {
        let type_id_a = self.ty_env.get_forwarded(type_id_a);
        let type_id_b = self.ty_env.get_forwarded(type_id_b);

        if type_id_a == type_id_b {
            return Ok(());
        }

        debug!(
            "insert_constraint -- type_id_a: {}, ty_a: {:#?}\ntype_id_b: {}, ty_b: {:#?}",
            type_id_a,
            self.ty_env.ty(type_id_a)?,
            type_id_b,
            self.ty_env.ty(type_id_b)?,
        );

        if !self.ty_env.is_any(type_id_a)? && !self.ty_env.is_any(type_id_b)? {
            self.unify(type_id_a, type_id_b)?;
        }

        Ok(())
    }

    /// "Unifies" the two types `type_id_a` and `type_id_b`.
    /// This function also inserts new constraints for any inner types.
    fn unify(&mut self, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<TypeId> {
        // TODO: Make safe.
        let tmp_self = unsafe { (self as *const TyCtx).as_ref().unwrap() };

        let inf_type_id = self.sub_sets.union(tmp_self, type_id_a, type_id_b)?;

        debug!(
            "After union, type_id_a: {}, type_id_b: {}, inf_type_id: {}",
            type_id_a, type_id_b, inf_type_id
        );

        self.insert_constraint_inner(type_id_a, inf_type_id)?;
        self.insert_constraint_inner(type_id_b, inf_type_id)?;

        Ok(inf_type_id)
    }

    /// Inserts constraints for potential "inner types" of types `ty` and `inferred_ty`.
    fn insert_constraint_inner(&mut self, type_id: TypeId, inf_type_id: TypeId) -> LangResult<()> {
        let type_id = self.ty_env.get_forwarded(type_id);
        let inf_type_id = self.ty_env.get_forwarded(inf_type_id);

        if type_id == inf_type_id {
            return Ok(());
        }

        let ty = self.ty_env.ty(type_id)?.clone();
        let inf_ty = self.ty_env.ty(inf_type_id)?.clone();

        match (ty, inf_ty) {
            (Ty::CompoundType(_, ty_a_gens, ..), Ty::CompoundType(_, ty_b_gens, ..)) => {
                for (ty_a_gen, ty_b_gen) in ty_a_gens.iter_types().zip(ty_b_gens.iter_types()) {
                    self.insert_constraint_inner(*ty_a_gen, *ty_b_gen)?;
                    self.insert_constraint(*ty_a_gen, *ty_b_gen)?;
                }
                Ok(())
            }

            (Ty::Pointer(ty_a_inner, ..), Ty::Pointer(ty_b_inner, ..))
            | (Ty::Array(ty_a_inner, ..), Ty::Array(ty_b_inner, ..))
            | (Ty::UnknownArrayMember(ty_a_inner, ..), Ty::UnknownArrayMember(ty_b_inner, ..)) => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner)?;
                self.insert_constraint(ty_a_inner, ty_b_inner)
            }

            (
                Ty::UnknownAdtMethod(ty_a_inner, a_name, ..),
                Ty::UnknownAdtMethod(ty_b_inner, b_name, ..),
            )
            | (
                Ty::UnknownAdtMember(ty_a_inner, a_name, ..),
                Ty::UnknownAdtMember(ty_b_inner, b_name, ..),
            ) if a_name == b_name => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner)?;
                self.insert_constraint(ty_a_inner, ty_b_inner)
            }

            (
                Ty::UnknownMethodArgument(ty_a_inner, a_name, a_idx_or_name, ..),
                Ty::UnknownMethodArgument(ty_b_inner, b_name, b_idx_or_name, ..),
            ) if a_name == b_name && a_idx_or_name == b_idx_or_name => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner)?;
                self.insert_constraint(ty_a_inner, ty_b_inner)
            }

            (
                Ty::UnknownMethodGeneric(ty_a_inner, a_name, a_idx, ..),
                Ty::UnknownMethodGeneric(ty_b_inner, b_name, b_idx, ..),
            ) if a_name == b_name && a_idx == b_idx => {
                self.insert_constraint_inner(ty_a_inner, ty_b_inner)?;
                self.insert_constraint(ty_a_inner, ty_b_inner)
            }

            _ => Ok(()),
        }
    }

    /// Iterates through all types found in the TypeEnvironment and tries to
    /// solve them.
    pub fn solve_all(&mut self, ast_ctx: &AstCtx) -> LangResult<()> {
        for type_id in self.ty_env.all_types() {
            self.solve(ast_ctx, type_id)?;
        }
        Ok(())
    }

    /// Iterates through all types in the type environment. For any type that
    /// is the inferred type of its substitution set and is a type that currently
    /// isn't known but can be turned into a default type, convert it to the
    /// default type.
    pub fn convert_defaults(&mut self) -> LangResult<()> {
        for type_id in self.ty_env.all_types() {
            let inf_type_id = self.inferred_type(type_id)?;
            if type_id == inf_type_id {
                self.convert_default(type_id)?;
            }
        }
        Ok(())
    }

    /// Converts any unknown values to their corresponding "default" values
    /// if possible. This includes ints and floats that are converted to i32
    /// and f32 respectively.
    pub fn convert_default(&mut self, type_id: TypeId) -> LangResult<()> {
        let inf_type_id = self.inferred_type(type_id)?;

        match self.ty_env.ty(inf_type_id)?.clone() {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if inner_ty.is_unknown_int() || inner_ty.is_unknown_float() {
                    self.replace_default(inf_type_id)?;
                }

                for gen_type_id in generics.iter_types() {
                    self.convert_default(*gen_type_id)?;
                }
            }

            Ty::Array(type_id_i, expr_opt, ..) => {
                self.convert_default(type_id_i)?;

                if let Some(expr) = expr_opt {
                    if let Ok(expr_type_id) = expr.get_expr_type() {
                        self.convert_default(expr_type_id)?;
                    }
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.convert_default(ret_type_id)?;
                }
                for gen_type_id in gens {
                    self.convert_default(gen_type_id)?;
                }
                for param_type_id in params {
                    self.convert_default(param_type_id)?;
                }
            }

            Ty::Expr(expr, _) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.convert_default(expr_type_id)?;
                }
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.convert_default(type_id_i)?;
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(())
    }

    /// When this function is called, the type with ID `id` is known to be a
    /// compound type containing either a int or float unknown.
    fn replace_default(&mut self, id: TypeId) -> LangResult<()> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty_env.ty_mut(id)? {
            if inner_ty.is_unknown_int() {
                *inner_ty = InnerTy::default_int();
            } else if inner_ty.is_unknown_float() {
                *inner_ty = InnerTy::default_float();
            }
        }
        Ok(())
    }

    /// Given a type `type_id`, tries to solve it recursively by looking at structure/
    /// function declarations. This is needed for types that can't be figured out
    /// directly from looking at the "use site" in the source code, some information
    /// needs to be fetched from somewhere else to deduce the correct type.
    ///
    /// This function should be the entry call when solving a new type. If this
    /// is a recursive solve call inside a solve, the `priv_solve()` should be
    /// used which keeps track of types seen to prevent infinite recursion.
    ///
    /// OBS! This function does NOT return the final solved type ID, it might
    ///      return a partially solved type in some cases. If you want the type
    ///      ID that are "closest" to the correctly solved type ID, use the
    ///      `inferred_type()` function.
    pub fn solve(&mut self, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<TypeId> {
        let mut seen_type_ids = HashSet::default();
        self.solve_priv(ast_ctx, type_id, &mut seen_type_ids)
    }

    /// Tries to solve the given `type_id` type and also keeps track of type IDs
    /// seen during the solving to prevent infinite loops (in `seen_type_ids`).
    ///
    /// The solving process first starts by looking at the substitition sets to
    /// see if the given type is solved already. If that is the case, that solved
    /// type will be returned. If it is not solved already, a "manual" solve will
    /// be done. This will recursively traverse through the type and its nested
    /// types to try and solve them.
    ///
    /// This two step logic will be applied to the given `type_id` first and if
    /// that is not solvable, the same logic will be applied to its inferred type.
    fn solve_priv(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let fwd_type_id = self.ty_env.get_forwarded(type_id);

        debug!(
            "solve_priv -- type_id: {}, fwd_type_id: {}, fwd_ty: {:#?}",
            type_id,
            fwd_type_id,
            self.ty_env.ty(fwd_type_id)
        );

        if seen_type_ids.contains(&fwd_type_id) {
            return self.inferred_type(fwd_type_id);
        } else {
            seen_type_ids.insert(fwd_type_id);
        }

        // TODO: Make safe.
        let tmp_self = unsafe { (self as *const TyCtx).as_ref().unwrap() };

        let inf_type_id = self.inferred_type(fwd_type_id)?;

        let check_inf = false;
        let solve_cond = SolveCond::new().excl_default().excl_unknown();
        if self
            .ty_env
            .is_solved(&self.sub_sets, fwd_type_id, check_inf, solve_cond)?
        {
            let root_type_id = self.sub_sets.promote(tmp_self, fwd_type_id)?;
            self.insert_constraint(fwd_type_id, inf_type_id)?;
            return Ok(root_type_id);
        }

        let solved_type_id = self.solve_manual(ast_ctx, fwd_type_id, seen_type_ids)?;

        let check_inf = false;
        let solve_cond = SolveCond::new().excl_default().excl_unknown();
        if self
            .ty_env
            .is_solved(&self.sub_sets, solved_type_id, check_inf, solve_cond)?
        {
            return Ok(solved_type_id);
        }

        let inf_type_id = self.inferred_type(fwd_type_id)?;

        let check_inf = true;
        let solve_cond = SolveCond::new().excl_default().excl_unknown();
        if self
            .ty_env
            .is_solved(&self.sub_sets, inf_type_id, check_inf, solve_cond)?
        {
            self.insert_constraint(fwd_type_id, inf_type_id)?;
            return Ok(inf_type_id);
        }

        if seen_type_ids.contains(&inf_type_id) {
            return self.inferred_type(inf_type_id);
        } else {
            seen_type_ids.insert(inf_type_id);
        }

        let inf_solved_type_id = self.solve_manual(ast_ctx, inf_type_id, seen_type_ids)?;
        self.insert_constraint(inf_type_id, inf_solved_type_id)?;

        Ok(inf_solved_type_id)
    }

    /// Given a type `type_id`, tries to solve it. This function does NOT look
    /// in either the `substition_sets` or its inferred type to solve this
    /// specific `type_id` (use `solve()` in that case). This is only true for
    /// the first "layer" of the solve, any inner types will be solved recursively
    /// with using lookups etc.
    fn solve_manual(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        match self.ty_env.ty(type_id)? {
            Ty::CompoundType(..) => self.solve_compound(ast_ctx, type_id, seen_type_ids),
            Ty::Pointer(..) | Ty::Array(..) => {
                self.solve_aggregate(ast_ctx, type_id, seen_type_ids)
            }

            Ty::Expr(..) => self.solve_expr(ast_ctx, type_id, seen_type_ids),
            Ty::Fn(..) => self.solve_fn(ast_ctx, type_id, seen_type_ids),

            Ty::UnknownAdtMember(..) => {
                self.solve_unknown_adt_member(ast_ctx, type_id, seen_type_ids)
            }
            Ty::UnknownAdtMethod(..) => {
                self.solve_unknown_adt_method(ast_ctx, type_id, seen_type_ids)
            }
            Ty::UnknownMethodArgument(..) => {
                self.solve_unknown_method_argument(ast_ctx, type_id, seen_type_ids)
            }
            Ty::UnknownMethodGeneric(..) => {
                self.solve_unknown_method_generic(ast_ctx, type_id, seen_type_ids)
            }
            Ty::UnknownArrayMember(..) => {
                self.solve_unknown_array_member(ast_ctx, type_id, seen_type_ids)
            }

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(type_id),
        }
    }

    /// Solves compound types (i.e. types that might contain generics).
    fn solve_compound(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty_clone = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_compound -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
            type_id, seen_type_ids, ty_clone
        );

        let mut was_updated = false;

        let (inner_ty, generics) = if let Ty::CompoundType(inner_ty, generics, ..) = &mut ty_clone {
            (inner_ty, generics)
        } else {
            unreachable!();
        };

        let mut new_nested_seen_type_ids: HashSet<TypeId> = HashSet::new();
        for gen_type_id in generics.iter_types_mut() {
            let mut seen_type_ids_snapshot = seen_type_ids.clone();
            self.solve_priv(ast_ctx, *gen_type_id, &mut seen_type_ids_snapshot)?;
            new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));

            let inf_gen_type_id = self.inferred_type(*gen_type_id)?;
            if *gen_type_id != inf_gen_type_id {
                *gen_type_id = inf_gen_type_id;
                was_updated = true;
            }
        }
        seen_type_ids.extend(new_nested_seen_type_ids);

        // Solve the inner structure type.
        if let InnerTy::UnknownIdent(path, block_id) = inner_ty {
            let full_path_opt = if let Ok(full_path) =
                ast_ctx.calculate_adt_full_path(self, &path.without_gens(), *block_id)
            {
                Some(full_path)
            } else if let Ok(full_path) = ast_ctx.calculate_trait_full_path(self, &path, *block_id)
            {
                Some(full_path)
            } else {
                None
            };

            if let Some(full_path) = full_path_opt {
                // Add the already (possible) existing generics to the full path.
                let mut full_path_with_gens = full_path.clone();
                let last_part = full_path_with_gens.pop().unwrap();
                full_path_with_gens.push(LangPathPart(last_part.0, Some(generics.clone())));

                let new_inner_type_id = if ast_ctx.is_struct(self, &full_path) {
                    Some(InnerTy::Struct(full_path_with_gens))
                } else if ast_ctx.is_enum(self, &full_path) {
                    Some(InnerTy::Enum(full_path_with_gens))
                } else if ast_ctx.is_union(self, &full_path) {
                    Some(InnerTy::Union(full_path_with_gens))
                } else if ast_ctx.is_trait(self, &full_path) {
                    Some(InnerTy::Trait(full_path_with_gens))
                } else {
                    None
                };

                if let Some(new_inner_type_id) = new_inner_type_id {
                    *inner_ty = new_inner_type_id;
                    was_updated = true;
                }
            }
        }

        let inf_type_id = if was_updated {
            let new_type_id = self.ty_env.id(&ty_clone)?;
            self.insert_constraint(type_id, new_type_id)?;
            new_type_id
        } else {
            type_id
        };

        Ok(inf_type_id)
    }

    /// Solves aggregate types (array or pointer).
    fn solve_aggregate(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_aggregate -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
            type_id, seen_type_ids, ty
        );

        // TODO: Probably need to implement solving of the array dimension expr.
        let new_type_id = match &mut ty {
            Ty::Pointer(aggr_type_id, ..) | Ty::Array(aggr_type_id, ..) => {
                self.solve_priv(ast_ctx, *aggr_type_id, seen_type_ids)?;
                let inf_type_id = self.inferred_type(*aggr_type_id)?;

                if &inf_type_id != aggr_type_id {
                    *aggr_type_id = inf_type_id;

                    let new_type_id = self.ty_env.id(&ty)?;
                    self.insert_constraint(type_id, new_type_id)?;

                    new_type_id
                } else {
                    type_id
                }
            }
            _ => unreachable!(),
        };

        let inf_type_id = self.inferred_type(type_id)?;
        self.insert_constraint_inner(type_id, inf_type_id)?;

        // TODO: Should this return `inf_type_id`? The `new_type_id` is not 100%
        //       the most optimaly solved type, but might need to return so that
        //       the caller can work on that type.
        Ok(new_type_id)
    }

    fn solve_expr(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let ty = self.ty_env.ty_mut(type_id)?;

        debug!(
            "solve_expr -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
            type_id, seen_type_ids, ty
        );

        let inf_type_id = if let Ty::Expr(expr, ..) = ty {
            let expr_type_id = expr.get_expr_type()?;
            self.solve_priv(ast_ctx, expr_type_id, seen_type_ids)?
        } else {
            type_id
        };

        Ok(inf_type_id)
    }

    fn solve_fn(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let ty = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_fn -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
            type_id, seen_type_ids, ty
        );

        if let Ty::Fn(gens, params, ret_type_id_opt, ..) = ty {
            let mut new_nested_seen_type_ids: HashSet<TypeId> = HashSet::new();
            for type_id_i in gens.iter().chain(params.iter()) {
                let mut seen_type_ids_snapshot = seen_type_ids.clone();
                self.solve_priv(ast_ctx, *type_id_i, &mut seen_type_ids_snapshot)?;
                new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));
            }

            if let Some(ret_type_id) = ret_type_id_opt {
                let mut seen_type_ids_snapshot = seen_type_ids.clone();
                self.solve_priv(ast_ctx, ret_type_id, &mut seen_type_ids_snapshot)?;
                new_nested_seen_type_ids.extend(seen_type_ids_snapshot.difference(seen_type_ids));
            }
            seen_type_ids.extend(new_nested_seen_type_ids);
        } else {
            unreachable!();
        }

        // TODO: Does this need to be returned?
        Ok(type_id)
    }

    fn solve_unknown_adt_member(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty_clone = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_unknown_adt_member -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
            type_id, seen_type_ids, ty_clone
        );

        let (adt_type_id, member_name) =
            if let Ty::UnknownAdtMember(adt_type_id, member_name, ..) = &mut ty_clone {
                (adt_type_id, member_name)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) =
            match self.solve_adt_type(ast_ctx, *adt_type_id, seen_type_ids)? {
                AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),
                AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
                    *adt_type_id = gen_adt_type_id;
                    let new_type_id = self.ty_env.id(&ty_clone)?;
                    self.insert_constraint(type_id, new_type_id)?;
                    return Ok(new_type_id);
                }
                AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => {
                    return Ok(type_id);
                }
            };

        let mut new_type_id = ast_ctx
            .get_adt_member(
                self,
                &adt_path.without_gens(),
                &member_name,
                self.ty_env.file_pos(*adt_type_id).cloned(),
            )?
            .borrow()
            .ty
            .clone()
            .unwrap();

        // TODO: Is this needed? Does this do anything atm?
        // Replace potential generics with impls from the solved ADT type.
        if let Some(new_new_type_id) = self.ty_env.replace_gen_impls(new_type_id, &adt_gens)? {
            new_type_id = new_new_type_id;
        }

        // Start from max of u64 to prevent clashing with "normal" unique IDs.
        let unique_id = u64::MAX - type_id.0;
        if let Some(new_new_type_id) = self
            .ty_env
            .replace_gens_with_gen_instances(new_type_id, unique_id)?
        {
            self.insert_constraint(new_type_id, new_new_type_id)?;
            new_type_id = new_new_type_id;
        }

        self.solve_priv(ast_ctx, new_type_id, seen_type_ids)?;
        let inf_new_type_id = self.inferred_type(new_type_id)?;

        self.insert_constraint(type_id, inf_new_type_id)?;
        Ok(inf_new_type_id)
    }

    fn solve_unknown_adt_method(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty_clone = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_unknown_adt_method -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
            type_id, seen_type_ids, ty_clone
        );

        let (adt_type_id, method_name, fn_call_gens_vec, type_info) =
            if let Ty::UnknownAdtMethod(ty, method_name, fn_call_gens_vec, _, type_info) =
                &mut ty_clone
            {
                (ty, method_name, fn_call_gens_vec, type_info)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) =
            match self.solve_adt_type(ast_ctx, *adt_type_id, seen_type_ids)? {
                AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),
                AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
                    *adt_type_id = gen_adt_type_id;
                    let new_type_id = self.ty_env.id(&ty_clone)?;
                    self.insert_constraint(type_id, new_type_id)?;
                    return Ok(new_type_id);
                }
                AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
            };

        let method = ast_ctx.get_method(self, &adt_path.without_gens(), &method_name)?;
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
                fn_call_gens.insert(gen_name.clone(), *gen_unknown_ty);
            }
            fn_call_gens
        };

        let inf_new_type_id = if let Some(mut new_type_id) = method.ret_type {
            if let Some(new_new_type_id) =
                self.ty_env.replace_gen_impls(new_type_id, &fn_call_gens)?
            {
                new_type_id = new_new_type_id;
            }
            if let Some(new_new_type_id) = self.ty_env.replace_gen_impls(new_type_id, &adt_gens)? {
                new_type_id = new_new_type_id;
            }

            // Start from max of u64 to prevent clashing with "normal" unique IDs.
            let unique_id = u64::MAX - type_id.0;
            if let Some(new_new_type_id) = self
                .ty_env
                .replace_gens_with_gen_instances(new_type_id, unique_id)?
            {
                self.insert_constraint(new_type_id, new_new_type_id)?;
                new_type_id = new_new_type_id;
            }

            self.solve_priv(ast_ctx, new_type_id, seen_type_ids)?;
            self.inferred_type(new_type_id)?
        } else {
            // The return type of the method is None == Void.
            self.ty_env.id(&Ty::CompoundType(
                InnerTy::Void,
                Generics::empty(),
                TypeInfo::None,
            ))?
        };

        self.insert_constraint(type_id, inf_new_type_id)?;
        Ok(inf_new_type_id)
    }

    fn solve_unknown_method_argument(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty_clone = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_unknown_method_argument -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:#?}",
            type_id, seen_type_ids, ty_clone
        );

        let (adt_type_id, method_name, fn_call_gens_vec, name_or_idx, type_info) =
            if let Ty::UnknownMethodArgument(
                type_id,
                method_name,
                fn_call_gens_vec,
                name_or_idx,
                _,
                type_info,
            ) = &mut ty_clone
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

        let (adt_path, adt_gens) =
            match self.solve_adt_type(ast_ctx, *adt_type_id, seen_type_ids)? {
                AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),
                AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
                    *adt_type_id = gen_adt_type_id;
                    let new_type_id = self.ty_env.id(&ty_clone)?;
                    self.insert_constraint(type_id, new_type_id)?;
                    return Ok(new_type_id);
                }
                AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
            };

        let actual_idx = match name_or_idx {
            Either::Left(arg_name) => ast_ctx.get_method_param_idx(
                self,
                &adt_path.without_gens(),
                &method_name,
                &arg_name,
            )?,
            Either::Right(idx) => *idx,
        };

        let mut new_type_id = ast_ctx.get_method_param_type(
            self,
            &adt_path.without_gens(),
            &method_name,
            actual_idx,
        )?;

        let method = ast_ctx.get_method(self, &adt_path.without_gens(), &method_name)?;
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
                fn_call_gens.insert(gen_name.clone(), *gen_unknown_ty);
            }
            fn_call_gens
        };

        if let Some(new_new_type_id) = self.ty_env.replace_gen_impls(new_type_id, &fn_call_gens)? {
            new_type_id = new_new_type_id;
        }
        if let Some(new_new_type_id) = self.ty_env.replace_gen_impls(new_type_id, &adt_gens)? {
            new_type_id = new_new_type_id;
        }

        // Start from max of u64 to prevent clashing with "normal" unique IDs.
        let unique_id = u64::MAX - type_id.0;
        if let Some(new_new_type_id) = self
            .ty_env
            .replace_gens_with_gen_instances(new_type_id, unique_id)?
        {
            self.insert_constraint(new_type_id, new_new_type_id)?;
            new_type_id = new_new_type_id;
        }

        self.solve_priv(ast_ctx, new_type_id, seen_type_ids)?;
        let inf_new_type_id = self.inferred_type(new_type_id)?;

        self.insert_constraint(type_id, inf_new_type_id)?;
        Ok(inf_new_type_id)
    }

    fn solve_unknown_method_generic(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let mut ty_clone = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_unknown_method_generic -- type_id: {}, seen_type_ids: {:?}, ty_clone: {:?}",
            type_id, seen_type_ids, ty_clone
        );

        let (adt_type_id, method_name, gen_idx_or_name, type_info) =
            if let Ty::UnknownMethodGeneric(
                adt_type_id,
                method_name,
                gen_idx_or_name,
                _,
                type_info,
            ) = &mut ty_clone
            {
                (adt_type_id, method_name, gen_idx_or_name, type_info)
            } else {
                unreachable!()
            };

        let (adt_path, adt_gens) =
            match self.solve_adt_type(ast_ctx, *adt_type_id, seen_type_ids)? {
                AdtSolveStatus::Solved(adt_path, adt_gens) => (adt_path, adt_gens),
                AdtSolveStatus::GenericInstance(gen_adt_type_id) => {
                    *adt_type_id = gen_adt_type_id;
                    let new_type_id = self.ty_env.id(&ty_clone)?;
                    self.insert_constraint(type_id, new_type_id)?;
                    return Ok(new_type_id);
                }
                AdtSolveStatus::Progress(_) | AdtSolveStatus::NoProgress => return Ok(type_id),
            };

        let method = ast_ctx.get_method(self, &adt_path.without_gens(), &method_name)?;
        let method = method.borrow();

        let gen_name = match gen_idx_or_name {
            Either::Left(idx) => {
                if let Some(generic_name) = method
                    .generics
                    .as_ref()
                    .map(|gens| gens.get_name(*idx))
                    .flatten()
                {
                    generic_name
                } else {
                    return Err(ast_ctx.err(format!(
                        "Method call specified generic at index {}. \
                        Method declaration for \"{}\" has no generic at that index.",
                        gen_idx_or_name, method_name
                    )));
                }
            }
            Either::Right(gen_name) => gen_name.clone(),
        };

        let unique_id = self.ty_env.new_unique_id();
        let mut new_type_id =
            self.ty_env
                .id(&Ty::GenericInstance(gen_name, unique_id, type_info.clone()))?;

        if let Some(new_new_type_id) = self.ty_env.replace_gen_impls(new_type_id, &adt_gens)? {
            new_type_id = new_new_type_id;
        }
        if let Some(method_generics) = &self.new_method_generics(&method, &type_info)? {
            if let Some(new_new_type_id) = self
                .ty_env
                .replace_gen_impls(new_type_id, &method_generics)?
            {
                new_type_id = new_new_type_id;
            }
        }

        self.solve_priv(ast_ctx, new_type_id, seen_type_ids)?;
        let inf_new_type_id = self.inferred_type(new_type_id)?;

        self.insert_constraint(type_id, inf_new_type_id)?;
        Ok(inf_new_type_id)
    }

    fn solve_unknown_array_member(
        &mut self,
        ast_ctx: &AstCtx,
        type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<TypeId> {
        let ty = self.ty_env.ty(type_id)?.clone();

        debug!(
            "solve_unknown_array_member -- type_id: {}, seen_type_ids: {:?}, ty: {:#?}",
            type_id, seen_type_ids, ty
        );

        let arr_type_id = if let Ty::UnknownArrayMember(arr_type_id, ..) = ty {
            arr_type_id
        } else {
            unreachable!()
        };

        self.solve_priv(ast_ctx, arr_type_id, seen_type_ids)?;
        let new_arr_type_id = self.inferred_type(arr_type_id)?;

        if arr_type_id == new_arr_type_id {
            // No progress made.
            return Ok(type_id);
        }

        self.insert_constraint(arr_type_id, new_arr_type_id)?;

        let new_arr_ty = self.ty_env.ty(new_arr_type_id)?.clone();
        if let Ty::Array(new_member_type_id, ..) = new_arr_ty {
            self.solve_priv(ast_ctx, new_member_type_id, seen_type_ids)?;
            let inf_new_member_type_id = self.inferred_type(new_member_type_id)?;

            self.insert_constraint(inf_new_member_type_id, type_id)?;
            Ok(new_member_type_id)
        } else {
            Ok(type_id)
        }
    }

    /// Given a potential ADT type `adt_type_id`, tries to solve it.
    /// Returns a `AdtSolveStatus` enum indicating if any progress was made
    /// solving the ADT type.
    fn solve_adt_type(
        &mut self,
        ast_ctx: &AstCtx,
        adt_type_id: TypeId,
        seen_type_ids: &mut HashSet<TypeId>,
    ) -> LangResult<AdtSolveStatus> {
        debug!(
            "solve_adt_type -- adt_type_id: {}, seen_type_ids: {:?}",
            adt_type_id, seen_type_ids,
        );

        self.solve_priv(ast_ctx, adt_type_id, seen_type_ids)?;
        let inf_adt_type_id = self.inferred_type(adt_type_id)?;

        let is_generic = self.ty_env.is_generic(inf_adt_type_id)?;
        let check_inf = true;
        let solve_cond = SolveCond::new().excl_unknown();

        if is_generic && adt_type_id != inf_adt_type_id {
            Ok(AdtSolveStatus::GenericInstance(inf_adt_type_id))
        } else if is_generic {
            Ok(AdtSolveStatus::NoProgress)
        } else if self
            .ty_env
            .is_solved(&self.sub_sets, inf_adt_type_id, check_inf, solve_cond)?
        {
            if adt_type_id != inf_adt_type_id {
                self.set_generic_names(ast_ctx, inf_adt_type_id)?;
            }

            let inf_adt_ty = self.ty_env.ty(inf_adt_type_id)?.clone();
            let (inner_ty, adt_generics) = match inf_adt_ty {
                Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

                // TODO: Fix this edge case. This might be a pointer to ADT to
                //       represent a "{this}" function. Remove the need for this
                //       logic in the future.
                Ty::Pointer(type_id_i, type_info) => {
                    self.solve_priv(ast_ctx, type_id_i, seen_type_ids)?;
                    let inf_type_id_i = self.inferred_type(type_id_i)?;

                    let is_generic = self.ty_env.is_generic(inf_type_id_i)?;
                    if is_generic && adt_type_id != inf_adt_type_id {
                        let new_type_id = self.ty_env.id(&Ty::Pointer(inf_type_id_i, type_info))?;
                        return Ok(AdtSolveStatus::GenericInstance(new_type_id));
                    } else if is_generic {
                        return Ok(AdtSolveStatus::NoProgress);
                    }

                    let inf_ty_i = self.ty_env.ty(inf_type_id_i)?.clone();
                    if let Ty::CompoundType(inner_ty, generics, ..) = inf_ty_i {
                        (inner_ty, generics)
                    } else {
                        unreachable!(
                            "ADT type not pointer to compound, inf_adt_type_id: {}",
                            inf_adt_type_id
                        );
                    }
                }

                _ => unreachable!("ADT type not valid: {:#?}", inf_adt_ty),
            };

            if let Some(adt_path) = inner_ty.get_ident() {
                Ok(AdtSolveStatus::Solved(adt_path, adt_generics))
            } else {
                Err(ast_ctx.err(format!(
                    "ADT CompoundType isn't generic or identifier, is: {:?}\nADT type ID: {}",
                    inner_ty, adt_type_id
                )))
            }
        } else if adt_type_id == inf_adt_type_id {
            Ok(AdtSolveStatus::NoProgress)
        } else {
            Ok(AdtSolveStatus::Progress(inf_adt_type_id))
        }
    }

    /// Creates a new `Generic` where the types will be new `GenericInstance`s.
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
                let unique_id = self.ty_env.new_unique_id();
                let type_id = self.ty_env.id(&Ty::GenericInstance(
                    generic_name.clone(),
                    unique_id,
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
    pub fn set_generic_names(&mut self, ast_ctx: &AstCtx, type_id: TypeId) -> LangResult<()> {
        let ty = self.ty_env.ty(type_id)?.clone();

        let (inner_ty, generics) = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => (inner_ty, generics),

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                return self.set_generic_names(ast_ctx, type_id_i);
            }

            _ => return Ok(()),
        };

        if !generics.is_empty() && generics.len_names() == 0 && inner_ty.is_adt() {
            let path = inner_ty.get_ident().unwrap().without_gens();

            let adt = match ast_ctx.get_adt(self, &path) {
                Ok(adt) => adt,
                Err(err) => return Err(err),
            };
            let adt = adt.borrow();

            if let Some(adt_gens) = adt.generics.clone() {
                let ty = self.ty_env.ty_mut(type_id)?;

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
