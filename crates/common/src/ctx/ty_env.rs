use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap, HashSet},
};

use either::Either;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    path::{LangPath, LangPathPart},
    token::expr::Expr,
    ty::{
        generics::Generics, inner_ty::InnerTy, substitution_sets::SubstitutionSets, ty::Ty,
        type_info::TypeInfo,
    },
    TypeId,
};

/// A struct containing all types. All code outside of this environment will
/// only store references to types (TypeId). To access the actual type, they
/// will have to fetch it from this environment.
///
/// Handling all types in this environment allows for easy updating of types
/// during type inference. The same type can be used in multiple places in the
/// AST and they all have to be updated/synchronized when the type is inferred to
/// a new type. This environment handles that synchronization and the AST only
/// contains references to the type in this environment, ensuring that the type
/// only needs to be updated in one place and is always up-to-date.
#[derive(Debug)]
pub struct TyEnv {
    id_to_ty: HashMap<TypeId, Ty>,
    ty_to_id: HashMap<Ty, TypeId>,

    /// Contains a map of type IDs that should be "forwareded" to some other ID.
    /// This is use for example with GenericInstance's where multiple type IDs
    /// should map to the exact same instance of a type.
    /// The key is the type ID that should map to something else and the value
    /// is to which type it should map.
    forward_ty_to_ty: HashMap<TypeId, TypeId>,

    /// A counter that is used to create new `TypeId`s. It will be increment
    /// for every new type created. The type ID 0 is used for special purposes,
    /// so this value will always start counting from 1.
    type_id: u64,

    /// A counter used to make "generic" unique IDs. This will be used for
    /// unknown and generic unique IDs.
    unique_id: u64,
}

impl Default for TyEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Struct used to indicate what counts as solved.
///
/// Types that (potentially) can be considered solved:
///  * Primitives
///  * ADTs
///  * Traits
///  * Pointers containing solved types
///  * Arrays containing solved types
///  * Unknown ints (default)
///  * Unknown floats (default)
///  * Any
///  * Generic/GenericInstance
///  * Unknown ADT/method related types
///
/// The list above is all types that possible can be considered solved.
/// A subset of this list can be used instead depending on the bools set in a
/// given `SolveCond`. Any bool values set to false will be exluded from being
/// counted as solvable, meaning that the specific type is always considered
/// unsolved.
#[derive(Debug, Clone, Copy)]
pub struct SolveCond {
    default: bool,
    generic: bool,
    generic_instance: bool,
    unknown: bool,
}

impl Default for SolveCond {
    fn default() -> Self {
        Self {
            default: true,
            generic: true,
            generic_instance: true,
            unknown: true,
        }
    }
}

impl SolveCond {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn excl_default(mut self) -> Self {
        self.default = false;
        self
    }

    pub fn excl_gen(mut self) -> Self {
        self.generic = false;
        self
    }

    pub fn excl_gen_inst(mut self) -> Self {
        self.generic_instance = false;
        self
    }

    pub fn excl_unknown(mut self) -> Self {
        self.unknown = false;
        self
    }

    pub fn can_solve_default(self) -> bool {
        self.default
    }

    pub fn can_solve_gen(self) -> bool {
        self.generic
    }

    pub fn can_solve_gen_inst(self) -> bool {
        self.generic_instance
    }

    pub fn can_solve_unknown(self) -> bool {
        self.unknown
    }
}

impl TyEnv {
    pub fn new() -> Self {
        Self {
            id_to_ty: HashMap::default(),
            ty_to_id: HashMap::default(),
            forward_ty_to_ty: HashMap::default(),
            type_id: 1,
            unique_id: 1,
        }
    }

    /// Inserts a new type `ty` into this environment and return a new TypeId
    /// that refers to that type. If the type already exists in this environment,
    /// a error will be returned.
    pub fn new_ty(&mut self, ty: Ty) -> LangResult<TypeId> {
        if !self.ty_to_id.contains_key(&ty) {
            let id = TypeId(self.type_id);
            self.type_id += 1;

            debug!("new_ty -- type_id: {}, ty: {:#?}", id, ty);

            if self.type_id == 56 + 1 {
                //panic!("new_ty -- type_id: {}, ty: {:#?}", id, ty);
            }

            self.ty_to_id.insert(ty.clone(), id);
            self.id_to_ty.insert(id, ty);

            Ok(id)
        } else {
            Err(LangError::new(
                format!(
                    "Tried to create new type that already exists in environment: {:#?}",
                    ty
                ),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn current_type_id(&self) -> u64 {
        self.type_id
    }

    pub fn new_unique_id(&mut self) -> u64 {
        let unique_id = self.unique_id;
        self.unique_id += 1;
        unique_id
    }

    pub fn all_types(&self) -> HashSet<TypeId> {
        self.id_to_ty.keys().cloned().collect::<HashSet<_>>()
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If this environment already have "seen" the type `ty`, the reference to
    /// that type is returned. If `ty` is a new type that haven't been "seen"
    /// before, a new TypeId will be assigned to it and returned.
    pub fn id(&mut self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(type_id) = self.ty_to_id.get(ty) {
            Ok(*type_id)
        } else {
            self.new_ty(ty.clone())
        }
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If `ty` is a type that doesn't exists in this type environment, a error
    /// is returned.
    pub fn id_try(&self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(type_id) = self.ty_to_id.get(ty) {
            Ok(*type_id)
        } else {
            Err(LangError::new(
                format!("Unable to find type in environment: {:#?}", ty),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Given a type ID `type_id`, gets the type that the `type_id` refers to.
    /// If `type_id` doesn't refer to a type, a error will be returned.
    pub fn ty(&self, type_id: TypeId) -> LangResult<&Ty> {
        let type_id = self.get_forwarded(type_id);
        if let Some(ty) = self.id_to_ty.get(&type_id) {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to find type with type ID {}.", type_id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Given a type ID `type_id`, gets the type that the `type_id` refers to as
    /// mutable. If `type_id` doesn't refer to a type, a error will be returned.
    pub fn ty_mut(&mut self, type_id: TypeId) -> LangResult<&mut Ty> {
        let type_id = self.get_forwarded(type_id);
        if let Some(ty) = self.id_to_ty.get_mut(&type_id) {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to find type with type ID {}.", type_id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Updates the the type with type ID `type_id` to the type `ty` and returns
    /// the old type. If no type with ID `type_id` exists in the environment, a
    /// error is returned.
    pub fn update(&mut self, type_id: TypeId, ty: Ty) -> LangResult<Ty> {
        match self.id_to_ty.entry(type_id) {
            Entry::Occupied(mut o) => Ok(o.insert(ty)),
            Entry::Vacant(_) => Err(LangError::new(
                format!(
                    "Unable to find type with type ID {} when updating to type: {:#?}.",
                    type_id, ty
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    /// Removes the type with ID `type_id` from this type environment. This is
    /// NOT safe to do if there are references to this type ID in the AST since
    /// they will then reference a non-existing type.
    pub fn remove(&mut self, type_id: TypeId) {
        if let Some(ty) = self.id_to_ty.remove(&type_id) {
            self.ty_to_id.remove(&ty);
        }

        // Nice to be able to reuse the type ID if possible (if no new types have
        // been created between the `type_id` was created and then removed).
        if self.type_id > 0 && self.type_id - 1 == type_id.0 {
            self.type_id -= 1;
        }
    }

    /// Forwards the the type with type ID `from_type_id` to the type with ID
    /// `to_type_id`. If no type with either ID exists in the environment, a error
    /// is returned.
    ///
    /// From this point on, any use of the `from_type_id` will internally use
    /// the type `to_type_id`.
    pub fn forward(&mut self, from_type_id: TypeId, to_type_id: TypeId) -> LangResult<()> {
        if from_type_id == to_type_id {
            return Ok(());
        }

        // Ensures that the types actual exists in this type environment.
        self.ty(from_type_id)?;
        self.ty(to_type_id)?;

        // Need to consider that the `to_type_id` has a forward itself. In that
        // case, that forward type should be used instead.
        let fwd_to_type_id = self.get_forwarded(to_type_id);
        self.forward_ty_to_ty.insert(from_type_id, fwd_to_type_id);

        if let Some(from_ty) = self.id_to_ty.remove(&from_type_id) {
            self.ty_to_id.insert(from_ty, fwd_to_type_id);
        }

        Ok(())
    }

    pub fn get_forwarded(&self, type_id: TypeId) -> TypeId {
        if let Some(forwarded_type_id) = self.forward_ty_to_ty.get(&type_id) {
            *forwarded_type_id
        } else {
            type_id
        }
    }

    pub fn get_forwards(&self) -> HashMap<TypeId, TypeId> {
        self.forward_ty_to_ty.clone()
    }

    /// Checks if the type referenced by the type ID `id` only contains solved types.
    ///
    /// What counts as solved depends on the given `solve_cond` enum, see the
    /// description of the `SolveCond` enum for more information about what/how
    /// types are solvable and the different variants.
    ///
    /// It first checks if the given `type_id` is solved. If that is not the case
    /// and `check_inf` is set to true, it checks its inferred type for solvability
    /// as well (unless `type_id` is the preferred type in its set).
    /// The `check_inf` flag is only used for the first call to this function.
    /// Any recursive calls inside tvhis funmction will set it to true.
    ///
    /// Any "Unknown..." types that have their ADTs/arrays will be considered
    /// solvable if `incl_unknowns` is set to true. This is needed since they
    /// might not be possible to solve until all generics have been resolved
    /// which is done at a later stage than the "normal" solving of the types.
    pub fn is_solved(
        &self,
        sub_sets: &SubstitutionSets,
        type_id: TypeId,
        check_inf: bool,
        solve_cond: SolveCond,
    ) -> LangResult<bool> {
        debug!(
            "is_solved -- type_id: {}, check_inf: {}, solve_cond: {:?}",
            type_id, check_inf, solve_cond
        );

        let is_solved = match self.ty(type_id)? {
            Ty::CompoundType(inner_ty, generics, ..) => {
                let inner_solved = inner_ty.is_solved(solve_cond);
                let mut gens_solved = true;
                for type_id in generics.iter_types() {
                    if !self.is_solved(sub_sets, *type_id, check_inf, solve_cond)? {
                        gens_solved = false;
                    }
                }
                inner_solved && gens_solved
            }

            Ty::Pointer(type_id_i, ..) => self.is_solved(sub_sets, *type_id_i, true, solve_cond)?,

            Ty::Array(type_id_i, expr_opt, ..) => {
                let ty_solved = self.is_solved(sub_sets, *type_id_i, check_inf, solve_cond)?;
                let expr_ty_solved = if let Some(expr_type_id) = expr_opt
                    .as_ref()
                    .map(|expr| expr.get_expr_type().ok())
                    .flatten()
                {
                    self.is_solved(sub_sets, expr_type_id, true, solve_cond)?
                } else {
                    true
                };
                ty_solved && expr_ty_solved
            }

            Ty::Fn(gens, params, ret_ty_id, ..) => {
                let ty_solved = if let Some(ret_ty_id) = ret_ty_id {
                    self.is_solved(sub_sets, *ret_ty_id, true, solve_cond)?
                } else {
                    true
                };
                let mut gens_solved = true;
                for gen_type_id in gens {
                    if !self.is_solved(sub_sets, *gen_type_id, true, solve_cond)? {
                        gens_solved = false;
                    }
                }
                let mut params_solved = true;
                for param_type_id in params {
                    if !self.is_solved(sub_sets, *param_type_id, true, solve_cond)? {
                        params_solved = false;
                    }
                }
                ty_solved && gens_solved && params_solved
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.is_solved(sub_sets, type_id_i, true, solve_cond)?
                } else {
                    true
                }
            }

            Ty::GenericInstance(..) => solve_cond.can_solve_gen_inst(),
            Ty::Generic(..) => solve_cond.can_solve_gen(),
            Ty::Any(..) => true,

            Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..)
                if solve_cond.can_solve_unknown() =>
            {
                self.is_solved(sub_sets, *type_id_i, true, solve_cond)?
            }

            Ty::UnknownAdtMethod(type_id_i, _, gens, ..)
            | Ty::UnknownMethodArgument(type_id_i, _, gens, ..)
                if solve_cond.can_solve_unknown() =>
            {
                let mut is_solved = self.is_solved(sub_sets, *type_id_i, true, solve_cond)?;
                for gen_type_id in gens {
                    if !self.is_solved(sub_sets, *gen_type_id, true, solve_cond)? {
                        is_solved = false;
                    }
                }
                is_solved
            }

            Ty::UnknownAdtMember(..)
            | Ty::UnknownMethodGeneric(..)
            | Ty::UnknownArrayMember(..)
            | Ty::UnknownAdtMethod(..)
            | Ty::UnknownMethodArgument(..) => false,
        };

        let inf_type_id = sub_sets.inferred_type(type_id)?;
        if !check_inf || is_solved || type_id == inf_type_id {
            Ok(is_solved)
        } else {
            self.is_solved(sub_sets, inf_type_id, true, solve_cond)
        }
    }

    pub fn get_inner(&self, id: TypeId) -> LangResult<&InnerTy> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty(id)? {
            Ok(inner_ty)
        } else {
            Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn get_inner_mut(&mut self, id: TypeId) -> LangResult<&mut InnerTy> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty_mut(id)? {
            Ok(inner_ty)
        } else {
            Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn get_generic_ident(&self, id: TypeId) -> LangResult<&str> {
        match self.ty(id)? {
            Ty::Generic(ident, ..) | Ty::GenericInstance(ident, ..) => Ok(ident),
            _ => Err(LangError::new(
                format!(
                    "Type with ID {} not a Generic or GenericInstance: {:#?}",
                    id,
                    self.ty(id)?
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    /// Returns the identifier if this type represents a ADT.
    /// If this type isn't a ADT, None is returned.
    pub fn get_ident(&self, id: TypeId) -> LangResult<Option<LangPath>> {
        match self.ty(id)? {
            Ty::CompoundType(inner_ty, ..) => Ok(inner_ty.get_ident()),
            _ => Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    pub fn type_info(&self, id: TypeId) -> Option<&TypeInfo> {
        Some(self.ty(id).ok()?.type_info())
    }

    pub fn type_info_mut(&mut self, id: TypeId) -> Option<&mut TypeInfo> {
        Some(self.ty_mut(id).ok()?.type_info_mut())
    }

    pub fn file_pos(&self, id: TypeId) -> Option<&FilePosition> {
        self.ty(id).ok()?.file_pos()
    }

    pub fn file_pos_mut(&mut self, id: TypeId) -> Option<&mut FilePosition> {
        self.ty_mut(id).ok()?.file_pos_mut()
    }

    // TODO: This only fetched a expression if it is the outer most type.
    //       How should expression in ex. generics be handled? Should this
    //       return a iterator or a list?
    // TODO: Rewrite this is a safe way. Temporary hack to get something to work.
    pub fn get_exprs_mut(&mut self, id: TypeId) -> LangResult<Vec<&mut Expr>> {
        let mut exprs = Vec::default();

        match self.ty(id)?.clone() {
            Ty::Expr(..) => {
                exprs.push(self.get_exprs_mut_priv(id)?);
            }

            Ty::Array(type_id, dim_expr_opt, ..) => {
                let inner_exprs = self.get_exprs_mut(type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }

                if dim_expr_opt.is_some() {
                    exprs.push(self.get_exprs_mut_priv(id)?);
                }
            }

            Ty::CompoundType(_, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let inner_exprs = self.get_exprs_mut(*gen_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let inner_exprs = self.get_exprs_mut(type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let inner_exprs = self.get_exprs_mut(ret_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }

                for gen_type_id in gens.iter() {
                    let inner_exprs = self.get_exprs_mut(*gen_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }

                for param_type_id in params.iter() {
                    let inner_exprs = self.get_exprs_mut(*param_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(exprs)
    }

    /// This function is called when the type with ID `id` is known to contain
    /// a expression. This is true for: Ty::Expr & Ty::Array.
    fn get_exprs_mut_priv(&mut self, id: TypeId) -> LangResult<&mut Expr> {
        match self.ty_mut(id)? {
            Ty::Expr(expr, ..) | Ty::Array(_, Some(expr), ..) => unsafe {
                Ok((expr.as_mut() as *mut Expr).as_mut().unwrap())
            },
            _ => Err(LangError::new(
                format!(
                    "Unable to get mutable exprs from type: {:#?}",
                    self.ty_mut(id)?
                ),
                LangErrorKind::AnalyzeError,
                None,
            )),
        }
    }

    /// Recursively replaces any generic identifiers from "UnknownIdent" wrapped
    /// inside a "CompoundType" into "Generic"s.
    pub fn replace_gens(&mut self, id: TypeId, generics: &Generics) -> LangResult<()> {
        match self.ty(id)?.clone() {
            Ty::CompoundType(InnerTy::UnknownIdent(path, ..), gens, type_info) => {
                for gen_type_id in gens.iter_types() {
                    self.replace_gens(*gen_type_id, generics)?;
                }

                if path.count() == 1 {
                    let possible_gen_name = path.first().unwrap().name();
                    for gen_name in generics.iter_names() {
                        if gen_name == possible_gen_name {
                            let new_ty = Ty::Generic(
                                possible_gen_name.into(),
                                self.new_unique_id(),
                                type_info.clone(),
                            );
                            self.update(id, new_ty)?;
                        }
                    }
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.replace_gens(type_id, generics)?;
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.replace_gens(ret_type_id, generics)?;
                }
                for gen_type_id in gens.iter() {
                    self.replace_gens(*gen_type_id, generics)?;
                }
                for param_type_id in params.iter() {
                    self.replace_gens(*param_type_id, generics)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.replace_gens(type_id, generics)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    /// Recursively replaces any "Generic" types with the actual implementation
    /// type. I.e. any "Generic" type that has a ident that is a key in the
    /// `generics_impl` will be replaced with the value in the map.
    ///
    /// The returned value indicates if the given type stored in `type_id` have
    /// been updated. If that is the case, a new type ID representing the new
    /// type will be returned. If the type haven't been updated/modified, None
    /// is returned.
    ///
    /// This function will recursively create new type IDs for all the types that
    /// contains nested generics. This is needed to not modify the already existing
    /// types in the type environment when replace the generics.
    pub fn replace_gen_impls(
        &mut self,
        type_id: TypeId,
        generics_impl: &Generics,
    ) -> LangResult<Option<TypeId>> {
        let mut ty_clone = self.ty(type_id)?.clone();

        debug!(
            "replace_gen_impls -- type_id: {}, ty: {:#?} generics_impl: {:#?}",
            type_id, ty_clone, generics_impl
        );

        let ty_was_updated = match &mut ty_clone {
            Ty::Generic(ident, ..) => {
                if let Some(impl_type_id) = generics_impl.get(ident) {
                    ty_clone = self.ty(impl_type_id)?.clone();
                    true
                } else {
                    false
                }
            }

            Ty::GenericInstance(ident, unique_id, ..) => {
                if let Some(impl_type_id) = generics_impl.get(ident) {
                    match self.ty(impl_type_id)? {
                        Ty::Generic(..) => false,
                        Ty::GenericInstance(_, impl_unique_id, ..)
                            if impl_unique_id == unique_id =>
                        {
                            false
                        }
                        _ => {
                            ty_clone = self.ty(impl_type_id)?.clone();
                            true
                        }
                    }
                } else {
                    false
                }
            }

            // Need to update generics both in the type `gens_clone` and the
            // generics declared inside a potential LangPath of the InnerTy.
            Ty::CompoundType(inner_ty, gens_clone, ..) => {
                let mut was_updated = false;

                for gen_type_id in gens_clone.iter_types_mut() {
                    if let Some(new_gen_type_id) =
                        self.replace_gen_impls(*gen_type_id, generics_impl)?
                    {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }

                if let Some(path) = inner_ty.get_ident_mut() {
                    if let Some(inner_gens_clune) =
                        path.last_mut().map(|part| part.1.as_mut()).flatten()
                    {
                        for gen_type_id in inner_gens_clune.iter_types_mut() {
                            if let Some(new_gen_type_id) =
                                self.replace_gen_impls(*gen_type_id, generics_impl)?
                            {
                                *gen_type_id = new_gen_type_id;
                                was_updated = true;
                            }
                        }
                    }
                }

                was_updated
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                if let Some(new_type_id_i) = self.replace_gen_impls(*type_id_i, generics_impl)? {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                let mut was_updated = false;
                if let Some(ret_type_id) = ret_type_id_opt {
                    if let Some(new_ret_type_id) =
                        self.replace_gen_impls(*ret_type_id, generics_impl)?
                    {
                        *ret_type_id = new_ret_type_id;
                        was_updated = true;
                    }
                }
                for gen_type_id in gens.iter_mut() {
                    if let Some(new_gen_type_id) =
                        self.replace_gen_impls(*gen_type_id, generics_impl)?
                    {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }
                for param_type_id in params.iter_mut() {
                    if let Some(new_param_type_id) =
                        self.replace_gen_impls(*param_type_id, generics_impl)?
                    {
                        *param_type_id = new_param_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type_mut() {
                    if let Some(new_type_id_i) =
                        self.replace_gen_impls(*type_id_i, generics_impl)?
                    {
                        *type_id_i = new_type_id_i;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            Ty::Any(..) => false,
        };

        // If any nested type was updated, the `ty_clone` will have been updated
        // and represents the new type for the current `type_id`. Insert it into
        // the type environment (if it is a new unique type) and return the new
        // type ID to indicate to the caller that the type have changed.
        Ok(if ty_was_updated {
            Some(self.id(&ty_clone)?)
        } else {
            None
        })
    }

    /// Recursively replaces any structure types with idents that matches the
    /// old structure name. These will be replaced with the new type with the
    /// generics "replaced"/"implemented".
    ///
    /// The returned value indicates if the given type stored in `type_id` have
    /// been updated. If that is the case, a new type ID representing the new
    /// type will be returned. If the type haven't been updated/modified, None
    /// is returned.
    ///
    /// This function will recursively create new type IDs for all the types that
    /// contains nested generics. This is needed to not modify the already existing
    /// types in the type environment when replace the generics.
    pub fn replace_self(
        &mut self,
        type_id: TypeId,
        old_path: &LangPath,
        new_self_type_id: TypeId,
    ) -> LangResult<Option<TypeId>> {
        debug!(
            "replace_self -- type_id: {}, old_path: {:?}, new_self_type_id: {}",
            type_id, old_path, new_self_type_id
        );

        let mut ty_clone = self.ty(type_id)?.clone();

        let ty_was_updated = match &mut ty_clone {
            Ty::CompoundType(inner_ty, ..) => match inner_ty {
                InnerTy::Struct(path)
                | InnerTy::Enum(path)
                | InnerTy::Union(path)
                | InnerTy::Trait(path)
                | InnerTy::UnknownIdent(path, ..) => {
                    if path == old_path {
                        ty_clone = self.ty(new_self_type_id)?.clone();
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            },

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                if let Some(new_type_id) =
                    self.replace_self(*type_id_i, old_path, new_self_type_id)?
                {
                    *type_id_i = new_type_id;
                    true
                } else {
                    false
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                let mut was_updated = false;
                if let Some(ret_type_id) = ret_type_id_opt {
                    if let Some(new_type_id) =
                        self.replace_self(*ret_type_id, old_path, new_self_type_id)?
                    {
                        *ret_type_id = new_type_id;
                        was_updated = true;
                    }
                }
                for gen_type_id in gens.iter_mut() {
                    if let Some(new_type_id) =
                        self.replace_self(*gen_type_id, old_path, new_self_type_id)?
                    {
                        *gen_type_id = new_type_id;
                        was_updated = true;
                    }
                }
                for param_type_id in params.iter_mut() {
                    if let Some(new_type_id) =
                        self.replace_self(*param_type_id, old_path, new_self_type_id)?
                    {
                        *param_type_id = new_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type_mut() {
                    if let Some(new_type_id) =
                        self.replace_self(*type_id_i, old_path, new_self_type_id)?
                    {
                        *type_id_i = new_type_id;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            _ => false,
        };

        // If any nested type was updated, the `ty_clone` will have been updated
        // and represents the new type for the current `type_id`. Insert it into
        // the type environment (if it is a new unique type) and return the new
        // type ID to indicate to the caller that the type have changed.
        Ok(if ty_was_updated {
            Some(self.id(&ty_clone)?)
        } else {
            None
        })
    }

    /// Recursively replaces any "Generic" types with newly created "GenericInstance"s.
    ///
    /// This is done to prevent any "Generic"s to leak out of function parameters
    /// or return values. The "Generic" will be shared while the "GenericInstance"s
    /// created will be unique.
    ///
    /// The returned value indicates if the given type stored in `type_id` have
    /// been updated. If that is the case, a new type ID representing the new
    /// type will be returned. If the type haven't been updated/modified, None
    /// is returned.
    ///
    /// The given `unique_id` is the value that will be assigned to the newly
    /// created "GenericInstance"s to make them "unique". To ensure that
    /// duplicates aren't created every time a sepcific type is solved, the
    /// unique ID will be given as a argument so that the uniqueness can be
    /// decided by the caller instead of a new generic instance being created for
    /// every call to this function.
    pub fn replace_gens_with_gen_instances(
        &mut self,
        type_id: TypeId,
        unique_id: u64,
    ) -> LangResult<Option<TypeId>> {
        debug!(
            "replace_gens_with_gen_instances -- type_id: {}, unique_id: {}",
            type_id, unique_id
        );

        let mut ty_clone = self.ty(type_id)?.clone();

        let ty_was_updated = match &mut ty_clone {
            Ty::Generic(ident, ..) => {
                let new_gen_inst = self.id(&Ty::GenericInstance(
                    ident.clone(),
                    unique_id,
                    ty_clone.type_info().clone(),
                ))?;

                ty_clone = self.ty(new_gen_inst)?.clone();
                true
            }

            Ty::CompoundType(_, gens_clone, ..) => {
                let mut was_updated = false;
                for gen_type_id in gens_clone.iter_types_mut() {
                    if let Some(new_gen_type_id) =
                        self.replace_gens_with_gen_instances(*gen_type_id, unique_id)?
                    {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                if let Some(new_type_id_i) =
                    self.replace_gens_with_gen_instances(*type_id_i, unique_id)?
                {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                let mut was_updated = false;
                if let Some(ret_type_id) = ret_type_id_opt {
                    if let Some(new_ret_type_id) =
                        self.replace_gens_with_gen_instances(*ret_type_id, unique_id)?
                    {
                        *ret_type_id = new_ret_type_id;
                        was_updated = true;
                    }
                }
                for gen_type_id in gens.iter_mut() {
                    if let Some(new_gen_type_id) =
                        self.replace_gens_with_gen_instances(*gen_type_id, unique_id)?
                    {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }
                for param_type_id in params.iter_mut() {
                    if let Some(new_param_type_id) =
                        self.replace_gens_with_gen_instances(*param_type_id, unique_id)?
                    {
                        *param_type_id = new_param_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type_mut() {
                    if let Some(new_type_id_i) =
                        self.replace_gens_with_gen_instances(*type_id_i, unique_id)?
                    {
                        *type_id_i = new_type_id_i;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }

            Ty::GenericInstance(..) | Ty::Any(..) => false,
        };

        // If any nested type was updated, the `ty_clone` will have been updated
        // and represents the new type for the current `type_id`. Insert it into
        // the type environment (if it is a new unique type) and return the new
        // type ID to indicate to the caller that the type have changed.
        Ok(if ty_was_updated {
            Some(self.id(&ty_clone)?)
        } else {
            None
        })
    }

    /// Recursively replaces any found unique IDs in the given type.
    ///
    /// This can be done when creating a "copy" of the type so that the two types
    /// doesn't have types mapping to eachother because they contain the same
    /// unique ID.
    pub fn replace_unique_ids(&mut self, type_id: TypeId) -> LangResult<Option<TypeId>> {
        debug!("replace_unique_ids -- type_id: {},", type_id);

        let mut ty_clone = self.ty(type_id)?.clone();

        let ty_was_updated = match &mut ty_clone {
            Ty::GenericInstance(_, unique_id, ..)
            | Ty::Any(unique_id, ..)
            | Ty::Generic(_, unique_id, ..) => {
                *unique_id = self.new_unique_id();
                true
            }

            Ty::UnknownAdtMember(type_id_i, _, unique_id, ..)
            | Ty::UnknownAdtMethod(type_id_i, _, _, unique_id, ..)
            | Ty::UnknownMethodArgument(type_id_i, _, _, _, unique_id, ..)
            | Ty::UnknownMethodGeneric(type_id_i, _, _, unique_id, ..)
            | Ty::UnknownArrayMember(type_id_i, unique_id, ..) => {
                if let Some(new_type_id_i) = self.replace_unique_ids(*type_id_i)? {
                    *type_id_i = new_type_id_i;
                }

                *unique_id = self.new_unique_id();
                true
            }

            Ty::CompoundType(_, gens_clone, ..) => {
                let mut was_updated = false;
                for gen_type_id in gens_clone.iter_types_mut() {
                    if let Some(new_gen_type_id) = self.replace_unique_ids(*gen_type_id)? {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                if let Some(new_type_id_i) = self.replace_unique_ids(*type_id_i)? {
                    *type_id_i = new_type_id_i;
                    true
                } else {
                    false
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                let mut was_updated = false;
                if let Some(ret_type_id) = ret_type_id_opt {
                    if let Some(new_ret_type_id) = self.replace_unique_ids(*ret_type_id)? {
                        *ret_type_id = new_ret_type_id;
                        was_updated = true;
                    }
                }
                for gen_type_id in gens.iter_mut() {
                    if let Some(new_gen_type_id) = self.replace_unique_ids(*gen_type_id)? {
                        *gen_type_id = new_gen_type_id;
                        was_updated = true;
                    }
                }
                for param_type_id in params.iter_mut() {
                    if let Some(new_param_type_id) = self.replace_unique_ids(*param_type_id)? {
                        *param_type_id = new_param_type_id;
                        was_updated = true;
                    }
                }
                was_updated
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type_mut() {
                    if let Some(new_type_id_i) = self.replace_unique_ids(*type_id_i)? {
                        *type_id_i = new_type_id_i;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        };

        // If any nested type was updated, the `ty_clone` will have been updated
        // and represents the new type for the current `type_id`. Insert it into
        // the type environment (if it is a new unique type) and return the new
        // type ID to indicate to the caller that the type have changed.
        Ok(if ty_was_updated {
            Some(self.id(&ty_clone)?)
        } else {
            None
        })
    }

    /// Gets a vector of all "Generic" types that is contained in the given
    /// type `self`.
    pub fn get_generics(&self, id: TypeId) -> LangResult<Vec<TypeId>> {
        let mut generics = Vec::default();

        match self.ty(id)? {
            Ty::Generic(..) => {
                generics.push(id);
            }

            Ty::CompoundType(_, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let mut inner_generics = self.get_generics(*gen_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let mut inner_generics = self.get_generics(*type_id)?;
                generics.append(&mut inner_generics);
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let mut inner_generics = self.get_generics(*ret_type_id)?;
                    generics.append(&mut inner_generics);
                }
                for gen_type_id in gens {
                    let mut inner_generics = self.get_generics(*gen_type_id)?;
                    generics.append(&mut inner_generics);
                }
                for param_type_id in params {
                    let mut inner_generics = self.get_generics(*param_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    let mut inner_generics = self.get_generics(expr_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            _ => (),
        }

        Ok(generics)
    }

    /// Gets a set of all unsolvable types that is contained in the given type `self`.
    /// This exludes Generic's, GenericInstance's and Any's.
    pub fn get_unsolvable(
        &self,
        type_id: TypeId,
        solve_cond: SolveCond,
    ) -> LangResult<HashSet<TypeId>> {
        let mut unsolvable = HashSet::default();

        match self.ty(type_id)? {
            Ty::CompoundType(inner_ty, gens, _) => {
                if !inner_ty.is_solved(SolveCond::new()) {
                    unsolvable.insert(type_id);
                }
                for gen_type_id in gens.iter_types() {
                    unsolvable.extend(self.get_unsolvable(*gen_type_id, solve_cond)?);
                }
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                unsolvable.extend(self.get_unsolvable(*type_id_i, solve_cond)?);
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    unsolvable.extend(self.get_unsolvable(*ret_type_id, solve_cond)?);
                }
                for gen_type_id in gens {
                    unsolvable.extend(self.get_unsolvable(*gen_type_id, solve_cond)?);
                }
                for param_type_id in params {
                    unsolvable.extend(self.get_unsolvable(*param_type_id, solve_cond)?);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    unsolvable.extend(self.get_unsolvable(type_id_i, solve_cond)?);
                }
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(unsolvable)
    }

    /// Gets a set of the paths of all ADTs/traits that is contained in the type.
    /// Set `full_paths` to true to fetch the names as full names, set to false
    /// to just return the names without generics.
    pub fn get_adt_and_trait_paths(
        &self,
        id: TypeId,
        full_paths: bool,
    ) -> LangResult<HashSet<LangPath>> {
        let mut paths = HashSet::default();

        match self.ty(id)? {
            Ty::CompoundType(inner_ty, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let inner_paths = self.get_adt_and_trait_paths(*gen_type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }

                let generics_opt = if gens.len_types() > 0 {
                    Some(gens.clone())
                } else {
                    None
                };

                match inner_ty {
                    InnerTy::Struct(path)
                    | InnerTy::Enum(path)
                    | InnerTy::Union(path)
                    | InnerTy::Trait(path)
                    | InnerTy::UnknownIdent(path, ..) => {
                        let mut path_clone = path.clone();
                        let last_part = path_clone.pop().unwrap();
                        if full_paths {
                            path_clone.push(LangPathPart(last_part.0, generics_opt));
                        } else {
                            path_clone.push(LangPathPart(last_part.0, None));
                        }

                        paths.insert(path_clone);
                    }
                    _ => (),
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let inner_paths = self.get_adt_and_trait_paths(*type_id, full_paths)?;
                paths.extend(inner_paths.into_iter());
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let inner_paths = self.get_adt_and_trait_paths(*ret_type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }
                for gen_type_id in gens {
                    let inner_paths = self.get_adt_and_trait_paths(*gen_type_id, full_paths)?;
                    paths.extend(inner_paths);
                }
                for param_type_id in params {
                    let inner_paths = self.get_adt_and_trait_paths(*param_type_id, full_paths)?;
                    paths.extend(inner_paths);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    let inner_paths = self.get_adt_and_trait_paths(type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }
            }

            _ => (),
        }

        Ok(paths)
    }

    pub fn is_int(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_int())
            .unwrap_or(false))
    }

    pub fn is_float(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_float())
            .unwrap_or(false))
    }

    pub fn is_bool(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_bool())
            .unwrap_or(false))
    }

    pub fn is_char(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_char())
            .unwrap_or(false))
    }

    pub fn is_string(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_string())
            .unwrap_or(false))
    }

    pub fn is_primitive(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_primitive())
            .unwrap_or(false))
    }

    pub fn is_unknown(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown())
            .unwrap_or(false))
    }

    pub fn is_unknown_ident(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_ident())
            .unwrap_or(false))
    }

    pub fn is_unknown_int(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_int())
            .unwrap_or(false))
    }

    pub fn is_unknown_float(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_float())
            .unwrap_or(false))
    }

    pub fn is_aggregate(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::CompoundType(..)))
    }

    pub fn is_pointer(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Pointer(..)))
    }

    pub fn is_array(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Array(..)))
    }

    pub fn is_fn(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Fn(..)))
    }

    pub fn is_expr(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Expr(..)))
    }

    pub fn is_any(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Any(..)))
    }

    pub fn is_generic(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Generic(..) | Ty::GenericInstance(..)))
    }

    pub fn is_unknown_adt_member(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownAdtMember(..)))
    }

    pub fn is_unknown_adt_method(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownAdtMethod(..)))
    }

    pub fn is_unknown_method_argument(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownMethodArgument(..)))
    }

    pub fn is_unknown_method_generic(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownMethodGeneric(..)))
    }

    pub fn is_unknown_array_member(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownArrayMember(..)))
    }

    pub fn is_unknown_any(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(match ty {
            Ty::UnknownAdtMember(..)
            | Ty::UnknownAdtMethod(..)
            | Ty::UnknownMethodArgument(..)
            | Ty::UnknownMethodGeneric(..)
            | Ty::UnknownArrayMember(..) => true,
            Ty::CompoundType(inner_ty, ..) => {
                inner_ty.is_unknown()
                    || inner_ty.is_unknown_ident()
                    || inner_ty.is_unknown_int()
                    || inner_ty.is_unknown_float()
            }
            _ => false,
        })
    }

    /// Checks if the given type is signed. This returns true if this is a signed
    /// integer, returns false for every other type (includingn non-int types).
    pub fn is_signed(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(match ty {
            Ty::CompoundType(inner_ty, ..) => matches!(
                inner_ty,
                InnerTy::I8 | InnerTy::I16 | InnerTy::I32 | InnerTy::I64 | InnerTy::I128
            ),
            _ => false,
        })
    }

    /// Given a type ID `type_id`, returns all "nested" type IDs that it contains.
    ///
    /// The set of all nested type IDs are given as a parameter instead of as a
    /// return value to prevent infinite recursion. Those situations can happen
    /// when ex. solving an array member which will check nested types for both
    /// the member AND the "wrapping parent" array.
    ///
    /// If `incl_inf` is set to true, this will include nested type IDs for any
    /// inferred types as well.
    pub fn nested_type_ids(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        all_nested_type_ids: &mut HashSet<TypeId>,
        type_id: TypeId,
        incl_inf: bool,
    ) -> LangResult<()> {
        if all_nested_type_ids.contains(&type_id) {
            return Ok(());
        } else {
            all_nested_type_ids.insert(type_id);
        }

        match self.ty(type_id)? {
            Ty::CompoundType(_, gens, ..) => {
                for gen_type_id in gens.iter_types() {
                    self.nested_type_ids(sub_sets, all_nested_type_ids, *gen_type_id, incl_inf)?;
                }
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.nested_type_ids(sub_sets, all_nested_type_ids, *type_id_i, incl_inf)?;
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.nested_type_ids(sub_sets, all_nested_type_ids, *ret_type_id, incl_inf)?;
                }
                for type_id_i in gens.iter().chain(params.iter()) {
                    self.nested_type_ids(sub_sets, all_nested_type_ids, *type_id_i, incl_inf)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.nested_type_ids(sub_sets, all_nested_type_ids, type_id_i, incl_inf)?;
                }
            }

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => (),
        }

        if incl_inf {
            if let Some(sub_sets) = sub_sets {
                let inf_type_id = sub_sets.inferred_type(type_id)?;
                if type_id != inf_type_id {
                    self.nested_type_ids(
                        Some(sub_sets),
                        all_nested_type_ids,
                        inf_type_id,
                        incl_inf,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Checks if the type with ID `type_id` contains generics with any of the
    /// names found in `gen_names`. The function will check both Generic's and
    /// GenericInstance's.
    pub fn contains_generic_with_name(
        &self,
        type_id: TypeId,
        gen_names: &[String],
    ) -> LangResult<bool> {
        Ok(match self.ty(type_id)? {
            Ty::Generic(gen_name, ..) | Ty::GenericInstance(gen_name, ..) => {
                gen_names.contains(gen_name)
            }

            Ty::CompoundType(_, gens, ..) => {
                for type_id in gens.iter_types() {
                    if self.contains_generic_with_name(*type_id, gen_names)? {
                        return Ok(true);
                    }
                }
                false
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.contains_generic_with_name(*type_id, gen_names)?
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_generic_with_name(type_id, gen_names)?
                } else {
                    false
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                for type_id_i in gens.iter().chain(params) {
                    if self.contains_generic_with_name(*type_id_i, gen_names)? {
                        return Ok(true);
                    }
                }

                if let Some(ret_type_id) = ret_type_id_opt {
                    if self.contains_generic_with_name(*ret_type_id, gen_names)? {
                        return Ok(true);
                    }
                }

                false
            }

            Ty::Any(..) => false,
        })
    }

    /// Checks if the type `child_ty` can be found in type with ID `parent_id`.
    /// This is a "shallow" check i.e. it doesn't check the contents of the type,
    /// only that the "type" of the type is correct.
    ///
    /// # Example
    ///
    /// If this function is called with:
    ///
    ///  `parent_id` = CompoundType("abc")
    ///   &
    ///  `child_id` = CompoundType("xyz")
    ///
    /// the function would return true even though "abc" != "xyz".
    pub fn contains_ty_shallow(&self, parent_id: TypeId, child_ty: &Ty) -> LangResult<bool> {
        let parent_ty = self.ty(parent_id)?;

        match (parent_ty, child_ty) {
            (Ty::CompoundType(..), Ty::CompoundType(..))
            | (Ty::Pointer(..), Ty::Pointer(..))
            | (Ty::Array(..), Ty::Array(..))
            | (Ty::Fn(..), Ty::Fn(..))
            | (Ty::Expr(..), Ty::Expr(..))
            | (Ty::Generic(..), Ty::Generic(..))
            | (Ty::Any(..), Ty::Any(..))
            | (Ty::GenericInstance(..), Ty::GenericInstance(..))
            | (Ty::UnknownAdtMember(..), Ty::UnknownAdtMember(..))
            | (Ty::UnknownAdtMethod(..), Ty::UnknownAdtMethod(..))
            | (Ty::UnknownMethodArgument(..), Ty::UnknownMethodArgument(..))
            | (Ty::UnknownMethodGeneric(..), Ty::UnknownMethodGeneric(..))
            | (Ty::UnknownArrayMember(..), Ty::UnknownArrayMember(..)) => return Ok(true),
            _ => (),
        }

        Ok(match parent_ty {
            Ty::CompoundType(_, gens, _) => {
                let mut contains = false;
                for type_id in gens.iter_types() {
                    if self.contains_ty_shallow(*type_id, child_ty)? {
                        contains = true;
                        break;
                    }
                }
                contains
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.contains_ty_shallow(*type_id, child_ty)?
            }

            Ty::UnknownAdtMethod(type_id, _, gen_type_ids, ..)
            | Ty::UnknownMethodArgument(type_id, _, gen_type_ids, ..) => {
                let mut contains_ty = self.contains_ty_shallow(*type_id, child_ty)?;
                for gen_type_id in gen_type_ids {
                    if self.contains_ty_shallow(*gen_type_id, child_ty)? {
                        contains_ty = true;
                    }
                }
                contains_ty
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_ty_shallow(type_id, child_ty)?
                } else {
                    false
                }
            }

            _ => false,
        })
    }

    fn contains_inner_ty_shallow(&self, id: TypeId, inner_ty: &InnerTy) -> LangResult<bool> {
        match self.ty(id)? {
            Ty::CompoundType(cur_inner_ty, gens, _) => {
                if cur_inner_ty.contains_inner_ty_shallow(inner_ty) {
                    return Ok(true);
                }

                for gen_type_id in gens.iter_types() {
                    if self.contains_inner_ty_shallow(*gen_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }

                Ok(false)
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.contains_inner_ty_shallow(*type_id, inner_ty)
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    if self.contains_inner_ty_shallow(*ret_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                for gen_type_id in gens {
                    if self.contains_inner_ty_shallow(*gen_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                for param_type_id in params {
                    if self.contains_inner_ty_shallow(*param_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_inner_ty_shallow(type_id, inner_ty)
                } else {
                    Ok(false)
                }
            }

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(false),
        }
    }

    pub fn contains_generic_decl_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let gen_ty = Ty::Generic("".into(), 0, TypeInfo::None);
        self.contains_ty_shallow(id, &gen_ty)
    }

    pub fn contains_generic_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let gen_ty = Ty::Generic("".into(), 0, TypeInfo::None);
        let gen_inst_ty = Ty::GenericInstance("".into(), 0, TypeInfo::None);
        Ok(self.contains_ty_shallow(id, &gen_ty)? || self.contains_ty_shallow(id, &gen_inst_ty)?)
    }

    pub fn contains_any_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let any_ty = Ty::Any(0, TypeInfo::None);
        self.contains_ty_shallow(id, &any_ty)
    }

    pub fn contains_unknown_any_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let unknown_adt_member = Ty::UnknownAdtMember(TypeId(0), "".into(), 0, TypeInfo::None);
        let unknown_adt_method = Ty::UnknownAdtMethod(
            TypeId(0),
            "".into(),
            Vec::with_capacity(0),
            0,
            TypeInfo::None,
        );
        let unknown_method_argument = Ty::UnknownMethodArgument(
            TypeId(0),
            "".into(),
            Vec::with_capacity(0),
            Either::Right(0),
            0,
            TypeInfo::None,
        );
        let unknown_method_generic =
            Ty::UnknownMethodGeneric(TypeId(0), "".into(), Either::Left(0), 0, TypeInfo::None);
        let unknown_array_member = Ty::UnknownArrayMember(TypeId(0), 0, TypeInfo::None);

        Ok(self.contains_unknown_inner_shallow(id)?
            || self.contains_unknown_ident_shallow(id)?
            || self.contains_unknown_int_shallow(id)?
            || self.contains_unknown_float_shallow(id)?
            || self.contains_ty_shallow(id, &unknown_adt_member)?
            || self.contains_ty_shallow(id, &unknown_adt_method)?
            || self.contains_ty_shallow(id, &unknown_method_argument)?
            || self.contains_ty_shallow(id, &unknown_method_generic)?
            || self.contains_ty_shallow(id, &unknown_array_member)?)
    }

    pub fn contains_unknown_inner_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::Unknown(0))
    }

    pub fn contains_unknown_int_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownInt(0, 0))
    }

    pub fn contains_unknown_float_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownFloat(0))
    }

    pub fn contains_unknown_ident_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownIdent(LangPath::empty(), 0))
    }

    pub fn assert_compatible(&self, first_id: TypeId, second_id: TypeId) -> LangResult<()> {
        if self.is_compatible(first_id, second_id)? {
            Ok(())
        } else {
            let first_ty = self.ty(first_id)?;
            let second_ty = self.ty(second_id)?;
            Err(LangError::new(
                format!(
                    "Tried to map incompatible types.\nFirst:\n{:#?}\nSecond:\n{:#?}",
                    first_ty, second_ty
                ),
                LangErrorKind::AnalyzeError,
                None,
            ))
        }
    }

    pub fn is_compatible(&self, first_id: TypeId, second_id: TypeId) -> LangResult<bool> {
        debug!(
            "is_compatible -- first_id: {}, second_id: {}",
            first_id, second_id
        );

        let first_id = self.get_forwarded(first_id);
        let second_id = self.get_forwarded(second_id);

        // Handles all cases with "Unknown" types.
        if self.is_generic(first_id)?
            || self.is_generic(second_id)?
            || self.is_any(first_id)?
            || self.is_any(second_id)?
        {
            return Ok(true);
        } else if self.is_unknown_int(first_id)? {
            if self.is_unknown_float(second_id)? || self.is_unknown_ident(second_id)? {
                return Ok(false);
            } else if self.is_int(second_id)? || self.is_unknown_any(second_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_int(second_id)? {
            if self.is_unknown_float(first_id)? || self.is_unknown_ident(first_id)? {
                return Ok(false);
            } else if self.is_int(first_id)? || self.is_unknown_any(first_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_float(first_id)? {
            if self.is_unknown_int(second_id)? || self.is_unknown_ident(second_id)? {
                return Ok(false);
            } else if self.is_float(second_id)? || self.is_unknown_any(second_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_float(second_id)? {
            if self.is_unknown_int(first_id)? || self.is_unknown_ident(first_id)? {
                return Ok(false);
            } else if self.is_float(first_id)? || self.is_unknown_any(first_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_any(first_id)? || self.is_unknown_any(second_id)? {
            return Ok(true);
        } else if self.is_string(first_id)? || self.is_string(second_id)? {
            // Add support for compatibility with string and {u8}. This is what
            // the string will be compiled down to for now, but should be changed
            // to a custom String struct later.
            // This allows for easy interop with C string during development.
            match (self.ty(first_id)?, self.ty(second_id)?) {
                (Ty::Pointer(ptr_type_id, ..), _) | (_, Ty::Pointer(ptr_type_id, ..)) => {
                    if let Ty::CompoundType(InnerTy::U8, ..) = self.ty(*ptr_type_id)? {
                        return Ok(true);
                    }
                }
                _ => (),
            }
        }

        // Handles all cases regarding types that isn't "Unknown" or generic.
        match (self.ty(first_id)?, self.ty(second_id)?) {
            (Ty::Pointer(inner_a, ..), Ty::Pointer(inner_b, ..))
            | (Ty::Array(inner_a, ..), Ty::Array(inner_b, ..)) => {
                self.is_compatible(*inner_a, *inner_b)
            }

            (Ty::CompoundType(comp_a, gens_a, ..), Ty::CompoundType(comp_b, gens_b, ..)) => {
                Ok(self.is_compatible_inner_ty(comp_a, comp_b)?
                    && self.is_compatible_gens(gens_a, gens_b)?)
            }

            (
                Ty::Fn(gens_a, params_a, ret_type_id_opt_a, ..),
                Ty::Fn(gens_b, params_b, ret_type_id_opt_b, ..),
            ) => {
                match (ret_type_id_opt_a, ret_type_id_opt_b) {
                    (Some(ret_type_id_a), Some(ret_type_id_b)) => {
                        return self.is_compatible(*ret_type_id_a, *ret_type_id_b);
                    }
                    (None, Some(_)) => return Ok(false),
                    (Some(_), None) => return Ok(false),
                    (None, None) => (),
                }

                if gens_a.len() != gens_b.len() {
                    return Ok(false);
                }
                for (gen_type_id_a, gen_type_id_b) in gens_a.iter().zip(gens_b) {
                    if !self.is_compatible(*gen_type_id_a, *gen_type_id_b)? {
                        return Ok(false);
                    }
                }

                if params_a.len() != params_b.len() {
                    return Ok(false);
                }
                for (param_type_id_a, param_type_id_b) in params_a.iter().zip(params_b) {
                    if !self.is_compatible(*param_type_id_a, *param_type_id_b)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            (Ty::Expr(expr_a, ..), Ty::Expr(expr_b, ..)) => {
                let type_id_a = expr_a.get_expr_type()?;
                let type_id_b = expr_b.get_expr_type()?;
                self.is_compatible(type_id_a, type_id_b)
            }
            (Ty::Expr(expr, ..), other_ty) | (other_ty, Ty::Expr(expr, ..)) => {
                let expr_type_id = expr.get_expr_type()?;
                let other_type_id = self.id_try(other_ty)?;
                self.is_compatible(expr_type_id, other_type_id)
            }

            _ => Ok(false),
        }
    }

    #[allow(clippy::suspicious_operation_groupings)]
    pub fn is_compatible_inner_ty(
        &self,
        first_inner_ty: &InnerTy,
        second_inner_ty: &InnerTy,
    ) -> LangResult<bool> {
        if (first_inner_ty.is_unknown() || second_inner_ty.is_unknown())
            || (first_inner_ty.is_unknown_int() && second_inner_ty.is_int())
            || (second_inner_ty.is_unknown_int() && first_inner_ty.is_int())
            || (first_inner_ty.is_unknown_float() && second_inner_ty.is_float())
            || (second_inner_ty.is_unknown_float() && first_inner_ty.is_float())
        {
            return Ok(true);
        } else if (
                (first_inner_ty.is_adt() || first_inner_ty.is_trait() || first_inner_ty.is_unknown_ident())
                &&
                !(second_inner_ty.is_adt() || second_inner_ty.is_trait() || second_inner_ty.is_unknown_ident())
            )
            ||  // (this comment disables auto-formatting)
            (
                (second_inner_ty.is_adt() || second_inner_ty.is_trait() || second_inner_ty.is_unknown_ident())
                &&
                !(first_inner_ty.is_adt() || first_inner_ty.is_trait() || first_inner_ty.is_unknown_ident())
            )
        {
            return Ok(false);
        }

        match (first_inner_ty, second_inner_ty) {
            (InnerTy::Struct(path_a), InnerTy::Struct(path_b))
            | (InnerTy::Struct(path_a), InnerTy::UnknownIdent(path_b, ..))
            | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Struct(path_b, ..))
            | (InnerTy::Enum(path_a), InnerTy::Enum(path_b))
            | (InnerTy::Enum(path_a), InnerTy::UnknownIdent(path_b, ..))
            | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Enum(path_b, ..))
            | (InnerTy::Union(path_a), InnerTy::Union(path_b))
            | (InnerTy::Union(path_a), InnerTy::UnknownIdent(path_b, ..))
            | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Union(path_b, ..))
            | (InnerTy::Trait(path_a), InnerTy::Trait(path_b))
            | (InnerTy::Trait(path_a), InnerTy::UnknownIdent(path_b, ..))
            | (InnerTy::UnknownIdent(path_a, ..), InnerTy::Trait(path_b, ..)) => {
                self.is_compatible_path(path_a, path_b)
            }

            (InnerTy::Void, InnerTy::Void)
            | (InnerTy::Character, InnerTy::Character)
            | (InnerTy::String, InnerTy::String)
            | (InnerTy::Boolean, InnerTy::Boolean)
            | (InnerTy::I8, InnerTy::I8)
            | (InnerTy::U8, InnerTy::U8)
            | (InnerTy::I16, InnerTy::I16)
            | (InnerTy::U16, InnerTy::U16)
            | (InnerTy::I32, InnerTy::I32)
            | (InnerTy::U32, InnerTy::U32)
            | (InnerTy::F32, InnerTy::F32)
            | (InnerTy::I64, InnerTy::I64)
            | (InnerTy::U64, InnerTy::U64)
            | (InnerTy::F64, InnerTy::F64)
            | (InnerTy::I128, InnerTy::I128)
            | (InnerTy::U128, InnerTy::U128) => Ok(true),

            _ => Ok(false),
        }
    }

    pub fn is_compatible_path(
        &self,
        first_path: &LangPath,
        second_path: &LangPath,
    ) -> LangResult<bool> {
        // There is a possiblity that one of the paths is prepended with the
        // module path while the other isn't. Do the comparison back to front so
        // that if one of the paths contains the module, that part will be skipped.
        for (first_part, second_part) in first_path
            .parts
            .iter()
            .rev()
            .zip(second_path.parts.iter().rev())
        {
            if first_part.name() != second_part.name() {
                return Ok(false);
            }

            // TODO: Is there a possiblity that one of the paths doesn't have the
            //       generics specified while the other one does at this point?
            match (first_part.generics(), second_part.generics()) {
                (Some(first_gens), Some(second_gens)) => {
                    if !self.is_compatible_gens(first_gens, second_gens)? {
                        return Ok(false);
                    }
                }
                (Some(_), None) | (None, Some(_)) => return Ok(false),
                (None, None) => (),
            }
        }

        // The only path out of this function that returns `true`. Any earlier
        // return from this function will be `false`.
        Ok(true)
    }

    pub fn is_compatible_gens(
        &self,
        first_gens: &Generics,
        second_gens: &Generics,
    ) -> LangResult<bool> {
        if first_gens.len_types() != second_gens.len_types() {
            return Ok(false);
        }

        for (first_name, second_name) in first_gens.iter_names().zip(second_gens.iter_names()) {
            if first_name != second_name {
                return Ok(false);
            }
        }

        for (first_id, second_id) in first_gens.iter_types().zip(second_gens.iter_types()) {
            if !self.is_compatible(*first_id, *second_id)? {
                return Ok(false);
            }
        }

        // The only path out of this function that returns `true`. Any earlier
        // return from this function will be `false`.
        Ok(true)
    }

    // TODO: Can the preference between point 4 and 5 fail some valid programs?
    //       Ex. a pointer to a unknown is currently preferred over a GenericInstance.
    //       Should there be two different points for fn/array/pointer containing
    //       solved types and fn/array/pointer containing unsolved types? These
    //       would then be inserted before and after the GenericInstance respectively
    //       in the preference list below.
    /// Checks the precedence for the two given types when it comes to mapping
    /// during type inference. Returns a `std::cmp::Ordering` indicating the
    /// preference between the given types.
    ///
    /// `Ordering::Less` indicates that the type with ID `first_id` is preferred.
    /// `Ordering::Greater` indicates that the type with ID`second_id` is preferred.
    ///
    /// If two types have the exact same preference, the one with the lowest
    /// type ID will be picked. This is done to try and keep thev precedence as
    /// "uniform" as possible. If the given type IDs are equal, the first one
    /// is chosen (this function returns `Ordering::Less`).
    ///
    /// General ordering list (most preferred at the top, least at bottom):
    ///  1.  Primitive
    ///  2.  ADT/Trait
    ///  3.  UnknownInt
    ///      UnknownFloat
    ///  4.  Fn
    ///      Array
    ///      Pointer
    ///  5.  GenericInstance
    ///  6.  UnknownIdent
    ///      UnknownMethodGeneric
    ///      UnknownMethodArgument
    ///      UnknownAdtMethod
    ///      UnknownAdtMember
    ///  7.  UnknownArrayMember
    ///  8.  Generic
    ///  9.  Unknown
    ///  10. Any
    ///  11. Expr   (expression evaluated to a type)
    pub fn precedence(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        first_id: TypeId,
        second_id: TypeId,
    ) -> LangResult<Ordering> {
        match self.prec_allow_eq(sub_sets, first_id, second_id)? {
            Ordering::Equal => Ok(self.prec_eq(first_id, second_id)),
            res => Ok(res),
        }
    }

    /// This function is equal to the `precedence()` with the only difference that
    /// it returns `Ordering::Equal` for some types that are equals. This is
    /// needed to comapre the types properly to then return a "final" answer
    /// to the caller of `precedence()` that will either prefer the first or
    /// second type.
    fn prec_allow_eq(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        first_id: TypeId,
        second_id: TypeId,
    ) -> LangResult<Ordering> {
        if first_id == second_id {
            return Ok(self.prec_eq(first_id, second_id));
        }

        let inf_first_id = sub_sets.map_or(Ok(first_id), |sets| sets.inferred_type(first_id))?;
        let inf_second_id = sub_sets.map_or(Ok(second_id), |sets| sets.inferred_type(second_id))?;

        // If the two types are in the same substitution set, use the "original"
        // type IDs when checking the preference. If they are in two different sets,
        // use the inferred type i.e. the root of their respective subtitution
        // sets to check the preference.
        let (prec_first_id, prec_second_id) = if inf_first_id == inf_second_id {
            (first_id, second_id)
        } else {
            (inf_first_id, inf_second_id)
        };

        debug!(
            "precedence_allow_equal -- first_id: {}, inf_first_id: {}, prec_first_id: {}, \n\
            second_id: {}, inf_second_id: {}, prec_second_id: {}",
            first_id, inf_first_id, prec_first_id, second_id, inf_second_id, prec_second_id
        );

        match (self.ty(prec_first_id)?, self.ty(prec_second_id)?) {
            (Ty::Expr(expr_a, ..), Ty::Expr(expr_b, ..)) => {
                let type_id_a = expr_a.get_expr_type()?;
                let type_id_b = expr_b.get_expr_type()?;
                self.prec_allow_eq(sub_sets, type_id_a, type_id_b)
            }
            (Ty::Expr(expr, ..), other_ty) | (other_ty, Ty::Expr(expr, ..)) => {
                let expr_type_id = expr.get_expr_type()?;
                let other_type_id = self.id_try(other_ty)?;
                self.prec_allow_eq(sub_sets, expr_type_id, other_type_id)
            }

            (Ty::Any(id_a, ..), Ty::Any(id_b, ..)) => Ok(id_a.cmp(id_b)),
            (Ty::Any(..), _) => Ok(Ordering::Greater),
            (_, Ty::Any(..)) => Ok(Ordering::Less),

            (Ty::CompoundType(inner_a, ..), Ty::CompoundType(inner_b, ..))
                if inner_a.is_unknown() && inner_b.is_unknown() =>
            {
                Ok(self.prec_inner_ty(inner_a, inner_b))
            }
            (Ty::CompoundType(inner, ..), _) if inner.is_unknown() => Ok(Ordering::Greater),
            (_, Ty::CompoundType(inner, ..)) if inner.is_unknown() => Ok(Ordering::Less),

            (Ty::Generic(_, id_a, ..), Ty::Generic(_, id_b, ..)) => Ok(id_a.cmp(id_b)),
            (Ty::Generic(..), _) => Ok(Ordering::Greater),
            (_, Ty::Generic(..)) => Ok(Ordering::Less),

            (
                Ty::UnknownArrayMember(type_id_a, unique_id_a, ..),
                Ty::UnknownArrayMember(type_id_b, unique_id_b, ..),
            ) => {
                let ord = self.prec_allow_eq(sub_sets, *type_id_a, *type_id_b)?;
                if ord != Ordering::Equal {
                    Ok(ord)
                } else if unique_id_a != unique_id_b {
                    Ok(unique_id_a.cmp(unique_id_b))
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Ty::UnknownArrayMember(..), _) => Ok(Ordering::Greater),
            (_, Ty::UnknownArrayMember(..)) => Ok(Ordering::Less),

            (
                Ty::UnknownAdtMember(type_id_a, _, unique_id_a, ..),
                Ty::UnknownAdtMember(type_id_b, _, unique_id_b, ..),
            ) => {
                let ord = self.prec_allow_eq(sub_sets, *type_id_a, *type_id_b)?;
                if ord != Ordering::Equal {
                    Ok(ord)
                } else if unique_id_a != unique_id_b {
                    Ok(unique_id_a.cmp(unique_id_b))
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Ty::UnknownAdtMember(..), _) => Ok(Ordering::Greater),
            (_, Ty::UnknownAdtMember(..)) => Ok(Ordering::Less),

            (
                Ty::UnknownAdtMethod(type_id_a, _, gens_a, unique_id_a, ..),
                Ty::UnknownAdtMethod(type_id_b, _, gens_b, unique_id_b, ..),
            ) => {
                let ord = self.prec_incl_gens(sub_sets, *type_id_a, gens_a, *type_id_b, gens_b)?;
                if ord != Ordering::Equal {
                    Ok(ord)
                } else if unique_id_a != unique_id_b {
                    Ok(unique_id_a.cmp(unique_id_b))
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Ty::UnknownAdtMethod(..), _) => Ok(Ordering::Greater),
            (_, Ty::UnknownAdtMethod(..)) => Ok(Ordering::Less),

            (
                Ty::UnknownMethodArgument(type_id_a, _, gens_a, either_a, unique_id_a, ..),
                Ty::UnknownMethodArgument(type_id_b, _, gens_b, either_b, unique_id_b, ..),
            ) => {
                let ord = self.prec_incl_gens(sub_sets, *type_id_a, gens_a, *type_id_b, gens_b)?;
                if ord != Ordering::Equal {
                    Ok(ord)
                } else if either_a != either_b {
                    Ok(either_a.cmp(either_b))
                } else if unique_id_a != unique_id_b {
                    Ok(unique_id_a.cmp(unique_id_b))
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Ty::UnknownMethodArgument(..), _) => Ok(Ordering::Greater),
            (_, Ty::UnknownMethodArgument(..)) => Ok(Ordering::Less),

            (
                Ty::UnknownMethodGeneric(type_id_a, _, either_a, unique_id_a, ..),
                Ty::UnknownMethodGeneric(type_id_b, _, either_b, unique_id_b, ..),
            ) => {
                let ord = self.prec_allow_eq(sub_sets, *type_id_a, *type_id_b)?;
                if ord != Ordering::Equal {
                    Ok(ord)
                } else if either_a != either_b {
                    Ok(either_a.cmp(either_b))
                } else if unique_id_a != unique_id_b {
                    Ok(unique_id_a.cmp(unique_id_b))
                } else {
                    Ok(Ordering::Equal)
                }
            }
            (Ty::UnknownMethodGeneric(..), _) => Ok(Ordering::Greater),
            (_, Ty::UnknownMethodGeneric(..)) => Ok(Ordering::Less),

            (Ty::CompoundType(inner_a, ..), Ty::CompoundType(inner_b, ..))
                if inner_a.is_unknown_ident() && inner_b.is_unknown_ident() =>
            {
                Ok(self.prec_inner_ty(inner_a, inner_b))
            }
            (Ty::CompoundType(inner, ..), _) if inner.is_unknown_ident() => Ok(Ordering::Greater),
            (_, Ty::CompoundType(inner, ..)) if inner.is_unknown_ident() => Ok(Ordering::Less),

            (Ty::GenericInstance(_, id_a, ..), Ty::GenericInstance(_, id_b, ..)) => {
                Ok(id_a.cmp(id_b))
            }
            (Ty::GenericInstance(..), _) => Ok(Ordering::Greater),
            (_, Ty::GenericInstance(..)) => Ok(Ordering::Less),

            (Ty::Pointer(type_id_a, ..), Ty::Pointer(type_id_b, ..)) => {
                self.prec_allow_eq(sub_sets, *type_id_a, *type_id_b)
            }
            (Ty::Pointer(..), _) => Ok(Ordering::Greater),
            (_, Ty::Pointer(..)) => Ok(Ordering::Less),

            (Ty::Array(type_id_a, ..), Ty::Array(type_id_b, ..)) => {
                // TODO: Probably makes sense to consider the dimension during compare.
                self.prec_allow_eq(sub_sets, *type_id_a, *type_id_b)
            }
            (Ty::Array(..), _) => Ok(Ordering::Greater),
            (_, Ty::Array(..)) => Ok(Ordering::Less),

            (
                Ty::Fn(gen_type_ids_a, param_type_ids_a, ret_type_id_a, ..),
                Ty::Fn(gen_type_ids_b, param_type_ids_b, ret_type_id_b, ..),
            ) => {
                match (ret_type_id_a, ret_type_id_b) {
                    (Some(ret_type_id_a), Some(ret_type_id_b)) => {
                        let ord = self.prec_allow_eq(sub_sets, *ret_type_id_a, *ret_type_id_b)?;
                        if ord != Ordering::Equal {
                            return Ok(ord);
                        }
                    }
                    (None, None) => (),
                    _ => unreachable!(
                        "ret_type_id_a: {:?}, ret_type_id_b: {:?}",
                        ret_type_id_a, ret_type_id_b
                    ),
                }

                assert!(
                    gen_type_ids_a.len() == gen_type_ids_b.len(),
                    "gen len diff. gen_type_ids_a: {:#?}, gen_type_ids_b: {:#?}",
                    gen_type_ids_a,
                    gen_type_ids_b
                );
                for (gen_type_id_a, gen_type_id_b) in
                    gen_type_ids_a.iter().zip(gen_type_ids_b.iter())
                {
                    let ord = self.prec_allow_eq(sub_sets, *gen_type_id_a, *gen_type_id_b)?;
                    if ord != Ordering::Equal {
                        return Ok(ord);
                    }
                }

                assert!(
                    param_type_ids_a.len() == param_type_ids_b.len(),
                    "gen len diff. param_type_ids_a: {:#?}, param_type_ids_b: {:#?}",
                    param_type_ids_a,
                    param_type_ids_b
                );
                for (par_type_id_a, par_type_id_b) in
                    param_type_ids_a.iter().zip(param_type_ids_b.iter())
                {
                    let ord = self.prec_allow_eq(sub_sets, *par_type_id_a, *par_type_id_b)?;
                    if ord != Ordering::Equal {
                        return Ok(ord);
                    }
                }

                Ok(Ordering::Equal)
            }
            (Ty::Fn(..), _) => Ok(Ordering::Greater),
            (_, Ty::Fn(..)) => Ok(Ordering::Less),

            (Ty::CompoundType(inner_a, gens_a, ..), Ty::CompoundType(inner_b, gens_b, ..)) => {
                let ord = self.prec_inner_ty(inner_a, inner_b);
                if ord != Ordering::Equal {
                    return Ok(ord);
                }

                assert!(
                    gens_a.len() == gens_b.len(),
                    "gen len diff. gens_a: {:#?}, gens_b: {:#?}",
                    gens_a,
                    gens_b
                );

                for (gen_a_type_id, gen_b_type_id) in gens_a.iter_types().zip(gens_b.iter_types()) {
                    let ord = self.prec_allow_eq(sub_sets, *gen_a_type_id, *gen_b_type_id)?;
                    if ord != Ordering::Equal {
                        return Ok(ord);
                    }
                }

                Ok(Ordering::Equal)
            }
        }
    }

    fn prec_inner_ty(&self, first_inner_ty: &InnerTy, second_inner_ty: &InnerTy) -> Ordering {
        match (first_inner_ty, second_inner_ty) {
            (InnerTy::Unknown(unique_id_a), InnerTy::Unknown(unique_id_b)) => {
                if unique_id_a != unique_id_b {
                    unique_id_a.cmp(unique_id_b)
                } else {
                    Ordering::Equal
                }
            }
            (InnerTy::Unknown(_), _) => Ordering::Greater,
            (_, InnerTy::Unknown(_)) => Ordering::Less,

            (InnerTy::UnknownInt(unique_id_a, ..), InnerTy::UnknownInt(unique_id_b, ..)) => {
                if unique_id_a != unique_id_b {
                    unique_id_a.cmp(unique_id_b)
                } else {
                    Ordering::Equal
                }
            }
            (InnerTy::UnknownInt(..), _) => Ordering::Greater,
            (_, InnerTy::UnknownInt(..)) => Ordering::Less,

            (InnerTy::UnknownFloat(unique_id_a), InnerTy::UnknownFloat(unique_id_b)) => {
                if unique_id_a != unique_id_b {
                    unique_id_a.cmp(unique_id_b)
                } else {
                    Ordering::Equal
                }
            }
            (InnerTy::UnknownFloat(..), _) => Ordering::Greater,
            (_, InnerTy::UnknownFloat(..)) => Ordering::Less,

            (InnerTy::UnknownIdent(..), InnerTy::UnknownIdent(..)) => Ordering::Equal,
            (InnerTy::UnknownIdent(..), _) => Ordering::Greater,
            (_, InnerTy::UnknownIdent(..)) => Ordering::Less,

            (InnerTy::Struct(_), InnerTy::Struct(_))
            | (InnerTy::Enum(_), InnerTy::Enum(_))
            | (InnerTy::Union(_), InnerTy::Union(_))
            | (InnerTy::Trait(_), InnerTy::Trait(_))
            | (InnerTy::Void, InnerTy::Void)
            | (InnerTy::Character, InnerTy::Character)
            | (InnerTy::String, InnerTy::String)
            | (InnerTy::Boolean, InnerTy::Boolean)
            | (InnerTy::I8, InnerTy::I8)
            | (InnerTy::U8, InnerTy::U8)
            | (InnerTy::I16, InnerTy::I16)
            | (InnerTy::U16, InnerTy::U16)
            | (InnerTy::I32, InnerTy::I32)
            | (InnerTy::U32, InnerTy::U32)
            | (InnerTy::F32, InnerTy::F32)
            | (InnerTy::I64, InnerTy::I64)
            | (InnerTy::U64, InnerTy::U64)
            | (InnerTy::F64, InnerTy::F64)
            | (InnerTy::I128, InnerTy::I128)
            | (InnerTy::U128, InnerTy::U128) => Ordering::Equal,

            _ => unreachable!(
                "first_inner_ty: {:#?}, second_inner_ty: {:#?}",
                first_inner_ty, second_inner_ty
            ),
        }
    }

    fn prec_incl_gens(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        first_id: TypeId,
        first_ids: &[TypeId],
        second_id: TypeId,
        second_ids: &[TypeId],
    ) -> LangResult<Ordering> {
        let ord = self.prec_allow_eq(sub_sets, first_id, second_id)?;
        if ord != Ordering::Equal {
            return Ok(ord);
        }

        assert!(
            first_ids.len() == second_ids.len(),
            "call gen len diff. first_ids: {:#?}, inf_second_id: {:#?}",
            first_ids,
            second_ids
        );

        for (first_id_i, second_id_i) in first_ids.iter().zip(second_ids.iter()) {
            let ord = self.prec_allow_eq(sub_sets, *first_id_i, *second_id_i)?;
            if ord != Ordering::Equal {
                return Ok(ord);
            }
        }

        Ok(Ordering::Equal)
    }

    /// Function used when the two types have the same preference. This function
    /// will decide which type is preferred. The type with the lowest type ID is
    /// preferred. If the given type IDs are equal, the first is preferred.
    fn prec_eq(&self, first_id: TypeId, second_id: TypeId) -> Ordering {
        if first_id <= second_id {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}
