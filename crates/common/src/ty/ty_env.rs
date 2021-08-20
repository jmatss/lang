use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::Mutex,
};

use lazy_static::lazy_static;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    hash::DerefType,
    TypeId,
};

use super::{substitution_sets::SubstitutionSets, ty::Ty, ty_interner::TyInterner};

lazy_static! {
    pub static ref TY_ENV: Mutex<TyEnv> = Mutex::new(TyEnv::default());
}

#[derive(Debug)]
pub struct TyEnv {
    pub interner: TyInterner,
    pub sub_sets: SubstitutionSets,

    /// This will be set to `true` when it is time to start solving all
    /// the types. When this is activated, newly created types will be inserted
    /// into the `unsolved`. This is used so that one can quickly fetch any
    /// new types created during the solving phase.
    pub solve_mode: bool,
    pub new_type_ids: HashSet<TypeId>,
}

impl Default for TyEnv {
    fn default() -> Self {
        Self {
            interner: TyInterner::default(),
            sub_sets: SubstitutionSets::default(),
            solve_mode: false,
            new_type_ids: HashSet::default(),
        }
    }
}

impl TyEnv {
    /// Returns the type contained in the root node of the set that contains the
    /// type `type_id`. This will be the type with the highest precedence in the
    /// set.
    ///
    /// If the given `type_id` doesn't exist in any set, returns the given
    /// `type_id` itself.
    pub fn inferred_type(&self, type_id: TypeId) -> LangResult<TypeId> {
        if let Some(id) = self.sub_sets.ty_to_id.get(&type_id) {
            let root_id = self.sub_sets.find_root(*id).unwrap();
            let root_node = self.sub_sets.id_to_node.get(&root_id).unwrap();

            Ok(root_node.type_id)
        } else {
            Ok(type_id)
        }
    }

    /// Inserts a new type `ty` into this environment and return a new TypeId
    /// that refers to that type. If the type already exists in this environment,
    /// a error will be returned.
    fn new_ty(&mut self, ty: Ty) -> LangResult<TypeId> {
        let type_id = TypeId(self.interner.type_id);
        self.interner.type_id += 1;

        debug!("new_ty -- type_id: {}, ty: {:#?}", type_id, ty);

        self.interner.id_to_ty.insert(type_id, ty.clone());

        let deref_type = DerefType::Shallow;
        if self.interner.ty_to_id.contains_key(self, deref_type, &ty)? {
            return Err(LangError::new(
                format!(
                    "Tried to create new type that already exists in environment: {:#?}",
                    ty
                ),
                LangErrorKind::GeneralError,
                None,
            ));
        }

        // TODO: Make safe.
        let unsafe_self = unsafe { (self as *const TyEnv).as_ref().unwrap() };
        self.interner
            .ty_to_id
            .insert(unsafe_self, deref_type, ty, type_id)?;

        if self.solve_mode {
            self.new_type_ids.insert(type_id);
        }

        Ok(type_id)
    }

    pub fn current_type_id(&self) -> u64 {
        self.interner.type_id
    }

    pub fn new_unique_id(&mut self) -> u64 {
        let unique_id = self.interner.unique_id;
        self.interner.unique_id += 1;
        unique_id
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If this environment already have "seen" the type `ty`, the reference to
    /// that type is returned. If `ty` is a new type that haven't been "seen"
    /// before, a new TypeId will be assigned to it and returned.
    pub fn id(&mut self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(type_id) = self.interner.ty_to_id.get(self, DerefType::Shallow, &ty)? {
            Ok(*type_id)
        } else {
            self.new_ty(ty.clone())
        }
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If `ty` is a type that doesn't exists in this type environment, a error
    /// is returned.
    pub fn id_try(&self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(type_id) = self.interner.ty_to_id.get(self, DerefType::Shallow, &ty)? {
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
        let type_id = self.forwarded(type_id);
        if let Some(ty) = self.interner.id_to_ty.get(&type_id) {
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
        let type_id = self.forwarded(type_id);
        if let Some(ty) = self.interner.id_to_ty.get_mut(&type_id) {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to find type with type ID {}.", type_id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Given a type ID `type_id`, gets the type that the `type_id` refers to.
    /// If `type_id` doesn't refer to a type, a error will be returned.
    pub fn ty_clone(&self, type_id: TypeId) -> LangResult<Ty> {
        self.ty(type_id).map(|ty_ref| ty_ref.clone())
    }

    /// Updates the the type with type ID `type_id` to the type `ty` and returns
    /// the old type. If no type with ID `type_id` exists in the environment, a
    /// error is returned.
    pub fn update(&mut self, type_id: TypeId, ty: Ty) -> LangResult<Ty> {
        match self.interner.id_to_ty.entry(type_id) {
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
        if let Some(ty) = self.interner.id_to_ty.remove(&type_id) {
            // TODO: Make safe.
            let unsafe_self = unsafe { (self as *const TyEnv).as_ref().unwrap() };
            if let Err(err) = self
                .interner
                .ty_to_id
                .remove(unsafe_self, DerefType::Shallow, &ty)
            {
                panic!(
                    "ty_env::remove -- type_id: {}, ty: {:#?}, err: {:#?}",
                    type_id, ty, err
                );
            }
        }

        // Nice to be able to reuse the type ID if possible (if no new types have
        // been created between the `type_id` was created and then removed).
        if self.interner.type_id > 0 && self.interner.type_id - 1 == type_id.0 {
            self.interner.type_id -= 1;
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
        let fwd_to_type_id = self.forwarded(to_type_id);
        self.interner.forwards.insert(from_type_id, fwd_to_type_id);

        if let Some(from_ty) = self.interner.id_to_ty.remove(&from_type_id) {
            // TODO: Make safe.
            let unsafe_self = unsafe { (self as *const TyEnv).as_ref().unwrap() };
            self.interner.ty_to_id.insert(
                unsafe_self,
                DerefType::Shallow,
                from_ty,
                fwd_to_type_id,
            )?;
        }

        Ok(())
    }

    pub fn forwarded(&self, type_id: TypeId) -> TypeId {
        if let Some(forwarded_type_id) = self.interner.forwards.get(&type_id) {
            *forwarded_type_id
        } else {
            type_id
        }
    }

    pub fn forwards(&self) -> HashMap<TypeId, TypeId> {
        self.interner.forwards.clone()
    }
}
