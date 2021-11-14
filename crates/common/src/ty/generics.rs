use std::{collections::HashMap, hash::Hash};

use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    path::LangPath,
    traverse::traverse_ctx::TraverseCtx,
    ty::to_string::to_string_path,
    TypeId,
};

use super::{ty::Ty, ty_env::TyEnv, type_info::TypeInfo};

/// Used to indicate what kind this "Generics" is.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericsKind {
    /// Indicates that this "Generics" struct is a declaration of generics.
    /// Examples are declarations of generics on structs etc.
    ///   (<K, V>)
    Decl,

    /// Indicates that this "Generics" struct is a implementation of generics.
    /// This is the real types that will replace the declared generics.
    ///   (<&str, i32>)
    Impl,

    /// Is not supposed to have any generics so this struct should be empty.
    Empty,
}

#[derive(Debug, Clone, Default)]
pub struct Generics {
    pub names: Vec<String>,
    pub types: Vec<TypeId>,

    /// A map used to do fast lookups. The key is the name of the generic and
    /// the value is the index of that generic in both `names` and `types`.
    lookup: HashMap<String, usize>,
}

impl Generics {
    pub fn new() -> Self {
        Self {
            names: Vec::default(),
            types: Vec::default(),
            lookup: HashMap::default(),
        }
    }

    pub fn empty() -> Self {
        Self {
            names: Vec::with_capacity(0),
            types: Vec::with_capacity(0),
            lookup: HashMap::with_capacity(0),
        }
    }

    pub fn len(&self) -> usize {
        if self.names.is_empty() {
            self.types.len()
        } else if self.types.is_empty() {
            self.names.len()
        } else {
            (self.names.len() + self.types.len()) / 2
        }
    }

    pub fn len_names(&self) -> usize {
        self.names.len()
    }

    pub fn len_types(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty() && self.types.is_empty()
    }

    pub fn is_empty_names(&self) -> bool {
        self.names.is_empty()
    }

    pub fn is_empty_types(&self) -> bool {
        self.types.is_empty()
    }

    pub fn get(&self, name: &str) -> Option<TypeId> {
        if let Some(idx) = self.lookup.get(name) {
            self.types.get(*idx).copied()
        } else {
            None
        }
    }

    pub fn get_name(&self, idx: usize) -> Option<String> {
        self.names.get(idx).cloned()
    }

    pub fn contains(&self, name: &str) -> bool {
        self.names.contains(&name.into())
    }

    /// Inserts a new generic type into this Generics. If the generic with the
    /// name `name` already has been set, this will change the type of that
    /// generic instead of creating a new entry.
    pub fn insert(&mut self, name: String, id: TypeId) {
        if let Some(idx) = self.lookup.get(&name) {
            self.types[*idx] = id;
        } else {
            let idx = self.names.len();
            self.lookup.insert(name.clone(), idx);

            self.names.push(name);
            self.types.push(id);
        }
    }

    pub fn insert_lookup(&mut self, name: String, idx: usize) {
        self.lookup.insert(name, idx);
    }

    pub fn insert_name(&mut self, name: String) {
        if !self.names.contains(&name) {
            self.names.push(name);
        }
    }

    pub fn insert_type(&mut self, id: TypeId) {
        self.types.push(id);
    }

    pub fn iter_names(&self) -> std::slice::Iter<String> {
        self.names.iter()
    }

    pub fn iter_names_mut(&mut self) -> std::slice::IterMut<String> {
        self.names.iter_mut()
    }

    pub fn iter_types(&self) -> std::slice::Iter<TypeId> {
        self.types.iter()
    }

    pub fn iter_types_mut(&mut self) -> std::slice::IterMut<TypeId> {
        self.types.iter_mut()
    }
}

impl From<&[TypeId]> for Generics {
    fn from(type_ids: &[TypeId]) -> Self {
        let mut gens = Generics::new();
        for type_id_i in type_ids {
            gens.insert_type(*type_id_i);
        }
        gens
    }
}

impl TyEnvHash for Generics {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        if !matches!(deref_type, DerefType::None) {
            if !self.types.is_empty() {
                for gen_type_id in self.types.iter() {
                    gen_type_id.hash_with_state(ty_env, deref_type, state)?;
                }
            } else if !self.names.is_empty() {
                self.names.hash(state);
            }
        }
        Ok(())
    }
}

// TODO: Clean up. Either remove this function or try to combine in some way
//       with `set_generic_names`. Makes no sense being able to call this
//       function when `adt_path` is None.
/// Given a ADT with partial path `adt_path`, combines the names of the ADT generic
/// declarations with the types from the ADT instance generics (`gen_impls`).
///
/// If the given ADT doesn't have any generics, this function will return None.
/// If the given `gen_impls` is None or empty but the ADT have declared generics,
/// new "GenericInstance" types will be created and used as the types.
///
/// The given `fn_call_path` is just for error printing, it will never be used
/// in a "normal flow" of this function.
pub fn combine_generics_adt(
    ctx: &mut TraverseCtx,
    adt_path: &LangPath,
    gen_impls: Option<&Generics>,
    fn_call_path: &LangPath,
) -> LangResult<Option<Generics>> {
    let adt =
        ctx.ast_ctx
            .get_adt_partial(&ctx.ty_env.lock(), &adt_path.without_gens(), ctx.block_id)?;
    let adt = adt.read();

    let gen_names = if let Some(gens) = &adt.generics {
        gens.iter_names().cloned().collect::<Vec<_>>()
    } else {
        return Ok(None);
    };

    combine_generics(ctx, &gen_names, gen_impls, fn_call_path, Some(adt_path))
}

/// `fn_call_path` and `adt_path` are only used for error printing.
pub fn combine_generics(
    ctx: &mut TraverseCtx,
    gen_names: &[String],
    gen_impls: Option<&Generics>,
    fn_call_path: &LangPath,
    adt_path: Option<&LangPath>,
) -> LangResult<Option<Generics>> {
    if gen_names.is_empty() {
        return Ok(None);
    }

    let mut new_gens = Generics::new();

    // If the generics impls have been specified, use those to populate the
    // Generics inside `new_gens`.
    // Else if no generic implements have been specified, create new
    // "GenericInstance" types that will be used to create the generics.
    if let Some(gen_impls) = gen_impls {
        if gen_names.len() != gen_impls.len_types() {
            let fn_name = to_string_path(&ctx.ty_env.lock(), fn_call_path);
            let err_names = if let Some(adt_path) = adt_path {
                format!("Adt name: {:?}, method name: {}", &adt_path, &fn_name)
            } else {
                format!("Function name: {}", &fn_name)
            };

            return Err(ctx.ast_ctx.err(format!(
                "Wrong amount of generics. Actual: {}, expected: {}\n{}\nGeneric names: {:?}",
                gen_impls.len_types(),
                gen_names.len(),
                err_names,
                gen_names
            )));
        }

        for (gen_name, gen_type_id) in gen_names.iter().zip(gen_impls.iter_types()) {
            new_gens.insert(gen_name.into(), *gen_type_id);
        }
    } else {
        for gen_name in gen_names {
            let unique_id = ctx.ty_env.lock().new_unique_id();
            let gen_type_id = ctx.ty_env.lock().id(&Ty::GenericInstance(
                gen_name.clone(),
                unique_id,
                TypeInfo::None,
            ))?;

            new_gens.insert(gen_name.clone(), gen_type_id);
        }
    }

    Ok(Some(new_gens))
}
