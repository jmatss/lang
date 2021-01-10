use super::context::TypeContext;
use crate::block::BlockInfo;
use common::{error::LangError, traverser::TraverseContext, ty::ty::Ty, visitor::Visitor};
use std::collections::{hash_map::Entry, HashMap};

/// Iterates through the tokens and gathers all structures containing generics.
/// This information will be used to create new instances of the structures where
/// the generics are "implemented". Ex. "Struct<T>" might be implemented as
/// "Struct<i64>" & "Struct<f32>".
pub struct GenericStructsCollector<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,

    /// Will contain all types containing generics. This will then be used to
    /// create new generic structures with the specific generic implementations.
    /// The String key is the name of the structure/type. The Ty values are the
    /// actual types containing generic implentations/instances.
    pub generic_structures: HashMap<String, Vec<Ty>>,

    errors: Vec<LangError>,
}

impl<'a, 'tctx> GenericStructsCollector<'a, 'tctx> {
    pub fn new(type_context: &'a mut TypeContext<'tctx>) -> Self {
        Self {
            type_context,
            generic_structures: HashMap::default(),
            errors: Vec::default(),
        }
    }

    /// If the given type represents a type that contains generics, this function
    /// will insert those into `self.generic_structures`. This map will in a later
    /// stage be used to create all the structures containing the different
    /// generic types.
    /// This function also adds the names for the generics if they aren't already
    /// set and that information is attainable.
    fn store_generic_struct(&mut self, ty: &mut Ty) {
        // Do not create a "copy" of the actual structure type that contains the
        // generic declarations, should only create "copies" for the structures
        // that "implements" the generics.
        if ty.contains_generic() {
            return;
        }

        let ident = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if !generics.is_empty() {
                    inner_ty.to_string()
                } else {
                    return;
                }
            }

            Ty::Pointer(ty_box, ..) | Ty::Array(ty_box, ..) => {
                self.store_generic_struct(ty_box);
                return;
            }

            _ => return,
        };

        // TODO: Don't hardcode default id.
        let id = BlockInfo::DEFAULT_BLOCK_ID;

        // Set names of generics if they aren't set already.
        if let Err(err) = self.type_context.set_generic_names(ty, id) {
            self.errors.push(err);
            return;
        }

        match self.generic_structures.entry(ident) {
            Entry::Occupied(mut o) => {
                if !o.get().contains(ty) {
                    o.get_mut().push(ty.clone());
                }
            }
            Entry::Vacant(v) => {
                v.insert(vec![ty.clone()]);
            }
        }
    }
}

impl<'a, 'tctx> Visitor for GenericStructsCollector<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.store_generic_struct(ty);
    }
}
