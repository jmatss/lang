use common::{
    error::LangError,
    file::FilePosition,
    token::{ast::AstToken, block::BlockHeader},
    traverser::AstTraverser,
    traverser::TraverseContext,
    ty::ty::Ty,
    util,
    visitor::Visitor,
    BlockId,
};
use log::debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{AnalyzeContext, BlockInfo};

use super::generic_replace::GenericsReplacer;

pub struct TypeConverter<'a> {
    /// Needed to look up structures.
    analyze_context: &'a mut AnalyzeContext,

    /// Contains types/structures that have generic placeholders. These structures
    /// should be removed and new structure/methods should be created that have
    /// implemented the generics.
    ///
    /// The key is the name of the structure and the values are the unique types
    /// of the structure with the generics implemented.
    generic_structures: HashMap<String, Vec<Ty>>,

    errors: Vec<LangError>,
}

/// Remove any structs/interfaces that contain generics. Add new struct/interfaces
/// that contain replaced generics (static dispatch). The old blocks are set to
/// "Empty". This will done for the "implement" block and all its contained
/// methods as well.
impl<'a> TypeConverter<'a> {
    pub fn new(
        analyze_context: &'a mut AnalyzeContext,
        mut generic_structures: HashMap<String, Vec<Ty>>,
    ) -> Self {
        // TODO: Are there any other way to do this in a better way?
        // Removes the `generic_structures` that contains generics themselves.
        // This can happen for example if a structure with a generic `T` has
        // a method that creates a new instance of itself with the generic `T`
        // as a generic impl.
        for generic_structure in generic_structures.values_mut() {
            *generic_structure = generic_structure
                .iter()
                .filter_map(|ty| {
                    if ty.contains_generic() {
                        None
                    } else {
                        Some(ty.clone())
                    }
                })
                .collect::<Vec<_>>();
        }

        debug!("generic_structures: {:#?}", generic_structures);

        Self {
            analyze_context,
            generic_structures,
            errors: Vec::default(),
        }
    }

    /// Creates new instance of the structure with name `old_name`. These new
    /// instances will have the generics replaced with some implementation.
    /// Returns the amount of new structure blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly created structures.
    ///
    /// When the new structs have been created, the old one will be removed.
    /// It will removed even though no new structs are created.
    fn create_structure_instance(
        &mut self,
        old_name: &str,
        body: &mut Vec<AstToken>,
        file_pos: &FilePosition,
        old_idx: usize,
        old_id: BlockId,
        parent_id: BlockId,
    ) -> Option<usize> {
        if let Some(generic_structure_tys) = self.generic_structures.get(old_name).cloned() {
            // TODO: Do not hardcode default block ID, get ID from somewhere else.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            // TODO: Implement for types other than structs.

            let struct_ = match self.analyze_context.get_struct(old_name, id) {
                Ok(struct_) => Rc::clone(&struct_),
                Err(err) => {
                    self.errors.push(err);
                    return None;
                }
            };

            for gen_structure_ty in &generic_structure_tys {
                // Create a new instance of the structure. This new instance will
                // replace all generic "placeholders" with the actual generics
                // implementations/instanaces.
                let mut new_struct = struct_.borrow().clone();

                // The methods are RCs inside the "Struct" struct which means that
                // they are still tied to the "old" struct. Empty the methods
                // map, it will be filled in later with a completly new implement
                // block that will be tied to this new structure.
                if let Some(methods) = &mut new_struct.methods {
                    methods.clear();
                }

                let generics = if let Ty::CompoundType(.., generics) = gen_structure_ty {
                    generics.clone()
                } else {
                    let err = self.analyze_context.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_structure_ty
                    ));
                    self.errors.push(err);
                    return None;
                };

                let new_name = util::to_generic_struct_name(&new_struct.name, &generics);

                // Give the new copy of the struct type the "full name" containing
                // information about the generic arguments.
                new_struct.name = new_name.clone();

                // TODO: Can this be done with a visitor/traverser? The current
                //       default traverser takes a AstToken, so would need to create
                //       a wrapper type around the struct in that case.
                // For every member of the struct, replace any generic types with
                // the type of the struct_init generics. Also replace any reference
                // to the old name with the new full name (containing generics).
                if let Some(members) = &mut new_struct.members {
                    for member in members {
                        let mut new_member = member.borrow().clone();

                        if let Some(ty) = &mut new_member.ty {
                            ty.replace_generics_impl(&generics);
                            ty.replace_self(&old_name, gen_structure_ty);
                        }

                        *member = Rc::new(RefCell::new(new_member));
                    }
                }

                // Insert the new struct variants that have replaced the generic
                // parameters with an actual real type. These new struct will be
                // inserted both into the AST and into the struct lookup table
                // (in AnalyzeContext).
                debug!(
                    "Creating new generic struct in block id {}: {:#?}",
                    parent_id, new_struct
                );

                let new_struct_rc = Rc::new(RefCell::new(new_struct));

                // Insert the new struct into the lookup table.
                let key = (new_name.clone(), parent_id);
                self.analyze_context
                    .structs
                    .insert(key, Rc::clone(&new_struct_rc));

                // Create a new AST token that will be inserted
                // into the AST.
                let header = BlockHeader::Struct(Rc::clone(&new_struct_rc));
                let struct_body = Vec::with_capacity(0);

                // Slower to shift all the ast tokens to the
                // right, but ensure that the tokens are
                // inserted next to the old struct and
                // doesn't ex. get added after the EOF token.
                body.insert(
                    old_idx + 1,
                    AstToken::Block(header, file_pos.to_owned(), old_id, struct_body),
                );
            }

            // Remove the old, now unused, structure.
            self.remove_structure_instance(old_name, body, old_idx, parent_id);

            Some(generic_structure_tys.len())
        } else {
            None
        }
    }

    /// Remove the old structure containing the generics placeholders.
    /// Removed both from the AST and the look-up table.
    fn remove_structure_instance(
        &mut self,
        old_name: &str,
        body: &mut Vec<AstToken>,
        old_idx: usize,
        parent_id: BlockId,
    ) {
        let key = (old_name.into(), parent_id);
        self.analyze_context.structs.remove(&key);

        *body.get_mut(old_idx).expect("Known to be in bounds.") = AstToken::Empty;
    }

    /// Returns the amount of new impl blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly implemented blocks.
    fn create_method_instance(
        &mut self,
        body: &mut Vec<AstToken>,
        old_idx: usize,
        old_name: &str,
        old_impl_token: &mut AstToken,
    ) -> Option<usize> {
        if let Some(generic_structure_tys) = self.generic_structures.get(old_name).cloned() {
            // TODO: Implement for types other than structs.

            for (new_idx, gen_structure_ty) in generic_structure_tys.iter().enumerate() {
                let generics = if let Ty::CompoundType(.., generics) = &gen_structure_ty {
                    generics.clone()
                } else {
                    let err = self.analyze_context.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_structure_ty
                    ));
                    self.errors.push(err);
                    return None;
                };

                // Make a clone of the old implement block and change the name of
                // this new impl block to contain the generics.
                let mut new_impl_token = old_impl_token.clone();
                let (new_impl_body, new_name) =
                    if let AstToken::Block(BlockHeader::Implement(ident), .., new_impl_body) =
                        &mut new_impl_token
                    {
                        *ident = util::to_generic_struct_name(&old_name, &generics);
                        (new_impl_body, ident.clone())
                    } else {
                        unreachable!("TODO: Add err handling.");
                    };

                // TODO: Do not hardcode default block ID, get ID from somewhere else.
                let id = BlockInfo::DEFAULT_BLOCK_ID;

                // Get the new instance of the structure that has had the generics
                // implemented.
                let new_struct = match self.analyze_context.get_struct(&new_name, id) {
                    Ok(new_struct) => Rc::clone(&new_struct),
                    Err(err) => {
                        self.errors.push(err);
                        return None;
                    }
                };

                let mut generics_replacer = GenericsReplacer::new(
                    &mut self.analyze_context,
                    Rc::clone(&new_struct),
                    &generics,
                    old_name,
                    gen_structure_ty,
                );

                let mut traverser = AstTraverser::new();
                traverser
                    .add_visitor(&mut generics_replacer)
                    .set_deep_copy(true);

                // For every method of the struct, replace any generic types with
                // the type of the generics instances. Also replace any reference
                // to the old name with the new full name (containing generics).
                //
                // This will be done for parameters, return types and bodies.
                // New instances will be created for all shared references (see
                // `set_deep_copy(true)` in `AstTraverser`).
                for method in new_impl_body {
                    if let AstToken::Block(BlockHeader::Function(..), ..) = method {
                        traverser.set_deep_copy_nr(new_idx).traverse_token(method);
                    }
                }

                if let Err(mut err) = traverser.take_errors() {
                    self.errors.append(&mut err);
                    return None;
                }

                // Slower to shift all the ast tokens to the right, but ensure
                // that the tokens are inserted next to the old "implement" and
                // doesn't ex. get added after the EOF token.
                body.insert(old_idx + new_idx + 1, new_impl_token);
            }

            // Remove the old, now unused, implement block.
            self.remove_impl_instance(body, old_idx);

            Some(generic_structure_tys.len())
        } else {
            None
        }
    }

    /// Remove the old implement block containing the generics placeholders.
    /// Removed both from the AST (TODO: and the look-up table).
    fn remove_impl_instance(&mut self, body: &mut Vec<AstToken>, old_idx: usize) {
        *body.get_mut(old_idx).expect("Known to be in bounds.") = AstToken::Empty;
    }
}

impl<'a> Visitor for TypeConverter<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: Currently the assumption is that all structs are stored in the
    //       default block. For later, when this might not be the case, need
    //       to find another better way to do this. Will probably have to
    //       implement some helper functions to modify the AST, remove/add etc.
    /// Create new structs for generic implementations.
    ///
    /// OBS! This needs to be ran first, before
    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Default, _, parent_id, body) = &mut ast_token {
            let mut i = 0;

            // Iterate through all the structures in the AST.
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // TODO: Implement for other types other than struct (enum/interface).

                // Modify and create the new structs. The old struct will also
                // be removed.
                if let AstToken::Block(BlockHeader::Struct(struct_), file_pos, old_id, ..) =
                    &body_token
                {
                    if self.generic_structures.contains_key(&struct_.borrow().name) {
                        if let Some(skip) = self.create_structure_instance(
                            &struct_.borrow().name.clone(),
                            body,
                            file_pos,
                            i,
                            *old_id,
                            *parent_id,
                        ) {
                            // Skip the newly created structure blocks (if any).
                            i += skip;
                        }
                    } else if struct_
                        .borrow()
                        .generics
                        .as_ref()
                        .map(|gens| !gens.is_empty())
                        .unwrap_or(false)
                    {
                        // If the structure contains generics but isn't in
                        // `self.generic_structures`, it means that the structure
                        // isn't used anywhere and contains generics. Need to remove
                        // if from the AST:
                        self.remove_structure_instance(
                            &struct_.borrow().name.clone(),
                            body,
                            i,
                            *parent_id,
                        );
                    }
                }

                i += 1;
            }

            i = 0;

            // Iterate through all the implement blocks in the AST.
            while i < body.len() {
                let mut body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // Modify and create new implement block for the structures that
                // contains generics. These new impl blocks will contain the actual
                // generic implementations/instances. The old impl blocks containing
                // the generic placeholders will be removed.
                if let AstToken::Block(BlockHeader::Implement(structure_name), ..) = &body_token {
                    let structure_name = structure_name.clone();

                    if self.generic_structures.contains_key(&structure_name) {
                        if let Some(skip) =
                            self.create_method_instance(body, i, &structure_name, &mut body_token)
                        {
                            // Skip the newly created impl blocks (if any).
                            i += skip;
                        }
                    } else {
                        // TODO: Don't hardcode default id.
                        let id = BlockInfo::DEFAULT_BLOCK_ID;

                        // If this is a impl block for a structure that has been
                        // removed, remove the impl block as well.
                        if self
                            .analyze_context
                            .get_struct(&structure_name, id)
                            .is_err()
                            && self.analyze_context.get_enum(&structure_name, id).is_err()
                            && self
                                .analyze_context
                                .get_interface(&structure_name, id)
                                .is_err()
                        {
                            self.remove_impl_instance(body, i);
                        }
                    }
                }

                i += 1;
            }
        }
    }

    // TODO: Implement similar generic logic as for structs.
    fn visit_interface(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
    fn visit_enum(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
}
