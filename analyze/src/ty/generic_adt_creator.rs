use super::{context::TypeContext, generic_replace::GenericsReplacer};
use crate::block::BlockInfo;
use common::{
    error::LangError,
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader},
    },
    traverser::AstTraverser,
    traverser::TraverseContext,
    ty::ty::Ty,
    util,
    visitor::Visitor,
    BlockId,
};
use log::debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub struct GenericAdtCreator<'a, 'tctx> {
    /// Needed to look up ADTs and types.
    type_context: &'a mut TypeContext<'tctx>,

    /// Contains types/ADTs that have generic placeholders. These ADTs should be
    /// removed and new ADTs/methods should be created that have implemented the generics.
    ///
    /// The key is the name of the ADT and the values are the unique types
    /// of the ADT with the generics implemented.
    generic_adts: HashMap<String, Vec<Ty>>,

    errors: Vec<LangError>,
}

/// Remove any ADTs that contain generics. Add new ADTs that contain replaced
/// generics (static dispatch). The old blocks are set to "Empty".
/// This will done for the "implement" block and all its contained methods as well.
impl<'a, 'tctx> GenericAdtCreator<'a, 'tctx> {
    pub fn new(
        type_context: &'a mut TypeContext<'tctx>,
        generic_adts: HashMap<String, Vec<Ty>>,
    ) -> Self {
        debug!("generic_adts: {:#?}", generic_adts);

        Self {
            type_context,
            generic_adts,
            errors: Vec::default(),
        }
    }

    /// Creates new instance of the ADT with name `old_name`. These new instances
    /// will have the generics replaced with some implementation.
    /// Returns the amount of new ADT blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly created ADTs.
    ///
    /// When the new ADTs have been created, the old one will be removed.
    /// It will removed even though no new ADTs are created.
    fn create_adt_instance(
        &mut self,
        old_name: &str,
        body: &mut Vec<AstToken>,
        file_pos: &FilePosition,
        old_idx: usize,
        old_id: BlockId,
        parent_id: BlockId,
    ) -> Option<usize> {
        if let Some(generic_adt_tys) = self.generic_adts.get(old_name).cloned() {
            // TODO: Do not hardcode default block ID, get ID from somewhere else.
            let id = BlockInfo::DEFAULT_BLOCK_ID;

            let adt = match self.type_context.analyze_context.get_adt(old_name, id) {
                Ok(adt) => Rc::clone(&adt),
                Err(err) => {
                    self.errors.push(err);
                    return None;
                }
            };

            for gen_adt_ty in &generic_adt_tys {
                // Create a new instance of the ADT. This new instance will
                // replace all generic "placeholders" with the actual generics
                // implementations/instances.
                let mut new_adt = adt.borrow().clone();

                // The methods are RCs inside the "Adt" struct which means that
                // they are still tied to the "old" ADT. Empty the methods
                // map, it will be filled in later with a completly new implement
                // block that will be tied to this new ADT.
                new_adt.methods.clear();

                let generics = if let Ty::CompoundType(_, generics, ..) = gen_adt_ty {
                    generics.clone()
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_adt_ty
                    ));
                    self.errors.push(err);
                    return None;
                };

                let new_name = util::to_generic_name(&new_adt.name, &generics);

                // Give the new copy of the ADT type the "full name" containing
                // information about the generic arguments.
                new_adt.name = new_name.clone();

                // TODO: Can this be done with a visitor/traverser? The current
                //       default traverser takes a AstToken, so would need to create
                //       a wrapper type around the ADT in that case.
                // For every member of the ADT, replace any generic types with
                // the type of the adt_init generics. Also replace any reference
                // to the old name with the new full name (containing generics).
                for member in &mut new_adt.members {
                    let mut new_member = member.borrow().clone();

                    if let Some(ty) = &mut new_member.ty {
                        ty.replace_generics_impl(&generics);
                        ty.replace_self(&old_name, gen_adt_ty);
                    }

                    *member = Rc::new(RefCell::new(new_member));
                }

                // Insert the new ADT variants that have replaced the generic
                // parameters with an actual real type. These new ADTs will be
                // inserted both into the AST and into the ADT lookup table
                // (in AnalyzeContext).
                debug!(
                    "Creating new generic ADT in block id {}: {:#?}",
                    parent_id, new_adt
                );

                let new_adt_rc = Rc::new(RefCell::new(new_adt));

                // Insert the new ADT into the lookup table.
                let key = (new_name.clone(), parent_id);
                self.type_context
                    .analyze_context
                    .adts
                    .insert(key, Rc::clone(&new_adt_rc));

                // Create a new AST token that will be inserted
                // into the AST.
                let header = match adt.borrow().kind {
                    AdtKind::Struct => BlockHeader::Struct(Rc::clone(&new_adt_rc)),
                    AdtKind::Union => BlockHeader::Union(Rc::clone(&new_adt_rc)),
                    AdtKind::Enum | AdtKind::Unknown => {
                        panic!("Bad adt kind: {:?}", adt.borrow().kind)
                    }
                };
                let adt_body = Vec::with_capacity(0);

                // Slower to shift all the ast tokens to the right, but ensures that
                // the tokens are inserted next to the old ADT and doesn't ex. get
                // added after the EOF token.
                body.insert(
                    old_idx + 1,
                    AstToken::Block(header, file_pos.to_owned(), old_id, adt_body),
                );
            }

            // Remove the old, now unused, ADT.
            self.remove_adt_instance(old_name, body, old_idx, parent_id);

            Some(generic_adt_tys.len())
        } else {
            None
        }
    }

    /// Remove the old ADT containing the generics placeholders.
    /// Removed both from the AST and the look-up table.
    fn remove_adt_instance(
        &mut self,
        old_name: &str,
        body: &mut Vec<AstToken>,
        old_idx: usize,
        parent_id: BlockId,
    ) {
        let key = (old_name.into(), parent_id);
        self.type_context.analyze_context.adts.remove(&key);

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
        if let Some(generic_adt_tys) = self.generic_adts.get(old_name).cloned() {
            for (new_idx, gen_adt_ty) in generic_adt_tys.iter().enumerate() {
                let generics = if let Ty::CompoundType(_, generics, ..) = &gen_adt_ty {
                    generics.clone()
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_adt_ty
                    ));
                    self.errors.push(err);
                    return None;
                };

                // Make a clone of the old implement block and change the name of
                // this new impl block to contain the generics.
                let mut new_impl_token = old_impl_token.clone();
                let (new_impl_body, new_name) =
                    if let AstToken::Block(BlockHeader::Implement(ident, _), .., new_impl_body) =
                        &mut new_impl_token
                    {
                        *ident = util::to_generic_name(&old_name, &generics);
                        (new_impl_body, ident.clone())
                    } else {
                        unreachable!("TODO: Add err handling.");
                    };

                // TODO: Do not hardcode default block ID, get ID from somewhere else.
                let id = BlockInfo::DEFAULT_BLOCK_ID;

                // Get the new instance of the ADT that has had the generics implemented.
                let new_adt = match self.type_context.analyze_context.get_adt(&new_name, id) {
                    Ok(new_adt) => Rc::clone(&new_adt),
                    Err(err) => {
                        self.errors.push(err);
                        return None;
                    }
                };

                let mut generics_replacer = GenericsReplacer::new_adt(
                    &mut self.type_context,
                    Rc::clone(&new_adt),
                    &generics,
                    old_name,
                    gen_adt_ty,
                );

                let mut traverser = AstTraverser::new();
                traverser
                    .add_visitor(&mut generics_replacer)
                    .set_deep_copy(true);

                // For every method of the ADT, replace any generic types with
                // the type of the generics instances. Also replace any reference
                // to the old name with the new full name (containing generics).
                //
                // This will be done for parameters, return types and bodies.
                // New instances will be created for all shared references (see
                // `set_deep_copy(true)` in `AstTraverser`).
                for mut method in new_impl_body {
                    if let AstToken::Block(BlockHeader::Function(..), ..) = method {
                        traverser
                            .set_deep_copy_nr(new_idx)
                            .traverse_token(&mut method);
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

            Some(generic_adt_tys.len())
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

impl<'a, 'tctx> Visitor for GenericAdtCreator<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: Currently the assumption is that all ADTs are stored in the
    //       default block. For later, when this might not be the case, need
    //       to find another better way to do this. Will probably have to
    //       implement some helper functions to modify the AST, remove/add etc.
    /// Create new ADTs for generic implementations.
    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Default, _, parent_id, body) = &mut ast_token {
            let mut i = 0;

            // Iterate through all the ADTs in the AST.
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // TODO: Implemement for other types other than struct as well.
                // Modify and create the new ADTs. The old ADT will also be removed.
                if let AstToken::Block(header, file_pos, old_id, ..) = &body_token {
                    match header {
                        BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                            if self.generic_adts.contains_key(&adt.borrow().name) {
                                if let Some(skip) = self.create_adt_instance(
                                    &adt.borrow().name.clone(),
                                    body,
                                    file_pos,
                                    i,
                                    *old_id,
                                    *parent_id,
                                ) {
                                    // Skip the newly created ADT blocks (if any).
                                    i += skip;
                                }
                            } else if adt
                                .borrow()
                                .generics
                                .as_ref()
                                .map(|gens| !gens.is_empty())
                                .unwrap_or(false)
                            {
                                // If the ADT contains generics but isn't in `self.generic_adts`,
                                // it means that the ADT isn't used anywhere and contains generics.
                                // Need to remove if from the AST.
                                self.remove_adt_instance(
                                    &adt.borrow().name.clone(),
                                    body,
                                    i,
                                    *parent_id,
                                );
                            }
                        }

                        _ => (),
                    }
                }

                i += 1;
            }

            i = 0;

            // Iterate through all the implement blocks in the AST.
            while i < body.len() {
                let mut body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // Modify and create new implement block for the ADTs that contains
                // generics. These new impl blocks will contain the actual generic
                // implementations/instances. The old impl blocks containing the
                // generic placeholders will be removed.
                if let AstToken::Block(BlockHeader::Implement(adt_name, _), ..) = &body_token {
                    let adt_name = adt_name.clone();

                    if self.generic_adts.contains_key(&adt_name) {
                        if let Some(skip) =
                            self.create_method_instance(body, i, &adt_name, &mut body_token)
                        {
                            // Skip the newly created impl blocks (if any).
                            i += skip;
                        }
                    } else {
                        // TODO: Don't hardcode default id.
                        let id = BlockInfo::DEFAULT_BLOCK_ID;

                        // TODO: Union.
                        // If this is a impl block for a ADT that has been
                        // removed, remove the impl block as well.
                        if self
                            .type_context
                            .analyze_context
                            .get_adt(&adt_name, id)
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
}
