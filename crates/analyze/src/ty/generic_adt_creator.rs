use std::sync::{Arc, RwLock};

use log::debug;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse_with_deep_copy, visitor::Visitor},
    ty::{
        replace::{replace_gen_impls, replace_self},
        solve::set_generic_names,
        substitution_sets::sub_sets_debug_print,
        to_string::to_string_type_id,
        ty::Ty,
        ty_env::TyEnv,
        type_id::TypeId,
    },
    BlockId,
};

use super::generic_replace::GenericsReplacer;

/// Remove any ADTs that contain generics. Add new ADTs that contain replaced
/// generics (static dispatch). The old blocks are set to "Empty".
/// This will done for the "implement" block and all its contained methods as well.
pub struct GenericAdtCreator {
    /// Contains types/ADTs that have generic placeholders. These ADTs should be
    /// removed and new ADTs/methods should be created that have implemented the generics.
    ///
    /// The key is the name of the ADT and the values are the unique types
    /// of the ADT with the generics implemented.
    generic_adts: TyEnvHashMap<LangPath, Vec<TypeId>>,

    errors: Vec<LangError>,
}

impl GenericAdtCreator {
    pub fn new(generic_adts: TyEnvHashMap<LangPath, Vec<TypeId>>) -> Self {
        debug!("generic_adts: {:#?}", generic_adts);

        Self {
            generic_adts,
            errors: Vec::default(),
        }
    }

    /// Creates new instance of the ADT with path `old_path`. This new instances
    /// will have the generics replaced with some implementation.
    ///
    /// The given `old_path` is the path to the ADT WITHOUT any generics. The
    /// `new_path` will represent the new ADTs which will contain generics.
    ///
    /// Returns the amount of new ADT blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly created ADTs.
    ///
    /// When the new ADTs have been created, the old one will be removed.
    /// It will removed even though no new ADTs are created.
    fn create_adt_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        old_path: &LangPath,
        body: &mut Vec<AstToken>,
        file_pos: &FilePosition,
        old_idx: usize,
        old_id: BlockId,
        parent_id: BlockId,
    ) -> LangResult<Option<usize>> {
        let generic_adt_tys_res = self
            .generic_adts
            .get(&ctx.ty_env.lock().unwrap(), DerefType::None, old_path)?
            .cloned();
        if let Some(generic_adt_tys) = generic_adt_tys_res {
            let adt = {
                let adt = ctx.ast_ctx.get_adt(&ctx.ty_env.lock().unwrap(), old_path)?;
                Arc::clone(&adt)
            };

            for gen_adt_type_id in &generic_adt_tys {
                set_generic_names(
                    &mut ctx.ty_env.lock().unwrap(),
                    &ctx.ast_ctx,
                    *gen_adt_type_id,
                )?;

                // Create a new instance of the ADT. This new instance will
                // replace all generic "placeholders" with the actual generics
                // implementations/instances.
                let mut new_adt = adt.as_ref().read().unwrap().clone();

                // The methods are RCs inside the "Adt" struct which means that
                // they are still tied to the "old" ADT. Empty the methods
                // map, it will be filled in later with a completly new implement
                // block that will be tied to this new ADT.
                new_adt.methods.clear();

                let gen_adt_ty =
                    if let Ok(gen_adt_ty) = ctx.ty_env.lock().unwrap().ty(*gen_adt_type_id) {
                        gen_adt_ty.clone()
                    } else {
                        panic!("{:?}", gen_adt_type_id);
                    };

                let mut gens = if let Ty::CompoundType(_, gens, ..) = &gen_adt_ty {
                    gens.clone()
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_adt_type_id
                    ));
                    return Err(err);
                };

                // Before creating the new ADT, make sure that the generics are
                // fully inferred.
                for gen_type_id in gens.iter_types_mut() {
                    let inf_type_id = ctx.ty_env.lock().unwrap().inferred_type(*gen_type_id)?;
                    if *gen_type_id != inf_type_id {
                        *gen_type_id = inf_type_id;
                    }
                }

                // Create the new path containing generics.
                let mut new_path = old_path.clone();
                let last_part = new_path.pop().unwrap();
                new_path = new_path.clone_push(&last_part.0, Some(&gens), old_path.file_pos);

                // TODO: Can this be done with a visitor/traverser? The current
                //       default traverser takes a AstToken, so would need to create
                //       a wrapper type around the ADT in that case.
                // For every member of the ADT, replace any generic types with
                // the type of the adt_init generics. Also replace any reference
                // to the old name with the new full name (containing generics).
                for member in &mut new_adt.members {
                    let mut new_member = member.as_ref().read().unwrap().clone();

                    if let Some(type_id) = &mut new_member.ty {
                        match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, *type_id, &gens) {
                            Ok(Some(new_type_id)) => *type_id = new_type_id,
                            Ok(None) => (),
                            Err(err) => self.errors.push(err),
                        }

                        match replace_self(
                            &mut ctx.ty_env.lock().unwrap(),
                            *type_id,
                            &old_path,
                            *gen_adt_type_id,
                        ) {
                            Ok(Some(new_type_id)) => *type_id = new_type_id,
                            Ok(None) => (),
                            Err(err) => self.errors.push(err),
                        }
                    }

                    *member = Arc::new(RwLock::new(new_member));
                }

                new_adt.generics = Some(gens);

                // Insert the new ADT variants that have replaced the generic
                // parameters with an actual real type. These new ADTs will be
                // inserted both into the AST and into the ADT lookup table
                // (in AnalyzeContext).
                debug!(
                    "Creating new generic ADT in block id {}, new_path: {:?} -- {:#?}",
                    parent_id, new_path, new_adt
                );

                let new_adt_rc = Arc::new(RwLock::new(new_adt));

                // Insert the new ADT into the lookup table.
                let key = (new_path, parent_id);
                ctx.ast_ctx.adts.insert(
                    &ctx.ty_env.lock().unwrap(),
                    DerefType::Deep,
                    key,
                    Arc::clone(&new_adt_rc),
                )?;

                // Create a new AST token that will be inserted
                // into the AST.
                let header = match adt.as_ref().read().unwrap().kind {
                    AdtKind::Struct => BlockHeader::Struct(Arc::clone(&new_adt_rc)),
                    AdtKind::Union => BlockHeader::Union(Arc::clone(&new_adt_rc)),
                    AdtKind::Enum | AdtKind::Unknown => {
                        panic!("Bad adt kind: {:?}", adt.as_ref().read().unwrap().kind)
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
            self.remove_adt_instance(
                &ctx.ty_env.lock().unwrap(),
                &mut ctx.ast_ctx,
                old_path,
                body,
                old_idx,
                parent_id,
            )?;

            Ok(Some(generic_adt_tys.len()))
        } else {
            Ok(None)
        }
    }

    /// Remove the old ADT containing the generics placeholders.
    /// Removed both from the AST and the look-up table.
    fn remove_adt_instance(
        &mut self,
        ty_env: &TyEnv,
        ast_ctx: &mut AstCtx,
        old_path: &LangPath,
        body: &mut Vec<AstToken>,
        old_idx: usize,
        parent_id: BlockId,
    ) -> LangResult<()> {
        let key = (old_path.clone(), parent_id);

        ast_ctx.adts.remove(ty_env, DerefType::None, &key)?;
        *body.get_mut(old_idx).expect("Known to be in bounds.") = AstToken::Empty;

        Ok(())
    }

    /// Returns the amount of new impl blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly implemented blocks.
    fn create_impl_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        body: &mut Vec<AstToken>,
        old_idx: usize,
        old_path: &LangPath,
        old_impl_token: &mut AstToken,
    ) -> LangResult<Option<usize>> {
        let generic_adt_tys_res = self
            .generic_adts
            .get(&ctx.ty_env.lock().unwrap(), DerefType::None, old_path)?
            .cloned();

        if let Some(generic_adt_tys) = generic_adt_tys_res {
            for (new_idx, gen_adt_type_id) in generic_adt_tys.iter().enumerate() {
                set_generic_names(
                    &mut ctx.ty_env.lock().unwrap(),
                    &ctx.ast_ctx,
                    *gen_adt_type_id,
                )?;

                let gen_adt_ty =
                    if let Ok(gen_adt_ty) = ctx.ty_env.lock().unwrap().ty(*gen_adt_type_id) {
                        gen_adt_ty.clone()
                    } else {
                        panic!("{:?}", gen_adt_type_id);
                    };

                let mut gens = if let Ty::CompoundType(_, gens, ..) = &gen_adt_ty {
                    gens.clone()
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "Generic instance type not compound: {:#?}",
                        gen_adt_type_id
                    ));
                    return Err(err);
                };

                // Before creating the new impl, make sure that the generics are
                // fully inferred.
                for gen_type_id in gens.iter_types_mut() {
                    let inf_type_id = ctx.ty_env.lock().unwrap().inferred_type(*gen_type_id)?;
                    if *gen_type_id != inf_type_id {
                        *gen_type_id = inf_type_id;
                    }
                }

                // Make a clone of the old implement block and change the name of
                // this new impl block to contain the generics.
                let mut new_impl_token = old_impl_token.clone();
                let (new_impl_body, new_path) = if let AstToken::Block(
                    BlockHeader::Implement(new_impl_path, ..),
                    ..,
                    new_impl_body,
                ) = &mut new_impl_token
                {
                    let new_path = old_path.with_gens(gens.clone());
                    *new_impl_path = new_path.clone();
                    (new_impl_body, new_path)
                } else {
                    unreachable!("TODO: Add err handling.");
                };

                // Get the new instance of the ADT that has had the generics implemented.
                let new_adt = {
                    let adt = ctx
                        .ast_ctx
                        .get_adt(&ctx.ty_env.lock().unwrap(), &new_path)?;
                    Arc::clone(&adt)
                };

                let mut generics_replacer = GenericsReplacer::new_adt(
                    Arc::clone(&new_adt),
                    &gens,
                    old_path,
                    *gen_adt_type_id,
                );

                // For every method of the ADT, replace any generic types with
                // the type of the generics instances. Also replace any reference
                // to the old name with the new full name (containing generics).
                //
                // This will be done for parameters, return types and bodies.
                // New instances will be created for all shared references (see
                // `set_deep_copy(true)` in `AstTraverser`).
                for method in new_impl_body {
                    if let AstToken::Block(BlockHeader::Fn(..), ..) = method {
                        if let Err(mut errs) =
                            traverse_with_deep_copy(ctx, &mut generics_replacer, method, new_idx)
                        {
                            self.errors.append(&mut errs);
                            continue;
                        }
                    }
                }

                if !self.errors.is_empty() {
                    return Ok(None);
                }

                // Slower to shift all the ast tokens to the right, but ensure
                // that the tokens are inserted next to the old "implement" and
                // doesn't ex. get added after the EOF token.
                body.insert(old_idx + new_idx + 1, new_impl_token);
            }

            // Remove the old, now unused, implement block.
            self.remove_impl_instance(body, old_idx);

            Ok(Some(generic_adt_tys.len()))
        } else {
            Ok(None)
        }
    }

    /// Remove the old implement block containing the generics placeholders.
    /// Removed both from the AST (TODO: and the look-up table).
    fn remove_impl_instance(&mut self, body: &mut Vec<AstToken>, old_idx: usize) {
        *body.get_mut(old_idx).expect("Known to be in bounds.") = AstToken::Empty;
    }
}

impl Visitor for GenericAdtCreator {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        let mut all_type_ids = ctx
            .ty_env
            .lock()
            .unwrap()
            .interner
            .all_types()
            .into_iter()
            .collect::<Vec<_>>();
        all_type_ids.sort_unstable();

        let mut all_types_string = String::new();
        for type_id in all_type_ids {
            all_types_string.push_str(&format!(
                "\ntype_id: {} - {:?}",
                type_id,
                to_string_type_id(&ctx.ty_env.lock().unwrap(), type_id)
            ));
        }

        debug!(
            "Generics creating done.\nforwards: {:#?}\nall types: {}\nsubs:",
            ctx.ty_env.lock().unwrap().forwards(),
            all_types_string
        );

        sub_sets_debug_print(&ctx.ty_env);
    }

    // TODO: Currently the assumption is that all ADTs are stored in the
    //       default block. For later, when this might not be the case, need
    //       to find another better way to do this. Will probably have to
    //       implement some helper functions to modify the AST, remove/add etc.
    /// Create new ADTs for generic implementations.
    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Default, _, parent_id, body) = &mut ast_token {
            let mut i = 0;

            // Iterate through all the ADTs in the AST.
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // Modify and create the new ADTs. The old ADT will also be removed.
                if let AstToken::Block(header, file_pos, old_id, ..) = &body_token {
                    match header {
                        BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                            let full_path = {
                                let module = match ctx.ast_ctx.get_module(*old_id) {
                                    Ok(Some(module)) => module,
                                    Ok(None) => LangPath::default(),
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                                let adt = adt.as_ref().read().unwrap();
                                module.clone_push(&adt.name, None, Some(adt.file_pos))
                            };

                            let contains_key = match self.generic_adts.contains_key(
                                &ctx.ty_env.lock().unwrap(),
                                DerefType::None,
                                &full_path,
                            ) {
                                Ok(contains_key) => contains_key,
                                Err(err) => {
                                    self.errors.push(err);
                                    return;
                                }
                            };

                            if contains_key {
                                let create_adt_instance_res = match self.create_adt_instance(
                                    ctx, &full_path, body, file_pos, i, *old_id, *parent_id,
                                ) {
                                    Ok(create_adt_instance_res) => create_adt_instance_res,
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                                if let Some(skip) = create_adt_instance_res {
                                    // Skip the newly created ADT blocks (if any).
                                    i += skip;
                                }
                            } else if adt
                                .as_ref()
                                .read()
                                .unwrap()
                                .generics
                                .as_ref()
                                .map(|gens| !gens.is_empty())
                                .unwrap_or(false)
                            {
                                // If the ADT contains generics but isn't in `self.generic_adts`,
                                // it means that the ADT isn't used anywhere and contains generics.
                                // Need to remove it.
                                if let Err(err) = self.remove_adt_instance(
                                    &ctx.ty_env.lock().unwrap(),
                                    &mut ctx.ast_ctx,
                                    &full_path,
                                    body,
                                    i,
                                    *parent_id,
                                ) {
                                    self.errors.push(err);
                                    return;
                                }
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
                if let AstToken::Block(BlockHeader::Implement(adt_path, _), _, old_id, _) =
                    &body_token
                {
                    let full_path = {
                        let module = match ctx.ast_ctx.get_module(*old_id) {
                            Ok(Some(module)) => module,
                            Ok(None) => LangPath::default(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let last_part = adt_path.last().unwrap();
                        module.clone_push(&last_part.0, None, adt_path.file_pos)
                    };

                    let contains_key = match self.generic_adts.contains_key(
                        &ctx.ty_env.lock().unwrap(),
                        DerefType::None,
                        &full_path,
                    ) {
                        Ok(contains_key) => contains_key,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if contains_key {
                        let create_method_instance_res = match self.create_impl_instance(
                            ctx,
                            body,
                            i,
                            &full_path,
                            &mut body_token,
                        ) {
                            Ok(create_method_instance_res) => create_method_instance_res,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        if let Some(skip) = create_method_instance_res {
                            // Skip the newly created impl blocks (if any).
                            i += skip;
                        }
                    } else {
                        // If this is a impl block for a ADT that has been
                        // removed, remove the impl block as well.
                        if ctx
                            .ast_ctx
                            .get_adt(&ctx.ty_env.lock().unwrap(), &full_path)
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
