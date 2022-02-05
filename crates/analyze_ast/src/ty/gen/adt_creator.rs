use std::sync::Arc;

use log::{debug, log_enabled, Level};

use common::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    error::{LangError, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_map::TyEnvHashMap,
    hash_set::TyEnvHashSet,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{AdtKind, Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse_with_deep_copy, visitor::Visitor},
    ty::{
        get::get_gens, substitution_sets::sub_sets_debug_print, to_string::to_string_type_id,
        ty_env::TyEnv, type_id::TypeId,
    },
    BlockId,
};

use crate::ty::{gen::replace::GenericsReplacer, solve::set_generic_names};

// TODO: Must clean-up and simplify this logic. Can for example probably merge
//       a lot of shared logic between creating ADT and impl blocks.
//       Must also simplify all the removal/adding/replacing logic, very hard to
//       follow what is going on.

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
    /// The methods stored inside the ADT body will also be cloned and have
    /// their generics implemented.
    ///
    /// The given `adt_path_without_gens` is the path to the ADT without any
    /// generics. The `new_path` will represent the new ADTs which will contain
    /// generics.
    ///
    /// Returns the amount of new ADT blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly created ADTs.
    ///
    /// When the new ADTs have been created, the old one will be removed.
    /// It will removed even if no new ADTs are created.
    fn create_adt_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path_without_gens: &LangPath,
        default_body: &mut Vec<AstToken>,
        adt_body: &mut Vec<AstToken>,
        file_pos: &FilePosition,
        old_idx: usize,
        old_id: BlockId,
        parent_id: BlockId,
    ) -> LangResult<Option<usize>> {
        let generic_adt_tys_res = self
            .generic_adts
            .get(&ctx.ty_env.lock(), DerefType::None, adt_path_without_gens)?
            .cloned();

        if let Some(generic_adt_tys) = generic_adt_tys_res {
            let adt = {
                let adt = ctx
                    .ast_ctx
                    .get_adt(&ctx.ty_env.lock(), adt_path_without_gens)?;
                Arc::clone(&adt)
            };

            for (new_idx, gen_adt_type_id) in generic_adt_tys.iter().enumerate() {
                set_generic_names(&mut ctx.ty_env.lock(), ctx.ast_ctx, *gen_adt_type_id)?;

                let mut gens = if let Some(gens) = get_gens(&ctx.ty_env.lock(), *gen_adt_type_id)? {
                    gens.clone()
                } else {
                    return Err(ctx.ast_ctx.err(format!(
                        "Expected type with id {} to contain generics.",
                        gen_adt_type_id
                    )));
                };

                // Before creating the new ADT, make sure that the generics are
                // fully inferred.
                for gen_type_id in gens.iter_types_mut() {
                    let inf_type_id = ctx.ty_env.lock().inferred_type(*gen_type_id)?;
                    if *gen_type_id != inf_type_id {
                        *gen_type_id = inf_type_id;
                    }
                }

                let adt_kind = adt.read().kind;
                let header = match adt_kind {
                    AdtKind::Struct => BlockHeader::Struct(Arc::clone(&adt)),
                    AdtKind::Union => BlockHeader::Union(Arc::clone(&adt)),
                    AdtKind::Enum | AdtKind::Tuple | AdtKind::Unknown => {
                        panic!("Bad adt kind: {:?}", adt.read().kind)
                    }
                };

                let mut adt_block = AstToken::Block(Block {
                    header,
                    body: adt_body.clone(),
                    id: old_id,
                    file_pos: file_pos.to_owned(),
                });

                let mut generics_replacer =
                    GenericsReplacer::new_adt(&gens, adt_path_without_gens, *gen_adt_type_id);

                if let Err(mut errs) =
                    traverse_with_deep_copy(ctx, &mut generics_replacer, &mut adt_block, new_idx)
                {
                    self.errors.append(&mut errs);
                    continue;
                }

                // Slower to shift all the ast tokens to the right, but ensures that
                // the tokens are inserted next to the old ADT and doesn't ex. get
                // added after the EOF token.
                default_body.insert(old_idx + 1, adt_block);
            }

            // Remove the old, now unused, ADT.
            self.remove_adt_instance(
                &ctx.ty_env.lock(),
                &mut ctx.ast_ctx,
                adt_path_without_gens,
                default_body,
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
        // TODO: Do I need to set it to empty? Can it not just be removed from
        //       the `body` vector?
        *body.get_mut(old_idx).expect("Known to be in bounds.") = AstToken::Empty;

        Ok(())
    }

    /// Returns the amount of new impl blocks. Since this function will most
    /// likely be called from a loop iterating through the AstTokens, the returned
    /// value can be used to skip over the newly implemented blocks.
    fn create_impl_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        default_body: &mut Vec<AstToken>,
        old_idx: usize,
        adt_path_without_gens: &LangPath,
        old_impl_token: &AstToken,
    ) -> LangResult<Option<usize>> {
        let generic_adt_tys_res = self
            .generic_adts
            .get(&ctx.ty_env.lock(), DerefType::None, adt_path_without_gens)?
            .cloned();

        if let Some(generic_adt_tys) = generic_adt_tys_res {
            for (new_idx, gen_adt_type_id) in generic_adt_tys.iter().enumerate() {
                set_generic_names(&mut ctx.ty_env.lock(), ctx.ast_ctx, *gen_adt_type_id)?;

                let mut gens = if let Some(gens) = get_gens(&ctx.ty_env.lock(), *gen_adt_type_id)? {
                    gens.clone()
                } else {
                    return Err(ctx.ast_ctx.err(format!(
                        "Expected type with id {} to contain generics.",
                        gen_adt_type_id
                    )));
                };

                // Before creating the new impl, make sure that the generics are
                // fully inferred.
                for gen_type_id in gens.iter_types_mut() {
                    let inf_type_id = ctx.ty_env.lock().inferred_type(*gen_type_id)?;
                    if *gen_type_id != inf_type_id {
                        *gen_type_id = inf_type_id;
                    }
                }

                // Make a clone of the old implement block and change the name of
                // this new impl block to contain the generics.
                let mut new_impl_token = old_impl_token.clone();
                let (new_impl_body, new_path) = if let AstToken::Block(Block {
                    header: BlockHeader::Implement(new_impl_path, ..),
                    body: new_impl_body,
                    ..
                }) = &mut new_impl_token
                {
                    let new_path = adt_path_without_gens.with_gens(gens.clone());
                    *new_impl_path = new_path.clone();
                    (new_impl_body, new_path)
                } else {
                    unreachable!("TODO: Add err handling.");
                };

                // Get the new instance of the ADT that has had the generics implemented.
                let new_adt = {
                    let adt = ctx.ast_ctx.get_adt(&ctx.ty_env.lock(), &new_path)?;
                    Arc::clone(&adt)
                };

                let mut generics_replacer =
                    GenericsReplacer::new_adt(&gens, adt_path_without_gens, *gen_adt_type_id);
                generics_replacer.new_adt = Some(Arc::clone(&new_adt));

                // For every method of the impl, replace any generic types with
                // the type of the generics instances. Also replace any reference
                // to the old name with the new full name (containing generics).
                //
                // This will be done for parameters, return types and bodies.
                // New instances will be created for all shared references (see
                // `set_deep_copy(true)` in `AstTraverser`).
                for method in new_impl_body {
                    if let AstToken::Block(Block {
                        header: BlockHeader::Fn(..),
                        ..
                    }) = method
                    {
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
                default_body.insert(old_idx + new_idx + 1, new_impl_token);
            }

            // Remove the old, now unused, implement block.
            self.remove_impl_instance(default_body, old_idx);

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
        if log_enabled!(Level::Debug) {
            let mut all_type_ids = ctx
                .ty_env
                .lock()
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
                    to_string_type_id(&ctx.ty_env.lock(), type_id)
                ));
            }

            let ty_env_guard = ctx.ty_env.lock();
            debug!(
                "Generics creating done.\nforwards: {:#?}\nall types: {}\nsubs: {}",
                ty_env_guard.forwards(),
                all_types_string,
                sub_sets_debug_print(&ty_env_guard)
            );
        }
    }

    // TODO: Currently the assumption is that all ADTs are stored in the
    //       default block. For later, when this might not be the case, need
    //       to find another better way to do this. Will probably have to
    //       implement some helper functions to modify the AST, remove/add etc.
    /// Create new ADTs for generic implementations.
    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let default_body = if let Block {
            header: BlockHeader::Default,
            body: default_body,
            ..
        } = block
        {
            default_body
        } else {
            unreachable!();
        };

        // This set will store paths of ADTs that have been removed. These are
        // ADTs with generics that aren't used anywhere in the code.
        // They will be stored in a set since we also need to remove their
        // corresponding impl blocks. And since the impl blocks are traversed
        // afterwards, this set will be used to see which impls to remove.
        let mut unused_adts = TyEnvHashSet::default();

        let mut i = 0;

        // Iterate through all ADTs in the AST. Create new ADTs for ADTs with
        // generics, where the generics have their "real" types implemented.
        while i < default_body.len() {
            let mut body_token = default_body.get(i).cloned().unwrap();

            if let AstToken::Block(Block {
                header,
                body: child_body,
                id: child_id,
                file_pos: child_file_pos,
                ..
            }) = &mut body_token
            {
                let adt_path_without_gens = match header {
                    BlockHeader::Struct(adt) | BlockHeader::Union(adt)
                        if adt.read().name != "Tuple" =>
                    {
                        let adt = adt.read();
                        adt.module.clone_push(&adt.name, None, Some(adt.file_pos))
                    }
                    _ => {
                        i += 1;
                        continue;
                    }
                };

                let amount_of_adts_created = match self.create_adt_instance(
                    ctx,
                    &adt_path_without_gens,
                    default_body,
                    child_body,
                    child_file_pos,
                    i,
                    *child_id,
                    BlockCtx::DEFAULT_BLOCK_ID,
                ) {
                    Ok(Some(amount)) => amount,
                    Ok(None) => 0,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                i += amount_of_adts_created;

                // If true: No new ADT blocks were created. If the ADT has generics,
                // but no new ADT blocks were created, it means that the current
                // ADT isn't used anywhere in the code.
                // In this case the ADT must be removed since its generics are
                // never inferred/implemented to any type.
                if amount_of_adts_created == 0 {
                    let ty_env_guard = ctx.ty_env.lock();
                    let adt = match ctx.ast_ctx.get_adt(&ty_env_guard, &adt_path_without_gens) {
                        Ok(adt) => adt,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    let has_gens = adt
                        .read()
                        .generics
                        .as_ref()
                        .map(|gens| !gens.is_empty())
                        .unwrap_or(false);

                    if has_gens {
                        if let Err(err) = self.remove_adt_instance(
                            &ty_env_guard,
                            &mut ctx.ast_ctx,
                            &adt_path_without_gens,
                            default_body,
                            i,
                            BlockCtx::DEFAULT_BLOCK_ID,
                        ) {
                            self.errors.push(err);
                            return;
                        }

                        if let Err(err) = unused_adts.insert(
                            &ty_env_guard,
                            DerefType::None,
                            adt_path_without_gens,
                        ) {
                            self.errors.push(err);
                            return;
                        }
                    }
                }
            }

            i += 1;
        }

        i = 0;

        // TODO: Possible to merge the two loops in some way (ADT+impl)?
        //       Currently the ADT need to be created first since the impl block
        //       logic uses the created ADTs to look-up replace paths etc.
        //       correctly.
        // Iterate through all impl blocks in the AST. This logic is very similar
        // to the loop above where we iterate through all ADTs and create new
        // copies of them. But instead we iterate through the impl blocks and
        // creates copies of them with the generics implemented.
        while i < default_body.len() {
            let body_token = default_body.get(i).cloned().unwrap();

            if let AstToken::Block(Block {
                header: BlockHeader::Implement(adt_path, ..),
                ..
            }) = &body_token
            {
                let adt_path_without_gens = adt_path.without_gens();

                // If the impl is stored in `unused_adts`, it is a impl block for
                // a ADT containing generics that never was used in the code.
                // The ADT have already been removed, remove the impl block as well.
                match unused_adts.contains(
                    &ctx.ty_env.lock(),
                    DerefType::None,
                    &adt_path_without_gens,
                ) {
                    Ok(true) => self.remove_impl_instance(default_body, i),
                    Ok(false) => (),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let amount_of_impls_created = match self.create_impl_instance(
                    ctx,
                    default_body,
                    i,
                    &adt_path_without_gens,
                    &body_token,
                ) {
                    Ok(Some(amount)) => amount,
                    Ok(None) => 0,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                i += amount_of_impls_created;
            }

            i += 1;
        }
    }
}
