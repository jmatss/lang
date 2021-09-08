use std::{collections::HashSet, sync::Arc};

use parking_lot::RwLock;

use common::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    error::{LangError, LangResult},
    hash::DerefType,
    path::LangPath,
    token::{
        block::{Adt, Block, BlockHeader},
        expr::Var,
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        generics::Generics,
        is::is_solved,
        replace::{replace_gen_impls, replace_self},
        to_string::to_string_type_id,
        ty::SolveCond,
        ty_env::TyEnv,
        type_id::TypeId,
    },
    BlockId,
};

/// Used when replacing generics in functions and methods containing to a
/// specific generic implementation. This will be used to replace all types in
/// the body of the functions/methods.
///
/// This can be used for replacing generics declared in ADTs and generics
/// declared in functions. If they are declared in functions, the ADT related
/// fields will be set to None.
///
/// OBS! When using this `GenericsReplacer`, one should use the `Traverser` with
///      `deep_copy` set to true. This will make new copies of all things wrapped
///      in Arc's (fns, vars etc.).
///      This `GenericsReplacer` will then replace the generics with the given
///      implementations and then insert references to the new methods into the
///      specified ADT (both in look-ups and AST tree).
pub struct GenericsReplacer<'a> {
    /// A set containing the name+blockID for the "old" variables that now have
    /// been modified and given a `copy_nr`. This set will be used to figure out
    /// which "variable uses" that need to set a `copy_nr`.
    modified_variables: HashSet<(String, BlockId)>,

    generics_impl: &'a Generics,

    /// If a new ADT is created/replaced, it will be stored in this variable
    /// when it has been traversed. This ADT will then be populated with all
    /// its new methods with replaced generics.
    pub new_adt: Option<Arc<RwLock<Adt>>>,

    old_path: Option<&'a LangPath>,
    new_type_id: Option<TypeId>,

    errors: Vec<LangError>,
}

impl<'a> GenericsReplacer<'a> {
    /// Used when one wants to create new generic ADTs.
    pub fn new_adt(
        generics_impl: &'a Generics,
        old_path: &'a LangPath,
        new_type_id: TypeId,
    ) -> Self {
        Self {
            modified_variables: HashSet::default(),
            generics_impl,
            new_adt: None,
            old_path: Some(old_path),
            new_type_id: Some(new_type_id),
            errors: Vec::default(),
        }
    }

    /// Used when one wants to create new functions or methds.
    pub fn new_func(generics_impl: &'a Generics) -> Self {
        Self {
            modified_variables: HashSet::default(),
            generics_impl,
            new_adt: None,
            old_path: None,
            new_type_id: None,
            errors: Vec::default(),
        }
    }

    fn replace_type_id(&self, type_id: &mut TypeId, ctx: &mut TraverseCtx) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        if let Some(new_type_id) = replace_gen_impls(
            &mut ty_env_guard,
            &ctx.ast_ctx,
            *type_id,
            &self.generics_impl,
        )? {
            *type_id = new_type_id
        }

        if let (Some(old_path), Some(new_ty)) = (self.old_path, self.new_type_id) {
            if let Some(new_type_id) = replace_self(&mut ty_env_guard, *type_id, old_path, new_ty)?
            {
                *type_id = new_type_id;
            }
        }

        let inf_type_id = ty_env_guard.inferred_type(*type_id)?;

        let check_inf = true;
        let solve_cond = SolveCond::new();
        if is_solved(&ty_env_guard, inf_type_id, check_inf, solve_cond)? {
            Ok(())
        } else {
            let new_type = to_string_type_id(&ty_env_guard, inf_type_id)?;
            Err(ctx.ast_ctx.err(format!(
                "Replaced generic type not solved. Type id: {} ({})",
                new_type, type_id
            )))
        }
    }

    /// The traverser will have created a new copy of the ADT. The given `new_adt`
    /// is the new ADT that will have all its generics replaced.
    fn store_new_adt(
        &mut self,
        ty_env: &TyEnv,
        ast_ctx: &mut AstCtx,
        new_adt: &Arc<RwLock<Adt>>,
    ) -> LangResult<()> {
        self.new_adt = Some(Arc::clone(new_adt));

        // Insert the new ADT into the lookup table. This needs to be done
        // before running the `visit_fn()` in this analyzer since it will
        // insert the methods into this ADT.
        let new_path = self.old_path.unwrap().with_gens(self.generics_impl.clone());
        let parent_id = BlockCtx::DEFAULT_BLOCK_ID;
        let key = (new_path, parent_id);
        ast_ctx
            .adts
            .insert(ty_env, DerefType::Deep, key, Arc::clone(&new_adt))?;

        // The methods are RCs inside the "ADT" which means that they are
        // still tied to the "old" ADT. Empty the methods map, it will be
        // filled in later with the same methods that then have had their
        // generics replaced/implemented.
        new_adt.write().methods.clear();
        new_adt.write().generics = Some(self.generics_impl.clone());

        Ok(())
    }
}

impl<'a> Visitor for GenericsReplacer<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.replace_type_id(type_id, ctx) {
            self.errors.push(err);
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this function will store the newly copied/created variables.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let old_key = (var.read().name.clone(), ctx.block_id);
            self.modified_variables.insert(old_key);

            let new_key = (var.read().full_name(), ctx.block_id);
            ctx.ast_ctx.variables.insert(new_key, Arc::clone(var));
        }
    }

    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(new_adt),
            ..
        } = block
        {
            if let Err(err) = self.store_new_adt(&ctx.ty_env.lock(), ctx.ast_ctx, new_adt) {
                self.errors.push(err);
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(new_adt),
            ..
        } = block
        {
            if let Err(err) = self.store_new_adt(&ctx.ty_env.lock(), ctx.ast_ctx, new_adt) {
                self.errors.push(err);
            }
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this logic inserts a reference from the new ADT type to the new method.
    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let new_adt = if let Some(new_adt) = &self.new_adt {
            new_adt
        } else {
            return;
        };

        let module = match ctx.ast_ctx.get_module(ctx.block_id) {
            Ok(Some(module)) => module,
            Ok(None) => LangPath::default(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let new_adt_path = {
            let new_adt = new_adt.read();
            module.clone_push(
                &new_adt.name,
                Some(self.generics_impl),
                Some(new_adt.file_pos),
            )
        };

        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            func.write().method_adt = self.new_type_id;

            // Insert a reference from the "new" ADT to this new method.
            // The name set will be the "half name" containing the generics
            // for the function.
            new_adt
                .write()
                .methods
                .insert(func.read().half_name(&ctx.ty_env.lock()), Arc::clone(&func));

            // Inserts a reference to this new method into the `analyze_context`
            // look-up table.
            if let Err(err) =
                ctx.ast_ctx
                    .insert_method(&ctx.ty_env.lock(), &new_adt_path, Arc::clone(&func))
            {
                self.errors.push(err);
            }
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// need to add the `copy_nr` to all variables uses which declarations have
    /// been given a `copy_nr`. These "modified" declarations can be found in
    /// `self.modified_variables`.
    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {
        match ctx.ast_ctx.get_var_decl_scope(&var.name, ctx.block_id) {
            Ok(decl_block_id) => {
                let key = (var.name.clone(), decl_block_id);
                if self.modified_variables.contains(&key) {
                    var.copy_nr = ctx.copy_nr;
                }
            }
            Err(err) => self.errors.push(err),
        }
    }
}
