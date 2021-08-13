use std::{
    collections::HashSet,
    sync::{Arc, RwLock},
};

use common::{
    error::LangError,
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
        solve::inferred_type,
        ty::SolveCond,
        type_id::TypeId,
    },
    BlockId,
};

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
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

    new_adt: Option<Arc<RwLock<Adt>>>,
    old_path: Option<&'a LangPath>,
    new_type_id: Option<TypeId>,

    errors: Vec<LangError>,
}

impl<'a> GenericsReplacer<'a> {
    pub fn new_adt(
        new_adt: Arc<RwLock<Adt>>,
        generics_impl: &'a Generics,
        old_path: &'a LangPath,
        new_type_id: TypeId,
    ) -> Self {
        Self {
            modified_variables: HashSet::default(),
            generics_impl,
            new_adt: Some(new_adt),
            old_path: Some(old_path),
            new_type_id: Some(new_type_id),
            errors: Vec::default(),
        }
    }

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
        match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, *type_id, &self.generics_impl) {
            Ok(Some(new_type_id)) => *type_id = new_type_id,
            Ok(None) => (),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        }

        if let (Some(old_path), Some(new_ty)) = (self.old_path, self.new_type_id) {
            match replace_self(&mut ctx.ty_env.lock().unwrap(), *type_id, old_path, new_ty) {
                Ok(Some(new_type_id)) => *type_id = new_type_id,
                Ok(None) => (),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        let inf_type_id = match inferred_type(&ctx.ty_env.lock().unwrap(), *type_id) {
            Ok(inf_type_id) => inf_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let check_inf = true;
        let solve_cond = SolveCond::new();
        match is_solved(
            &ctx.ty_env.lock().unwrap(),
            inf_type_id,
            check_inf,
            solve_cond,
        ) {
            Ok(true) => *type_id = inf_type_id,
            Ok(false) => {
                let err = ctx
                    .ast_ctx
                    .err(format!("Unable to solve type: {:#?}", type_id));
                self.errors.push(err);
            }
            Err(err) => self.errors.push(err),
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this function will store the newly copied/created variables.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let old_key = (var.as_ref().read().unwrap().name.clone(), ctx.block_id);
            self.modified_variables.insert(old_key);

            let new_key = (var.as_ref().read().unwrap().full_name(), ctx.block_id);
            ctx.ast_ctx.variables.insert(new_key, Arc::clone(var));
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this logic inserts a reference from the new ADT type to the new method.
    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Some(new_adt) = &self.new_adt {
            let module = match ctx.ast_ctx.get_module(ctx.block_id) {
                Ok(Some(module)) => module,
                Ok(None) => LangPath::default(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let new_adt_path = {
                let new_adt = new_adt.as_ref().read().unwrap();
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
                func.as_ref().write().unwrap().method_adt = self.new_type_id;

                // Insert a reference from the "new" ADT to this new method.
                // The name set will be the "half name" containing the generics
                // for the function.
                new_adt.as_ref().write().unwrap().methods.insert(
                    func.as_ref()
                        .read()
                        .unwrap()
                        .half_name(&ctx.ty_env.lock().unwrap()),
                    Arc::clone(&func),
                );

                // Inserts a reference to this new method into the `analyze_context`
                // look-up table.
                if let Err(err) = ctx.ast_ctx.insert_method(
                    &ctx.ty_env.lock().unwrap(),
                    &new_adt_path,
                    Arc::clone(&func),
                ) {
                    self.errors.push(err);
                }
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
