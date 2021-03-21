use super::context::TypeContext;
use common::{
    error::LangError,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
        expr::Var,
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, ty::Ty},
    visitor::Visitor,
    BlockId, TypeId,
};
use std::{cell::RefCell, collections::HashSet, rc::Rc};

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
///
/// This can be used for replacing generics declared in ADTs and generics
/// declared in functions. If they are declared in functions, the ADT related
/// fields will be set to None.
pub struct GenericsReplacer<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,

    /// A set containing the name+blockID for the "old" variables that now have
    /// been modified and given a `copy_nr`. This set will be used to figure out
    /// which "variable uses" that need to set a `copy_nr`.
    modified_variables: HashSet<(String, BlockId)>,

    generics_impl: &'a Generics,

    new_adt: Option<Rc<RefCell<Adt>>>,
    old_path: Option<&'a LangPath>,
    new_ty: Option<&'a Ty>,

    errors: Vec<LangError>,
}

impl<'a, 'tctx> GenericsReplacer<'a, 'tctx> {
    pub fn new_adt(
        type_context: &'a mut TypeContext<'tctx>,
        new_adt: Rc<RefCell<Adt>>,
        generics_impl: &'a Generics,
        old_path: &'a LangPath,
        new_ty: &'a Ty,
    ) -> Self {
        Self {
            type_context,
            modified_variables: HashSet::default(),
            generics_impl,
            new_adt: Some(new_adt),
            old_path: Some(old_path),
            new_ty: Some(new_ty),
            errors: Vec::default(),
        }
    }

    pub fn new_func(type_context: &'a mut TypeContext<'tctx>, generics_impl: &'a Generics) -> Self {
        Self {
            type_context,
            modified_variables: HashSet::default(),
            generics_impl,
            new_adt: None,
            old_path: None,
            new_ty: None,
            errors: Vec::default(),
        }
    }
}

impl<'a, 'tctx> Visitor for GenericsReplacer<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseContext) {
        if let Err(err) = self
            .type_context
            .analyze_context
            .ty_env
            .replace_generics_impl(*type_id, &self.generics_impl)
        {
            self.errors.push(err);
            return;
        }

        if let (Some(old_path), Some(new_ty)) = (self.old_path, self.new_ty) {
            if let Err(err) = self
                .type_context
                .analyze_context
                .ty_env
                .replace_self(*type_id, old_path, new_ty)
            {
                self.errors.push(err);
                return;
            }
        }

        let inferred_type_id = match self.type_context.inferred_type(*type_id, ctx.block_id) {
            Ok(inferred_type_id) => inferred_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let sub_sets = match self.type_context.analyze_context.get_root_id(ctx.block_id) {
            Ok(root_id) => self.type_context.substitutions.get(&root_id),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        match self
            .type_context
            .analyze_context
            .ty_env
            .is_solved(sub_sets, inferred_type_id)
        {
            Ok(true) => *type_id = inferred_type_id,
            Ok(false) => {
                let err = self
                    .type_context
                    .analyze_context
                    .err(format!("Unable to solve type: {:#?}", type_id));
                self.errors.push(err);
            }
            Err(err) => self.errors.push(err),
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this function will store the newly copied/created variables.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseContext) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let old_key = (var.borrow().name.clone(), ctx.block_id);
            self.modified_variables.insert(old_key);

            let new_key = (var.borrow().full_name(), ctx.block_id);
            self.type_context
                .analyze_context
                .variables
                .insert(new_key, Rc::clone(var));
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this logic inserts a reference from the new ADT type to the new method.
    fn visit_fn(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseContext) {
        if let Some(new_adt) = &self.new_adt {
            let module = match self.type_context.analyze_context.get_module(ctx.block_id) {
                Ok(Some(module)) => module,
                Ok(None) => LangPath::default(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let new_adt_name = {
                let new_adt = new_adt.borrow();
                module.clone_push(&new_adt.name, Some(self.generics_impl))
            };

            if let AstToken::Block(BlockHeader::Fn(func), _, old_id, ..) = ast_token {
                let type_id_opt = if let Some(new_ty) = self.new_ty {
                    match self.type_context.analyze_context.ty_env.id(new_ty) {
                        Ok(type_id) => Some(type_id),
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    None
                };
                func.borrow_mut().method_adt = type_id_opt;

                // Insert a reference from the "new" ADT to this new method.
                // The name set will be the "half name" containing the generics
                // for the function.
                new_adt
                    .borrow_mut()
                    .methods
                    .insert(func.borrow().half_name(), Rc::clone(&func));

                // Inserts a reference to this new method into the `analyze_context`
                // look-up table.
                if let Err(err) = self.type_context.analyze_context.insert_method(
                    &new_adt_name,
                    Rc::clone(&func),
                    *old_id,
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
    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseContext) {
        match self
            .type_context
            .analyze_context
            .get_var_decl_scope(&var.name, ctx.block_id)
        {
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
