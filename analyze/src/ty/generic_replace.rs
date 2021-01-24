use super::context::TypeContext;
use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::{BlockHeader, Struct},
        expr::Var,
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, ty::Ty},
    visitor::Visitor,
    BlockId,
};
use std::{cell::RefCell, collections::HashSet, rc::Rc};

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
/// This can be used for replacing generics declared in structs and generics
/// declared in functions. If they are declared in functions, the struct related
/// fields will be set to None.
pub struct GenericsReplacer<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,

    /// A set containing the name+blockID for the "old" variables that now have
    /// been modified and given a `copy_nr`. This set will be used to figure out
    /// which "variable uses" that need to set a `copy_nr`.
    modified_variables: HashSet<(String, BlockId)>,

    generics_impl: &'a Generics,

    new_struct: Option<Rc<RefCell<Struct>>>,
    old_name: Option<&'a str>,
    new_ty: Option<&'a Ty>,

    errors: Vec<LangError>,
}

impl<'a, 'tctx> GenericsReplacer<'a, 'tctx> {
    pub fn new_struct(
        type_context: &'a mut TypeContext<'tctx>,
        new_struct: Rc<RefCell<Struct>>,
        generics_impl: &'a Generics,
        old_name: &'a str,
        new_ty: &'a Ty,
    ) -> Self {
        Self {
            type_context,
            modified_variables: HashSet::default(),
            generics_impl,
            new_struct: Some(new_struct),
            old_name: Some(old_name),
            new_ty: Some(new_ty),
            errors: Vec::default(),
        }
    }

    pub fn new_func(type_context: &'a mut TypeContext<'tctx>, generics_impl: &'a Generics) -> Self {
        Self {
            type_context,
            modified_variables: HashSet::default(),
            generics_impl,
            new_struct: None,
            old_name: None,
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

    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        ty.replace_generics_impl(self.generics_impl);

        if let (Some(old_name), Some(new_ty)) = (self.old_name, self.new_ty) {
            ty.replace_self(old_name, new_ty);
        }

        // Now that the generics might have been replaced, try to solve the type
        // again.
        let inferred_ty = match self.type_context.inferred_type(ty, ctx.block_id) {
            Ok(inferred_ty) => inferred_ty,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        if inferred_ty.is_solved() {
            *ty = inferred_ty;
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to solve type: {:#?}", ty));
            self.errors.push(err);
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this function will store the newly copied/created variables.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
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
    /// this logic inserts a reference from the new structure type to the new method.
    fn visit_func(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Some(new_struct) = &self.new_struct {
            let new_struct_name = new_struct.borrow().name.clone();

            if let AstToken::Block(BlockHeader::Function(func), _, old_id, ..) = ast_token {
                func.borrow_mut().method_structure = self.new_ty.cloned();

                // Insert a reference from the "new" structure to this new method.
                // The name set will be the "half name" containing the generics
                // for the function.
                if let Some(methods) = new_struct.borrow_mut().methods.as_mut() {
                    methods.insert(func.borrow().half_name(), Rc::clone(&func));
                }

                // Inserts a reference to this new method into the `analyze_context`
                // look-up table.
                if let Err(err) = self.type_context.analyze_context.insert_method(
                    &new_struct_name,
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
    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {
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
