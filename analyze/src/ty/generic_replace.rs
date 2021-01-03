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

use crate::AnalyzeContext;

pub struct GenericsReplacer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    new_struct: Rc<RefCell<Struct>>,

    /// A set containing the name+blockID for the "old" variables that now have
    /// been modified and given a `copy_nr`. This set will be used to figure out
    /// which "variable uses" that need to set a `copy_nr`.
    modified_variables: HashSet<(String, BlockId)>,

    generics_impl: &'a Generics,
    old_name: &'a str,
    new_ty: &'a Ty,

    errors: Vec<LangError>,
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> GenericsReplacer<'a> {
    pub fn new(
        analyze_context: &'a mut AnalyzeContext,
        new_struct: Rc<RefCell<Struct>>,
        generics_impl: &'a Generics,
        old_name: &'a str,
        new_ty: &'a Ty,
    ) -> Self {
        Self {
            analyze_context,
            new_struct,
            modified_variables: HashSet::default(),
            generics_impl,
            old_name,
            new_ty,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for GenericsReplacer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        ty.replace_generics_impl(self.generics_impl);
        ty.replace_self(self.old_name, self.new_ty);
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this function will store the newly copied/created variables.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let old_key = (var.borrow().name.clone(), ctx.block_id);
            self.modified_variables.insert(old_key);

            let new_key = (var.borrow().full_name(), ctx.block_id);
            self.analyze_context
                .variables
                .insert(new_key, Rc::clone(var));
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// this logic inserts a reference from the new structure type to the new method.
    fn visit_func(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let new_struct_name = self.new_struct.borrow().name.clone();

        if let AstToken::Block(BlockHeader::Function(func), _, old_id, ..) = ast_token {
            func.borrow_mut().method_structure = Some(self.new_ty.clone());

            // Insert a reference from the "new" structure to this new method.
            if let Some(methods) = self.new_struct.borrow_mut().methods.as_mut() {
                methods.insert(func.borrow().name.clone(), Rc::clone(&func));
            }

            // Inserts a reference to this new method into the `analyze_context`
            // look-up table.
            if let Err(err) =
                self.analyze_context
                    .insert_method(&new_struct_name, Rc::clone(&func), *old_id)
            {
                self.errors.push(err);
            }
        }
    }

    /// Since this `GenericsReplacer` is called with `deep_copy` set to true,
    /// need to add the `copy_nr` to all variables uses which declarations have
    /// been given a `copy_nr`. These "modified" declarations can be found in
    /// `self.modified_variables`.
    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {
        match self
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
