use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{ast::AstToken, expr::Var, stmt::Stmt},
    traverser::TraverseContext,
    visitor::Visitor,
    BlockId,
};
use std::{cell::RefCell, collections::hash_map::Entry, rc::Rc};

/// Gathers information about all variable declarations found in the AST and
/// inserts them into the `analyze_context`.
///
/// Also ensures that all uses of a variable `x` are done after the declaration
/// of the variable. If that is not the case, a error will be returned.
pub struct DeclVarAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,

    /// Store information about uses of variables for which a valid declaration
    /// couldn't be found. At the end of this analyzing, these variables will be
    /// tested again to see if a declaration can be found. If that is the case,
    /// a hint can be given in the returned error.
    vars_not_found: Vec<(Var, BlockId)>,

    errors: Vec<LangError>,
}

impl<'a> DeclVarAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            vars_not_found: Vec::default(),
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for DeclVarAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        let analyze_context = self.analyze_context.borrow();

        if self.errors.is_empty() && self.vars_not_found.is_empty() {
            None
        } else {
            // Make a last try to get more information about why a variable
            // couldn't be found.
            for (var, block_id) in &self.vars_not_found {
                let err = match analyze_context.get_var(&var.name, *block_id) {
                    Ok(var_decl) => analyze_context.err(format!(
                        "Variable \"{}\" used before declaration.\nDeclared at pos:\n{:#?}\nUsed at pos:\n{:#?}",
                        &var_decl.borrow().name,
                        &var_decl.borrow().file_pos,
                        &var.file_pos
                    )),
                    Err(err) => err,
                };
                self.errors.push(err);
            }

            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseContext) {
        self.analyze_context.borrow_mut().file_pos =
            ast_token.file_pos().cloned().unwrap_or_default();
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseContext) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let mut analyze_context = self.analyze_context.borrow_mut();

            let key = (var.borrow().name.clone(), ctx.block_id);
            match analyze_context.variables.entry(key) {
                Entry::Vacant(v) => {
                    v.insert(Rc::clone(var));
                }
                Entry::Occupied(o) => {
                    let old_file_pos = o.get().borrow().file_pos;
                    let err = analyze_context.err(format!(
                        "Variable \"{}\" declared multiple times in the same scope.\n\
                        First declaration at pos:\n{:#?}\nRe-declared at pos:\n{:#?}",
                        &var.borrow().name,
                        old_file_pos,
                        &var.borrow().file_pos,
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseContext) {
        if self
            .analyze_context
            .borrow()
            .get_var_decl_scope(&var.name, ctx.block_id)
            .is_err()
        {
            self.vars_not_found.push((var.clone(), ctx.block_id));
        }
    }
}
