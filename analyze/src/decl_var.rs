use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{ast::AstToken, stmt::Stmt},
    traverser::TraverseContext,
    visitor::Visitor,
};
use std::{cell::RefCell, collections::hash_map::Entry};

/// Gathers information about all variable declarations found in the AST and
/// inserts them into the `analyze_context`.
pub struct DeclVarAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> DeclVarAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for DeclVarAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.analyze_context.borrow_mut().cur_line_nr = ast_token.line_nr;
        self.analyze_context.borrow_mut().cur_column_nr = ast_token.column_nr;
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Stmt::VariableDecl(var, _) = stmt {
            let var = var.borrow();
            let key = (var.name.clone(), ctx.block_id);

            if let Entry::Vacant(v) = analyze_context.variables.entry(key) {
                v.insert(var.clone());
            } else {
                let err_msg = format!(
                    "A variable with name \"{}\" already declared in this scope ({}).",
                    &var.name, ctx.block_id
                );
                let err = analyze_context.err(err_msg);
                self.errors.push(err);
            }
        }
    }
}
