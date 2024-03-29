use std::{collections::hash_map::Entry, sync::Arc};

use common::{
    ctx::block_ctx::BlockCtx,
    error::LangError,
    token::{expr::Var, stmt::Stmt},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    BlockId,
};

/// Gathers information about all variable declarations found in the AST and
/// inserts them into the `analyze_context`. Params are also included since
/// they are considered `VariableDecl`s as well.
///
/// Also ensures that all uses of a variable `x` are done after the declaration
/// of the variable. If that is not the case, a error will be returned.
pub struct DeclVarAnalyzer {
    /// Store information about uses of variables for which a valid declaration
    /// couldn't be found. At the end of this analyzing, these variables will be
    /// tested again to see if a declaration can be found. If that is the case,
    /// a hint can be given in the returned error.
    vars_not_found: Vec<(Var, BlockId)>,

    errors: Vec<LangError>,
}

impl DeclVarAnalyzer {
    pub fn new() -> Self {
        Self {
            vars_not_found: Vec::default(),
            errors: Vec::default(),
        }
    }
}

impl Visitor for DeclVarAnalyzer {
    fn take_errors(&mut self, ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() && self.vars_not_found.is_empty() {
            None
        } else {
            // Make a last try to get more information about why a variable
            // couldn't be found.
            for (var, block_id) in &self.vars_not_found {
                let err = match ctx.ast_ctx.get_var(&var.name, *block_id) {
                    Ok(var_decl) => ctx.ast_ctx.err(format!(
                        "Variable \"{}\" used before declaration.\nDeclared at pos:\n{:#?}\nUsed at pos:\n{:#?}",
                        &var_decl.read().name,
                        &var_decl.read().file_pos,
                        &var.file_pos
                    )),
                    Err(err) => err,
                };
                self.errors.push(err);
            }

            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            if ctx.block_id == BlockCtx::DEFAULT_BLOCK_ID {
                var.write().is_global = true;
            }

            let key = (var.read().name.clone(), ctx.block_id);
            match ctx.ast_ctx.variables.entry(key) {
                Entry::Vacant(v) => {
                    v.insert(Arc::clone(var));
                }
                Entry::Occupied(o) => {
                    let old_file_pos = o.get().read().file_pos;
                    let err = ctx.ast_ctx.err(format!(
                        "Variable \"{}\" declared multiple times in the same scope ({}).\n\
                        First declaration at pos:\n{:#?}\nRe-declared at pos:\n{:#?}",
                        &var.read().name,
                        ctx.block_id,
                        old_file_pos,
                        &var.read().file_pos,
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {
        if ctx
            .ast_ctx
            .get_var_decl_scope(&var.name, ctx.block_id)
            .is_err()
        {
            self.vars_not_found.push((var.clone(), ctx.block_id));
        }
    }
}
