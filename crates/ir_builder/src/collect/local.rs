use std::collections::HashMap;

use parking_lot::Mutex;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        expr::Var,
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::ty_env::TyEnv,
    BlockId,
};
use ir::{module::Module, LocalVarIdx};

use crate::{fn_full_name, to_ir_type, VarModifier};

pub type LocalVarMap = HashMap<(String, BlockId), (LocalVarIdx, VarModifier)>;

/// Collects all local variables for every function and insertes them into
/// the corresponding `FuncDecl` in the given `module`.
///
/// This function will return a map mapping the variable names to their
/// corresponding `LocalVarIdx`. This is needed since the names of the variables
/// aren't stored in the module, it only uses indices. So one can use the maps
/// to lookup the variable uses in the AST (using names) and translate it to the
/// correct variable in the IR (using indices).
///
/// In the returned map, the first String key is the name of the function that
/// the locals was found in. The values of the map are maps with the keys being
/// the variable name+decl scope and the values being the `LocalVarIdx`s.
pub fn collect_locals(
    module: &mut Module,
    ast_ctx: &mut AstCtx,
    ty_env: &Mutex<TyEnv>,
    ast_root: &mut AstToken,
) -> Result<HashMap<String, LocalVarMap>, Vec<LangError>> {
    let mut locals = HashMap::default();
    let mut traverse_ctx = TraverseCtx::new(ast_ctx, ty_env);
    let mut visitor = LocalVarCollector::new(module, &mut locals);
    traverse(&mut traverse_ctx, &mut visitor, ast_root)?;
    Ok(locals)
}

/// Collects all locals, in the order in which they are traversed, and inserts
/// their index and type into the `module`. Also inserts the mapping between
/// variable name+BlockId into the `locals` which is needed to generate
/// correct IR.
struct LocalVarCollector<'a> {
    module: &'a mut Module,
    locals: &'a mut HashMap<String, LocalVarMap>,
    cur_func_name: Option<String>,
    errors: Vec<LangError>,
}

impl<'a> LocalVarCollector<'a> {
    fn new(module: &'a mut Module, locals: &'a mut HashMap<String, LocalVarMap>) -> Self {
        LocalVarCollector {
            module,
            locals,
            cur_func_name: None,
            errors: Vec::default(),
        }
    }

    /// Inserts the given var  into the its corresponding `self.module` function.
    /// Also adds it as an element in the `self.params_and_locals` map.
    fn add_local_var(
        &mut self,
        ast_ctx: &mut AstCtx,
        ty_env: &TyEnv,
        var: &Var,
        block_id: BlockId,
    ) -> LangResult<()> {
        let cur_func_name = if let Some(cur_func_name) = &self.cur_func_name {
            cur_func_name.clone()
        } else {
            return Err(LangError::new(
                format!(
                    "cur_func_name not set when collection local var: {:#?}",
                    var
                ),
                LangErrorKind::IrError,
                var.file_pos,
            ));
        };

        let ir_func = if let Some(ir_func) = self.module.get_func_mut(&cur_func_name) {
            ir_func
        } else {
            return Err(LangError::new(
                format!(
                    "Unable to find function with name \"{:?}\" in module.",
                    self.cur_func_name
                ),
                LangErrorKind::IrError,
                var.file_pos,
            ));
        };

        let type_id = if let Some(type_id) = var.ty {
            type_id
        } else {
            return Err(LangError::new(
                format!("Variable decl have no type: {:#?}", var),
                LangErrorKind::IrError,
                var.file_pos,
            ));
        };

        let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
        let idx = ir_func.add_local_var(ir_type);
        let var_modifier = if var.is_const {
            VarModifier::Const
        } else {
            VarModifier::None
        };

        let entry = self.locals.entry(cur_func_name).or_default();
        entry.insert((var.full_name(), block_id), (idx, var_modifier));

        Ok(())
    }
}

impl<'a> Visitor for LocalVarCollector<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// Store the name of the function that is being visited. This will allow
    /// us to check in which function we are when we are looking at the
    /// variable declarations.
    ///
    /// This will not be correct all the time since when we are between functions,
    /// a `cur_func_name` will be set even though we aren't in a function.
    /// But since we only care about variable declarations inside function,
    /// we will never check the `cur_func_name` variable while we are between
    /// two functions, so this is not a problem for us.
    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            let func = func.read();
            let fn_full_name = match fn_full_name(&ctx.ty_env.lock(), &func) {
                Ok(fn_full_name) => fn_full_name,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            self.cur_func_name = Some(fn_full_name);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let var = var.read();

            if !var.is_global {
                if let Err(err) =
                    self.add_local_var(ctx.ast_ctx, &ctx.ty_env.lock(), &var, ctx.block_id)
                {
                    self.errors.push(err);
                }
            }
        }
    }
}
