use std::{collections::HashMap, sync::Mutex};

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
    ty::{to_string::to_string_path, ty_env::TyEnv},
    BlockId,
};
use either::Either;
use ir::{module::Module, LocalVarIdx, ParamVarIdx};

use crate::to_ir_type;

/// Collects and adds all local and parameter variables for every function.
///
/// They will be added into the `Func`s inside the given `module`. This function
/// will also return a map mapping the variable names to their corresponding
/// `LocalVarIdx`. This is needed since the names of the variables aren't stored
/// in the module, it only uses indices. So one can use the map to lookup the
/// variable uses in the AST (using names) and translate it to the correct variable
/// in the IR (using indices).
///
/// In the returned map, the first String key is the name of the function that
/// the params/locals was found in. The value of this map is then a map with the
/// keys being the variable name+decl scope and the values being the `LocalVarIdx`s.
///
/// OBS! The parameters of functions will already be stored in their respective
///      functions inside the `module` at this point. So the only parameter
///      information collected in this step are the mapping between name and idx,
///      this information is collected into the returned map.
pub(crate) fn collect_locals_and_params(
    module: &mut Module,
    ast_ctx: &mut AstCtx,
    ty_env: &Mutex<TyEnv>,
    ast_root: &mut AstToken,
) -> LangResult<HashMap<String, HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>>> {
    let mut params_and_locals = HashMap::default();

    collect_params(module, ast_ctx, ty_env, &mut params_and_locals, ast_root)?;
    collect_locals(module, ast_ctx, ty_env, &mut params_and_locals, ast_root);

    Ok(params_and_locals)
}

fn collect_params(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
    params_and_locals: &mut HashMap<
        String,
        HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>,
    >,
    ast_root: &AstToken,
) -> LangResult<()> {
    if let AstToken::Block(Block {
        header: BlockHeader::Default,
        body,
        ..
    }) = ast_root
    {
        Ok(collect_params_rec(
            module,
            ast_ctx,
            &ty_env.lock().unwrap(),
            params_and_locals,
            body,
        ))
    } else {
        Err(LangError::new(
            format!("Given \"ast_root\" not default block, was: {:#?}", ast_root),
            LangErrorKind::IrError,
            None,
        ))
    }
}

fn collect_params_rec(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &TyEnv,
    params_and_locals: &mut HashMap<
        String,
        HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>,
    >,
    tokens: &[AstToken],
) {
    for token in tokens {
        // If this is a function, inserts the mapping between name and index for
        // all its parameters into the `params_and_locals` map.
        if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
            id: block_id,
            ..
        }) = token
        {
            let func = func.as_ref().read().unwrap();
            let params = if let Some(params) = &func.parameters {
                params
            } else {
                continue;
            };

            let func_path =
                func.module
                    .clone_push(&func.name, func.generics.as_ref(), Some(func.file_pos));
            let func_full_name = to_string_path(ty_env, &func_path);

            let func_entry = params_and_locals.entry(func_full_name).or_default();
            for (idx, param) in params.iter().enumerate() {
                let key = (param.read().unwrap().name.clone(), *block_id);
                func_entry.insert(key, Either::Right(ParamVarIdx(idx)));
            }
        } else if let AstToken::Block(Block { body, .. }) = token {
            collect_params_rec(module, ast_ctx, ty_env, params_and_locals, body);
        }
    }
}

fn collect_locals(
    module: &mut Module,
    ast_ctx: &mut AstCtx,
    ty_env: &Mutex<TyEnv>,
    params_and_locals: &mut HashMap<
        String,
        HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>,
    >,
    ast_root: &mut AstToken,
) -> Result<(), Vec<LangError>> {
    let mut traverse_ctx = TraverseCtx::new(ast_ctx, &ty_env);
    let mut visitor = LocalVarCollector::new(module, params_and_locals);
    traverse(&mut traverse_ctx, &mut visitor, ast_root)
}

/// Collects all locals, in the order in which they are traversed, and inserts
/// their index and type into the `module`. Also inserts the mapping between
/// variable name+BlockId into the `params_and_locals` which is needed to generate
/// correct IR.
struct LocalVarCollector<'a> {
    module: &'a mut Module,
    params_and_locals:
        &'a mut HashMap<String, HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>>,
    cur_func_name: Option<String>,
    errors: Vec<LangError>,
}

impl<'a> LocalVarCollector<'a> {
    fn new(
        module: &'a mut Module,
        params_and_locals: &'a mut HashMap<
            String,
            HashMap<(String, BlockId), Either<LocalVarIdx, ParamVarIdx>>,
        >,
    ) -> Self {
        LocalVarCollector {
            module,
            params_and_locals,
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

        let ir_func = if let Some(ir_func) = self.module.get_function_mut(&cur_func_name) {
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

        let entry = self.params_and_locals.entry(cur_func_name).or_default();
        entry.insert((var.name.clone(), block_id), Either::Left(idx));

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
    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            let func = func.read().unwrap();
            let func_path =
                func.module
                    .clone_push(&func.name, func.generics.as_ref(), Some(func.file_pos));
            let func_full_name = to_string_path(&ctx.ty_env.lock().unwrap(), &func_path);

            self.cur_func_name = Some(func_full_name);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let var = var.read().unwrap();

            if !var.is_global {
                let ast_ctx = ctx.ast_ctx;
                let ty_env_guard = ctx.ty_env.lock().unwrap();
                if let Err(err) = self.add_local_var(ast_ctx, &ty_env_guard, &var, ctx.block_id) {
                    self.errors.push(err);
                }
            }
        }
    }
}
