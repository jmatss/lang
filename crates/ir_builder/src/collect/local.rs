use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Mutex,
};

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{to_string::to_string_path, ty_env::TyEnv},
};
use ir::{GlobalVarIdx, LocalVarIdx, decl::func::Func, module::Module};

use crate::to_ir_type;

// TODO: I guess the returned map needs to store the variable name+scope since
//       there might be multiple variables with the same name in a function.
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
/// keys being the variable names and the values being the `LocalVarIdx`s.
///
/// OBS! The parameters of functions will already be stored in their respective
///      functions inside the `module` at this point. So the only parameter
///      information collected in this step are the mapping between name and idx,
///      this information is collected into the returned map.
pub(crate) fn collect_params_and_local_vars(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &TyEnv,
    ast_root: &AstToken,
) -> LangResult<HashMap<String, HashMap<String, LocalVarIdx>>> {
    let mut params_and_locals = HashMap::default();

    collect_params(module, ast_ctx, ty_env, &mut params_and_locals, ast_root)?;
    collect_locals(module, ast_ctx, ty_env, &mut params_and_locals, ast_root);

    for token in ast_root {}
    let mut globals = HashMap::default();
    for var in ast_ctx.variables.values() {
        let var = var.as_ref().read().unwrap();
        if var.is_global {
            let type_id = if let Some(type_id) = var.ty {
                type_id
            } else {
                return Err(LangError::new(
                    format!("Global var type not set: {:#?}", var),
                    LangErrorKind::IrError,
                    None,
                ));
            };

            let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
            let global_var_idx = module.add_global_var(ir_type);
            globals.insert(var.name.clone(), global_var_idx);
        }
    }

    Ok(globals)
}

fn collect_params(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &TyEnv,
    params_and_locals: &mut HashMap<String, HashMap<String, LocalVarIdx>>,
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
            ty_env,
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
    params_and_locals: &mut HashMap<String, HashMap<String, LocalVarIdx>>,
    tokens: &[AstToken],
) {
    for token in tokens {
        // If this is a function, inserts the mapping between name and index for
        // all its parameters into the `params_and_locals` map.
        if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
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
                let var_name = param.read().unwrap().name.clone();
                func_entry.insert(var_name, LocalVarIdx(idx));
            }
        } else if let AstToken::Block(Block { body, .. }) = token {
            collect_params_rec(module, ast_ctx, ty_env, params_and_locals, body);
        }
    }
}

pub(crate) fn collect_locals(
    module: &mut Module,
    ast_ctx: &mut AstCtx,
    ty_env: &Mutex<TyEnv>,
    params_and_locals: &mut HashMap<String, HashMap<String, LocalVarIdx>>,
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
pub(crate) struct LocalVarCollector<'a> {
    module: &'a mut Module,
    params_and_locals: &'a mut HashMap<String, HashMap<String, LocalVarIdx>>,
    cur_func_name: Option<String>,
    errors: Vec<LangError>,
}

impl<'a> LocalVarCollector<'a> {
    pub fn new(
        module: &'a mut Module,
        params_and_locals: &'a mut HashMap<String, HashMap<String, LocalVarIdx>>,
    ) -> Self {
        LocalVarCollector {
            module,
            params_and_locals,
            cur_func_name: None,
            errors: Vec::default(),
        }
    }

    fn get_func(&mut self) -> LangError<&mut Func> {
        self.m
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
            let ty_env_guard = ctx.ty_env.lock().unwrap();
            let var = var.read().unwrap();

            let type_id = if let Some(type_id) = var.ty {
                type_id
            } else {
                self.errors.push(LangError::new(
                    format!("Variable decl have no type: {:#?}", var),
                    LangErrorKind::IrError,
                    var.file_pos,
                ));
                return;
            };

            let ir_type = match to_ir_type(ctx.ast_ctx, &ty_env_guard, type_id) {
                Ok(ir_type) => ir_type,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let ir_func = if let Some(ir_func) = self.module.get_function_mut(name) {
                ir_func
            } else {
                self.errors.push(LangError::new(
                    format!("Unable to find function with name \"{}\" in module." , f),
                    LangErrorKind::IrError,
                    var.file_pos,
                ));
                return;
            };
        }
    }
}
