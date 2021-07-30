use std::sync::Mutex;

use common::{
    ctx::ast_ctx::AstCtx,
    error::LangResult,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    ty::ty_env::TyEnv,
};
use ir::module::Module;

use crate::{into_err, to_ir_func};

/// Collects all functions that can be found in the `AstCtx` and inserts them into
/// the given `module.`
pub(crate) fn collect_func_decls(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
    ast_token: &AstToken,
) -> LangResult<()> {
    if let AstToken::Block(Block { header, body, .. }) = ast_token {
        if let BlockHeader::Fn(func) = header {
            let func = func.as_ref().read().unwrap();
            let ir_func = to_ir_func(ast_ctx, &ty_env.lock().unwrap(), &func)?;
            module
                .add_function(ir_func.name.clone(), ir_func)
                .map_err(|e| into_err(e))?;
        }

        for token in body {
            collect_func_decls(module, ast_ctx, ty_env, token)?;
        }
    }
    Ok(())
}
