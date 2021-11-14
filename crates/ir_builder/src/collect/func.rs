use parking_lot::Mutex;

use common::{
    ctx::ast_ctx::AstCtx,
    error::LangResult,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        stmt::{ExternalDecl, Stmt},
    },
    ty::ty_env::TyEnv,
};
use ir::module::Module;

use crate::{into_err, to_ir_func};

/// Collects all functions that can be found in the `AstCtx` and inserts them into
/// the given `module.` This also includes externally declared functions.
pub(crate) fn collect_func_decls(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
    ast_token: &AstToken,
) -> LangResult<()> {
    if let AstToken::Block(Block { header, body, .. }) = ast_token {
        if let BlockHeader::Fn(func) = header {
            let func = func.read();
            let ir_func = to_ir_func(ast_ctx, &ty_env.lock(), &func)?;
            module
                .add_func(ir_func.name.clone(), ir_func)
                .map_err(into_err)?;
        }

        for token in body {
            collect_func_decls(module, ast_ctx, ty_env, token)?;
        }
    } else if let AstToken::Stmt(Stmt::ExternalDecl(ExternalDecl::Fn(func), _)) = ast_token {
        let func = func.read();
        let ir_func = to_ir_func(ast_ctx, &ty_env.lock(), &func)?;

        // Allow mulitple declaration of the same extern function.
        if module.get_func(&ir_func.name).is_none() {
            module
                .add_func(ir_func.name.clone(), ir_func)
                .map_err(into_err)?;
        }
    }
    Ok(())
}
