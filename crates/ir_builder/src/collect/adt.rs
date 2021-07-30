use std::sync::Mutex;

use common::{ctx::ast_ctx::AstCtx, error::LangResult, ty::ty_env::TyEnv};
use ir::module::Module;

use crate::{into_err, to_ir_adt};

/// Collects all ADTs (struct/enum/unions) that can be found in the `AstCtx` and
/// inserts them into the given `module.`
pub(crate) fn collect_type_decls(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
) -> LangResult<()> {
    let ty_env_guard = ty_env.lock().unwrap();
    for adt in ast_ctx.adts.values() {
        let adt = adt.as_ref().read().unwrap();
        module
            .add_adt(to_ir_adt(ast_ctx, &ty_env_guard, &adt)?)
            .map_err(|e| into_err(e))?;
    }
    Ok(())
}
