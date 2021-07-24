use common::{ctx::ast_ctx::AstCtx, error::LangResult, ty::ty_env::TyEnv};
use ir::module::Module;

use crate::{into_err, to_ir_adt};

/// Collects all ADTs (struct/enum/unions) that can be found in the `AstCtx` and
/// inserts them into the given `module.`
pub(crate) fn collect_type_decls(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &TyEnv,
) -> LangResult<()> {
    for adt in ast_ctx.adts.values() {
        let adt = adt.as_ref().read().unwrap();
        module
            .add_adt(to_ir_adt(ast_ctx, ty_env, &adt)?)
            .map_err(|e| into_err(e))?;
    }

    Ok(())
}
