use std::collections::HashMap;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangErrorKind, LangResult},
    ty::ty_env::TyEnv,
};
use ir::{module::Module, GlobalVarIdx};

use crate::to_ir_type;

/// Collects and adds all global variables to the module. This function also
/// returns a map of the variables so that one can map the variable names to the
/// corresponding `GlobalVarIdx` of the variable in the module.
/// This is needed since the names of the variables aren't stored in the module,
/// it only uses indices.
pub(crate) fn collect_global_vars(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &TyEnv,
) -> LangResult<HashMap<String, GlobalVarIdx>> {
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
