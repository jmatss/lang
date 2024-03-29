use std::collections::HashMap;

use parking_lot::Mutex;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangErrorKind, LangResult},
    token::{expr::Expr, lit::Lit},
    ty::ty_env::TyEnv,
};
use ir::{Data, GlobalVarIdx, Module};

use crate::to_ir_type;

// TODO: How to handle the init value of globals?

/// Collects and adds all global variables to the module. This function also
/// returns a map of the variables so that one can map the variable names to the
/// corresponding `GlobalVarIdx` of the variable in the module.
/// This is needed since the names of the variables aren't stored in the module,
/// it only uses indices.
pub(crate) fn collect_globals(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
) -> LangResult<HashMap<String, GlobalVarIdx>> {
    let ty_env_guard = ty_env.lock();
    let mut globals = HashMap::default();
    for var in ast_ctx.variables.values() {
        let var = var.read();
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

            let lit_opt = if let Some(init_expr) = &var.value {
                if let Expr::Lit(ref lit, ..) = **init_expr {
                    Some(match lit {
                        // TODO: Handle different string types.
                        Lit::String(s, _) => {
                            let data = Data::StringLit(s.into());
                            ir::Lit::String(module.add_data(data))
                        }
                        // TODO: Handle error of char.
                        Lit::Char(c) => ir::Lit::Char(c.chars().next().unwrap()),
                        Lit::Bool(b) => ir::Lit::Bool(*b),
                        // TODO: Handle radix of integer.
                        Lit::Integer(i, _) => ir::Lit::Integer(i.into()),
                        Lit::Float(f) => ir::Lit::Float(f.into()),
                    })
                } else {
                    return Err(LangError::new(
                        format!("Init value of global \"{}\" must be literal.", var.name),
                        LangErrorKind::IrError,
                        None,
                    ));
                }
            } else {
                None
            };

            let ir_type = to_ir_type(ast_ctx, &ty_env_guard, module.ptr_size, type_id)?;
            let global_var_idx = module.add_global_var(ir_type, lit_opt);
            globals.insert(var.full_name(), global_var_idx);
        }
    }
    Ok(globals)
}
