use parking_lot::Mutex;

use crate::ty::ty_env::TyEnv;

use super::ast_ctx::AstCtx;

/// Will wrap context found during the analyzing step. This struct will be
/// given to the codegen stage and will be used to fetch information to correctly
/// generate the code.
#[derive(Debug)]
pub struct AnalyzeCtx<'a> {
    pub ast_ctx: AstCtx,
    pub ty_env: &'a Mutex<TyEnv>,
}
