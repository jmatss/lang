use super::{ast_ctx::AstCtx, ty_ctx::TyCtx};

/// Will wrap context found during the analyzing step. This struct will be
/// given to the codegen stage and will be used to fetch information to correctly
/// generate the code.
#[derive(Debug)]
pub struct AnalyzeCtx {
    pub ast_ctx: AstCtx,
    pub ty_ctx: TyCtx,
}
