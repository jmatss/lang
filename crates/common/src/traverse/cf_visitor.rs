use crate::{
    cf_traverser::ControlFlowTraverseContext, error::LangError, token::ast::AstToken,
    token::block::Function,
};

/// A visitor that visits all control flows in all functions.
#[allow(unused_variables)]
pub trait ControlFlowVisitor {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        None
    }

    fn visit_cf_start(&mut self, cf_id: usize, ctx: &ControlFlowTraverseContext) {}
    fn visit_cf_end(&mut self, cf_id: usize, ctx: &ControlFlowTraverseContext) {}
    fn visit_fn_start(&mut self, func: &Function, ctx: &ControlFlowTraverseContext) {}
    fn visit_fn_end(&mut self, func: &Function, ctx: &ControlFlowTraverseContext) {}
    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &ControlFlowTraverseContext) {}
}
