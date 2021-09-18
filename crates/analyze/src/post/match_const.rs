use common::{
    ctx::ast_ctx::AstCtx,
    error::LangError,
    error::LangResult,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
};

// TODO: Currently there is no way to see if the given expressions are compile-
//       time const. This needs to be implemented in the future. For now we just
//       check if the expressions are run-time const which only guarantees that
//       the value will not be allocated on the stack. The expressions can still
//       not be compile-time const, so this analyzer does NOT catch all cases.
//       We will get an error during LLVM codegen if we don't catch it here.

/// Makes sure that expressions in the match-cases are compile-time `const`.
/// This is a requirement to generate a `switch`-statement instead of compiling
/// it down into multiple `if`-statements.
///
/// The expression that is match on does NOT need to be `const`.
pub struct MatchConstAnalyzer {
    errors: Vec<LangError>,
}

impl MatchConstAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn validate_case_exprs_const(
        &self,
        ast_ctx: &AstCtx,
        match_cases: &[AstToken],
    ) -> LangResult<()> {
        for match_case in match_cases {
            if let AstToken::Block(Block {
                header: BlockHeader::MatchCase(Some(expr)),
                ..
            }) = match_case
            {
                if !expr.is_const() {
                    return Err(ast_ctx.err(format!(
                        "Expression in match case wasn't const, was: {:#?}",
                        match_case
                    )));
                }
            }
        }

        Ok(())
    }
}

impl Visitor for MatchConstAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_match(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Match(..),
            body: match_cases,
            ..
        } = block
        {
            if let Err(err) = self.validate_case_exprs_const(ctx.ast_ctx, match_cases) {
                self.errors.push(err);
            }
        }
    }
}
