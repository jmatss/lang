use std::collections::HashSet;

use common::{
    ctx::ast_ctx::AstCtx,
    error::LangError,
    error::LangResult,
    file::FilePosition,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        expr::Expr,
        op::{Op, UnOperator},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{inner_ty::InnerTy, ty::Ty, ty_env::TyEnv},
};

// TODO: Currently only implemented for hardcoded match case expression values
//       since then you have a "known" set of values at this part of the compiler
//       process. Otherwise, if you want to solve this for case expression containing
//       variables, you would need to solve the expression to figure out what they
//       "cover". So this would need some new functionality to be implemented in
//       the compiler.
//       This new functionality might be helpful in general to solve constants,
//       so might be good to implement in the future. Then the compiler would
//       depend less on LLVMs contant support which would be good.

/// Makes sure that match statements are exhaustive, otherwise a error should be
/// reported.
pub struct MatchExhaustAnalyzer {
    errors: Vec<LangError>,
}

impl MatchExhaustAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn exhaust(
        &self,
        ctx: &TraverseCtx,
        match_expr: &mut Expr,
        match_cases: &[AstToken],
    ) -> LangResult<()> {
        let ty_env = ctx.ty_env.lock();
        let block_id = ctx.block_id;

        let file_pos = match_expr.file_pos();
        let type_id = match_expr.get_expr_type()?;
        let match_expr_ty = ty_env.ty(type_id)?;

        match match_expr_ty {
            Ty::CompoundType(InnerTy::Enum(partial_path), ..) => {
                let full_path =
                    ctx.ast_ctx
                        .calculate_adt_full_path(&ty_env, partial_path, block_id)?;
                self.exhaust_enum(&ty_env, ctx.ast_ctx, file_pos, &full_path, match_cases)
            }

            Ty::CompoundType(inner_ty, ..) if inner_ty.is_int() => self.exhaust_int(),

            _ => Err(ctx.ast_ctx.err(format!(
                "Invalid type of match expression: {:#?}",
                match_expr
            ))),
        }
    }

    fn exhaust_enum(
        &self,
        ty_env: &TyEnv,
        ast_ctx: &AstCtx,
        file_pos: Option<&FilePosition>,
        full_path: &LangPath,
        match_cases: &[AstToken],
    ) -> LangResult<()> {
        // Gather all names of the members for the enum into a hash set.
        let enum_ = ast_ctx.get_adt(ty_env, full_path)?;
        let mut member_names = enum_
            .read()
            .members
            .iter()
            .map(|x| x.read().name.clone())
            .collect::<HashSet<_>>();

        // Go through all case expressions and remove any enum members with the
        // "same" name from the `member_names` set. When all cases have been
        // traversed, the `member_names` set should be empty if the cases covers
        // all enum members. If not, a error will be returned indicating that all
        // possible enum members aren't covered.
        //
        // If a "default" case block is found, all possibled match cases are known
        // to be covered and a early Ok return can be done.
        //
        // This loop will return error if any of the case expressions isn't a
        // "EnumAccess" since it isn't supported atm (should be in the future).
        for match_case in match_cases {
            if let AstToken::Block(Block {
                header: BlockHeader::MatchCase(Some(expr)),
                ..
            }) = match_case
            {
                if let Expr::Op(Op::UnOp(op)) = expr {
                    if let UnOperator::EnumAccess(name, ..) = &op.operator {
                        member_names.remove(name);
                        continue;
                    }
                }

                return Err(ast_ctx.err(format!(
                    "Expected match case expression to be enum member, was: {:#?}",
                    match_case
                )));
            } else if let AstToken::Block(Block {
                header: BlockHeader::MatchCase(None),
                ..
            }) = match_case
            {
                // "default" block found. This case handles all possible values,
                // this match case exhaust all possible cases/values.
                return Ok(());
            }
        }

        if member_names.is_empty() {
            Ok(())
        } else {
            Err(ast_ctx.err(format!(
                "\"match\" on enum \"{}\" at position:\n{:#?}\ndoes NOT cover all enum members. Missing members:\n{:#?}",
                &enum_.read().name, file_pos, member_names
            )))
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn exhaust_int(&self) -> LangResult<()> {
        // TODO: Implement, currently unable to do it unless every case expr is
        //       hardcoded, but then they would need to cover all possible ints
        //       for a specific bit size, which is unfeasible.
        Ok(())
    }
}

impl Visitor for MatchExhaustAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// The expression in match statements are compiled to int values, so they
    /// will need to be convertable to ints.
    /// Currently supported types:
    ///   ints
    ///   enums
    fn visit_match(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Match(match_expr),
            body: match_cases,
            ..
        } = block
        {
            if let Err(err) = self.exhaust(ctx, match_expr, match_cases) {
                self.errors.push(err);
            }
        }
    }
}
