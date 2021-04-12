use std::collections::HashSet;

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::LangError,
    error::LangResult,
    path::LangPath,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::Expr,
        op::{Op, UnOperator},
    },
    traverse::visitor::Visitor,
    ty::{inner_ty::InnerTy, ty::Ty},
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
pub struct ExhaustAnalyzer {
    errors: Vec<LangError>,
}

impl ExhaustAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn exhaust_enum(
        &self,
        ctx: &mut TraverseCtx,
        full_path: &LangPath,
        match_cases: &[AstToken],
    ) -> LangResult<()> {
        // Gather all names of the members for the enum into a hash set.
        let enum_ = ctx.ast_ctx.get_adt(&ctx.ty_ctx, full_path)?;
        let mut member_names = enum_
            .borrow()
            .members
            .iter()
            .map(|x| x.borrow().name.clone())
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
            if let AstToken::Block(BlockHeader::MatchCase(Some(expr)), ..) = match_case {
                if let Expr::Op(Op::UnOp(op)) = expr {
                    if let UnOperator::EnumAccess(name, ..) = &op.operator {
                        member_names.remove(name);
                        continue;
                    }
                }

                return Err(ctx.ast_ctx.err(format!(
                    "Expected match case expression to be enum member, was: {:#?}",
                    match_case
                )));
            } else if let AstToken::Block(BlockHeader::MatchCase(None), ..) = match_case {
                // "default" block found. This case handles all possible values,
                // this match case exhaust all possible cases/values.
                return Ok(());
            }
        }

        if member_names.is_empty() {
            Ok(())
        } else {
            Err(ctx.ast_ctx.err(format!(
                "\"match\" on enum \"{}\" at position:\n{:#?}\ndoes NOT cover all enum members. Missing members:\n{:#?}",
                &enum_.borrow().name, ctx.file_pos, member_names
            )))
        }
    }

    fn exhaust_int(&mut self, ctx: &mut TraverseCtx, inner_ty: &InnerTy) -> LangResult<()> {
        // TODO: Implement, currently unable to do it unless every case expr is
        //       hardcoded, but then they would need to cover all possible ints
        //       for a specific bit size, which is unfeasible.
        Ok(())
    }
}

impl Visitor for ExhaustAnalyzer {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
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
    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Match(match_expr), _, _, match_cases) = ast_token {
            let type_id = match match_expr.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let match_case_ty = match ctx.ty_ctx.ty_env.ty(type_id) {
                Ok(ty) => ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            match match_case_ty {
                Ty::CompoundType(InnerTy::Enum(partial_path), ..) => {
                    let full_path = match ctx.ast_ctx.calculate_adt_full_path(
                        &ctx.ty_ctx,
                        &partial_path,
                        ctx.block_id,
                    ) {
                        Ok(full_path) => full_path,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if let Err(err) = self.exhaust_enum(ctx, &full_path, match_cases) {
                        self.errors.push(err);
                    }
                }

                Ty::CompoundType(inner_ty, ..) if inner_ty.is_int() => {
                    if let Err(err) = self.exhaust_int(ctx, &inner_ty) {
                        self.errors.push(err);
                    }
                }

                _ => {
                    let err = ctx.ast_ctx.err(format!(
                        "Invalid type of match expression: {:#?}",
                        match_expr
                    ));
                    self.errors.push(err);
                    return;
                }
            }
        }
    }
}
