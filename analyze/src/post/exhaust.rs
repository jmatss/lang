use crate::AnalyzeContext;
use common::{
    error::CustomResult,
    error::LangError,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::Expr,
        op::{Op, UnOperator},
    },
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
    BlockId,
};
use std::collections::HashSet;

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
pub struct ExhaustAnalyzer<'a> {
    analyze_context: &'a AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> ExhaustAnalyzer<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn exhaust_enum(
        &mut self,
        ident: &str,
        block_id: BlockId,
        match_cases: &mut Vec<AstToken>,
        ctx: &TraverseContext,
    ) -> CustomResult<()> {
        // Gather all names of the members for the enum into a hash set.
        let enum_ = self.analyze_context.get_enum(ident, block_id)?;
        let mut member_names = if let Some(members) = &enum_.borrow().members {
            members
                .iter()
                .map(|x| x.borrow().name.clone())
                .collect::<HashSet<_>>()
        } else {
            HashSet::default()
        };

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

                return Err(self.analyze_context.err(format!(
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
            Err(self.analyze_context.err(format!(
                "\"match\" on enum \"{}\" at position:\n{:#?}\ndoes NOT cover all enum members. Missing members:\n{:#?}",
                &enum_.borrow().name, ctx.file_pos, member_names
            )))
        }
    }

    fn exhaust_int(&mut self, inner_ty: &InnerTy, ctx: &TraverseContext) -> CustomResult<()> {
        // TODO: Implement, currently unable to do it unless every case expr is
        //       hardcoded, but then they would need to cover all possible ints
        //       for a specific bit size, which is unfeasible.
        Ok(())
    }
}

impl<'a> Visitor for ExhaustAnalyzer<'a> {
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
    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Match(match_expr), _, _, match_cases) = ast_token {
            let match_case_ty = match match_expr.get_expr_type() {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            match &match_case_ty {
                Ty::CompoundType(InnerTy::Enum(ident), ..) => {
                    if let Err(err) = self.exhaust_enum(ident, ctx.block_id, match_cases, ctx) {
                        self.errors.push(err);
                    }
                }

                Ty::CompoundType(inner_ty, ..) if inner_ty.is_int() => {
                    if let Err(err) = self.exhaust_int(inner_ty, ctx) {
                        self.errors.push(err);
                    }
                }

                _ => {
                    let err = self.analyze_context.err(format!(
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
