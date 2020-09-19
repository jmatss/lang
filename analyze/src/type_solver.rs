use crate::type_context::{SubResult, TypeContext};
use common::{
    error::LangError,
    token::op::UnOperator,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    types::Type,
    visitor::Visitor,
};

pub struct TypeSolver<'a> {
    type_context: &'a mut TypeContext<'a>,
    errors: Vec<LangError>,
}

impl<'a> TypeSolver<'a> {
    pub fn new(type_context: &'a mut TypeContext<'a>) -> Self {
        Self {
            type_context,
            errors: Vec::default(),
        }
    }

    fn subtitute_type(&mut self, ty: &mut Type, finalize: bool) {
        match self.type_context.get_substitution(ty, finalize) {
            SubResult::Solved(sub_ty) => {
                *ty = sub_ty;
            }
            SubResult::UnSolved(un_sub_ty) => {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to resolve type {:?}. Got back unsolved: {:?}.",
                    ty, un_sub_ty
                ));
                self.errors.push(err);
            }
            SubResult::Err(err) => {
                self.errors.push(err);
            }
        }
    }
}

impl<'a> Visitor for TypeSolver<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// There will always be a single default block that wraps everything.
    /// So this code will be ran at the start only once. It will make sure that
    /// all expressions with implicit types had their types inferred correctly;
    /// otherwise a error will be reported.
    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        // TODO: How to check that all types have been inferred?
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        self.type_context.analyze_context.cur_line_nr = ast_token.line_nr;
        self.type_context.analyze_context.cur_column_nr = ast_token.column_nr;
    }

    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    /// Iterate through all expressions and substitute all Unknown types that
    /// was used temporarily during the type inference stage. They should be
    /// now be replaced with "real" types that can be found in the `substitutions`.
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
        // Do the substitute for "Type" here, all other exprs will make the subs
        // in their own visit funcs.
        if let Expr::Type(ty) = expr {
            self.subtitute_type(ty, true);
        }
    }

    fn visit_lit(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
        if let Expr::Lit(_, Some(ty)) = expr {
            self.subtitute_type(ty, true);
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to find infer type for lit: {:?}", expr));
            self.errors.push(err);
        }
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {
        if let Some(ty) = &mut var.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to find infer type for var: {:?}", var));
            self.errors.push(err);
        }
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        if let Some(ty) = &mut func_call.ret_type {
            // Insert the, now known, struct name into the func call if this is
            // a method call.
            if func_call.is_method {
                if let Some(this_arg) = func_call.arguments.first_mut() {
                    match this_arg.value.get_expr_type_mut() {
                        Ok(Type::Pointer(struct_ty)) => {
                            if let Type::Custom(struct_name) = struct_ty.as_ref() {
                                func_call.method_struct = Some(struct_name.clone());
                            } else {
                                let err = self.type_context.analyze_context.err(format!(
                                    "First argument of method call \"{}\" not a pointer to struct type (\"this\"/\"self\"): {:?}",
                                    &func_call.name, this_arg
                                ));
                                self.errors.push(err);
                                return;
                            }
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                        _ => {
                            let err = self.type_context.analyze_context.err(format!(
                                "First argument of method call \"{}\" not a pointer to struct type (\"this\"/\"self\"): {:?}",
                                &func_call.name, this_arg
                            ));
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Method call \"{}\" has no arguments, expected atleast \"this\"/\"self\".",
                        &func_call.name
                    ));
                    self.errors.push(err);
                    return;
                }
            }

            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for func call: {:?}",
                func_call
            ));
            self.errors.push(err);
        }
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        if let Some(ty) = &mut struct_init.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for struct init: {:?}",
                struct_init
            ));
            self.errors.push(err);
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &TraverseContext) {
        if let Some(ty) = &mut array_init.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for array init: {:?}",
                array_init
            ));
            self.errors.push(err);
        }
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &TraverseContext) {
        if let Some(ty) = &mut bin_op.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for bin op ret_type: {:?}",
                bin_op
            ));
            self.errors.push(err);
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {
        if let Some(ty) = &mut un_op.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for un op ret_type: {:?}",
                un_op
            ));
            self.errors.push(err);
        }

        // Edge case logic for struct access. Need to figure out the index
        // of the member that is being accessed.
        if let UnOperator::StructAccess(member_name, member_idx, member_ty) = &mut un_op.operator {
            *member_ty = un_op.ret_type.clone();

            match un_op.value.get_expr_type_mut() {
                Ok(Type::Custom(struct_name)) => {
                    match self.type_context.analyze_context.get_struct_member_index(
                        struct_name,
                        member_name,
                        ctx.block_id,
                    ) {
                        Ok(idx) => *member_idx = Some(idx),
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }

                Err(err) => {
                    self.errors.push(err);
                }
                _ => {
                    let err = self.type_context.analyze_context.err(format!(
                        "Expression that was struct accessed wasn't struct, was: {:?}",
                        un_op.value
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    // TODO: Need to make sure that the global var in `analyze_context` has its
    //       type updated as well.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, expr_opt) = stmt {
            // Update type for rhs of var decl if it exists.
            if expr_opt.is_some() {
                match self.type_context.get_expr_type_opt(expr_opt.as_mut()) {
                    Ok(expr_ty) => match self.type_context.get_substitution(expr_ty, true) {
                        SubResult::Solved(sub_ty) => {
                            *expr_ty = sub_ty;
                        }
                        SubResult::UnSolved(un_sub_ty) => {
                            let err = self.type_context.analyze_context.err(format!(
                                "Unable to resolve type {:?} for rhs of var decl: {:?}. Got back unsolved type: {:?}.",
                                expr_ty, var, un_sub_ty
                            ));
                            self.errors.push(err);
                            return;
                        }
                        SubResult::Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    },
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            }

            // Update type for lhs of var decl.
            if let Some(ty) = &mut var.ret_type {
                match self.type_context.get_substitution(ty, true) {
                    SubResult::Solved(sub_ty) => {
                        *ty = sub_ty;
                    }
                    SubResult::UnSolved(un_sub_ty) => {
                        let err = self.type_context.analyze_context.err(format!(
                            "Unable to resolve type {:?} for lhs of var decl. Got back unsolved type: {:?}.",
                            ty, un_sub_ty
                        ));
                        self.errors.push(err);
                        return;
                    }
                    SubResult::Err(mut err) => {
                        err.msg.push_str(&format!(" -- {:#?}.", var));
                        self.errors.push(err);
                        return;
                    }
                }
            } else {
                let err = self
                    .type_context
                    .analyze_context
                    .err(format!("Unable to get ret type for var: {}.", &var.name));
                self.errors.push(err);
                return;
            }

            let decl_block_id = match self
                .type_context
                .analyze_context
                .get_var_decl_scope(&var.name, ctx.block_id)
            {
                Ok(decl_block_id) => decl_block_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let key = (var.name.clone(), decl_block_id);

            // Get the substitution type for the variable in the `analyze_context`.
            let sub_type = if let Some(context_var) =
                self.type_context.analyze_context.variables.get(&key)
            {
                if let Some(ty) = &context_var.ret_type {
                    match self.type_context.get_substitution(ty, true) {
                        SubResult::Solved(sub_ty) => sub_ty,
                        SubResult::UnSolved(un_sub_ty) => {
                            let err = self.type_context.analyze_context.err(format!(
                                    "Unable to resolve type {:?} for context var: {:?}. Got back unsolved type: {:?}.",
                                    ty, context_var, un_sub_ty
                                ));
                            self.errors.push(err);
                            return;
                        }
                        SubResult::Err(mut err) => {
                            err.msg.push_str(&format!(" -- {:#?}.", stmt));
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Unable to get ret type for var in analyze context with key: {:?}.",
                        &key
                    ));
                    self.errors.push(err);
                    return;
                }
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to find var with name \"{}\" in decl block {}.",
                    &var.name, decl_block_id
                ));
                self.errors.push(err);
                return;
            };

            // Update the type to the var in `analyze_context`.
            if let Some(context_var) = self.type_context.analyze_context.variables.get_mut(&key) {
                if let Some(ty) = &mut context_var.ret_type {
                    *ty = sub_type;
                }
            }
        }
    }

    fn visit_func(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_yield(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_match_case(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_anon(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_if(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_if_case(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_for(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_while(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_test(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_break(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_continue(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_use(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_package(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_defer(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_defer_exec(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_modifier(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}
}
