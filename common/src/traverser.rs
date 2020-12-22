use crate::{
    error::{LangError, LangErrorKind::TraversalError},
    file::FilePosition,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::Expr,
        op::{Op, UnOperator},
        stmt::Stmt,
    },
    visitor::Visitor,
};

pub struct AstTraverser<'a> {
    visitors: Vec<&'a mut dyn Visitor>,
    traverse_context: TraverseContext,

    errors: Vec<LangError>,
}

// TODO: Add more context here.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TraverseContext {
    pub block_id: usize,
    pub parent_block_id: usize,

    // TODO: Should this contains file information about the parent as well?
    //       Ex. if this is a type, should the information about what this type
    //       is assigned to also be here?
    pub file_pos: FilePosition,
}

impl<'a> Default for AstTraverser<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Traverses the AST.
impl<'a> AstTraverser<'a> {
    pub fn new() -> Self {
        Self {
            visitors: Vec::default(),
            errors: Vec::default(),
            traverse_context: TraverseContext {
                block_id: 0,
                parent_block_id: usize::MAX,
                file_pos: FilePosition::default(),
            },
        }
    }

    pub fn add_visitor(&mut self, visitor: &'a mut dyn Visitor) -> &mut Self {
        self.visitors.push(visitor);
        self
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn take_errors(&mut self) -> Result<&mut Self, Vec<LangError>> {
        for visitor in self.visitors.iter_mut() {
            if let Some(ref mut visitor_errs) = visitor.take_errors() {
                self.errors.append(visitor_errs);
            }
        }

        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    pub fn traverse(&mut self, mut ast_token: &mut AstToken) -> &mut Self {
        if let Some(file_pos) = ast_token.file_pos() {
            self.traverse_context.file_pos = file_pos.clone();
        }

        for v in self.visitors.iter_mut() {
            v.visit_token(ast_token, &self.traverse_context);
        }

        // TODO: Can one move this into the match block below? Currently needed
        //       since can't borrow mut twice.
        if let AstToken::Block(_, id, _) = &ast_token {
            if self.traverse_context.block_id != *id {
                self.traverse_context.parent_block_id = self.traverse_context.block_id;
                self.traverse_context.block_id = *id;
            }
            self.traverse_block(ast_token);
        }

        match &mut ast_token {
            AstToken::Block(_, id, body) => {
                for body_token in body {
                    if self.traverse_context.block_id != *id {
                        self.traverse_context.parent_block_id = self.traverse_context.block_id;
                        self.traverse_context.block_id = *id;
                    }
                    self.traverse(body_token);
                }
            }
            AstToken::Expr(expr) => self.traverse_expr(expr),
            AstToken::Stmt(stmt) => self.traverse_stmt(stmt),
            AstToken::Empty => debug!("Visiting Empty block"),
            AstToken::EOF => {
                debug!("Visiting EOF");
                for v in self.visitors.iter_mut() {
                    v.visit_eof(ast_token, &self.traverse_context);
                }
            }
        }

        self
    }

    pub fn traverse_block(&mut self, mut ast_token: &mut AstToken) {
        if let Some(file_pos) = ast_token.file_pos() {
            self.traverse_context.file_pos = file_pos.clone();
        }

        debug!("Visiting block -- {:#?}", ast_token);
        for v in self.visitors.iter_mut() {
            v.visit_block(ast_token, &self.traverse_context);
        }

        match &mut ast_token {
            AstToken::Block(header, ..) => match header {
                BlockHeader::Default => {
                    debug!("Visiting default block");
                    for v in self.visitors.iter_mut() {
                        v.visit_default_block(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Function(_) => {
                    debug!("Visiting func");
                    for v in self.visitors.iter_mut() {
                        v.visit_func(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Struct(_) => {
                    debug!("Visiting struct");
                    for v in self.visitors.iter_mut() {
                        v.visit_struct(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Enum(_) => {
                    debug!("Visiting enum");
                    for v in self.visitors.iter_mut() {
                        v.visit_enum(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Interface(_) => {
                    debug!("Visiting interface");
                    for v in self.visitors.iter_mut() {
                        v.visit_interface(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Implement(_) => {
                    debug!("Visiting impl");
                    for v in self.visitors.iter_mut() {
                        v.visit_impl(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Anonymous => {
                    debug!("Visiting anon");
                    for v in self.visitors.iter_mut() {
                        v.visit_anon(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::If => {
                    debug!("Visiting if");
                    for v in self.visitors.iter_mut() {
                        v.visit_if(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::IfCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting if case");
                    for v in self.visitors.iter_mut() {
                        v.visit_if_case(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Match(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match");
                    for v in self.visitors.iter_mut() {
                        v.visit_match(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::MatchCase(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match case");
                    for v in self.visitors.iter_mut() {
                        v.visit_match_case(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::For(_, expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting for");
                    for v in self.visitors.iter_mut() {
                        v.visit_for(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::While(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting while");
                    for v in self.visitors.iter_mut() {
                        v.visit_while(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Test(_) => {
                    debug!("Visiting test");
                    for v in self.visitors.iter_mut() {
                        v.visit_test(ast_token, &self.traverse_context);
                    }
                }
            },
            _ => {
                let err = LangError::new(
                    format!(
                        "Expected block token when traversing block, got: {:?}",
                        ast_token
                    ),
                    TraversalError,
                );
                self.errors.push(err);
            }
        }
    }

    pub fn traverse_expr(&mut self, expr: &mut Expr) {
        if let Some(file_pos) = expr.file_pos() {
            self.traverse_context.file_pos = file_pos.clone();
        }

        debug!("Visiting expr -- {:#?}", expr);
        for v in self.visitors.iter_mut() {
            v.visit_expr(expr, &self.traverse_context)
        }

        match expr {
            Expr::Lit(..) => {
                debug!("Visiting lit");
                for v in self.visitors.iter_mut() {
                    v.visit_lit(expr, &self.traverse_context)
                }
            }
            Expr::Var(var) => {
                debug!("Visiting var");
                for v in self.visitors.iter_mut() {
                    v.visit_var(var, &self.traverse_context)
                }
            }
            Expr::FuncCall(func_call) => {
                for arg in &mut func_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting func call");
                for v in self.visitors.iter_mut() {
                    v.visit_func_call(func_call, &self.traverse_context)
                }
            }
            Expr::BuiltInCall(built_in_call) => {
                for arg in &mut built_in_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting built in call");
                for v in self.visitors.iter_mut() {
                    v.visit_built_in_call(built_in_call, &self.traverse_context)
                }
            }
            Expr::StructInit(struct_init) => {
                for arg in &mut struct_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting struct init");
                for v in self.visitors.iter_mut() {
                    v.visit_struct_init(struct_init, &self.traverse_context)
                }
            }
            Expr::ArrayInit(array_init) => {
                for arg in &mut array_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting array init");
                for v in self.visitors.iter_mut() {
                    v.visit_array_init(array_init, &self.traverse_context)
                }
            }
            Expr::Op(Op::BinOp(bin_op)) => {
                self.traverse_expr(&mut bin_op.lhs);
                self.traverse_expr(&mut bin_op.rhs);
                debug!("Visiting bin op");
                for v in self.visitors.iter_mut() {
                    v.visit_bin_op(bin_op, &self.traverse_context)
                }
            }
            Expr::Op(Op::UnOp(un_op)) => {
                self.traverse_expr(&mut un_op.value);
                // Edge case to traverse expr in array access, the only Op that
                // contains a expression.
                if let UnOperator::ArrayAccess(expr) = &mut un_op.operator {
                    self.traverse_expr(expr);
                }
                debug!("Visiting un op");
                for v in self.visitors.iter_mut() {
                    v.visit_un_op(un_op, &self.traverse_context)
                }
            }

            Expr::Type(..) => (),
        }
    }

    pub fn traverse_stmt(&mut self, stmt: &mut Stmt) {
        if let Some(file_pos) = stmt.file_pos() {
            self.traverse_context.file_pos = file_pos.clone();
        }

        debug!("Visiting stmt -- {:#?}", stmt);
        for v in self.visitors.iter_mut() {
            v.visit_stmt(stmt, &self.traverse_context)
        }

        match stmt {
            Stmt::Return(expr_opt, _) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting return");
                for v in self.visitors.iter_mut() {
                    v.visit_return(stmt, &self.traverse_context)
                }
            }
            Stmt::Yield(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting yield");
                for v in self.visitors.iter_mut() {
                    v.visit_yield(stmt, &self.traverse_context)
                }
            }
            Stmt::Break(_) => {
                debug!("Visiting break");
                for v in self.visitors.iter_mut() {
                    v.visit_break(stmt, &self.traverse_context)
                }
            }
            Stmt::Continue(_) => {
                debug!("Visiting continue");
                for v in self.visitors.iter_mut() {
                    v.visit_continue(stmt, &self.traverse_context)
                }
            }
            Stmt::Use(..) => {
                debug!("Visiting use");
                for v in self.visitors.iter_mut() {
                    v.visit_use(stmt, &self.traverse_context)
                }
            }
            Stmt::Package(..) => {
                debug!("Visiting package");
                for v in self.visitors.iter_mut() {
                    v.visit_package(stmt, &self.traverse_context)
                }
            }
            Stmt::Increment(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting increment");
                for v in self.visitors.iter_mut() {
                    v.visit_inc(stmt, &self.traverse_context)
                }
            }
            Stmt::Decrement(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting decrement");
                for v in self.visitors.iter_mut() {
                    v.visit_dec(stmt, &self.traverse_context)
                }
            }
            Stmt::Defer(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting defer");
                for v in self.visitors.iter_mut() {
                    v.visit_defer(stmt, &self.traverse_context)
                }
            }
            Stmt::DeferExec(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting defer exec");
                for v in self.visitors.iter_mut() {
                    v.visit_defer_exec(stmt, &self.traverse_context)
                }
            }
            Stmt::Assignment(_, lhs, rhs, _) => {
                self.traverse_expr(lhs);
                self.traverse_expr(rhs);
                debug!("Visiting assginment");
                for v in self.visitors.iter_mut() {
                    v.visit_assignment(stmt, &self.traverse_context)
                }
            }
            Stmt::VariableDecl(_, expr_opt, _) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting var decl");
                for v in self.visitors.iter_mut() {
                    v.visit_var_decl(stmt, &self.traverse_context)
                }
            }
            Stmt::ExternalDecl(..) => {
                debug!("Visiting extern decl");
                for v in self.visitors.iter_mut() {
                    v.visit_extern_decl(stmt, &self.traverse_context)
                }
            }
            Stmt::Modifier(_) => {
                debug!("Visiting modifier");
                for v in self.visitors.iter_mut() {
                    v.visit_modifier(stmt, &self.traverse_context)
                }
            }
        }
    }
}
