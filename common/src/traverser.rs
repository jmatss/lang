use crate::{
    error::{LangError, LangErrorKind::TraversalError},
    token::ast::Token,
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
    visitor: &'a mut dyn Visitor,
    errors: Vec<LangError>,
}

/// Traverses the AST.
impl<'a> AstTraverser<'a> {
    // TODO: Change how this default visitor works.
    pub fn new(visitor: &'a mut dyn Visitor) -> Self {
        Self {
            visitor,
            errors: Vec::default(),
        }
    }

    pub fn set_visitor(&mut self, visitor: &'a mut dyn Visitor) -> &mut Self {
        self.visitor = visitor;
        self
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn take_errors(&mut self) -> Result<&mut Self, Vec<LangError>> {
        if let Some(ref mut visitor_errs) = self.visitor.take_errors() {
            self.errors.append(visitor_errs);
        }

        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    pub fn traverse(&mut self, ast_token: &mut AstToken) -> &mut Self {
        self.visitor.visit_token(ast_token);

        // TODO: Can i move this into the match block? Currently needed since
        //       can't borrow mut twice.
        if let Token::Block(..) = ast_token.token {
            self.traverse_block(ast_token);
        }

        match &mut ast_token.token {
            Token::Block(_, _, body) => {
                for body_token in body {
                    self.traverse(body_token);
                }
            }
            Token::Expr(expr) => self.traverse_expr(expr),
            Token::Stmt(stmt) => self.traverse_stmt(stmt),
            Token::EOF => {
                debug!("Visiting EOF");
                self.visitor.visit_eof(ast_token);
            }
        }

        self
    }

    pub fn traverse_block(&mut self, ast_token: &mut AstToken) {
        debug!("Visiting block -- {:#?}", ast_token);
        self.visitor.visit_block(ast_token);

        match &mut ast_token.token {
            Token::Block(header, ..) => match header {
                BlockHeader::Default => {
                    debug!("Visiting default block");
                    self.visitor.visit_default_block(ast_token);
                }
                BlockHeader::Function(_) => {
                    debug!("Visiting func");
                    self.visitor.visit_func(ast_token);
                }
                BlockHeader::Struct(_) => {
                    debug!("Visiting struct");
                    self.visitor.visit_struct(ast_token);
                }
                BlockHeader::Enum(_) => {
                    debug!("Visiting enum");
                    self.visitor.visit_enum(ast_token);
                }
                BlockHeader::Interface(_) => {
                    debug!("Visiting interface");
                    self.visitor.visit_interface(ast_token);
                }
                BlockHeader::Implement(_) => {
                    debug!("Visiting impl");
                    self.visitor.visit_impl(ast_token);
                }
                BlockHeader::Anonymous => {
                    debug!("Visiting anon");
                    self.visitor.visit_anon(ast_token);
                }
                BlockHeader::If => {
                    debug!("Visiting if");
                    self.visitor.visit_if(ast_token);
                }
                BlockHeader::IfCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting if case");
                    self.visitor.visit_if_case(ast_token);
                }
                BlockHeader::Match(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match");
                    self.visitor.visit_match(ast_token);
                }
                BlockHeader::MatchCase(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match case");
                    self.visitor.visit_match_case(ast_token);
                }
                BlockHeader::For(_, expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting for");
                    self.visitor.visit_for(ast_token);
                }
                BlockHeader::While(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting while");
                    self.visitor.visit_while(ast_token);
                }
                BlockHeader::Test(_) => {
                    debug!("Visiting test");
                    self.visitor.visit_test(ast_token);
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
        debug!("Visiting expr -- {:#?}", expr);
        self.visitor.visit_expr(expr);

        match expr {
            Expr::Lit(..) => {
                debug!("Visiting lit");
                self.visitor.visit_lit(expr);
            }
            Expr::Var(var) => {
                debug!("Visiting var");
                self.visitor.visit_var(var);
            }
            Expr::FuncCall(func_call) => {
                for arg in &mut func_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting func call");
                self.visitor.visit_func_call(func_call);
            }
            Expr::StructInit(struct_init) => {
                for arg in &mut struct_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting struct init");
                self.visitor.visit_struct_init(struct_init);
            }
            Expr::ArrayInit(array_init) => {
                for arg in &mut array_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting array init");
                self.visitor.visit_array_init(array_init);
            }
            Expr::Op(Op::BinOp(bin_op)) => {
                self.traverse_expr(&mut bin_op.lhs);
                self.traverse_expr(&mut bin_op.rhs);
                debug!("Visiting bin op");
                self.visitor.visit_bin_op(bin_op);
            }
            Expr::Op(Op::UnOp(un_op)) => {
                self.traverse_expr(&mut un_op.value);
                // Edge case to traverse expr in array access, the only Op that
                // contains a expression.
                if let UnOperator::ArrayAccess(expr) = &mut un_op.operator {
                    self.traverse_expr(expr);
                }
                debug!("Visiting un op");
                self.visitor.visit_un_op(un_op);
            }

            Expr::Type(_) => (),
        }
    }

    pub fn traverse_stmt(&mut self, stmt: &mut Stmt) {
        debug!("Visiting stmt -- {:#?}", stmt);
        self.visitor.visit_stmt(stmt);

        match stmt {
            Stmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting return");
                self.visitor.visit_return(stmt);
            }
            Stmt::Yield(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting yield");
                self.visitor.visit_yield(stmt);
            }
            Stmt::Break => {
                debug!("Visiting break");
                self.visitor.visit_break(stmt);
            }
            Stmt::Continue => {
                debug!("Visiting continue");
                self.visitor.visit_continue(stmt);
            }
            Stmt::Use(_) => {
                debug!("Visiting use");
                self.visitor.visit_use(stmt);
            }
            Stmt::Package(_) => {
                debug!("Visiting package");
                self.visitor.visit_package(stmt);
            }
            Stmt::Defer(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting defer");
                self.visitor.visit_defer(stmt);
            }
            Stmt::DeferExec(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting defer exec");
                self.visitor.visit_defer_exec(stmt);
            }
            Stmt::Assignment(_, lhs, rhs) => {
                self.traverse_expr(lhs);
                self.traverse_expr(rhs);
                debug!("Visiting assginment");
                self.visitor.visit_assignment(stmt);
            }
            Stmt::VariableDecl(_, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting var decl");
                self.visitor.visit_var_decl(stmt);
            }
            Stmt::ExternalDecl(_) => {
                debug!("Visiting extern decl");
                self.visitor.visit_extern_decl(stmt);
            }
            Stmt::Modifier(_) => {
                debug!("Visiting modifier");
                self.visitor.visit_modifier(stmt);
            }
        }
    }
}
