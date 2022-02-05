use std::collections::HashMap;

use crate::{
    cf_visitor::ControlFlowVisitor,
    error::{LangError, LangErrorKind::TraversalError},
    token::ast::Token,
    token::{
        ast::AstToken,
        block::BlockHeader,
        block::Function,
        expr::Expr,
        op::{Op, UnOperator},
        stmt::Stmt,
    },
    BlockId,
};

pub struct ControlFlowTraverser<'a> {
    visitors: Vec<&'a mut dyn ControlFlowVisitor>,
    traverse_context: ControlFlowTraverseContext,

    errors: Vec<LangError>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ControlFlowTraverseContext {
    pub block_id: usize,
    pub parent_block_id: usize,

    pub func_id: BlockId,

    /// Contains a map of the paths that have been traversed.
    /// The "usize" key is the "branch" number/id, so the first if/switch
    /// statement found on the current path will be given id 0, the next 1 etc.
    /// The value vector is which paths have been taken at that specific branch
    /// point.
    pub paths: HashMap<usize, Vec<bool>>,

    // TODO: Explanation.
    pub contains_more_paths: bool,
}

impl<'a> Default for ControlFlowTraverser<'a> {
    fn default() -> Self {
        Self::new()
    }
}

/// Traverses the AST.
impl<'a> ControlFlowTraverser<'a> {
    pub fn new() -> Self {
        Self {
            visitors: Vec::default(),
            errors: Vec::default(),
            traverse_context: ControlFlowTraverseContext {
                block_id: 0,
                parent_block_id: usize::MAX,
                func_id: 0,
                paths: HashMap::default(),
                contains_more_paths: false,
            },
        }
    }

    pub fn add_visitor(&mut self, visitor: &'a mut dyn ControlFlowVisitor) -> &mut Self {
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

    pub fn traverse(&mut self, ast_token: &mut AstToken) -> &mut Self {
        loop {
            match &mut ast_token.token {
                Token::Block(header, id, body) => {
                    if self.traverse_context.block_id != *id {
                        self.traverse_context.parent_block_id = self.traverse_context.block_id;
                        self.traverse_context.block_id = *id;
                    }
                    match header {
                        BlockHeader::Function(func) => {
                            self.traverse_context.func_id = *id;
                            self.traverse_func(func, body);
                        }
                        _ => {
                            for body_token in body {
                                self.traverse(body_token);
                            }
                        }
                    }
                }
                Token::EOF => break,
                _ => (),
            }
        }

        self
    }

    pub fn traverse_func(&mut self, func: &mut Function, body: &mut Vec<AstToken>) {
        debug!("Visiting func -- {:#?}", func);
        for v in self.visitors.iter_mut() {
            v.visit_fn_start(func, &self.traverse_context);
        }

        for body_token in body {
            debug!("Visiting token -- {:#?}", body_token);
            for v in self.visitors.iter_mut() {
                v.visit_token(body_token, &self.traverse_context);
            }

            match body_token.token {
                Token::Expr(expr) => match expr {
                    Expr::Lit(_, _) => {}
                    Expr::Type(_) => {}
                    Expr::Var(_) => {}
                    Expr::FuncCall(_) => {}
                    Expr::StructInit(_) => {}
                    Expr::ArrayInit(_) => {}
                    Expr::Op(_) => {}
                },
                Token::Stmt(stmt) => match stmt {
                    Stmt::Return(_) => {}
                    Stmt::Yield(_) => {}
                    Stmt::Break => {}
                    Stmt::Continue => {}
                    Stmt::Use(_) => {}
                    Stmt::Package(_) => {}
                    Stmt::Increment(_) => {}
                    Stmt::Decrement(_) => {}
                    Stmt::Defer(_) => {}
                    Stmt::DeferExec(_) => {}
                    Stmt::Assignment(_, _, _) => {}
                    Stmt::VariableDecl(_, _) => {}
                    Stmt::ExternalDecl(_) => {}
                    Stmt::Modifier(_) => {}
                },
                Token::Block(_, _, _) => {}
                Token::EOF => {}
            }
        }

        for v in self.visitors.iter_mut() {
            v.visit_fn_end(func, &self.traverse_context);
        }
    }

    pub fn traverse_expr(&mut self, expr: &mut Expr) {
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
            Expr::FnCall(fn_call) => {
                for arg in &mut fn_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting fn call");
                for v in self.visitors.iter_mut() {
                    v.visit_fn_call(fn_call, &self.traverse_context)
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

            Expr::Type(_) => (),
        }
    }

    pub fn traverse_stmt(&mut self, stmt: &mut Stmt) {
        debug!("Visiting stmt -- {:#?}", stmt);
        for v in self.visitors.iter_mut() {
            v.visit_stmt(stmt, &self.traverse_context)
        }

        match stmt {
            Stmt::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting return");
                for v in self.visitors.iter_mut() {
                    v.visit_return(stmt, &self.traverse_context)
                }
            }
            Stmt::Yield(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting yield");
                for v in self.visitors.iter_mut() {
                    v.visit_yield(stmt, &self.traverse_context)
                }
            }
            Stmt::Break => {
                debug!("Visiting break");
                for v in self.visitors.iter_mut() {
                    v.visit_break(stmt, &self.traverse_context)
                }
            }
            Stmt::Continue => {
                debug!("Visiting continue");
                for v in self.visitors.iter_mut() {
                    v.visit_continue(stmt, &self.traverse_context)
                }
            }
            Stmt::Use(_) => {
                debug!("Visiting use");
                for v in self.visitors.iter_mut() {
                    v.visit_use(stmt, &self.traverse_context)
                }
            }
            Stmt::Package(_) => {
                debug!("Visiting package");
                for v in self.visitors.iter_mut() {
                    v.visit_package(stmt, &self.traverse_context)
                }
            }
            Stmt::Increment(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting increment");
                for v in self.visitors.iter_mut() {
                    v.visit_inc(stmt, &self.traverse_context)
                }
            }
            Stmt::Decrement(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting decrement");
                for v in self.visitors.iter_mut() {
                    v.visit_dec(stmt, &self.traverse_context)
                }
            }
            Stmt::Defer(expr) => {
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
            Stmt::Assignment(_, lhs, rhs) => {
                self.traverse_expr(lhs);
                self.traverse_expr(rhs);
                debug!("Visiting assginment");
                for v in self.visitors.iter_mut() {
                    v.visit_assignment(stmt, &self.traverse_context)
                }
            }
            Stmt::VariableDecl(_, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting var decl");
                for v in self.visitors.iter_mut() {
                    v.visit_var_decl(stmt, &self.traverse_context)
                }
            }
            Stmt::ExternalDecl(_) => {
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
