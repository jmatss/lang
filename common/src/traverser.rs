use crate::{
    error::{LangError, LangErrorKind},
    file::FilePosition,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::Expr,
        op::{Op, UnOperator},
        stmt::Stmt,
    },
    ty::ty::Ty,
    visitor::Visitor,
};
use std::{cell::RefCell, rc::Rc};

pub struct AstTraverser<'a> {
    visitors: Vec<&'a mut dyn Visitor>,
    traverse_context: TraverseContext,

    errors: Vec<LangError>,
}

// TODO: Add more context here.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TraverseContext {
    /// Indicates if any found shared references (ex. RefCount) should be "deep"
    /// copied before the traverser visits it. This comes in handy when working
    /// with generics for example, where you want to make transformations to
    /// different instances.
    pub deep_copy: bool,

    /// A number that is used to mark copies so that they can be uniquely identified.
    /// This will only be used if `deep_copy` is set to true.
    pub copy_nr: Option<usize>,

    pub block_id: usize,

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
                deep_copy: false,
                copy_nr: None,
                block_id: 0,
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

    pub fn set_deep_copy(&mut self, deep_copy: bool) -> &mut Self {
        self.traverse_context.deep_copy = deep_copy;
        self
    }

    pub fn set_deep_copy_nr(&mut self, copy_nr: usize) -> &mut Self {
        self.traverse_context.copy_nr = Some(copy_nr);
        self
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

    pub fn traverse_token(&mut self, mut ast_token: &mut AstToken) -> &mut Self {
        let old_pos = self.traverse_context.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.traverse_context.file_pos = file_pos.to_owned();
        }

        for v in self.visitors.iter_mut() {
            v.visit_token(ast_token, &self.traverse_context);
        }

        match &mut ast_token {
            AstToken::Block(..) => {
                self.traverse_block(ast_token);
                if let AstToken::Block(.., id, body) = ast_token {
                    for body_token in body {
                        self.traverse_context.block_id = *id;
                        self.traverse_token(body_token);
                    }
                }
            }
            AstToken::Expr(expr) => self.traverse_expr(expr),
            AstToken::Stmt(stmt) => self.traverse_stmt(stmt),
            AstToken::Comment(msg, ..) => debug!("Visiting Comment block: {}", msg),
            AstToken::Empty => debug!("Visiting Empty block"),
            AstToken::EOF => {
                debug!("Visiting EOF");
                for v in self.visitors.iter_mut() {
                    v.visit_eof(ast_token, &self.traverse_context);
                }
            }
        }

        self.traverse_context.file_pos = old_pos;

        self
    }

    pub fn traverse_block(&mut self, mut ast_token: &mut AstToken) {
        let old_pos = self.traverse_context.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.traverse_context.file_pos = file_pos.to_owned();
        }

        if let AstToken::Block(.., id, _) = ast_token {
            self.traverse_context.block_id = *id;
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
                BlockHeader::Function(func) => {
                    if self.traverse_context.deep_copy {
                        let mut new_func = func.borrow().clone();

                        if let Some(params) = &mut new_func.parameters {
                            for param in params {
                                let mut new_param = param.borrow().clone();
                                new_param.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *param = Rc::new(RefCell::new(new_param));
                            }
                        }

                        *func = Rc::new(RefCell::new(new_func));
                    }

                    // TODO: Iterate through the `generics`.
                    if let Some(params) = &mut func.borrow_mut().parameters {
                        for param in params {
                            if let Some(ty) = &mut param.borrow_mut().ty {
                                self.traverse_type(ty);
                            }
                            if let Some(value) = &mut param.borrow_mut().value {
                                self.traverse_expr(value);
                            }

                            // Iterate through the parametersa of functions as
                            // variable declarations. One have to temporary wrap
                            // them in a `Stmt::VariableDecl` for it to work
                            // smootyly.
                            let file_pos = param.borrow().file_pos.to_owned();
                            let mut var_decl = Stmt::VariableDecl(Rc::clone(param), file_pos);
                            for v in self.visitors.iter_mut() {
                                v.visit_var_decl(&mut var_decl, &self.traverse_context);
                            }
                        }
                    }

                    if let Some(ret_ty) = &mut func.borrow_mut().ret_type {
                        self.traverse_type(ret_ty);
                    }

                    debug!("Visiting func");
                    for v in self.visitors.iter_mut() {
                        v.visit_func(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Struct(struct_) => {
                    if self.traverse_context.deep_copy {
                        let mut new_struct = struct_.borrow().clone();

                        if let Some(members) = &mut new_struct.members {
                            for member in members {
                                let mut new_member = member.borrow().clone();
                                new_member.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *member = Rc::new(RefCell::new(new_member));
                            }
                        }

                        *struct_ = Rc::new(RefCell::new(new_struct));
                    }

                    // TODO: Visit `implements` and possible generics?
                    if let Some(members) = &mut struct_.borrow_mut().members {
                        for member in members {
                            if self.traverse_context.deep_copy {
                                let mut new_member = member.borrow().clone();
                                new_member.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *member = Rc::new(RefCell::new(new_member));
                            }

                            if let Some(ty) = &mut member.borrow_mut().ty {
                                self.traverse_type(ty);
                            }
                            if let Some(value) = &mut member.borrow_mut().value {
                                self.traverse_expr(value);
                            }
                        }
                    }

                    debug!("Visiting struct");
                    for v in self.visitors.iter_mut() {
                        v.visit_struct(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Enum(enum_) => {
                    if self.traverse_context.deep_copy {
                        let mut new_enum = enum_.borrow().clone();

                        if let Some(members) = &mut new_enum.members {
                            for member in members {
                                let mut new_member = member.borrow().clone();
                                new_member.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *member = Rc::new(RefCell::new(new_member));
                            }
                        }

                        *enum_ = Rc::new(RefCell::new(new_enum));
                    }

                    // TODO: Visit possible generics?
                    if let Some(members) = &mut enum_.borrow_mut().members {
                        for member in members {
                            if let Some(ty) = &mut member.borrow_mut().ty {
                                self.traverse_type(ty);
                            }
                        }
                    }

                    debug!("Visiting enum");
                    for v in self.visitors.iter_mut() {
                        v.visit_enum(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::Interface(interface) => {
                    if self.traverse_context.deep_copy {
                        let mut new_interface = interface.borrow().clone();

                        if let Some(members) = &mut new_interface.members {
                            for member in members {
                                let mut new_member = member.borrow().clone();
                                new_member.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *member = Rc::new(RefCell::new(new_member));
                            }
                        }

                        *interface = Rc::new(RefCell::new(new_interface));
                    }

                    // TODO: Visit possible generics?
                    if let Some(members) = &mut interface.borrow_mut().members {
                        for member in members {
                            if let Some(ty) = &mut member.borrow_mut().ty {
                                self.traverse_type(ty);
                            }
                        }
                    }

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
                BlockHeader::MatchCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting match case");
                    for v in self.visitors.iter_mut() {
                        v.visit_match_case(ast_token, &self.traverse_context);
                    }
                }
                BlockHeader::For(var, expr) => {
                    if let Some(ty) = &mut var.ty {
                        self.traverse_type(ty);
                    }

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
                BlockHeader::Test(func) => {
                    if self.traverse_context.deep_copy {
                        if let Some(params) = &mut func.parameters {
                            for param in params {
                                let mut new_param = param.borrow().clone();
                                new_param.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                                *param = Rc::new(RefCell::new(new_param));
                            }
                        }
                    }

                    // TODO: Iterate through the `generics`.
                    if let Some(params) = &mut func.parameters {
                        for param in params {
                            if let Some(ty) = &mut param.borrow_mut().ty {
                                self.traverse_type(ty);
                            }
                        }
                    }

                    if let Some(ret_ty) = &mut func.ret_type {
                        self.traverse_type(ret_ty);
                    }

                    debug!("Visiting test");
                    for v in self.visitors.iter_mut() {
                        v.visit_test(ast_token, &self.traverse_context);
                    }
                }
            },
            _ => {
                // TODO: Is it possible that `self.traverse_context.file_pos`
                //       contains a old FilePosition at this point.
                let err = LangError::new(
                    format!(
                        "Expected block token when traversing block, got: {:?}",
                        ast_token
                    ),
                    LangErrorKind::GeneralError,
                    Some(self.traverse_context.file_pos),
                );
                self.errors.push(err);
            }
        }

        self.traverse_context.file_pos = old_pos;
    }

    pub fn traverse_expr(&mut self, expr: &mut Expr) {
        let old_pos = self.traverse_context.file_pos.to_owned();
        if let Some(file_pos) = expr.file_pos() {
            self.traverse_context.file_pos = file_pos.to_owned();
        }

        if let Ok(ty) = expr.get_expr_type_mut() {
            self.traverse_type(ty)
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
                if let Some(value) = &mut var.value {
                    self.traverse_expr(value);
                }

                debug!("Visiting var");
                for v in self.visitors.iter_mut() {
                    v.visit_var(var, &self.traverse_context)
                }
            }
            Expr::FuncCall(func_call) => {
                if let Some(gen_tys) = &mut func_call.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty)
                    }
                }

                if let Some(ty) = &mut func_call.method_structure {
                    self.traverse_type(ty);
                }

                for arg in &mut func_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }

                debug!("Visiting func call");
                for v in self.visitors.iter_mut() {
                    v.visit_func_call(func_call, &self.traverse_context)
                }
            }
            Expr::BuiltInCall(built_in_call) => {
                if let Some(gen_tys) = &mut built_in_call.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty)
                    }
                }

                for arg in &mut built_in_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }

                debug!("Visiting built in call");
                for v in self.visitors.iter_mut() {
                    v.visit_built_in_call(built_in_call, &self.traverse_context)
                }
            }
            Expr::StructInit(struct_init) => {
                if let Some(gen_tys) = &mut struct_init.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty)
                    }
                }

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
                // Edge case to traverse expr in ArrayAccess, the only Op that
                // contains a expression.
                if let UnOperator::ArrayAccess(expr) = &mut un_op.operator {
                    self.traverse_expr(expr);
                }

                debug!("Visiting un op");
                for v in self.visitors.iter_mut() {
                    v.visit_un_op(un_op, &self.traverse_context)
                }
            }
            Expr::Type(ty, ..) => self.traverse_type(ty),
        }

        self.traverse_context.file_pos = old_pos;
    }

    pub fn traverse_stmt(&mut self, stmt: &mut Stmt) {
        let old_pos = self.traverse_context.file_pos.to_owned();
        if let Some(file_pos) = stmt.file_pos() {
            self.traverse_context.file_pos = file_pos.to_owned();
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
                debug!("Visiting assignment");
                for v in self.visitors.iter_mut() {
                    v.visit_assignment(stmt, &self.traverse_context)
                }
            }
            Stmt::VariableDecl(var, _) => {
                if self.traverse_context.deep_copy {
                    let mut new_var = var.borrow().clone();
                    new_var.set_copy_nr(self.traverse_context.copy_nr.unwrap());
                    *var = Rc::new(RefCell::new(new_var));
                }

                if let Some(value) = &mut var.borrow_mut().value {
                    self.traverse_expr(value);
                }

                if let Some(ty) = &mut var.borrow_mut().ty {
                    self.traverse_type(ty)
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

        self.traverse_context.file_pos = old_pos;
    }

    pub fn traverse_type(&mut self, ty: &mut Ty) {
        // TODO: Does the FilePosition need to be updated?

        debug!("Visiting type -- {:#?}", ty);
        for v in self.visitors.iter_mut() {
            v.visit_type(ty, &self.traverse_context)
        }

        if let Some(exprs) = ty.get_exprs_mut() {
            for expr in exprs {
                self.traverse_expr(expr);
            }
        }
    }
}
