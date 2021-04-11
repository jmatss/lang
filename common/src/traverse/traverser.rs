use std::{cell::RefCell, rc::Rc, time::Instant};

use crate::{
    ctx::{ast_ctx::AstCtx, traverse_ctx::TraverseCtx, ty_ctx::TyCtx},
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
    TypeId,
};

use super::visitor::Visitor;

pub struct AstTraverser<'a> {
    visitors: Vec<&'a mut dyn Visitor>,
    ctx: TraverseCtx<'a>,

    errors: Vec<LangError>,
}

/// Traverses the AST.
impl<'a> AstTraverser<'a> {
    pub fn new(ast_ctx: &'a mut AstCtx, ty_ctx: &'a mut TyCtx) -> Self {
        Self {
            visitors: Vec::default(),
            errors: Vec::default(),
            ctx: TraverseCtx {
                ast_ctx,
                ty_ctx,
                deep_copy: false,
                copy_nr: None,
                block_id: 0,
                file_pos: FilePosition::default(),
                stop: false,
            },
        }
    }

    pub fn from_ctx(traverse_ctx: &'a mut TraverseCtx) -> Self {
        Self::new(&mut traverse_ctx.ast_ctx, &mut traverse_ctx.ty_ctx)
    }

    pub fn get_ctx(&mut self) -> &mut TraverseCtx<'a> {
        &mut self.ctx
    }

    pub fn add_visitor(&mut self, visitor: &'a mut dyn Visitor) -> &mut Self {
        self.visitors.push(visitor);
        self
    }

    pub fn clear_visitors(&mut self) -> &mut Self {
        self.visitors.clear();
        self
    }

    /// A convenient help function equivalent to calling the chain of commands:
    ///
    /// ```no_run
    /// traverser.add_visitor(v).traverse_token(t).take_errors()?.clear_visitors()
    /// ```
    pub fn traverse_with_visitor(
        &mut self,
        visitor: &'a mut dyn Visitor,
        ast_token: &mut AstToken,
    ) -> Result<&mut Self, Vec<LangError>> {
        let timer = Instant::now();
        let result = self
            .add_visitor(visitor)
            .traverse_token(ast_token)
            .take_errors();
        debug!("Elapsed: {:?}", timer.elapsed());
        Ok(result?.clear_visitors())
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn set_deep_copy(&mut self, deep_copy: bool) -> &mut Self {
        self.ctx.deep_copy = deep_copy;
        self
    }

    pub fn set_deep_copy_nr(&mut self, copy_nr: usize) -> &mut Self {
        self.ctx.copy_nr = Some(copy_nr);
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

    pub fn take_errors_with_ctx(&mut self) -> Result<&mut Self, Vec<LangError>> {
        for visitor in self.visitors.iter_mut() {
            if let Some(ref mut visitor_errs) = visitor.take_errors_with_ctx(&mut self.ctx) {
                self.errors.append(visitor_errs);
            }
        }

        if self.errors.is_empty() {
            Ok(self)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    pub fn traverse_token(&mut self, ast_token: &mut AstToken) -> &mut Self {
        self.traverse_token_priv(ast_token);
        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return self;
            }
            v.visit_end(&mut self.ctx);
        }
        self
    }

    fn traverse_token_priv(&mut self, mut ast_token: &mut AstToken) -> &mut Self {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return self;
            }
            v.visit_token(ast_token, &mut self.ctx);
        }

        match &mut ast_token {
            AstToken::Block(..) => {
                self.traverse_block(ast_token);
                if let AstToken::Block(.., id, body) = ast_token {
                    for body_token in body {
                        self.ctx.block_id = *id;
                        self.traverse_token_priv(body_token);
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
                    if self.ctx.stop {
                        return self;
                    }
                    v.visit_eof(ast_token, &mut self.ctx);
                }
            }
        }

        self.ctx.file_pos = old_pos;

        self
    }

    pub fn traverse_block(&mut self, mut ast_token: &mut AstToken) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        if let AstToken::Block(.., id, _) = ast_token {
            self.ctx.block_id = *id;
        }

        debug!("Visiting block -- {:#?}", ast_token);
        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return;
            }
            v.visit_block(ast_token, &mut self.ctx);
        }

        match &mut ast_token {
            AstToken::Block(header, ..) => match header {
                BlockHeader::Default => {
                    debug!("Visiting default block");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_default_block(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Fn(func) => {
                    if self.ctx.deep_copy {
                        let mut new_func = func.borrow().clone();

                        if let Some(params) = &mut new_func.parameters {
                            for param in params {
                                let mut new_param = param.borrow().clone();
                                new_param.set_copy_nr(self.ctx.copy_nr.unwrap());
                                *param = Rc::new(RefCell::new(new_param));
                            }
                        }

                        *func = Rc::new(RefCell::new(new_func));
                    }

                    // TODO: Iterate through the `generics`.
                    if let Some(params) = &mut func.borrow_mut().parameters {
                        for param in params {
                            if let Some(type_id) = &mut param.borrow_mut().ty {
                                self.traverse_type(type_id);
                            }
                            if let Some(value) = &mut param.borrow_mut().value {
                                self.traverse_expr(value);
                            }

                            // Iterate through the parameters of functions as
                            // variable declarations. One have to temporary wrap
                            // them in a `Stmt::VariableDecl` for it to work
                            // smoothly.
                            let file_pos = param.borrow().file_pos.to_owned();
                            let mut var_decl = Stmt::VariableDecl(Rc::clone(param), file_pos);
                            for v in self.visitors.iter_mut() {
                                if self.ctx.stop {
                                    return;
                                }
                                v.visit_var_decl(&mut var_decl, &mut self.ctx);
                            }
                        }
                    }

                    if let Some(generic_impls) = &mut func.borrow_mut().generics {
                        for ty in generic_impls.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(ret_type_id) = &mut func.borrow_mut().ret_type {
                        self.traverse_type(ret_type_id);
                    }

                    if let Some(adt_type_id) = &mut func.borrow_mut().method_adt {
                        self.traverse_type(adt_type_id);
                    }

                    debug!("Visiting func");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_fn(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Struct(struct_) => {
                    if self.ctx.deep_copy {
                        let mut new_struct = struct_.borrow().clone();

                        for member in new_struct.members.iter_mut() {
                            let mut new_member = member.borrow().clone();
                            new_member.set_copy_nr(self.ctx.copy_nr.unwrap());
                            *member = Rc::new(RefCell::new(new_member));
                        }

                        *struct_ = Rc::new(RefCell::new(new_struct));
                    }

                    // TODO: Visit `implements`?
                    for member in struct_.borrow_mut().members.iter_mut() {
                        if self.ctx.deep_copy {
                            let mut new_member = member.borrow().clone();
                            new_member.set_copy_nr(self.ctx.copy_nr.unwrap());
                            *member = Rc::new(RefCell::new(new_member));
                        }

                        if let Some(ty) = &mut member.borrow_mut().ty {
                            self.traverse_type(ty);
                        }
                        if let Some(value) = &mut member.borrow_mut().value {
                            self.traverse_expr(value);
                        }
                    }

                    if let Some(gens) = &mut struct_.borrow_mut().generics {
                        for ty in gens.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(impls) = &mut struct_.borrow_mut().implements {
                        for tys in impls.values_mut() {
                            for ty in tys {
                                self.traverse_type(ty);
                            }
                        }
                    }

                    debug!("Visiting struct");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_struct(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Enum(enum_) => {
                    if self.ctx.deep_copy {
                        let mut new_enum = enum_.borrow().clone();

                        for member in new_enum.members.iter_mut() {
                            let mut new_member = member.borrow().clone();
                            new_member.set_copy_nr(self.ctx.copy_nr.unwrap());
                            *member = Rc::new(RefCell::new(new_member));
                        }

                        *enum_ = Rc::new(RefCell::new(new_enum));
                    }

                    // TODO: Visit possible generics?
                    for member in enum_.borrow_mut().members.iter_mut() {
                        if let Some(ty) = &mut member.borrow_mut().ty {
                            self.traverse_type(ty);
                        }
                    }

                    debug!("Visiting enum");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_enum(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Union(union) => {
                    if self.ctx.deep_copy {
                        let mut new_union = union.borrow().clone();

                        for member in new_union.members.iter_mut() {
                            let mut new_member = member.borrow().clone();
                            new_member.set_copy_nr(self.ctx.copy_nr.unwrap());
                            *member = Rc::new(RefCell::new(new_member));
                        }

                        *union = Rc::new(RefCell::new(new_union));
                    }

                    for member in union.borrow_mut().members.iter_mut() {
                        if self.ctx.deep_copy {
                            let mut new_member = member.borrow().clone();
                            new_member.set_copy_nr(self.ctx.copy_nr.unwrap());
                            *member = Rc::new(RefCell::new(new_member));
                        }

                        if let Some(ty) = &mut member.borrow_mut().ty {
                            self.traverse_type(ty);
                        }
                        if let Some(value) = &mut member.borrow_mut().value {
                            self.traverse_expr(value);
                        }
                    }

                    if let Some(gens) = &mut union.borrow_mut().generics {
                        for ty in gens.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(impls) = &mut union.borrow_mut().implements {
                        for tys in impls.values_mut() {
                            for ty in tys {
                                if self.ctx.stop {
                                    return;
                                }
                                self.traverse_type(ty);
                            }
                        }
                    }

                    debug!("Visiting union");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_union(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Trait(_) => {
                    // TODO: Visit containing methods?

                    debug!("Visiting trait");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_trait(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Implement(..) => {
                    debug!("Visiting impl");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_impl(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Anonymous => {
                    debug!("Visiting anon");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_anon(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::If => {
                    debug!("Visiting if");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_if(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::IfCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting if case");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_if_case(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Match(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_match(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::MatchCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting match case");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_match_case(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::For(var, expr) => {
                    if let Some(ty) = &mut var.ty {
                        self.traverse_type(ty);
                    }

                    self.traverse_expr(expr);
                    debug!("Visiting for");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_for(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::While(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting while");
                    for v in self.visitors.iter_mut() {
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_while(ast_token, &mut self.ctx);
                    }
                }
                BlockHeader::Test(func) => {
                    if self.ctx.deep_copy {
                        if let Some(params) = &mut func.parameters {
                            for param in params {
                                let mut new_param = param.borrow().clone();
                                new_param.set_copy_nr(self.ctx.copy_nr.unwrap());
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
                        if self.ctx.stop {
                            return;
                        }
                        v.visit_test(ast_token, &mut self.ctx);
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
                    Some(self.ctx.file_pos),
                );
                self.errors.push(err);
            }
        }

        self.ctx.file_pos = old_pos;
    }

    pub fn traverse_expr(&mut self, expr: &mut Expr) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = expr.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        match expr {
            Expr::Lit(..) => {
                debug!("Visiting lit");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_lit(expr, &mut self.ctx);
                }
            }
            Expr::Var(var) => {
                if let Some(value) = &mut var.value {
                    self.traverse_expr(value);
                }

                debug!("Visiting var");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_var(var, &mut self.ctx);
                }
            }
            Expr::FnCall(fn_call) => {
                if let Some(gen_tys) = &mut fn_call.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty)
                    }
                }

                if let Some(ty) = &mut fn_call.method_adt {
                    self.traverse_type(ty);
                }

                for arg in &mut fn_call.arguments {
                    self.traverse_expr(&mut arg.value);
                }

                debug!("Visiting fn call");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_fn_call(fn_call, &mut self.ctx);
                }
            }
            Expr::FnPtr(fn_ptr) => {
                if let Some(gen_tys) = &mut fn_ptr.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty);
                    }
                }

                if let Some(ty) = &mut fn_ptr.fn_ty {
                    self.traverse_type(ty);
                }

                debug!("Visiting fn ptr");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_fn_ptr(expr, &mut self.ctx);
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
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_built_in_call(built_in_call, &mut self.ctx);
                }
            }
            Expr::AdtInit(adt_init) => {
                if let Some(gen_tys) = &mut adt_init.generics {
                    for gen_ty in gen_tys.iter_types_mut() {
                        self.traverse_type(gen_ty)
                    }
                }

                for arg in &mut adt_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }

                debug!("Visiting ADT init");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_adt_init(adt_init, &mut self.ctx);
                }
            }
            Expr::ArrayInit(array_init) => {
                for arg in &mut array_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting array init");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_array_init(array_init, &mut self.ctx);
                }
            }
            Expr::Op(Op::BinOp(bin_op)) => {
                self.traverse_expr(&mut bin_op.lhs);
                self.traverse_expr(&mut bin_op.rhs);

                debug!("Visiting bin op");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_bin_op(bin_op, &mut self.ctx);
                }
            }
            Expr::Op(Op::UnOp(un_op)) => {
                self.traverse_expr(&mut un_op.value);

                // Edge case to traverse expr in ArrayAccess and stmt in UnionIs.
                if let UnOperator::ArrayAccess(expr) = &mut un_op.operator {
                    self.traverse_expr(expr);
                } else if let UnOperator::UnionIs(_, stmt) = &mut un_op.operator {
                    self.traverse_stmt(stmt);
                }

                debug!("Visiting un op");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_un_op(un_op, &mut self.ctx);
                }
            }
            Expr::Type(ty, ..) => self.traverse_type(ty),
        }

        self.ctx.file_pos = old_pos;

        if let Ok(ty) = expr.get_expr_type_mut() {
            self.traverse_type(ty);
        }

        debug!("Visiting expr -- {:#?}", expr);
        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return;
            }
            v.visit_expr(expr, &mut self.ctx);
        }

        self.ctx.file_pos = old_pos;
    }

    pub fn traverse_stmt(&mut self, stmt: &mut Stmt) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = stmt.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        match stmt {
            Stmt::Return(expr_opt, _) => {
                if let Some(expr) = expr_opt {
                    self.traverse_expr(expr);
                }
                debug!("Visiting return");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_return(stmt, &mut self.ctx);
                }
            }
            Stmt::Yield(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting yield");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_yield(stmt, &mut self.ctx);
                }
            }
            Stmt::Break(_) => {
                debug!("Visiting break");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_break(stmt, &mut self.ctx);
                }
            }
            Stmt::Continue(_) => {
                debug!("Visiting continue");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_continue(stmt, &mut self.ctx);
                }
            }
            Stmt::Use(..) => {
                debug!("Visiting use");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_use(stmt, &mut self.ctx);
                }
            }
            Stmt::Module(..) => {
                debug!("Visiting package");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_package(stmt, &mut self.ctx);
                }
            }
            Stmt::Increment(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting increment");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_inc(stmt, &mut self.ctx);
                }
            }
            Stmt::Decrement(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting decrement");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_dec(stmt, &mut self.ctx);
                }
            }
            Stmt::Defer(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting defer");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_defer(stmt, &mut self.ctx);
                }
            }
            Stmt::DeferExec(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting defer exec");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_defer_exec(stmt, &mut self.ctx);
                }
            }
            Stmt::Assignment(_, lhs, rhs, _) => {
                self.traverse_expr(lhs);
                self.traverse_expr(rhs);
                debug!("Visiting assignment");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_assignment(stmt, &mut self.ctx);
                }
            }
            Stmt::VariableDecl(var, _) => {
                if self.ctx.deep_copy {
                    let mut new_var = var.borrow().clone();
                    new_var.set_copy_nr(self.ctx.copy_nr.unwrap());
                    *var = Rc::new(RefCell::new(new_var));
                }

                if let Some(value) = &mut var.borrow_mut().value {
                    self.traverse_expr(value);
                }

                if let Some(ty) = &mut var.borrow_mut().ty {
                    self.traverse_type(ty);
                }

                debug!("Visiting var decl");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_var_decl(stmt, &mut self.ctx);
                }
            }
            Stmt::ExternalDecl(..) => {
                debug!("Visiting extern decl");
                for v in self.visitors.iter_mut() {
                    if self.ctx.stop {
                        return;
                    }
                    v.visit_extern_decl(stmt, &mut self.ctx);
                }
            }
        }

        self.ctx.file_pos = old_pos;

        debug!("Visiting stmt -- {:#?}", stmt);
        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return;
            }
            v.visit_stmt(stmt, &mut self.ctx);
        }

        self.ctx.file_pos = old_pos;
    }

    pub fn traverse_type(&mut self, id: &mut TypeId) {
        // TODO: Does the FilePosition need to be updated?

        // TODO: Make safe.
        if let Ok(exprs) = self.ctx.ty_ctx.ty_env.get_exprs_mut(*id) {
            let mut unsafe_exprs = Vec::with_capacity(exprs.len());
            for expr in exprs {
                let unsafe_expr = unsafe { (expr as *mut Expr).as_mut().unwrap() };
                unsafe_exprs.push(unsafe_expr);
            }
            for expr in unsafe_exprs {
                self.traverse_expr(expr);
            }
        }

        debug!("Visiting type -- {:#?}", id);
        for v in self.visitors.iter_mut() {
            if self.ctx.stop {
                return;
            }
            v.visit_type(id, &mut self.ctx);
        }
    }
}
