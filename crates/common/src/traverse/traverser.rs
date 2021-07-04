use std::{
    sync::{Arc, RwLock},
    time::Instant,
};

use log::debug;

use crate::{
    error::{LangError, LangErrorKind},
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::Expr,
        op::{Op, UnOperator},
        stmt::Stmt,
    },
    ty::{get::get_exprs_mut, type_id::TypeId},
};

use super::{traverse_ctx::TraverseCtx, visitor::Visitor};

/// Traverses AST nodes staring from the node `ast_token` with the context found
/// in `ctx`.  For every AST node traversed, `visitor` will be called depending
/// on what node type it is.
///
/// The context will be reset both before and after the traversal. This is done
/// to ensure that a previous traversal doesn't affect the result of this new
/// traversal.
pub fn traverse<V>(
    ctx: &mut TraverseCtx,
    visitor: &mut V,
    ast_token: &mut AstToken,
) -> Result<(), Vec<LangError>>
where
    V: Visitor,
{
    traverse_priv(ctx, visitor, ast_token, None)
}

pub fn traverse_with_deep_copy<V>(
    ctx: &mut TraverseCtx,
    visitor: &mut V,
    ast_token: &mut AstToken,
    deep_copy_nr: usize,
) -> Result<(), Vec<LangError>>
where
    V: Visitor,
{
    traverse_priv(ctx, visitor, ast_token, Some(deep_copy_nr))
}

fn traverse_priv<V>(
    ctx: &mut TraverseCtx,
    visitor: &mut V,
    ast_token: &mut AstToken,
    deep_copy_nr: Option<usize>,
) -> Result<(), Vec<LangError>>
where
    V: Visitor,
{
    ctx.reset();
    let start_timer = Instant::now();
    let result = AstTraverser::new(ctx, visitor)
        .set_deep_copy_nr(deep_copy_nr)
        .traverse_token_with_end(ast_token)
        .take_errors();
    debug!("Traverse time: {:?}", start_timer.elapsed());
    ctx.reset();
    result
}

/// A struct used to traverse AST nodes. This traverser will recursively traverse
/// a given AST node and call functions on the given `self.visitor` depending
/// on the node type that it is visiting(/traversing).
struct AstTraverser<'a, 'ctx, V: Visitor> {
    /// Context regarding the AST that this traverse will keep track of. This
    /// will be given to `self.visitor` during the traversal to provide more
    /// context and also a way to "communicate" between Visitor and Traverser.
    ctx: &'a mut TraverseCtx<'ctx>,

    /// The struct that wants to traverse the AST with this traverser.
    visitor: &'a mut V,

    errors: Vec<LangError>,
}

impl<'a, 'ctx, V: Visitor> AstTraverser<'a, 'ctx, V> {
    fn new(ctx: &'a mut TraverseCtx<'ctx>, visitor: &'a mut V) -> Self {
        Self {
            ctx,
            visitor,
            errors: Vec::default(),
        }
    }

    fn set_deep_copy_nr(&mut self, copy_nr: Option<usize>) -> &mut Self {
        self.ctx.copy_nr = copy_nr;
        self
    }

    pub fn take_errors(&mut self) -> Result<(), Vec<LangError>> {
        if let Some(errors) = self.visitor.take_errors(&mut self.ctx) {
            Err(errors)
        } else {
            Ok(())
        }
    }

    fn traverse_token_with_end(&mut self, ast_token: &mut AstToken) -> &mut Self {
        self.traverse_token(ast_token);
        if !self.ctx.stop {
            self.visitor.visit_end(&mut self.ctx);
        }
        self
    }

    fn traverse_token(&mut self, mut ast_token: &mut AstToken) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        if !self.ctx.stop {
            self.visitor.visit_token(ast_token, &mut self.ctx);
        } else {
            return;
        }

        match &mut ast_token {
            AstToken::Block(..) => {
                self.traverse_block(ast_token);
                if let AstToken::Block(.., id, body) = ast_token {
                    for body_token in body {
                        self.ctx.block_id = *id;
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
                if !self.ctx.stop {
                    self.visitor.visit_eof(ast_token, &mut self.ctx);
                } else {
                    return;
                }
            }
        }

        self.ctx.file_pos = old_pos;
    }

    fn traverse_block(&mut self, mut ast_token: &mut AstToken) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = ast_token.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        if let AstToken::Block(.., id, _) = ast_token {
            self.ctx.block_id = *id;
        }

        debug!("Visiting block -- {:#?}", ast_token);
        if !self.ctx.stop {
            self.visitor.visit_block(ast_token, &mut self.ctx);
        } else {
            return;
        }

        match &mut ast_token {
            AstToken::Block(header, ..) => match header {
                BlockHeader::Default => {
                    debug!("Visiting default block");
                    if !self.ctx.stop {
                        self.visitor.visit_default_block(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Fn(func) => {
                    if let Some(deep_copy_nr) = self.ctx.copy_nr {
                        let mut new_func = func.as_ref().read().unwrap().clone();

                        if let Some(params) = &mut new_func.parameters {
                            for param in params {
                                let mut new_param = param.as_ref().read().unwrap().clone();
                                new_param.set_copy_nr(deep_copy_nr);
                                *param = Arc::new(RwLock::new(new_param));
                            }
                        }

                        *func = Arc::new(RwLock::new(new_func));
                    }

                    // TODO: Iterate through the `generics`.
                    if let Some(params) = &mut func.as_ref().write().unwrap().parameters {
                        for param in params {
                            if let Some(type_id) = &mut param.as_ref().write().unwrap().ty {
                                self.traverse_type(type_id);
                            }
                            if let Some(value) = &mut param.as_ref().write().unwrap().value {
                                self.traverse_expr(value);
                            }

                            // Iterate through the parameters of functions as
                            // variable declarations. One have to temporary wrap
                            // them in a `Stmt::VariableDecl` for it to work
                            // smoothly.
                            let file_pos = param.as_ref().read().unwrap().file_pos.to_owned();
                            let mut var_decl = Stmt::VariableDecl(Arc::clone(param), file_pos);
                            if !self.ctx.stop {
                                self.visitor.visit_var_decl(&mut var_decl, &mut self.ctx);
                            } else {
                                return;
                            }
                        }
                    }

                    if let Some(generic_impls) = &mut func.as_ref().write().unwrap().generics {
                        for ty in generic_impls.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(ret_type_id) = &mut func.as_ref().write().unwrap().ret_type {
                        self.traverse_type(ret_type_id);
                    }

                    if let Some(adt_type_id) = &mut func.as_ref().write().unwrap().method_adt {
                        self.traverse_type(adt_type_id);
                    }

                    debug!("Visiting func");
                    if !self.ctx.stop {
                        self.visitor.visit_fn(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Struct(struct_) => {
                    if let Some(deep_copy_nr) = self.ctx.copy_nr {
                        let mut new_struct = struct_.as_ref().read().unwrap().clone();

                        for member in new_struct.members.iter_mut() {
                            let mut new_member = member.as_ref().read().unwrap().clone();
                            new_member.set_copy_nr(deep_copy_nr);
                            *member = Arc::new(RwLock::new(new_member));
                        }

                        *struct_ = Arc::new(RwLock::new(new_struct));
                    }

                    // TODO: Visit `implements`?
                    for member in struct_.as_ref().write().unwrap().members.iter_mut() {
                        if let Some(deep_copy_nr) = self.ctx.copy_nr {
                            let mut new_member = member.as_ref().read().unwrap().clone();
                            new_member.set_copy_nr(deep_copy_nr);
                            *member = Arc::new(RwLock::new(new_member));
                        }

                        if let Some(ty) = &mut member.as_ref().write().unwrap().ty {
                            self.traverse_type(ty);
                        }
                        if let Some(value) = &mut member.as_ref().write().unwrap().value {
                            self.traverse_expr(value);
                        }
                    }

                    if let Some(gens) = &mut struct_.as_ref().write().unwrap().generics {
                        for ty in gens.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(impls) = &mut struct_.as_ref().write().unwrap().implements {
                        for tys in impls.values_mut() {
                            for ty in tys {
                                self.traverse_type(ty);
                            }
                        }
                    }

                    debug!("Visiting struct");
                    if !self.ctx.stop {
                        self.visitor.visit_struct(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Enum(enum_) => {
                    if let Some(deep_copy_nr) = self.ctx.copy_nr {
                        let mut new_enum = enum_.as_ref().read().unwrap().clone();

                        for member in new_enum.members.iter_mut() {
                            let mut new_member = member.as_ref().read().unwrap().clone();
                            new_member.set_copy_nr(deep_copy_nr);
                            *member = Arc::new(RwLock::new(new_member));
                        }

                        *enum_ = Arc::new(RwLock::new(new_enum));
                    }

                    // TODO: Visit possible generics?
                    for member in enum_.as_ref().write().unwrap().members.iter_mut() {
                        if let Some(ty) = &mut member.as_ref().write().unwrap().ty {
                            self.traverse_type(ty);
                        }
                    }

                    debug!("Visiting enum");
                    if !self.ctx.stop {
                        self.visitor.visit_enum(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Union(union) => {
                    if let Some(deep_copy_nr) = self.ctx.copy_nr {
                        let mut new_union = union.as_ref().read().unwrap().clone();

                        for member in new_union.members.iter_mut() {
                            let mut new_member = member.as_ref().read().unwrap().clone();
                            new_member.set_copy_nr(deep_copy_nr);
                            *member = Arc::new(RwLock::new(new_member));
                        }

                        *union = Arc::new(RwLock::new(new_union));
                    }

                    for member in union.as_ref().write().unwrap().members.iter_mut() {
                        if let Some(deep_copy_nr) = self.ctx.copy_nr {
                            let mut new_member = member.as_ref().read().unwrap().clone();
                            new_member.set_copy_nr(deep_copy_nr);
                            *member = Arc::new(RwLock::new(new_member));
                        }

                        if let Some(ty) = &mut member.as_ref().write().unwrap().ty {
                            self.traverse_type(ty);
                        }
                        if let Some(value) = &mut member.as_ref().write().unwrap().value {
                            self.traverse_expr(value);
                        }
                    }

                    if let Some(gens) = &mut union.as_ref().write().unwrap().generics {
                        for ty in gens.iter_types_mut() {
                            self.traverse_type(ty);
                        }
                    }

                    if let Some(impls) = &mut union.as_ref().write().unwrap().implements {
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
                    if !self.ctx.stop {
                        self.visitor.visit_union(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Trait(_) => {
                    // TODO: Visit containing methods?

                    debug!("Visiting trait");
                    if !self.ctx.stop {
                        self.visitor.visit_trait(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Implement(..) => {
                    debug!("Visiting impl");
                    if !self.ctx.stop {
                        self.visitor.visit_impl(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Anonymous => {
                    debug!("Visiting anon");
                    if !self.ctx.stop {
                        self.visitor.visit_anon(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::If => {
                    debug!("Visiting if");
                    if !self.ctx.stop {
                        self.visitor.visit_if(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::IfCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting if case");
                    if !self.ctx.stop {
                        self.visitor.visit_if_case(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Match(expr) => {
                    self.traverse_expr(expr);
                    debug!("Visiting match");
                    if !self.ctx.stop {
                        self.visitor.visit_match(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::MatchCase(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting match case");
                    if !self.ctx.stop {
                        self.visitor.visit_match_case(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::For(var, expr) => {
                    if let Some(ty) = &mut var.ty {
                        self.traverse_type(ty);
                    }

                    self.traverse_expr(expr);
                    debug!("Visiting for");
                    if !self.ctx.stop {
                        self.visitor.visit_for(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::While(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.traverse_expr(expr);
                    }
                    debug!("Visiting while");
                    if !self.ctx.stop {
                        self.visitor.visit_while(ast_token, &mut self.ctx);
                    } else {
                        return;
                    }
                }
                BlockHeader::Test(func) => {
                    if let Some(deep_copy_nr) = self.ctx.copy_nr {
                        if let Some(params) = &mut func.parameters {
                            for param in params {
                                let mut new_param = param.as_ref().read().unwrap().clone();
                                new_param.set_copy_nr(deep_copy_nr);
                                *param = Arc::new(RwLock::new(new_param));
                            }
                        }
                    }

                    // TODO: Iterate through the `generics`.
                    if let Some(params) = &mut func.parameters {
                        for param in params {
                            if let Some(ty) = &mut param.as_ref().write().unwrap().ty {
                                self.traverse_type(ty);
                            }
                        }
                    }

                    if let Some(ret_ty) = &mut func.ret_type {
                        self.traverse_type(ret_ty);
                    }

                    debug!("Visiting test");
                    if !self.ctx.stop {
                        self.visitor.visit_test(ast_token, &mut self.ctx);
                    } else {
                        return;
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

    fn traverse_expr(&mut self, expr: &mut Expr) {
        let old_pos = self.ctx.file_pos.to_owned();
        if let Some(file_pos) = expr.file_pos() {
            self.ctx.file_pos = file_pos.to_owned();
        }

        match expr {
            Expr::Lit(..) => {
                debug!("Visiting lit");
                if !self.ctx.stop {
                    self.visitor.visit_lit(expr, &mut self.ctx);
                } else {
                    return;
                }
            }
            Expr::Var(var) => {
                if let Some(value) = &mut var.value {
                    self.traverse_expr(value);
                }

                debug!("Visiting var");
                if !self.ctx.stop {
                    self.visitor.visit_var(var, &mut self.ctx);
                } else {
                    return;
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
                if !self.ctx.stop {
                    self.visitor.visit_fn_call(fn_call, &mut self.ctx);
                } else {
                    return;
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
                if !self.ctx.stop {
                    self.visitor.visit_fn_ptr(expr, &mut self.ctx);
                } else {
                    return;
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
                if !self.ctx.stop {
                    self.visitor
                        .visit_built_in_call(built_in_call, &mut self.ctx);
                } else {
                    return;
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
                if !self.ctx.stop {
                    self.visitor.visit_adt_init(adt_init, &mut self.ctx);
                } else {
                    return;
                }
            }
            Expr::ArrayInit(array_init) => {
                for arg in &mut array_init.arguments {
                    self.traverse_expr(&mut arg.value);
                }
                debug!("Visiting array init");
                if !self.ctx.stop {
                    self.visitor.visit_array_init(array_init, &mut self.ctx);
                } else {
                    return;
                }
            }
            Expr::Op(Op::BinOp(bin_op)) => {
                self.traverse_expr(&mut bin_op.lhs);
                self.traverse_expr(&mut bin_op.rhs);

                debug!("Visiting bin op");
                if !self.ctx.stop {
                    self.visitor.visit_bin_op(bin_op, &mut self.ctx);
                } else {
                    return;
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
                if !self.ctx.stop {
                    self.visitor.visit_un_op(un_op, &mut self.ctx);
                } else {
                    return;
                }
            }
            Expr::Type(ty, ..) => self.traverse_type(ty),
        }

        self.ctx.file_pos = old_pos;

        if let Ok(ty) = expr.get_expr_type_mut() {
            self.traverse_type(ty);
        }

        debug!("Visiting expr -- {:#?}", expr);
        if !self.ctx.stop {
            self.visitor.visit_expr(expr, &mut self.ctx);
        } else {
            return;
        }

        self.ctx.file_pos = old_pos;
    }

    fn traverse_stmt(&mut self, stmt: &mut Stmt) {
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
                if !self.ctx.stop {
                    self.visitor.visit_return(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Yield(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting yield");
                if !self.ctx.stop {
                    self.visitor.visit_yield(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Break(_) => {
                debug!("Visiting break");
                if !self.ctx.stop {
                    self.visitor.visit_break(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Continue(_) => {
                debug!("Visiting continue");
                if !self.ctx.stop {
                    self.visitor.visit_continue(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Use(..) => {
                debug!("Visiting use");
                if !self.ctx.stop {
                    self.visitor.visit_use(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Module(..) => {
                debug!("Visiting package");
                if !self.ctx.stop {
                    self.visitor.visit_package(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Defer(expr, _) => {
                self.traverse_expr(expr);
                debug!("Visiting defer");
                if !self.ctx.stop {
                    self.visitor.visit_defer(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::DeferExec(expr) => {
                self.traverse_expr(expr);
                debug!("Visiting defer exec");
                if !self.ctx.stop {
                    self.visitor.visit_defer_exec(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::Assignment(_, lhs, rhs, _) => {
                self.traverse_expr(lhs);
                self.traverse_expr(rhs);
                debug!("Visiting assignment");
                if !self.ctx.stop {
                    self.visitor.visit_assignment(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::VariableDecl(var, _) => {
                if let Some(deep_copy_nr) = self.ctx.copy_nr {
                    let mut new_var = var.as_ref().read().unwrap().clone();
                    new_var.set_copy_nr(deep_copy_nr);
                    *var = Arc::new(RwLock::new(new_var));
                }

                let unsafe_var_value = unsafe {
                    ((&mut var.as_ref().write().unwrap().value) as *mut Option<Box<Expr>>)
                        .as_mut()
                        .unwrap()
                };
                if let Some(value) = unsafe_var_value {
                    self.traverse_expr(value);
                }

                if let Some(ty) = &mut var.as_ref().write().unwrap().ty {
                    self.traverse_type(ty);
                }

                debug!("Visiting var decl");
                if !self.ctx.stop {
                    self.visitor.visit_var_decl(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
            Stmt::ExternalDecl(..) => {
                debug!("Visiting extern decl");
                if !self.ctx.stop {
                    self.visitor.visit_extern_decl(stmt, &mut self.ctx);
                } else {
                    return;
                }
            }
        }

        self.ctx.file_pos = old_pos;

        debug!("Visiting stmt -- {:#?}", stmt);
        if !self.ctx.stop {
            self.visitor.visit_stmt(stmt, &mut self.ctx);
        } else {
            return;
        }

        self.ctx.file_pos = old_pos;
    }

    fn traverse_type(&mut self, type_id: &mut TypeId) {
        if !self.ctx.stop {
            // TODO: Need to traverse the types of expression nested in types,
            //       bot is not safe to do. This is a temporary work around,
            //       in the future this need to be changed. Could ex. store the
            //       expression found in types in a separate data-structure and
            //       only store IDs/references to those expression inside the
            //       types.
            let ty_exprs = match get_exprs_mut(&mut self.ctx.ty_env.lock().unwrap(), *type_id) {
                Ok(ty_exprs) => ty_exprs,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            for ty_expr in ty_exprs {
                self.traverse_expr(ty_expr);
            }

            debug!("Visiting type -- {}", type_id);
            self.visitor.visit_type(type_id, &mut self.ctx);

            // TODO: Does the types need to be traversed recursively?
            /*
            // TODO: Make safe.
            let unsafe_self = unsafe { (self as *mut AstTraverser<V>).as_mut().unwrap() };

            match TY_INTERNER.lock().unwrap().ty_mut(*type_id).unwrap() {
                Ty::Pointer(type_id_i, ..)
                | Ty::UnknownAdtMember(type_id_i, ..)
                | Ty::UnknownMethodGeneric(type_id_i, ..)
                | Ty::UnknownArrayMember(type_id_i, ..) => {
                    unsafe_self.traverse_type(type_id_i);
                }

                Ty::CompoundType(inner_ty, gens, ..) => {
                    if let Some(path) = inner_ty.get_ident_mut() {
                        if let Some(path_gens) =
                            path.last_mut().map(|part| part.1.as_mut()).flatten()
                        {
                            for gen_type_id in path_gens.iter_types_mut() {
                                unsafe_self.traverse_type(gen_type_id);
                            }
                        }
                    }

                    for gen_type_id in gens.iter_types_mut() {
                        unsafe_self.traverse_type(gen_type_id);
                    }
                }

                Ty::Array(type_id_i, expr_opt, ..) => {
                    unsafe_self.traverse_type(type_id_i);

                    if let Some(expr) = expr_opt {
                        unsafe_self.traverse_expr(expr);
                    }
                }

                Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                    for type_id_i in gens.iter_mut().chain(params) {
                        unsafe_self.traverse_type(type_id_i);
                    }

                    if let Some(ret_type_id) = ret_type_id_opt {
                        unsafe_self.traverse_type(ret_type_id);
                    }
                }

                Ty::Expr(expr, ..) => {
                    unsafe_self.traverse_expr(expr);
                }

                Ty::UnknownAdtMethod(type_id_i, _, gen_tys, ..)
                | Ty::UnknownMethodArgument(type_id_i, _, gen_tys, ..) => {
                    unsafe_self.traverse_type(type_id_i);

                    for gen_type_id in gen_tys.iter_mut() {
                        unsafe_self.traverse_type(gen_type_id);
                    }
                }

                Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
            }
            */
        }
    }
}
