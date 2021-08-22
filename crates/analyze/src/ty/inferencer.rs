use std::{
    borrow::Borrow,
    sync::{Arc, RwLock},
};

use log::{debug, log_enabled, Level};

use common::{
    error::LangError,
    token::{
        block::{Block, BlockHeader, Fn},
        expr::{AdtInit, ArrayInit, BuiltInCall, Expr, FnCall, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        inner_ty::InnerTy, substitution_sets::sub_sets_debug_print, to_string::to_string_type_id,
        ty::Ty, type_id::TypeId, type_info::TypeInfo,
    },
};

use super::{
    inf::{
        adt::infer_adt_gens,
        adt::infer_adt_init,
        arr_init::infer_array_init,
        built_in::infer_built_in,
        func::{infer_fn, infer_fn_ptr},
        func_call::infer_fn_call,
        lit::infer_lit,
        op::{infer_bin_op, infer_un_op},
        var::{infer_assignment, infer_var, infer_var_decl},
    },
    solve::insert_constraint,
};

/// Infers types for exprs that doesn't have a type explicitly set.
/// For more information about the algorithm, see:
///   http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
///
/// This struct only runs the first step of the algorithm which assigns temp
/// types to expressions and populates the "substitution" sets in "TypeContext"
/// that will be used to convert those temp types to "real" ones.
/// That conversion is done in another step by the "TypeSolver".
pub struct TypeInferencer {
    /// Keep a copy of the current function which body is being traversed.
    /// This will let the statements/exprs etc. inside the function know
    /// about the types of the parameters and the return type.
    cur_func: Option<Arc<RwLock<Fn>>>,

    /// Contains the current match expression. Its type needs to be the same
    /// as the type in the match cases.
    cur_match_expr: Option<Expr>,

    errors: Vec<LangError>,
}

impl TypeInferencer {
    pub fn new() -> Self {
        Self {
            cur_func: None,
            cur_match_expr: None,
            errors: Vec::default(),
        }
    }

    /// Helper function to insert a new constraint and store a potential error
    /// in `self.errors()`. Use this function do it in a single line func call
    /// instead of having to check for errors every time.
    fn insert_constraint(&mut self, ctx: &mut TraverseCtx, type_id_a: TypeId, type_id_b: TypeId) {
        if let Err(err) = insert_constraint(&mut ctx.ty_env.lock().unwrap(), type_id_a, type_id_b) {
            self.errors.push(err);
        }
    }
}

impl Visitor for TypeInferencer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        if log_enabled!(Level::Debug) {
            let mut all_type_ids = ctx
                .ty_env
                .lock()
                .unwrap()
                .interner
                .all_types()
                .into_iter()
                .collect::<Vec<_>>();
            all_type_ids.sort_unstable();

            let mut all_types_string = String::new();
            for type_id in all_type_ids {
                all_types_string.push_str(&format!(
                    "\ntype_id: {} - {:?}",
                    type_id,
                    to_string_type_id(&ctx.ty_env.lock().unwrap(), type_id)
                ));
            }

            debug!(
                "Type inference done.\nforwards: {:#?}\nall types: {}\nsubs:",
                ctx.ty_env.lock().unwrap().forwards(),
                all_types_string
            );
            sub_sets_debug_print(&ctx.ty_env.lock().unwrap());
        }
    }

    fn visit_lit(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_lit(expr, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_var(var, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_fn_call(fn_call, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_fn_ptr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        if let Expr::FnPtr(fn_ptr) = expr {
            if let Err(err) = infer_fn_ptr(fn_ptr, ctx) {
                self.errors.push(err);
            }
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_built_in(built_in_call, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_adt_init(adt_init, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_array_init(array_init, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_bin_op(bin_op, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {
        if let Err(err) = infer_un_op(un_op, ctx) {
            self.errors.push(err);
        }
    }

    fn visit_fn(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = &mut block
        {
            if let Err(err) = infer_fn(func, ctx) {
                self.errors.push(err);
            }

            // Save the current function in a place so that the stmts/exprs in the body
            // can access the types of the parameters and the return type of the func.
            self.cur_func = Some(Arc::clone(func));
        }
    }

    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(adt),
            ..
        } = &block
        {
            if let Err(err) = infer_adt_gens(&adt.read().unwrap(), ctx) {
                self.errors.push(err);
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(adt),
            ..
        } = &block
        {
            if let Err(err) = infer_adt_gens(&adt.read().unwrap(), ctx) {
                self.errors.push(err);
            }
        }
    }

    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::Assignment(_, lhs, rhs, ..) = stmt {
            if let Err(err) = infer_assignment(lhs, rhs, ctx) {
                self.errors.push(err);
            }
        }
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            if let Err(err) = infer_var_decl(&mut var.write().unwrap(), ctx) {
                self.errors.push(err);
            }
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::Return(expr_opt, ..) = stmt {
            if let Some(func) = &self.cur_func {
                let func_ret_type_id =
                    if let Some(type_id) = &func.as_ref().borrow().read().unwrap().ret_type {
                        *type_id
                    } else {
                        // TODO: Where should this pos be fetched from?
                        match ctx
                            .ty_env
                            .lock()
                            .unwrap()
                            .id(&Ty::CompoundType(InnerTy::Void, TypeInfo::None))
                        {
                            Ok(type_id) => type_id,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    };

                let expr = if let Some(expr) = expr_opt {
                    expr
                } else {
                    unreachable!("expr None -- stmt: {:#?}", stmt);
                };

                let expr_type_id = match expr.get_expr_type() {
                    Ok(expr_type_id) => expr_type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, func_ret_type_id, expr_type_id);
            } else {
                let err = ctx
                    .ast_ctx
                    .err("Unable to get cur func when looking at return stmt type.".into());
                self.errors.push(err);
            }
        }
    }

    /// Save the current match expr in a place so that the match cases in the body
    /// can access the type of the expr.
    fn visit_match(&mut self, block: &mut Block, _ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Match(expr),
            ..
        } = &block
        {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::MatchCase(match_case_expr),
            ..
        } = &mut block
        {
            if let Some(match_expr) = self.cur_match_expr.clone() {
                let match_expr_type_id = match match_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Some(match_case_expr) = match_case_expr {
                    let case_expr_type_id = match match_case_expr.get_expr_type() {
                        Ok(expr_ty) => expr_ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    self.insert_constraint(ctx, match_expr_type_id, case_expr_type_id);
                }
            } else {
                let err = ctx
                    .ast_ctx
                    .err("Unable to get cur match expr when looking at match case type.".into());
                self.errors.push(err);
            }
        }
    }
}
