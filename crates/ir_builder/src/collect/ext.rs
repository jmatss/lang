use std::sync::Mutex;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangErrorKind},
    token::{
        ast::AstToken,
        stmt::{ExternalDecl, Stmt},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::ty_env::TyEnv,
};
use ir::{decl::ty::Type, module::Module};

use crate::{to_ir_adt, to_ir_func};

/// Collects all external declarations (currently ADTs and functions) that can
/// be found in the AST. The declaration of these are inserted into the given
/// `module.`
pub(crate) fn collect_extern_decls(
    module: &mut Module,
    ast_ctx: &mut AstCtx,
    ty_env: &Mutex<TyEnv>,
    ast_root: &mut AstToken,
) -> Result<(), Vec<LangError>> {
    let mut traverse_ctx = TraverseCtx::new(ast_ctx, &ty_env);
    let mut visitor = ExtDeclCollector::new(module);
    traverse(&mut traverse_ctx, &mut visitor, ast_root)
}

/// Iterates through all statements in the AST. For every ExternalDecl statement,
/// adds the external function or type to `self.module`.
pub(crate) struct ExtDeclCollector<'a> {
    module: &'a mut Module,
    errors: Vec<LangError>,
}

impl<'a> ExtDeclCollector<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for ExtDeclCollector<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::ExternalDecl(ext_decl, ..) = stmt {
            let ty_env_guard = ctx.ty_env.lock().unwrap();

            match ext_decl {
                ExternalDecl::Fn(func) => {
                    let func = func.as_ref().read().unwrap();
                    let ir_func = match to_ir_func(ctx.ast_ctx, &ty_env_guard, &func) {
                        Ok(ir_func) => ir_func,
                        Err(err) => {
                            self.errors.push(LangError {
                                msg: err.msg,
                                kind: LangErrorKind::GeneralError,
                                file_pos: None,
                                backtrace: err.backtrace,
                            });
                            return;
                        }
                    };

                    // Allow mulitple declaration of the same extern function.
                    if self.module.get_function(&ir_func.name).is_none() {
                        if let Err(err) = self.module.add_function(ir_func.name.clone(), ir_func) {
                            self.errors.push(LangError {
                                msg: err.msg,
                                kind: LangErrorKind::GeneralError,
                                file_pos: None,
                                backtrace: err.backtrace,
                            });
                        }
                    }
                }

                ExternalDecl::Struct(struct_) => {
                    let adt = struct_.as_ref().read().unwrap();
                    let adt_type = match to_ir_adt(ctx.ast_ctx, &ty_env_guard, &adt) {
                        Ok(adt_type) => adt_type,
                        Err(err) => {
                            self.errors.push(LangError {
                                msg: err.msg,
                                kind: LangErrorKind::GeneralError,
                                file_pos: None,
                                backtrace: err.backtrace,
                            });
                            return;
                        }
                    };

                    let already_declared = match &adt_type {
                        Type::Struct(name, ..) => self.module.get_struct(name).is_some(),
                        Type::Enum(name, ..) => self.module.get_enum(name).is_some(),
                        Type::Union(name, ..) => self.module.get_union(name).is_some(),
                        _ => unreachable!("adt_type: {:#?}", adt_type),
                    };

                    // Allow mulitple declaration of the same extern ADT.
                    if !already_declared {
                        if let Err(err) = self.module.add_adt(adt_type) {
                            self.errors.push(LangError {
                                msg: err.msg,
                                kind: LangErrorKind::GeneralError,
                                file_pos: None,
                                backtrace: err.backtrace,
                            });
                        }
                    }
                }
            }
        }
    }
}
