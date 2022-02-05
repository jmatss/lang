use std::sync::Arc;

use parking_lot::RwLock;

use common::{
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader, Fn},
        expr::{Expr, Var, VarType},
        op::AssignOperator,
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
    ARGC_GLOBAL_VAR_NAME, ARGC_PARAM_VAR_NAME, ARGV_GLOBAL_VAR_NAME, ARGV_PARAM_VAR_NAME,
};

/// This analyzer "handles" the main function and the CLI arguments passed to
/// the program.
///
/// Two global variables `argc` and `argv` will be created. It doesn't matter if
/// the compiled module contains a main function or not. If no main function
/// exists, the contents of the variables will be zeroed. If a main function
/// exists, the values will be populated with the CLI arguments.
///
/// Checks for a "main" function. If one is found, it makes sure that it returns
/// the expected i32 and takes no parameters. This is enforced since we want to
/// rewrite the definition internally ourselves.
///
/// The main function will be rewritten to take the two parameters `argc` and
/// `argv`. Logic will then put into the start of the main function to store
/// these parameters in the global variables so that they can be accessed from
/// code via the built in functions `@argc()` and `@argv()` respectively.
///
/// # Only valid definition of main
/// ```no_run
/// fn main() -> int
/// ```
pub struct MainArgsAnalyzer {
    errors: Vec<LangError>,
}

impl MainArgsAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn verify_main(&mut self, ctx: &TraverseCtx, func: &Fn) -> LangResult<()> {
        if let Some(ret_type_id) = func.ret_type {
            let ret_ty = ctx.ty_env.lock().ty(ret_type_id)?.clone();
            if matches!(ret_ty, Ty::CompoundType(InnerTy::Int, ..)) {
                if func.parameters.is_none() {
                    Ok(())
                } else {
                    Err(ctx.ast_ctx.err(
                        "Found \"main\" function with parameters, should not have parameters."
                            .into(),
                    ))
                }
            } else {
                Err(ctx.ast_ctx.err(format!(
                    "Found \"main\" function with no that didn't return the expected int. \
                    Instead if returns type_id: {}, ty: {:?}",
                    ret_type_id, ret_ty
                )))
            }
        } else {
            // TODO: Add file pos of function.
            Err(ctx
                .ast_ctx
                .err("Found \"main\" function with no return type set, should return int.".into()))
        }
    }

    fn insert_args_decl(
        &mut self,
        ctx: &mut TraverseCtx,
        body: &mut Vec<AstToken>,
    ) -> LangResult<()> {
        let (argc_param, argv_param) = self.construct_params(ctx)?;

        let mut argc_global = argc_param;
        argc_global.set_global(true);
        argc_global.name = ARGC_GLOBAL_VAR_NAME.into();
        let argc_global_decl = Stmt::VariableDecl(Arc::new(RwLock::new(argc_global)), None);

        let mut argv_global = argv_param;
        let mut generics = Generics::new();
        generics.insert_type(argv_global.ty.unwrap());
        argv_global.set_global(true);
        argv_global.name = ARGV_GLOBAL_VAR_NAME.into();
        let argv_global_decl = Stmt::VariableDecl(Arc::new(RwLock::new(argv_global)), None);

        // If there is a Module statement at the top of this body, need to make
        // sure to insert the declaration below it. The Module MUST be the first
        // statement in a file if it is specified.
        let start_idx = if let Some(AstToken::Stmt(Stmt::Module(..))) = body.get(0) {
            1
        } else {
            0
        };
        body.insert(start_idx, AstToken::Stmt(argc_global_decl));
        body.insert(start_idx + 1, AstToken::Stmt(argv_global_decl));

        Ok(())
    }

    /// Assigns the values found inside variables with names
    ///   `ARGC_PARAM_VAR_NAME` & `ARGV_PARAM_VAR_NAME`
    /// into the variables with names
    ///   `ARGC_GLOBAL_VAR_NAME` and `ARGV_GLOBAL_VAR_NAME`
    /// respectively.
    ///
    /// This function should be called with the `body` from the main function
    /// which will assign the arguments given to the main to the global
    /// `argc`/`argv` variables.
    fn insert_args_assign(
        &mut self,
        ctx: &mut TraverseCtx,
        body: &mut Vec<AstToken>,
    ) -> LangResult<()> {
        let (argc_param, argv_param) = self.construct_params(ctx)?;

        let mut argc_global = argc_param.clone();
        argc_global.set_global(true);
        argc_global.name = ARGC_GLOBAL_VAR_NAME.into();
        let argc_global_assign = Stmt::Assignment(
            AssignOperator::Assignment,
            Expr::Var(argc_global),
            Expr::Var(argc_param),
            None,
        );

        let mut argv_global = argv_param.clone();
        argv_global.set_global(true);
        argv_global.name = ARGV_GLOBAL_VAR_NAME.into();
        let argv_global_assign = Stmt::Assignment(
            AssignOperator::Assignment,
            Expr::Var(argv_global),
            Expr::Var(argv_param),
            None,
        );

        body.insert(0, AstToken::Stmt(argc_global_assign));
        body.insert(1, AstToken::Stmt(argv_global_assign));

        Ok(())
    }

    /// The first item in the returned tuple is a variable representing `argc`
    /// and the second item represents `argv`.
    fn construct_params(&self, ctx: &mut TraverseCtx) -> LangResult<(Var, Var)> {
        let uint_type_id = ctx
            .ty_env
            .lock()
            .id(&Ty::CompoundType(InnerTy::Uint, TypeInfo::None))?;
        let argc_param = Var::new(
            ARGC_PARAM_VAR_NAME.into(),
            Some(uint_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        );

        let u8_type_id = ctx
            .ty_env
            .lock()
            .id(&Ty::CompoundType(InnerTy::U8, TypeInfo::None))?;
        let u8_ptr_type_id = ctx
            .ty_env
            .lock()
            .id(&Ty::Pointer(u8_type_id, TypeInfo::None))?;
        let u8_ptr_ptr_type_id = ctx
            .ty_env
            .lock()
            .id(&Ty::Pointer(u8_ptr_type_id, TypeInfo::None))?;
        let argv_param = Var::new(
            ARGV_PARAM_VAR_NAME.into(),
            Some(u8_ptr_ptr_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        );

        Ok((argc_param, argv_param))
    }
}

impl Visitor for MainArgsAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// Insert the global declarations for `argc` and `argv`. These will store
    /// the arguments as static so that they can be accessed from anywhere in
    /// the code.
    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Err(err) = self.insert_args_decl(ctx, &mut block.body) {
            self.errors.push(err);
        }
    }

    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            body,
            ..
        } = block
        {
            if func.read().name == "main" && func.read().module.count() == 0 {
                if let Err(err) = self.verify_main(ctx, &func.read()) {
                    self.errors.push(err);
                    return;
                }

                match self.construct_params(ctx) {
                    Ok((argc_param, argv_param)) => {
                        func.write().parameters = Some(vec![
                            Arc::new(RwLock::new(argc_param)),
                            Arc::new(RwLock::new(argv_param)),
                        ]);
                    }
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Err(err) = self.insert_args_assign(ctx, body) {
                    self.errors.push(err);
                    return;
                }
            }
        }
    }
}
