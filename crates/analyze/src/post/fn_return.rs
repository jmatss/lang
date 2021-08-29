use common::{
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader, Fn},
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    BlockId,
};

// TODO: Currently there are no easy way to see if all paths in a function
//       actually reutrn a value. This should probably be implemented when IR
//       is implemented and it is easier to analyze such things.
/// Validates that return statements are correct in functions.
///
/// If a function have specified a return type but the function doesn't contain
/// a return statement, this analyzer will report it as an error. If a function
/// without a return type doesn't contain e return statement, this analyzer will
/// add a return statement to the end of the function.
///
/// If a return statement contains a value but the function haven't specified
/// a return type, it will already have been caught and reported as an error in
/// `TypeInferencer`. If a return statement doesn't contain a value but the
/// function have specified a return type, it will already have been caught and
/// reported as an error in `TypeInferencer`.
pub struct FnReturnAnalyzer {
    errors: Vec<LangError>,
}

impl FnReturnAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn validate_fn(
        func: &Fn,
        body: &mut Vec<AstToken>,
        block_id: BlockId,
        ctx: &mut TraverseCtx,
    ) -> LangResult<()> {
        let block_ctx = if let Some(block_ctx) = ctx.ast_ctx.block_ctxs.get(&block_id) {
            block_ctx
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Unable to get block ID for function with ID: {}.",
                block_id
            )));
        };

        let contains_return = block_ctx.contains_return || block_ctx.all_children_contains_return;

        if !contains_return && func.ret_type.is_some() {
            // TODO: The `all_children_contains_return` check all children for
            //       return statements so given the example function:
            //       ```
            //       fn f() -> i32 {
            //           if x { }
            //           if y { return 1 }
            //       }
            //       ```
            //       `all_children_contains_return` would be false since the
            //       "x" if-statement doesn't contain a return. We can see that
            //       this function always returns a value, but it is hard to see
            //       it in the AST atm. Should be easier to implemented in IR.
            /*
            let fn_name = if let Some(adt_type_id) = func.method_adt {
                let ty_env_guard = ctx.ty_env.lock().unwrap();
                let adt_path = get_ident(&ty_env_guard, adt_type_id)?.unwrap();
                to_method_name(&ty_env_guard, &adt_path, &func.name, func.generics.as_ref())
            } else {
                func.name.clone()
            };

            return Err(ctx.ast_ctx.err(format!(
                "Function \"{}\" have a return type specified, \
                    but not all its code paths returns a value.",
                fn_name
            )));
            */
        } else if !contains_return && func.ret_type.is_none() {
            body.push(AstToken::Stmt(Stmt::Return(None, None)))
        }

        Ok(())
    }
}

impl Visitor for FnReturnAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            body,
            id,
            ..
        } = block
        {
            let func = func.read().unwrap();
            if let Err(err) = Self::validate_fn(&func, body, *id, ctx) {
                self.errors.push(err);
            }
        }
    }
}
