use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{AdtInit, FnCall},
    },
    traverse::visitor::Visitor,
    ty::ty::Ty,
    BlockId, TypeId,
};

/// Tries to solve partial paths (LangPath) in the code. This is done by trying
/// to prepend "use" statements with paths found in the code. If a match is found,
/// the partial path will be replaced with the full path.
pub struct PathResolver {
    errors: Vec<LangError>,
}

impl PathResolver {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn replace_inner_path(
        &mut self,
        ctx: &mut TraverseCtx,
        type_id: TypeId,
        block_id: BlockId,
    ) -> LangResult<()> {
        let inner_ty = match ctx.ty_ctx.ty_env.get_inner(type_id) {
            Ok(inner_ty) => inner_ty.clone(),
            _ => return Ok(()),
        };

        if inner_ty.is_adt() {
            let path = inner_ty.get_ident().unwrap();
            let full_path = ctx
                .ast_ctx
                .calculate_adt_full_path(&ctx.ty_ctx, &path, block_id)?;

            let inner_ty_mut = ctx.ty_ctx.ty_env.get_inner_mut(type_id)?;

            *inner_ty_mut.get_ident_mut().unwrap() = full_path;
        } else if inner_ty.is_trait() {
            let path = inner_ty.get_ident().unwrap();
            let full_path = ctx
                .ast_ctx
                .calculate_trait_full_path(&ctx.ty_ctx, &path, block_id)?;

            let inner_ty_mut = ctx.ty_ctx.ty_env.get_inner_mut(type_id)?;

            *inner_ty_mut.get_ident_mut().unwrap() = full_path;
        }

        Ok(())
    }

    fn resolve_ty_path(
        &mut self,
        ctx: &mut TraverseCtx,
        type_id: TypeId,
        block_id: BlockId,
    ) -> LangResult<()> {
        // TODO: From what I know, only Compound types needs to be solved here
        //       since they are created for enum accesses during parsing.
        //       Is this a correct assumption?
        let ty = ctx.ty_ctx.ty_env.ty(type_id)?.clone();
        match ty {
            Ty::CompoundType(_, gens, ..) => {
                for gen_type_id in gens.iter_types() {
                    self.resolve_ty_path(ctx, *gen_type_id, block_id)?;
                }
                self.replace_inner_path(ctx, type_id, block_id)?;
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, ..) => {
                for ty_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.resolve_ty_path(ctx, *ty_i, block_id)?;
                }
                if let Some(ret_type_id) = ret_ty_opt {
                    self.resolve_ty_path(ctx, ret_type_id, block_id)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.resolve_ty_path(ctx, expr_type_id, block_id)?;
                }
            }

            Ty::Array(inner_type_id, ..)
            | Ty::Pointer(inner_type_id, ..)
            | Ty::UnknownAdtMember(inner_type_id, ..)
            | Ty::UnknownAdtMethod(inner_type_id, ..)
            | Ty::UnknownMethodArgument(inner_type_id, ..)
            | Ty::UnknownMethodGeneric(inner_type_id, ..)
            | Ty::UnknownArrayMember(inner_type_id, ..) => {
                self.resolve_ty_path(ctx, inner_type_id, block_id)?;
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(())
    }
}

impl Visitor for PathResolver {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.is_fn_ptr_call || fn_call.is_method {
            // Method calls already solved during the "MethodAnalyzer" stage.
            return;
        }

        let half_path = fn_call.module.clone_push(&fn_call.name, None);

        match ctx
            .ast_ctx
            .calculate_fn_full_path(&ctx.ty_ctx, &half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                fn_call.module = full_path;
            }
            Err(err) => {
                self.errors.push(err);
            }
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        let half_path = adt_init.module.clone_push(&adt_init.name, None);

        match ctx
            .ast_ctx
            .calculate_adt_full_path(&ctx.ty_ctx, &half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                adt_init.module = full_path;
            }
            Err(err) => {
                self.errors.push(err);
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Implement(path, _), ..) = ast_token {
            if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_adt_full_path(&ctx.ty_ctx, path, ctx.block_id)
            {
                *path = full_path
            } else if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_trait_full_path(&ctx.ty_ctx, path, ctx.block_id)
            {
                *path = full_path
            } else {
                let mut err = ctx.ast_ctx.err_adt(
                    &ctx.ty_ctx,
                    format!(
                        "Unable to find full path for type defined in impl block: {}",
                        ctx.ty_ctx.ty_env.to_string_path(&ctx.ty_ctx, &path)
                    ),
                    path,
                );
                err = ctx.ast_ctx.err_trait(&ctx.ty_ctx, err.msg, path);
                self.errors.push(err);
            }
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.resolve_ty_path(ctx, *type_id, ctx.block_id) {
            self.errors.push(err);
        }
    }
}
