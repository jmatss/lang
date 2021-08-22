use common::{
    error::{LangError, LangResult},
    token::{
        block::{Block, BlockHeader},
        expr::{AdtInit, FnCall},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::{get_inner, get_inner_mut},
        to_string::to_string_path,
        ty::Ty,
        type_id::TypeId,
    },
    BlockId,
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
        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        let inner_ty = match get_inner(&ty_env_guard, type_id) {
            Ok(inner_ty) => inner_ty.clone(),
            _ => return Ok(()),
        };

        if inner_ty.is_adt() {
            let path = inner_ty.get_ident().unwrap();
            let full_path = ctx
                .ast_ctx
                .calculate_adt_full_path(&ty_env_guard, &path, block_id)?;

            let inner_ty_mut = get_inner_mut(&mut ty_env_guard, type_id)?;

            *inner_ty_mut.get_ident_mut().unwrap() = full_path;
        } else if inner_ty.is_trait() {
            let path = inner_ty.get_ident().unwrap();
            let full_path =
                ctx.ast_ctx
                    .calculate_trait_full_path(&ty_env_guard, &path, block_id)?;

            let inner_ty_mut = get_inner_mut(&mut ty_env_guard, type_id)?;

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
        let ty = ctx.ty_env.lock().unwrap().ty_clone(type_id)?;
        match ty {
            Ty::CompoundType(inner_ty, ..) => {
                if let Some(gens) = inner_ty.gens() {
                    for gen_type_id in gens.iter_types() {
                        self.resolve_ty_path(ctx, *gen_type_id, block_id)?;
                    }
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
            | Ty::UnknownFnArgument(Some(inner_type_id), ..)
            | Ty::UnknownFnGeneric(Some(inner_type_id), ..)
            | Ty::UnknownArrayMember(inner_type_id, ..) => {
                self.resolve_ty_path(ctx, inner_type_id, block_id)?;
            }

            Ty::Any(..)
            | Ty::Generic(..)
            | Ty::GenericInstance(..)
            | Ty::UnknownFnArgument(None, ..)
            | Ty::UnknownFnGeneric(None, ..) => (),
        }

        Ok(())
    }
}

impl Visitor for PathResolver {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
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

        let half_path = fn_call
            .module
            .clone_push(&fn_call.name, None, fn_call.file_pos);

        let ty_env_guard = ctx.ty_env.lock().unwrap();
        match ctx
            .ast_ctx
            .calculate_fn_full_path(&ty_env_guard, &half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                fn_call.module = full_path;
            }
            Err(_) => {
                let err = ctx.ast_ctx.err_fn(
                    &ty_env_guard,
                    format!(
                        "Unable to find function to call with name: {}",
                        to_string_path(&ty_env_guard, &half_path)
                    ),
                    &half_path,
                );

                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        let half_path = adt_init
            .module
            .clone_push(&adt_init.name, None, adt_init.file_pos);

        let ty_env_guard = ctx.ty_env.lock().unwrap();
        match ctx
            .ast_ctx
            .calculate_adt_full_path(&ty_env_guard, &half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                adt_init.module = full_path;
            }
            Err(_) => {
                let err = ctx.ast_ctx.err_adt(
                    &ty_env_guard,
                    format!(
                        "Unable to find ADT found in ADT init statement: {}",
                        to_string_path(&ty_env_guard, &half_path)
                    ),
                    &half_path,
                );

                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_impl(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Implement(adt_path, trait_path),
            ..
        } = block
        {
            let ty_env_guard = ctx.ty_env.lock().unwrap();

            if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_adt_full_path(&ty_env_guard, adt_path, ctx.block_id)
            {
                *adt_path = full_path;
            } else {
                let mut err = ctx.ast_ctx.err_adt(
                    &ty_env_guard,
                    format!(
                        "Unable to find full path for ADT defined in impl block: {}",
                        to_string_path(&ty_env_guard, &adt_path)
                    ),
                    adt_path,
                );
                err = ctx.ast_ctx.err_adt(&ty_env_guard, err.msg, adt_path);

                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }

            if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_trait_full_path(&ty_env_guard, trait_path, ctx.block_id)
            {
                *trait_path = full_path
            } else {
                let mut err = ctx.ast_ctx.err_trait(
                    &ty_env_guard,
                    format!(
                        "Unable to find full path for trait defined in impl block: {}",
                        to_string_path(&ty_env_guard, &trait_path)
                    ),
                    trait_path,
                );
                err = ctx.ast_ctx.err_trait(&ty_env_guard, err.msg, trait_path);

                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            match ctx.ast_ctx.get_module(ctx.block_id) {
                Ok(Some(module)) => func.write().unwrap().module = module,
                Ok(None) => (),
                Err(err) => {
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.resolve_ty_path(ctx, *type_id, ctx.block_id) {
            if !self.errors.contains(&err) {
                self.errors.push(err);
            }
        }
    }
}
