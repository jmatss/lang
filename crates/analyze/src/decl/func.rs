use std::sync::Arc;

use parking_lot::RwLock;

use common::{
    ctx::block_ctx::BlockCtx,
    error::{LangError, LangResult},
    hash::DerefType,
    path::LangPath,
    token::expr::Var,
    token::{
        ast::AstToken,
        block::{AdtKind, Block, BlockHeader, Fn},
        stmt::Stmt,
        stmt::{ExternalDecl, Modifier},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        inner_ty::InnerTy,
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
        type_info::TypeInfo,
    },
    BlockId,
};

/// Gathers information about all function/method declarations found in the AST
/// and inserts them into the `analyze_context`. This includes external function
/// declarations, functions and methods (in implement block).
pub struct DeclFnAnalyzer {
    errors: Vec<LangError>,
}

impl DeclFnAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn analyze_fn_header(
        &mut self,
        ctx: &mut TraverseCtx,
        func: &mut Arc<RwLock<Fn>>,
        fn_id: BlockId,
    ) -> LangResult<()> {
        // The function will be added in the scope of its parent, so fetch the
        // block id for the parent.
        let parent_id = ctx.ast_ctx.get_parent_id(fn_id)?;

        let path_without_gens = {
            let module = ctx.ast_ctx.get_module(fn_id)?.unwrap_or_default();
            let func = func.read();

            module.clone_push(&func.name, None, Some(func.file_pos))
        };

        // TODO: Add file positions to error message.
        if ctx
            .ast_ctx
            .get_fn(&ctx.ty_env.lock(), &path_without_gens)
            .is_ok()
        {
            let err = ctx.ast_ctx.err(format!(
                "Two declarations of function \"{}\" founds. Full path: {}",
                &func.read().name,
                to_string_path(&ctx.ty_env.lock(), &path_without_gens),
            ));
            return Err(err);
        }

        let key = (path_without_gens, parent_id);
        ctx.ast_ctx.fns.insert(
            &ctx.ty_env.lock(),
            DerefType::Shallow,
            key,
            Arc::clone(func),
        )?;

        Ok(())
    }

    /// If this is a non-static method, insert `this` as the first parameter
    /// to the given function. If this is a "primitive struct", the `this`
    /// should be the primitive type instead of the struct type.
    fn analyze_method_header(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path_without_gens: &LangPath,
        func: &mut Arc<RwLock<Fn>>,
    ) {
        let inner_ty = if ctx
            .ast_ctx
            .is_trait(&ctx.ty_env.lock(), &adt_path_without_gens)
        {
            InnerTy::Trait(adt_path_without_gens.clone())
        } else {
            match ctx
                .ast_ctx
                .get_adt(&ctx.ty_env.lock(), &adt_path_without_gens)
            {
                Ok(adt) => match adt.read().kind {
                    AdtKind::Struct => {
                        let ident = &adt.read().name;
                        if InnerTy::ident_to_type(ident, 0).is_primitive() {
                            InnerTy::ident_to_type(ident, 0)
                        } else {
                            InnerTy::Struct(adt_path_without_gens.clone())
                        }
                    }
                    AdtKind::Union => InnerTy::Union(adt_path_without_gens.clone()),
                    AdtKind::Enum => InnerTy::Enum(adt_path_without_gens.clone()),
                    AdtKind::Unknown => unreachable!("AdtKind::Unknown"),
                },
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        };

        // TODO: Should probably be changed to something better.
        // If this is a non-static method, the first parameter should be a
        // reference(/pointer) to "this"/"self".
        if !func.read().is_static() {
            static THIS_VAR_NAME: &str = "this";
            let mut func = func.write();

            let type_id = match ctx
                .ty_env
                .lock()
                .id(&Ty::CompoundType(inner_ty, TypeInfo::None))
            {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Will `TypeInfo::None` work? Does this need some sort of type
            //       info?
            let type_id = if func.modifiers.contains(&Modifier::This) {
                type_id
            } else if func.modifiers.contains(&Modifier::ThisPointer) {
                match ctx.ty_env.lock().id(&Ty::Pointer(type_id, TypeInfo::None)) {
                    Ok(ptr_type_id) => ptr_type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Non static function did not contain \"this\" or \"this ptr\" reference. \
                    ADT name: {}, func: {:#?}.",
                    to_string_path(&ctx.ty_env.lock(), &adt_path_without_gens),
                    func
                ));
                self.errors.push(err);
                return;
            };

            let var = Arc::new(RwLock::new(Var::new(
                THIS_VAR_NAME.into(),
                Some(type_id),
                None,
                None,
                None,
                None,
                false,
            )));

            if let Some(ref mut params) = func.parameters {
                params.insert(0, var);
            } else {
                func.parameters = Some(vec![var]);
            }
        }
    }
}

impl Visitor for DeclFnAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// Marks the methods in ADT/impl blocks with the name of the ADTs. This
    /// lets one differentiate between functions and methods by checking the
    /// `method_adt` field in the functions.
    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { header, body, .. } = block;
        let mut ty_env_guard = ctx.ty_env.lock();

        let path_without_gens = match header {
            BlockHeader::Implement(adt_path, ..) => adt_path.without_gens(),

            BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                let adt = adt.read();
                adt.module.clone_push(&adt.name, None, None)
            }

            BlockHeader::Trait(trait_) => {
                let trait_ = trait_.read();
                trait_.module.clone_push(&trait_.name, None, None)
            }

            _ => return,
        };

        let inner_ty = {
            if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_adt_full_path(&ty_env_guard, &path_without_gens, ctx.block_id)
            {
                if ctx.ast_ctx.is_struct(&ty_env_guard, &full_path) {
                    InnerTy::Struct(full_path)
                } else if ctx.ast_ctx.is_enum(&ty_env_guard, &full_path) {
                    InnerTy::Enum(full_path)
                } else if ctx.ast_ctx.is_union(&ty_env_guard, &full_path) {
                    InnerTy::Union(full_path)
                } else {
                    unreachable!("full_path: {:#?}", full_path);
                }
            } else if let Ok(full_path) = ctx.ast_ctx.calculate_trait_full_path(
                &ty_env_guard,
                &path_without_gens,
                ctx.block_id,
            ) {
                InnerTy::Trait(full_path)
            } else {
                let err = ctx.ast_ctx.err_adt(
                    &ty_env_guard,
                    format!(
                        "Unable to find ADT or trait with path: {}",
                        to_string_path(&ty_env_guard, &path_without_gens)
                    ),
                    &path_without_gens,
                );
                self.errors.push(err);
                return;
            }
        };

        let adt_type_id = match ty_env_guard.id(&Ty::CompoundType(inner_ty, TypeInfo::None)) {
            Ok(adt_type_id) => adt_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        for body_token in body {
            if body_token.is_skippable() {
                // skip
            } else if let AstToken::Block(Block {
                header: BlockHeader::Fn(method),
                ..
            }) = body_token
            {
                method.write().method_adt = Some(adt_type_id);
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "AST token in ADT block with type \"{:?}\" not a method: {:?}",
                    to_string_type_id(&ty_env_guard, adt_type_id),
                    body_token
                ));
                self.errors.push(err);
            }
        }
    }

    /// The `visit_block` is traversed before this function is called which
    /// means that all `method_adt`s will be set for the funtions.
    fn visit_fn(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            id: func_id,
            ..
        } = &mut block
        {
            let adt_ty = if let Some(adt_type_id) = func.read().method_adt {
                match ctx.ty_env.lock().ty(adt_type_id) {
                    Ok(adt_ty) => Some(adt_ty.clone()),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            } else {
                None
            };

            if let Some(adt_ty) = adt_ty {
                let adt_path_without_gens = match adt_ty {
                    Ty::CompoundType(inner_ty, ..) if inner_ty.is_primitive() => {
                        inner_ty.get_primitive_ident().into()
                    }

                    Ty::CompoundType(InnerTy::Struct(adt_path), ..)
                    | Ty::CompoundType(InnerTy::Enum(adt_path), ..)
                    | Ty::CompoundType(InnerTy::Union(adt_path), ..)
                    | Ty::CompoundType(InnerTy::Trait(adt_path), ..) => adt_path.without_gens(),

                    _ => unreachable!("Method method_adt not CompoundType: {:#?}", func),
                };

                self.analyze_method_header(ctx, &adt_path_without_gens, func);

                if let Err(err) = ctx.ast_ctx.insert_method(
                    &ctx.ty_env.lock(),
                    &adt_path_without_gens,
                    Arc::clone(func),
                ) {
                    self.errors.push(err);
                }
            } else if let Err(err) = self.analyze_fn_header(ctx, func, *func_id) {
                self.errors.push(err);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::ExternalDecl(ExternalDecl::Fn(func), ..) = stmt {
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.

            let key = {
                let func = func.read();
                let path = func
                    .module
                    .clone_push(&func.name, None, Some(func.file_pos));
                (path, BlockCtx::DEFAULT_BLOCK_ID)
            };

            if let Err(err) = ctx.ast_ctx.fns.insert(
                &ctx.ty_env.lock(),
                DerefType::Shallow,
                key,
                Arc::clone(func),
            ) {
                self.errors.push(err);
            };
        }
    }
}
