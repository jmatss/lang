use std::{cell::RefCell, rc::Rc};

use common::{
    ctx::{block_ctx::BlockCtx, traverse_ctx::TraverseCtx},
    error::LangError,
    path::{LangPath, LangPathPart},
    token::expr::Var,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader, Fn},
        stmt::Modifier,
        stmt::Stmt,
    },
    traverse::visitor::Visitor,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
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
        func: &mut Rc<RefCell<Fn>>,
        fn_id: BlockId,
    ) {
        // The function will be added in the scope of its parent, so fetch the
        // block id for the parent.
        let parent_id = match ctx.ast_ctx.get_parent_id(fn_id) {
            Ok(parent_id) => parent_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let full_path = match ctx.ast_ctx.get_module(fn_id) {
            Ok(module_opt) => {
                let module = if let Some(module) = module_opt {
                    module
                } else {
                    LangPath::default()
                };

                let func = func.borrow();
                module.clone_push(&func.name, func.generics.as_ref())
            }
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Add file positions to error message.
        if ctx.ast_ctx.get_fn(&ctx.ty_ctx, &full_path).is_ok() {
            let err = ctx.ast_ctx.err(format!(
                "Two declarations of function \"{}\" founds. Full path: {}",
                &func.borrow().name,
                ctx.ty_ctx.to_string_path(&full_path),
            ));
            self.errors.push(err);
        }

        // Add the function into decl lookup maps.
        let key = (full_path, parent_id);
        ctx.ast_ctx.fns.insert(key, Rc::clone(func));
    }

    fn analyze_method_header(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        func: &mut Rc<RefCell<Fn>>,
    ) {
        let inner_ty = if ctx.ast_ctx.is_trait(&ctx.ty_ctx, adt_path) {
            InnerTy::Trait(adt_path.clone())
        } else {
            match ctx.ast_ctx.get_adt(&ctx.ty_ctx, adt_path) {
                Ok(adt) => match adt.borrow().kind {
                    AdtKind::Struct => InnerTy::Struct(adt_path.clone()),
                    AdtKind::Union => InnerTy::Union(adt_path.clone()),
                    AdtKind::Enum => InnerTy::Enum(adt_path.clone()),
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
        if !func.borrow().is_static() {
            static THIS_VAR_NAME: &str = "this";
            let mut func = func.borrow_mut();

            let generics = Generics::new();

            let type_id =
                match ctx
                    .ty_ctx
                    .ty_env
                    .id(&Ty::CompoundType(inner_ty, generics, TypeInfo::None))
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
                match ctx.ty_ctx.ty_env.id(&Ty::Pointer(type_id, TypeInfo::None)) {
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
                    &ctx.ty_ctx.to_string_path(&adt_path),
                    func
                ));
                self.errors.push(err);
                return;
            };

            let var = Rc::new(RefCell::new(Var::new(
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

        // Insert this method into `methods` in the analyze context.
        if let Err(err) = ctx
            .ast_ctx
            .insert_method(&ctx.ty_ctx, adt_path, Rc::clone(func))
        {
            self.errors.push(err);
            return;
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

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        ctx.ast_ctx.file_pos = ast_token.file_pos().cloned().unwrap_or_default();
    }

    /// Marks the functions in this implement block with the name of the implement
    /// block. This lets one differentiate between functions and methods by checking
    /// the `method_struct` field in "Function"s.
    fn visit_impl(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        // TODO: This won't work for all cases. The `impl_path` doesn't consider
        //       the module path or "use"s.

        let (impl_path, body) =
            if let AstToken::Block(BlockHeader::Implement(impl_path, _), .., body) = &mut ast_token
            {
                (impl_path, body)
            } else {
                return;
            };

        let mut partial_path = impl_path.clone();
        let last_part = partial_path.pop().unwrap();
        partial_path.push(LangPathPart(last_part.0, None));

        let inner_ty = {
            if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_adt_full_path(&ctx.ty_ctx, impl_path, ctx.block_id)
            {
                if ctx.ast_ctx.is_struct(&ctx.ty_ctx, &full_path) {
                    InnerTy::Struct(full_path)
                } else if ctx.ast_ctx.is_enum(&ctx.ty_ctx, &full_path) {
                    InnerTy::Enum(full_path)
                } else if ctx.ast_ctx.is_union(&ctx.ty_ctx, &full_path) {
                    InnerTy::Union(full_path)
                } else {
                    unreachable!("full_path: {:#?}", full_path);
                }
            } else if let Ok(full_path) =
                ctx.ast_ctx
                    .calculate_trait_full_path(&ctx.ty_ctx, impl_path, ctx.block_id)
            {
                InnerTy::Trait(full_path)
            } else {
                let err = ctx.ast_ctx.err_adt(
                    &ctx.ty_ctx,
                    format!(
                        "Unable to find ADT with path: {}",
                        ctx.ty_ctx.to_string_path(&partial_path)
                    ),
                    &partial_path,
                );
                self.errors.push(err);
                return;
            }
        };

        let impl_type_id =
            match ctx
                .ty_ctx
                .ty_env
                .id(&Ty::CompoundType(inner_ty, Generics::new(), TypeInfo::None))
            {
                Ok(impl_type_id) => impl_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

        for mut body_token in body {
            if body_token.is_skippable() {
                // skip
            } else if let AstToken::Block(BlockHeader::Fn(func), ..) = &mut body_token {
                func.borrow_mut().method_adt = Some(impl_type_id);
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "AST token in impl block with name \"{}\" not a function: {:?}",
                    ctx.ty_ctx.to_string_path(impl_path),
                    body_token
                ));
                self.errors.push(err);
            }
        }
    }

    fn visit_fn(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Fn(func), _, func_id, ..) = &mut ast_token {
            let adt_ty = if let Some(adt_type_id) = func.borrow().method_adt {
                match ctx.ty_ctx.ty_env.ty(adt_type_id) {
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
                if let Ty::CompoundType(inner_ty, ..) = adt_ty {
                    match inner_ty {
                        InnerTy::Struct(path)
                        | InnerTy::Enum(path)
                        | InnerTy::Union(path)
                        | InnerTy::Trait(path) => {
                            self.analyze_method_header(ctx, &path, func);
                        }

                        _ => unreachable!(
                            "Method method_structure inner type not structure: {:#?}",
                            func
                        ),
                    }
                } else {
                    unreachable!("Method method_structure not CompoundType: {:#?}", func);
                }
            } else {
                self.analyze_fn_header(ctx, func, *func_id);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::ExternalDecl(func, ..) = stmt {
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.
            let key = {
                let func = func.borrow();
                let path = func.module.clone_push(&func.name, None);
                (path, BlockCtx::DEFAULT_BLOCK_ID)
            };
            ctx.ast_ctx.fns.insert(key, Rc::clone(func));
        }
    }
}
