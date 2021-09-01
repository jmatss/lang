use common::{
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{
        generics::Generics, inner_ty::InnerTy, to_string::to_string_path, ty::Ty, ty_env::TyEnv,
        type_id::TypeId,
    },
};

/// Iterates through "generic" parameters tied to ADTs and functions, and
/// replaces the uses of the generics with "Generic" types instead of the parsed
/// "UnknownIdent".
pub struct GenericsAnalyzer {
    errors: Vec<LangError>,
}

impl GenericsAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    /// Replaces any `UnknownIdent` with a name found in a generic declaration
    /// into a `Generic`. The generic declaration can either be declared in the
    /// function found in the `fn_token` or the generic can also be declared
    /// in the given `adt_gens`.
    fn replace_gens_in_fn_token(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_token: &mut AstToken,
        adt_gens: Option<&Generics>,
    ) {
        let func_gens = if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
            ..
        }) = fn_token
        {
            func.as_ref().read().unwrap().generics.clone()
        } else {
            return;
        };

        // No generics declared on either ADT/Trait or function, early skip.
        if adt_gens.is_none() && func_gens.is_none() {
            return;
        }

        // Replaces any generics declared on the function.
        if let Some(func_generics) = func_gens {
            let mut func_replacer = FuncGenericsReplacer::new(&func_generics);
            if let Err(mut errs) = traverse(ctx, &mut func_replacer, fn_token) {
                self.errors.append(&mut errs);
                return;
            }
        }

        // Replaces any generics declared on the ADT/Trait.
        if let Some(adt_gens) = adt_gens {
            let mut adt_replacer = FuncGenericsReplacer::new(&adt_gens);
            if let Err(mut errs) = traverse(ctx, &mut adt_replacer, fn_token) {
                self.errors.append(&mut errs);
                return;
            }
        }
    }
}

impl Visitor for GenericsAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// "Rewrites" the types of the generic member types to "Generic"s for
    /// the structure members.
    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(struct_),
            ..
        } = &block
        {
            let struct_ = struct_.read().unwrap();
            if let (Some(generics), members) = (&struct_.generics, &struct_.members) {
                for member in members {
                    if let Some(type_id) = member.write().unwrap().ty.as_mut() {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(union),
            ..
        } = &block
        {
            let union = union.read().unwrap();
            if let (Some(generics), members) = (&union.generics, &union.members) {
                for member in members {
                    if let Some(type_id) = member.write().unwrap().ty.as_mut() {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    fn visit_trait(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Trait(trait_),
            ..
        } = &block
        {
            let trait_ = trait_.read().unwrap();
            let trait_gens = if let Some(gen_names) = &trait_.generics {
                let mut gens = Generics::default();
                for gen_name in gen_names {
                    gens.insert_name(gen_name.clone());
                }
                Some(gens)
            } else {
                None
            };

            for method in &trait_.methods {
                if let Some(params) = &method.parameters {
                    for param in params {
                        let param = param.read().unwrap();
                        if let Some(param_type_id) = param.ty {
                            // Replace generics declared on trait in method params.
                            if let Some(trait_gens) = &trait_gens {
                                if let Err(err) = replace_gens(
                                    &mut ctx.ty_env.lock().unwrap(),
                                    param_type_id,
                                    trait_gens,
                                ) {
                                    self.errors.push(err);
                                    return;
                                }

                                // Replace generics declared on method in method params.
                                if let Some(method_gens) = &method.generics {
                                    if let Err(err) = replace_gens(
                                        &mut ctx.ty_env.lock().unwrap(),
                                        param_type_id,
                                        method_gens,
                                    ) {
                                        self.errors.push(err);
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }

                if let Some(ret_type_id) = &method.ret_type {
                    // Replace generics declared on trait in method return type
                    if let Some(trait_gens) = &trait_gens {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *ret_type_id, trait_gens)
                        {
                            self.errors.push(err);
                            return;
                        }
                    }

                    // Replace generics declared on method in method return type.
                    if let Some(method_gens) = &method.generics {
                        if let Err(err) =
                            replace_gens(&mut ctx.ty_env.lock().unwrap(), *ret_type_id, method_gens)
                        {
                            self.errors.push(err);
                            return;
                        }
                    }
                }
            }
        }
    }

    /// "Rewrites" generics parsed as "UnknownIdent"s to "Generic"s by matching
    /// the identifiers with known names for the generics defined on the structure.
    /// This will be done for both the method headers and everything in their bodies.
    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { header, body, .. } = block;

        let adt_gens = match header {
            BlockHeader::Implement(impl_path, ..) => {
                let path_without_gens = match ctx.ast_ctx.get_module(ctx.block_id) {
                    Ok(Some(module)) => {
                        let impl_name = impl_path.last().unwrap().0.clone();
                        module.clone_push(&impl_name, None, None)
                    }
                    Ok(None) => impl_path.without_gens(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Ok(adt) = ctx
                    .ast_ctx
                    .get_adt(&ctx.ty_env.lock().unwrap(), &path_without_gens)
                {
                    adt.read().unwrap().generics.clone()
                } else if ctx
                    .ast_ctx
                    .get_trait(&ctx.ty_env.lock().unwrap(), &path_without_gens)
                    .is_ok()
                {
                    None
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "Unable to find ADT/Trait for impl block with name \"{}\".",
                        to_string_path(&ctx.ty_env.lock().unwrap(), &path_without_gens),
                    ));
                    self.errors.push(err);
                    return;
                }
            }

            BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                adt.read().unwrap().generics.clone()
            }

            BlockHeader::Trait(_) => None,

            _ => return,
        };

        // Iterate through the body of one method at a time and replace all
        // "UnknownIdent"s representing generics to "Generic"s.
        for method in body {
            self.replace_gens_in_fn_token(ctx, method, adt_gens.as_ref());
        }
    }

    // TODO: Possible to do in something similar to the `visit_fn` function?
    //       Since `visit_fn` and `visit_block` takes `Block`s, they can be used
    //       with the traverser. But the body of the default block are tokens,
    //       so we can iterate through the function tokens and traverse them.
    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { body, .. } = block;
        for token in body {
            if let AstToken::Block(Block {
                header: BlockHeader::Fn(_),
                ..
            }) = token
            {
                self.replace_gens_in_fn_token(ctx, token, None);
            }
        }
    }
}

struct FuncGenericsReplacer<'a> {
    gen_decls: &'a Generics,
    errors: Vec<LangError>,
}

/// Used when replacing generics in functions/methods containing to a specific
/// generic implementation. This will be used to replace all types in the body
/// of the functions/methods.
impl<'a> FuncGenericsReplacer<'a> {
    pub fn new(gen_decls: &'a Generics) -> Self {
        Self {
            gen_decls,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for FuncGenericsReplacer<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = replace_gens(&mut ctx.ty_env.lock().unwrap(), *type_id, self.gen_decls) {
            self.errors.push(err);
        }
    }
}

/// Recursively replaces any generic identifiers from "UnknownIdent" wrapped
/// inside a "CompoundType" into "Generic"s.
fn replace_gens(ty_env: &mut TyEnv, id: TypeId, generics: &Generics) -> LangResult<()> {
    match ty_env.ty(id)?.clone() {
        Ty::CompoundType(InnerTy::UnknownIdent(path, ..), type_info) => {
            if let Some(gens) = path.gens() {
                for gen_type_id in gens.iter_types() {
                    replace_gens(ty_env, *gen_type_id, generics)?;
                }
            }

            if path.count() == 1 {
                let possible_gen_name = path.first().unwrap().name();
                for gen_name in generics.iter_names() {
                    if gen_name == possible_gen_name {
                        let new_ty = Ty::Generic(
                            possible_gen_name.into(),
                            ty_env.new_unique_id(),
                            type_info.clone(),
                        );
                        ty_env.update(id, new_ty)?;
                    }
                }
            }
        }

        Ty::Pointer(type_id, ..)
        | Ty::Array(type_id, ..)
        | Ty::UnknownAdtMember(type_id, ..)
        | Ty::UnknownAdtMethod(type_id, ..)
        | Ty::UnknownFnArgument(type_id, ..)
        | Ty::UnknownArrayMember(type_id, ..) => {
            replace_gens(ty_env, type_id, generics)?;
        }

        Ty::Fn(gens, params, ret_type_id_opt, ..) => {
            if let Some(ret_type_id) = ret_type_id_opt {
                replace_gens(ty_env, ret_type_id, generics)?;
            }
            for gen_type_id in gens.iter() {
                replace_gens(ty_env, *gen_type_id, generics)?;
            }
            for param_type_id in params.iter() {
                replace_gens(ty_env, *param_type_id, generics)?;
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id) = expr.get_expr_type() {
                replace_gens(ty_env, type_id, generics)?;
            }
        }

        _ => (),
    }

    Ok(())
}
