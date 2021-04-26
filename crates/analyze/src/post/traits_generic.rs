use std::collections::HashSet;

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::{LangError, LangResult},
    path::{LangPath, LangPathPart},
    token::block::TraitCompareError,
    traverse::visitor::Visitor,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    BlockId, TypeId,
};

/// Checks that all generics that have specified "where implements" clauses
/// actual are instances of types that implements the specified trait and all
/// its methods.
pub struct TraitsGenericAnalyzer {
    /// Contains a set keeping track of all seen types. This is done to prevent
    /// checking the same type multiple times.
    seen_types: HashSet<TypeId>,

    errors: Vec<LangError>,
}

impl TraitsGenericAnalyzer {
    pub fn new() -> Self {
        Self {
            seen_types: HashSet::default(),
            errors: Vec::default(),
        }
    }

    /// Given a ADT with the path `adt_path`, makes sure that the "instance"s of
    /// its generics implements the traits that are required on the generic
    /// declarations in a "where" clause.
    ///
    /// For example given the code below:
    ///
    /// ```no_run
    /// pub struct A<T> where T impls B {}
    /// var a = A<TImpl> {}
    /// ```
    ///
    /// The generic `T` are required to implement the trait `B`. In this case
    /// the `verify_adt_traits` function would make sure that the generic
    /// "instance" `TImpl` also implements the trait `B`.
    fn verify_adt_traits(
        &mut self,
        ctx: &TraverseCtx,
        adt_path: &LangPath,
        generics: &Generics,
    ) -> LangResult<()> {
        let mut adt_path_with_gens = adt_path.clone();
        let last_part = adt_path_with_gens.pop().unwrap();
        adt_path_with_gens.push(LangPathPart(last_part.0, Some(generics.clone())));

        let adt = ctx.ast_ctx.get_adt(&ctx.ty_ctx, &adt_path_with_gens)?;
        let adt = adt.borrow();

        // Iterate through all generics for this ADT and make sure that the
        // generics implements the traits specified in the "where" clause.
        for (gen_name, gen_type_id) in generics.iter_names().zip(generics.iter_types()) {
            let trait_tys = if let Some(trait_tys) = adt
                .implements
                .as_ref()
                .map(|impls| impls.get(gen_name))
                .flatten()
            {
                trait_tys
            } else {
                // If there are no "implements" clause for the specific generic,
                // nothing to do here, continue looking at the next generic.
                continue;
            };

            // The `gen_type_id` will be the instance type that has replaced the
            // generic with name `gen_name`. This is the type that should implement
            // the traits in `trait_tys`.
            let (generic_adt_name, generic_adt) =
                if let Ok(Some(ident)) = ctx.ty_ctx.ty_env.get_ident(*gen_type_id) {
                    (ident.clone(), ctx.ast_ctx.get_adt(&ctx.ty_ctx, &ident)?)
                } else {
                    let trait_names = trait_tys
                        .iter()
                        .map(|ty| format!("\n{}", ty.to_string()))
                        .collect::<String>();
                    let err = ctx.ast_ctx.err(format!(
                        "ADT \"{0}\" has \"where\" clause for type \"{1}\" which isn't a ADT. \
                        The type \"{1}\" can therefore not implement the required traits:{2}.",
                        ctx.ty_ctx.to_string_path(&adt_path_with_gens),
                        gen_type_id.to_string(),
                        trait_names,
                    ));
                    return Err(err);
                };
            let generic_adt = generic_adt.borrow();

            let gen_struct_methods = &generic_adt.methods;

            for trait_type_id in trait_tys {
                let trait_ty = ctx.ty_ctx.ty_env.ty(*trait_type_id)?;
                let trait_name = if let Ty::CompoundType(InnerTy::Trait(trait_name), ..) = trait_ty
                {
                    trait_name
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "Generic with name \"{}\" on ADT \"{}\" implements non trait type: {:#?}",
                        gen_name,
                        ctx.ty_ctx.to_string_path(&adt_path_with_gens),
                        trait_type_id
                    ));
                    return Err(err);
                };

                let trait_ = ctx.ast_ctx.get_trait(&ctx.ty_ctx, trait_name)?;

                for trait_method in &trait_.borrow().methods {
                    let method_name = trait_method.name.clone();

                    let gen_struct_method =
                        if let Some(struct_method) = gen_struct_methods.get(&method_name) {
                            struct_method
                        } else {
                            let err = ctx.ast_ctx.err(format!(
                                "Struct \"{0}\" requires that its generic type \"{1}\" implements \
                            the trait \"{2}\". The type \"{3}\" is used as generic \"{1}\", \
                            but it does NOT implement the function \"{4}\" from the trait \"{2}\".",
                                ctx.ty_ctx.to_string_path(&adt_path_with_gens),
                                gen_name,
                                ctx.ty_ctx.to_string_path(&trait_name),
                                ctx.ty_ctx.to_string_path(&generic_adt_name),
                                method_name
                            ));
                            return Err(err);
                        };

                    // TODO: Make safe. Gets a borrow error if done as usual,
                    //       there is a mutable borrow already. Where is that?
                    let struct_method_borrow =
                        unsafe { gen_struct_method.as_ptr().as_ref().unwrap() };

                    // Make the check to ensure that the trait method are correctly implemented.
                    if let Err(cmp_errors) = struct_method_borrow.trait_cmp(trait_method) {
                        let err_msg_start = format!(
                            "Struct \"{}\"s impl of trait \"{}\"s method \"{}\" is incorrect.\n",
                            ctx.ty_ctx.to_string_path(&adt_path_with_gens),
                            ctx.ty_ctx.to_string_path(&trait_name),
                            method_name,
                        );
                        let err_msg_end = format!(
                            "\nstruct_method: {:#?}\ntrait_method: {:#?}",
                            struct_method_borrow, trait_method
                        );

                        for cmp_err in cmp_errors {
                            let err_msg = match cmp_err {
                                TraitCompareError::ParamLenDiff(s_len, t_len, contains_this) => {
                                    let (s_len, t_len) = if contains_this {
                                        (s_len, t_len + 1)
                                    } else {
                                        (s_len, t_len)
                                    };
                                    format!(
                                        "Parameter list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::ParamTypeDiff(t_idx, contains_this) => {
                                    let s_idx = if contains_this { t_idx + 1 } else { t_idx };
                                    format!(
                                        "Parameter types at idx {} differs. Struct param type: {:#?}, trait param type: {:#?}",
                                        s_idx,
                                        struct_method_borrow.parameters.as_ref().unwrap().get(s_idx).unwrap().borrow().ty,
                                        trait_method.parameters.as_ref().unwrap().get(t_idx).unwrap().borrow().ty,
                                    )
                                }
                                TraitCompareError::ReturnTypeDiff => {
                                    format!(
                                        "Return types differ. Struct return type: {:#?}, trait return type: {:#?}",
                                        struct_method_borrow.ret_type,
                                        trait_method.ret_type,
                                    )
                                }
                                TraitCompareError::GenericsLenDiff(s_len, t_len) => {
                                    format!(
                                        "Generic list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::GenericsNameDiff(idx) => {
                                    format!(
                                        "Generic at idx {} differs. Struct generic name: {:#?}, trait generic name: {:#?}",
                                        idx,
                                        struct_method_borrow.generics.as_ref().unwrap().get_name(idx).unwrap(),
                                        trait_method.generics.as_ref().unwrap().get_name(idx).unwrap(),
                                    )
                                }
                                TraitCompareError::ImplsLenDiff(s_len, t_len) => {
                                    format!(
                                        "Implements list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(Some(s_name), None) => {
                                    format!(
                                        "Found impls for generic with name \"{}\" in struct, not found trait.",
                                        s_name,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(None, Some(t_name)) => {
                                    format!(
                                        "Found impls for generic with name \"{}\" in trait, not found struct.",
                                        t_name,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(..) => {
                                    unreachable!()
                                }
                                TraitCompareError::ImplsTypeDiff(gen_name) => {
                                    format!(
                                        "Impls list diff for generic with name \"{}\".",
                                        gen_name,
                                    )
                                }
                            };

                            let err = ctx
                                .ast_ctx
                                .err(format!("{}{}{}", err_msg_start, err_msg, err_msg_end));
                            self.errors.push(err);
                        }

                        return Ok(());
                    }
                }
            }
        }

        Ok(())
    }

    /// Recursively checks the types contained in the given `ty`. If any of the
    /// types are a ADT with a generic, checks that the generics of that ADT
    /// actually implements the specified traits in the "where" clause (if any).
    fn check_adt_traits(
        &mut self,
        ctx: &TraverseCtx,
        type_id: TypeId,
        block_id: BlockId,
    ) -> LangResult<()> {
        let inf_type_id = ctx.ty_ctx.inferred_type(type_id)?;
        match ctx.ty_ctx.ty_env.ty(inf_type_id)? {
            Ty::CompoundType(inner_ty, generics, ..) => {
                for gen_type_id in generics.iter_types() {
                    self.check_adt_traits(ctx, *gen_type_id, block_id)?;
                }

                match inner_ty {
                    InnerTy::Struct(path) | InnerTy::Union(path) => {
                        if !generics.is_empty() {
                            self.verify_adt_traits(ctx, path, generics)?;
                        }
                    }
                    _ => (),
                };
            }

            Ty::Array(arr_type_id, expr_opt, ..) => {
                if let Some(expr_type_id) = expr_opt
                    .as_ref()
                    .map(|expr| expr.get_expr_type().ok())
                    .flatten()
                {
                    self.check_adt_traits(ctx, expr_type_id, block_id)?;
                }
                self.check_adt_traits(ctx, *arr_type_id, block_id)?;
            }
            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.check_adt_traits(ctx, expr_type_id, block_id)?;
                }
            }

            Ty::Fn(gens, params, ret_ty, ..) => {
                if let Some(ret_type_id) = ret_ty {
                    self.check_adt_traits(ctx, *ret_type_id, block_id)?;
                }
                for gen_type_id in gens {
                    self.check_adt_traits(ctx, *gen_type_id, block_id)?;
                }
                for param_type_id in params {
                    self.check_adt_traits(ctx, *param_type_id, block_id)?;
                }
            }

            Ty::Pointer(type_id_i, _)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.check_adt_traits(ctx, *type_id_i, block_id)?;
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(())
    }
}

impl Visitor for TraitsGenericAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: More effective way to check this.
    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if !self.seen_types.contains(type_id) {
            self.seen_types.insert(*type_id);
            if let Err(err) = self.check_adt_traits(ctx, *type_id, ctx.block_id) {
                self.errors.push(err);
            }
        }
    }
}
