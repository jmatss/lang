use std::collections::HashSet;

use common::{
    error::{LangError, LangErrorKind},
    path::LangPath,
    token::block::{Fn, TraitCompareError},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::get_ident, inner_ty::InnerTy, solve::inferred_type, to_string::to_string_path, ty::Ty,
        ty_env::TyEnv, type_id::TypeId,
    },
    BlockId,
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

    /// Makes a check to ensure that the specified `trait_method` is correctly
    /// implement by `impl_method`.
    fn verify_method(
        ty_env: &TyEnv,
        impl_method: &Fn,
        trait_method: &Fn,
        adt_path: &LangPath,
        trait_path: &LangPath,
    ) -> Result<(), Vec<LangError>> {
        if let Err(cmp_errors) = impl_method.trait_cmp(trait_method) {
            let mut errs = Vec::with_capacity(cmp_errors.len());
            let err_msg_start = format!(
                "Struct \"{}\"s impl of trait \"{}\"s method \"{}\" is incorrect.\n",
                to_string_path(ty_env, &adt_path),
                to_string_path(ty_env, &trait_path),
                trait_method.name,
            );
            let err_msg_end = format!(
                "\nstruct_method: {:#?}\ntrait_method: {:#?}",
                impl_method, trait_method
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
                            s_len, t_len,
                        )
                    }
                    TraitCompareError::ParamTypeDiff(t_idx, contains_this) => {
                        let s_idx = if contains_this { t_idx + 1 } else { t_idx };
                        format!(
                            "Parameter types at idx {} differs. Struct param type: {:#?}, trait param type: {:#?}",
                            s_idx,
                            impl_method.parameters.as_ref().unwrap().get(s_idx).unwrap().as_ref().read().unwrap().ty,
                            trait_method.parameters.as_ref().unwrap().get(t_idx).unwrap().as_ref().read().unwrap().ty,
                        )
                    }
                    TraitCompareError::ReturnTypeDiff => {
                        format!(
                            "Return types differ. Struct return type: {:#?}, trait return type: {:#?}",
                            impl_method.ret_type,
                            trait_method.ret_type,
                        )
                    }
                    TraitCompareError::GenericsLenDiff(s_len, t_len) => {
                        format!(
                            "Generic list length differs. Struct len: {}, trait len: {}",
                            s_len, t_len,
                        )
                    }
                    TraitCompareError::GenericsNameDiff(idx) => {
                        format!(
                            "Generic at idx {} differs. Struct generic name: {:#?}, trait generic name: {:#?}",
                            idx,
                            impl_method.generics.as_ref().unwrap().get_name(idx).unwrap(),
                            trait_method.generics.as_ref().unwrap().get_name(idx).unwrap(),
                        )
                    }
                    TraitCompareError::ImplsLenDiff(s_len, t_len) => {
                        format!(
                            "Implements list length differs. Struct len: {}, trait len: {}",
                            s_len, t_len,
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
                        format!("Impls list diff for generic with name \"{}\".", gen_name,)
                    }
                };

                errs.push(LangError::new(
                    format!("{}{}{}", err_msg_start, err_msg, err_msg_end),
                    LangErrorKind::AnalyzeError,
                    None,
                ));
            }

            Err(errs)
        } else {
            Ok(())
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
    ) -> Result<(), Vec<LangError>> {
        let gens = if let Some(gens) = adt_path.gens() {
            gens
        } else {
            return Ok(());
        };

        let ty_env_guard = ctx.ty_env.lock().unwrap();

        let adt = ctx
            .ast_ctx
            .get_adt(&ty_env_guard, &adt_path)
            .map_err(|e| vec![e])?;
        let adt = adt.as_ref().read().unwrap();

        for (gen_name, gen_type_id) in gens.iter_names().zip(gens.iter_types()) {
            // The `trait_tys` will contain the traits that the generic with
            // name `gen_name` HAVE to implement according to the declarations
            // on the ADT.
            let trait_tys = if let Some(trait_tys) = adt
                .implements
                .as_ref()
                .map(|impls| impls.get(gen_name))
                .flatten()
            {
                trait_tys
            } else {
                continue;
            };

            // The `gen_type_id` will be the instance type that has replaced the
            // generic with name `gen_name`. This is the type that should implement
            // the traits in `trait_tys`. I.e. the adt `impl_adt` should implement
            // the methods specified in the `trait_tys`
            let impl_path = if let Ok(Some(path)) = get_ident(&ty_env_guard, *gen_type_id) {
                path.clone()
            } else {
                let trait_names = trait_tys
                    .iter()
                    .map(|ty| format!("\n{}", ty.to_string()))
                    .collect::<String>();
                return Err(vec![ctx.ast_ctx.err(format!(
                    "ADT \"{0}\" has \"where\" clause for type \"{1}\" which isn't a ADT. \
                        The type \"{1}\" can therefore not implement the required traits:{2}.",
                    to_string_path(&ty_env_guard, &adt_path),
                    gen_type_id.to_string(),
                    trait_names,
                ))]);
            };

            let impl_adt = ctx
                .ast_ctx
                .get_adt(&ty_env_guard, &impl_path)
                .map_err(|e| vec![e])?;
            let impl_adt = impl_adt.as_ref().read().unwrap();
            let impl_methods = &impl_adt.methods;

            for trait_type_id in trait_tys {
                let trait_ty = ty_env_guard.ty_clone(*trait_type_id).map_err(|e| vec![e])?;
                let trait_path = if let Ty::CompoundType(InnerTy::Trait(path), ..) = trait_ty {
                    path
                } else {
                    return Err(vec![ctx.ast_ctx.err(format!(
                        "Generic with name \"{}\" on ADT \"{}\" implements non trait type: {:#?}",
                        gen_name,
                        to_string_path(&ty_env_guard, &adt_path),
                        trait_type_id
                    ))]);
                };

                let trait_ = ctx
                    .ast_ctx
                    .get_trait(&ty_env_guard, &trait_path)
                    .map_err(|e| vec![e])?;
                let trait_ = trait_.read().unwrap();
                let trait_methods = &trait_.methods;

                for trait_method in trait_methods {
                    let method_name = trait_method.name.clone();

                    let impl_method = if let Some(impl_method) = impl_methods.get(&method_name) {
                        impl_method
                    } else {
                        return Err(vec![ctx.ast_ctx.err(format!(
                            "Struct \"{0}\" requires that its generic type \"{1}\" implements \
                            the trait \"{2}\". The type \"{3}\" is used as generic \"{1}\", \
                            but it does NOT implement the function \"{4}\" from the trait \"{2}\".",
                            to_string_path(&ty_env_guard, &adt_path),
                            gen_name,
                            to_string_path(&ty_env_guard, &trait_path),
                            to_string_path(&ty_env_guard, &impl_path),
                            method_name
                        ))]);
                    };
                    let impl_method = impl_method.read().unwrap();

                    if let Err(errs) = Self::verify_method(
                        &ty_env_guard,
                        &impl_method,
                        trait_method,
                        adt_path,
                        &trait_path,
                    ) {
                        self.errors.extend(errs);
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
    ) -> Result<(), Vec<LangError>> {
        let inf_type_id =
            inferred_type(&ctx.ty_env.lock().unwrap(), type_id).map_err(|e| vec![e])?;
        let ty_clone = ctx
            .ty_env
            .lock()
            .unwrap()
            .ty_clone(inf_type_id)
            .map_err(|e| vec![e])?;
        match ty_clone {
            Ty::CompoundType(inner_ty, ..) => {
                if let Some(gens) = inner_ty.gens() {
                    for gen_type_id in gens.iter_types() {
                        self.check_adt_traits(ctx, *gen_type_id, block_id)?;
                    }

                    match inner_ty {
                        InnerTy::Struct(path) | InnerTy::Union(path) => {
                            self.verify_adt_traits(ctx, &path)?;
                        }
                        _ => (),
                    }
                }
            }

            Ty::Array(arr_type_id, expr_opt, ..) => {
                if let Some(expr_type_id) = expr_opt
                    .as_ref()
                    .map(|expr| expr.get_expr_type().ok())
                    .flatten()
                {
                    self.check_adt_traits(ctx, expr_type_id, block_id)?;
                }
                self.check_adt_traits(ctx, arr_type_id, block_id)?;
            }
            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.check_adt_traits(ctx, expr_type_id, block_id)?;
                }
            }

            Ty::Fn(gens, params, ret_ty, ..) => {
                if let Some(ret_type_id) = ret_ty {
                    self.check_adt_traits(ctx, ret_type_id, block_id)?;
                }
                for gen_type_id in gens {
                    self.check_adt_traits(ctx, gen_type_id, block_id)?;
                }
                for param_type_id in params {
                    self.check_adt_traits(ctx, param_type_id, block_id)?;
                }
            }

            Ty::Pointer(type_id_i, _)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.check_adt_traits(ctx, type_id_i, block_id)?;
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
            if let Err(errs) = self.check_adt_traits(ctx, *type_id, ctx.block_id) {
                self.errors.extend(errs);
            }
        }
    }
}
