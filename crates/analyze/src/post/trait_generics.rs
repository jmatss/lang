use std::collections::HashSet;

use common::{
    error::LangError,
    hash::DerefType,
    path::LangPath,
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::get_ident,
        inner_ty::InnerTy,
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
        type_id::TypeId,
    },
    BlockId,
};

/// Checks that all generics that have specified "where implements" clauses
/// actual are instances of types that implements the specified trait and all
/// its methods.
pub struct TraitGenericsAnalyzer {
    /// Contains a set keeping track of all seen types. This is done to prevent
    /// checking the same type multiple times.
    seen_types: HashSet<TypeId>,
    errors: Vec<LangError>,
}

impl TraitGenericsAnalyzer {
    pub fn new() -> Self {
        Self {
            seen_types: HashSet::default(),
            errors: Vec::default(),
        }
    }

    // TODO: Merge logic `verify_adt_traits` with `verify_fn_traits`.
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

        let mut errors = Vec::default();
        let ty_env_guard = ctx.ty_env.lock().unwrap();

        let adt = ctx
            .ast_ctx
            .get_adt(&ty_env_guard, &adt_path)
            .map_err(|e| vec![e])?;
        let adt = adt.as_ref().read().unwrap();

        for (gen_name, gen_type_id) in gens.iter_names().zip(gens.iter_types()) {
            // The `trait_paths` will contain the traits that the generic with
            // name `gen_name` HAVE to implement according to the declarations
            // on the ADT.
            let trait_paths = if let Some(trait_paths) = adt
                .implements
                .as_ref()
                .map(|impls| impls.get(gen_name))
                .flatten()
            {
                trait_paths
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
                let trait_names = trait_paths
                    .iter()
                    .map(|path| format!("\n * {:?}", path))
                    .collect::<String>();
                errors.push(ctx.ast_ctx.err(format!(
                    "ADT \"{0}\" has \"where\" clause for type \"{1}\" which isn't a ADT. \
                    The type \"{1}\" can therefore not implement the required traits:{2}.",
                    to_string_path(&ty_env_guard, &adt_path),
                    gen_type_id.to_string(),
                    trait_names,
                )));
                continue;
            };

            let impl_adt = ctx
                .ast_ctx
                .get_adt(&ty_env_guard, &impl_path)
                .map_err(|e| vec![e])?;
            let impl_adt = impl_adt.as_ref().read().unwrap();
            let impl_methods = &impl_adt.methods;

            for trait_path in trait_paths {
                let trait_ = ctx
                    .ast_ctx
                    .get_trait(&ty_env_guard, &trait_path)
                    .map_err(|e| vec![e])?;
                let trait_ = trait_.read().unwrap();
                let trait_methods = &trait_.methods;

                for trait_method in trait_methods {
                    let method_name = trait_method.name.clone();
                    if !impl_methods.contains_key(&method_name) {
                        errors.push(ctx.ast_ctx.err(format!(
                            "Struct \"{0}\" requires that its generic type \"{1}\" implements \
                            the trait \"{2}\". The type \"{3}\" is used as generic \"{1}\", \
                            but it does NOT implement the function \"{4}\" from the trait \"{2}\".",
                            to_string_path(&ty_env_guard, &adt_path),
                            gen_name,
                            to_string_path(&ty_env_guard, &trait_path),
                            to_string_path(&ty_env_guard, &impl_path),
                            method_name
                        )));
                    };
                }
            }

            for trait_path in trait_paths {
                if !impl_adt
                    .implemented_traits
                    .contains(&ty_env_guard, DerefType::Deep, trait_path)
                    .map_err(|err| vec![err])?
                {
                    errors.push(ctx.ast_ctx.err(format!(
                        "ADT \"{0}\" requires that its generic type \"{1}\" implements \
                        the trait \"{2}\". The type \"{3}\" is used as generic \"{1}\", \
                        but it does NOT implement the required \"{2}\" trait.",
                        to_string_path(&ty_env_guard, &adt_path),
                        gen_name,
                        to_string_path(&ty_env_guard, &trait_path),
                        to_string_path(&ty_env_guard, &impl_path),
                    )));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn verify_fn_traits(
        &mut self,
        ctx: &TraverseCtx,
        fn_call: &FnCall,
    ) -> Result<(), Vec<LangError>> {
        // TODO: Implement for fn pointers.
        if fn_call.is_fn_ptr_call {
            return Ok(());
        }

        let fn_call_gens = if let Some(gens) = &fn_call.generics {
            gens
        } else {
            return Ok(());
        };

        let mut errors = Vec::default();
        let ty_env_guard = ctx.ty_env.lock().unwrap();

        let (func, adt_path) = if let Some(adt_type_id) = fn_call.method_adt {
            let adt_path = get_ident(&ty_env_guard, adt_type_id)
                .map_err(|err| vec![err])?
                .unwrap();
            let func = ctx
                .ast_ctx
                .get_method(&ty_env_guard, &adt_path, &fn_call.half_name(&ty_env_guard))
                .map_err(|err| vec![err])?;
            (func, Some(adt_path))
        } else {
            let fn_path = fn_call.module.clone_push(
                &fn_call.name,
                fn_call.generics.as_ref(),
                fn_call.file_pos,
            );
            let func = ctx
                .ast_ctx
                .get_fn(&ty_env_guard, &fn_path)
                .map_err(|err| vec![err])?;
            (func, None)
        };
        let func = func.read().unwrap();

        for (gen_name, gen_type_id) in fn_call_gens.iter_names().zip(fn_call_gens.iter_types()) {
            // The `trait_paths` will contain the traits that the generic with
            // name `gen_name` HAVE to implement according to the declarations
            // on the function/method.
            let trait_paths = if let Some(trait_paths) = func
                .implements
                .as_ref()
                .map(|impls| impls.get(gen_name))
                .flatten()
            {
                trait_paths
            } else {
                continue;
            };

            // The `gen_type_id` will be the instance type that has replaced the
            // generic with name `gen_name`. This is the type that should implement
            // the traits in `trait_tys`. I.e. the adt `impl_adt` should implement
            // the methods specified in the `trait_tys`
            let impl_adt_path = if let Ok(Some(path)) = get_ident(&ty_env_guard, *gen_type_id) {
                path.clone()
            } else {
                let trait_names = trait_paths
                    .iter()
                    .map(|path| format!("\n * {:?}", path))
                    .collect::<String>();

                let mut err_msg = String::new();

                if let Some(adt_path) = &adt_path {
                    err_msg.push_str(&format!(
                        "Method \"{}\" in ADT \"{}\"",
                        &func.name,
                        to_string_path(&ty_env_guard, &adt_path)
                    ));
                } else {
                    let fn_path = func
                        .module
                        .clone_push(&func.name, None, Some(func.file_pos));
                    err_msg.push_str(&format!(
                        "Function \"{}\"",
                        to_string_path(&ty_env_guard, &fn_path)
                    ));
                }

                errors.push(ctx.ast_ctx.err(format!(
                    "{0} has \"where\" clause for type \"{1}\" which isn't a ADT. \
                    The type \"{1}\" can therefore not implement the required traits:{2}.",
                    err_msg,
                    to_string_type_id(&ty_env_guard, *gen_type_id).map_err(|err| vec![err])?,
                    trait_names,
                )));
                continue;
            };

            let impl_adt = ctx
                .ast_ctx
                .get_adt(&ty_env_guard, &impl_adt_path)
                .map_err(|e| vec![e])?;
            let impl_adt = impl_adt.as_ref().read().unwrap();

            for trait_path in trait_paths {
                if !impl_adt
                    .implemented_traits
                    .contains(&ty_env_guard, DerefType::Deep, trait_path)
                    .map_err(|err| vec![err])?
                {
                    let mut err_msg = String::new();

                    if let Some(adt_path) = &adt_path {
                        err_msg.push_str(&format!(
                            "Method \"{}\" in ADT \"{}\"",
                            &func.name,
                            to_string_path(&ty_env_guard, &adt_path)
                        ));
                    } else {
                        let fn_path = func
                            .module
                            .clone_push(&func.name, None, Some(func.file_pos));
                        err_msg.push_str(&format!(
                            "Function \"{}\"",
                            to_string_path(&ty_env_guard, &fn_path)
                        ));
                    }

                    errors.push(ctx.ast_ctx.err(format!(
                        "{} requires that its generic type \"{1}\" implements the trait \"{2}\". \
                        The type \"{3}\" is used as generic \"{1}\", but it does NOT implement \
                        the required \"{2}\" trait.",
                        err_msg,
                        gen_name,
                        to_string_path(&ty_env_guard, &trait_path),
                        to_string_path(&ty_env_guard, &impl_adt_path),
                    )));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
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
        let inf_type_id = ctx
            .ty_env
            .lock()
            .unwrap()
            .inferred_type(type_id)
            .map_err(|e| vec![e])?;
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
            | Ty::UnknownFnArgument(Some(type_id_i), ..)
            | Ty::UnknownFnGeneric(Some(type_id_i), ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.check_adt_traits(ctx, type_id_i, block_id)?;
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

impl Visitor for TraitGenericsAnalyzer {
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

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Err(errs) = self.verify_fn_traits(ctx, fn_call) {
            self.errors.extend(errs);
        }
    }
}
