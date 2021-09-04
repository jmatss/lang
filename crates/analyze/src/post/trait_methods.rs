use std::collections::{HashMap, HashSet};

use common::{
    error::{LangError, LangErrorKind, LangResult},
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader, Fn, TraitCompareError},
        stmt::Modifier,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        generics::Generics,
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
        ty_env::TyEnv,
        type_id::TypeId,
    },
};

/// Checks that all ADTs that implements a trait implements it correctly.
/// See the `trait_cmp()` function on the `Fn` struct for the match conditions
/// of the methods.
pub struct TraitMethodsAnalyzer {
    errors: Vec<LangError>,
}

impl TraitMethodsAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    /// Validates that a `impl` block has implemented the trait correctly.
    pub fn validate_impl(
        ctx: &TraverseCtx,
        adt_path: &LangPath,
        trait_path: &LangPath,
        impl_body: &[AstToken],
    ) -> Result<(), Vec<LangError>> {
        let ty_env_guard = ctx.ty_env.lock();

        // Will start of containing all names of the method declared in the
        // trait. When iterating through the methods of the trait implementation,
        // the seen method names will be removed from this set. If the trait
        // declaration and impl methods match, this set should end up empty.
        let mut trait_method_names = ctx
            .ast_ctx
            .get_trait_method_names(&ty_env_guard, &trait_path.without_gens())
            .map_err(|err| vec![err])?;

        let trait_ = ctx
            .ast_ctx
            .get_trait(&ty_env_guard, &trait_path.without_gens())
            .map_err(|err| vec![err])?;
        let trait_ = trait_.read();

        let trait_methods = {
            let mut trait_methods = HashMap::with_capacity(trait_.methods.len());
            for method in &trait_.methods {
                trait_methods.insert(method.name.clone(), method);
            }
            trait_methods
        };

        let mut errors = Vec::default();

        // Validates and combines the generics for a trait.
        // The names of the generics will be fetched from the trait declaration
        // and the implementations will be fetched from the trait implementation.
        // The amount of generics must match.
        let combined_gens = match (trait_path.gens(), trait_.generics.as_ref()) {
            (Some(trait_impl_gens), Some(trait_decl_gens)) => {
                if trait_impl_gens.len_types() != trait_decl_gens.len() {
                    errors.push(ctx.ast_ctx.err(format!(
                        "Trait \"{}\" has {} declared generics, but the impl of the trait for \
                        ADT \"{}\" has {} generics implemented on the trait.",
                        to_string_path(&ty_env_guard, &trait_path.without_gens()),
                        trait_decl_gens.len(),
                        to_string_path(&ty_env_guard, adt_path),
                        trait_impl_gens.len_types(),
                    )));
                    return Err(errors);
                }

                let mut combined_gens = Generics::new();
                for (gen_name, gen_type) in trait_decl_gens.iter().zip(trait_impl_gens.iter_types())
                {
                    combined_gens.insert(gen_name.clone(), *gen_type);
                }
                Some(combined_gens)
            }

            (None, Some(trait_decl_gens)) => {
                errors.push(ctx.ast_ctx.err(format!(
                    "Trait \"{}\" has {} declared generics, but the impl of the trait for \
                    ADT \"{}\" has no generics implemented on the trait.",
                    to_string_path(&ty_env_guard, &trait_path.without_gens()),
                    trait_decl_gens.len(),
                    to_string_path(&ty_env_guard, adt_path),
                )));
                return Err(errors);
            }

            (Some(trait_impl_gens), None) => {
                errors.push(ctx.ast_ctx.err(format!(
                    "Trait \"{}\" has no declared generics, but the impl of the trait for \
                    ADT \"{}\" has {} generics implemented on the trait.",
                    to_string_path(&ty_env_guard, &trait_path.without_gens()),
                    to_string_path(&ty_env_guard, adt_path),
                    trait_impl_gens.len_types(),
                )));
                return Err(errors);
            }

            (None, None) => None,
        };

        for fn_token in impl_body {
            let impl_method = if fn_token.is_skippable() {
                continue;
            } else if let AstToken::Block(Block {
                header: BlockHeader::Fn(method),
                ..
            }) = fn_token
            {
                method
            } else {
                errors.push(ctx.ast_ctx.err(format!(
                    "AST token in impl block for ADT \"{}\" not a method: {:?}",
                    to_string_path(&ty_env_guard, adt_path),
                    fn_token
                )));
                continue;
            };
            let impl_method = impl_method.read();

            let trait_method = if let Some(trait_method) = trait_methods.get(&impl_method.name) {
                trait_method
            } else {
                errors.push(ctx.ast_ctx.err(format!(
                    "Impl of trait \"{}\" for ADT \"{}\" contains fn not declared in trait: {:?}",
                    to_string_path(&ty_env_guard, trait_path),
                    to_string_path(&ty_env_guard, adt_path),
                    &impl_method.name,
                )));
                continue;
            };

            trait_method_names.remove(&impl_method.name);

            if let Err(method_errs) = Self::validate_method(
                &ty_env_guard,
                &impl_method,
                trait_method,
                adt_path,
                trait_path,
            ) {
                errors.extend(method_errs);
                continue;
            }

            if let Some(gens) = &combined_gens {
                if let Err(method_errs) = Self::validate_generics(
                    &ty_env_guard,
                    &impl_method,
                    trait_method,
                    adt_path,
                    trait_path,
                    gens,
                ) {
                    errors.extend(method_errs);
                    continue;
                }
            }
        }

        // Check so that the `trait_method_names` is empty indicating the the
        // impl contains a implementation for all methods declared in the trait.
        if !trait_method_names.is_empty() {
            let mut unimplemented_methods = trait_method_names
                .iter()
                .map(|name| format!(" * {}", name))
                .collect::<Vec<_>>();
            unimplemented_methods.sort();

            errors.push(ctx.ast_ctx.err(format!(
                "Impl of trait \"{}\" for ADT \"{}\" doesn't implement all methods declared in \
                the trait. Missing methods in impl block:\n{}",
                to_string_path(&ty_env_guard, trait_path),
                to_string_path(&ty_env_guard, adt_path),
                unimplemented_methods.join("\n"),
            )));
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Makes a check to ensure that the specified `trait_method` is correctly
    /// implement by `impl_method`.
    fn validate_method(
        ty_env: &TyEnv,
        impl_method: &Fn,
        trait_method: &Fn,
        adt_path: &LangPath,
        trait_path: &LangPath,
    ) -> Result<(), Vec<LangError>> {
        if let Err(cmp_errors) = impl_method.trait_cmp(ty_env, trait_method) {
            let mut errs = Vec::with_capacity(cmp_errors.len());
            let err_msg_start = format!(
                "ADT \"{}\"s impl of trait \"{}\"s method \"{}\" is incorrect.\n",
                to_string_path(ty_env, &adt_path),
                to_string_path(ty_env, &trait_path),
                trait_method.name,
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
                        let impl_method_type = to_string_type_id(
                            ty_env,
                            impl_method
                                .parameters
                                .as_ref()
                                .unwrap()
                                .get(s_idx)
                                .unwrap()
                                .read()
                                .ty
                                .unwrap(),
                        );
                        let trait_method_type = to_string_type_id(
                            ty_env,
                            trait_method
                                .parameters
                                .as_ref()
                                .unwrap()
                                .get(t_idx)
                                .unwrap()
                                .read()
                                .ty
                                .unwrap(),
                        );
                        format!(
                            "Parameter types at idx {} differs. \
                            Struct param type: {}, trait param type: {}",
                            s_idx,
                            impl_method_type.unwrap(),
                            trait_method_type.unwrap(),
                        )
                    }
                    TraitCompareError::ReturnTypeDiff => {
                        format!(
                            "Return types differ. Struct return type: {}, trait return type: {}",
                            to_string_type_id(ty_env, impl_method.ret_type.unwrap()).unwrap(),
                            to_string_type_id(ty_env, trait_method.ret_type.unwrap()).unwrap(),
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
                            "Generic at idx {} differs. \
                            Struct generic name: {}, trait generic name: {}",
                            idx,
                            impl_method
                                .generics
                                .as_ref()
                                .unwrap()
                                .get_name(idx)
                                .unwrap(),
                            trait_method
                                .generics
                                .as_ref()
                                .unwrap()
                                .get_name(idx)
                                .unwrap(),
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
                    TraitCompareError::ThisDiff(Some(impl_mod), Some(decl_mod)) => {
                        format!(
                            "The implementation of the method have specified modifier \"{0:?}\" \
                            while the method in the trait declaration have modifier \"{1:?}\".\n\
                            Try changing the modifier in the implementation to \"{1:?}\"",
                            impl_mod, decl_mod
                        )
                    }
                    TraitCompareError::ThisDiff(Some(impl_mod), None) => {
                        format!(
                            "The implementation of the method have specified modifier \"{0:?}\" \
                            while the method in the trait declaration have no modifier.\n\
                            Try removing the \"{0:?}\" modifier from the implementation.",
                            impl_mod,
                        )
                    }
                    TraitCompareError::ThisDiff(None, Some(decl_mod)) => {
                        format!(
                            "The implementation of the method have specified no modifier \
                            while the method in the trait declaration have modifier \"{0:?}\".\n\
                            Try adding the modifier \"{0:?}\" to the implementation.",
                            decl_mod
                        )
                    }
                    TraitCompareError::ThisDiff(None, None) => {
                        unreachable!()
                    }
                };

                errs.push(LangError::new(
                    format!("{}{}", err_msg_start, err_msg),
                    LangErrorKind::AnalyzeError,
                    None,
                ));
            }

            Err(errs)
        } else {
            Ok(())
        }
    }

    /// If the trait have specified generics, this function ensures that the
    /// generics are implement correctly.
    ///
    /// For example given the trait:
    ///   `trait TestTrait<T> { fn test_fn(x: T) }`
    /// and the implemented:
    ///   `impl TestStrict: TestTrait<i8> { fn test_fn(x: i16) {} }`
    /// this function would see that the parameter `x` has the type `i16` when
    /// it should be the same type as `T` (`i8`) and would therefore report this
    /// as an error.
    ///
    /// This function should be called after `validate_method`. This allows us
    /// to assume that the methods and their parameters/return types are correct
    /// and we don't have to check counts etc.
    fn validate_generics(
        ty_env: &TyEnv,
        impl_method: &Fn,
        trait_method: &Fn,
        adt_path: &LangPath,
        trait_path: &LangPath,
        gens: &Generics,
    ) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::default();

        // Since the `this`/`self` parameter won't be set for the trait function,
        // need to take that into consideration if the function as a `this` modifier.
        let contains_this = impl_method.modifiers.contains(&Modifier::This)
            || impl_method.modifiers.contains(&Modifier::ThisPointer);

        // Collect pairs of the type in the trait implementation with the
        // corresponding types in the trait declaration. These types will then
        // be checked one-by-one to make sure that the generics are implemented
        // by the types found in `gens`.
        let mut type_ids = HashSet::new();

        match (
            &impl_method.parameters,
            &trait_method.parameters,
            contains_this,
        ) {
            // Both functions have parameters and contains "this".
            (Some(impl_params), Some(decl_params), true) => {
                for (impl_param, decl_param) in
                    impl_params[1..impl_params.len()].iter().zip(decl_params)
                {
                    let impl_id = impl_param.read().ty.unwrap();
                    let decl_id = decl_param.read().ty.unwrap();
                    type_ids.insert((impl_id, decl_id));
                }
            }

            // Both functions have parameters and does NOT contain "this".
            (Some(impl_params), Some(decl_params), false) => {
                for (impl_param, decl_param) in impl_params.iter().zip(decl_params) {
                    let impl_id = impl_param.read().ty.unwrap();
                    let decl_id = decl_param.read().ty.unwrap();
                    type_ids.insert((impl_id, decl_id));
                }
            }

            _ => (),
        }

        if let (Some(impl_id), Some(decl_id)) = (&impl_method.ret_type, &trait_method.ret_type) {
            type_ids.insert((*impl_id, *decl_id));
        }

        let method_name = &impl_method.name;
        for (impl_id, decl_id) in type_ids {
            if let Err(err) = Self::check_generic_match(
                ty_env,
                impl_id,
                decl_id,
                adt_path,
                trait_path,
                method_name,
                gens,
            ) {
                errors.push(err);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_generic_match(
        ty_env: &TyEnv,
        impl_id: TypeId,
        decl_id: TypeId,
        adt_path: &LangPath,
        trait_path: &LangPath,
        method_name: &str,
        gens: &Generics,
    ) -> LangResult<()> {
        let impl_ty = ty_env.ty(impl_id)?;
        let decl_ty = ty_env.ty(decl_id)?;
        match (impl_ty, decl_ty) {
            (_, Ty::Generic(gen_name, _, _)) | (_, Ty::GenericInstance(gen_name, _, _)) => {
                // Use the textual representation of the types since they might
                // come from different substitution sets, but at the same time
                // be the same type.
                let impl_type = to_string_type_id(ty_env, impl_id)?;
                let trait_impl_type = if let Some(trait_impl_type_id) = gens.get(gen_name) {
                    to_string_type_id(ty_env, trait_impl_type_id)?
                } else {
                    return Err(LangError::new(
                        format!(
                            "Found unknown generic with name \"{}\" in method \"{}\" in impl \
                            of trait \"{}\" for ADT \"{}\".",
                            gen_name,
                            method_name,
                            to_string_path(ty_env, trait_path),
                            to_string_path(ty_env, adt_path),
                        ),
                        LangErrorKind::AnalyzeError,
                        trait_path.file_pos,
                    ));
                };

                if impl_type != trait_impl_type {
                    return Err(LangError::new(
                        format!(
                            "Found incorrectly implement generic in impl of trait \"{}\" for \
                            ADT \"{}\". Expected generic with name \"{}\" to have type \"{}\" \
                            (as implemented on the impl-block), but the type found in the method \
                            \"{}\" was: {}",
                            to_string_path(ty_env, trait_path),
                            to_string_path(ty_env, adt_path),
                            gen_name,
                            trait_impl_type,
                            method_name,
                            impl_type
                        ),
                        LangErrorKind::AnalyzeError,
                        trait_path.file_pos,
                    ));
                }
            }

            (Ty::CompoundType(impl_inner_ty, ..), Ty::CompoundType(decl_inner_ty, ..)) => {
                if let (Some(impl_gens), Some(decl_gens)) =
                    (impl_inner_ty.gens(), decl_inner_ty.gens())
                {
                    for (impl_gen, decl_gen) in impl_gens.iter_types().zip(decl_gens.iter_types()) {
                        Self::check_generic_match(
                            ty_env,
                            *impl_gen,
                            *decl_gen,
                            adt_path,
                            trait_path,
                            method_name,
                            gens,
                        )?;
                    }
                }
            }

            (Ty::Pointer(impl_id_i, ..), Ty::Pointer(decl_id_i, ..)) => {
                Self::check_generic_match(
                    ty_env,
                    *impl_id_i,
                    *decl_id_i,
                    adt_path,
                    trait_path,
                    method_name,
                    gens,
                )?;
            }

            (Ty::Array(impl_id_i, ..), Ty::Array(decl_id_i, ..)) => {
                Self::check_generic_match(
                    ty_env,
                    *impl_id_i,
                    *decl_id_i,
                    adt_path,
                    trait_path,
                    method_name,
                    gens,
                )?;
            }

            (
                Ty::Fn(impl_gens, impl_params, impl_ret, ..),
                Ty::Fn(decl_gens, decl_params, decl_ret, ..),
            ) => {
                for (impl_gen, decl_gen) in impl_gens.iter().zip(decl_gens) {
                    Self::check_generic_match(
                        ty_env,
                        *impl_gen,
                        *decl_gen,
                        adt_path,
                        trait_path,
                        method_name,
                        gens,
                    )?;
                }
                for (impl_param, decl_param) in impl_params.iter().zip(decl_params) {
                    Self::check_generic_match(
                        ty_env,
                        *impl_param,
                        *decl_param,
                        adt_path,
                        trait_path,
                        method_name,
                        gens,
                    )?;
                }
                if let (Some(impl_ret), Some(decl_ret)) = (impl_ret, decl_ret) {
                    Self::check_generic_match(
                        ty_env,
                        *impl_ret,
                        *decl_ret,
                        adt_path,
                        trait_path,
                        method_name,
                        gens,
                    )?;
                }
            }

            _ => (),
        }

        Ok(())
    }
}

impl Visitor for TraitMethodsAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_impl(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let (adt_path, trait_path, impl_body) = if let Block {
            header: BlockHeader::Implement(adt_path, trait_path),
            body,
            ..
        } = block
        {
            (adt_path, trait_path, body)
        } else {
            return;
        };

        if let Err(errs) = Self::validate_impl(ctx, adt_path, trait_path, impl_body) {
            self.errors.extend(errs);
        }
    }
}
