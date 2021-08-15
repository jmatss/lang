use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
    sync::{Arc, RwLock},
};

use either::Either;
use log::debug;

use common::{
    error::LangError,
    file::FilePosition,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{AdtKind, Block, BlockHeader, Fn},
        expr::{AdtInit, ArrayInit, BuiltInCall, Expr, FnCall, Var},
        lit::{Lit, StringType},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Modifier,
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        generics::Generics,
        get::{get_file_pos, get_file_pos_mut, get_generics},
        inner_ty::InnerTy,
        replace::{replace_gen_impls, replace_unique_ids},
        solve::insert_constraint,
        substitution_sets::sub_sets_debug_print,
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
        type_id::TypeId,
        type_info::TypeInfo,
    },
};

use crate::util::generics::combine_generics;

/// Infers types for exprs that doesn't have a type explicitly set.
/// For more information about the algorithm, see:
///   http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
///
/// This struct only runs the first step of the algorithm which assigns temp
/// types to expressions and populates the "substitution" sets in "TypeContext"
/// that will be used to convert those temp types to "real" ones.
/// That conversion is done in another step by the "TypeSolver".
pub struct TypeInferencer {
    /// Keep a copy of the current function which body is being traversed.
    /// This will let the statements/exprs etc. inside the function know
    /// about the types of the parameters and the return type.
    cur_func: Option<Arc<RwLock<Fn>>>,

    /// Contains the current match expression. Its type needs to be the same
    /// as the type in the match cases.
    cur_match_expr: Option<Expr>,

    errors: Vec<LangError>,
}

impl TypeInferencer {
    pub fn new() -> Self {
        Self {
            cur_func: None,
            cur_match_expr: None,
            errors: Vec::default(),
        }
    }

    /// Helper function to insert a new constraint and store a potential error
    /// in `self.errors()`. Use this function do it in a single line func call
    /// instead of having to check for errors every time.
    fn insert_constraint(&mut self, ctx: &mut TraverseCtx, type_id_a: TypeId, type_id_b: TypeId) {
        if let Err(err) = insert_constraint(&ctx.ty_env, type_id_a, type_id_b) {
            self.errors.push(err);
        }
    }
}

impl Visitor for TypeInferencer {
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

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        let mut all_type_ids = ctx
            .ty_env
            .lock()
            .unwrap()
            .interner
            .all_types()
            .into_iter()
            .collect::<Vec<_>>();
        all_type_ids.sort_unstable();

        let mut all_types_string = String::new();
        for type_id in all_type_ids {
            all_types_string.push_str(&format!(
                "\ntype_id: {} - {:?}",
                type_id,
                to_string_type_id(&ctx.ty_env.lock().unwrap(), type_id)
            ));
        }

        debug!(
            "Type inference done.\nforwards: {:#?}\nall types: {}\nsubs:",
            ctx.ty_env.lock().unwrap().forwards(),
            all_types_string
        );
        sub_sets_debug_print(&ctx.ty_env.lock().unwrap());
    }

    /// Assigns a "Unknown" type for every expression that doesn't have a type
    /// explicitly set. This new type will then be temporarilty used during this
    /// stage and should be converted/subtituted into a "real" type before this
    /// analyzing step is done.
    fn visit_lit(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        let file_pos = expr.file_pos().cloned();

        let (lit, type_id_opt, type_info) = if let Expr::Lit(lit, type_id_opt, file_pos) = expr {
            let type_info = TypeInfo::Lit(file_pos.to_owned());
            (lit, type_id_opt, type_info)
        } else {
            unreachable!()
        };

        if type_id_opt.is_some() {
            return;
        }

        let inner_ty = match lit {
            Lit::String(_, string_type) => match string_type {
                StringType::Regular => InnerTy::Struct(LangPath::new(
                    vec!["std".into(), "StringView".into()],
                    file_pos,
                )),

                StringType::S | StringType::F => {
                    InnerTy::Struct(LangPath::new(vec!["std".into(), "String".into()], file_pos))
                }

                // A C string requires special logic since it should be a pointer,
                // it should be a compound type like all the other variants
                // handled in this function. Because of this, there is a early
                // return in this case.
                StringType::C => {
                    let u8_ty = Ty::CompoundType(InnerTy::U8, Generics::empty(), type_info.clone());
                    let u8_type_id = match ctx.ty_env.lock().unwrap().id(&u8_ty) {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    let ptr_ty = Ty::Pointer(u8_type_id, type_info);
                    let ptr_type_id = match ctx.ty_env.lock().unwrap().id(&ptr_ty) {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    *type_id_opt = Some(ptr_type_id);

                    return;
                }
            },

            Lit::Char(_) => InnerTy::Character,
            Lit::Bool(_) => InnerTy::Boolean,
            Lit::Integer(_, radix) => {
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                InnerTy::UnknownInt(unique_id, *radix)
            }
            Lit::Float(_) => {
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                InnerTy::UnknownFloat(unique_id)
            }
        };

        let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
            inner_ty,
            Generics::empty(),
            type_info,
        )) {
            Ok(type_id) => type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        *type_id_opt = Some(type_id);
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {
        let var_decl = match ctx.ast_ctx.get_var(&var.name, ctx.block_id) {
            Ok(var_decl) => var_decl,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let var_decl_type_id = if let Some(type_id) = var_decl.as_ref().read().unwrap().ty {
            type_id
        } else {
            let err = ctx
                .ast_ctx
                .err(format!("Ret type not set for var decl: {:#?}", var_decl));
            self.errors.push(err);
            return;
        };

        let var_type_id = if let Some(type_id) = &var.ty {
            *type_id
        } else {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let new_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                InnerTy::Unknown(unique_id),
                Generics::new(),
                TypeInfo::VarUse(var.file_pos.unwrap()),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            var.ty = Some(new_type_id);
            new_type_id
        };

        // Add type constraint between var "use" and var "decl",
        self.insert_constraint(ctx, var_decl_type_id, var_type_id);
    }

    // TODO: Clean up.
    /// Assign the return type of the function to the function call expr.
    /// Also tie the types of the function parameter to argument types.
    ///
    /// If either the return type or the parameters contains references to a
    /// generic type, creates new "GenericInstance"s of them instead and does NOT
    /// tie them together with a constraint. This is done since a Generic can
    /// have multiple differet types depending on the context, which isn't solvable
    /// through the regular type inference logic.
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        // TODO: Support varargs for fn pointers?
        // TODO: Support named arguments for fn pointers?

        let mut fn_ret_ty = if fn_call.is_fn_ptr_call {
            let var_name = fn_call.name.clone();
            let decl_id = match ctx.ast_ctx.get_var_decl_scope(&var_name, ctx.block_id) {
                Ok(decl_id) => decl_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (fn_call.name.clone(), decl_id);
            let var = match ctx.ast_ctx.variables.get(&key) {
                Some(var) => var,
                None => {
                    let err = ctx.ast_ctx.err(
                        format!(
                            "Unable to find variable named \"{}\" containing a fn ptr in decl scope {}.",
                            var_name, decl_id
                        ),
                    );
                    self.errors.push(err);
                    return;
                }
            };

            let (fn_gens, fn_params, fn_ret_ty) =
                if let Some(fn_type_id) = var.as_ref().borrow().read().unwrap().ty {
                    let fn_ty = match ctx.ty_env.lock().unwrap().ty_clone(fn_type_id) {
                        Ok(ty) => ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if let Ty::Fn(gens, params, ret_ty, _) = fn_ty {
                        (gens, params, ret_ty)
                    } else {
                        let err = ctx.ast_ctx.err(format!(
                            "Variable named \"{}\" expected to contain a fn ptr, but didn't: {:#?}",
                            var_name, fn_type_id
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "No type set for variable named \"{}\" expected to contain a fn ptr.",
                        var_name
                    ));
                    self.errors.push(err);
                    return;
                };

            // Fn pointers doesn't support named arguments, so can zip the args
            // and param types since their indices is always used to map them.
            if fn_call.arguments.len() == fn_params.len() {
                for (arg, param_type_id) in fn_call.arguments.iter().zip(fn_params.iter()) {
                    let arg_type_id = match arg.value.get_expr_type() {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };
                    self.insert_constraint(ctx, arg_type_id, *param_type_id);
                }
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Wrong amount of arguments for fn pointer call.\n\
                    Func call: {:#?}\nfn_params: {:#?}",
                    fn_call, fn_params
                ));
                self.errors.push(err);
                return;
            }

            let fn_call_gens = if let Some(gens) = &fn_call.generics {
                gens.iter_types().cloned().collect::<Vec<_>>()
            } else {
                Vec::with_capacity(0)
            };

            if fn_call_gens.len() == fn_gens.len() {
                for (fn_call_gen, fn_gen) in fn_call_gens.iter().zip(fn_gens.iter()) {
                    self.insert_constraint(ctx, *fn_call_gen, *fn_gen);
                }
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Wrong amount of generics for fn pointer call.\n\
                    Func call: {:#?}\nfn_gens: {:#?}",
                    fn_call, fn_gens
                ));
                self.errors.push(err);
                return;
            }

            if let Some(type_id) = fn_ret_ty {
                *get_file_pos_mut(&mut ctx.ty_env.lock().unwrap(), type_id).unwrap() =
                    fn_call.file_pos.unwrap();
                type_id
            } else {
                match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::Void,
                    Generics::empty(),
                    TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            }
        } else if fn_call.is_method {
            // Get the "owning" ADT type of this method. If it isn't set explicitly,
            // it should be set as a expression in the first argument with name "this".
            let mut adt_type_id = if let Some(adt_type_id) = &fn_call.method_adt {
                *adt_type_id
            } else if let Some(first_arg) = fn_call.arguments.first() {
                if first_arg.name.as_ref().map_or(false, |name| name == "this") {
                    match first_arg.value.get_expr_type() {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    panic!(
                        "First arg of method with no method_adt set not \"this\": {:#?}",
                        fn_call
                    );
                }
            } else {
                panic!(
                    "No params for method with no method_adt set: {:#?}",
                    fn_call
                );
            };

            // If the ADT type is know and contains generics, this logic will fetch
            // the ADT and combine the names for the generics found in the ADT
            // declaration with potential generic impls in the ADT init/func call.
            let fn_half_path = fn_call
                .module
                .clone_push(&fn_call.name, None, fn_call.file_pos);

            let mut adt_ty_clone = match ctx.ty_env.lock().unwrap().ty_clone(adt_type_id) {
                Ok(adt_ty) => adt_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Update the generics if necessary. This will combine/check the gens
            // of this method ADT use and the actual ADT declaration.
            let gens_was_updated = if let Ty::CompoundType(inner_ty, generic_types, ..) =
                &mut adt_ty_clone
            {
                match combine_generics(
                    ctx,
                    inner_ty.get_ident().as_ref(),
                    generic_types,
                    &fn_half_path,
                ) {
                    Ok(combined_gens) => *generic_types = combined_gens,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
                true
            } else if let Ty::Pointer(adt_type_id_i, ..) = &mut adt_ty_clone {
                let mut adt_ty_i_clone = match ctx.ty_env.lock().unwrap().ty_clone(*adt_type_id_i) {
                    Ok(adt_ty_i) => adt_ty_i,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Ty::CompoundType(inner_ty, generic_types, ..) = &mut adt_ty_i_clone {
                    match combine_generics(
                        ctx,
                        inner_ty.get_ident().as_ref(),
                        generic_types,
                        &fn_half_path,
                    ) {
                        Ok(combined_gens) => {
                            *generic_types = combined_gens;
                            let new_type_id_i = match ctx.ty_env.lock().unwrap().id(&adt_ty_i_clone)
                            {
                                Ok(new_type_id_i) => new_type_id_i,
                                Err(err) => {
                                    self.errors.push(err);
                                    return;
                                }
                            };
                            *adt_type_id_i = new_type_id_i;
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                    true
                } else {
                    false
                }
            } else {
                false
            };

            if gens_was_updated {
                let mut ty_env_guard = ctx.ty_env.lock().unwrap();
                match ty_env_guard.id(&adt_ty_clone) {
                    Ok(new_adt_type_id) => {
                        if adt_type_id != new_adt_type_id {
                            if let Err(err) = ty_env_guard.forward(adt_type_id, new_adt_type_id) {
                                self.errors.push(err);
                                return;
                            }

                            // Update the ADT to use in the rest of this function.
                            adt_type_id = new_adt_type_id;
                        }
                    }
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            }

            // Set the `method_adt` for the function call now that the `method_adt`
            // might have been updated. This call might have no effect if no
            // modifications have been done in the logic above.
            fn_call.method_adt = Some(adt_type_id);

            let fn_call_gens = fn_call
                .generics
                .clone()
                .unwrap_or_else(Generics::empty)
                .iter_types()
                .cloned()
                .collect::<Vec<_>>();

            // Insert constraints between the function call argument type and
            // the method parameter types that will be figured out later.
            for (idx, arg) in fn_call.arguments.iter().enumerate() {
                // If the argument is a named argument, give the argument name
                // to the new "UnknownMethodArgument" to try and figure out the
                // position of the argument through it. Otherwise use the index.
                let position = if let Some(arg_name) = &arg.name {
                    Either::Left(arg_name.into())
                } else {
                    Either::Right(idx)
                };

                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                let arg_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::UnknownMethodArgument(
                    adt_type_id,
                    fn_call.name.clone(),
                    fn_call_gens.clone(),
                    position,
                    unique_id,
                    TypeInfo::DefaultOpt(arg.value.file_pos().cloned()),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let arg_expr_type_id = match arg.value.get_expr_type() {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                // TODO: Need to be able to solve this in some way. Currently it
                //       sometimes causes a infinite loop because of nested types.
                //       This needs to be solved some way in the future because
                //       some param/args can't be type inferred/checked atm.
                // Don't add a constraint if the argument has the same type as
                // the ADT.
                if arg_expr_type_id != adt_type_id {
                    self.insert_constraint(ctx, arg_type_id, arg_expr_type_id);
                }
            }

            // Insert constraints between the function call generic type and
            // the method generic types that will be figured out later.
            if let Some(generics) = &fn_call.generics {
                for (idx, type_id) in generics.iter_types().enumerate() {
                    let type_id_file_pos =
                        get_file_pos(&ctx.ty_env.lock().unwrap(), *type_id).cloned();

                    let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                    let unknown_gen_type_id =
                        match ctx.ty_env.lock().unwrap().id(&Ty::UnknownMethodGeneric(
                            adt_type_id,
                            fn_call.name.clone(),
                            Either::Left(idx),
                            unique_id,
                            TypeInfo::DefaultOpt(type_id_file_pos),
                        )) {
                            Ok(type_id) => type_id,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                    self.insert_constraint(ctx, unknown_gen_type_id, *type_id);
                }
            }

            // The expected return type of the function call.
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            match ctx.ty_env.lock().unwrap().id(&Ty::UnknownAdtMethod(
                adt_type_id,
                fn_call.name.clone(),
                fn_call_gens,
                unique_id,
                TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        } else {
            let partial_path_without_gens =
                fn_call
                    .module
                    .clone_push(&fn_call.name, None, fn_call.file_pos);
            let full_path = match ctx.ast_ctx.calculate_fn_full_path(
                &ctx.ty_env.lock().unwrap(),
                &partial_path_without_gens,
                ctx.block_id,
            ) {
                Ok(full_path) => full_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let func = match ctx.ast_ctx.get_fn(&ctx.ty_env.lock().unwrap(), &full_path) {
                Ok(func) => func,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Iterate through all arguments of the function and match
            // up their types with the parameters of the function.
            // The amount of args/params will already have been checked before,
            // just make sure that this doesn't break for vararg functions.
            // The "similar" logic for methods will be done during type solving
            // in `type_context` since at this point there is no way to know
            // the type of the struct and indirectly the method.
            if let Some(params) = &func.as_ref().borrow().read().unwrap().parameters {
                for (idx, arg) in fn_call.arguments.iter().enumerate() {
                    // If the argument is a named argument, get the index for the
                    // named parameter instead of using the index of its position
                    // in the function call.
                    let inner_idx = if let Some(arg_name) = &arg.name {
                        match ctx.ast_ctx.get_fn_param_idx(
                            &ctx.ty_env.lock().unwrap(),
                            &full_path,
                            &arg_name,
                        ) {
                            Ok(idx) => idx,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        idx
                    };

                    let arg_type_id = match arg.value.get_expr_type() {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if func.as_ref().borrow().read().unwrap().is_var_arg
                        && inner_idx >= params.len()
                    {
                        continue;
                    }

                    let param_type_id = if let Some(type_id) = &params
                        .get(inner_idx as usize)
                        .map(|param| param.as_ref().borrow().read().unwrap().ty)
                        .flatten()
                    {
                        *type_id
                    } else {
                        let err = ctx.ast_ctx.err(
                            format!(
                                "Type for parameter \"{:?}\" with index {} in function \"{}\" set to None.",
                                arg.name, inner_idx, func.as_ref().borrow().read().unwrap().name
                            ),
                        );
                        self.errors.push(err);
                        return;
                    };

                    self.insert_constraint(ctx, arg_type_id, param_type_id);
                }
            }

            let fn_gens = func
                .read()
                .unwrap()
                .generics
                .clone()
                .unwrap_or_else(Generics::empty)
                .iter_types()
                .cloned()
                .collect::<Vec<_>>();

            let fn_call_gens = fn_call
                .generics
                .clone()
                .unwrap_or_else(Generics::empty)
                .iter_types()
                .cloned()
                .collect::<Vec<_>>();

            if fn_call_gens.len() == fn_gens.len() {
                for (fn_call_gen, fn_gen) in fn_call_gens.iter().zip(fn_gens.iter()) {
                    self.insert_constraint(ctx, *fn_call_gen, *fn_gen);
                }
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Wrong amount of generics for fn call.\n\
                    Func call: {:#?}\nfn_gens: {:#?}",
                    fn_call, fn_gens
                ));
                self.errors.push(err);
                return;
            }

            let func = func.as_ref().borrow().read().unwrap();
            if let Some(type_id) = func.ret_type {
                *get_file_pos_mut(&mut ctx.ty_env.lock().unwrap(), type_id).unwrap() =
                    fn_call.file_pos.unwrap();
                type_id
            } else {
                match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::Void,
                    Generics::empty(),
                    TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            }
        };

        // Replace any "Generic"s with "GenericInstances"s instead so that the
        // "Generic"s doesn't leak out to outside the function. Instead a
        // unique instance of a generic should be used instead. This will allow
        // for multiple different types to be mapped to the same single "Generic".
        let fn_ret_ty_gens = get_generics(&ctx.ty_env.lock().unwrap(), fn_ret_ty);
        if let Ok(gen_tys) = fn_ret_ty_gens {
            let mut generics_impl = Generics::new();

            for gen_type_id in &gen_tys {
                let gen_ty = match ctx.ty_env.lock().unwrap().ty_clone(*gen_type_id) {
                    Ok(gen_ty) => gen_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Ty::Generic(ident, ..) = gen_ty {
                    let file_pos = get_file_pos(&ctx.ty_env.lock().unwrap(), *gen_type_id).cloned();

                    let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                    let generic_impl_type_id =
                        match ctx.ty_env.lock().unwrap().id(&Ty::GenericInstance(
                            ident.clone(),
                            unique_id,
                            TypeInfo::DefaultOpt(file_pos),
                        )) {
                            Ok(type_id) => type_id,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                    generics_impl.insert(ident, generic_impl_type_id);
                } else {
                    unreachable!("Got non generic from `get_generics()`.");
                }
            }

            match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, fn_ret_ty, &generics_impl) {
                Ok(Some(new_type_id)) => fn_ret_ty = new_type_id,
                Ok(None) => (),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        // TODO: Is it correct to directly set the return type for the function
        //       call? Should be inserted as a constraint instead? Will this
        //       affect generics?
        fn_call.ret_type = Some(fn_ret_ty);
    }

    fn visit_fn_ptr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        let fn_ptr = if let Expr::FnPtr(fn_ptr) = expr {
            fn_ptr
        } else {
            unreachable!()
        };

        let fn_ptr_gens = if let Some(gens) = &fn_ptr.generics {
            gens.clone()
        } else {
            Generics::empty()
        };

        let partial_path =
            fn_ptr
                .module
                .clone_push(&fn_ptr.name, fn_ptr.generics.as_ref(), fn_ptr.file_pos);
        let full_path = match ctx.ast_ctx.calculate_fn_full_path(
            &ctx.ty_env.lock().unwrap(),
            &partial_path,
            ctx.block_id,
        ) {
            Ok(full_path) => full_path,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let func = match ctx.ast_ctx.get_fn(&ctx.ty_env.lock().unwrap(), &full_path) {
            Ok(func) => func,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let func = func.as_ref().borrow().read().unwrap();

        let fn_gen_names = if let Some(gens) = &func.generics {
            gens.iter_names().cloned().collect::<Vec<_>>()
        } else {
            Vec::with_capacity(0)
        };

        let mut fn_param_tys = if let Some(params) = &func.parameters {
            params
                .iter()
                .map(|var| {
                    var.as_ref()
                        .borrow()
                        .read()
                        .unwrap()
                        .ty
                        .as_ref()
                        .unwrap()
                        .to_owned()
                })
                .collect::<Vec<_>>()
        } else {
            Vec::with_capacity(0)
        };

        let mut fn_ret_ty = func.ret_type;

        if fn_ptr_gens.len() != fn_gen_names.len() {
            let err = ctx.ast_ctx.err(format!(
                "Function pointer to \"{}\" has incorrect amount of generics. Expected: {}, got: {}",
                to_string_path(&ctx.ty_env.lock().unwrap(), &full_path),
                fn_gen_names.len(),
                fn_ptr_gens.len()
            ));
            self.errors.push(err);
            return;
        }

        // Combine the names for the fn declaration generics with the impl types
        // of the function pointer. This will be used to replace the generics
        // in the param and return types of the functions so that the generic
        // decls doesn't "leak" outside the scope of the function.
        let mut gens_impl = Generics::new();
        for (gen_name, gen_ty) in fn_gen_names.iter().zip(fn_ptr_gens.iter_types()) {
            gens_impl.insert(gen_name.clone(), *gen_ty);
        }

        for fn_param_type_id in &mut fn_param_tys {
            match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, *fn_param_type_id, &gens_impl) {
                Ok(Some(new_type_id)) => *fn_param_type_id = new_type_id,
                Ok(None) => (),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        if let Some(ret_type_id) = &mut fn_ret_ty {
            match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, *ret_type_id, &gens_impl) {
                Ok(Some(new_type_id)) => *ret_type_id = new_type_id,
                Ok(None) => (),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        let new_fn_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Fn(
            fn_ptr_gens.iter_types().cloned().collect::<Vec<_>>(),
            fn_param_tys,
            fn_ret_ty,
            TypeInfo::DefaultOpt(fn_ptr.file_pos),
        )) {
            Ok(type_id) => type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        if let Some(fn_type_id) = &fn_ptr.fn_ty {
            if fn_type_id != &new_fn_type_id {
                let err = ctx.ast_ctx.err(format!(
                    "Bad function signature for function pointer, fn_path: {}. \
                    fn_ty: {:#?}, new_fn_ty: {:#?}. Function pointer pos: {:#?}",
                    to_string_path(&ctx.ty_env.lock().unwrap(), &full_path),
                    fn_type_id,
                    new_fn_type_id,
                    fn_ptr.file_pos
                ));
                self.errors.push(err);
            }
        } else {
            fn_ptr.fn_ty = Some(new_fn_type_id);
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, ctx: &mut TraverseCtx) {
        let built_in = match ctx.ast_ctx.get_built_in(&built_in_call.name) {
            Ok(built_in) => built_in.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        if built_in.is_var_arg && built_in_call.arguments.len() < built_in.parameters.len() {
            let err = ctx.ast_ctx.err(format!(
                "Incorrect amount of arguments given for built-in call to \"{}\". \
                Expected vararg parameter count greater than or equal to: {}, got: {}",
                &built_in.name,
                built_in.parameters.len(),
                built_in_call.arguments.len()
            ));
            self.errors.push(err);
            return;
        } else if !built_in.is_var_arg && built_in_call.arguments.len() != built_in.parameters.len()
        {
            let err = ctx.ast_ctx.err(format!(
                "Incorrect amount of arguments given for built-in call to \"{}\". \
                Expected parameter count of: {}, got: {}",
                &built_in.name,
                built_in.parameters.len(),
                built_in_call.arguments.len()
            ));
            self.errors.push(err);
            return;
        }

        for (built_in_param, built_in_call_arg) in
            built_in.parameters.iter().zip(&built_in_call.arguments)
        {
            let built_in_type_id = built_in_param.ty.unwrap();
            let built_in_call_type_id = match built_in_call_arg.value.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Need to replace any unique IDs found in the built-in declaration
            // types so that they actual are unique. Otherwise all uses of the
            // same function types would have the same unique ID which would lead
            // to that all types would map to eachother.
            let built_in_type_id =
                match replace_unique_ids(&mut ctx.ty_env.lock().unwrap(), built_in_type_id) {
                    Ok(Some(new_type_id)) => new_type_id,
                    Ok(None) => built_in_type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
            self.insert_constraint(ctx, built_in_type_id, built_in_call_type_id);
        }

        // Make sure that the amount of generic arguments are equals to the
        // amount of generic parameters. Also add constraints between them.
        if !(built_in.generics.is_none() && built_in_call.generics.is_none()) {
            let built_in_gens = if let Some(built_in_gens) = &built_in.generics {
                built_in_gens
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Built-in function doesn't have generics, but call has generics. Built-in: {:#?}, call: {:#?}",
                    &built_in,
                    &built_in_call
                ));
                self.errors.push(err);
                return;
            };

            let built_in_call_gens = if let Some(built_in_call_gens) = &built_in_call.generics {
                built_in_call_gens
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Built-in function have generic parameters, but call doesn't. Built-in: {:#?}, call: {:#?}",
                    &built_in,
                    &built_in_call
                ));
                self.errors.push(err);
                return;
            };

            if built_in_gens.len() != built_in_call_gens.len() {
                let err = ctx.ast_ctx.err(format!(
                    "Incorrect amount of generic arguments given for built-in call to \"{}\". Expected amount: {}, got: {}",
                    &built_in.name,
                    built_in_gens.len(),
                    built_in_call_gens.len()
                ));
                self.errors.push(err);
                return;
            }

            for (built_in_gen, built_in_call_gen) in
                built_in_gens.iter().zip(built_in_call_gens.iter_types())
            {
                // Need to replace any unique IDs found in the built-in declaration
                // types so that they actual are unique. Otherwise all uses of the
                // same function types would have the same unique ID which would lead
                // to that all types would map to eachother.
                let built_in_gen =
                    match replace_unique_ids(&mut ctx.ty_env.lock().unwrap(), *built_in_gen) {
                        Ok(Some(new_type_id)) => new_type_id,
                        Ok(None) => *built_in_gen,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };
                self.insert_constraint(ctx, built_in_gen, *built_in_call_gen);
            }
        }

        let built_in_ret_type =
            match replace_unique_ids(&mut ctx.ty_env.lock().unwrap(), built_in.ret_type) {
                Ok(Some(new_type_id)) => new_type_id,
                Ok(None) => built_in.ret_type,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
        if let Some(built_in_call_ret_type) = built_in_call.ret_type {
            self.insert_constraint(ctx, built_in_ret_type, built_in_call_ret_type);
        } else {
            built_in_call.ret_type = Some(built_in_ret_type);
        }

        if &built_in_call.name == "type" {
            // TODO: Temporary ugly hack to make `@type` work. Should do this in
            //       a different way and somewhere else.
            let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Expr(
                Box::new(built_in_call.arguments.first().unwrap().value.clone()),
                TypeInfo::BuiltInCall(built_in_call.file_pos),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            built_in_call.ret_type = Some(type_id);
        } else if &built_in_call.name == "ptr_add" || &built_in_call.name == "ptr_sub" {
            // TODO: "Temporary" hack to make the use of `@ptr_add`/`@ptr_sub` "better".
            //        Currently there are no direct link between the type of the ptr
            //        argument and the return type (which should have the same type).
            //        Add that link here.
            let arg_type = built_in_call
                .arguments
                .first()
                .map(|arg| arg.value.get_expr_type().ok())
                .flatten()
                .unwrap();
            let ret_type = built_in_call.ret_type.unwrap();
            self.insert_constraint(ctx, arg_type, ret_type);
        } else if &built_in_call.name == "array" {
            // TODO: The return type of the @array call should return a type
            //       tied to its arguments. Should be done somewhere else in
            //       the future.
            let init_val = built_in_call.arguments.get(0).unwrap().value.clone();
            let init_val_type_id = match init_val.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let arr_dim = built_in_call.arguments.get(1).unwrap().value.clone();

            let arr_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Array(
                init_val_type_id,
                Some(Box::new(arr_dim)),
                TypeInfo::BuiltInCall(built_in_call.file_pos),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            built_in_call.ret_type = Some(arr_type_id);
        }
    }

    /// Adds the correct type for the ADT init and ties the types of the ADT
    /// members with the type of the ADT init arguments.
    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        let partial_path = adt_init
            .module
            .clone_push(&adt_init.name, None, adt_init.file_pos);
        let full_path = match ctx.ast_ctx.calculate_adt_full_path(
            &ctx.ty_env.lock().unwrap(),
            &partial_path,
            ctx.block_id,
        ) {
            Ok(full_path) => full_path,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let adt = match ctx.ast_ctx.get_adt(&ctx.ty_env.lock().unwrap(), &full_path) {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let adt = adt.as_ref().borrow().read().unwrap();

        adt_init.kind = adt.kind.clone();

        // Gets a map if the generics that maps the ident of the generic
        // (ex. "T", "U" etc.) to a new unknown generic type. This is needed
        // to ensure that two members of a ADT with the same ident uses the same
        // unknown generic type. It is also needed to ensure that different ADTs
        // uses different types for the generics.
        let generics = if let Some(generics_decl) = &adt.generics {
            let mut generics = Generics::new();

            // If the ADT init call has specified explicitly the implementation
            // types for the generics, use those instead of unknown generics.
            // Currently these explicit types must be solved types.
            if let Some(generics_impl) = &adt_init.generics {
                if generics_decl.len() != generics_impl.len() {
                    let err = ctx.ast_ctx.err(format!(
                        "Wrong amount of generics for ADT init. ADT init: {:#?}, ADT: {:#?}",
                        adt_init, adt
                    ));
                    self.errors.push(err);
                    return;
                }

                for (name, gen_ty) in generics_decl.iter_names().zip(generics_impl.iter_types()) {
                    generics.insert(name.clone(), *gen_ty);
                }
            } else {
                for generic_name in generics_decl.iter_names() {
                    let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                    let gen_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::GenericInstance(
                        generic_name.clone(),
                        unique_id,
                        TypeInfo::None,
                    )) {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    generics.insert(generic_name.clone(), gen_type_id);
                }

                adt_init.generics = Some(generics.clone());
            }

            generics
        } else {
            Generics::new()
        };

        match adt_init
            .ret_type
            .map(|id| ctx.ty_env.lock().unwrap().ty_clone(id))
        {
            Some(Ok(Ty::CompoundType(..))) => {
                // If the type already is set to a compound, use that
                // already set type.
            }
            Some(Err(err)) => {
                self.errors.push(err);
                return;
            }
            _ => {
                let ret_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::UnknownIdent(full_path.clone(), ctx.block_id),
                    generics.clone(),
                    TypeInfo::Default(adt_init.file_pos.unwrap()),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                adt_init.ret_type = Some(ret_type_id);
            }
        };

        let members = &adt.members;

        // TODO: Move out logic to new function.
        match adt.kind {
            AdtKind::Struct => {
                if members.len() != adt_init.arguments.len() {
                    let err = ctx.ast_ctx.err(format!(
                        "ADT \"{}\" and ADT init has diff amount of members. ADT#: {:?}, init#: {:?}.",
                        &adt.name,
                        members.len(),
                        adt_init.arguments.len()
                    ));
                    self.errors.push(err);
                    return;
                }

                // TODO: Verify that all members are initialized.

                for (i, arg) in adt_init.arguments.iter_mut().enumerate() {
                    // If a name is set, this is a named member init. Don't use the
                    // iterator index, get the corrent index of the struct field with
                    // the name `arg.name`.
                    let index: usize = if let Some(arg_name) = &arg.name {
                        match ctx.ast_ctx.get_adt_member_index(
                            &ctx.ty_env.lock().unwrap(),
                            &full_path,
                            arg_name,
                        ) {
                            Ok(idx) => idx as usize,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        i
                    };

                    // TODO: Make sure that the ADT init argument is compatible
                    //       with the member ADT type. Currently this doesn't
                    //       get caught until the codegen stage.

                    // Add constraints mapping the type of the ADT init argument
                    // to the corresponding actual ADT member type.
                    match arg.value.get_expr_type() {
                        Ok(arg_type_id) => {
                            if let Some(member) = members.get(index) {
                                // Make a copy of the type to allow for multiple
                                // struct inits with different types for the generics.
                                let mut new_member =
                                    member.as_ref().borrow().read().unwrap().clone();

                                // Get the "actual" type of the member. If it contains
                                // a generic, it needs to get the actual unknown
                                // generic type from the `generics` map.
                                // Otherwise reuse the already set type.
                                let member_type_id = if let Some(member_type_id) =
                                    &mut new_member.ty
                                {
                                    match replace_gen_impls(
                                        &ctx.ty_env,
                                        &ctx.ast_ctx,
                                        *member_type_id,
                                        &generics,
                                    ) {
                                        Ok(Some(new_type_id)) => *member_type_id = new_type_id,
                                        Ok(None) => (),
                                        Err(err) => {
                                            self.errors.push(err);
                                            return;
                                        }
                                    }

                                    *member_type_id
                                } else {
                                    let err = ctx.ast_ctx.err(format!(
                                        "Member \"{:?}\" in struct \"{:?}\" doesn't have a type set.",
                                        members.get(index),
                                        &adt.name
                                    ));
                                    self.errors.push(err);
                                    return;
                                };

                                // Bind init member to actual type in struct definition.
                                self.insert_constraint(ctx, arg_type_id, member_type_id);

                                let arg_file_pos =
                                    get_file_pos(&ctx.ty_env.lock().unwrap(), arg_type_id).cloned();

                                // Bind type of member to the struct.
                                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                                let unknown_type_id =
                                    match ctx.ty_env.lock().unwrap().id(&Ty::UnknownAdtMember(
                                        adt_init.ret_type.unwrap(),
                                        new_member.name.clone(),
                                        unique_id,
                                        TypeInfo::DefaultOpt(arg_file_pos),
                                    )) {
                                        Ok(type_id) => type_id,
                                        Err(err) => {
                                            self.errors.push(err);
                                            return;
                                        }
                                    };

                                self.insert_constraint(ctx, arg_type_id, unknown_type_id);
                            } else {
                                let err = ctx.ast_ctx.err(format!(
                                    "Unable to get member at index {} in struct \"{:?}\".",
                                    index, &adt.name
                                ));
                                self.errors.push(err);
                                return;
                            }
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                }
            }

            AdtKind::Union => {
                if adt_init.arguments.len() > 1 {
                    let err = ctx.ast_ctx.err(format!(
                        "ADT init of union \"{}\" has more than one argument, has#: {}.",
                        &adt.name,
                        adt_init.arguments.len()
                    ));
                    self.errors.push(err);
                    return;
                } else if adt_init.arguments.is_empty() {
                    let err = ctx.ast_ctx.err(format!(
                        "ADT init of union \"{}\" has no argument, expected one.",
                        &adt.name,
                    ));
                    self.errors.push(err);
                    return;
                }

                let arg_init = adt_init.arguments.first().unwrap();

                let member_name = if let Some(member_name) = &arg_init.name {
                    member_name
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "ADT init of union \"{}\" doesn't have NAMED argument as expected.",
                        &adt.name,
                    ));
                    self.errors.push(err);
                    return;
                };

                let member = match ctx.ast_ctx.get_adt_member(
                    &ctx.ty_env.lock().unwrap(),
                    &full_path,
                    member_name,
                    adt_init.file_pos,
                ) {
                    Ok(member) => member,
                    Err(err) => {
                        if !self.errors.contains(&err) {
                            self.errors.push(err);
                        }
                        return;
                    }
                };

                match arg_init.value.get_expr_type() {
                    Ok(arg_type_id) => {
                        // Make a copy of the type to allow for multiple
                        // struct inits with different types for the generics.
                        let new_member = member.as_ref().borrow().read().unwrap().clone();

                        let member_type_id = if let Some(type_id) = &new_member.ty {
                            match replace_gen_impls(&ctx.ty_env, &ctx.ast_ctx, *type_id, &generics)
                            {
                                Ok(Some(new_type_id)) => new_type_id,
                                Ok(None) => *type_id,
                                Err(err) => {
                                    self.errors.push(err);
                                    return;
                                }
                            }
                        } else {
                            let err = ctx.ast_ctx.err(format!(
                                "Member \"{:?}\" in union \"{:?}\" doesn't have a type set.",
                                member.as_ref().borrow().read().unwrap(),
                                &adt.name
                            ));
                            self.errors.push(err);
                            return;
                        };

                        self.insert_constraint(ctx, arg_type_id, member_type_id);

                        let arg_file_pos =
                            get_file_pos(&ctx.ty_env.lock().unwrap(), arg_type_id).cloned();

                        // Bind type of member arg to the union.
                        let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                        let unknown_type_id =
                            match ctx.ty_env.lock().unwrap().id(&Ty::UnknownAdtMember(
                                adt_init.ret_type.unwrap(),
                                new_member.name,
                                unique_id,
                                TypeInfo::DefaultOpt(arg_file_pos),
                            )) {
                                Ok(type_id) => type_id,
                                Err(err) => {
                                    self.errors.push(err);
                                    return;
                                }
                            };
                        self.insert_constraint(ctx, arg_type_id, unknown_type_id);
                    }
                    Err(err) => {
                        self.errors.push(err);
                    }
                }
            }

            _ => unreachable!("ADT init kind: {:?}", adt.kind),
        };
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &mut TraverseCtx) {
        let ret_type_id = if let Some(ret_type_id) = &array_init.ret_type {
            *ret_type_id
        } else {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let new_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                InnerTy::Unknown(unique_id),
                Generics::new(),
                TypeInfo::Default(array_init.file_pos),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            array_init.ret_type = Some(new_type_id);
            new_type_id
        };

        let mut arg_types = Vec::new();
        for arg in &mut array_init.arguments {
            match arg.value.get_expr_type() {
                Ok(arg_type_id) => arg_types.push(arg_type_id),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        // TODO: What should the type of the index for the array size be?
        let arr_idx_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
            InnerTy::U32,
            Generics::empty(),
            TypeInfo::None,
        )) {
            Ok(type_id) => type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let dim = array_init.arguments.len();
        let dim_expr = Expr::Lit(
            Lit::Integer(dim.to_string(), 10),
            Some(arr_idx_type_id),
            Some(FilePosition::default()),
        );

        // Add a constraint for all arguments that they are members of the same
        // array type and and also add constraint between all the values in the
        // array init.
        for i in 0..array_init.arguments.len() {
            let left = arg_types.get(i).cloned().unwrap();

            let arr_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Array(
                left,
                Some(Box::new(dim_expr.clone())),
                TypeInfo::Default(array_init.file_pos),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.insert_constraint(ctx, ret_type_id, arr_type_id);

            for j in i + 1..array_init.arguments.len() {
                let right = arg_types.get(j).cloned().unwrap();
                self.insert_constraint(ctx, left, right);
            }
        }
    }

    /// Adds constraints for binary operations. Most of the bin ops requires
    /// that the lhs and rhs has the same type.
    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &mut TraverseCtx) {
        // The lhs and rhs exprs will already have been traversed and should
        // have been given a "unknown" type if they didn't have a type already.
        // The "ret_type" of this bin op will also be given a ret_type if it
        // doesn't already have a type set.
        let ret_type_id = if let Some(type_id) = &bin_op.ret_type {
            *type_id
        } else {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let new_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                InnerTy::Unknown(unique_id),
                Generics::new(),
                TypeInfo::DefaultOpt(bin_op.file_pos.to_owned()),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            bin_op.ret_type = Some(new_type_id);
            new_type_id
        };

        let lhs_type_id = match bin_op.lhs.get_expr_type() {
            Ok(lhs_type_id) => lhs_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let rhs_type_id = match bin_op.rhs.get_expr_type() {
            Ok(rhs_type_id) => rhs_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let bool_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
            InnerTy::Boolean,
            Generics::empty(),
            TypeInfo::DefaultOpt(bin_op.file_pos.to_owned()),
        )) {
            Ok(type_id) => type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        match bin_op.operator {
            // The lhs and rhs can be different in these operations, so shouldn't
            // add as constraints.
            // TODO:
            BinOperator::In | BinOperator::Is | BinOperator::Of => (),

            BinOperator::As => {
                if let Expr::Type(rhs_ty, ..) = &*bin_op.rhs {
                    // The rhs of a "as" will be a hardcoded type. The lhs
                    // doesn't have to be the same type (since it should be
                    // casted at this point), but the return type of the bin op
                    // must be the same type as the rhs.
                    //
                    // Change the type of the bin op directly so that it takes
                    // precedence during type inferencing. Also need to make sure
                    // to remove the newly created `ret_type_id` since it won't
                    // be used (and is therefore not solvable).
                    ctx.ty_env.lock().unwrap().remove(ret_type_id);
                    bin_op.ret_type = Some(*rhs_ty);
                } else {
                    let err = ctx
                        .ast_ctx
                        .err(format!("Rhs of \"as\" not a valid type: {:?}", bin_op.rhs));
                    self.errors.push(err);
                }
            }

            BinOperator::Dot => {
                self.insert_constraint(ctx, ret_type_id, rhs_type_id);
            }

            // TODO: What ret type should they have?
            BinOperator::Range | BinOperator::RangeInclusive => {
                self.insert_constraint(ctx, lhs_type_id, rhs_type_id);
            }

            BinOperator::Eq
            | BinOperator::Neq
            | BinOperator::Lt
            | BinOperator::Gt
            | BinOperator::Lte
            | BinOperator::Gte => {
                self.insert_constraint(ctx, ret_type_id, bool_type_id);
                self.insert_constraint(ctx, lhs_type_id, rhs_type_id);
            }

            BinOperator::BoolAnd | BinOperator::BoolOr => {
                self.insert_constraint(ctx, ret_type_id, bool_type_id);
                self.insert_constraint(ctx, lhs_type_id, bool_type_id);
                self.insert_constraint(ctx, rhs_type_id, bool_type_id);
            }

            BinOperator::Add
            | BinOperator::Sub
            | BinOperator::Mul
            | BinOperator::Div
            | BinOperator::Mod
            | BinOperator::BitAnd
            | BinOperator::BitOr
            | BinOperator::BitXor
            | BinOperator::ShiftLeft
            | BinOperator::ShiftRight => {
                self.insert_constraint(ctx, ret_type_id, lhs_type_id);
                self.insert_constraint(ctx, ret_type_id, rhs_type_id);
                self.insert_constraint(ctx, lhs_type_id, rhs_type_id);
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {
        // The expr value of this un op will already have been traversed and should
        // have been given a "unknown" type if it didn't have one type already.
        // The "ret_type" of this un op will also be given a ret_type if it
        // doesn't already have a type set.
        let ret_type_id = if let Some(type_id) = &un_op.ret_type {
            *type_id
        } else {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let new_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                InnerTy::Unknown(unique_id),
                Generics::new(),
                TypeInfo::Default(un_op.file_pos.unwrap()),
            )) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            un_op.ret_type = Some(new_type_id);
            new_type_id
        };

        let val_type_id = match un_op.value.get_expr_type() {
            Ok(val_type_id) => val_type_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let type_info = TypeInfo::Default(un_op.file_pos.unwrap());

        match &mut un_op.operator {
            UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::Increment
            | UnOperator::Decrement
            | UnOperator::BitComplement
            | UnOperator::BoolNot => {
                self.insert_constraint(ctx, ret_type_id, val_type_id);
            }
            UnOperator::Deref => {
                let ptr_type_id = match ctx
                    .ty_env
                    .lock()
                    .unwrap()
                    .id(&Ty::Pointer(ret_type_id, type_info))
                {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, ptr_type_id, val_type_id);
            }
            UnOperator::Address => {
                let ptr_type_id = match ctx
                    .ty_env
                    .lock()
                    .unwrap()
                    .id(&Ty::Pointer(val_type_id, type_info))
                {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, ret_type_id, ptr_type_id);
            }
            UnOperator::ArrayAccess(_) => {
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                let unknown_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::UnknownArrayMember(
                    val_type_id,
                    unique_id,
                    type_info,
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, ret_type_id, unknown_type_id);
            }
            UnOperator::UnionIs(member_name, var_decl) => {
                let var_decl_type_id = if let Stmt::VariableDecl(var, ..) = var_decl.as_ref() {
                    *var.as_ref().borrow().read().unwrap().ty.as_ref().unwrap()
                } else {
                    let err = ctx
                        .ast_ctx
                        .err(format!("lhs of \"UnionIs\" not a var decl: {:#?}", un_op));
                    self.errors.push(err);
                    return;
                };

                // TODO: Do in cleaner way.
                // The `val_ty` will be the return type of the union member,
                // so need to get the type for the "inner_un_op" which should be
                // a ADT access where the `value` will reference the ADT that is
                // being accessed. That will be inferred to the type of the union.
                let union_ty = if let Expr::Op(Op::UnOp(inner_un_op)) = un_op.value.as_ref() {
                    match inner_un_op.value.get_expr_type() {
                        Ok(union_ty) => union_ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    unreachable!("un_op.value not un op (i.e. union access): {:#?}", un_op);
                };

                // Link the type of the new var decl to the type of the member.
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                let unknown_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::UnknownAdtMember(
                    union_ty,
                    member_name.clone(),
                    unique_id,
                    type_info.clone(),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, var_decl_type_id, unknown_type_id);

                let bool_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::Boolean,
                    Generics::empty(),
                    type_info,
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                // Link the type of the whole expression to a boolean.
                self.insert_constraint(ctx, ret_type_id, bool_type_id);
            }
            UnOperator::AdtAccess(member_name, ..) | UnOperator::EnumAccess(member_name, ..) => {
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                let unknown_type_id = match ctx.ty_env.lock().unwrap().id(&Ty::UnknownAdtMember(
                    val_type_id,
                    member_name.clone(),
                    unique_id,
                    type_info,
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, ret_type_id, unknown_type_id);
            }
        }
    }

    fn visit_fn(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = &mut block
        {
            let func_ref = func.as_ref().borrow().write().unwrap();

            // If this is a method and the first argument is named "this", set
            // the type of it to the ADT that this method belongs to (which already
            // is stored in `method_adt`).
            if let Some(first_arg) = func_ref.parameters.as_ref().and_then(|args| args.first()) {
                let mut first_arg = first_arg.as_ref().borrow().write().unwrap();

                if &first_arg.name == "this" {
                    if let Some(adt_type_id) = &func_ref.method_adt {
                        let type_id = if func_ref.modifiers.contains(&Modifier::This) {
                            *adt_type_id
                        } else if func_ref.modifiers.contains(&Modifier::ThisPointer) {
                            // TODO: What file_pos should this pointer have?
                            match ctx
                                .ty_env
                                .lock()
                                .unwrap()
                                .id(&Ty::Pointer(*adt_type_id, TypeInfo::None))
                            {
                                Ok(type_id) => type_id,
                                Err(err) => {
                                    self.errors.push(err);
                                    return;
                                }
                            }
                        } else {
                            // TODO: This should be caught somewhere else earlier.
                            //       Keyword is not allowed to be used as parameter
                            //       names. Make this a unreachable at that point.
                            panic!(
                                "First parameter to function named keyword \"this\": {:#?}",
                                func_ref
                            );
                        };

                        first_arg.ty = Some(type_id);
                    }
                }
            }

            // Save the current function in a place so that the stmts/exprs in the body
            // can access the types of the parameters and the return type of the func.
            self.cur_func = Some(Arc::clone(func));
        }
    }

    // TODO: Clean up this logic, can merge stuff from `visit_struct` and `visit_impl`.

    // TODO: Implement for unions.
    /// Tie the generics in this specific struct to each other with constraints.
    /// Ties the generics in the struct members, method parameters and method
    /// return types.
    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(adt),
            ..
        } = &block
        {
            let adt = adt.as_ref().borrow().read().unwrap();

            // Populate this map with the "Generic(ident)" types where the key
            // is the name of the generic and the value is a list of all the
            // Generics that should have constraints between each other.
            let mut generics: HashMap<_, Vec<_>> = HashMap::default();

            // Gather all "Generic" types found in the members types into the
            // `generics` map. All the generic types in every entry will then
            // be tied together so that they all get infered to the same type.
            for member in &adt.members {
                let member = member.as_ref().borrow().read().unwrap();

                if let Some(type_id) = &member.ty {
                    let inner_generics = match get_generics(&ctx.ty_env.lock().unwrap(), *type_id) {
                        Ok(inner_generics) if inner_generics.is_empty() => continue,
                        Ok(inner_generics) => inner_generics,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    for gen_type_id in inner_generics {
                        let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                            Ok(ty) => ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let ident = if let Ty::Generic(ident, ..) = gen_ty {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_type_id);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_type_id);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_type_id]);
                            }
                        }
                    }
                }
            }

            for method in adt.methods.values() {
                // Gather "Generic" types from method parameters.
                if let Some(params) = &method.as_ref().borrow().read().unwrap().parameters {
                    for param in params {
                        if let Some(type_id) = param.as_ref().borrow().read().unwrap().ty.as_ref() {
                            let inner_generics =
                                match get_generics(&ctx.ty_env.lock().unwrap(), *type_id) {
                                    Ok(inner_generics) if inner_generics.is_empty() => continue,
                                    Ok(inner_generics) => inner_generics,
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                            for gen_type_id in inner_generics {
                                let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                                    Ok(ty) => ty.clone(),
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                                let ident = if let Ty::Generic(ident, ..) = gen_ty {
                                    ident
                                } else {
                                    unreachable!("gen_ty not generic: {:#?}", gen_type_id);
                                };

                                match generics.entry(ident.clone()) {
                                    Entry::Occupied(mut o) => {
                                        o.get_mut().push(gen_type_id);
                                    }
                                    Entry::Vacant(v) => {
                                        v.insert(vec![gen_type_id]);
                                    }
                                }
                            }
                        }
                    }
                }

                // Gather "Generic" types from method return type.
                if let Some(ret_type_id) =
                    method.as_ref().borrow().read().unwrap().ret_type.as_ref()
                {
                    let inner_generics =
                        match get_generics(&ctx.ty_env.lock().unwrap(), *ret_type_id) {
                            Ok(inner_generics) if inner_generics.is_empty() => continue,
                            Ok(inner_generics) => inner_generics,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                    for gen_type_id in inner_generics {
                        let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                            Ok(ty) => ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let ident = if let Ty::Generic(ident, ..) = gen_ty {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_type_id);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_type_id);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_type_id]);
                            }
                        }
                    }
                }
            }

            // Tie the types of the generics with the same ident to each other.
            for ident_generics in generics.values() {
                for i in 0..ident_generics.len() {
                    let left = ident_generics.get(i).cloned().unwrap();

                    for j in i + 1..ident_generics.len() {
                        let right = ident_generics.get(j).cloned().unwrap();
                        self.insert_constraint(ctx, left, right);
                    }
                }
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(adt),
            ..
        } = &block
        {
            let adt = adt.as_ref().borrow().read().unwrap();

            // Populate this map with the "Generic(ident)" types where the key
            // is the name of the generic and the value is a list of all the
            // Generics that should have constraints between each other.
            let mut generics: HashMap<_, Vec<_>> = HashMap::default();

            // Gather all "Generic" types found in the members types into the
            // `generics` map. All the generic types in every entry will then
            // be tied together so that they all get infered to the same type.
            for member in &adt.members {
                let member = member.as_ref().borrow().read().unwrap();

                if let Some(type_id) = &member.ty {
                    let inner_generics = match get_generics(&ctx.ty_env.lock().unwrap(), *type_id) {
                        Ok(inner_generics) if inner_generics.is_empty() => continue,
                        Ok(inner_generics) => inner_generics,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    for gen_type_id in inner_generics {
                        let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                            Ok(ty) => ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let ident = if let Ty::Generic(ident, ..) = gen_ty {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_ty);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_type_id);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_type_id]);
                            }
                        }
                    }
                }
            }

            for method in adt.methods.values() {
                // Gather "Generic" types from method parameters.
                if let Some(params) = &method.as_ref().borrow().read().unwrap().parameters {
                    for param in params {
                        if let Some(type_id) = param.as_ref().borrow().read().unwrap().ty.as_ref() {
                            let inner_generics =
                                match get_generics(&ctx.ty_env.lock().unwrap(), *type_id) {
                                    Ok(inner_generics) if inner_generics.is_empty() => continue,
                                    Ok(inner_generics) => inner_generics,
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                            for gen_type_id in inner_generics {
                                let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                                    Ok(ty) => ty.clone(),
                                    Err(err) => {
                                        self.errors.push(err);
                                        return;
                                    }
                                };

                                let ident = if let Ty::Generic(ident, ..) = gen_ty {
                                    ident
                                } else {
                                    unreachable!("gen_ty not generic: {:#?}", gen_type_id);
                                };

                                match generics.entry(ident.clone()) {
                                    Entry::Occupied(mut o) => {
                                        o.get_mut().push(gen_type_id);
                                    }
                                    Entry::Vacant(v) => {
                                        v.insert(vec![gen_type_id]);
                                    }
                                }
                            }
                        }
                    }
                }

                // Gather "Generic" types from method return type.
                if let Some(ret_type_id) =
                    method.as_ref().borrow().read().unwrap().ret_type.as_ref()
                {
                    let inner_generics =
                        match get_generics(&ctx.ty_env.lock().unwrap(), *ret_type_id) {
                            Ok(inner_generics) if inner_generics.is_empty() => continue,
                            Ok(inner_generics) => inner_generics,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                    for gen_type_id in inner_generics {
                        let gen_ty = match ctx.ty_env.lock().unwrap().ty(gen_type_id) {
                            Ok(ty) => ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let ident = if let Ty::Generic(ident, ..) = gen_ty {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_type_id);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_type_id);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_type_id]);
                            }
                        }
                    }
                }
            }

            // Tie the types of the generics with the same ident to each other.
            for ident_generics in generics.values() {
                for i in 0..ident_generics.len() {
                    let left = ident_generics.get(i).cloned().unwrap();

                    for j in i + 1..ident_generics.len() {
                        let right = ident_generics.get(j).cloned().unwrap();
                        self.insert_constraint(ctx, left, right);
                    }
                }
            }
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::Return(expr_opt, ..) = stmt {
            if let Some(func) = &self.cur_func {
                let func_ret_type_id =
                    if let Some(type_id) = &func.as_ref().borrow().read().unwrap().ret_type {
                        *type_id
                    } else {
                        // TODO: Where should this pos be fetched from?
                        match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                            InnerTy::Void,
                            Generics::empty(),
                            TypeInfo::None,
                        )) {
                            Ok(type_id) => type_id,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    };

                let expr = if let Some(expr) = expr_opt {
                    expr
                } else {
                    unreachable!("expr None -- stmt: {:#?}", stmt);
                };

                let expr_type_id = match expr.get_expr_type() {
                    Ok(expr_type_id) => expr_type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(ctx, func_ret_type_id, expr_type_id);
            } else {
                let err = ctx
                    .ast_ctx
                    .err("Unable to get cur func when looking at return stmt type.".into());
                self.errors.push(err);
            }
        }
    }

    // TODO: Write when yield gets implemented.
    fn visit_yield(&mut self, stmt: &mut Stmt, _ctx: &mut TraverseCtx) {}

    /// Save the current match expr in a place so that the match cases in the body
    /// can access the type of the expr.
    fn visit_match(&mut self, block: &mut Block, _ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Match(expr),
            ..
        } = &block
        {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::MatchCase(match_case_expr),
            ..
        } = &mut block
        {
            if let Some(match_expr) = self.cur_match_expr.clone() {
                let match_expr_type_id = match match_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Some(match_case_expr) = match_case_expr {
                    let case_expr_type_id = match match_case_expr.get_expr_type() {
                        Ok(expr_ty) => expr_ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    self.insert_constraint(ctx, match_expr_type_id, case_expr_type_id);
                }
            } else {
                let err = ctx
                    .ast_ctx
                    .err("Unable to get cur match expr when looking at match case type.".into());
                self.errors.push(err);
            }
        }
    }

    /// The types of the lhs and rhs of a assignment should be of the same type.
    /// Add it as a constraint.
    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::Assignment(_, lhs, rhs, ..) = stmt {
            debug!("ASSIGNMENT\nlhs: {:#?}\nrhs: {:#?}", lhs, rhs);

            let lhs_type_id = match lhs.get_expr_type() {
                Ok(lhs_type_id) => lhs_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let rhs_type_id = match rhs.get_expr_type() {
                Ok(rhs_type_id) => rhs_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.insert_constraint(ctx, lhs_type_id, rhs_type_id);
        }
    }

    /// The types of the lhs and rhs of a variable declaration with a init value
    /// should be of the same type, add as constraints.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            // No way to do type inference of rhs on var decl with no init value.
            let rhs_type_id_opt = {
                let var_lock = var.as_ref().borrow().read().unwrap();
                if var_lock.value.is_some() {
                    if let Some(a) = var_lock.value.clone().map(|x| *x).as_ref() {
                        match a.get_expr_type() {
                            Ok(rhs_ty) => Some(rhs_ty),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        unreachable!("expr var value None -- var: {:#?}", var);
                    }
                } else {
                    None
                }
            };

            let (var_ty_opt, var_file_pos_opt) = {
                let var_lock = var.as_ref().borrow().read().unwrap();
                (var_lock.ty, var_lock.file_pos)
            };

            // Create a unkown type if a type isn't already set. For simplicity
            // this new type will always be set, but it will contain the old type
            // if a type was already set.
            let new_type_id = if let Some(var_ty) = var_ty_opt {
                var_ty
            } else {
                // If the type isn't hardcoded, there are no file position since
                // it doesn't exist in the source code. In that case use the
                // position of the variable identifier instead.
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::Unknown(unique_id),
                    Generics::new(),
                    TypeInfo::VarDecl(var_file_pos_opt.unwrap(), false),
                )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            };

            var.as_ref().borrow().write().unwrap().ty = Some(new_type_id);

            // Add constraints only if this var decl has a init value.
            if let Some(rhs_type_id) = rhs_type_id_opt {
                self.insert_constraint(ctx, new_type_id, rhs_type_id);
            }
        }
    }
}
