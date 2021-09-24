use common::{
    error::LangResult,
    path::{LangPath, LangPathPart},
    token::expr::FnCall,
    traverse::traverse_ctx::TraverseCtx,
    ty::{
        generics::Generics,
        get::{get_file_pos, get_generics},
        inner_ty::InnerTy,
        replace::replace_gen_impls,
        ty::Ty,
        type_id::TypeId,
        type_info::TypeInfo,
    },
};
use either::Either;

use crate::{
    ty::solve::insert_constraint,
    util::generics::{combine_generics, combine_generics_adt},
};

/// Assign the return type of the function to the function call expr.
/// Also tie the types of the function parameter to argument types.
///
/// If either the return type or the parameters contains references to a
/// generic type, creates new "GenericInstance"s of them instead and does NOT
/// tie them together with a constraint. This is done since a Generic can
/// have multiple differet types depending on the context, which isn't solvable
/// through the regular type inference logic.
pub(crate) fn infer_fn_call(fn_call: &mut FnCall, ctx: &mut TraverseCtx) -> LangResult<()> {
    // TODO: Support varargs for fn pointers?
    // TODO: Support named arguments for fn pointers?

    let fn_ret_type_id = if fn_call.is_fn_ptr_call {
        infer_fn_call_fn_ptr(fn_call, ctx)?
    } else if fn_call.is_method {
        infer_fn_call_method(fn_call, ctx)?
    } else {
        infer_fn_call_fn(fn_call, ctx)?
    };

    // TODO: Is it correct to directly set the return type for the function
    //       call? Should be inserted as a constraint instead? Will this
    //       affect generics?
    fn_call.ret_type = Some(fn_ret_type_id);

    Ok(())
}

pub(super) fn infer_fn_call_fn_ptr(fn_call: &FnCall, ctx: &mut TraverseCtx) -> LangResult<TypeId> {
    let var_name = fn_call.name.clone();
    let decl_id = ctx.ast_ctx.get_var_decl_scope(&var_name, ctx.block_id)?;

    let key = (fn_call.name.clone(), decl_id);
    let var = match ctx.ast_ctx.variables.get(&key) {
        Some(var) => var,
        None => {
            return Err(ctx.ast_ctx.err(format!(
                "Unable to find variable named \"{}\" containing a fn ptr in decl scope {}.",
                var_name, decl_id
            )));
        }
    };

    let mut ty_env_guard = ctx.ty_env.lock();

    let (fn_gens, fn_params, fn_ret_type_id) = if let Some(fn_type_id) = var.read().ty {
        let fn_ty = ty_env_guard.ty_clone(fn_type_id)?;
        if let Ty::Fn(gens, params, ret_ty, _) = fn_ty {
            (gens, params, ret_ty)
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Variable named \"{}\" expected to contain a fn ptr, but didn't: {:#?}",
                var_name, fn_type_id
            )));
        }
    } else {
        return Err(ctx.ast_ctx.err(format!(
            "No type set for variable named \"{}\" expected to contain a fn ptr.",
            var_name
        )));
    };

    if fn_call.arguments.len() != fn_params.len() {
        return Err(ctx.ast_ctx.err(format!(
            "Wrong amount of arguments for fn pointer call.\nFunc call: {:#?}\nfn_params: {:#?}",
            fn_call, fn_params
        )));
    }

    // Fn pointers doesn't support named arguments, so can zip the args
    // and param types since their indices is always used to map them.
    for (arg, param_type_id) in fn_call.arguments.iter().zip(fn_params.iter()) {
        let arg_type_id = arg.value.get_expr_type()?;
        insert_constraint(&mut ty_env_guard, arg_type_id, *param_type_id)?;
    }

    let fn_call_gens = fn_call
        .generics
        .as_ref()
        .map(|gens| gens.iter_types().cloned().collect())
        .unwrap_or_else(|| Vec::with_capacity(0));

    if fn_call_gens.len() != fn_gens.len() {
        return Err(ctx.ast_ctx.err(format!(
            "Wrong amount of generics for fn pointer call.\n\
                Func call: {:#?}\nfn_gens: {:#?}",
            fn_call, fn_gens
        )));
    }

    for (fn_call_gen, fn_gen) in fn_call_gens.iter().zip(fn_gens.iter()) {
        insert_constraint(&mut ty_env_guard, *fn_call_gen, *fn_gen)?;
    }

    if let Some(mut fn_ret_type_id) = fn_ret_type_id {
        // Replace any "Generic"s with "GenericInstances"s instead so that the
        // "Generic"s doesn't leak out to outside the function. Instead a
        // unique instance of a generic should be used instead. This will allow
        // for multiple different types to be mapped to the same single "Generic".
        let fn_ret_type_id_gens = get_generics(&ty_env_guard, fn_ret_type_id)?;
        let mut gen_impls = Generics::new();

        for gen_type_id in &fn_ret_type_id_gens {
            let gen_ty = ty_env_guard.ty_clone(*gen_type_id)?;

            if let Ty::Generic(ident, ..) = gen_ty {
                let file_pos = get_file_pos(&ty_env_guard, *gen_type_id).cloned();

                let unique_id = ty_env_guard.new_unique_id();
                let gen_impl_type_id = ty_env_guard.id(&Ty::GenericInstance(
                    ident.clone(),
                    unique_id,
                    TypeInfo::DefaultOpt(file_pos),
                ))?;

                gen_impls.insert(ident, gen_impl_type_id);
            } else {
                unreachable!("Got non generic from `get_generics()`.");
            }
        }

        if let Some(new_type_id) =
            replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, fn_ret_type_id, &gen_impls)?
        {
            fn_ret_type_id = new_type_id;
        }

        Ok(fn_ret_type_id)
    } else {
        ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Void,
            TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
        ))
    }
}

fn infer_fn_call_method(method_call: &mut FnCall, ctx: &mut TraverseCtx) -> LangResult<TypeId> {
    // Get the "owning" ADT type of this method. If it isn't set explicitly,
    // it should be set as a expression in the first argument with name "this".
    let mut adt_type_id = if let Some(adt_type_id) = &method_call.method_adt {
        *adt_type_id
    } else if let Some(first_arg) = method_call.arguments.first() {
        if first_arg.name.as_ref().map_or(false, |name| name == "this") {
            first_arg.value.get_expr_type()?
        } else {
            unreachable!("First arg not this: {:#?}", method_call);
        }
    } else {
        unreachable!("Unable to get ADT: {:#?}", method_call);
    };

    // OBS! At this pointe the ADT type might be unknown.
    let mut adt_ty_clone = ctx.ty_env.lock().ty_clone(adt_type_id)?;

    // The `this` for the method might be wrapped in a pointer `fn {this}`.
    // Fetch and use the nested ADT in this case.
    if let Ty::Pointer(nested_adt_type_id, ..) = &adt_ty_clone {
        adt_type_id = *nested_adt_type_id;
        adt_ty_clone = ctx.ty_env.lock().ty_clone(adt_type_id)?;
    }

    // Update the fn_call generics. This will assign the names declared on the
    // ADT generics with the types specified in the fn_call generics. If no types
    // is specified in the fn_call generics, new GenericInstances will be created.
    if let Ty::CompoundType(inner_ty, ..) = &mut adt_ty_clone {
        if let Some(adt_path) = inner_ty.get_ident_mut() {
            let method = ctx.ast_ctx.get_method(
                &ctx.ty_env.lock(),
                &adt_path.without_gens(),
                &method_call.name,
            )?;

            let method_gen_names = if let Some(gens) = &method.read().generics {
                gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                Vec::with_capacity(0)
            };

            let method_path = method_call.module.clone_push(
                &method_call.name,
                method_call.generics.as_ref(),
                method_call.file_pos,
            );

            if let Some(new_fn_gens) = combine_generics(
                ctx,
                method_gen_names.as_ref(),
                method_call.generics.as_ref(),
                &method_path,
                Some(adt_path),
            )? {
                method_call.generics = Some(new_fn_gens);
            }

            // For the ADT, create a new `Generic`s where the types of all
            // generics are set to `GenericInstance`s. This is needed so that the
            // methods with ADTs with different generic impls doesn't doesn't
            // get inferred together.
            // The generic impls might also have been hard-coded in the code, in
            // that case the old hard-coded types are used.
            let adt_gens = adt_path.gens();
            if let Some(new_adt_gens) = combine_generics_adt(ctx, adt_path, adt_gens, &method_path)?
            {
                adt_path.last_mut().unwrap().1 = Some(new_adt_gens);

                // Create the new ADT and use it as the "actual" ADT in the
                // rest of this function.
                let mut ty_env_guard = ctx.ty_env.lock();
                let new_adt_type_id = ty_env_guard.id(&adt_ty_clone)?;
                ty_env_guard.forward(adt_type_id, new_adt_type_id)?;

                adt_type_id = new_adt_type_id;
            }
        }
    }

    method_call.method_adt = Some(adt_type_id);

    let method_name_with_gens = LangPath::new(
        vec![LangPathPart(
            method_call.name.clone(),
            method_call.generics.clone(),
        )],
        Some(method_call.file_pos.unwrap()),
    );

    let mut ty_env_guard = ctx.ty_env.lock();

    // Insert constraints between the function call argument type and
    // the method parameter types that will be figured out later.
    for (idx, arg) in method_call.arguments.iter().enumerate() {
        // If the argument is a named argument, give the argument name
        // to the new "UnknownMethodArgument" to try and figure out the
        // position of the argument through it. Otherwise use the index.
        let position = if let Some(arg_name) = &arg.name {
            Either::Left(arg_name.into())
        } else {
            Either::Right(idx)
        };

        let unique_id = ty_env_guard.new_unique_id();
        let arg_type_id = ty_env_guard.id(&Ty::UnknownFnArgument(
            adt_type_id,
            method_name_with_gens.clone(),
            position,
            unique_id,
            TypeInfo::DefaultOpt(arg.value.file_pos().cloned()),
        ))?;

        let arg_expr_type_id = arg.value.get_expr_type()?;

        // TODO: Need to be able to solve this in some way. Currently it
        //       sometimes causes a infinite loop because of nested types.
        //       This needs to be solved some way in the future because
        //       some param/args can't be type inferred/checked atm.
        // Don't add a constraint if the argument has the same type as
        // the ADT.
        if arg_expr_type_id != adt_type_id {
            insert_constraint(&mut ty_env_guard, arg_type_id, arg_expr_type_id)?;
        }
    }

    let method_arg_tys = {
        let mut method_arg_tys = Vec::default();
        for arg in &method_call.arguments {
            let arg_type_id = arg.value.get_expr_type()?;
            method_arg_tys.push(arg_type_id);
        }
        method_arg_tys
    };

    // The expected return type of the method call.
    let unique_id = ty_env_guard.new_unique_id();
    ty_env_guard.id(&Ty::UnknownAdtMethod(
        adt_type_id,
        method_name_with_gens,
        method_arg_tys,
        unique_id,
        TypeInfo::FuncCall(method_call.file_pos.unwrap()),
    ))
}

fn infer_fn_call_fn(fn_call: &mut FnCall, ctx: &mut TraverseCtx) -> LangResult<TypeId> {
    let partial_path = fn_call
        .module
        .clone_push(&fn_call.name, None, fn_call.file_pos);
    let fn_full_path =
        ctx.ast_ctx
            .calculate_fn_full_path(&ctx.ty_env.lock(), &partial_path, ctx.block_id)?;

    let func = ctx.ast_ctx.get_fn(&ctx.ty_env.lock(), &fn_full_path)?;
    let func = func.read();

    let new_gens = if let Some(gens_decl) = &func.generics {
        let mut gens = Generics::new();

        if let Some(gens_impl) = &fn_call.generics {
            if gens_decl.len() != gens_impl.len() {
                return Err(ctx.ast_ctx.err(format!(
                    "Wrong amount of generics at func call. Func decl: {:#?}, func call: {:#?}",
                    func, fn_call
                )));
            }

            for (name, gen_ty) in gens_decl.iter_names().zip(gens_impl.iter_types()) {
                gens.insert(name.clone(), *gen_ty);
            }
        } else {
            for gen_name in gens_decl.iter_names() {
                let unique_id = ctx.ty_env.lock().new_unique_id();
                let gen_type_id = ctx.ty_env.lock().id(&Ty::GenericInstance(
                    gen_name.clone(),
                    unique_id,
                    TypeInfo::None,
                ))?;

                gens.insert(gen_name.clone(), gen_type_id);
            }
        }

        fn_call.generics = Some(gens.clone());
        Some(gens)
    } else {
        None
    };

    // Iterate through all arguments of the function call and match up their
    // types with the parameters of the function.
    // The amount of args/params will already have been checked before,
    // just make sure that this doesn't break for vararg functions.
    if let Some(params) = &func.parameters {
        let mut ty_env_guard = ctx.ty_env.lock();

        for (idx, arg) in fn_call.arguments.iter().enumerate() {
            // If the argument is a named argument, get the index for the
            // named parameter instead of using the index of its position
            // in the function call.
            let actual_idx = if let Some(arg_name) = &arg.name {
                ctx.ast_ctx
                    .get_fn_param_idx(&ty_env_guard, &fn_full_path, arg_name)?
            } else {
                idx
            };

            if func.is_var_arg && actual_idx >= params.len() {
                continue;
            }

            let arg_type_id = arg.value.get_expr_type()?;

            let param_type_id = if let Some(type_id) = &params
                .get(actual_idx as usize)
                .map(|param| param.read().ty)
                .flatten()
            {
                let mut new_param_type_id = *type_id;
                if let Some(new_gens) = &new_gens {
                    if let Some(new_type_id) = replace_gen_impls(
                        &mut ty_env_guard,
                        ctx.ast_ctx,
                        new_param_type_id,
                        new_gens,
                    )? {
                        new_param_type_id = new_type_id
                    }
                }
                new_param_type_id
            } else {
                return Err(ctx.ast_ctx.err(format!(
                    "Type for parameter \"{:?}\" with index {} in function \"{}\" set to None.",
                    arg.name, actual_idx, func.name
                )));
            };

            insert_constraint(&mut ty_env_guard, arg_type_id, param_type_id)?;
        }
    }

    if let Some(ret_type_id) = func.ret_type {
        // Replace any `Generic`s in the returned type with the impl types in
        // the `new_gens` (either real type or `GenericInstance`).
        if let Some(new_gens) = &new_gens {
            if let Some(new_ret_type_id) =
                replace_gen_impls(&mut ctx.ty_env.lock(), ctx.ast_ctx, ret_type_id, new_gens)?
            {
                return Ok(new_ret_type_id);
            }
        }
        Ok(ret_type_id)
    } else {
        ctx.ty_env.lock().id(&Ty::CompoundType(
            InnerTy::Void,
            TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
        ))
    }
}
