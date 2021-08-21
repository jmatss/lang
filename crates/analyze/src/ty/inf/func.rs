use std::sync::{Arc, RwLock};

use common::{
    error::LangResult,
    token::{block::Fn, expr::FnPtr, stmt::Modifier},
    traverse::traverse_ctx::TraverseCtx,
    ty::{
        generics::Generics, replace::replace_gen_impls, to_string::to_string_path, ty::Ty,
        type_info::TypeInfo,
    },
};

pub(crate) fn infer_fn(func: &Arc<RwLock<Fn>>, ctx: &mut TraverseCtx) -> LangResult<()> {
    let func = func.write().unwrap();

    // If this is a method and the first argument is named "this", set
    // the type of it to the ADT that this method belongs to (which already
    // is stored in `method_adt`).
    if let Some(first_arg) = func.parameters.as_ref().and_then(|args| args.first()) {
        let mut first_arg = first_arg.write().unwrap();

        if &first_arg.name == "this" {
            if let Some(adt_type_id) = &func.method_adt {
                let type_id = if func.modifiers.contains(&Modifier::This) {
                    *adt_type_id
                } else if func.modifiers.contains(&Modifier::ThisPointer) {
                    // TODO: What file_pos should this pointer have?
                    ctx.ty_env
                        .lock()
                        .unwrap()
                        .id(&Ty::Pointer(*adt_type_id, TypeInfo::None))?
                } else {
                    // Keywords can't be used as parameter name.
                    unreachable!("{:#?}", func);
                };

                first_arg.ty = Some(type_id);
            }
        }
    }

    Ok(())
}

pub(crate) fn infer_fn_ptr(fn_ptr: &mut FnPtr, ctx: &mut TraverseCtx) -> LangResult<()> {
    let fn_ptr_gens = if let Some(gens) = &fn_ptr.generics {
        gens.clone()
    } else {
        Generics::empty()
    };

    let mut ty_env_guard = ctx.ty_env.lock().unwrap();

    let partial_path =
        fn_ptr
            .module
            .clone_push(&fn_ptr.name, fn_ptr.generics.as_ref(), fn_ptr.file_pos);
    let full_path =
        ctx.ast_ctx
            .calculate_fn_full_path(&ty_env_guard, &partial_path, ctx.block_id)?;

    let func = ctx.ast_ctx.get_fn(&ty_env_guard, &full_path)?;
    let func = func.read().unwrap();

    let fn_gen_names = if let Some(gens) = &func.generics {
        gens.iter_names().cloned().collect::<Vec<_>>()
    } else {
        Vec::with_capacity(0)
    };

    let mut fn_param_tys = if let Some(params) = &func.parameters {
        params
            .iter()
            .map(|var| var.read().unwrap().ty.as_ref().unwrap().to_owned())
            .collect::<Vec<_>>()
    } else {
        Vec::with_capacity(0)
    };

    let mut fn_ret_ty = func.ret_type;

    if fn_ptr_gens.len() != fn_gen_names.len() {
        return Err(ctx.ast_ctx.err(format!(
            "Function pointer to \"{}\" has incorrect amount of generics. Expected: {}, got: {}",
            to_string_path(&ty_env_guard, &full_path),
            fn_gen_names.len(),
            fn_ptr_gens.len()
        )));
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
        if let Some(new_type_id) = replace_gen_impls(
            &mut ty_env_guard,
            &ctx.ast_ctx,
            *fn_param_type_id,
            &gens_impl,
        )? {
            *fn_param_type_id = new_type_id
        }
    }

    if let Some(ret_type_id) = &mut fn_ret_ty {
        if let Some(new_type_id) =
            replace_gen_impls(&mut ty_env_guard, &ctx.ast_ctx, *ret_type_id, &gens_impl)?
        {
            *ret_type_id = new_type_id
        }
    }

    let new_fn_type_id = ty_env_guard.id(&Ty::Fn(
        fn_ptr_gens.iter_types().cloned().collect::<Vec<_>>(),
        fn_param_tys,
        fn_ret_ty,
        TypeInfo::DefaultOpt(fn_ptr.file_pos),
    ))?;

    if let Some(fn_type_id) = &fn_ptr.fn_ty {
        if fn_type_id != &new_fn_type_id {
            return Err(ctx.ast_ctx.err(format!(
                "Bad function signature for function pointer, fn_path: {}. \
                fn_ty: {:#?}, new_fn_ty: {:#?}. Function pointer pos: {:#?}",
                to_string_path(&ty_env_guard, &full_path),
                fn_type_id,
                new_fn_type_id,
                fn_ptr.file_pos
            )));
        }
    } else {
        fn_ptr.fn_ty = Some(new_fn_type_id);
    }

    Ok(())
}
