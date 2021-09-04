use common::{
    error::LangResult,
    token::expr::BuiltInCall,
    traverse::traverse_ctx::TraverseCtx,
    ty::{replace::replace_unique_ids, ty::Ty, type_info::TypeInfo},
};

use crate::ty::solve::insert_constraint;

pub(crate) fn infer_built_in(
    built_in_call: &mut BuiltInCall,
    ctx: &mut TraverseCtx,
) -> LangResult<()> {
    let built_in = ctx.ast_ctx.get_built_in(&built_in_call.name)?;

    if built_in.is_var_arg && built_in_call.arguments.len() < built_in.parameters.len() {
        return Err(ctx.ast_ctx.err(format!(
            "Incorrect amount of arguments given for built-in call to \"{}\". \
            Expected vararg parameter count greater than or equal to: {}, got: {}",
            &built_in.name,
            built_in.parameters.len(),
            built_in_call.arguments.len()
        )));
    } else if !built_in.is_var_arg && built_in_call.arguments.len() != built_in.parameters.len() {
        return Err(ctx.ast_ctx.err(format!(
            "Incorrect amount of arguments given for built-in call to \"{}\". \
            Expected parameter count of: {}, got: {}",
            &built_in.name,
            built_in.parameters.len(),
            built_in_call.arguments.len()
        )));
    }

    let mut ty_env_guard = ctx.ty_env.lock();

    for (built_in_param, built_in_call_arg) in
        built_in.parameters.iter().zip(&built_in_call.arguments)
    {
        let built_in_type_id = built_in_param.ty.unwrap();
        let built_in_call_type_id = built_in_call_arg.value.get_expr_type()?;

        // Need to replace any unique IDs found in the built-in declaration
        // types so that they actual are unique. Otherwise all uses of the
        // same function types would have the same unique ID which would lead
        // to that all types would map to eachother.
        let built_in_type_id = match replace_unique_ids(&mut ty_env_guard, built_in_type_id)? {
            Some(new_type_id) => new_type_id,
            None => built_in_type_id,
        };
        insert_constraint(&mut ty_env_guard, built_in_type_id, built_in_call_type_id)?;
    }

    // Make sure that the amount of generic arguments are equals to the
    // amount of generic parameters. Also add constraints between them.
    if !(built_in.generics.is_none() && built_in_call.generics.is_none()) {
        let built_in_gens = if let Some(built_in_gens) = &built_in.generics {
            built_in_gens
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Built-in function doesn't have generics, but call has generics. \
                Built-in: {:#?}, call: {:#?}",
                &built_in, &built_in_call
            )));
        };

        let built_in_call_gens = if let Some(built_in_call_gens) = &built_in_call.generics {
            built_in_call_gens
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Built-in function have generic parameters, but call doesn't. \
                Built-in: {:#?}, call: {:#?}",
                &built_in, &built_in_call
            )));
        };

        if built_in_gens.len() != built_in_call_gens.len() {
            return Err(ctx.ast_ctx.err(format!(
                "Incorrect amount of generic arguments given for built-in call to \"{}\". \
                Expected amount: {}, got: {}",
                &built_in.name,
                built_in_gens.len(),
                built_in_call_gens.len()
            )));
        }

        for (built_in_gen, built_in_call_gen) in
            built_in_gens.iter().zip(built_in_call_gens.iter_types())
        {
            // Need to replace any unique IDs found in the built-in declaration
            // types so that they actual are unique. Otherwise all uses of the
            // same function types would have the same unique ID which would lead
            // to that all types would map to eachother.
            let built_in_gen = match replace_unique_ids(&mut ty_env_guard, *built_in_gen)? {
                Some(new_type_id) => new_type_id,
                None => *built_in_gen,
            };
            insert_constraint(&mut ty_env_guard, built_in_gen, *built_in_call_gen)?;
        }
    }

    let built_in_ret_type = match replace_unique_ids(&mut ty_env_guard, built_in.ret_type)? {
        Some(new_type_id) => new_type_id,
        None => built_in.ret_type,
    };
    if let Some(built_in_call_ret_type) = built_in_call.ret_type {
        insert_constraint(&mut ty_env_guard, built_in_ret_type, built_in_call_ret_type)?;
    } else {
        built_in_call.ret_type = Some(built_in_ret_type);
    }

    if &built_in_call.name == "type" {
        // TODO: Temporary ugly hack to make `@type` work. Should do this in
        //       a different way and somewhere else.
        let type_id = ty_env_guard.id(&Ty::Expr(
            Box::new(built_in_call.arguments.first().unwrap().value.clone()),
            TypeInfo::BuiltInCall(built_in_call.file_pos),
        ))?;

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
        insert_constraint(&mut ty_env_guard, arg_type, ret_type)?;
    } else if &built_in_call.name == "array" {
        // TODO: The return type of the @array call should return a type
        //       tied to its arguments. Should be done somewhere else in
        //       the future.
        let init_val = built_in_call.arguments.get(0).unwrap().value.clone();
        let init_val_type_id = init_val.get_expr_type()?;
        let arr_dim = built_in_call.arguments.get(1).unwrap().value.clone();

        let arr_type_id = ty_env_guard.id(&Ty::Array(
            init_val_type_id,
            Some(Box::new(arr_dim)),
            TypeInfo::BuiltInCall(built_in_call.file_pos),
        ))?;

        built_in_call.ret_type = Some(arr_type_id);
    }

    Ok(())
}
