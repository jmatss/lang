use common::{
    error::LangResult,
    path::LangPath,
    traverse::traverse_ctx::TraverseCtx,
    ty::{generics::Generics, to_string::to_string_path, ty::Ty, type_info::TypeInfo},
};

// TODO: Clean up. Either remove this function or try to combine in some way
//       with `set_generic_names`. Makes no sense being able to call this
//       function when `adt_path` is None.
/// Given a ADT with partial path `adt_path`, combines the names of the ADT generic
/// declarations with the types from the ADT instance generics (`gen_impls`).
///
/// If the given ADT doesn't have any generics, this function will return None.
/// If the given `gen_impls` is None or empty but the ADT have declared generics,
/// new "GenericInstance" types will be created and used as the types.
///
/// The given `fn_call_path` is just for error printing, it will never be used
/// in a "normal flow" of this function.
pub fn combine_generics(
    ctx: &mut TraverseCtx,
    adt_path_opt: Option<&LangPath>,
    gen_impls: Option<&Generics>,
    fn_call_path: &LangPath,
) -> LangResult<Option<Generics>> {
    let mut new_gens = Generics::new();

    let adt = if let Some(adt_path) = adt_path_opt {
        ctx.ast_ctx.get_adt_partial(
            &ctx.ty_env.lock().unwrap(),
            &adt_path.without_gens(),
            ctx.block_id,
        )?
    } else {
        return Ok(None);
    };

    let adt_gen_names = if let Some(adt_gens) = &adt.read().unwrap().generics {
        adt_gens.iter_names().cloned().collect::<Vec<_>>()
    } else {
        return Ok(None);
    };

    // If the generics impls have been specified, use those to populate the
    // Generics inside `new_gens`.
    // Else if no generic implements have been specified, create new
    // "GenericInstance" types that will be used to create the generics.
    if let Some(gen_impls) = gen_impls {
        if adt_gen_names.len() != gen_impls.len_types() {
            return Err(ctx.ast_ctx.err(format!(
                "Wrong amount of generics on ADT for static call. Found: {}, expected: {}.\n\
                ADT path: {:?}\nMethod name: {}\nAdt generic names: {:?}",
                gen_impls.len_types(),
                adt_gen_names.len(),
                adt_path_opt,
                to_string_path(&ctx.ty_env.lock().unwrap(), &fn_call_path),
                adt_gen_names
            )));
        }

        for (gen_name, gen_type_id) in adt_gen_names.iter().zip(gen_impls.iter_types()) {
            new_gens.insert(gen_name.into(), *gen_type_id);
        }
    } else {
        for gen_name in adt_gen_names {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let gen_type_id = ctx.ty_env.lock().unwrap().id(&Ty::GenericInstance(
                gen_name.clone(),
                unique_id,
                TypeInfo::None,
            ))?;

            new_gens.insert(gen_name.clone(), gen_type_id);
        }
    }

    Ok(Some(new_gens))
}
