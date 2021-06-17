use std::borrow::Borrow;

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::LangResult,
    path::LangPath,
    ty::{generics::Generics, to_string::to_string_path, ty::Ty, type_info::TypeInfo},
};

/// Combines the generics from a "instance" with the generics from a ADT declaration.
/// Given a ADT with partial path `adt_path`, combines the nams of the ADT generic
/// declarations with the types from the ADT instance generics.
/// If the given `gen_impls` is empty, new "GenericInstance"s will be created
/// and used instead.
pub fn combine_generics(
    ctx: &mut TraverseCtx,
    adt_path_opt: Option<&LangPath>,
    gen_impls: &Generics,
    fn_call_path: &LangPath,
) -> LangResult<Generics> {
    let mut new_gens = Generics::new();

    let adt_path = if let Some(adt_path) = adt_path_opt {
        adt_path
    } else {
        return Ok(new_gens);
    };

    let adt_gen_names = if let Ok(adt) = ctx.ast_ctx.get_adt_partial(
        &ctx.ty_env.lock().unwrap(),
        &adt_path.without_gens(),
        ctx.block_id,
    ) {
        if let Some(adt_gens) = &adt.as_ref().borrow().read().unwrap().generics {
            adt_gens.iter_names().cloned().collect::<Vec<_>>()
        } else {
            Vec::default()
        }
    } else {
        Vec::default()
    };
    // If no generic implements have been specified, create new "GenericInstance"s.
    // Else if the generics impls have been specified, use those to populate
    // the Generics.
    if gen_impls.is_empty() {
        for gen_name in adt_gen_names {
            let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
            let gen_type_id = ctx.ty_env.lock().unwrap().id(&Ty::GenericInstance(
                gen_name.clone(),
                unique_id,
                TypeInfo::None,
            ))?;

            new_gens.insert(gen_name.clone(), gen_type_id);
        }
    } else {
        if adt_gen_names.len() != gen_impls.len_types() {
            return Err(ctx.ast_ctx.err(format!(
                "Wrong amount of generics on ADT for static call. Found: {}, expected: {}.\n\
                Adt path: {:?}\nMethod name: {}\nAdt generic names: {:?}",
                gen_impls.len_types(),
                adt_gen_names.len(),
                adt_path,
                to_string_path(&ctx.ty_env.lock().unwrap(), &fn_call_path),
                adt_gen_names
            )));
        }

        adt_gen_names
            .iter()
            .cloned()
            .zip(gen_impls.iter_types().cloned())
            .for_each(|(gen_name, gen_ty)| new_gens.insert(gen_name, gen_ty));
    }

    Ok(new_gens)
}
