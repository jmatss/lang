use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        block::{Adt, AdtKind},
        expr::AdtInit,
    },
    traverse::traverse_ctx::TraverseCtx,
    ty::{
        generics::Generics, get::get_file_pos, inner_ty::InnerTy, replace::replace_gen_impls,
        ty::Ty, type_info::TypeInfo,
    },
};

use crate::ty::solve::insert_constraint;

/// Creates type constraints for the arguments specified in a ADT init to the
/// types in the specific ADT declaration.
pub(crate) fn infer_adt_init(adt_init: &mut AdtInit, ctx: &mut TraverseCtx) -> LangResult<()> {
    let partial_path = adt_init
        .module
        .clone_push(&adt_init.name, None, adt_init.file_pos);

    let full_path_without_gens = ctx.ast_ctx.calculate_adt_full_path(
        &ctx.ty_env.lock().unwrap(),
        &partial_path,
        ctx.block_id,
    )?;

    let adt = ctx
        .ast_ctx
        .get_adt(&ctx.ty_env.lock().unwrap(), &full_path_without_gens)?;
    let adt = adt.read().unwrap();

    adt_init.kind = adt.kind.clone();

    // Gets a map if the generics that maps the ident of the generic
    // (ex. "T", "U" etc.) to a new unknown generic type. This is needed
    // to ensure that two members of a ADT with the same ident uses the same
    // unknown generic type. It is also needed to ensure that different ADTs
    // uses different types for the generics.
    let gens = if let Some(gens_decl) = &adt.generics {
        let mut gens = Generics::new();

        // If the ADT init call has specified explicitly the implementation
        // types for the generics, use those instead of unknown generics.
        // Currently these explicit types must be solved types.
        if let Some(gens_impl) = &adt_init.generics {
            if gens_decl.len() != gens_impl.len() {
                return Err(ctx.ast_ctx.err(format!(
                    "Wrong amount of generics for ADT init. ADT init: {:#?}, ADT: {:#?}",
                    adt_init, adt
                )));
            }

            for (name, gen_ty) in gens_decl.iter_names().zip(gens_impl.iter_types()) {
                gens.insert(name.clone(), *gen_ty);
            }
        } else {
            for gen_name in gens_decl.iter_names() {
                let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                let gen_type_id = ctx.ty_env.lock().unwrap().id(&Ty::GenericInstance(
                    gen_name.clone(),
                    unique_id,
                    TypeInfo::None,
                ))?;

                gens.insert(gen_name.clone(), gen_type_id);
            }
        }

        adt_init.generics = Some(gens.clone());
        Some(gens)
    } else {
        None
    };

    let full_path_with_gens = full_path_without_gens.with_gens_opt(gens.clone());

    let adt_ty = adt_init
        .adt_type_id
        .map(|id| ctx.ty_env.lock().unwrap().ty_clone(id));
    match adt_ty {
        Some(Ok(Ty::CompoundType(..))) => {
            // If the type already is set to a compound, use that
            // already set type.
        }
        Some(Err(err)) => return Err(err),
        _ => {
            let ret_type_id = ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                InnerTy::UnknownIdent(full_path_with_gens, ctx.block_id),
                TypeInfo::Default(adt_init.file_pos.unwrap()),
            ))?;
            adt_init.adt_type_id = Some(ret_type_id);
        }
    };

    match adt.kind {
        AdtKind::Struct => infer_adt_init_struct(ctx, adt_init, &adt, gens),
        AdtKind::Union => infer_adt_init_union(ctx, adt_init, &adt, gens),
        _ => unreachable!("ADT init kind: {:?}", adt.kind),
    }
}

/// `gens` is a possible combination of the generic names from the `adt`
/// declaration with the possible types from the `adt_init`.
fn infer_adt_init_struct(
    ctx: &mut TraverseCtx,
    adt_init: &mut AdtInit,
    adt: &Adt,
    gens: Option<Generics>,
) -> LangResult<()> {
    if adt.members.len() != adt_init.arguments.len() {
        return Err(LangError::new(
            format!(
                "ADT \"{}\" and ADT init has diff amount of members. ADT#: {:?}, init#: {:?}.",
                &adt.name,
                adt.members.len(),
                adt_init.arguments.len()
            ),
            LangErrorKind::AnalyzeError,
            adt_init.file_pos,
        ));
    }

    // TODO: Does the generics need to be set? Shouldn't be necessary since the
    //       ADT in the look-up shouldn't contain any generics.
    let adt_path_without_gens = adt.module.clone_push(&adt.name, None, Some(adt.file_pos));

    // TODO: Verify that all members are initialized.
    let mut ty_env_guard = ctx.ty_env.lock().unwrap();

    for (i, arg) in adt_init.arguments.iter().enumerate() {
        // If a name is set, this is a named member init. Don't use the
        // iterator index, get the correct index of the struct field with
        // the name `arg.name`.
        let index: usize = if let Some(arg_name) = &arg.name {
            ctx.ast_ctx
                .get_adt_member_index(&ty_env_guard, &adt_path_without_gens, arg_name)?
                as usize
        } else {
            i
        };

        // TODO: Make sure that the ADT init argument is compatible
        //       with the member ADT type. Currently this doesn't
        //       get caught until the codegen stage.

        // Add constraints mapping the type of the ADT init argument
        // to the corresponding actual ADT member type.
        if let Some(member) = adt.members.get(index) {
            let member = member.read().unwrap();

            // Get the "actual" type of the member. If it contains a generic,
            // it needs to get the actual unknown generic type from the `gens`.
            // Otherwise reuse the already set type.
            let member_type_id = if let Some(member_type_id) = member.ty {
                let mut new_member_type_id = member_type_id;
                if let Some(gens) = &gens {
                    if let Some(new_type_id) = replace_gen_impls(
                        &mut ty_env_guard,
                        &ctx.ast_ctx,
                        new_member_type_id,
                        gens,
                    )? {
                        new_member_type_id = new_type_id
                    }
                }
                new_member_type_id
            } else {
                return Err(ctx.ast_ctx.err(format!(
                    "Member \"{:?}\" in struct \"{:?}\" doesn't have a type set.",
                    adt.members.get(index),
                    &adt.name
                )));
            };

            let arg_type_id = arg.value.get_expr_type()?;
            insert_constraint(&mut ty_env_guard, arg_type_id, member_type_id)?;

            // Bind type of member to the struct. This is required to being able
            // to solve the generic types in the `adt_type_id`.
            let unique_id = ty_env_guard.new_unique_id();
            let type_info = TypeInfo::DefaultOpt(get_file_pos(&ty_env_guard, arg_type_id).cloned());
            let unknown_type_id = ty_env_guard.id(&Ty::UnknownAdtMember(
                adt_init.adt_type_id.unwrap(),
                member.name.clone(),
                unique_id,
                type_info,
            ))?;

            insert_constraint(&mut ty_env_guard, arg_type_id, unknown_type_id)?;
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Unable to get member at index {} in struct \"{:?}\".",
                index, &adt.name
            )));
        }
    }

    Ok(())
}

/// `gens` is a possible combination of the generic names from the `adt`
/// declaration with the possible types from the `adt_init`.
fn infer_adt_init_union(
    ctx: &mut TraverseCtx,
    adt_init: &mut AdtInit,
    adt: &Adt,
    gens: Option<Generics>,
) -> LangResult<()> {
    if adt_init.arguments.len() > 1 {
        return Err(ctx.ast_ctx.err(format!(
            "ADT init of union \"{}\" has more than one argument, has#: {}.",
            &adt.name,
            adt_init.arguments.len()
        )));
    } else if adt_init.arguments.is_empty() {
        return Err(ctx.ast_ctx.err(format!(
            "ADT init of union \"{}\" has no argument, expected one.",
            &adt.name,
        )));
    }

    let arg_init = adt_init.arguments.first().unwrap();

    let member_name = if let Some(member_name) = &arg_init.name {
        member_name
    } else {
        return Err(ctx.ast_ctx.err(format!(
            "ADT init of union \"{}\" doesn't have NAMED argument as expected.",
            &adt.name,
        )));
    };

    let mut ty_env_guard = ctx.ty_env.lock().unwrap();

    // TODO: Does the generics need to be set? Shouldn't be necessary since the
    //       ADT in the look-up shouldn't contain any generics.
    let adt_path_without_gens = adt.module.clone_push(&adt.name, None, Some(adt.file_pos));

    let member = ctx.ast_ctx.get_adt_member(
        &ty_env_guard,
        &adt_path_without_gens,
        member_name,
        adt_init.file_pos,
    )?;

    // Make a copy of the type to allow for multiple
    // struct inits with different types for the generics.
    let new_member = member.read().unwrap().clone();

    let member_type_id = if let Some(type_id) = &new_member.ty {
        if let Some(gens) = &gens {
            match replace_gen_impls(&mut ty_env_guard, &ctx.ast_ctx, *type_id, gens)? {
                Some(new_type_id) => new_type_id,
                None => *type_id,
            }
        } else {
            *type_id
        }
    } else {
        return Err(ctx.ast_ctx.err(format!(
            "Member \"{:?}\" in union \"{:?}\" doesn't have a type set.",
            member.read().unwrap(),
            &adt.name
        )));
    };

    let arg_type_id = arg_init.value.get_expr_type()?;
    insert_constraint(&mut ty_env_guard, arg_type_id, member_type_id)?;

    // Bind type of member arg to the union.
    let unique_id = ty_env_guard.new_unique_id();
    let type_info = TypeInfo::DefaultOpt(get_file_pos(&ty_env_guard, arg_type_id).cloned());
    let unknown_type_id = ty_env_guard.id(&Ty::UnknownAdtMember(
        adt_init.adt_type_id.unwrap(),
        new_member.name,
        unique_id,
        type_info,
    ))?;
    insert_constraint(&mut ty_env_guard, arg_type_id, unknown_type_id)?;

    Ok(())
}
