use common::{
    error::LangResult,
    token::expr::{Expr, Var},
    traverse::traverse_ctx::TraverseCtx,
    ty::{inner_ty::InnerTy, solve::insert_constraint, ty::Ty, type_info::TypeInfo},
};

pub(crate) fn infer_var(var: &mut Var, ctx: &mut TraverseCtx) -> LangResult<()> {
    let var_decl = ctx.ast_ctx.get_var(&var.name, ctx.block_id)?;

    let var_decl_type_id = if let Some(type_id) = var_decl.as_ref().read().unwrap().ty {
        type_id
    } else {
        return Err(ctx
            .ast_ctx
            .err(format!("Return type not set for var decl: {:#?}", var_decl)));
    };

    let var_type_id = if let Some(type_id) = &var.ty {
        *type_id
    } else {
        let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
        let new_type_id = ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
            InnerTy::Unknown(unique_id),
            TypeInfo::VarUse(var.file_pos.unwrap()),
        ))?;

        var.ty = Some(new_type_id);
        new_type_id
    };

    insert_constraint(
        &mut ctx.ty_env.lock().unwrap(),
        var_decl_type_id,
        var_type_id,
    )?;

    Ok(())
}

/// The types of the lhs and rhs of a variable declaration with a init value
/// should be of the same type, add as constraints.
pub(crate) fn infer_var_decl(var: &mut Var, ctx: &mut TraverseCtx) -> LangResult<()> {
    let rhs_type_id_opt = if let Some(value) = &var.value {
        Some(value.get_expr_type()?)
    } else {
        None
    };

    // Create a unkown type if a type isn't already set.
    if var.ty.is_none() {
        let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
        let new_type_id = ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
            InnerTy::Unknown(unique_id),
            TypeInfo::VarDecl(var.file_pos.unwrap(), false),
        ))?;

        var.ty = Some(new_type_id);
    }

    // Add constraints only if this var decl has a init value.
    if let Some(rhs_type_id) = rhs_type_id_opt {
        insert_constraint(
            &mut ctx.ty_env.lock().unwrap(),
            var.ty.unwrap(),
            rhs_type_id,
        )?;
    }

    Ok(())
}

/// The types of the lhs and rhs of a assignment should be of the same type.
/// Add it as a constraint.
pub(crate) fn infer_assignment(
    lhs: &mut Expr,
    rhs: &mut Expr,
    ctx: &mut TraverseCtx,
) -> LangResult<()> {
    let lhs_type_id = lhs.get_expr_type()?;
    let rhs_type_id = rhs.get_expr_type()?;
    insert_constraint(&mut ctx.ty_env.lock().unwrap(), lhs_type_id, rhs_type_id)?;
    Ok(())
}
