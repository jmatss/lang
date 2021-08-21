use common::{
    error::LangResult,
    file::FilePosition,
    token::{
        expr::{ArrayInit, Expr},
        lit::Lit,
    },
    traverse::traverse_ctx::TraverseCtx,
    ty::{inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
};

use crate::ty::solve::insert_constraint;

pub(crate) fn infer_array_init(
    array_init: &mut ArrayInit,
    ctx: &mut TraverseCtx,
) -> LangResult<()> {
    let mut ty_env_guard = ctx.ty_env.lock().unwrap();

    let ret_type_id = if let Some(ret_type_id) = &array_init.ret_type {
        *ret_type_id
    } else {
        let unique_id = ty_env_guard.new_unique_id();
        let new_type_id = ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Unknown(unique_id),
            TypeInfo::Default(array_init.file_pos),
        ))?;

        array_init.ret_type = Some(new_type_id);
        new_type_id
    };

    let mut arg_types = Vec::new();
    for arg in &mut array_init.arguments {
        arg_types.push(arg.value.get_expr_type()?);
    }

    // TODO: What should the type of the index for the array size be?
    let arr_idx_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::U32, TypeInfo::None))?;

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

        let arr_type_id = ty_env_guard.id(&Ty::Array(
            left,
            Some(Box::new(dim_expr.clone())),
            TypeInfo::Default(array_init.file_pos),
        ))?;
        insert_constraint(&mut ty_env_guard, ret_type_id, arr_type_id)?;

        for j in i + 1..array_init.arguments.len() {
            let right = arg_types.get(j).cloned().unwrap();
            insert_constraint(&mut ty_env_guard, left, right)?;
        }
    }

    Ok(())
}
