use common::{
    error::LangResult,
    token::{
        expr::Expr,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    traverse::traverse_ctx::TraverseCtx,
    ty::{inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
};

use crate::ty::solve::insert_constraint;

pub(crate) fn infer_bin_op(bin_op: &mut BinOp, ctx: &mut TraverseCtx) -> LangResult<()> {
    let mut ty_env_guard = ctx.ty_env.lock();

    // The lhs and rhs exprs will already have been traversed and should
    // have been given a "unknown" type if they didn't have a type already.
    // The "ret_type" of this bin op will also be given a ret_type if it
    // doesn't already have a type set.
    let ret_type_id = if let Some(type_id) = &bin_op.ret_type {
        *type_id
    } else {
        let unique_id = ty_env_guard.new_unique_id();
        let new_type_id = ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Unknown(unique_id),
            TypeInfo::DefaultOpt(bin_op.file_pos.to_owned()),
        ))?;

        bin_op.ret_type = Some(new_type_id);
        new_type_id
    };

    let lhs_type_id = bin_op.lhs.get_expr_type()?;
    let rhs_type_id = bin_op.rhs.get_expr_type()?;

    let bool_type_id = ty_env_guard.id(&Ty::CompoundType(
        InnerTy::Boolean,
        TypeInfo::DefaultOpt(bin_op.file_pos.to_owned()),
    ))?;

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
                ty_env_guard.remove(ret_type_id);
                bin_op.ret_type = Some(*rhs_ty);
            } else {
                return Err(ctx
                    .ast_ctx
                    .err(format!("Rhs of \"as\" not a valid type: {:?}", bin_op.rhs)));
            }
        }

        BinOperator::Dot => {
            insert_constraint(&mut ty_env_guard, ret_type_id, rhs_type_id)?;
        }

        // TODO: What ret type should they have?
        BinOperator::Range | BinOperator::RangeInclusive => {
            insert_constraint(&mut ty_env_guard, lhs_type_id, rhs_type_id)?;
        }

        BinOperator::Eq
        | BinOperator::Neq
        | BinOperator::Lt
        | BinOperator::Gt
        | BinOperator::Lte
        | BinOperator::Gte => {
            insert_constraint(&mut ty_env_guard, ret_type_id, bool_type_id)?;
            insert_constraint(&mut ty_env_guard, lhs_type_id, rhs_type_id)?;
        }

        BinOperator::BoolAnd | BinOperator::BoolOr => {
            insert_constraint(&mut ty_env_guard, ret_type_id, bool_type_id)?;
            insert_constraint(&mut ty_env_guard, lhs_type_id, bool_type_id)?;
            insert_constraint(&mut ty_env_guard, rhs_type_id, bool_type_id)?;
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
            insert_constraint(&mut ty_env_guard, ret_type_id, lhs_type_id)?;
            insert_constraint(&mut ty_env_guard, ret_type_id, rhs_type_id)?;
            insert_constraint(&mut ty_env_guard, lhs_type_id, rhs_type_id)?;
        }
    }

    Ok(())
}

pub(crate) fn infer_un_op(un_op: &mut UnOp, ctx: &mut TraverseCtx) -> LangResult<()> {
    let mut ty_env_guard = ctx.ty_env.lock();

    // The expr value of this un op will already have been traversed and should
    // have been given a "unknown" type if it didn't have one type already.
    // The "ret_type" of this un op will also be given a ret_type if it
    // doesn't already have a type set.
    let ret_type_id = if let Some(type_id) = &un_op.ret_type {
        *type_id
    } else {
        let unique_id = ty_env_guard.new_unique_id();
        let new_type_id = ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Unknown(unique_id),
            TypeInfo::Default(un_op.file_pos.unwrap()),
        ))?;

        un_op.ret_type = Some(new_type_id);
        new_type_id
    };

    let val_type_id = un_op.value.get_expr_type()?;

    let type_info = TypeInfo::Default(un_op.file_pos.unwrap());

    match &mut un_op.operator {
        UnOperator::Positive
        | UnOperator::Negative
        | UnOperator::Increment
        | UnOperator::Decrement
        | UnOperator::BitComplement
        | UnOperator::BoolNot => {
            insert_constraint(&mut ty_env_guard, ret_type_id, val_type_id)?;
        }
        UnOperator::Deref => {
            let ptr_type_id = ty_env_guard.id(&Ty::Pointer(ret_type_id, type_info))?;
            insert_constraint(&mut ty_env_guard, ptr_type_id, val_type_id)?;
        }
        UnOperator::Address => {
            let ptr_type_id = ty_env_guard.id(&Ty::Pointer(val_type_id, type_info))?;
            insert_constraint(&mut ty_env_guard, ret_type_id, ptr_type_id)?;
        }
        UnOperator::ArrayAccess(_) => {
            let unique_id = ty_env_guard.new_unique_id();
            let unknown_type_id =
                ty_env_guard.id(&Ty::UnknownArrayMember(val_type_id, unique_id, type_info))?;
            insert_constraint(&mut ty_env_guard, ret_type_id, unknown_type_id)?;
        }
        UnOperator::UnionIs(member_name, var_decl) => {
            let var_decl_type_id = if let Stmt::VariableDecl(var, ..) = var_decl.as_ref() {
                *var.read().ty.as_ref().unwrap()
            } else {
                return Err(ctx
                    .ast_ctx
                    .err(format!("lhs of \"UnionIs\" not a var decl: {:#?}", un_op)));
            };

            // TODO: Do in cleaner way.
            // The `val_ty` will be the return type of the union member,
            // so need to get the type for the "inner_un_op" which should be
            // a ADT access where the `value` will reference the ADT that is
            // being accessed. That will be inferred to the type of the union.
            let union_ty = if let Expr::Op(Op::UnOp(inner_un_op)) = un_op.value.as_ref() {
                inner_un_op.value.get_expr_type()?
            } else {
                unreachable!("un_op.value not un op (i.e. union access): {:#?}", un_op);
            };

            // Link the type of the new var decl to the type of the member.
            let unique_id = ty_env_guard.new_unique_id();
            let unknown_type_id = ty_env_guard.id(&Ty::UnknownAdtMember(
                union_ty,
                member_name.clone(),
                unique_id,
                type_info.clone(),
            ))?;

            let bool_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::Boolean, type_info))?;

            insert_constraint(&mut ty_env_guard, var_decl_type_id, unknown_type_id)?;
            insert_constraint(&mut ty_env_guard, ret_type_id, bool_type_id)?;
        }
        UnOperator::AdtAccess(member_name, ..) | UnOperator::EnumAccess(member_name, ..) => {
            let unique_id = ty_env_guard.new_unique_id();
            let unknown_type_id = ty_env_guard.id(&Ty::UnknownAdtMember(
                val_type_id,
                member_name.clone(),
                unique_id,
                type_info,
            ))?;
            insert_constraint(&mut ty_env_guard, ret_type_id, unknown_type_id)?;
        }
    }

    Ok(())
}
