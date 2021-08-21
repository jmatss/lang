use common::{
    error::LangResult,
    path::LangPath,
    token::{
        expr::Expr,
        lit::{Lit, StringType},
    },
    traverse::traverse_ctx::TraverseCtx,
    ty::{inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
};

/// Assigns a "Unknown" type for every expression that doesn't have a type
/// explicitly set. This new type will then be temporarilty used during this
/// stage and should be converted/subtituted into a "real" type before this
/// analyzing step is done.
pub(crate) fn infer_lit(expr: &mut Expr, ctx: &mut TraverseCtx) -> LangResult<()> {
    let file_pos = expr.file_pos().cloned();

    let (lit, expr_type_id, type_info) = if let Expr::Lit(lit, type_id_opt, file_pos) = expr {
        let type_info = TypeInfo::Lit(file_pos.to_owned());
        (lit, type_id_opt, type_info)
    } else {
        unreachable!()
    };

    if expr_type_id.is_some() {
        return Ok(());
    }

    let mut ty_env_guard = ctx.ty_env.lock().unwrap();

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
                let u8_ty = Ty::CompoundType(InnerTy::U8, type_info.clone());
                let u8_type_id = ty_env_guard.id(&u8_ty)?;

                let ptr_ty = Ty::Pointer(u8_type_id, type_info);
                let ptr_type_id = ty_env_guard.id(&ptr_ty)?;

                *expr_type_id = Some(ptr_type_id);

                return Ok(());
            }
        },

        Lit::Char(_) => InnerTy::Character,
        Lit::Bool(_) => InnerTy::Boolean,
        Lit::Integer(_, radix) => {
            let unique_id = ty_env_guard.new_unique_id();
            InnerTy::UnknownInt(unique_id, *radix)
        }
        Lit::Float(_) => {
            let unique_id = ty_env_guard.new_unique_id();
            InnerTy::UnknownFloat(unique_id)
        }
    };

    let type_id = ty_env_guard.id(&Ty::CompoundType(inner_ty, type_info))?;
    *expr_type_id = Some(type_id);

    Ok(())
}
