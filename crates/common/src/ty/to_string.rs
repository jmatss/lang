use either::Either;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    path::{LangPath, LangPathPart},
    token::{expr::Expr, lit::Lit, op::Op},
    ty::ty::Ty,
    util, TypeId,
};

use super::{generics::Generics, inner_ty::InnerTy, ty_env::TyEnv};

// TODO: This currently doesn't print the inferred type, so might look weird.
pub fn to_string_type_id(ty_env: &TyEnv, type_id: TypeId) -> LangResult<String> {
    let mut result = String::new();

    let inf_type_id = ty_env.inferred_type(type_id)?;
    let inf_ty = ty_env.ty_clone(inf_type_id)?;
    match inf_ty {
        Ty::CompoundType(ref inner_ty, ..) => {
            // The generics are generated from the `inner_ty` LangPath, not
            // from the `Generics` on the type.
            result.push_str(&to_string_inner_ty(ty_env, inner_ty));
        }

        Ty::Pointer(type_id_i, ..) => {
            result.push('{');
            result.push_str(&to_string_type_id(ty_env, type_id_i)?);
            result.push('}');
        }

        Ty::Fn(gen_type_ids, params, ret_type_id_opt, ..) => {
            result.push_str("fn");

            if !gen_type_ids.is_empty() {
                result.push('<');
                result.push_str(&to_string_list(ty_env, &gen_type_ids)?);
                result.push('>');
            }

            result.push('(');
            if !params.is_empty() {
                result.push_str(&to_string_list(ty_env, &params)?);
            }
            result.push(')');

            if let Some(ret_type_id) = ret_type_id_opt {
                result.push_str("->");
                result.push_str(&to_string_type_id(ty_env, ret_type_id)?);
            }
        }

        Ty::Expr(expr, ..) => {
            if let Ok(type_id_i) = expr.get_expr_type() {
                result.push_str(&to_string_type_id(ty_env, type_id_i)?);
            } else {
                panic!("Expr type None.");
            }
        }

        Ty::Array(type_id_i, dim_opt, ..) => {
            result.push('[');
            result.push_str(&to_string_type_id(ty_env, type_id_i)?);
            if let Some(dim) = dim_opt {
                let dim_str = match dim.as_ref() {
                    Expr::Lit(Lit::Integer(value, ..), ..) => value.clone(),

                    // TODO: These array dimensions aren't correct, need to
                    //       be able to get the actual int value. This is
                    //       better than nothing for now, but might also
                    //       break any compares done of the dimension.
                    Expr::Var(var) => format!("var({})", var.name),
                    Expr::FnCall(fn_call) => format!("fn_call({})", fn_call.name),
                    Expr::FnPtr(fn_ptr) => format!("fn_ptr({})", fn_ptr.name),
                    Expr::BuiltInCall(bi_call) => format!("bi_call({})", bi_call.name),
                    Expr::Op(Op::BinOp(_)) => "bin_op".into(),
                    Expr::Op(Op::UnOp(_)) => "un_op".into(),

                    _ => {
                        return Err(LangError::new(
                            format!("Found invalid expression in array dimension: {:#?}", dim),
                            LangErrorKind::GeneralError,
                            dim.file_pos().cloned(),
                        ))
                    }
                };
                result.push(',');
                result.push_str(&dim_str);
            }
            result.push(']');
        }

        Ty::Generic(gen_ident, id, ..) => {
            result.push_str(&format!("gen({}-{})", gen_ident, id));
        }
        Ty::GenericInstance(gen_ident, id, ..) => {
            result.push_str(&format!("genInst({}-{})", gen_ident, id));
        }

        Ty::Any(id, ..) => result.push_str(&format!("any({})", id)),

        // TODO: Can these be solved in a better way? Currently only print
        //       the type ID for the ADT because otherwise it can cause a
        //       infinite loop.
        Ty::UnknownAdtMember(adt_type_id, ref member, ..) => {
            result.push_str("adtMember(");
            //result.push_str(&self.to_string_type_id(ty_ctx, *adt_type_id)?);
            result.push_str(&format!("typeId({})", adt_type_id));
            result.push('.');
            match member {
                Either::Left(name) => result.push_str(name),
                Either::Right(idx) => result.push_str(&idx.to_string()),
            }
            result.push(')');
        }
        Ty::UnknownAdtMethod(adt_type_id, ref method_path, ..) => {
            result.push_str("adtMethod(");
            //result.push_str(&self.to_string_type_id(ty_ctx, *adt_type_id)?);
            result.push_str(&format!("typeId({})", adt_type_id));
            result.push('.');
            result.push_str(&to_string_path(ty_env, method_path));
            result.push(')');
        }
        Ty::UnknownFnArgument(adt_type_id, ref method_path, name_or_idx, ..) => {
            result.push_str("adtMethodArg(");
            result.push_str(&format!("typeId({})", adt_type_id));
            result.push('.');
            result.push_str(&to_string_path(ty_env, method_path));
            result.push('.');
            match name_or_idx {
                Either::Left(name) => result.push_str(&name),
                Either::Right(idx) => result.push_str(&idx.to_string()),
            }
            result.push(')');
        }
        Ty::UnknownArrayMember(arr_type_id, ..) => {
            result.push_str("arr(");
            result.push_str(&to_string_type_id(ty_env, arr_type_id)?);
            result.push(')')
        }
    }

    Ok(result)
}

pub fn to_string_inner_ty(ty_env: &TyEnv, inner_ty: &InnerTy) -> String {
    match inner_ty {
        InnerTy::Void => "void".into(),
        InnerTy::Character => "char".into(),
        InnerTy::String => "String".into(),
        InnerTy::Boolean => "bool".into(),
        InnerTy::Int => "int".into(),
        InnerTy::Uint => "uint".into(),
        InnerTy::I8 => "i8".into(),
        InnerTy::U8 => "u8".into(),
        InnerTy::I16 => "i16".into(),
        InnerTy::U16 => "u16".into(),
        InnerTy::I32 => "i32".into(),
        InnerTy::U32 => "u32".into(),
        InnerTy::F32 => "f32".into(),
        InnerTy::I64 => "i64".into(),
        InnerTy::U64 => "u64".into(),
        InnerTy::F64 => "f64".into(),
        InnerTy::I128 => "i128".into(),
        InnerTy::U128 => "u128".into(),

        InnerTy::Struct(path)
        | InnerTy::Enum(path)
        | InnerTy::Union(path)
        | InnerTy::Trait(path)
        | InnerTy::Tuple(path) => to_string_path(ty_env, path),

        InnerTy::Unknown(_) => "unknown".into(),
        InnerTy::UnknownInt(_, _) => "unknown_int".into(),
        InnerTy::UnknownFloat(_) => "unknown_float".into(),
        InnerTy::UnknownIdent(path, ..) => {
            format!("unknown_ident({})", to_string_path(ty_env, path))
        }
    }
}

pub fn to_string_generics(ty_env: &TyEnv, generics: &Generics) -> LangResult<String> {
    let type_ids = generics.iter_types().copied().collect::<Vec<_>>();
    Ok(format!("<{}>", to_string_list(ty_env, &type_ids)?))
}

pub fn to_string_path(ty_env: &TyEnv, path: &LangPath) -> String {
    const SEP: &str = "::";
    path.parts
        .iter()
        .map(|path_part| to_string_path_part(ty_env, path_part))
        .collect::<Vec<_>>()
        .join(SEP)
}

pub fn to_string_path_part(ty_env: &TyEnv, path_part: &LangPathPart) -> String {
    if let Some(generics) = &path_part.1 {
        util::to_generic_name(ty_env, &path_part.0, generics)
    } else {
        path_part.0.clone()
    }
}

pub fn to_string_list(ty_env: &TyEnv, ids: &[TypeId]) -> LangResult<String> {
    let mut result_vec = Vec::default();
    for type_id in ids {
        result_vec.push(to_string_type_id(ty_env, *type_id)?);
    }

    let mut result = String::new();
    result.push_str(&result_vec.join(","));
    Ok(result)
}
