use std::cmp::Ordering;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    ty::ty::Ty,
    TypeId,
};

use super::{inner_ty::InnerTy, ty_env::TyEnv};

// TODO: Can the preference between point 4 and 5 fail some valid programs?
//       Ex. a pointer to a unknown is currently preferred over a GenericInstance.
//       Should there be two different points for fn/array/pointer containing
//       solved types and fn/array/pointer containing unsolved types? These
//       would then be inserted before and after the GenericInstance respectively
//       in the preference list below.
/// Checks the precedence for the two given types when it comes to mapping
/// during type inference. Returns a `std::cmp::Ordering` indicating the
/// preference between the given types.
///
/// `Ordering::Less` indicates that the type with ID `first_id` is preferred.
/// `Ordering::Greater` indicates that the type with ID`second_id` is preferred.
///
/// If two types have the exact same preference, the one with the lowest
/// type ID will be picked. This is done to try and keep thev precedence as
/// "uniform" as possible. If the given type IDs are equal, the first one
/// is chosen (this function returns `Ordering::Less`).
///
/// General ordering list (most preferred at the top, least at bottom):
///  1.  Primitive
///  2.  ADT/Trait/Tuple
///  3.  Fn
///      Array
///      Pointer
///  4.  Generic
///  5.  UnknownInt
///      UnknownFloat
///  6.  GenericInstance
///  7.  UnknownIdent
///      UnknownMethodGeneric
///      UnknownMethodArgument
///      UnknownAdtMethod
///      UnknownAdtMember
///  8.  UnknownArrayMember
///  9.  Unknown
///  10. Any
///  11. Expr   (expression evaluated to a type)
///
/// The reason for `Generic` being prefered over `UnknownInt`/`UnknownFloat` is
/// to allow for returning a default literal int/float from a function returning
/// a generic type.
pub fn precedence(ty_env: &TyEnv, first_id: TypeId, second_id: TypeId) -> LangResult<Ordering> {
    match prec_allow_eq(ty_env, first_id, second_id)? {
        Ordering::Equal => Ok(prec_eq(first_id, second_id)),
        res => Ok(res),
    }
}

/// This function is equal to the `precedence()` with the only difference that
/// it returns `Ordering::Equal` for some types that are equals. This is
/// needed to comapre the types properly to then return a "final" answer
/// to the caller of `precedence()` that will either prefer the first or
/// second type.
///
/// OBS! The type IDs of the ADTs in "Unknown..." types ex. "UnknownAdtMember"
///      can't be compared since different ADTs might have members of the
///      same type. These can then be mapped together during type inference.
fn prec_allow_eq(ty_env: &TyEnv, first_id: TypeId, second_id: TypeId) -> LangResult<Ordering> {
    if first_id == second_id {
        return Ok(prec_eq(first_id, second_id));
    }

    let inf_first_id = ty_env.inferred_type(first_id)?;
    let inf_second_id = ty_env.inferred_type(second_id)?;

    // If the two types are in the same substitution set, use the "original"
    // type IDs when checking the preference. If they are in two different sets,
    // use the inferred type i.e. the root of their respective subtitution
    // sets to check the preference.
    let (prec_first_id, prec_second_id) = if inf_first_id == inf_second_id {
        (first_id, second_id)
    } else {
        (inf_first_id, inf_second_id)
    };

    debug!(
        "precedence_allow_equal -- first_id: {}, inf_first_id: {}, prec_first_id: {}, \n\
            second_id: {}, inf_second_id: {}, prec_second_id: {}",
        first_id, inf_first_id, prec_first_id, second_id, inf_second_id, prec_second_id
    );

    match (ty_env.ty(prec_first_id)?, ty_env.ty(prec_second_id)?) {
        (Ty::Expr(expr_a, ..), Ty::Expr(expr_b, ..)) => {
            let type_id_a = expr_a.get_expr_type()?;
            let type_id_b = expr_b.get_expr_type()?;
            prec_allow_eq(ty_env, type_id_a, type_id_b)
        }
        (Ty::Expr(expr, ..), other_ty) | (other_ty, Ty::Expr(expr, ..)) => {
            let expr_type_id = expr.get_expr_type()?;
            let other_type_id = ty_env.id_try(other_ty)?;
            prec_allow_eq(ty_env, expr_type_id, other_type_id)
        }

        (Ty::Any(id_a, ..), Ty::Any(id_b, ..)) => Ok(id_a.cmp(id_b)),
        (Ty::Any(..), _) => Ok(Ordering::Greater),
        (_, Ty::Any(..)) => Ok(Ordering::Less),

        (Ty::CompoundType(inner_a, info_a), Ty::CompoundType(inner_b, info_b))
            if inner_a.is_unknown() && inner_b.is_unknown() =>
        {
            prec_inner_ty(inner_a, info_a.file_pos(), inner_b, info_b.file_pos())
        }
        (Ty::CompoundType(inner, ..), _) if inner.is_unknown() => Ok(Ordering::Greater),
        (_, Ty::CompoundType(inner, ..)) if inner.is_unknown() => Ok(Ordering::Less),

        (
            Ty::UnknownArrayMember(type_id_a, unique_id_a, ..),
            Ty::UnknownArrayMember(type_id_b, unique_id_b, ..),
        ) => {
            let ord = prec_allow_eq(ty_env, *type_id_a, *type_id_b)?;
            if ord != Ordering::Equal {
                Ok(ord)
            } else if unique_id_a != unique_id_b {
                Ok(unique_id_a.cmp(unique_id_b))
            } else {
                Ok(Ordering::Equal)
            }
        }
        (Ty::UnknownArrayMember(..), _) => Ok(Ordering::Greater),
        (_, Ty::UnknownArrayMember(..)) => Ok(Ordering::Less),

        (
            Ty::UnknownAdtMember(_, _, unique_id_a, ..),
            Ty::UnknownAdtMember(_, _, unique_id_b, ..),
        ) => {
            if unique_id_a != unique_id_b {
                Ok(unique_id_a.cmp(unique_id_b))
            } else {
                Ok(Ordering::Equal)
            }
        }
        (Ty::UnknownAdtMember(..), _) => Ok(Ordering::Greater),
        (_, Ty::UnknownAdtMember(..)) => Ok(Ordering::Less),

        (
            Ty::UnknownAdtMethod(_, _, unique_id_a, ..),
            Ty::UnknownAdtMethod(_, _, unique_id_b, ..),
        ) => {
            if unique_id_a != unique_id_b {
                Ok(unique_id_a.cmp(unique_id_b))
            } else {
                Ok(Ordering::Equal)
            }
        }
        (Ty::UnknownAdtMethod(..), _) => Ok(Ordering::Greater),
        (_, Ty::UnknownAdtMethod(..)) => Ok(Ordering::Less),

        (
            Ty::UnknownFnArgument(_, _, _, unique_id_a, ..),
            Ty::UnknownFnArgument(_, _, _, unique_id_b, ..),
        ) => {
            if unique_id_a != unique_id_b {
                Ok(unique_id_a.cmp(unique_id_b))
            } else {
                Ok(Ordering::Equal)
            }
        }
        (Ty::UnknownFnArgument(..), _) => Ok(Ordering::Greater),
        (_, Ty::UnknownFnArgument(..)) => Ok(Ordering::Less),

        (Ty::CompoundType(inner_a, info_a), Ty::CompoundType(inner_b, info_b))
            if inner_a.is_unknown_ident() && inner_b.is_unknown_ident() =>
        {
            prec_inner_ty(inner_a, info_a.file_pos(), inner_b, info_b.file_pos())
        }
        (Ty::CompoundType(inner, ..), _) if inner.is_unknown_ident() => Ok(Ordering::Greater),
        (_, Ty::CompoundType(inner, ..)) if inner.is_unknown_ident() => Ok(Ordering::Less),

        (Ty::GenericInstance(_, id_a, ..), Ty::GenericInstance(_, id_b, ..)) => Ok(id_a.cmp(id_b)),
        (Ty::GenericInstance(..), _) => Ok(Ordering::Greater),
        (_, Ty::GenericInstance(..)) => Ok(Ordering::Less),

        (Ty::CompoundType(inner_a, info_a), Ty::CompoundType(inner_b, info_b))
            if (inner_a.is_unknown_int() || inner_a.is_unknown_float())
                && (inner_b.is_unknown_int() || inner_b.is_unknown_float()) =>
        {
            prec_inner_ty(inner_a, info_a.file_pos(), inner_b, info_b.file_pos())
        }
        (Ty::CompoundType(inner, ..), _) if inner.is_unknown_int() || inner.is_unknown_float() => {
            Ok(Ordering::Greater)
        }
        (_, Ty::CompoundType(inner, ..)) if inner.is_unknown_int() || inner.is_unknown_float() => {
            Ok(Ordering::Less)
        }

        (Ty::Generic(_, id_a, ..), Ty::Generic(_, id_b, ..)) => Ok(id_a.cmp(id_b)),
        (Ty::Generic(..), _) => Ok(Ordering::Greater),
        (_, Ty::Generic(..)) => Ok(Ordering::Less),

        (Ty::Pointer(type_id_a, ..), Ty::Pointer(type_id_b, ..)) => {
            prec_allow_eq(ty_env, *type_id_a, *type_id_b)
        }
        (Ty::Pointer(..), _) => Ok(Ordering::Greater),
        (_, Ty::Pointer(..)) => Ok(Ordering::Less),

        (Ty::Array(type_id_a, ..), Ty::Array(type_id_b, ..)) => {
            // TODO: Probably makes sense to consider the dimension during compare.
            prec_allow_eq(ty_env, *type_id_a, *type_id_b)
        }
        (Ty::Array(..), _) => Ok(Ordering::Greater),
        (_, Ty::Array(..)) => Ok(Ordering::Less),

        (
            Ty::Fn(gen_type_ids_a, param_type_ids_a, ret_type_id_a, ..),
            Ty::Fn(gen_type_ids_b, param_type_ids_b, ret_type_id_b, ..),
        ) => {
            match (ret_type_id_a, ret_type_id_b) {
                (Some(ret_type_id_a), Some(ret_type_id_b)) => {
                    let ord = prec_allow_eq(ty_env, *ret_type_id_a, *ret_type_id_b)?;
                    if ord != Ordering::Equal {
                        return Ok(ord);
                    }
                }
                (None, None) => (),
                _ => unreachable!(
                    "ret_type_id_a: {:?}, ret_type_id_b: {:?}",
                    ret_type_id_a, ret_type_id_b
                ),
            }

            assert!(
                gen_type_ids_a.len() == gen_type_ids_b.len(),
                "gen len diff. gen_type_ids_a: {:#?}, gen_type_ids_b: {:#?}",
                gen_type_ids_a,
                gen_type_ids_b
            );
            for (gen_type_id_a, gen_type_id_b) in gen_type_ids_a.iter().zip(gen_type_ids_b.iter()) {
                let ord = prec_allow_eq(ty_env, *gen_type_id_a, *gen_type_id_b)?;
                if ord != Ordering::Equal {
                    return Ok(ord);
                }
            }

            assert!(
                param_type_ids_a.len() == param_type_ids_b.len(),
                "param len diff. param_type_ids_a: {:#?}, param_type_ids_b: {:#?}",
                param_type_ids_a,
                param_type_ids_b
            );
            for (par_type_id_a, par_type_id_b) in
                param_type_ids_a.iter().zip(param_type_ids_b.iter())
            {
                let ord = prec_allow_eq(ty_env, *par_type_id_a, *par_type_id_b)?;
                if ord != Ordering::Equal {
                    return Ok(ord);
                }
            }

            Ok(Ordering::Equal)
        }
        (Ty::Fn(..), _) => Ok(Ordering::Greater),
        (_, Ty::Fn(..)) => Ok(Ordering::Less),

        (Ty::CompoundType(inner_a, info_a), Ty::CompoundType(inner_b, info_b)) => {
            let ord = prec_inner_ty(inner_a, info_a.file_pos(), inner_b, info_b.file_pos())?;
            if ord != Ordering::Equal {
                return Ok(ord);
            }

            let gens_a = inner_a.gens();
            let gens_b = inner_b.gens();

            if gens_a.is_none() && gens_b.is_none() {
                return Ok(Ordering::Equal);
            }

            assert!(
                gens_a.is_some() == gens_b.is_some(),
                "gens diff Some/None. gens_a: {:#?}, gens_b: {:#?}",
                gens_a,
                gens_b
            );

            let gens_a = gens_a.unwrap();
            let gens_b = gens_b.unwrap();

            for (gen_a_type_id, gen_b_type_id) in gens_a.iter_types().zip(gens_b.iter_types()) {
                let ord = prec_allow_eq(ty_env, *gen_a_type_id, *gen_b_type_id)?;
                if ord != Ordering::Equal {
                    return Ok(ord);
                }
            }

            Ok(Ordering::Equal)
        }
    }
}

fn prec_inner_ty(
    first_inner_ty: &InnerTy,
    first_file_pos: Option<&FilePosition>,
    second_inner_ty: &InnerTy,
    second_file_pos: Option<&FilePosition>,
) -> LangResult<Ordering> {
    Ok(match (first_inner_ty, second_inner_ty) {
        (InnerTy::Unknown(unique_id_a), InnerTy::Unknown(unique_id_b)) => {
            if unique_id_a != unique_id_b {
                unique_id_a.cmp(unique_id_b)
            } else {
                Ordering::Equal
            }
        }
        (InnerTy::Unknown(_), _) => Ordering::Greater,
        (_, InnerTy::Unknown(_)) => Ordering::Less,

        (InnerTy::UnknownInt(unique_id_a, ..), InnerTy::UnknownInt(unique_id_b, ..)) => {
            if unique_id_a != unique_id_b {
                unique_id_a.cmp(unique_id_b)
            } else {
                Ordering::Equal
            }
        }
        (InnerTy::UnknownInt(..), _) => Ordering::Greater,
        (_, InnerTy::UnknownInt(..)) => Ordering::Less,

        (InnerTy::UnknownFloat(unique_id_a), InnerTy::UnknownFloat(unique_id_b)) => {
            if unique_id_a != unique_id_b {
                unique_id_a.cmp(unique_id_b)
            } else {
                Ordering::Equal
            }
        }
        (InnerTy::UnknownFloat(..), _) => Ordering::Greater,
        (_, InnerTy::UnknownFloat(..)) => Ordering::Less,

        (InnerTy::UnknownIdent(..), InnerTy::UnknownIdent(..)) => Ordering::Equal,
        (InnerTy::UnknownIdent(..), _) => Ordering::Greater,
        (_, InnerTy::UnknownIdent(..)) => Ordering::Less,

        (InnerTy::Struct(_), InnerTy::Struct(_))
        | (InnerTy::Enum(_), InnerTy::Enum(_))
        | (InnerTy::Union(_), InnerTy::Union(_))
        | (InnerTy::Trait(_), InnerTy::Trait(_))
        | (InnerTy::Tuple(_), InnerTy::Tuple(_))
        | (InnerTy::Void, InnerTy::Void)
        | (InnerTy::Character, InnerTy::Character)
        | (InnerTy::String, InnerTy::String)
        | (InnerTy::Boolean, InnerTy::Boolean)
        | (InnerTy::I8, InnerTy::I8)
        | (InnerTy::U8, InnerTy::U8)
        | (InnerTy::I16, InnerTy::I16)
        | (InnerTy::U16, InnerTy::U16)
        | (InnerTy::I32, InnerTy::I32)
        | (InnerTy::U32, InnerTy::U32)
        | (InnerTy::F32, InnerTy::F32)
        | (InnerTy::I64, InnerTy::I64)
        | (InnerTy::U64, InnerTy::U64)
        | (InnerTy::F64, InnerTy::F64)
        | (InnerTy::I128, InnerTy::I128)
        | (InnerTy::U128, InnerTy::U128) => Ordering::Equal,

        _ => {
            return Err(LangError::new(
                format!(
                    "Tried to map incompatible inner types. First: {:#?}, second: {:#?}.\n\
                    First type found at position: {:#?}.\nSecond found at position: {:#?}",
                    first_inner_ty, second_inner_ty, first_file_pos, second_file_pos
                ),
                LangErrorKind::AnalyzeError,
                None,
            ))
        }
    })
}

/// Function used when the two types have the same preference. This function
/// will decide which type is preferred. The type with the lowest type ID is
/// preferred. If the given type IDs are equal, the first is preferred.
fn prec_eq(first_id: TypeId, second_id: TypeId) -> Ordering {
    if first_id <= second_id {
        Ordering::Less
    } else {
        Ordering::Greater
    }
}
