use std::hash::Hash;

use either::Either;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    hash::{DerefType, TyEnvHash},
    path::LangPath,
    token::{expr::Expr, lit::Lit},
    UniqueId,
};

use super::{inner_ty::InnerTy, ty_env::TyEnv, type_id::TypeId, type_info::TypeInfo};

#[derive(Debug, Clone)]
pub enum Ty {
    /// A base type that can contain generic types. The first boxed type is
    /// the actual type. If this type contains generics, it will be stored inside
    /// the ADT path of the `InnerTy`.
    CompoundType(InnerTy, TypeInfo),

    /// A pointer to a type.
    Pointer(TypeId, TypeInfo),

    /// The Option in the "Array" enum indicates the size. If it is None, assume
    /// size is unknown (probably slice).
    Array(TypeId, Option<Box<Expr>>, TypeInfo),

    /// A pointer to a function. The first vec are the generics, the second vec
    /// are the parameters and the last TypeId is the return type.
    Fn(Vec<TypeId>, Vec<TypeId>, Option<TypeId>, TypeInfo),

    /// Represents a type that can be of any type. This will ex. be used for
    /// functions that takes a Type as a parameter, then the parameter type
    /// would be "Any".
    ///
    /// The unique ID is used to differentiate between Any types so that they all
    /// don't map to each other.
    Any(UniqueId, TypeInfo),

    /// A generic type. Ex. a generic "T" on a struct would be represented
    /// as a "Generic" containing the string "T".
    ///
    /// The `UniqueId` is used to differentiate between Generic types so that
    /// Generic's with the same name (ex. for two different structs) does NOT get
    /// mapped together.
    Generic(String, UniqueId, TypeInfo),

    /// A generic type that represent the actual instance/implementation of a
    /// generic type. This is a type that is inferred from either outside the
    /// struct/function with the generic, or it is "hardcoded" at the struct init
    /// or function call.
    ///
    /// The first String is the name of the generic type (ex. "T").
    /// The `UniqueId` is used to differentiate between GenericInstance types so
    /// that GenericIntances's with the same name (ex. for two different structs)
    /// does NOT get mapped together.
    GenericInstance(String, UniqueId, TypeInfo),

    /// Represents a expression parsed as a type. The resulting type will be
    /// the return type of the expression.
    ///
    /// Example for a built in call:
    /// ```ignore
    /// var i: i64 = 123
    /// var x = StructWithGeneric<@type(i)> {}
    /// ```
    /// where `@type(i)` would return the type of expression `i` which is `i64`.
    Expr(Box<Expr>, TypeInfo),

    /// Unknown member of the struct/enum/trait of the first TypeId.
    /// The "Either<String, usize>" is either the index or the name of the member
    UnknownAdtMember(TypeId, Either<String, usize>, UniqueId, TypeInfo),

    /// Unknown method of the struct/enum/trait type "TypeId".
    /// The "Langpath" is the name of the method + potential generics.
    /// The vector of type ids are the arguments of the method (will be used
    /// to infer generics if needed).
    UnknownAdtMethod(TypeId, LangPath, Vec<TypeId>, UniqueId, TypeInfo),

    /// Unknown argument for a method on the ADT "TypeId".
    /// The "Langpath" is the module+name of the fn/method and the
    /// "Either<String, usize>" is either the index or the name of the argument
    /// in the fn call.
    UnknownFnArgument(TypeId, LangPath, Either<String, usize>, UniqueId, TypeInfo),

    /// Unknown type of array member of array with type "TypeId".
    UnknownArrayMember(TypeId, UniqueId, TypeInfo),
}

impl TyEnvHash for Ty {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        match self {
            Ty::CompoundType(inner_ty, ..) => {
                0.hash(state);
                inner_ty.hash_with_state(ty_env, deref_type, state)?;
            }
            Ty::Pointer(type_id, _) => {
                1.hash(state);
                type_id.hash_with_state(ty_env, deref_type, state)?;
            }
            Ty::Array(type_id, expr_opt, _) => {
                2.hash(state);
                type_id.hash_with_state(ty_env, deref_type, state)?;
                if let Some(expr) = expr_opt {
                    if let Some(dim) = arr_dim_hash(expr)? {
                        dim.hash(state);
                    }
                }
            }
            Ty::Fn(gens, pars, ret_opt, _) => {
                3.hash(state);
                "gens".hash(state);
                gens.as_slice().hash_with_state(ty_env, deref_type, state)?;
                "params".hash(state);
                pars.as_slice().hash_with_state(ty_env, deref_type, state)?;
                "ret".hash(state);
                if let Some(type_id) = ret_opt {
                    type_id.hash_with_state(ty_env, deref_type, state)?;
                }
            }
            Ty::Any(unique_id, _) => {
                4.hash(state);
                unique_id.hash(state);
            }
            Ty::Generic(ident, unique_id, _) => {
                5.hash(state);
                ident.hash(state);
                unique_id.hash(state);
            }
            Ty::GenericInstance(ident, unique_id, _) => {
                6.hash(state);
                ident.hash(state);
                unique_id.hash(state);
            }
            Ty::Expr(expr, _) => {
                7.hash(state);
                // TODO: How should this be handled? Will the current solution work
                //       correctly? Since the "parsing" hash will be different from
                //       the hash produced when a type is set, there will be two types
                //       in the TyEnv for the different versions. Will this break
                //       anything?
                // If this hashing is done during parsing, expression will not have
                // their types set at this point. Use a random number to make the
                // hashes unique.
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    expr_type_id.hash_with_state(ty_env, deref_type, state)?;
                } else {
                    let rand_number = rand::random::<u64>();
                    rand_number.hash(state);
                }
            }
            Ty::UnknownAdtMember(.., unique_id, _) => {
                8.hash(state);
                unique_id.hash(state);
            }
            Ty::UnknownAdtMethod(.., unique_id, _) => {
                9.hash(state);
                unique_id.hash(state);
            }
            Ty::UnknownFnArgument(.., unique_id, _) => {
                10.hash(state);
                unique_id.hash(state);
            }
            Ty::UnknownArrayMember(_, unique_id, _) => {
                11.hash(state);
                unique_id.hash(state);
            }
        }

        Ok(())
    }
}

fn arr_dim_hash(expr: &Expr) -> LangResult<Option<u64>> {
    match expr {
        Expr::Lit(Lit::Integer(number, radix), _, _) => {
            u64::from_str_radix(number, *radix).map(Some).map_err(|_| {
                LangError::new(
                    format!("Unable to hash array dimension, expr: {:#?}", expr),
                    LangErrorKind::GeneralError,
                    expr.file_pos().cloned(),
                )
            })
        }

        // TODO: Add more valid array dimensions in the future.
        _ => Ok(None),
    }
}

impl Ty {
    pub fn type_info(&self) -> &TypeInfo {
        match self {
            Ty::CompoundType(.., type_info)
            | Ty::Pointer(.., type_info)
            | Ty::Array(.., type_info)
            | Ty::Fn(.., type_info)
            | Ty::Any(.., type_info)
            | Ty::Generic(.., type_info)
            | Ty::GenericInstance(.., type_info)
            | Ty::Expr(.., type_info)
            | Ty::UnknownAdtMember(.., type_info)
            | Ty::UnknownAdtMethod(.., type_info)
            | Ty::UnknownFnArgument(.., type_info)
            | Ty::UnknownArrayMember(.., type_info) => type_info,
        }
    }

    pub fn type_info_mut(&mut self) -> &mut TypeInfo {
        match self {
            Ty::CompoundType(.., type_info)
            | Ty::Pointer(.., type_info)
            | Ty::Array(.., type_info)
            | Ty::Fn(.., type_info)
            | Ty::Any(.., type_info)
            | Ty::Generic(.., type_info)
            | Ty::GenericInstance(.., type_info)
            | Ty::Expr(.., type_info)
            | Ty::UnknownAdtMember(.., type_info)
            | Ty::UnknownAdtMethod(.., type_info)
            | Ty::UnknownFnArgument(.., type_info)
            | Ty::UnknownArrayMember(.., type_info) => type_info,
        }
    }

    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self.type_info() {
            TypeInfo::Default(file_pos)
            | TypeInfo::VarUse(file_pos)
            | TypeInfo::BuiltInCall(file_pos)
            | TypeInfo::VarDecl(file_pos, _)
            | TypeInfo::FuncCall(file_pos)
            | TypeInfo::Enum(file_pos)
            | TypeInfo::EnumMember(_, (_, file_pos))
            | TypeInfo::Generic(file_pos) => Some(file_pos),

            TypeInfo::Lit(file_pos_opt) | TypeInfo::DefaultOpt(file_pos_opt) => {
                file_pos_opt.as_ref()
            }

            TypeInfo::None | TypeInfo::BuiltIn => None,
        }
    }

    pub fn file_pos_mut(&mut self) -> Option<&mut FilePosition> {
        match self.type_info_mut() {
            TypeInfo::Default(file_pos)
            | TypeInfo::VarUse(file_pos)
            | TypeInfo::BuiltInCall(file_pos)
            | TypeInfo::VarDecl(file_pos, _)
            | TypeInfo::FuncCall(file_pos)
            | TypeInfo::Enum(file_pos)
            | TypeInfo::EnumMember(_, (_, file_pos))
            | TypeInfo::Generic(file_pos) => Some(file_pos),

            TypeInfo::Lit(file_pos_opt) | TypeInfo::DefaultOpt(file_pos_opt) => {
                file_pos_opt.as_mut()
            }

            TypeInfo::None | TypeInfo::BuiltIn => None,
        }
    }
}

/// Struct used to indicate what counts as solved.
///
/// Types that (potentially) can be considered solved:
///  * Primitives
///  * ADTs
///  * Traits
///  * Pointers containing solved types
///  * Arrays containing solved types
///  * Unknown ints (default)
///  * Unknown floats (default)
///  * Any
///  * Generic/GenericInstance
///  * Unknown ADT/method related types
///
/// The list above is all types that possible can be considered solved.
/// A subset of this list can be used instead depending on the bools set in a
/// given `SolveCond`. Any bool values set to false will be exluded from being
/// counted as solvable, meaning that the specific type is always considered
/// unsolved.
#[derive(Debug, Clone, Copy)]
pub struct SolveCond {
    default: bool,
    generic: bool,
    generic_instance: bool,
    unknown: bool,
    any: bool,
}

impl Default for SolveCond {
    fn default() -> Self {
        Self {
            default: true,
            generic: true,
            generic_instance: true,
            unknown: true,
            any: true,
        }
    }
}

impl SolveCond {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn excl_default(mut self) -> Self {
        self.default = false;
        self
    }

    pub fn excl_gen(mut self) -> Self {
        self.generic = false;
        self
    }

    pub fn excl_gen_inst(mut self) -> Self {
        self.generic_instance = false;
        self
    }

    pub fn excl_unknown(mut self) -> Self {
        self.unknown = false;
        self
    }

    pub fn excl_any(mut self) -> Self {
        self.any = false;
        self
    }

    pub fn can_solve_default(self) -> bool {
        self.default
    }

    pub fn can_solve_gen(self) -> bool {
        self.generic
    }

    pub fn can_solve_gen_inst(self) -> bool {
        self.generic_instance
    }

    pub fn can_solve_unknown(self) -> bool {
        self.unknown
    }

    pub fn can_solve_any(self) -> bool {
        self.any
    }
}
