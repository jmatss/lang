use std::hash::Hash;

use either::Either;

use crate::{file::FilePosition, token::expr::Expr, TypeId, UniqueId};

use super::{generics::Generics, inner_ty::InnerTy, type_info::TypeInfo};

#[derive(Debug, Clone, Eq)]
pub enum Ty {
    /// A base type that can contain generic types. The first boxed type is
    /// the actual type and the vector of types are the generic types.
    CompoundType(InnerTy, Generics, TypeInfo),

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

    /// Unknown member of the struct/enum/trait of the first TypeId. The first
    /// String is the name of the member.
    UnknownAdtMember(TypeId, String, UniqueId, TypeInfo),

    /// Unknown method of the struct/enum/trait type "Type" with the name of the
    /// first "String". The vector are generics specified at the function call,
    /// the usize is the index and the types are "UnknownMethodGeneric" types.
    UnknownAdtMethod(TypeId, String, Vec<TypeId>, UniqueId, TypeInfo),

    /// Unknown method argument of the struct/enum/trait type "TypeId" with
    /// the name "String". The vector are generics specified at the function call,
    /// and the "Either" is either the name of the argument or the index of the
    /// argument in the method call if no argument name is set.
    UnknownMethodArgument(
        TypeId,
        String,
        Vec<TypeId>,
        Either<String, usize>,
        UniqueId,
        TypeInfo,
    ),

    /// Unknown method generic argument of the struct/enum/trait type "TypeId"
    /// with the name "String". The "Either<usize, String>" is either the index
    /// or the name of the generic argument in the method call.
    UnknownMethodGeneric(TypeId, String, Either<usize, String>, UniqueId, TypeInfo),

    /// Unknown type of array member of array with type "TypeId".
    UnknownArrayMember(TypeId, UniqueId, TypeInfo),
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
            | Ty::UnknownMethodArgument(.., type_info)
            | Ty::UnknownMethodGeneric(.., type_info)
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
            | Ty::UnknownMethodArgument(.., type_info)
            | Ty::UnknownMethodGeneric(.., type_info)
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

impl Hash for Ty {
    #[allow(clippy::many_single_char_names)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: Better way to hash other than adding an arbitrary int to make
        //       the enum variants "unique"?
        match self {
            Ty::CompoundType(a, b, _) => {
                0.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::Pointer(a, _) => {
                1.hash(state);
                a.hash(state);
            }
            Ty::Array(a, b, _) => {
                2.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::Fn(a, b, c, _) => {
                3.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }
            Ty::Any(a, _) => {
                4.hash(state);
                a.hash(state);
            }
            Ty::Generic(a, b, _) => {
                5.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::GenericInstance(a, b, _) => {
                6.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::Expr(a, _) => {
                7.hash(state);
                a.hash(state);
            }
            Ty::UnknownAdtMember(a, b, c, _) => {
                8.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }
            Ty::UnknownAdtMethod(a, b, c, d, _) => {
                9.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
            Ty::UnknownMethodArgument(a, b, c, d, e, _) => {
                10.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
                e.hash(state);
            }
            Ty::UnknownMethodGeneric(a, b, c, d, _) => {
                11.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
            Ty::UnknownArrayMember(a, b, _) => {
                12.hash(state);
                a.hash(state);
                b.hash(state);
            }
        }
    }
}

impl PartialEq for Ty {
    /// Compares two types. This function is overriden to allow for types (ex.
    /// generics) to contain type hints that isn't used during the compare.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Ty::CompoundType(self_inner, self_generics, ..),
                Ty::CompoundType(other_inner, other_generics, ..),
            ) => self_inner == other_inner && self_generics == other_generics,

            (Ty::Pointer(self_ty, ..), Ty::Pointer(other_ty, ..)) => self_ty == other_ty,

            (Ty::Array(self_ty, self_dim, ..), Ty::Array(other_ty, other_dim, ..)) => {
                self_ty == other_ty && self_dim == other_dim
            }

            (
                Ty::Fn(self_gens, self_params, self_ret, ..),
                Ty::Fn(other_gens, other_params, other_ret, ..),
            ) => self_gens == other_gens && self_params == other_params && self_ret == other_ret,

            (Ty::Any(self_id, ..), Ty::Any(other_id, ..)) => self_id == other_id,

            (Ty::Generic(self_ident, self_id, ..), Ty::Generic(other_ident, other_id, ..)) => {
                self_ident == other_ident && self_id == other_id
            }
            (
                Ty::GenericInstance(self_ident, self_id, ..),
                Ty::GenericInstance(other_ident, other_id, ..),
            ) => self_ident == other_ident && self_id == other_id,

            (Ty::Expr(self_expr, ..), Ty::Expr(other_expr, ..)) => self_expr == other_expr,

            (
                Ty::UnknownAdtMember(self_ty, self_ident, self_id, ..),
                Ty::UnknownAdtMember(other_ty, other_ident, other_id, ..),
            ) => self_ty == other_ty && self_ident == other_ident && self_id == other_id,

            (
                Ty::UnknownAdtMethod(self_ty, self_ident, self_gens, self_id, ..),
                Ty::UnknownAdtMethod(other_ty, other_ident, other_gens, other_id, ..),
            ) => {
                self_ty == other_ty
                    && self_ident == other_ident
                    && self_gens == other_gens
                    && self_id == other_id
            }

            (
                Ty::UnknownMethodArgument(self_ty, self_ident, self_pos, self_gens, self_id, ..),
                Ty::UnknownMethodArgument(
                    other_ty,
                    other_ident,
                    other_pos,
                    other_gens,
                    other_id,
                    ..,
                ),
            ) => {
                self_ty == other_ty
                    && self_ident == other_ident
                    && self_pos == other_pos
                    && self_gens == other_gens
                    && self_id == other_id
            }

            (
                Ty::UnknownMethodGeneric(self_ty, self_ident, self_pos, self_id, ..),
                Ty::UnknownMethodGeneric(other_ty, other_ident, other_pos, other_id, ..),
            ) => {
                self_ty == other_ty
                    && self_ident == other_ident
                    && self_pos == other_pos
                    && self_id == other_id
            }

            (
                Ty::UnknownArrayMember(self_ty, self_id, ..),
                Ty::UnknownArrayMember(other_ty, other_id, ..),
            ) => self_ty == other_ty && self_id == other_id,

            _ => false,
        }
    }
}
