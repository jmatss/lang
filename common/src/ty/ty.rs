use super::{generics::Generics, inner_ty::InnerTy};
use crate::{token::expr::Expr, type_info::TypeInfo, TypeId};
use either::Either;
use std::hash::Hash;

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
    Any(TypeInfo),

    /// A generic type. Ex. a generic "T" on a struct would be represented
    /// as a "Generic" containing the string "T".
    Generic(String, TypeInfo),

    /// A generic type that represent the actual instance/implementation of a
    /// generic type. This is a type that is inferred from either outside the
    /// struct/function with the generic, or it is "hardcoded" at the struct init
    /// or function call.
    ///
    /// The first String is the name of the generic type (ex. "T"). The second
    /// String is a unique identifier.
    GenericInstance(String, String, TypeInfo),

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
    /// String is the name of the member and the second String is a unique identifier.
    UnknownAdtMember(TypeId, String, String, TypeInfo),

    /// Unknown method of the struct/enum/trait type "Type" with the name of the
    /// first "String". The vector are generics specified at the function call,
    /// the usize is the index and the types are "UnknownMethodGeneric" types.
    /// The second String is a unique identifier.
    UnknownAdtMethod(TypeId, String, Vec<TypeId>, String, TypeInfo),

    /// Unknown method argument of the struct/enum/trait type "TypeId" with
    /// the name "String". The vector are generics specified at the function call,
    /// and the "Either" is either the name of the argument or the index of the
    /// argument in the method call if no argument name is set.
    /// The second String is a unique identifier.
    UnknownMethodArgument(
        TypeId,
        String,
        Vec<TypeId>,
        Either<String, usize>,
        String,
        TypeInfo,
    ),

    /// Unknown method generic argument of the struct/enum/trait type "TypeId"
    /// with the name "String". The "Either<usize, String>" is either the index
    /// or the name of the generic argument in the method call.
    /// The second String is a unique identifier.
    UnknownMethodGeneric(TypeId, String, Either<usize, String>, String, TypeInfo),

    /// Unknown type of array member of array with type "TypeId".
    /// The String is a unique identifier.
    UnknownArrayMember(TypeId, String, TypeInfo),
}

impl Hash for Ty {
    #[allow(clippy::many_single_char_names)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // TODO: Better way to hash other than adding an arbitrary int to make
        //       the enum variants "unique"?
        match self {
            Ty::CompoundType(a, b, c) => {
                0.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }
            Ty::Pointer(a, b) => {
                1.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::Array(a, b, c) => {
                2.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }
            Ty::Fn(a, b, c, d) => {
                3.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
            Ty::Any(a) => {
                4.hash(state);
                a.hash(state);
            }
            Ty::Generic(a, b) => {
                5.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::GenericInstance(a, b, c) => {
                6.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
            }
            Ty::Expr(a, b) => {
                7.hash(state);
                a.hash(state);
                b.hash(state);
            }
            Ty::UnknownAdtMember(a, b, c, d) => {
                8.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
            Ty::UnknownAdtMethod(a, b, c, d, e) => {
                9.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
                e.hash(state);
            }
            Ty::UnknownMethodArgument(a, b, c, d, e, f) => {
                10.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
                e.hash(state);
                f.hash(state);
            }
            Ty::UnknownMethodGeneric(a, b, c, d, e) => {
                11.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
                e.hash(state);
            }
            Ty::UnknownArrayMember(a, b, c) => {
                12.hash(state);
                a.hash(state);
                b.hash(state);
                c.hash(state);
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

            (Ty::Any(..), Ty::Any(..)) => true,

            (Ty::Generic(self_ident, ..), Ty::Generic(other_ident, ..)) => {
                self_ident == other_ident
            }
            (
                Ty::GenericInstance(self_ident, self_unique, ..),
                Ty::GenericInstance(other_ident, other_unique, ..),
            ) => self_ident == other_ident && self_unique == other_unique,

            (Ty::Expr(self_expr, ..), Ty::Expr(other_expr, ..)) => self_expr == other_expr,

            (
                Ty::UnknownAdtMember(self_ty, self_ident, self_id, ..),
                Ty::UnknownAdtMember(other_ty, other_ident, other_id, ..),
            ) => self_ty == other_ty && self_ident == other_ident && self_id == other_id,

            (
                Ty::UnknownAdtMethod(self_ty, self_ident, self_id, ..),
                Ty::UnknownAdtMethod(other_ty, other_ident, other_id, ..),
            ) => self_ty == other_ty && self_ident == other_ident && self_id == other_id,

            (
                Ty::UnknownMethodArgument(self_ty, self_ident, self_pos, self_id, ..),
                Ty::UnknownMethodArgument(other_ty, other_ident, other_pos, other_id, ..),
            ) => {
                self_ty == other_ty
                    && self_ident == other_ident
                    && self_pos == other_pos
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
