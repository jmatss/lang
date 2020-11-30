use std::{fmt::Display, hash::Hash};

use crate::token::expr::Expr;

use either::Either;

use super::{generics::Generics, inner_ty::InnerTy};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    /// A base type that can contain generic types. The first boxed type is
    /// the actual type and the vector of types are the generic types.
    CompoundType(InnerTy, Generics),

    /// A pointer to a type.
    Pointer(Box<Ty>),

    // The Option in the "Array" enum indicates the size. If it is None, assume
    // size is unknown (probably slice).
    Array(Box<Ty>, Option<Box<Expr>>),

    /// A generic type. Ex. a generic "T" on a struct would be represented
    /// as a "Generic" containing the string "T".
    Generic(String),

    /// Unknown member of the struct/enum/interface type "Type" with the member
    /// name "String".
    UnknownStructureMember(Box<Ty>, String),

    /// Unknown method of the struct/enum/interface type "Type" with the name
    /// "String".
    UnknownStructureMethod(Box<Ty>, String),

    /// Unknown method argument of the struct/enum/interface type "Type" with
    /// the name "String". The "Either" is either the name of the argument or
    /// the index of the argument in the method call if no argument name is set.
    UnknownMethodArgument(Box<Ty>, String, Either<String, usize>),

    /// Unknown type of array member of array with type "Type".
    UnknownArrayMember(Box<Ty>),
}

#[allow(clippy::match_like_matches_macro)]
impl Ty {
    pub fn get_inner(&self) -> Option<&InnerTy> {
        if let Ty::CompoundType(inner_ty, ..) = self {
            Some(inner_ty)
        } else {
            None
        }
    }

    /// Recursively replaces any generic identifiers from "UnknownIdent" wrapped
    /// inside a "CompoundType" into "Generic"s.
    pub fn replace_generics(&mut self, generic_names: &[String]) {
        match self {
            Ty::CompoundType(InnerTy::UnknownIdent(ident, ..), generics) => {
                for generic in generics.iter_types_mut() {
                    generic.replace_generics(generic_names);
                }

                *self = Ty::Generic(ident.clone());
            }

            Ty::Pointer(ty) | Ty::Array(ty, _) => ty.replace_generics(generic_names),

            _ => (),
        }
    }

    /// Recursively replaces any "Generic" types with any matches from the
    /// `generics_impl` map. I.e. any "Generic" type that has a ident that is a
    /// key in the `generics_impl` will be replaced with the value in the map.
    pub fn replace_generics_impl(&mut self, generics_impl: &Generics) {
        match self {
            Ty::Generic(ident) => {
                if let Some(new_ty) = generics_impl.get(ident) {
                    *self = new_ty.clone();
                }
            }

            Ty::CompoundType(_, generics) => {
                for generic in generics.iter_types_mut() {
                    generic.replace_generics_impl(generics_impl);
                }
            }

            Ty::Pointer(ty) | Ty::Array(ty, _) => ty.replace_generics_impl(generics_impl),

            _ => (),
        }
    }

    /// Recursively replaces any structure types with idents that matches the
    /// old structure name. These will be replaced with the new type with the
    /// generics "replaced"/"implemented".
    pub fn replace_self(&mut self, old_name: &str, new_self_ty: &Ty) {
        match self {
            Ty::CompoundType(inner_ty, _) => match inner_ty {
                InnerTy::Struct(ident)
                | InnerTy::Enum(ident)
                | InnerTy::Interface(ident)
                | InnerTy::UnknownIdent(ident, ..) => {
                    if ident == old_name {
                        *self = new_self_ty.clone();
                    }
                }
                _ => (),
            },

            Ty::Pointer(ty)
            | Ty::Array(ty, ..)
            | Ty::UnknownStructureMember(ty, ..)
            | Ty::UnknownStructureMethod(ty, ..)
            | Ty::UnknownMethodArgument(ty, ..)
            | Ty::UnknownArrayMember(ty) => ty.replace_self(old_name, new_self_ty),
            _ => (),
        }
    }

    pub fn is_int(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_int())
    }

    pub fn is_float(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_float())
    }

    pub fn is_bool(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_bool())
    }

    pub fn is_char(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_char())
    }

    pub fn is_string(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_string())
    }

    pub fn is_primitive(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_primitive())
    }

    pub fn is_unknown(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_unknown())
    }

    pub fn is_unknown_ident(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_unknown_ident())
    }

    pub fn is_unknown_int(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_unknown_int())
    }

    pub fn is_unknown_float(&self) -> bool {
        self.get_inner().map_or(false, |ty| ty.is_unknown_float())
    }

    pub fn is_aggregate(&self) -> bool {
        if let Ty::CompoundType(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_pointer(&self) -> bool {
        if let Ty::Pointer(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_array(&self) -> bool {
        if let Ty::Array(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_generic(&self) -> bool {
        if let Ty::Generic(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_struct_member(&self) -> bool {
        if let Ty::UnknownStructureMember(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_struct_method(&self) -> bool {
        if let Ty::UnknownStructureMethod(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_method_argument(&self) -> bool {
        if let Ty::UnknownMethodArgument(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_array_member(&self) -> bool {
        if let Ty::UnknownArrayMember(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_any(&self) -> bool {
        match self {
            Ty::UnknownStructureMember(..)
            | Ty::UnknownStructureMethod(..)
            | Ty::UnknownMethodArgument(..)
            | Ty::UnknownArrayMember(_) => true,
            Ty::CompoundType(inner_ty, ..) => {
                inner_ty.is_unknown()
                    || inner_ty.is_unknown_ident()
                    || inner_ty.is_unknown_int()
                    || inner_ty.is_unknown_float()
            }
            _ => false,
        }
    }

    /// Checks if the Type `ty` can be found in self.
    fn contains_ty(&self, ty: &Ty) -> bool {
        match (self, ty) {
            (Ty::CompoundType(_, _), Ty::CompoundType(_, _))
            | (Ty::Pointer(_), Ty::Pointer(_))
            | (Ty::Array(_, _), Ty::Array(_, _))
            | (Ty::Generic(..), Ty::Generic(..))
            | (Ty::UnknownStructureMember(_, _), Ty::UnknownStructureMember(_, _))
            | (Ty::UnknownStructureMethod(_, _), Ty::UnknownStructureMethod(_, _))
            | (Ty::UnknownMethodArgument(_, _, _), Ty::UnknownMethodArgument(_, _, _))
            | (Ty::UnknownArrayMember(_), Ty::UnknownArrayMember(_)) => return true,
            _ => (),
        }

        match self {
            Ty::CompoundType(_, generics) => {
                let mut contains = false;
                for new_ty in generics.iter_types() {
                    if new_ty.contains_ty(ty) {
                        contains = true;
                        break;
                    }
                }
                contains
            }

            Ty::Pointer(new_ty)
            | Ty::Array(new_ty, _)
            | Ty::UnknownStructureMember(new_ty, _)
            | Ty::UnknownStructureMethod(new_ty, _)
            | Ty::UnknownMethodArgument(new_ty, _, _)
            | Ty::UnknownArrayMember(new_ty) => new_ty.contains_ty(ty),

            _ => false,
        }
    }

    fn contains_inner_ty(&self, inner_ty: &InnerTy) -> bool {
        match self {
            Ty::CompoundType(ty, generics) => {
                if ty.contains_inner_ty(inner_ty) {
                    return true;
                }

                for generic in generics.iter_types() {
                    if generic.contains_inner_ty(inner_ty) {
                        return true;
                    }
                }

                false
            }

            Ty::Pointer(ty)
            | Ty::Array(ty, _)
            | Ty::UnknownStructureMember(ty, _)
            | Ty::UnknownStructureMethod(ty, _)
            | Ty::UnknownMethodArgument(ty, _, _)
            | Ty::UnknownArrayMember(ty) => ty.contains_inner_ty(inner_ty),

            Ty::Generic(..) => false,
        }
    }

    pub fn contains_generic(&self) -> bool {
        self.contains_ty(&Ty::Generic("DOES_NOT_MATTER".into()))
    }

    pub fn contains_unknown_any(&self) -> bool {
        let tmp_ty = Ty::CompoundType(InnerTy::Void, Generics::new());
        let tmp_str: String = "".into();

        self.contains_inner_ty(&InnerTy::Unknown(tmp_str.clone()))
            | self.contains_unknown_ident()
            | self.contains_unknown_int()
            | self.contains_unknown_float()
            | self.contains_ty(&Ty::UnknownStructureMember(
                Box::new(tmp_ty.clone()),
                tmp_str.clone(),
            ))
            | self.contains_ty(&Ty::UnknownStructureMethod(
                Box::new(tmp_ty.clone()),
                tmp_str.clone(),
            ))
            | self.contains_ty(&Ty::UnknownMethodArgument(
                Box::new(tmp_ty.clone()),
                tmp_str,
                Either::Right(0),
            ))
            | self.contains_ty(&Ty::UnknownArrayMember(Box::new(tmp_ty)))
    }

    pub fn contains_unknown_int(&self) -> bool {
        self.contains_inner_ty(&InnerTy::UnknownInt("".into(), 0))
    }

    pub fn contains_unknown_float(&self) -> bool {
        self.contains_inner_ty(&InnerTy::UnknownFloat("".into()))
    }

    pub fn contains_unknown_ident(&self) -> bool {
        self.contains_inner_ty(&InnerTy::UnknownIdent("".into(), 0))
    }

    pub fn contains_unknown_array_member(&self) -> bool {
        let tmp_ty = Ty::CompoundType(InnerTy::Void, Generics::new());
        self.contains_ty(&Ty::UnknownArrayMember(Box::new(tmp_ty)))
    }

    // TODO: Currently aggregated types with different inner types will return
    //       true. Should this be the case?
    pub fn is_compatible(&self, other: &Ty) -> bool {
        // Handles all cases with "Unknown" types.
        if self.is_unknown_int() {
            if other.is_unknown_float() || other.is_unknown_ident() {
                return false;
            } else if other.is_int() || other.is_unknown_any() || other.is_generic() {
                return true;
            }
        } else if other.is_unknown_int() {
            if self.is_unknown_float() || self.is_unknown_ident() {
                return false;
            } else if self.is_int() || self.is_unknown_any() || self.is_generic() {
                return true;
            }
        } else if self.is_unknown_float() {
            if other.is_unknown_int() || other.is_unknown_ident() {
                return false;
            } else if other.is_float() || other.is_unknown_any() || other.is_generic() {
                return true;
            }
        } else if other.is_unknown_float() {
            if self.is_unknown_int() || self.is_unknown_ident() {
                return false;
            } else if self.is_float() || self.is_unknown_any() || self.is_generic() {
                return true;
            }
        } else if self.is_unknown_any()
            || other.is_unknown_any()
            || self.is_generic()
            || other.is_generic()
        {
            return true;
        }

        // Handles all cases regarding types that isn't "Unknown" or generic.
        match (self, other) {
            (Ty::Pointer(inner_a), Ty::Pointer(inner_b))
            | (Ty::Array(inner_a, ..), Ty::Array(inner_b, ..)) => inner_a.is_compatible(inner_b),

            (Ty::CompoundType(comp_a, gens_a), Ty::CompoundType(comp_b, gens_b)) => {
                comp_a == comp_b && gens_a.len() == gens_b.len()
            }

            _ => false,
        }
    }

    /// Checks the precedence for the two given types when it comes to mapping
    /// during type inference. If this function return true, it means that
    /// `self` has precedence and that `other` should be mapped to `self` during
    /// type inference.
    pub fn precedence(&self, other: &Self) -> bool {
        // How many "layers" down the search will go when look at nested types.
        // After this number of iterations it will give up and return the current
        // highest precedence number.
        static MAX_DEPTH: usize = 10;

        // TODO: Makes this in a more effective way so that one doesn't have to
        //       re-calculate the "1..i" every iteration.
        // Start by jsut looking at the outer most types and then interatively
        // look deeper to find differences.
        for i in 1..=MAX_DEPTH {
            let self_prec = self.precedence_priv(0, i);
            let other_prec = other.precedence_priv(0, i);

            if self_prec != other_prec {
                return self_prec < other_prec;
            }
        }

        // Give up and default to giving `self` precedence.
        true
    }

    /// Returns a number indicating the precedence for the type when it comes
    /// to the mapping of types during type inference. A lower number indicates
    /// that this type is strongly prefered while a higner number is the opposite.
    /// So the type with the higher number should be mapped to the the with the
    /// lower number.
    ///
    /// The precedence number that is returned is the type that is found in the
    /// given `self` recursively with the highest precedence. This includes the
    /// generics.
    ///
    /// Examples:
    ///   i32<UnknownIdent>       =>  2
    ///   ExampleStruct<i32, T>   =>  6 (where T is generic)
    ///
    /// Precedence:
    ///   0  contains primitive
    ///   1  contains structure (struct/enum/interface)
    ///   2  contains unknown structure (UnknownIdent)
    ///      contains unknown structure member (UnknownStructureMember)
    ///      contains unknown structure method (UnknownStructureMethod)
    ///      contains unknown structure method argument (UnknownMethodArgument)
    ///   3  contains pointer
    ///      contains array
    ///   4  contains unknown int (UnknownInt)
    ///      contains unknown float (UnknownFloat)
    ///   5  contains unknown array member (UnknownArrayMember)
    ///   6  contains generic (Generic)
    ///   7  contains unknown (Unknown)
    fn precedence_priv(&self, mut highest: usize, depth: usize) -> usize {
        if depth == 0 {
            return highest;
        }

        let next_depth = depth - 1;

        match self {
            Ty::CompoundType(inner_ty, generics) => {
                for generic in generics.iter_types() {
                    // The generics are treated as being at the same depth as
                    // the current type.
                    highest = generic.precedence_priv(highest, depth);
                }

                if inner_ty.is_primitive() {
                    usize::max(highest, 0)
                } else if inner_ty.is_structure() {
                    usize::max(highest, 1)
                } else {
                    match inner_ty {
                        InnerTy::UnknownIdent(..) => usize::max(highest, 2),
                        InnerTy::UnknownInt(..) | InnerTy::UnknownFloat(_) => {
                            usize::max(highest, 4)
                        }
                        InnerTy::Unknown(..) => usize::max(highest, 7),
                        _ => unreachable!("All other inner types already \"matched\"."),
                    }
                }
            }

            Ty::Pointer(ty) | Ty::Array(ty, _) => {
                highest = usize::max(highest, 3);
                ty.precedence_priv(highest, next_depth)
            }

            Ty::Generic(_) => usize::max(highest, 6),

            Ty::UnknownStructureMember(ty, _)
            | Ty::UnknownStructureMethod(ty, _)
            | Ty::UnknownMethodArgument(ty, _, _) => {
                highest = usize::max(highest, 2);
                ty.precedence_priv(highest, next_depth)
            }

            Ty::UnknownArrayMember(ty) => {
                highest = usize::max(highest, 5);
                ty.precedence_priv(highest, next_depth)
            }
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();

        match self {
            Ty::CompoundType(inner_ty, generics) => {
                result.push_str(&inner_ty.to_string());

                if !generics.is_empty() {
                    result.push('<');

                    let generic_string = generics
                        .iter_types()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(",");
                    result.push_str(&generic_string);

                    result.push('>');
                }
            }

            Ty::Pointer(inner_ty) => {
                result.push('{');
                result.push_str(&inner_ty.to_string());
                result.push('}');
            }

            Ty::Array(inner_ty, dim_opt) => {
                panic!("TODO: `to_string` for array type.")
                /*
                result.push('[');
                result.push_str(&ty.to_string());

                if let Some(dim) = dim_opt {
                    result.push(':');
                    result.push_str(&dim.as_ref().to_string());
                }
                result.push(']');
                */
            }

            _ => unreachable!("Invalid type when calling `to_string` on type: {:?}", self),
        }

        write!(f, "{}", result)
    }
}
