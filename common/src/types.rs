use std::{collections::BTreeMap, fmt::Display};

use crate::token::expr::Expr;

// TODO: Implement generics.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// A type that contains generic types. The first boxed type if the actual
    /// type and the vector of types are the generic types. Currently only
    /// structs are allowed to be compound types (the first type only allows
    /// Strings which can only be translated to structs).
    /// Need to use "BTreeMap" instead of "HashMap" to make this "Type" structure
    /// hashable with the default derived "Hash".
    /// The key of the map is the name/identifier of the generic and the value
    /// is the actual type.
    CompoundType(String, BTreeMap<String, Type>),

    Pointer(Box<Type>),
    // The Option in the "Array" enum indicates the size. If it is None, assume
    // size is unknown (probably slice).
    Array(Box<Type>, Option<Box<Expr>>),
    Void,
    Character,
    String, // TODO: String type (?)
    Boolean,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    F32,
    I64,
    U64,
    F64,
    I128,
    U128,

    /// Struct type.
    Custom(String),

    /// A generic type. Ex. a generic "T" on a struct would be represented
    /// as a "Generic" containing the string "T".
    Generic(String),

    /// This is used during the type inference stage. Every expr must have a type,
    /// so this is used for expr that doesn't have a known type. The String will be
    /// a unique ID that will be used when the unknown type is to be "converted"
    /// into a real type at the end of the type inference step.
    /// The unique String will be a "random" number concatenated with the line
    /// number and column numbers where the type was "declared".
    Unknown(String),

    /// A int type used during type inferece. It is known that it is of a int
    /// type, but the bit size isn't known.
    /// The first String is the unique ID and the second u32 is the radix.
    UnknownInt(String, u32),

    /// A float type used during type inferece. It is known that it is of a float
    /// type, but the bit size isn't known.
    UnknownFloat(String),

    /// Unknown member of the struct type "Type" with the member name "String".
    UnknownStructMember(Box<Type>, String),

    /// Unknown method of the struct type "Type" with the name "String".
    UnknownStructMethod(Box<Type>, String),

    /// Unknown method argument of the struct type "Type" with then name "String".
    /// The optional string is the name of the argument if specificed.
    /// The u64 is the index of the argument in the function call.
    UnknownMethodArgument(Box<Type>, String, Option<String>, u64),

    /// Unknown type of array member of array with type "Type".
    UnknownArrayMember(Box<Type>),
}

impl Type {
    pub fn ident_to_type(s: &str) -> Self {
        match s {
            "void" => Type::Void,
            "char" => Type::Character,
            "String" => Type::String,
            "bool" => Type::Boolean,
            "i8" => Type::I8,
            "u8" => Type::U8,
            "i16" => Type::I16,
            "u16" => Type::U16,
            "i32" => Type::I32,
            "u32" => Type::U32,
            "f32" => Type::F32,
            "i64" => Type::I64,
            "u64" => Type::U64,
            "f64" => Type::F64,
            "i128" => Type::I128,
            "u128" => Type::U128,
            _ => Type::Custom(s.to_string()),
        }
    }

    /// Recursively replaces any generic identifiers from "Custom" to "Generic".
    pub fn replace_generics(&mut self, generics: &[String]) {
        match self {
            Type::CompoundType(_, gens) => {
                for gen in gens.values_mut() {
                    gen.replace_generics(generics);
                }
            }
            Type::Pointer(ty) | Type::Array(ty, _) => ty.replace_generics(generics),
            Type::Custom(ident) => {
                if generics.contains(ident) {
                    *self = Type::Generic(ident.clone());
                }
            }
            _ => (),
        }
    }

    /// Recursively replaces any "Generic" types with any matches from the
    /// `generics_impl` map. I.e. any "Generic" type that has a ident that is a
    /// key in the `generics_impl` will be replaced with the value in the map.
    pub fn replace_generics_impl(&mut self, generics_impl: &BTreeMap<String, Type>) {
        match self {
            Type::CompoundType(_, gens) => {
                for gen in gens.values_mut() {
                    gen.replace_generics_impl(generics_impl);
                }
            }
            Type::Pointer(ty) | Type::Array(ty, _) => ty.replace_generics_impl(generics_impl),
            Type::Generic(ident) => {
                if let Some(gen_ty) = generics_impl.get(ident) {
                    *self = gen_ty.clone();
                }
            }
            _ => (),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::I128
            | Type::U128
            | Type::UnknownInt(..) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 | Type::UnknownFloat(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        if let Type::Boolean = self {
            true
        } else {
            false
        }
    }

    pub fn is_char(&self) -> bool {
        if let Type::Character = self {
            true
        } else {
            false
        }
    }

    pub fn is_string(&self) -> bool {
        if let Type::String = self {
            true
        } else {
            false
        }
    }

    pub fn is_aggregated(&self) -> bool {
        match self {
            Type::CompoundType(..) | Type::Pointer(_) | Type::Array(..) | Type::Custom(_) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Void
            | Type::Character
            | Type::String
            | Type::Boolean
            | Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::F32
            | Type::I64
            | Type::U64
            | Type::F64
            | Type::I128
            | Type::U128 => true,
            _ => false,
        }
    }

    pub fn is_generic(&self) -> bool {
        if let Type::Generic(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown(&self) -> bool {
        if let Type::Unknown(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_int(&self) -> bool {
        if let Type::UnknownInt(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_float(&self) -> bool {
        if let Type::UnknownFloat(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_struct_member(&self) -> bool {
        if let Type::UnknownStructMember(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_struct_method(&self) -> bool {
        if let Type::UnknownStructMethod(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_method_argument(&self) -> bool {
        if let Type::UnknownMethodArgument(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_array_member(&self) -> bool {
        if let Type::UnknownArrayMember(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_any(&self) -> bool {
        match self {
            Type::Unknown(_)
            | Type::UnknownInt(..)
            | Type::UnknownFloat(_)
            | Type::UnknownStructMember(..)
            | Type::UnknownStructMethod(..)
            | Type::UnknownMethodArgument(..)
            | Type::UnknownArrayMember(_) => true,
            _ => false,
        }
    }

    // TODO: Currently aggregated types with different inner types will return
    //       true. Should this be the case?
    pub fn is_compatible(&self, other: &Type) -> bool {
        // Handles all cases with "Unknown" types.
        if self.is_unknown_int() {
            if other.is_unknown_float() {
                return false;
            } else if other.is_int() || other.is_unknown_any() || other.is_generic() {
                return true;
            }
        } else if other.is_unknown_int() {
            if self.is_unknown_float() {
                return false;
            } else if self.is_int() || self.is_unknown_any() || self.is_generic() {
                return true;
            }
        } else if self.is_unknown_float() {
            if self.is_unknown_int() {
                return false;
            } else if other.is_float() || other.is_unknown_any() || other.is_generic() {
                return true;
            }
        } else if other.is_unknown_float() {
            if self.is_unknown_int() {
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
            (Type::Pointer(_), Type::Pointer(_))
            | (Type::Array(..), Type::Array(..))
            | (Type::Void, Type::Void)
            | (Type::Character, Type::Character)
            | (Type::String, Type::String)
            | (Type::Boolean, Type::Boolean)
            | (Type::I8, Type::I8)
            | (Type::U8, Type::U8)
            | (Type::I16, Type::I16)
            | (Type::U16, Type::U16)
            | (Type::I32, Type::I32)
            | (Type::U32, Type::U32)
            | (Type::F32, Type::F32)
            | (Type::I64, Type::I64)
            | (Type::U64, Type::U64)
            | (Type::F64, Type::F64)
            | (Type::I128, Type::U128)
            | (Type::U128, Type::U128) => true,
            (Type::Custom(a), Type::Custom(b)) if a == b => true,
            (Type::CompoundType(comp_a, gens_a), Type::CompoundType(comp_b, gens_b)) => {
                comp_a == comp_b && gens_a.len() == gens_b.len()
            }

            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();

        match self {
            Type::CompoundType(ty_string, gen_tys) => {
                result.push_str(ty_string);
                result.push('<');

                let mut gen_tys_strings = Vec::default();
                for gen_ty in gen_tys.keys() {
                    gen_tys_strings.push(gen_ty.clone())
                }
                result.push_str(&gen_tys_strings.join(","));

                result.push('>');
            }
            Type::Pointer(ptr) => {
                result.push('{');
                result.push_str(&ptr.to_string());
                result.push('}');
            }
            Type::Array(ty, dim_opt) => {
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
            Type::Void => result.push_str("void"),
            Type::Character => result.push_str("char"),
            Type::String => result.push_str("String"),
            Type::Boolean => result.push_str("bool"),
            Type::I8 => result.push_str("i8"),
            Type::U8 => result.push_str("u8"),
            Type::I16 => result.push_str("i16"),
            Type::U16 => result.push_str("u16"),
            Type::I32 => result.push_str("i32"),
            Type::U32 => result.push_str("u32"),
            Type::F32 => result.push_str("f32"),
            Type::I64 => result.push_str("i64"),
            Type::U64 => result.push_str("u64"),
            Type::F64 => result.push_str("f64"),
            Type::I128 => result.push_str("i128"),
            Type::U128 => result.push_str("u128"),
            Type::Custom(ident) | Type::Generic(ident) => result.push_str(ident),
            _ => unreachable!("Invalid type when calling `to_string`: {:?}", self),
        }

        write!(f, "{}", result)
    }
}
