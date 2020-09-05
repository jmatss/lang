use crate::TypeAnalyzer;
use common::types::{Type, TypeStruct};

pub enum TypeChoice {
    First,
    Second,
}

/// Tries to decide which type of the two given arguments to use.
pub fn unify(
    first_opt: Option<&TypeStruct>,
    second_opt: Option<&TypeStruct>,
) -> Option<TypeChoice> {
    if let Some(first) = first_opt {
        if let Some(second) = second_opt {
            if !first.is_inferred && !second.is_inferred {
                None
            } else if !first.is_inferred {
                Some(TypeChoice::First)
            } else if !second.is_inferred {
                Some(TypeChoice::Second)
            } else {
                None
            }
        } else {
            Some(TypeChoice::First)
        }
    } else if second_opt.is_some() {
        Some(TypeChoice::Second)
    } else {
        None
    }
}

impl<'a> TypeAnalyzer<'a> {
    /// Used for integer and float promotion. Compares two "TypeStruct"s and
    /// returns the type that should be used if the two types are used in a
    /// binary operation. This only returns Some if the two types are either
    /// integers or floats.
    pub fn compare_type(&self, first: &TypeStruct, second: &TypeStruct) -> Option<TypeChoice> {
        // TODO: Generics?
        // TODO: Implement for more types.
        match first.ty {
            Type::Pointer(_) => None,
            Type::Array(_, _) => None,
            Type::Void => None,
            Type::Character => None,
            Type::String => None,
            Type::Boolean => None,
            Type::Int | Type::Uint => match second.ty {
                Type::Uint
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32 => Some(TypeChoice::First),

                Type::Int
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64
                | Type::F64
                | Type::I128
                | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::I8 | Type::U8 => match second.ty {
                Type::U8 => Some(TypeChoice::First),

                Type::I8
                | Type::Int
                | Type::Uint
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64
                | Type::F64
                | Type::I128
                | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::I16 | Type::U16 => match second.ty {
                Type::I8 | Type::U8 => Some(TypeChoice::First),

                Type::Int
                | Type::Uint
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64
                | Type::F64
                | Type::I128
                | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::I32 | Type::U32 => match second.ty {
                Type::Int | Type::Uint | Type::I8 | Type::U8 | Type::I16 | Type::U16 => {
                    Some(TypeChoice::First)
                }

                Type::I32
                | Type::U32
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64
                | Type::F64
                | Type::I128
                | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::I64 | Type::U64 => match second.ty {
                Type::Int
                | Type::Uint
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float
                | Type::F32 => Some(TypeChoice::First),

                Type::I64 | Type::U64 | Type::F64 | Type::I128 | Type::U128 => {
                    Some(TypeChoice::Second)
                }

                Type::Custom(_) => None,
                _ => None,
            },
            Type::I128 | Type::U128 => match second.ty {
                Type::Int
                | Type::Uint
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64
                | Type::F64 => Some(TypeChoice::First),

                Type::I128 | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::F32 | Type::Float => match second.ty {
                Type::Int
                | Type::Uint
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float => Some(TypeChoice::First),

                Type::F32 | Type::I64 | Type::U64 | Type::F64 | Type::I128 | Type::U128 => {
                    Some(TypeChoice::Second)
                }

                Type::Custom(_) => None,
                _ => None,
            },
            Type::F64 => match second.ty {
                Type::Int
                | Type::Uint
                | Type::I8
                | Type::U8
                | Type::I16
                | Type::U16
                | Type::I32
                | Type::U32
                | Type::Float
                | Type::F32
                | Type::I64
                | Type::U64 => Some(TypeChoice::First),

                Type::F64 | Type::I128 | Type::U128 => Some(TypeChoice::Second),

                Type::Custom(_) => None,
                _ => None,
            },
            Type::Custom(_) => None,
        }
    }
}
