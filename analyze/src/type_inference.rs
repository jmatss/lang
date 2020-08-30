use crate::TypeAnalyzer;
use common::variable_type::{Type, TypeStruct};

pub enum TypeChoice {
    First,
    Second,
}

impl<'a> TypeAnalyzer<'a> {
    /// Used for integer and float promotion. Compares two "TypeStruct"s and
    /// returns the type that should be used if the two types are used in a
    /// binary operation. This only returns Some if the two types are either
    /// integers or floats.
    pub fn compare_type(&self, first: &TypeStruct, second: &TypeStruct) -> Option<TypeChoice> {
        // TODO: Generics?
        // TODO: Implement for more types.
        match first.t {
            Type::Pointer(_) => None,
            Type::Array(_, _) => None,
            Type::Void => None,
            Type::Character => None,
            Type::String => None,
            Type::Boolean => None,
            Type::Int | Type::Uint => match second.t {
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
            Type::I8 | Type::U8 => match second.t {
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
            Type::I16 | Type::U16 => match second.t {
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
            Type::I32 | Type::U32 => match second.t {
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
            Type::I64 | Type::U64 => match second.t {
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
            Type::I128 | Type::U128 => match second.t {
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
            Type::F32 | Type::Float => match second.t {
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
            Type::F64 => match second.t {
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
