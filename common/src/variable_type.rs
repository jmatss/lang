// TODO: Make Int & Uint unbounded? But then what about pointer sized int?

use crate::token::expr::Expression;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStruct {
    // None == void
    pub ty: Type,
    pub generics: Option<Vec<TypeStruct>>,
    pub is_inferred: bool,
}

impl TypeStruct {
    pub fn new(t: Type, generics: Option<Vec<TypeStruct>>, is_inferred: bool) -> Self {
        TypeStruct {
            ty: t,
            generics,
            is_inferred,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer(Box<TypeStruct>),
    // The Option in the "Array" enum indicates the size. If it is None, assume
    // size is unknown (probably slice).
    Array(Box<TypeStruct>, Option<Box<Expression>>),
    Void,
    Character,
    String, // TODO: String type (?)
    Boolean,
    Int,
    Uint,
    Float,
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
    Custom(String),
}

impl Type {
    pub fn ident_to_type(s: &str) -> Self {
        match s {
            "void" => Type::Void,
            "char" => Type::Character,
            "String" => Type::String,
            "bool" => Type::Boolean,
            "int" => Type::Int,
            "uint" => Type::Uint,
            "float" => Type::Float,
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

    pub fn is_int(&self) -> bool {
        match self {
            Type::Int
            | Type::Uint
            | Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::I128
            | Type::U128 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::Float | Type::F32 | Type::F64 => true,
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
}