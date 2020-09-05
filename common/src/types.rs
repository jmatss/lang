use crate::token::expr::Expr;

/// A type that can contain generics. All types can contain generics.
#[derive(Debug, Clone, PartialEq)]
pub enum GenericableType {
    /// The first Type is the actual type. The second vector of GenericableType
    /// will be set if this type contains generics.
    Type(Type, Option<Vec<GenericableType>>),

    /// A type that is to be inferred. This is used during the type inference
    /// stage. Every expr must have a type, so this is used for expr that doesn't
    /// have a known type. The int will be a unique number.
    Unknown(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer(Box<GenericableType>),
    // The Option in the "Array" enum indicates the size. If it is None, assume
    // size is unknown (probably slice).
    Array(Box<GenericableType>, Option<Box<Expr>>),
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
    Custom(String),
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
            | Type::U128 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Type::F32 | Type::F64 => true,
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
