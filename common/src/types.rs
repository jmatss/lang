use crate::token::expr::Expr;

// TODO: Implement generics.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
    Custom(String),

    /// This is used during the type inference stage. Every expr must have a type,
    /// so this is used for expr that doesn't have a known type. The int will be
    /// a unique number that will be used when ti unknown type is to be "converted"
    /// into a real type at the end of the type inference step.
    Unknown(usize),

    /// A int type used during type inferece. It is known that it is of a int
    /// type, but the bit size isn't known.
    /// The first usize is the unique number and the second u32 is the radix.
    UnknownInt(usize, u32),

    /// A float type used during type inferece. It is known that it is of a float
    /// type, but the bit size isn't known.
    UnknownFloat(usize),

    /// Unknown member of the struct type "Type" with the member name "String".
    UnknownStructMember(Box<Type>, String),

    /// Array access on expr of type "Type" that should be an array type.
    UnknownArrayAccess(Box<Type>),
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
            Type::Pointer(_) | Type::Array(..) | Type::Custom(_) => true,
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

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown(_)
            | Type::UnknownInt(..)
            | Type::UnknownFloat(_)
            | Type::UnknownStructMember(..)
            | Type::UnknownArrayAccess(_) => true,
            _ => false,
        }
    }

    // TODO: Currently aggregated types with different inner types will return
    //       true. Should this be the case?
    pub fn is_compatible(&self, other: &Type) -> bool {
        // Handles all cases with "Unknown" types.
        if let Type::UnknownInt(..) = self {
            if let Type::UnknownFloat(_) = other {
                return false;
            }
            if other.is_int() || other.is_unknown() {
                return true;
            }
        } else if let Type::UnknownInt(..) = other {
            if let Type::UnknownFloat(_) = self {
                return false;
            }
            if self.is_int() || self.is_unknown() {
                return true;
            }
        } else if let Type::UnknownFloat(_) = self {
            if let Type::UnknownInt(..) = self {
                return false;
            }
            if other.is_float() || other.is_unknown() {
                return true;
            }
        } else if let Type::UnknownFloat(_) = other {
            if let Type::UnknownInt(..) = self {
                return false;
            }
            if self.is_float() || self.is_unknown() {
                return true;
            }
        } else if let Type::Unknown(_) = self {
            return true;
        } else if let Type::Unknown(_) = other {
            return true;
        }

        // Handles all cases regarding types that isn't "Unknown".
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

            _ => false,
        }
    }
}
