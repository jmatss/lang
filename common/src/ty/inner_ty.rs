use crate::BlockId;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InnerTy {
    Struct(String),
    Enum(String),
    Trait(String),

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

    /// This is used during the type inference stage. Every expr must have a type,
    /// so this is used for expr that doesn't have a known type. The string will
    /// be a arbitrary unique ID.
    Unknown(String),

    /// This is used during the type inference stage. This type will be given to
    /// found identifier type. This can be ex. the name of a struct/interface/enum.
    /// The block id is used to find the structure in the correct scope.
    UnknownIdent(String, BlockId),

    /// A int type used during type inferece. It is known that it is of a int
    /// type, but the bit size isn't known.
    /// The first String is the unique ID and the second u32 is the radix.
    UnknownInt(String, u32),

    /// A float type used during type inferece. It is known that it is of a float
    /// type, but the bit size isn't known.
    UnknownFloat(String),
}

#[allow(clippy::match_like_matches_macro)]
impl InnerTy {
    pub fn default_int() -> InnerTy {
        InnerTy::I32
    }

    pub fn default_float() -> InnerTy {
        InnerTy::F32
    }

    pub fn ident_to_type(s: &str, id: BlockId) -> Self {
        match s {
            "void" => InnerTy::Void,
            "char" => InnerTy::Character,
            "String" => InnerTy::String,
            "bool" => InnerTy::Boolean,
            "i8" => InnerTy::I8,
            "u8" => InnerTy::U8,
            "i16" => InnerTy::I16,
            "u16" => InnerTy::U16,
            "i32" => InnerTy::I32,
            "u32" => InnerTy::U32,
            "f32" => InnerTy::F32,
            "i64" => InnerTy::I64,
            "u64" => InnerTy::U64,
            "f64" => InnerTy::F64,
            "i128" => InnerTy::I128,
            "u128" => InnerTy::U128,
            _ => InnerTy::UnknownIdent(s.to_string(), id),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            InnerTy::I8
            | InnerTy::U8
            | InnerTy::I16
            | InnerTy::U16
            | InnerTy::I32
            | InnerTy::U32
            | InnerTy::I64
            | InnerTy::U64
            | InnerTy::I128
            | InnerTy::U128
            | InnerTy::UnknownInt(..) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            InnerTy::F32 | InnerTy::F64 | InnerTy::UnknownFloat(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        if let InnerTy::Boolean = self {
            true
        } else {
            false
        }
    }

    pub fn is_char(&self) -> bool {
        if let InnerTy::Character = self {
            true
        } else {
            false
        }
    }

    pub fn is_string(&self) -> bool {
        if let InnerTy::String = self {
            true
        } else {
            false
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            InnerTy::Void
            | InnerTy::Character
            | InnerTy::String
            | InnerTy::Boolean
            | InnerTy::I8
            | InnerTy::U8
            | InnerTy::I16
            | InnerTy::U16
            | InnerTy::I32
            | InnerTy::U32
            | InnerTy::F32
            | InnerTy::I64
            | InnerTy::U64
            | InnerTy::F64
            | InnerTy::I128
            | InnerTy::U128 => true,
            _ => false,
        }
    }

    /// Checks if this InnerTy is a structure i.e. a struct, enum or interface.
    /// It does NOT check if this is a UnknownIdent.
    pub fn is_structure(&self) -> bool {
        match self {
            InnerTy::Struct(_) | InnerTy::Enum(_) | InnerTy::Trait(_) => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        if let InnerTy::Unknown(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_ident(&self) -> bool {
        if let InnerTy::UnknownIdent(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_int(&self) -> bool {
        if let InnerTy::UnknownInt(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_float(&self) -> bool {
        if let InnerTy::UnknownFloat(..) = self {
            true
        } else {
            false
        }
    }

    pub fn contains_inner_ty(&self, inner_ty: &InnerTy) -> bool {
        match (self, inner_ty) {
            (InnerTy::Struct(_), InnerTy::Struct(_))
            | (InnerTy::Enum(_), InnerTy::Enum(_))
            | (InnerTy::Trait(_), InnerTy::Trait(_))
            | (InnerTy::Void, InnerTy::Void)
            | (InnerTy::Character, InnerTy::Character)
            | (InnerTy::String, InnerTy::String)
            | (InnerTy::Boolean, InnerTy::Boolean)
            | (InnerTy::I8, InnerTy::I8)
            | (InnerTy::U8, InnerTy::U8)
            | (InnerTy::I16, InnerTy::I16)
            | (InnerTy::U16, InnerTy::U16)
            | (InnerTy::I32, InnerTy::I32)
            | (InnerTy::U32, InnerTy::U32)
            | (InnerTy::F32, InnerTy::F32)
            | (InnerTy::I64, InnerTy::I64)
            | (InnerTy::U64, InnerTy::U64)
            | (InnerTy::F64, InnerTy::F64)
            | (InnerTy::I128, InnerTy::I128)
            | (InnerTy::U128, InnerTy::U128)
            | (InnerTy::Unknown(_), InnerTy::Unknown(_))
            | (InnerTy::UnknownIdent(..), InnerTy::UnknownIdent(..))
            | (InnerTy::UnknownInt(_, _), InnerTy::UnknownInt(_, _))
            | (InnerTy::UnknownFloat(_), InnerTy::UnknownFloat(_)) => true,
            _ => false,
        }
    }
}

impl Display for InnerTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = match self {
            InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Trait(ident) => ident,
            InnerTy::Void => "void",
            InnerTy::Character => "char",
            InnerTy::String => "String",
            InnerTy::Boolean => "bool",
            InnerTy::I8 => "i8",
            InnerTy::U8 => "u8",
            InnerTy::I16 => "i16",
            InnerTy::U16 => "u16",
            InnerTy::I32 => "i32",
            InnerTy::U32 => "u32",
            InnerTy::F32 => "f32",
            InnerTy::I64 => "i64",
            InnerTy::U64 => "u64",
            InnerTy::F64 => "f64",
            InnerTy::I128 => "i128",
            InnerTy::U128 => "u128",
            _ => unreachable!(
                "Invalid type when calling `to_string` on inner type: {:?}",
                self
            ),
        };

        write!(f, "{}", result)
    }
}
