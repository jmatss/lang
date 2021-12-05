use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    /// The string is the name of the ADT that is being referenced.
    Adt(String),
    /// The string is the name of the function that is being referenced.
    Func(String),

    Pointer(Box<Type>),
    Array(Box<Type>, Option<u32>),

    /// The type of a function pointer. The vector are the parameter types and
    /// the boxed type is the return type.
    FuncPointer(Vec<Type>, Box<Type>),

    Void,
    Char,
    Bool,
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
}

impl Type {
    pub fn is_number(&self) -> bool {
        self.is_int() || self.is_float()
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
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
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128
        )
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Adt(name) => write!(f, "{}", name),
            Self::Func(name) => write!(f, "fn({})", name),
            Self::Pointer(type_i) => write!(f, "{{{:?}}}", type_i),
            Self::Array(type_i, Some(dim)) => write!(f, "[{:?}: {}]", type_i, dim),
            Self::Array(type_i, None) => write!(f, "[{:?}]", type_i),
            Self::FuncPointer(params, ret) => {
                let params_string = params
                    .iter()
                    .map(|param| format!("{:?}", param))
                    .collect::<Vec<_>>()
                    .join(",");
                let ret_string = if matches!(**ret, Type::Void) {
                    "".into()
                } else {
                    format!(" -> {:?}", ret)
                };
                write!(f, "fn_ptr({}{})", params_string, ret_string)
            }
            Self::Void => write!(f, "void"),
            Self::Char => write!(f, "char"),
            Self::Bool => write!(f, "bool"),
            Self::I8 => write!(f, "i8"),
            Self::U8 => write!(f, "u8"),
            Self::I16 => write!(f, "i16"),
            Self::U16 => write!(f, "u16"),
            Self::I32 => write!(f, "i32"),
            Self::U32 => write!(f, "u32"),
            Self::F32 => write!(f, "f32"),
            Self::I64 => write!(f, "i64"),
            Self::U64 => write!(f, "u64"),
            Self::F64 => write!(f, "f64"),
            Self::I128 => write!(f, "i128"),
            Self::U128 => write!(f, "u128"),
        }
    }
}
