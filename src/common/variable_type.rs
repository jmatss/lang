// TODO: Make Int & Uint unbounded? But then what about pointer sized int?

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Character,
    String,
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
    Unknown(String),
}

impl Type {
    pub fn identifier_to_type(s: &str) -> Self {
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
            _ => Type::Unknown(s.to_string()),
        }
    }
}
