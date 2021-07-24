#[derive(Debug, Clone)]
pub enum Type {
    Struct(String, Vec<Type>),
    Enum(String, usize),
    Union(String, Vec<Type>),

    Pointer(Box<Type>),
    Array(Box<Type>, Option<u32>),
    Func(Vec<Type>, Box<Type>),

    Void,
    Character,
    // TODO: How should strings be handled?
    String,
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
}
