#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Lit {
    String(String, StringType),
    Char(String),
    Bool(bool),
    Integer(String, u32), // u32 => radix
    Float(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum StringType {
    Regular,
    C,
    F,
    S,
}
