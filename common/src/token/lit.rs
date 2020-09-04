#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    String(String),
    Char(String),
    Bool(bool),
    Integer(String, u32), // u32 => radix
    Float(String),
}
