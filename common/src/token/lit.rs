#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    StringLiteral(String),
    CharLiteral(String),
    Bool(bool),
    Integer(String, u32), // u32 => radix
    Float(String),
}
