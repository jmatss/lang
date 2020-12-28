use super::expr::Expr;
use crate::{file::FilePosition, ty::ty::Ty, BlockId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Op {
    BinOp(BinOp),
    UnOp(UnOp),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinOp {
    pub operator: BinOperator,
    pub ret_type: Option<Ty>,
    pub file_pos: Option<FilePosition>,
    pub is_const: bool,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl BinOp {
    pub fn new(
        operator: BinOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        file_pos: Option<FilePosition>,
    ) -> Self {
        BinOp {
            operator,
            ret_type: None,
            file_pos,
            is_const: false,
            lhs,
            rhs,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnOp {
    pub operator: UnOperator,
    pub ret_type: Option<Ty>,
    pub file_pos: Option<FilePosition>,
    pub is_const: bool,
    pub value: Box<Expr>,
}

impl UnOp {
    pub fn new(operator: UnOperator, value: Box<Expr>, file_pos: Option<FilePosition>) -> Self {
        UnOp {
            operator,
            ret_type: None,
            file_pos,
            is_const: false,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOperator {
    /* GENERAL */
    // Used in for loops etc.
    // TODO: Maybe make keyword instead and only allow use it in "for" loops.
    In,
    // pattern matching
    Is,
    // cast
    As,
    Of,
    Range,
    RangeInclusive,

    // Access function/fields ex. list.add(), tuple.0
    Dot,
    // Access static method.
    DoubleColon,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,

    /* NUMBERS */
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    /* NUMBERS (BIT) */
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    /* BOOL */
    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOperator {
    // Dereference pointer and take address of value (.* and .&).
    Deref,
    Address,

    // ex: (+a + -b)  =>  { Positive(a) + Negative(b) }
    Positive,
    Negative,

    // TODO: Slice/slicing.
    /// The expression is the dimension.
    ArrayAccess(Box<Expr>),

    /// The string is the name of the member. The u64 is the index of the struct
    /// member being accessed.
    StructAccess(String, Option<u64>),

    /// The string is the name of the member. The BlockId is the block ID in
    /// which this enum access was done. This is needed to find the enum structure
    /// during code generation.
    EnumAccess(String, BlockId),

    BitComplement,

    BoolNot,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignOperator {
    Assignment, // "Normal" assignment ("x = y").
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,

    AssignBitAnd,
    AssignBitOr,
    AssignBitXor,
    AssignShl,
    AssignShr,
}
