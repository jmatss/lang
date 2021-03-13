use super::{expr::Expr, stmt::Stmt};
use crate::{file::FilePosition, ty::ty::Ty, BlockId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Op {
    BinOp(BinOp),
    UnOp(UnOp),
}

impl Op {
    pub fn file_pos(&self) -> Option<FilePosition> {
        match self {
            Op::BinOp(bin_op) => bin_op.file_pos.to_owned(),
            Op::UnOp(un_op) => un_op.file_pos.to_owned(),
        }
    }
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

    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,

    /* NUMBERS */
    Add,
    Sub,
    Mul,
    Div,
    Mod,
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

    /// Will represent a re-written "is" bin op for a union member. The String is
    /// the name of the member that is being accessed. The statement represents
    /// the lhs variable declaration.
    UnionIs(String, Box<Stmt>),

    /// The string is the name of the member. The u64 is the index of the ADT
    /// member being accessed.
    AdtAccess(String, Option<u64>),

    /// The string is the name of the member. The BlockId is the block ID in
    /// which this enum access was done. This is needed to find the enum ADT
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
