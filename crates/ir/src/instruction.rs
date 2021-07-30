use crate::{decl::ty::Type, DataIdx, Val, VarIdx};

#[derive(Debug, Clone)]
pub enum Instr {
    Expr(Val, ExprInstr),
    Stmt(StmtInstr),
    End(EndInstr),
}

#[derive(Debug, Clone)]
pub enum ExprInstr {
    Lit(Type, Lit),

    /// The first String is the name of the function to call and the vector of
    /// `Val`s are the arguments.
    FnCall(String, Vec<Val>),
    /// For `FnPtrCall`, the first `Val` is the temporary variable containing the
    /// function pointer to call and the vector of `Val`s are the arguments.
    FnPtrCall(Val, Vec<Val>),

    Op(Type, Op),
    StructInit(Type, Vec<Val>),

    /// The first `Type` is the union type, the `usize` is the index for the
    /// member to be initialized and `Val` is the init value.
    UnionInit(Type, usize, Val),
}

#[derive(Debug, Clone)]
pub enum StmtInstr {
    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Div(Val, Val),
    Mod(Val, Val),
    BitAnd(Val, Val),
    BitOr(Val, Val),
    BitXor(Val, Val),
    Shl(Val, Val),
    Shr(Val, Val),
}

#[derive(Debug, Clone)]
pub enum EndInstr {
    Return(Option<Val>),
    Exit,

    /// Unconditional branch, the String is the label to branch to.
    Branch(String),

    /// The given `Var` must be of type bool. If `Var` is true, this will branch
    /// to the basic block with the name found in the first `String`. If it is
    /// false, it will branch to the block with the name of the second `String`.
    BranchIf(Val, String, String),

    /// The first `Val` is the value of the expression in the match/switch
    /// statement. The second `String` is the name of the default block to branch
    /// to if no cases matches.
    /// The vector contains tuples of all cases where the `Val` is the case
    /// value/expression and the `String` is the name of the block to branch
    /// to if the case is matched.
    BranchSwitch(Val, String, Vec<(Val, String)>),

    // TODO: This is an EndInstr, should probably be possible to have an
    //       unreachable as a regular stmt instruction as well.
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum Lit {
    String(DataIdx),
    Char(String),
    Bool(bool),
    Integer(String, u32), // u32 => radix
    Float(String),
}

#[derive(Debug, Clone)]
pub enum Op {
    BinOp {
        oper: BinOper,
        lhs: Box<Val>,
        rhs: Box<Val>,
    },
    UnOp {
        oper: UnOper,
        value: Box<Val>,
    },
}

#[derive(Debug, Clone)]
pub enum BinOper {
    As,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    BoolAnd,
    BoolOr,
}

#[derive(Debug, Clone)]
pub enum UnOper {
    Deref,
    Address,

    // TODO: UnionIs
    // TODO: Merge these? Are able to see the difference by looking at the
    //       `Type` inside the `Var`.
    ArrayAccess(usize),
    AdtAccess(usize),
    EnumAccess(usize),

    BitComplement,
    BoolNot,
}
