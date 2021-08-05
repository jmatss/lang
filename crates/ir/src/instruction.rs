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

    FnPtr(String),

    /// Gets the value stored inside the variable with index `VarIdx`.
    Var(VarIdx),

    Op(Type, Op),

    /// Needs to be the first instruction of a basic block. This expr instruction
    /// will be assigned one of the `Val`s inside the vector depending on which
    /// basic block we arrived from.
    ///
    /// The String in the tuple is a label of a basic block, and the Val is the
    /// value that will be assigned to this phi if we arrived from the corresponding
    /// basic block.
    ///
    /// # Example
    ///
    /// If we have a phi-instruction that looks like this at the start of a block:
    /// ```
    /// res = Phi[(Block1, 123), (Block2, 456)]
    /// ```
    /// If the previos block that we came from was `Block1`, the value 123 will
    /// be assigned to `res`. If we arrived from `Block2`, the value 456 will be
    /// assigned to `res`.
    ///
    /// OBS! The phi-instruction must be exhaustive meaning that all possible
    ///      blocks that we can arrive from must have a entry in the phi-instruction.
    Phi(Vec<(String, Val)>),

    /// Casts the value `Val` to the type `Type`.
    Cast(Type, Val),

    Cmp(Cmp, Val, Val, Signed),

    Add(Val, Val),
    Sub(Val, Val),
    Mul(Val, Val),
    Div(Val, Val, Signed),
    Mod(Val, Val, Signed),
    BitAnd(Val, Val),
    BitOr(Val, Val),
    BitXor(Val, Val),
    Shl(Val, Val),
    Shr(Val, Val, Signed),

    Deref(Val),
    Address(Val),

    /// Expectes `Val` to be pointer/address to the integer that is to be
    /// incremented/decremented.
    Inc(Val),
    Dec(Val),

    StructInit(Type, Vec<Val>),

    /// The first `Type` is the union type, the `usize` is the index for the
    /// member to be initialized and `Val` is the init value.
    UnionInit(Type, usize, Val),

    ArrayInit(Type, Vec<Val>),
}

#[derive(Debug, Clone)]
pub enum StmtInstr {}

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

#[derive(Debug, Copy, Clone)]
pub enum Cmp {
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Copy, Clone)]
pub enum Signed {
    True,
    False,
}
