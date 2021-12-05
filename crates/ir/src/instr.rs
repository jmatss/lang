use std::fmt::Debug;

use either::Either;

use crate::{ty::Type, DataIdx, Val, VarIdx};

#[derive(Debug, Clone)]
pub struct Instr(pub Either<ExprInstr, EndInstr>);

#[derive(Clone)]
pub struct ExprInstr {
    pub val: Val,
    pub kind: ExprInstrKind,
}

#[derive(Debug, Clone)]
pub enum ExprInstrKind {
    Lit(Lit),

    /// The first String is the name of the function to call and the vector of
    /// `Val`s are the arguments.
    FnCall(String, Vec<Val>),

    /// For `FnPtrCall`, the first `Val` is the temporary variable containing the
    /// function pointer to call and the vector of `Val`s are the arguments.
    FnPtrCall(Val, Vec<Val>),

    FnPtr(String),

    /// Contains/returns the address of the given variable.
    VarAddress(VarIdx),
    /// Contains/returns the address of the given global data.
    DataAddress(DataIdx),

    /// The first `Val` is the address of the variable and the second `Val` is
    /// the value that will be stored.
    Store(Val, Val),
    /// The `Val` is the address of the variable.
    Load(Val),

    Op(Op),

    /// If used, needs to be the first instruction of a basic block.
    /// This instruction will be assigned one of the `Val`s inside the vector
    /// depending on which basic block we arrived from.
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

    /// The string is the name of the struct and the vector are the init arguments.
    StructInit(String, Vec<Val>),

    /// Creates an array with the length of the vector and the inner type of the
    /// arguments
    ArrayInit(Vec<Val>),
}

#[derive(Clone)]
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
    Char(char),
    Bool(bool),
    Integer(String), // always radix 10.
    Float(String),
}

#[derive(Debug, Clone)]
pub enum Op {
    BinOp {
        oper: BinOper,
        lhs: Box<Val>,
        rhs: Box<Val>,
        signed: Signed,
    },
    UnOp {
        oper: UnOper,
        value: Box<Val>,
    },
}

#[derive(Debug, Copy, Clone)]
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
}

#[derive(Debug, Clone)]
pub enum UnOper {
    /// ADT/enum access. The `usize` is the index that is being
    /// indexed.
    AdtAccess(usize),
    ArrayAccess(Val),

    IsNull,

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

/// Default is false which will be used if we don't care about the signess.
#[derive(Debug, Copy, Clone)]
pub enum Signed {
    True,
    False,
}

impl Debug for ExprInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match &self.kind {
            ExprInstrKind::Lit(lit) => match lit {
                Lit::String(data_idx) => format!("string {}", data_idx.0),
                Lit::Char(char) => format!("char '{}'", char),
                Lit::Bool(bool) => format!("bool {}", bool),
                Lit::Integer(int) => format!("int {}", int),
                Lit::Float(float) => format!("float {}", float),
            },
            ExprInstrKind::FnCall(name, args) => {
                let args_string = args
                    .iter()
                    .map(|val| format!("${}: {:?}", val.0, val.1))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("call {}({})", name, args_string)
            }
            ExprInstrKind::FnPtrCall(fn_ptr_val, args) => {
                let args_string = args
                    .iter()
                    .map(|val| format!("${}: {:?}", val.0, val.1))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_string = if let Type::Void = fn_ptr_val.1 {
                    "".into()
                } else {
                    format!(" -> {:?}", fn_ptr_val.1)
                };
                format!(
                    "call_fn_ptr {}({}){}",
                    fn_ptr_val.0, args_string, ret_string
                )
            }
            ExprInstrKind::FnPtr(name) => {
                format!("fn_ptr {}", name)
            }
            ExprInstrKind::VarAddress(var_idx) => {
                format!("{:?}", var_idx)
            }
            ExprInstrKind::DataAddress(data_idx) => {
                format!("data_address {:?}", data_idx)
            }
            ExprInstrKind::Store(ptr_val, val) => {
                format!("store ${} into ${}", val.0, ptr_val.0,)
            }
            ExprInstrKind::Load(ptr_val) => {
                format!("load ${}", ptr_val.0)
            }
            ExprInstrKind::Op(op) => match op {
                Op::BinOp {
                    oper,
                    lhs,
                    rhs,
                    signed,
                } => match oper {
                    BinOper::As => format!("cast ${} to {:?}", lhs.0, self.val.1),
                    BinOper::Eq => format!("eq ${} ${}", lhs.0, rhs.0),
                    BinOper::Neq => format!("neq ${} ${}", lhs.0, rhs.0),
                    BinOper::Lt => format!("lt ${} ${}", lhs.0, rhs.0),
                    BinOper::Gt => format!("gt ${} ${}", lhs.0, rhs.0),
                    BinOper::Lte => format!("lte ${} ${}", lhs.0, rhs.0),
                    BinOper::Gte => format!("gte ${} ${}", lhs.0, rhs.0),
                    BinOper::Add => format!("add ${} ${}", lhs.0, rhs.0),
                    BinOper::Sub => format!("sub ${} ${}", lhs.0, rhs.0),
                    BinOper::Mul => format!("mul ${} ${}", lhs.0, rhs.0),
                    BinOper::Div if matches!(signed, Signed::True) => {
                        format!("div_s ${} ${}", lhs.0, rhs.0)
                    }
                    BinOper::Div => format!("div_u ${} ${}", lhs.0, rhs.0),
                    BinOper::Mod if matches!(signed, Signed::True) => {
                        format!("mod_s ${} ${}", lhs.0, rhs.0)
                    }
                    BinOper::Mod => format!("mod_u ${} ${}", lhs.0, rhs.0),
                    BinOper::BitAnd => format!("bit_and ${} ${}", lhs.0, rhs.0),
                    BinOper::BitOr => format!("bit_or ${} ${}", lhs.0, rhs.0),
                    BinOper::BitXor => format!("bit_xor ${} ${}", lhs.0, rhs.0),
                    BinOper::ShiftLeft => format!("shift_left ${} ${}", lhs.0, rhs.0),
                    BinOper::ShiftRight if matches!(signed, Signed::True) => {
                        format!("shift_right_s ${} ${}", lhs.0, rhs.0)
                    }
                    BinOper::ShiftRight => format!("shift_right_u ${} ${}", lhs.0, rhs.0),
                },
                Op::UnOp { oper, value } => match oper {
                    UnOper::AdtAccess(idx) => {
                        format!("adt_access ${} idx {}", value.0, idx)
                    }
                    UnOper::ArrayAccess(inner_value) => {
                        format!("array_access ${} idx {}", value.0, inner_value.0)
                    }
                    UnOper::IsNull => format!("is_null ${}", value.0),
                    UnOper::BitComplement => format!("bit_complement ${}", value.0),
                    UnOper::BoolNot => format!("bool_not ${}", value.0),
                },
            },
            ExprInstrKind::Phi(phi_branches) => {
                let branches_string = phi_branches
                    .iter()
                    .map(|(label, val)| format!("({}, ${})", label, val.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("phi [{}]", branches_string)
            }
            ExprInstrKind::StructInit(name, args) => {
                let args_string = args
                    .iter()
                    .map(|val| format!("${}", val.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("struct_init {}({})", name, args_string)
            }
            ExprInstrKind::ArrayInit(args) => {
                let args_string = args
                    .iter()
                    .map(|val| format!("${}", val.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                let type_string =
                    if let Some(ir_type) = args.first().map(|val| format!("{:?}", val.1)) {
                        format!(": [{:?}: {}]", ir_type, args.len())
                    } else {
                        "".into()
                    };
                format!("array_init ({}){}", args_string, type_string)
            }
        };
        write!(f, "${}: {:?} = {}", self.val.0, self.val.1, output)
    }
}

impl Debug for EndInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Self::Return(Some(val)) => format!("return ${}", val.0),
            Self::Return(None) => "return".into(),
            Self::Exit => "exit".into(),
            Self::Branch(label) => format!("branch \"{}\"", label),
            Self::BranchIf(val, label_true, label_false) => {
                format!(
                    "branch_if ${} \"{}\" \"{}\"",
                    val.0, label_true, label_false
                )
            }
            Self::BranchSwitch(val, label_default, cases) => {
                let cases_string = cases
                    .iter()
                    .map(|(val, label)| format!("(${}, \"{}\")", val.0, label))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "branch_switch ${} \"{}\" [{}]",
                    val.0, label_default, cases_string
                )
            }
            Self::Unreachable => "unreachable".into(),
        };
        write!(f, "{}", output)
    }
}
