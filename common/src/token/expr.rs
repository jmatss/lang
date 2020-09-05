use super::{
    lit::Lit,
    op::{BinOperator, Op, UnOperator},
    stmt::Modifier,
};
use crate::{types::GenericableType, BlockId};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Lit(Lit, Option<GenericableType>),

    // TODO: Is it ok to have type as a expression? This lets one handle binary
    //       operators like ex. "as" in a simple way.
    Type(GenericableType),

    // TODO: FIXME: The "Variable" struct contains a type. Should the type be
    //              in this enum instead?
    Var(Var),
    FuncCall(FuncCall),
    StructInit(StructInit),
    ArrayInit(Vec<Argument>, Option<GenericableType>),
    //MacroCall(Option<MacroCall>),
    Op(Op),
}

impl Expr {
    pub fn is_var(&self) -> bool {
        match self {
            Expr::Var(_) => true,
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => match bin_op.operator {
                    BinOperator::Dot => bin_op.left.is_var() && bin_op.right.is_var(),
                    _ => false,
                },
                Op::UnOp(un_op) => match un_op.operator {
                    UnOperator::Deref | UnOperator::Address | UnOperator::ArrayAccess(_) => true,
                    _ => false,
                },
            },
            _ => false,
        }
    }

    pub fn get_access_type(&self) -> Option<AccessType> {
        match self {
            Expr::Var(_) => Some(AccessType::Regular),
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => match bin_op.operator {
                    BinOperator::Dot => {
                        if bin_op.right.is_deref() {
                            Some(AccessType::Deref)
                        } else if bin_op.right.is_address() {
                            Some(AccessType::Address)
                        } else if bin_op.right.is_array_access() {
                            Some(AccessType::ArrayAccess)
                        } else if bin_op.left.is_var() && bin_op.right.is_var() {
                            Some(AccessType::Regular)
                        } else {
                            panic!("TODO: lhs & rhs isn't both var, might be func call.");
                        }
                    }
                    _ => None,
                },
                Op::UnOp(un_op) => match un_op.operator {
                    UnOperator::Deref => Some(AccessType::Deref),
                    UnOperator::Address => Some(AccessType::Address),
                    UnOperator::ArrayAccess(_) => Some(AccessType::ArrayAccess),
                    _ => None,
                },
            },
            _ => None,
        }
    }

    pub fn is_deref(&self) -> bool {
        if let Expr::Op(Op::UnOp(un_op)) = self {
            if let UnOperator::Deref = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_address(&self) -> bool {
        if let Expr::Op(Op::UnOp(un_op)) = self {
            if let UnOperator::Address = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_array_access(&self) -> bool {
        if let Expr::Op(Op::UnOp(un_op)) = self {
            if let UnOperator::ArrayAccess(_) = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_struct_access(&self) -> bool {
        if let Expr::Op(Op::BinOp(bin_op)) = self {
            if let BinOperator::Dot = bin_op.operator {
                return bin_op.left.is_var() && bin_op.right.is_var();
            }
        }
        false
    }

    /// If this is a Dot operation, this function will return the variable from
    /// the rhs.
    pub fn eval_to_var(&mut self) -> Option<&mut Var> {
        match self {
            Expr::Var(var) => Some(var),
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => match bin_op.operator {
                    BinOperator::Dot => {
                        if bin_op.left.eval_to_var().is_some() {
                            if bin_op.right.eval_to_var().is_some() {
                                bin_op.right.eval_to_var()
                            } else if bin_op.right.eval_to_func_call().is_some() {
                                // If the rhs is a function call, this is a method
                                // call on a variable. Return the var from the
                                // lhs.
                                bin_op.left.eval_to_var()
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                Op::UnOp(un_op) => match un_op.operator {
                    // TODO: This returns the variable that this un op is applied
                    // on and not the variable resulting from the addr/deref op.
                    UnOperator::Deref | UnOperator::Address | UnOperator::ArrayAccess(_) => {
                        un_op.value.eval_to_var()
                    }
                    _ => None,
                },
            },
            _ => None,
        }
    }

    /// Returns Some if this expr is a function call or if this is a binary Dot
    /// operation where the rhs evals to a function call.
    pub fn eval_to_func_call(&mut self) -> Option<&mut FuncCall> {
        match self {
            Expr::FuncCall(func_call) => Some(func_call),
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => match bin_op.operator {
                    BinOperator::Dot => {
                        if bin_op.right.eval_to_func_call().is_some() {
                            bin_op.right.eval_to_func_call()
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }
}

// TODO: Rethink this approach of using "AccessType"s, should not do it.
//       This is because sometimes it is just ignored, as for example with
//       "StructAccess" since that is handled in another way.
#[derive(Debug)]
pub enum AccessType {
    Regular,
    Deref,
    Address,
    // StructAccess, Struct access is currently handled in other ways
    ArrayAccess,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
    pub ret_type: Option<GenericableType>,
    pub modifiers: Option<Vec<Modifier>>,
    pub is_const: bool,

    /// If `access_instrs` is set, access this Variable is NOT a "basic" variable
    /// that can be accessed directly. It will either be a struct member,
    /// some sort of deref/address/array access or a combination of them. This
    /// will require extra instructions when accessing the contents of the variable
    /// that will be "reproduced" when generating the code to access this var.
    /// The "RootVariable" is the "root" variable that will is the entry point
    /// and to get this specific self "variable" instace, one will apply
    /// the "AccessInstruction"s on it.
    pub access_instrs: Option<(RootVariable, Vec<AccessInstruction>)>,
}

impl Var {
    pub fn new(
        name: String,
        ret_type: Option<GenericableType>,
        modifiers: Option<Vec<Modifier>>,
        is_const: bool,
    ) -> Self {
        Var {
            name,
            ret_type,
            modifiers,
            is_const,
            access_instrs: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub name: String,
    pub arguments: Vec<Argument>,

    /// See the `access_instrs` in "Variable" for a explanation.
    pub access_instrs: Option<(RootVariable, Vec<AccessInstruction>)>,

    /// Will be set if this function call is a method call. This variable will
    /// be set to the name/struct ... that this method belongs to.
    pub struct_name: Option<String>,
}

impl FuncCall {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        Self {
            name,
            arguments,
            access_instrs: None,
            struct_name: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    pub name: String,
    pub arguments: Vec<Argument>,
}

impl StructInit {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        Self { name, arguments }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    // Named used for named arguments.
    pub name: Option<String>,
    pub value: Expr,
}

impl Argument {
    pub fn new(name: Option<String>, value: Expr) -> Self {
        Argument { name, value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessInstruction {
    /// The first string is the name of the struct that this var is a member of.
    /// The second string is the name of this member/variable and the u32 is the
    /// position/index of this var in the struct.
    StructMember(String, String, Option<u32>),

    Deref,
    Address,

    /// The expression if the index of the array to access.
    ArrayAccess(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RootVariable {
    pub name: String,
    pub decl_block_id: BlockId,
    pub is_struct: bool,
}

impl RootVariable {
    pub fn new(name: String, decl_block_id: BlockId, is_struct: bool) -> Self {
        Self {
            name,
            decl_block_id,
            is_struct,
        }
    }
}
