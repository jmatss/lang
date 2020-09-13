use super::{
    lit::Lit,
    op::{BinOperator, Op, UnOperator},
    stmt::Modifier,
};
use crate::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    types::Type,
    BlockId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Lit(Lit, Option<Type>),

    // TODO: Is it ok to have type as a expression? This lets one handle binary
    //       operators like ex. "as" in a simple way.
    Type(Type),

    // TODO: FIXME: The "Variable" struct contains a type. Should the type be
    //              in this enum instead?
    Var(Var),
    FuncCall(FuncCall),
    StructInit(StructInit),
    ArrayInit(ArrayInit),
    //MacroCall(Option<MacroCall>),
    Op(Op),
}

impl Expr {
    pub fn get_expr_type(&mut self) -> CustomResult<&mut Type> {
        Ok(match self {
            Expr::Lit(_, Some(ty)) => ty,
            Expr::Type(ty) => ty,
            Expr::Var(var) if var.ret_type.is_some() => {
                if let Some(ty) = &mut var.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::FuncCall(func_call) if func_call.ret_type.is_some() => {
                if let Some(ty) = &mut func_call.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::StructInit(struct_init) if struct_init.ret_type.is_some() => {
                if let Some(ty) = &mut struct_init.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::ArrayInit(array_init) if array_init.ret_type.is_some() => {
                if let Some(ty) = &mut array_init.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::Op(Op::BinOp(bin_op)) if bin_op.ret_type.is_some() => {
                if let Some(ty) = &mut bin_op.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::Op(Op::UnOp(un_op)) if un_op.ret_type.is_some() => {
                if let Some(ty) = &mut un_op.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            _ => {
                return Err(LangError::new(
                    "Unable to get type of expr.".into(),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                ))
            }
        })
    }

    pub fn is_var(&self) -> bool {
        match self {
            Expr::Var(_) => true,
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => match bin_op.operator {
                    BinOperator::Dot => bin_op.lhs.is_var() && bin_op.rhs.is_var(),
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

    pub fn is_deref(&self) -> bool {
        if let Expr::Op(Op::UnOp(un_op)) = self {
            if let UnOperator::Deref = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn contains_deref(&self) -> bool {
        match self {
            Expr::Op(Op::BinOp(bin_op)) => {
                bin_op.lhs.contains_deref() || bin_op.rhs.contains_deref()
            }
            Expr::Op(Op::UnOp(un_op)) => match un_op.operator {
                UnOperator::Deref => true,
                _ => un_op.value.contains_deref(),
            },
            _ => false,
        }
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

    pub fn contains_struct_access(&self) -> bool {
        match self {
            Expr::Op(Op::BinOp(bin_op)) => {
                bin_op.lhs.contains_struct_access() || bin_op.rhs.contains_struct_access()
            }
            Expr::Op(Op::UnOp(un_op)) => match un_op.operator {
                UnOperator::StructAccess(..) => true,
                _ => un_op.value.contains_struct_access(),
            },
            _ => false,
        }
    }

    pub fn is_struct_access(&self) -> bool {
        if let Expr::Op(Op::BinOp(bin_op)) = self {
            if let BinOperator::Dot = bin_op.operator {
                return bin_op.lhs.is_var() && bin_op.rhs.is_var();
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
                        if bin_op.lhs.eval_to_var().is_some() {
                            if bin_op.rhs.eval_to_var().is_some() {
                                bin_op.rhs.eval_to_var()
                            } else if bin_op.rhs.eval_to_func_call().is_some() {
                                // If the rhs is a function call, this is a method
                                // call on a variable. Return the var from the
                                // lhs.
                                bin_op.lhs.eval_to_var()
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
                        if bin_op.rhs.eval_to_func_call().is_some() {
                            bin_op.rhs.eval_to_func_call()
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
    pub ret_type: Option<Type>,
    pub modifiers: Option<Vec<Modifier>>,
    pub is_const: bool,
}

impl Var {
    pub fn new(
        name: String,
        ret_type: Option<Type>,
        modifiers: Option<Vec<Modifier>>,
        is_const: bool,
    ) -> Self {
        Var {
            name,
            ret_type,
            modifiers,
            is_const,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncCall {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Type>,

    // TODO: Is this needed? Currently set in type solver and the used when
    //       renaming the method call in codegen. The codegen could get the
    //       name of the struct from the ret type of the first argument in the
    //       method call, but would be more work.
    /// Will be set if this is a method call i.e. a function being a member of
    /// a struct. The String will be the name of the struct.
    pub method_struct: Option<String>,

    pub is_method: bool,
}

impl FuncCall {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            method_struct: None,
            is_method: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructInit {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Type>,
}

impl StructInit {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayInit {
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Type>,
}

impl ArrayInit {
    pub fn new(arguments: Vec<Argument>) -> Self {
        Self {
            arguments,
            ret_type: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
