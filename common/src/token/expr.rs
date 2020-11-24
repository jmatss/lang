use std::hash::Hash;

use super::{
    lit::Lit,
    op::{BinOperator, Op, UnOperator},
    stmt::Modifier,
};
use crate::{
    error::{
        CustomResult, LangError,
        LangErrorKind::{self, AnalyzeError},
    },
    r#type::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    util, BlockId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Lit(Lit, Option<Ty>),

    // TODO: Is it ok to have type as a expression? This lets one handle binary
    //       operators like ex. "as" in a simple way.
    Type(Ty),
    Var(Var),
    FuncCall(FuncCall),
    StructInit(StructInit),
    ArrayInit(ArrayInit),
    //MacroCall(Option<MacroCall>),
    Op(Op),
}

impl Expr {
    pub fn get_expr_type(&self) -> CustomResult<Ty> {
        Ok(match self {
            Expr::Lit(_, Some(ty)) | Expr::Type(ty) => ty.clone(),
            Expr::Var(var) => {
                if let Some(ty) = &var.ret_type {
                    ty.clone()
                } else {
                    return Err(LangError::new(
                        format!("Type of var was None: {:?}", var),
                        AnalyzeError {
                            line_nr: 0,
                            column_nr: 0,
                        },
                    ));
                }
            }
            Expr::FuncCall(func_call) if func_call.ret_type.is_some() => {
                if let Some(ty) = &func_call.ret_type {
                    ty.clone()
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::StructInit(struct_init) if struct_init.ret_type.is_some() => {
                if let Some(ty) = &struct_init.ret_type {
                    ty.clone()
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::ArrayInit(array_init) if array_init.ret_type.is_some() => {
                if let Some(ty) = &array_init.ret_type {
                    ty.clone()
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::Op(Op::BinOp(bin_op)) if bin_op.ret_type.is_some() => {
                if let Some(ty) = &bin_op.ret_type {
                    ty.clone()
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::Op(Op::UnOp(un_op)) if un_op.ret_type.is_some() => {
                if let Some(ty) = &un_op.ret_type {
                    ty.clone()
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

    #[allow(clippy::match_like_matches_macro)]
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
    pub fn eval_to_var(&mut self) -> Option<&Var> {
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
    pub ret_type: Option<Ty>,
    pub modifiers: Option<Vec<Modifier>>,
    pub default_value: Option<Box<Expr>>,
    pub is_const: bool,
}

impl Var {
    pub fn new(
        name: String,
        ret_type: Option<Ty>,
        modifiers: Option<Vec<Modifier>>,
        default_value: Option<Box<Expr>>,
        is_const: bool,
    ) -> Self {
        Var {
            name,
            ret_type,
            modifiers,
            default_value,
            is_const,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncCall {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,

    /// Will be set if the function call "hardcoded" generics. Otherwise the
    /// generics will be fetched from the type of this function call.
    pub generics: Option<Generics>,

    /// Will be set if this is a method call. It will be set to the structure
    /// that this method is called on.
    pub method_structure: Option<Ty>,
    pub is_method: bool,
}

impl FuncCall {
    pub fn new(name: String, arguments: Vec<Argument>, generics: Option<Generics>) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            generics,
            method_structure: None,
            is_method: false,
        }
    }

    /// Returns the generics. If generics was set at the function call, this
    /// function will replace the types of the types parsed during type inference
    /// with type specified at the func call.
    pub fn generics(&mut self) -> Option<&Generics> {
        if let Some(Ty::CompoundType(_, ty_generics)) = &mut self.ret_type {
            Some(ty_generics)
        } else {
            None
        }

        /*
        if let Some(generics) = &self.generics {
            for (self_gen, ty_gen) in generics.iter_types().zip(ty_generics.iter_types_mut()) {
                *ty_gen = self_gen.clone();
            }
        }

        Some(ty_generics)
        */
    }

    /// Returns the "full name" which is the name containing possible structure
    /// and generics as well.
    ///
    /// Format:
    ///   "<STRUCTURE_NAME>:<GENERICS>-<FUNCTION_NAME>"
    pub fn full_name(&mut self) -> CustomResult<String> {
        let generics = if let Some(generics) = self.generics() {
            generics.clone()
        } else {
            Generics::new()
        };

        if let Some(ty) = &self.method_structure {
            let structure_name = if let Ty::CompoundType(inner_ty, _) = ty {
                match inner_ty {
                    InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Interface(ident) => {
                        ident
                    }
                    _ => unreachable!("Method call on non structure type: {:#?}", self),
                }
            } else {
                return Err(LangError::new(
                    format!("Unable to get full name for method call: {:#?}", self),
                    LangErrorKind::GeneralError,
                ));
            };

            Ok(util::to_method_name(structure_name, &generics, &self.name))
        } else {
            // TODO: Possible generics on functions, need to handle it here.
            Ok(self.name.clone())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructInit {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,
    pub generics: Option<Generics>,
}

impl StructInit {
    pub fn new(name: String, arguments: Vec<Argument>, generics: Option<Generics>) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            generics,
        }
    }

    /// Returns the generics. If generics was set at the struct init call, this
    /// function will replace the types of the types parsed during type inference
    /// with type specified at the init call.
    pub fn generics(&mut self) -> Option<&Generics> {
        if let Some(Ty::CompoundType(_, ty_generics)) = &mut self.ret_type {
            Some(ty_generics)
        } else {
            panic!("Struct init type not struct.");
        }

        /*
        if let Some(generics) = &self.generics {
            for (self_gen, ty_gen) in generics.iter_types().zip(ty_generics.iter_types_mut()) {
                *ty_gen = self_gen.clone();
            }
        }

        Some(ty_generics)
        */
    }

    /// Returns the "full name" which is the name containing possible generics
    /// as well.
    pub fn full_name(&mut self) -> CustomResult<String> {
        let generics = if let Some(generics) = self.generics() {
            generics.clone()
        } else {
            Generics::new()
        };

        if let Some(ty) = &self.ret_type {
            if let Ty::CompoundType(InnerTy::Struct(ident), _) = ty {
                Ok(util::to_generic_struct_name(ident, &generics))
            } else {
                Err(LangError::new(
                    format!("Unable to get full name for struct init: {:#?}", self),
                    LangErrorKind::GeneralError,
                ))
            }
        } else {
            unreachable!("Struct init has no type: {:#?}", self);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayInit {
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,
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
