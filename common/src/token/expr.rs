use std::hash::Hash;

use super::{
    block::AdtKind,
    lit::Lit,
    op::{BinOperator, Op, UnOperator},
    stmt::Modifier,
};
use crate::{
    error::{CustomResult, LangError, LangErrorKind},
    file::FilePosition,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    util, BlockId,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Lit(Lit, Option<Ty>, Option<FilePosition>),

    // TODO: Is it ok to have type as a expression? This lets one handle binary
    //       operators like ex. "as" in a simple way.
    Type(Ty, Option<FilePosition>),
    Var(Var),
    FuncCall(FuncCall),
    BuiltInCall(BuiltInCall),
    AdtInit(AdtInit),
    ArrayInit(ArrayInit),
    //MacroCall(Option<MacroCall>),
    Op(Op),
}

impl Expr {
    pub fn get_expr_type(&self) -> CustomResult<Ty> {
        Ok(match self {
            Expr::Lit(_, Some(ty), _) | Expr::Type(ty, _) => ty.clone(),
            Expr::Var(var) => {
                if let Some(ty) = &var.ty {
                    ty.clone()
                } else {
                    return Err(LangError::new(
                        format!("Type of var was None: {:?}", var),
                        LangErrorKind::GeneralError,
                        self.file_pos().cloned(),
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
            Expr::BuiltInCall(built_in_call) if built_in_call.ret_type.is_some() => {
                if let Some(ty) = &built_in_call.ret_type {
                    ty.clone()
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::AdtInit(adt_init) if adt_init.ret_type.is_some() => {
                if let Some(ty) = &adt_init.ret_type {
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
                    LangErrorKind::GeneralError,
                    self.file_pos().cloned(),
                ))
            }
        })
    }

    pub fn get_expr_type_mut(&mut self) -> CustomResult<&mut Ty> {
        let file_pos = self.file_pos().cloned();

        Ok(match self {
            Expr::Lit(_, Some(ty), _) | Expr::Type(ty, _) => ty,
            Expr::Var(var) => {
                if let Some(ty) = &mut var.ty {
                    ty
                } else {
                    return Err(LangError::new(
                        "Type of var was None.".into(),
                        LangErrorKind::GeneralError,
                        file_pos,
                    ));
                }
            }
            Expr::FuncCall(func_call) if func_call.ret_type.is_some() => {
                if let Some(ty) = &mut func_call.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::BuiltInCall(built_in_call) if built_in_call.ret_type.is_some() => {
                if let Some(ty) = &mut built_in_call.ret_type {
                    ty
                } else {
                    unreachable!("Value already verified to be Some.");
                }
            }
            Expr::AdtInit(adt_init) if adt_init.ret_type.is_some() => {
                if let Some(ty) = &mut adt_init.ret_type {
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
                    LangErrorKind::GeneralError,
                    file_pos,
                ))
            }
        })
    }

    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            Expr::Lit(.., file_pos) | Expr::Type(.., file_pos) => file_pos.as_ref(),
            Expr::Var(var) => var.file_pos.as_ref(),
            Expr::FuncCall(func_call) => func_call.file_pos.as_ref(),
            Expr::BuiltInCall(built_in_call) => Some(&built_in_call.file_pos),
            Expr::AdtInit(adt_init) => adt_init.file_pos.as_ref(),
            Expr::ArrayInit(array_init) => Some(&array_init.file_pos),
            Expr::Op(op) => match op {
                Op::BinOp(bin_op) => bin_op.file_pos.as_ref(),
                Op::UnOp(un_op) => un_op.file_pos.as_ref(),
            },
        }
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
                UnOperator::AdtAccess(..) => true,
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
    pub ty: Option<Ty>,
    pub modifiers: Option<Vec<Modifier>>,
    pub value: Option<Box<Expr>>,
    pub is_const: bool,

    /// The file position spanning the whole variable.
    pub file_pos: Option<FilePosition>,

    /// The file position spanning only the type of the variable.
    pub ty_file_pos: Option<FilePosition>,

    /// If this is a exact copy of another variable, this index will be set to
    /// help tell the variables aparat.
    pub copy_nr: Option<usize>,
}

impl Var {
    pub fn new(
        name: String,
        ty: Option<Ty>,
        modifiers: Option<Vec<Modifier>>,
        default_value: Option<Box<Expr>>,
        file_pos: Option<FilePosition>,
        ty_file_pos: Option<FilePosition>,
        is_const: bool,
    ) -> Self {
        Var {
            name,
            ty,
            modifiers,
            value: default_value,
            file_pos,
            ty_file_pos,
            is_const,
            copy_nr: None,
        }
    }

    pub fn set_copy_nr(&mut self, copy_nr: usize) {
        self.copy_nr = Some(copy_nr);
    }

    /// Since variables might be duplicated when structs/functions have generics,
    /// a "full_name" is required to uniquely identfy the "clones".The combination
    /// of name+blockID is NOT enough, in those cases a `copy_nr` is appended
    /// to the name.
    ///
    /// This is only a problem during code generation, so this will be used
    /// to store the `name` of the variable uniquely in a `variables` look-up
    /// hash table.
    pub fn full_name(&self) -> String {
        if let Some(copy_nr) = self.copy_nr {
            util::to_var_name(&self.name, copy_nr)
        } else {
            self.name.clone()
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

    pub file_pos: Option<FilePosition>,

    /// Will be set if this is a method call. It will be set to the ADT
    /// that this method is called on.
    pub method_adt: Option<Ty>,
    pub is_method: bool,
}

impl FuncCall {
    pub fn new(
        name: String,
        arguments: Vec<Argument>,
        generics: Option<Generics>,
        file_pos: Option<FilePosition>,
    ) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            generics,
            file_pos,
            method_adt: None,
            is_method: false,
        }
    }

    /// Returns the generics. If generics was set at the function call, this
    /// function will replace the types of the types parsed during type inference
    /// with type specified at the func call.
    pub fn generics(&self) -> Option<&Generics> {
        if let Some(generics) = &self.generics {
            Some(generics)
        } else {
            None
        }
    }

    /// Returns the "full name" which is the name containing possible ADT
    /// and generics as well.
    pub fn full_name(&self) -> CustomResult<String> {
        let (adt_name, adt_generics) = if let Some(adt_ty) = &self.method_adt {
            if let Ty::CompoundType(inner_ty, adt_generics, ..) = adt_ty {
                if inner_ty.is_adt() {
                    let ident = inner_ty.get_ident().unwrap();
                    (ident, Some(adt_generics))
                } else {
                    unreachable!("Method call on non ADT type: {:#?}", self)
                }
            } else {
                return Err(LangError::new(
                    format!("Unable to get full name for method call: {:#?}", self),
                    LangErrorKind::GeneralError,
                    self.file_pos.to_owned(),
                ));
            }
        } else {
            return Ok(self.name.clone());
        };

        Ok(util::to_method_name(
            &adt_name,
            adt_generics,
            &self.name,
            self.generics.as_ref(),
        ))
    }

    /// Returns the "half name" which is the name that does NOT contain anything
    /// related to the structure but will contain function generics (if any).
    pub fn half_name(&self) -> String {
        if let Some(generics) = &self.generics {
            util::to_generic_name(&self.name, generics)
        } else {
            self.name.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltInCall {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,
    pub generics: Option<Generics>,
    pub file_pos: FilePosition,
}

impl BuiltInCall {
    pub fn new(
        name: String,
        arguments: Vec<Argument>,
        generics: Option<Generics>,
        file_pos: FilePosition,
    ) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            generics,
            file_pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AdtInit {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,
    pub generics: Option<Generics>,
    pub kind: AdtKind,
    pub file_pos: Option<FilePosition>,
}

impl AdtInit {
    pub fn new(
        name: String,
        arguments: Vec<Argument>,
        generics: Option<Generics>,
        file_pos: Option<FilePosition>,
        kind: AdtKind,
    ) -> Self {
        Self {
            name,
            arguments,
            ret_type: None,
            generics,
            kind,
            file_pos,
        }
    }

    /// Returns the generics. If generics was set at the struct init call, this
    /// function will replace the types of the types parsed during type inference
    /// with type specified at the init call.
    pub fn generics(&mut self) -> Option<&Generics> {
        let ty_generics = if let Some(Ty::CompoundType(_, ty_generics, _)) = &self.ret_type {
            ty_generics
        } else {
            panic!("Struct init type not struct.");
        };

        if let Some(generics) = &mut self.generics {
            // TODO: This is done every time this function is called and the names
            //       are overwritten if they already are set. Better way to do this?
            for (idx, name) in ty_generics.iter_names().enumerate() {
                generics.insert_lookup(name.clone(), idx);
            }
            Some(generics)
        } else {
            Some(ty_generics)
        }
    }

    /// Returns the "full name" which is the name containing possible generics
    /// as well.
    pub fn full_name(&mut self) -> CustomResult<String> {
        let adt_init_generics = if let Some(generics) = self.generics() {
            Some(generics.clone())
        } else {
            None
        };

        if let Some(ty) = &self.ret_type {
            if let Ty::CompoundType(inner_ty, adt_generics, ..) = ty {
                let generics = if let Some(adt_init_generics) = adt_init_generics {
                    adt_init_generics
                } else {
                    adt_generics.clone()
                };

                Ok(util::to_generic_name(
                    &inner_ty.get_ident().unwrap(),
                    &generics,
                ))
            } else {
                Err(LangError::new(
                    format!("Unable to get full name for ADT init: {:#?}", self),
                    LangErrorKind::GeneralError,
                    self.file_pos.to_owned(),
                ))
            }
        } else {
            unreachable!("ADT init has no type: {:#?}", self);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArrayInit {
    pub arguments: Vec<Argument>,
    pub ret_type: Option<Ty>,
    pub file_pos: FilePosition,
}

impl ArrayInit {
    pub fn new(arguments: Vec<Argument>, file_pos: FilePosition) -> Self {
        Self {
            arguments,
            ret_type: None,
            file_pos,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Argument {
    // Named used for named arguments.
    pub name: Option<String>,
    pub name_file_pos: Option<FilePosition>,
    pub value: Expr,
}

impl Argument {
    pub fn new(name: Option<String>, name_file_pos: Option<FilePosition>, value: Expr) -> Self {
        Argument {
            name,
            name_file_pos,
            value,
        }
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
