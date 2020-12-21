use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    expr::{Expr, Var},
    stmt::Modifier,
};

use crate::{
    error::{CustomResult, LangError, LangErrorKind},
    ty::{inner_ty::InnerTy, ty::Ty},
    util,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,

    Function(Rc<RefCell<Function>>),
    Struct(Rc<RefCell<Struct>>),
    Enum(Rc<RefCell<Enum>>),
    Interface(Rc<RefCell<Interface>>),

    /// The string is the name of the structure that this impl block implements
    /// and the body of this block will contain the functions.
    Implement(String),

    /// A anonymous block "{ ... }" that can be used to limit the scope.
    Anonymous,

    // Any `IfCase` blocks should be grouped together under one `If`.
    // A `Ifcase` is a if/elif/else with its corresponding eval expression.
    //
    // Example 1:
    //   if(x) {} else {}
    // =>
    //   Block(If, [ Block(IfCase(x), ...), Block(IfCase(_), ...) ])
    //
    // Example 2:
    //   if (x) {
    //     if (y) {
    //       ...
    //     }
    //   } else (z) {
    //     ...
    //   } else {
    //     ...
    //   }
    // =>
    //   Block(
    //     header: If,
    //     body: [
    //        Block(
    //          header: IfCase(x),
    //          body: [
    //            Block(
    //              header: If,
    //              body: [
    //                Block(
    //                  header: IfCase(y),
    //                  body: [ ... ]
    //                )
    //              ]
    //            )
    //          ]
    //        ),
    //        Block(
    //          header: IfCase(z),
    //          body: [ ... ]
    //        ),
    //        Block(
    //          header: IfCase(_),
    //          body: [ ... ]
    //        )
    //     ]
    //   )
    If,
    IfCase(Option<Expr>),

    // Any `MatchCase` blocks should be grouped together under one `Match`.
    // See the example above for `If`, should be structured ~similarly.
    Match(Expr),
    MatchCase(Expr),

    For(Var, Expr),
    // TODO: Maybe merge while and loop (?)
    While(Option<Expr>),

    // Function for testing, allows strings as test names with spaces etc.
    Test(Function),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub generic_params: Option<Vec<String>>,
    pub implements: Option<Vec<Ty>>,
    pub members: Option<Vec<Rc<RefCell<Var>>>>, // TODO: extends: Vec<Type>

    /// The key is the name of the method.
    pub methods: Option<HashMap<String, Rc<RefCell<Function>>>>,
}

impl Struct {
    pub fn new(name: String) -> Self {
        Self {
            name,
            generic_params: None,
            implements: None,
            members: None,
            methods: None,
        }
    }

    /// Returns the index of the member with name `member_name` in this struct.
    pub fn member_index(&self, member_name: &str) -> Option<usize> {
        if let Some(members) = &self.members {
            for (idx, member) in members.iter().enumerate() {
                if member.borrow().name == member_name {
                    return Some(idx);
                }
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub generics: Option<Vec<Ty>>,
    pub parameters: Option<Vec<Rc<RefCell<Var>>>>,
    pub ret_type: Option<Ty>,
    pub modifiers: Vec<Modifier>,
    pub is_var_arg: bool,

    /// Will be set if this is a function in a "impl" block which means that
    /// this is a function tied to a struct. The type will be the structure
    /// (or the "ident" of the impl block if other than struct are allowed).
    pub method_structure: Option<Ty>,
}

impl Function {
    pub fn new(
        name: String,
        generics: Option<Vec<Ty>>,
        parameters: Option<Vec<Rc<RefCell<Var>>>>,
        ret_type: Option<Ty>,
        modifiers: Vec<Modifier>,
        is_var_arg: bool,
    ) -> Self {
        Function {
            name,
            generics,
            parameters,
            ret_type,
            is_var_arg,
            modifiers,
            method_structure: None,
        }
    }

    /// Returns the "full name" which is the name containing possible structure
    /// and generics as well.
    ///
    /// Format:
    ///   "<STRUCTURE_NAME>:<GENERICS>-<FUNCTION_NAME>"
    pub fn full_name(&self) -> CustomResult<String> {
        if let Some(ty) = &self.method_structure {
            let (structure_name, generics) = if let Ty::CompoundType(inner_ty, generics) = ty {
                match inner_ty {
                    InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Interface(ident) => {
                        (ident, generics)
                    }
                    _ => unreachable!("Method on non structure type: {:#?}", self),
                }
            } else {
                return Err(LangError::new(
                    format!("Unable to get full name for method: {:#?}", self),
                    LangErrorKind::GeneralError,
                ));
            };

            Ok(util::to_method_name(structure_name, generics, &self.name))
        } else {
            // TODO: Possible generics on functions, need to handle it here.
            Ok(self.name.clone())
        }
    }

    pub fn is_static(&self) -> bool {
        (!self.modifiers.contains(&Modifier::This)
            && !self.modifiers.contains(&Modifier::ThisPointer))
            || self.modifiers.contains(&Modifier::Static)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub generics: Option<Vec<String>>,
    pub members: Option<Vec<Rc<RefCell<Var>>>>,
    // TODO: extends: Vec<Type>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltIn {
    pub name: &'static str,
    pub parameters: Vec<Var>,
    pub generics: Option<Vec<Ty>>,
    pub ret_type: Ty,
    pub is_var_arg: bool,
}

impl BuiltIn {
    pub fn new(
        name: &'static str,
        parameters: Vec<Var>,
        generics: Option<Vec<Ty>>,
        ret_type: Ty,
        is_var_arg: bool,
    ) -> Self {
        BuiltIn {
            name,
            parameters,
            generics,
            ret_type,
            is_var_arg,
        }
    }
}

impl Enum {
    pub fn new(
        name: String,
        generics: Option<Vec<String>>,
        members: Option<Vec<Rc<RefCell<Var>>>>,
    ) -> Self {
        Enum {
            name,
            generics,
            members,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub name: String,
    pub generics: Option<Vec<String>>,
    pub members: Option<Vec<Rc<RefCell<Var>>>>,
}

impl Interface {
    pub fn new(
        name: String,
        generics: Option<Vec<String>>,
        members: Option<Vec<Rc<RefCell<Var>>>>,
    ) -> Self {
        Interface {
            name,
            generics,
            members,
        }
    }
}
