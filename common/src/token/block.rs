use super::expr::{Expr, Var};
use crate::types::GenericableType;

#[derive(Debug, Clone, PartialEq)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Interface(Interface),

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

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Option<Vec<GenericableType>>,
    pub implements: Option<Vec<GenericableType>>,
    pub members: Option<Vec<Var>>, // TODO: extends: Vec<Type>
}

impl Struct {
    pub fn new(
        name: String,
        generics: Option<Vec<GenericableType>>,
        implements: Option<Vec<GenericableType>>,
        members: Option<Vec<Var>>,
    ) -> Self {
        Self {
            name,
            generics,
            implements,
            members,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub generics: Option<Vec<GenericableType>>,
    pub parameters: Option<Vec<Var>>,
    pub ret_type: Option<GenericableType>,
    pub is_var_arg: bool,

    /// Will be set if this is a function in a "impl" block which means that
    /// this is a function tied to a struct. The string will be the struct name.
    pub method_struct: Option<String>,
}

impl Function {
    pub fn new(
        name: String,
        generics: Option<Vec<GenericableType>>,
        parameters: Option<Vec<Var>>,
        ret_type: Option<GenericableType>,
        is_var_arg: bool,
    ) -> Self {
        Function {
            name,
            generics,
            parameters,
            ret_type,
            is_var_arg,
            method_struct: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<GenericableType>,
}

impl Enum {
    pub fn new(name: String, generics: Vec<GenericableType>) -> Self {
        Enum { name, generics }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<GenericableType>,
}

impl Interface {
    pub fn new(name: String, generics: Vec<GenericableType>) -> Self {
        Interface { name, generics }
    }
}
