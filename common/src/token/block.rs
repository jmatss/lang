use super::expr::{Expression, Variable};
use crate::variable_type::TypeStruct;

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

    /// The string is the name of the structure that this impl block implements.
    Implement(String, Vec<Function>),

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
    IfCase(Option<Expression>),

    // Any `MatchCase` blocks should be grouped together under one `Match`.
    // See the example above for `If`, should be structured ~similarly.
    Match(Expression),
    MatchCase(Expression),

    For(Variable, Expression),
    // TODO: Maybe merge while and loop (?)
    While(Option<Expression>),

    // Function for testing, allows strings as test names with spaces etc.
    Test(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Option<Vec<TypeStruct>>,
    pub implements: Option<Vec<TypeStruct>>,
    pub members: Option<Vec<Variable>>, // TODO: extends: Vec<Type>
}

impl Struct {
    pub fn new(
        name: String,
        generics: Option<Vec<TypeStruct>>,
        implements: Option<Vec<TypeStruct>>,
        members: Option<Vec<Variable>>,
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
    pub generics: Option<Vec<TypeStruct>>,
    pub parameters: Option<Vec<Variable>>,
    pub ret_type: Option<TypeStruct>,
    pub is_var_arg: bool,
}

impl Function {
    pub fn new(
        name: String,
        generics: Option<Vec<TypeStruct>>,
        parameters: Option<Vec<Variable>>,
        ret_type: Option<TypeStruct>,
        is_var_arg: bool,
    ) -> Self {
        Function {
            name,
            generics,
            parameters,
            ret_type,
            is_var_arg,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<TypeStruct>,
}

impl Enum {
    pub fn new(name: String, generics: Vec<TypeStruct>) -> Self {
        Enum { name, generics }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<TypeStruct>,
}

impl Interface {
    pub fn new(name: String, generics: Vec<TypeStruct>) -> Self {
        Interface { name, generics }
    }
}
