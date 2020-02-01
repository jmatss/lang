use std::collections::HashMap;
use crate::parser::token::Variable;
use crate::CustomResult;
use crate::parser::abstract_syntax_tree::ScopeIndex;

// Use a number to indicate scope.
// Make an ordered list all the times the variable in mentioned.
// Keep the value of the variable at all the stages, and it will be changed every time
// am assignment is made on the variable.

// - The lifetime of a variable is only per scope (with the exception of static variables).
//   Valid scopes are:
//      default("local" variables, static variables)
//      functions(parameters, local variables),
//      class(member fields)
//      macro(parameters, local variables)
//      if(local variables, "of" cast if it evaluates to true)
//      else(local variables, "of" cast if it evaluates to true)
//      match("of"(/"is"))
//      match cases(local variables)
//      for(local variables, iterator expression)
//      while(local variables)
//      with(local variables, expressions to close(if any))
//      catch(local variables, caught throwable)

// - A stack or heap allocated variable in a function can not be borrowed from there.
// - See if variables are primitive(default copy) or regular(default move)
// - Need to parse if a value is being moved, borrowed or copied
// - Need to keep track of when the variable is used as parameter(/maybe argument)
// - Need to keep track of when it is moved, that it isn't used again.
// - If a variable is owned by a function and that function never moves it, free at end.
// - Need to remember shadowing.

// TODO: When lambdas are added, make sure that the variables live long enough for the use of the
//  closure.
// TODO: Lifetime of throws? How should it find the next catch?
// TODO: Maybe at assignment to case's (rusts: id @ pattern)
// TODO: Make sure to consider functions/variables included with "use".
pub struct ActionTree {
    // The vector contains all the times the Variable is used in order.
    variables: HashMap<ScopeIndex, Vec<Variable>>,
}

impl ActionTree {
    pub fn new() -> Self {
        ActionTree { variables: HashMap::new() }
    }

    // Go through and assemble all variables into action_tree.variables.
    pub fn step1() -> CustomResult<()> {
        Ok(())
    }
}
