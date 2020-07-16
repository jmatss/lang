/*
use crate::common::variable_type::Type;
use crate::error::CustomError;
use crate::error::CustomError::ParseError;
use crate::lexer::lex_token::{LexToken, Literal as SimpleLiteral, Symbol};
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token::{
    Argument, ArrayAccess, BinaryOperation, BinaryOperator, Block, Struct, Constructor, Enum,
    Expression, Function, FunctionCall, GenericHeader, Interface, Literal, Macro, MacroCall,
    Operation, Operator, Output, Path, Statement, ParseToken, TypeStruct, UnaryOperation, UnaryOperator,
    Variable,
};
use crate::CustomResult;

// TODO: Let line breaks be OK sometimes and treated as whitespace,
//  ex. when having a long parameter list or argument list.

// TODO: Fix correct parsing of block headers inside parse_expression().
//  Example:
//      a = if 1==1: 1 else: 2
//  In the example the if-expression needs to be added to the AST,
//  but also needs to be treated as an expression.

// TODO: Add array and map literals for initialization.
//  Examples:
//      arr @ Character[] = ['a', 'c']
//      arr @ Map<String, Integer> = {"a_key": 1, "str": 123}
//      x @ String = ['h', 'e', 'l', 'l', 'o'].join()

// TODO: Init with:
//      id @= expr      ==> id @ = expr
//  It the same as a regular declaration but with an empty type/modifiers.

// TODO: Let generics implement interfaces. Example
//      function f<T: Iterable<I>>(x @ T):
//          iterate y in x: ...

// TODO: Implement tuples. Examples:
//      x @= (expr, expr2)
//      (a, b) @ (Integer, Integer) = x
//  Maybe implement match on existing variable:
//      x @= 1
//      y @= 2
//      (x, y) = (3, 4)      // => x = 3, y = 4
//
// TODO: Add closures. Variables can be moved/borrowed into them.
//
// TODO: Add ignore linebreak symbol. Ex. "\" so that one can write:
//      id @ \
//      = 3
//
// TODO: How should the owned/borrowed work?
//  Should one be able to tie the lifetime to a specific function?
//
// TODO: Don't allow increment/decrement in other expressions.
//  Maybe remove prefix and only allow postfix.
//  Examples of INVALID usage of inc/dec:
//      print_line("i: {}", i++)
//      (--i - 3)
//  Examples of VALID usages:
//      print_line("i: {}", i); i++
//      --i; (i - 3)
//
// TODO: Implement "cleanup" block that will be executed in a stack like fashion before
//  returning to the caller. Similar to defer, but it is called before the return statement.
//
// TODO: Make "map" indexable like an array. Example:
//      map["key"] = 3

const DEBUG: bool = true;

pub struct ParseTokenIter<'a> {
    pub simple_tokens: &'a [LexToken],
    pub position: usize,

    pub ast: AST,

    // indent_level is normalized by indent_fixed_size to levels of: (0), 1, 2, 3...
    pub line_number: usize,
    pub indent_level: usize,

    pub indent_fixed_size: usize,
}

// Returns a closure: &|simple_token: &SimpleToken| -> bool
macro_rules! stop_on {
    // Default returns true if any of these symbols are matched:
    //      BreakSymbol (LineBreak, SemiColon, EndOfFile), Colon or Comma.
    (default) => {
        &|simple_token: &SimpleToken| -> bool {
            stop_on!(simple_token! default);
        }
    };

    // Used to specify symbols to stop on during expression parsing.
    ($( $symbol:path ),+) => {
        &|simple_token: &SimpleToken| -> bool {
            stop_on!(simple_token! $($symbol),+;);
        }
    };

    // Used to specify symbols and variables containing symbols to stop on during expression parsing.
    ($( $symbol:path ),+; $( $symbol_var:ident ),*) => {
        &|simple_token: &SimpleToken| -> bool {
            stop_on!(simple_token! $($symbol),+; $($symbol_var),*);
        }
    };

    ($simple_token:ident! default) => {
        stop_on!($simple_token! break_symbol);
        stop_on!($simple_token! Symbol::Colon, Symbol::Comma;);
    };

    ($simple_token:ident! break_symbol) => {
        if SimpleToken::is_break_symbol($simple_token) {
            return true;
        }
    };

    ($simple_token:ident!  $( $symbol:path ),+) => {
        stop_on!($simple_token! $($symbol),+;);
    };

    ($simple_token:ident!  $( $symbol:path ),+; $( $symbol_var:ident ),*) => {
        match $simple_token {
            $( SimpleToken::Symbol($symbol) => return true, )*
            $( SimpleToken::Symbol(symbol) if $symbol_var == *symbol  => return true, )*
            _ => return false
        }
    };
}

impl<'a> ParseTokenIter<'a> {
    pub fn new(simple_tokens: &'a [LexToken], indent_fixed_size: usize) -> Self {
        ParseTokenIter {
            simple_tokens,
            position: 0,

            ast: AST::new(),

            line_number: 1,
            indent_level: 0,

            indent_fixed_size,
        }
    }

    pub fn parse_abstract_syntax_tree(&'a mut self) -> CustomResult<AST> {
        loop {
            let current = self.next().ok_or_else(|| {
                ParseError("No more simple_tokens in start of parse_ast...".to_string())
            })?;

            if DEBUG {
                println!(
                    "Current SimpleToken: {:?}, line_number: {}, indent_level: {}",
                    &current, self.line_number, self.indent_level
                );
            }

            match current {
                LexToken::Identifier(identifier) => {
                    if DEBUG {
                        println!("Identifier");
                    }
                    let token = self.parse_identifier(&identifier)?;

                    // If BlockHeader: Add this as a new block to the AST.
                    // Else if Expression: Parse the rest of the expression.
                    // Else: Add this as a token to the AST.
                    match &token {
                        ParseToken::Block(_) => {
                            self.ast
                                .insert_block(token, self.line_number, self.indent_level)?
                        }

                        ParseToken::Expression(expression) => {
                            let expression_token =
                                ParseToken::Expression(self.parse_expression_with_previous(
                                    stop_on!(default),
                                    Some(expression.clone()),
                                )?);
                            self.ast.insert_token(
                                expression_token,
                                self.line_number,
                                self.indent_level,
                            )?
                        }

                        _ => self
                            .ast
                            .insert_token(token, self.line_number, self.indent_level)?,
                    }
                }

                LexToken::Symbol(Symbol::LineBreak) => {
                    if DEBUG {
                        println!("Linebreak");
                    }
                    self.update_indent_and_line_number(true)?
                }

                LexToken::Symbol(Symbol::SemiColon) => {
                    if DEBUG {
                        println!("Semocolon");
                    }
                    continue;
                }

                // TODO: Make sure to update indent size when getting LineBreak.
                LexToken::Symbol(_) | LexToken::Number(_) | LexToken::Literal(_) => {
                    if DEBUG {
                        println!("Symbol, number or literal.");
                    }
                    self.rewind();
                    let expression = self.parse_expression(stop_on!(default))?;
                    let token = ParseToken::Expression(expression);
                    self.ast
                        .insert_token(token, self.line_number, self.indent_level)?;
                }

                LexToken::EndOfFile => {
                    if DEBUG {
                        println!("EndOfFile");
                    }
                    return Ok(std::mem::replace(&mut self.ast, AST::new()));
                }

                LexToken::Unknown(unknown) => {
                    return Err(ParseError(format!(
                        "Bad string (SimpleToken) during parse_next: {:?}",
                        unknown
                    )))
                }
            }
        }
    }

    /// Returns the next `LexToken` from the iterator.
    #[inline]
    fn next(&mut self) -> Option<LexToken> {
        let result = self.simple_tokens.get(self.position).cloned();
        self.position += 1;
        result
    }

    /// Returns the next `LexToken` from the iterator and skips any white spaces.
    #[inline]
    fn next_skip_whitespace(&mut self) -> Option<LexToken> {
        self.skip_whitespace();
        self.next()
    }

    /// Returns the next `LexToken` from the iterator and skips any white spaces
    /// and line breaks.
    #[inline]
    fn next_skip_whitespace_and_line_break(
        &mut self,
        update_indent: bool,
    ) -> CustomResult<Option<LexToken>> {
        self.skip_whitespace_and_line_break(update_indent)?;
        Ok(self.next())
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        if let Some(LexToken::Symbol(Symbol::WhiteSpace(_))) = self.peek() {
            self.next();
        }
    }

    #[inline]
    fn skip_whitespace_and_line_break(&mut self, update_indent: bool) -> CustomResult<()> {
        // Need to do it in a loop if there are multiple whitespace/linebreaks in a row.
        loop {
            match self.next() {
                Some(LexToken::Symbol(Symbol::WhiteSpace(_))) => continue,
                Some(LexToken::Symbol(Symbol::LineBreak)) => {
                    self.update_indent_and_line_number(update_indent)?;
                    continue;
                }
                _ => {
                    self.rewind();
                    break;
                }
            }
        }

        Ok(())
    }

    #[inline]
    fn peek(&mut self) -> Option<LexToken> {
        self.peek_n(0)
    }

    #[inline]
    fn peek_n(&mut self, n: isize) -> Option<LexToken> {
        // n is isize to allow for negative peeks.
        let index = self.position as isize + n;
        if index < 0 {
            return None;
        }
        self.simple_tokens.get(index as usize).cloned()
    }

    #[inline]
    fn peek_skip_whitespace(&mut self) -> Option<LexToken> {
        self.skip_whitespace();
        self.peek()
    }

    #[inline]
    fn peek_skip_whitespace_and_line_break(&mut self) -> CustomResult<LexToken> {
        let mut peek_amount = 0;
        loop {
            let result = self.peek_n(peek_amount).ok_or_else(|| {
                ParseError("Out of tokens during peek_skip_whitespace_and_line_break.".to_string())
            })?;
            match result {
                LexToken::Symbol(Symbol::WhiteSpace(_)) | LexToken::Symbol(Symbol::LineBreak) => {
                    peek_amount += 1;
                    continue;
                }

                _ => return Ok(result),
            }
        }
    }

    #[inline]
    fn rewind(&mut self) {
        self.position -= 1;
    }

    #[inline]
    fn rewind_skip_whitespace(&mut self) {
        self.position -= 1;
        if let Some(LexToken::Symbol(Symbol::WhiteSpace(_))) = self.peek() {
            self.position -= 1;
        }
    }

    // Ensure that the next token is a block header end.
    fn assert_block_header_end(&mut self) -> CustomResult<()> {
        let next = self.next_skip_whitespace();
        if let Some(LexToken::Symbol(Symbol::Colon)) = next {
            Ok(())
        } else {
            Err(ParseError(format!(
                "Incorrect block ending. Expected: Colon, got: {:?}.",
                next
            )))
        }
    }

    // Can be called after a LineBreak have been consumed or right at the LineBreak.
    // Ensures that the new indent is valid and increases line_number by 1.
    // The "update_indent" arg is used to indicate if the indentation should be updated or not.
    // Examples when it should be set to false is LineBreaks before and after Commas.
    fn update_indent_and_line_number(&mut self, update_indent: bool) -> CustomResult<()> {
        if update_indent {
            if let Some(LexToken::Symbol(Symbol::LineBreak)) = self.peek() {
                self.next(); // Consume Linebreak.
            } else if let Some(LexToken::Symbol(Symbol::LineBreak)) = self.peek_n(-1) {
                // LineBreak already consumed, move along.
            } else {
                return Err(ParseError(
                    "Called update_indent_and_line_number with no line break in sight.".to_string(),
                ));
            }

            let indent = self.next_whitespace();

            // Edge case if the row is empty, ignore indents.
            if let Some(LexToken::Symbol(Symbol::LineBreak)) = self.peek_skip_whitespace() {
                self.line_number += 1;
                return Ok(());
            }
            if indent % self.indent_fixed_size != 0 {
                return Err(ParseError(format!(
                    "Current indent not divisible by specified fixed indent. Fixed: {}, got: {}",
                    self.indent_fixed_size, indent
                )));
            }

            // If the indent size increases (i.e. a new block), ensure that it only increased
            // with at most 1 indent level.
            // It can be as small as it want if it decreases though.
            let current_indent_level = indent / self.indent_fixed_size;
            if current_indent_level > self.indent_level + 1 {
                return Err(ParseError(format!(
                    "Current indent not not valid relative to header.\
                     \"BlockHeader\" indent level: {}, current indent level: {}",
                    self.indent_level, current_indent_level
                )));
            }

            self.indent_level = current_indent_level;
        }

        self.line_number += 1;

        Ok(())
    }

    fn next_whitespace(&mut self) -> usize {
        if let Some(LexToken::Symbol(Symbol::WhiteSpace(size))) = self.next() {
            size
        } else {
            self.rewind();
            0
        }
    }

    fn next_identifier(&mut self) -> CustomResult<String> {
        let simple_token = self.next_skip_whitespace();
        if let Some(LexToken::Identifier(identifier)) = simple_token {
            Ok(identifier)
        } else {
            Err(ParseError(format!(
                "Did not get an identifier when in next_identifier: {:?}",
                simple_token
            )))
        }
    }

    // Valid formats for variables:
    //      id
    //      id @ Type* modifier*
    // (The assignment is handled separately, ex. id = expression)
    // TODO: Add "name, nameN" for declaring multiple variables on the same line.
    // TODO: Add support for tuples.
    fn parse_variable_with_identifier(&mut self, identifier: &str) -> CustomResult<Variable> {
        let mut variable = Variable::new(identifier.to_string());

        // Make sure that the variable doesn't have a name that clashes with a keyword.
        if let Some(keyword) = ParseToken::lookup_identifier(&identifier) {
            return Err(CustomError::ParseError(format!(
                "A keyword was used as variable name: {:?}",
                keyword
            )));
        }

        // If true: This variable has specified type/modifiers, and is therefor a declaration.
        if let Some(LexToken::Symbol(Symbol::At)) = self.peek_skip_whitespace() {
            variable.set_declaration();
            self.next_skip_whitespace(); // Remove "At" symbol.
            self.parse_next_type_and_modifiers(&mut variable)?;
        }

        Ok(variable)
    }

    fn parse_variable(&mut self) -> CustomResult<Variable> {
        let peek = self.peek_skip_whitespace();
        if let Some(LexToken::Symbol(symbol)) = peek {
            return Err(CustomError::ParseError(format!(
                "A symbol was used as variable name: {:?}",
                symbol
            )));
        }

        let identifier = self.next_identifier()?;
        self.parse_variable_with_identifier(&identifier)
    }

    fn parse_next_type_and_modifiers(&mut self, variable: &mut Variable) -> CustomResult<()> {
        // Parse the type, which have to be the first "identifier".
        // If the first identifier is a modifier token, there is no type and var_type it is set to None.
        let peek = self.peek_skip_whitespace();
        match peek {
            Some(LexToken::Identifier(identifier)) => {
                let lookup = ParseToken::lookup_identifier(&identifier);
                if let Some(ParseToken::Statement(Statement::Modifier(_))) = lookup {
                    variable.var_type = None;
                } else {
                    variable.var_type = Some(self.next_type()?);
                }

                while let Some(LexToken::Identifier(identifier)) = self.next_skip_whitespace() {
                    let lookup = ParseToken::lookup_identifier(&identifier);
                    if let Some(ParseToken::Statement(Statement::Modifier(modifier))) = lookup {
                        variable.modifiers.push(modifier);
                    } else {
                        break;
                    }
                }

                self.rewind_skip_whitespace();

                Ok(())
            }

            Some(LexToken::Symbol(Symbol::Equals)) => Ok(()), // No type/modifiers specified.

            _ => Err(ParseError(format!(
                "Expected identifier(s) in parse_next_type_and_modifiers, got: {:?}",
                peek
            ))),
        }
    }

    fn next_type(&mut self) -> CustomResult<TypeStruct> {
        let identifier = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        let generics = if generics.is_empty() {
            Some(generics)
        } else {
            None
        };

        Ok(TypeStruct::new(
            Type::ident_to_type(&identifier),
            generics,
        ))
    }

    // Valid formats:
    //      Type
    //      Type1<T>, TypeN
    // Whitespace and line breaks are allowed between the types(before and after comma).
    fn next_type_list(&mut self) -> CustomResult<Vec<TypeStruct>> {
        let mut types = Vec::new();

        // Loop through and parse all items in the "type list".
        loop {
            self.skip_whitespace_and_line_break(false)?;
            types.push(self.next_type()?);

            let current = self.peek_skip_whitespace_and_line_break()?;
            if let LexToken::Symbol(Symbol::Comma) = current {
                self.next_skip_whitespace_and_line_break(false)?; // Remove Comma symbol.
                continue;
            } else {
                break;
            }
        }

        Ok(types)
    }

    // PS: If there are no generic list i.e. the first symbol is NOT a PointBracketBegin,
    // returns an empty vector and does NOT return an error.
    fn next_generic_list(&mut self) -> CustomResult<Vec<TypeStruct>> {
        let mut generics = Vec::new();

        if let Some(LexToken::Symbol(Symbol::PointyBracketBegin)) = self.peek_skip_whitespace() {
            self.next_skip_whitespace(); // Remove PointBracketBegin symbol.

            generics = self.next_type_list()?;

            let current = self.next_skip_whitespace_and_line_break(false)?;
            if let Some(LexToken::Symbol(Symbol::PointyBracketEnd)) = current {
                // Everything alright.
            } else {
                return Err(ParseError(format!(
                    "Expected a PointBracketEnd after parsing generic list, got: {:?}",
                    current
                )));
            }
        }

        Ok(generics)
    }

    // Path is used in "use" and "package".
    // Valid format:
    //      id1
    //      id1.iN      (A list of identifiers separated by dots)
    fn parse_path(&mut self) -> CustomResult<Path> {
        let mut path = Path::new();

        // Loop through the path one identifier at a time.
        loop {
            path.push(self.next_identifier()?);

            let peek = self
                .peek_skip_whitespace()
                .ok_or_else(|| ParseError("No more tokens in parse_path.".to_string()))?;
            if let LexToken::Symbol(Symbol::Dot) = peek {
                self.next_skip_whitespace(); // Remove Dot symbol.
                continue;
            } else if LexToken::is_break_symbol(&peek) {
                break;
            } else {
                return Err(ParseError(format!(
                    "Incorrect symbol found during parsing of next_path: {:?}",
                    peek
                )));
            }
        }

        Ok(path)
    }

    /*
    fn next_tuple(&mut self) -> CustomResult {

    }
    */

    fn parse_identifier(&mut self, identifier: &str) -> CustomResult<ParseToken> {
        // If true: A reserved keyword (either statement or header).
        // Else: a function call, variable, array access etc.
        if let Some(token) = ParseToken::lookup_identifier(&identifier) {
            match token {
                ParseToken::Statement(statement) => {
                    Ok(ParseToken::Statement(self.parse_statement(statement)?))
                }

                ParseToken::Block(block_header) => {
                    Ok(ParseToken::Block(self.parse_block_header(block_header)?))
                }

                _ => Err(ParseError(format!(
                    "Received incorrect token during parse_identifier: {:?}",
                    token
                ))),
            }
        } else {
            self.parse_identifier_as_expression(identifier)
        }
    }

    // Neither a statement nor a block.
    // Either a function call, variable, array access etc.
    fn parse_identifier_as_expression(&mut self, identifier: &str) -> CustomResult<ParseToken> {
        let peek = self.peek_skip_whitespace().ok_or_else(|| {
            ParseError("No more tokens in parse_identifier_as_expression".to_string())
        })?;
        match peek {
            LexToken::Symbol(Symbol::ParenthesisBegin) => {
                // This is a function call.
                let function_call = self.parse_function_call(&identifier)?;
                Ok(ParseToken::Expression(Expression::FunctionCall(Some(
                    function_call,
                ))))
            }

            LexToken::Symbol(Symbol::CurlyBracketBegin) => {
                // This is a macro call.
                let macro_call = self.parse_macro_call(&identifier)?;
                Ok(ParseToken::Expression(Expression::MacroCall(Some(macro_call))))
            }

            // TODO: Parse multiple dimension arrays
            //  Can only do one, ex. id[expr], need to do: id[expr1][expr2]...
            // TODO: Allow linebreak after SquareBracketBegin(easy) and before SquareBracketEnd(hard).
            // TODO: Add support for init of empty vector etc. (x = [])
            LexToken::Symbol(Symbol::SquareBracketBegin) => {
                // This is an array access (ex. id[expr])
                let array_access = self.parse_array_access(identifier)?;
                Ok(ParseToken::Expression(Expression::ArrayAccess(Some(
                    array_access,
                ))))
            }

            _ => {
                // Treat it as an variable.
                let variable = self.parse_variable_with_identifier(identifier)?;
                Ok(ParseToken::Expression(Expression::Variable(variable)))
            }
        }
    }

    fn parse_array_access(&mut self, identifier: &str) -> CustomResult<ArrayAccess> {
        let next = self.next_skip_whitespace();
        if let Some(LexToken::Symbol(Symbol::SquareBracketBegin)) = next {
            // Everything ok, the SquareBracketBegin have been removed.
        } else {
            return Err(ParseError(format!(
                "First simple_token in parse_array_access isn't a SquareBracketBegin: {:?}",
                next
            )));
        }

        // FIXME: Is it allowed for a variable that doesn't declare itself to use modifiers/Type (?)
        //  Probably not, but currently the parsing allows it everywhere else, so keep it for now.
        let variable = self.parse_variable_with_identifier(identifier)?;
        let expression = self.parse_expression(stop_on!(Symbol::SquareBracketEnd))?;

        self.next_skip_whitespace(); // Remove the SquareBracketEnd.
        let array_access = ArrayAccess::new(variable, Box::new(expression));

        Ok(array_access)
    }

    fn parse_statement(&mut self, statement: Statement) -> CustomResult<Statement> {
        match statement {
            Statement::Return(_) => {
                // If true: Next symbol is a break which means that this return-statement
                // doesn't return anything.
                // Else: It returns the next expression.
                let peek = self
                    .peek_skip_whitespace()
                    .ok_or_else(|| ParseError("Out of tokens in parse_statement.".to_string()))?;
                if LexToken::is_break_symbol(&peek) {
                    Ok(Statement::Return(None))
                } else {
                    Ok(Statement::Return(Some(
                        self.parse_expression(stop_on!(default))?,
                    )))
                }
            }

            Statement::Yield(_) => Ok(Statement::Yield(Some(
                self.parse_expression(stop_on!(default))?,
            ))),

            Statement::Break | Statement::Continue => Ok(statement),

            Statement::Use(_) => Ok(Statement::Use(Some(self.parse_path()?))),

            Statement::Package(_) => Ok(Statement::Package(Some(self.parse_path()?))),

            Statement::Throw(_) => Ok(Statement::Throw(Some(
                self.parse_expression(stop_on!(default))?,
            ))),

            // Ignore modifiers.
            _ => Err(ParseError(format!(
                "Received invalid statement in parse_statement: {:?}",
                statement
            ))),
        }
    }

    // Shunting-yard algorithm
    // Parses expression until stop_condition evaluates to true.
    // Does NOT consume the SimpleToken that returned true.
    // PS: Skips/ignores LineBreak if they aren't specified in the stop_condition().
    fn parse_expression_with_previous(
        &mut self,
        stop_condition: &dyn Fn(&LexToken) -> bool,
        previous_expression: Option<Expression>,
    ) -> CustomResult<Expression> {
        let mut output_stack: Vec<Output> = Vec::new();
        let mut operator_stack: Vec<Operator> = Vec::new();

        // This bool is used to ensure that all "operands" are separated by operators.
        // "operand" == not an operator, ex. literals, numbers, variables or function calls.
        let mut previous_was_operand = if let Some(expression) = previous_expression {
            // Add previously parsed expression if it exists.
            output_stack.push(Output::Operand(expression));
            true
        } else {
            false
        };

        loop {
            let current = self.next_skip_whitespace().ok_or_else(|| {
                ParseError("No more tokens in parse_expression_with_previous_until".to_string())
            })?;

            /*
            println!("\"current\", \"output_stack\" and \"operator_stack\" in expression parsing:\n{:?}", &current);
            dbg!(&output_stack, &operator_stack);
            println!();
            */

            // TODO: Clean up LineBreak special case.
            // Break and stop parsing expression if stop_condition() evaluates to true.
            if stop_condition(&current) {
                // Put back the char
                self.rewind_skip_whitespace();
                break;
            }

            // If Linebreak: Update indent/linebreak and skip.
            // This allows one to use LineBreak's freely in expressions that doesn't
            // have LineBreak as a stop_condition.
            if let LexToken::Symbol(Symbol::LineBreak) = current {
                self.update_indent_and_line_number(false)?;
                continue;
            }

            match current {
                LexToken::Identifier(identifier) => {
                    if previous_was_operand {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string(),
                        ));
                    }

                    // TODO: Add support for parsing if, match etc.
                    if let ParseToken::Expression(expression) =
                        self.parse_identifier_as_expression(&identifier)?
                    {
                        // Function call, variable or array access.
                        output_stack.push(Output::Operand(expression));
                    } else {
                        return Err(ParseError(format!(
                            "Bad identifier parsed during parse_expression: {:?}",
                            identifier
                        )));
                    }

                    previous_was_operand = true;
                }

                LexToken::Number(number_string) => {
                    if previous_was_operand {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string(),
                        ));
                    }

                    // TODO: Maybe parse postfix types here (?).
                    let number = if number_string.contains('.') {
                        Expression::Float(number_string, None)
                    } else {
                        Expression::Integer(number_string, None)
                    };
                    output_stack.push(Output::Operand(number));

                    previous_was_operand = true;
                }

                LexToken::Literal(literal) => {
                    if previous_was_operand {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string(),
                        ));
                    }

                    let expression = match literal {
                        SimpleLiteral::StringLiteral(string) => {
                            Expression::Literal(Literal::StringLiteral(string))
                        }
                        SimpleLiteral::CharLiteral(string) => {
                            Expression::Literal(Literal::CharLiteral(string))
                        }
                    };
                    output_stack.push(Output::Operand(expression));

                    previous_was_operand = true;
                }

                LexToken::Symbol(symbol) => {
                    let operator = ParseToken::lookup_operator(symbol.clone()).ok_or_else(|| {
                        ParseError(format!(
                            "Parsed None operator during expression: {:?}",
                            symbol
                        ))
                    })?;
                    match operator {
                        Operator::ParenthesisBegin => operator_stack.push(operator),

                        Operator::ParenthesisEnd => {
                            loop {
                                let popped_operator = operator_stack.pop()
                                    .ok_or_else(|| ParseError(
                                        "Exhausted operator stack while looking for parenthesisBegin.".to_string()
                                    ))?;

                                if let Operator::ParenthesisBegin = popped_operator {
                                    break;
                                } else {
                                    output_stack.push(Output::Operator(popped_operator));
                                }
                            }
                        }

                        Operator::Increment => {
                            // Set as postfix operator only if the previous operator
                            // wasn't a postfix operation.
                            let increment_operator = if previous_was_operand {
                                Operator::UnaryOperator(UnaryOperator::IncrementPostfix)
                            } else if let Some(Operator::UnaryOperator(prev_op)) =
                                operator_stack.last()
                            {
                                if *prev_op == UnaryOperator::DecrementPostfix
                                    || *prev_op == UnaryOperator::IncrementPostfix
                                {
                                    Operator::UnaryOperator(UnaryOperator::IncrementPostfix)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::IncrementPrefix)
                                }
                            } else {
                                Operator::UnaryOperator(UnaryOperator::IncrementPrefix)
                            };

                            self.add_operator(
                                increment_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Decrement => {
                            // Set as postfix operator only if the previous operator
                            // wasn't a postfix operation.
                            let decrement_operator = if previous_was_operand {
                                Operator::UnaryOperator(UnaryOperator::DecrementPostfix)
                            } else if let Some(Operator::UnaryOperator(prev_op)) =
                                operator_stack.last()
                            {
                                if *prev_op == UnaryOperator::DecrementPostfix
                                    || *prev_op == UnaryOperator::IncrementPostfix
                                {
                                    Operator::UnaryOperator(UnaryOperator::DecrementPostfix)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::DecrementPrefix)
                                }
                            } else {
                                Operator::UnaryOperator(UnaryOperator::DecrementPrefix)
                            };

                            self.add_operator(
                                decrement_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Plus => {
                            // Set as unary operator only if the previous operator
                            // wasn't a postfix operation.
                            let plus_operator = if previous_was_operand {
                                Operator::BinaryOperator(BinaryOperator::Addition)
                            } else if let Some(Operator::UnaryOperator(prev_op)) =
                                operator_stack.last()
                            {
                                if *prev_op == UnaryOperator::DecrementPostfix
                                    || *prev_op == UnaryOperator::IncrementPostfix
                                {
                                    Operator::BinaryOperator(BinaryOperator::Addition)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::Positive)
                                }
                            } else {
                                Operator::UnaryOperator(UnaryOperator::Positive)
                            };

                            self.add_operator(
                                plus_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Minus => {
                            // Set as unary operator only if the previous operator
                            // wasn't a postfix operation.
                            let minus_operator = if previous_was_operand {
                                Operator::BinaryOperator(BinaryOperator::Subtraction)
                            } else if let Some(Operator::UnaryOperator(prev_op)) =
                                operator_stack.last()
                            {
                                if *prev_op == UnaryOperator::DecrementPostfix
                                    || *prev_op == UnaryOperator::IncrementPostfix
                                {
                                    Operator::BinaryOperator(BinaryOperator::Subtraction)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::Negative)
                                }
                            } else {
                                Operator::UnaryOperator(UnaryOperator::Negative)
                            };

                            self.add_operator(
                                minus_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        _ => {
                            // Rest of the operators that aren't parenthesis, neg/pos or inc/dec.
                            self.add_operator(operator, &mut output_stack, &mut operator_stack)?;
                        }
                    }

                    // Previous was (/current is) Symbol.
                    previous_was_operand = false;
                }

                // Ignore modifiers.
                _ => {
                    return Err(ParseError(format!(
                        "Received invalid simple_token in parse_expression: {:?}",
                        current.clone()
                    )))
                }
            }
        }

        while let Some(operator) = operator_stack.pop() {
            output_stack.push(Output::Operator(operator));
        }

        println!("Output_stack: {:#?}", output_stack);

        Ok(self.polish_to_expression(&output_stack)?)
    }

    fn add_operator(
        &self,
        operator: Operator,
        output_stack: &mut Vec<Output>,
        operator_stack: &mut Vec<Operator>,
    ) -> CustomResult<()> {
        let operator_precedence = operator.precedence().ok_or_else(|| {
            ParseError(format!(
                "Unable to see precedence for operator: {:?}",
                operator
            ))
        })?;
        while let Some(popped_operator) = operator_stack.pop() {
            if let Operator::ParenthesisBegin = popped_operator {
                // put back the start parenthesis.
                operator_stack.push(popped_operator);
                break;
            }

            let popped_operator_prec = popped_operator.precedence().ok_or_else(|| {
                ParseError(format!(
                    "Unable to see precedence for popped_operator: {:?}",
                    popped_operator
                ))
            })?;
            let popped_operator_eval_left_to_right =
                popped_operator.evaluate_left_to_right().ok_or_else(|| {
                    ParseError("Unable to see if operator eval left to right.".to_string())
                })?;

            // "popped operator" higher precedence than "current operator".
            // or same precedence with left-to-right evaluation.
            if operator_precedence > popped_operator_prec
                || (operator_precedence == popped_operator_prec
                    && popped_operator_eval_left_to_right)
            {
                output_stack.push(Output::Operator(popped_operator));
                continue;
            } else {
                operator_stack.push(popped_operator); // Put back
                break;
            }
        }

        operator_stack.push(operator);
        Ok(())
    }

    fn parse_expression(
        &mut self,
        stop_condition: &dyn Fn(&LexToken) -> bool,
    ) -> CustomResult<Expression> {
        self.parse_expression_with_previous(stop_condition, None)
    }

    fn parse_expression_list(
        &mut self,
        stop_condition: &dyn Fn(&LexToken) -> bool,
    ) -> CustomResult<Vec<Expression>> {
        let mut expressions = Vec::new();

        // Parses a list of expressions separated with Commas.
        // Stops when "stop_condition" is met.
        loop {
            let expression = self.parse_expression(&|simple_token| {
                if stop_condition(simple_token) {
                    return true;
                }
                stop_on!(simple_token! Symbol::Comma);
            })?;

            expressions.push(expression);

            if let Some(LexToken::Symbol(Symbol::Comma)) = self.peek() {
                self.next(); // Remove comma
                continue;
            } else {
                break;
            }
        }

        Ok(expressions)
    }

    fn polish_to_expression(&self, output_stack: &[Output]) -> CustomResult<Expression> {
        let mut expression_stack = Vec::new();
        for i in 0..output_stack.len() {
            let current = output_stack
                .get(i)
                .ok_or_else(|| ParseError(format!("Empty item in output_stack at index: {}", i)))?;

            match current {
                Output::Operator(Operator::BinaryOperator(operator)) => {
                    let right = expression_stack.pop().ok_or_else(|| {
                        ParseError(
                            "Empty item in expression_stack when popping  (binary).".to_string(),
                        )
                    })?;
                    let left = expression_stack.pop().ok_or_else(|| {
                        ParseError(
                            "Empty item in expression_stack when popping left (binary)."
                                .to_string(),
                        )
                    })?;

                    let binary_operation =
                        BinaryOperation::new(operator.clone(), Box::new(left), Box::new(right));

                    expression_stack.push(Expression::Operation(Operation::BinaryOperation(Some(
                        binary_operation,
                    ))));
                }

                Output::Operator(Operator::UnaryOperator(operator)) => {
                    let value = expression_stack.pop().ok_or_else(|| {
                        ParseError(
                            "Empty item in expression_stack when popping value (unary)."
                                .to_string(),
                        )
                    })?;

                    let unary_operation = UnaryOperation::new(operator.clone(), Box::new(value));

                    expression_stack.push(Expression::Operation(Operation::UnaryOperation(Some(
                        unary_operation,
                    ))));
                }

                Output::Operand(expression) => expression_stack.push(expression.clone()),

                _ => {
                    return Err(ParseError(format!(
                        "Bad match during polish_to_expression with Output: {:?}",
                        current
                    )))
                }
            }
        }

        if expression_stack.len() != 1 {
            return Err(ParseError(format!(
                "Not one expression left at end of polish_to_expression, amount: {}",
                expression_stack.len()
            )));
        }

        Ok(expression_stack.pop().ok_or_else(|| {
            ParseError("Unable to return last expression of expression_stack.".to_string())
        })?)
    }

    fn parse_argument_list(
        &mut self,
        begin_symbol: Symbol,
        end_symbol: Symbol,
    ) -> CustomResult<Vec<Argument>> {
        let mut arguments = Vec::new();

        let current = self.next_skip_whitespace();
        match current {
            Some(LexToken::Symbol(ref symbol)) if symbol == &begin_symbol => {
                // Edge case if there are no arguments
                let peek = self.peek_skip_whitespace_and_line_break()?;
                match peek {
                    LexToken::Symbol(ref symbol) if symbol == &end_symbol => {
                        self.next_skip_whitespace_and_line_break(false)?; // Remove end symbol.
                        return Ok(arguments);
                    }
                    _ => (),
                }

                // FIXME: Can figure out that it isn't a named argument at the first if-statement
                //  if it doesn't match a identifier, so can clean up code.
                loop {
                    // Start with the assumption that this is a named argument.
                    // Parse as identifier and peek on the next simple token, that is assumed to be Equals.
                    // If it is false, redo everything with the assumption that it is not a named argument.
                    self.skip_whitespace_and_line_break(false)?;
                    let next = self.next_skip_whitespace_and_line_break(false)?;
                    let name = if let Some(LexToken::Identifier(identifier)) = next {
                        identifier
                    } else {
                        "".to_string()
                    };

                    // If true: This is a named argument,
                    // Else: This is a regular argument.
                    // FIXME: Maybe allow line break in front/behind equals sign.
                    if let Some(LexToken::Symbol(Symbol::Equals)) = self.peek_skip_whitespace() {
                        self.next_skip_whitespace(); // Remove Equals symbol.
                        let expr = self.parse_expression(stop_on!(Symbol::Comma; end_symbol))?;
                        arguments.push(Argument::new(Some(name), expr));
                    } else {
                        self.rewind_skip_whitespace();
                        let expr = self.parse_expression(stop_on!(Symbol::Comma; end_symbol))?;
                        arguments.push(Argument::new(None, expr));
                    }

                    let next = self.next_skip_whitespace_and_line_break(false)?;
                    match next {
                        Some(LexToken::Symbol(Symbol::Comma)) => continue,
                        Some(LexToken::Symbol(ref symbol)) if symbol == &end_symbol => break,
                        _ => {
                            return Err(ParseError(format!(
                                "Received invalid symbol during parse_argument_list: {:?}",
                                next
                            )))
                        }
                    }
                }
            }

            _ => {
                return Err(ParseError(format!(
                "Expected begin_symbol at start of parse_argument_list. Expected: {:?}, got: {:?}",
                begin_symbol, current
            )))
            }
        }

        Ok(arguments)
    }

    fn parse_parameter_list(
        &mut self,
        begin_symbol: Symbol,
        end_symbol: Symbol,
    ) -> CustomResult<Vec<Variable>> {
        let mut parameters = Vec::new();

        let current = self.next_skip_whitespace_and_line_break(false)?;
        match current {
            Some(LexToken::Symbol(ref symbol)) if symbol == &begin_symbol => {
                // Edge case if there are no parameters
                let peek = self.peek_skip_whitespace_and_line_break()?;
                match peek {
                    LexToken::Symbol(ref symbol) if symbol == &end_symbol => {
                        self.next_skip_whitespace_and_line_break(false)?; // Remove end symbol.
                        return Ok(parameters);
                    }
                    _ => (),
                }

                // Loop through the parameters and parse one at a time until the end parenthesis.
                loop {
                    self.skip_whitespace_and_line_break(false)?;
                    let mut current_parameter = self.parse_variable()?;

                    // If true: this parameter has a default value set.
                    if let Some(LexToken::Symbol(Symbol::Equals)) = self.peek_skip_whitespace() {
                        // TODO: parse_expression breaks at end of parenthesis.
                        //  Fix so that parenthesis is allowed inside the expressions.
                        self.next_skip_whitespace(); // Remove Equals symbol.
                        let expression =
                            self.parse_expression(stop_on!(Symbol::Comma, Symbol::ParenthesisEnd))?;

                        current_parameter.value = Some(Box::new(expression));
                    }

                    parameters.push(current_parameter);

                    let next = self.next_skip_whitespace_and_line_break(false)?;
                    match next {
                        Some(LexToken::Symbol(Symbol::Comma)) => continue,
                        Some(LexToken::Symbol(ref symbol)) if symbol == &end_symbol => break,
                        _ => {
                            return Err(ParseError(format!(
                                "Received invalid symbol during parse_parameter_list: {:?}",
                                next
                            )))
                        }
                    }
                }
            }

            _ => {
                return Err(ParseError(format!(
                "Expected begin_symbol at start of parse_parameter_list. Expected: {:?}, got: {:?}",
                begin_symbol, current
            )))
            }
        }

        Ok(parameters)
    }

    fn parse_macro_call(&mut self, macro_name: &str) -> CustomResult<MacroCall> {
        let arguments =
            self.parse_argument_list(Symbol::CurlyBracketBegin, Symbol::CurlyBracketEnd)?;
        Ok(MacroCall::new(macro_name.to_string(), arguments))
    }

    fn parse_function_call(&mut self, function_name: &str) -> CustomResult<FunctionCall> {
        let arguments =
            self.parse_argument_list(Symbol::ParenthesisBegin, Symbol::ParenthesisEnd)?;
        Ok(FunctionCall::new(function_name.to_string(), arguments))
    }

    fn parse_block_header(&mut self, block_header: Block) -> CustomResult<Block> {
        match block_header {
            Block::Function(_) => self.parse_function_header(),

            Block::Class(_) => self.parse_class_header(),

            Block::Enum(_) => self.parse_enum_header(),

            Block::Interface(_) => self.parse_interface_header(),

            Block::Macro(_) => self.parse_macro_header(),

            Block::Constructor(_) => self.parse_constructor_header(),

            Block::Destructor => self.parse_destructor_header(),

            Block::If(_) => Ok(Block::If(Some(self.parse_expression_header()?))),

            Block::Else(_) => {
                if self.peek_skip_whitespace() == Some(LexToken::Symbol(Symbol::Colon)) {
                    self.assert_block_header_end()?;
                    Ok(Block::Else(None))
                } else {
                    Ok(Block::Else(Some(self.parse_expression_header()?)))
                }
            }

            Block::Match(_) => Ok(Block::Match(Some(self.parse_expression_header()?))),

            Block::For(_) => {
                // TODO: Allow for comma separated list of loops.
                let expr = self.parse_expression_header()?;
                if let Expression::Operation(Operation::BinaryOperation(Some(binary_operation))) =
                    &expr
                {
                    if binary_operation.operator == BinaryOperator::In {
                        return Ok(Block::For(Some(binary_operation.clone())));
                    }
                }

                Err(ParseError(
                    format!(
                        "Unable to parse and match \"for\" expression. Expected: In expression, got: {:?}",
                        expr
                    )
                ))
            }

            Block::With(_) => {
                // Edge case if empty expression list, is allowed.
                if let Some(LexToken::Symbol(Symbol::Colon)) = self.peek_skip_whitespace() {
                    self.assert_block_header_end()?;
                    Ok(Block::With(None))
                } else {
                    let expressions = self.parse_expression_list(stop_on!(Symbol::Colon))?;
                    Ok(Block::With(Some(expressions)))
                }
            }

            _ => Err(ParseError(format!(
                "Received invalid block_header in parse_statement: {:?}",
                block_header
            ))),
        }
    }

    // Parses a header in this order:
    //      name -> generic list -> arguments -> return type -> :
    // If one of the types isn't found, it tries to parse the next in the order.
    // The only mandatory thing is the ending Colon
    fn parse_generic_header(&mut self) -> CustomResult<GenericHeader> {
        let mut generic_header = GenericHeader::new();

        if let Some(LexToken::Identifier(_)) = self.peek_skip_whitespace() {
            generic_header.name = Some(self.next_identifier()?);
        }

        let generics = self.next_generic_list()?;
        if !generics.is_empty() {
            generic_header.generics = Some(generics);
        }

        let mut parameters = Vec::new();
        let peek = self.peek_skip_whitespace_and_line_break()?;
        if let LexToken::Symbol(Symbol::ParenthesisBegin) = peek {
            parameters =
                self.parse_parameter_list(Symbol::ParenthesisBegin, Symbol::ParenthesisEnd)?;
        }
        if !parameters.is_empty() {
            generic_header.parameters = Some(parameters);
        }

        let return_type = if let Some(LexToken::Symbol(Symbol::At)) = self.peek_skip_whitespace() {
            self.next_skip_whitespace(); // Remove At symbol.
            Some(self.next_type()?)
        } else {
            None
        };
        generic_header.return_type = return_type;

        self.assert_block_header_end()?;

        Ok(generic_header)
    }

    fn parse_class_header(&mut self) -> CustomResult<Block> {
        let class_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;
        let mut implements = Vec::new();

        // See if this class implements any interfaces.
        let peek = self.peek_skip_whitespace();
        if let Some(LexToken::Symbol(Symbol::Is)) = peek {
            self.next_skip_whitespace(); // Remove Is symbol.
            implements = self.next_type_list()?;
        }

        self.assert_block_header_end()?;

        Ok(Block::Class(Some(Struct::new(
            class_name, generics, implements,
        ))))
    }

    fn parse_function_header(&mut self) -> CustomResult<Block> {
        let generic_header = self.parse_generic_header()?;

        let name = generic_header
            .name
            .ok_or_else(|| ParseError("Function has no name specified.".to_string()))?;
        let generics = generic_header.generics;
        let parameters = generic_header.parameters;
        let return_type = if let Some(t) = generic_header.return_type {
            t
        } else {
            TypeStruct::new(Type::Void, None)
        };

        let function_header = Function::new(name, generics, parameters, Some(return_type));
        Ok(Block::Function(Some(function_header)))
    }

    fn parse_enum_header(&mut self) -> CustomResult<Block> {
        let generic_header = self.parse_generic_header()?;

        let name = generic_header
            .name
            .ok_or_else(|| ParseError("Enum has no name specified.".to_string()))?;
        let generics = if let Some(generics) = generic_header.generics {
            generics
        } else {
            Vec::new()
        };

        let enum_header = Enum::new(name, generics);
        Ok(Block::Enum(Some(enum_header)))
    }

    fn parse_interface_header(&mut self) -> CustomResult<Block> {
        let generic_header = self.parse_generic_header()?;

        let name = generic_header
            .name
            .ok_or_else(|| ParseError("Interface has no name specified.".to_string()))?;
        let generics = if let Some(generics) = generic_header.generics {
            generics
        } else {
            Vec::new()
        };

        let interface_header = Interface::new(name, generics);
        Ok(Block::Interface(Some(interface_header)))
    }

    fn parse_macro_header(&mut self) -> CustomResult<Block> {
        let generic_header = self.parse_generic_header()?;

        let name = generic_header
            .name
            .ok_or_else(|| ParseError("Macro has no name specified.".to_string()))?;
        let generics = if let Some(generics) = generic_header.generics {
            generics
        } else {
            Vec::new()
        };
        let parameters = generic_header
            .parameters
            .ok_or_else(|| ParseError("Macro has no parameters specified.".to_string()))?;

        let macro_header = Macro::new(name, generics, parameters);
        Ok(Block::Macro(Some(macro_header)))
    }

    fn parse_constructor_header(&mut self) -> CustomResult<Block> {
        let generic_header = self.parse_generic_header()?;

        let generics = if let Some(generics) = generic_header.generics {
            generics
        } else {
            Vec::new()
        };
        let parameters = generic_header
            .parameters
            .ok_or_else(|| ParseError("Constructor has no parameters specified.".to_string()))?;

        let constructor_header = Constructor::new(generics, parameters);
        Ok(Block::Constructor(Some(constructor_header)))
    }

    fn parse_destructor_header(&mut self) -> CustomResult<Block> {
        let next = self.next_skip_whitespace();
        if let Some(LexToken::Symbol(Symbol::ParenthesisBegin)) = next {
            let next = self.next_skip_whitespace_and_line_break(false)?;
            if let Some(LexToken::Symbol(Symbol::ParenthesisEnd)) = next {
                Ok(Block::Destructor)
            } else {
                Err(ParseError(format!(
                    "Expected ParenthesisEnd in destructor header, got: {:?}",
                    next
                )))
            }
        } else {
            Err(ParseError(format!(
                "Expected ParenthesisBegin in destructor header, got: {:?}",
                next
            )))
        }
    }

    fn parse_expression_header(&mut self) -> CustomResult<Expression> {
        let expression = self.parse_expression(stop_on!(Symbol::Colon))?;
        self.assert_block_header_end()?;
        Ok(expression)
    }
}
*/