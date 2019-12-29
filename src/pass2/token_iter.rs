use std::str::Chars;
use itertools::{PutBack, put_back};

use crate::pass2::lexer::CustomResult;
use crate::error::CustomError::Pass1Error;
use crate::pass2::token::{Token, Statement, Expression, BlockHeader, BinaryOperation, UnaryOperation, Class, Function, FunctionCall};
use std::error::Error;
use std::fs::File;
use std::io::Read;
use crate::pass1::simple_token::{SimpleToken, Symbol};
use std::collections::LinkedList;
use crate::pass1::simple_token::Symbol::LineBreak;

const AMOUNT_OF_PREVIOUS_TOKENS: usize = 1 << 16;

pub struct TokenIter<'a> {
    simple_tokens: Vec<SimpleToken>,
    position: usize,

    current_group_simple_tokens: Vec<SimpleToken>,
    current_group_position: usize,
    open_parenthesis: usize,
    current_indent: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(simple_tokens: Vec<SimpleToken>) -> Self {
        TokenIter {
            simple_tokens,
            position: 0,
            current_group_simple_tokens: Vec::new(),
            current_group_position: 0,
            open_parenthesis: 0,
            current_indent: 0,
        }
    }

    #[inline]
    fn next(&mut self) -> Option<&SimpleToken> {
        if self.current_group_position >= self.current_group_simple_tokens.len() {
            None
        }

        let ret = self.current_group_simple_tokens.get(self.current_group_position);
        self.current_group_position += 1;
        ret
    }

    #[inline]
    pub fn skip(&mut self) {
        self.current_group_position += 1;
    }

    #[inline]
    pub fn peek(&mut self) -> Option<&SimpleToken> {
        if self.current_group_position >= self.current_group_simple_tokens.len() {
            None
        } else {
            self.current_group_simple_tokens.get(self.current_group_position)
        }
    }

    // WORKING HERE









    #[inline]
    pub fn peek_trim(&mut self) -> Option<&SimpleToken> {
        if self.current_group_position >= self.current_group_simple_tokens.len() {
            None
        } else {
            let next = self.current_group_simple_tokens.get(self.current_group_position);
            next.map_or(None, |x| x == SimpleToken)
            if let Some(n) = next {

            }
        }
    }

    fn refill_simple_token_group(&mut self) {
        self.current_group_simple_tokens = self.get_simple_tokens_until_end_break();
        self.current_group_position = 0;
    }

    // (line break or manual break(;))
    fn get_simple_tokens_until_end_break(&mut self) -> Vec<SimpleToken> {
        let mut simple_tokens_group = Vec::new();

        for i in 0.. {
            if let Some(current) = self.simple_tokens.get(self.position + i) {
                simple_tokens_group.push(current.to_owned());

                if ((*current == SimpleToken::Symbol(LineBreak) || *current == SimpleToken::Symbol(Symbol::SemiColon))
                    && self.prev_ignores_end_break(i + self.position))
                    || *current == SimpleToken::EndOfFile
                {
                    break;
                }
            } else {
                break;
            }
        }

        self.position += simple_tokens_group.len();
        simple_tokens_group
    }

    fn prev_ignores_end_break(&mut self, current_pos: usize) -> bool {
        if current_pos - 1 < 0 {
            return false;
        }

        let previous_pos = current_pos - 1;
        if let Some(previous_simple_token) = self.simple_tokens.get(previous_pos) {
            match previous_simple_token {
                | SimpleToken::Symbol(Symbol::Dot)
                | SimpleToken::Symbol(Symbol::Comma)
                | SimpleToken::Symbol(Symbol::Equals) => true,
                _ => false
            }
        } else {
            false
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let mut count = 0;

        self.refill_simple_token_group();

        while let Some(current) = self.next() {
            match current {
                SimpleToken::Identifier(id) => {
                    if let Some(t) = self.lookup_identifier(id) {

                    }
                }
                SimpleToken::Number(number) => {}
                SimpleToken::Symbol(symbol) => {}
                SimpleToken::EndOfFile => {}
                SimpleToken::Unknown(string) => {}
            }
        }
    }

    fn next_expression(&mut self) -> Option<Expression> {
        let mut parenthesis = false;
        while let Some(simple_token) = self.next() {
            if simple_token == SimpleToken::Symbol(Symbol::ParenthesisBegin) {
                parenthesis += 1;
            } else if
        }

        None
    }


    fn parse_identifier(&mut self, id: String) -> Token {}


    pub fn next_while_true(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, true)
    }

    pub fn next_while_false(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, false)
    }

    pub fn next_char(&mut self) -> Option<char> {
        self.trim();
        self.it.next()
    }
    /*
        pub fn next_token(&mut self) -> CustomResult<Token> {
            if let Some(next_char) = self.trim_peek() {
                // TODO: hex/binary
                let radix = 10;
                if next_char.is_digit(radix) || next_char == '-' || next_char == '+' {
                    return Ok(self.next_number(radix)?);
                } else if next_char.is_alphanumeric() || next_char == '_' {
                    return Ok(self.next_identifier()?);
                }
            } else {
                Ok(Token::Control(Control::NewLine))
            }
        }
        */

    pub fn next_number(&mut self, radix: u32) -> CustomResult<Token> {
        self.trim();

        // Will be multiplied with the result to "decide" plus or minus sign.
        let sign = if let Some(first_char) = self.next_char() {
            if first_char == '-' {
                -1
            } else if first_char == '+' {
                1
            } else {    // a digit
                self.it.put_back(first_char);
                1
            }
        } else {
            return Err(Pass1Error("EOF while reading start of number.".to_string()));
        };

        let number = self.next_integer(radix)?;
        // True if float number.
        if let Some('.') = self.trim_peek() {
            self.next_char();   // Remove dot.
            let decimal_part = self.next_integer(radix)?;

            let float = [number, decimal_part]
                .join(".")
                .parse::<f64>()
                .map_err(|e| Pass1Error(e.description().to_string()))?;
            let float_signed = float * sign as f64;

            Ok(Token::ShortHand(ShortHand::Float(float_signed)))
        } else {
            let integer = number
                .parse::<i128>()
                .map_err(|e| Pass1Error(e.description().to_string()))?;
            let integer_signed = integer * sign;

            Ok(Token::ShortHand(ShortHand::Integer(integer_signed)))
        }

        // TODO: check case (as) and suffix etc.
    }

    pub fn next_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();
        while let Some(next_char) = self.it.next() {
            if next_char.is_digit(radix) {
                numbers.push(next_char);
            } else {
                self.it.put_back(next_char);
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    pub fn next_word(&mut self) -> CustomResult<String> {
        self.next_while_false(&|c| -> bool { c.is_whitespace() })
    }

    pub fn next_identifier(&mut self) -> CustomResult<String> {
        self.next_while_true(&|c| -> bool { c.is_alphanumeric() || c == '_' })
    }

    pub fn next_type(&mut self) -> CustomResult<String> {
        let mut result = Vec::new();
        result.push(self.next_identifier()?);

        if let Some('<') = self.trim_peek() {
            result.push(self.next_generic()?);
        }

        Ok(result.join(""))
    }
    /*
        pub fn next_expression(&mut self) -> CustomResult<Token>  {
            let name = self.next_identifier()?;
            let token = Token::lookup(name)?;
            match token {
                Token::Header(Header::If) =>
                _ => Err(LexError("Incorrect match of token in next_expression.".to_string()))
            }
        }
        */

    /*
    pub fn next_keyword(&mut self) -> CustomResult<Token> {
        let name = self.next_identifier()?;
        let token = Token::lookup(name);
    }
    */

    fn next_generic(&mut self) -> CustomResult<String> {
        let mut result: Vec<String> = Vec::new();

        if let Some('<') = self.next_char() {
            result.push('<'.to_string());
        } else {
            return Err(Pass1Error("next_generic doesn't start with a '<'.".to_string()));
        }

        result.push(self.next_identifier()?);

        loop {
            if let Some(next_char) = self.next_char() {
                result.push(next_char.to_string());

                match next_char {
                    ',' | '<' => result.push(self.next_type()?),
                    '>' => break,
                    _ => return Err(Pass1Error("Incorrect char in match of next_generic.".to_string()))
                };
            } else {
                return Err(Pass1Error("Reached EOF while looping char for next_generic.".to_string()));
            }
        }

        Ok(result.join(""))
    }

    /*
    pub fn next_boolean_statement(&mut self) -> CustomResult<String> {
        self.next_while_true(&|c| -> bool { c.is_alphanumeric() || c.is_digit(10) || c == '_' || c == '<' || c == '>' })
    }
    */

    pub fn next_return_type(&mut self) -> CustomResult<Token> {
        if let Some(next_char) = self.next_char() {
            if next_char == '-' && self.next_char() == Some('>') {
                Ok(Token::ReturnType(self.next_type()?))
            } else {
                Err(Pass1Error("Incorrect return type syntax.".to_string()))
            }
        } else {
            Ok(Token::ReturnType(String::from("")))
        }
    }

    pub fn next_arguments(&mut self) -> CustomResult<Vec<Token>> {
        self.trim();

        // TODO: inverse
        if let Some('(') = self.next_char() {} else {
            return Err(Pass1Error("Expected start curly bracket while lexing arguments.".to_string()));
        }

        let mut arguments: Vec<Token> = Vec::new();
        loop {
            let name = self.next_identifier()?;

            if let Some(next_char) = self.next_char() {
                let var_type =
                    if next_char == ':' {
                        self.next_type()?
                    } else {
                        return Err(Pass1Error("No type specifier char while parsing argument.".to_string()));
                    };
                arguments.push(Token::Variable(Variable { name, var_type }));
            }

            if let Some(next_char) = self.next_char() {
                if next_char == ')' {
                    break;
                } else if next_char == ',' {
                    continue;
                } else {
                    return Err(Pass1Error("Incorrect separator between arguments or missing end parenthesis.".to_string()));
                }
            }
        }

        Ok(arguments)
    }

    pub fn lookup_identifier(&mut self, name: &str) -> Option<Statement> {
        Some(
            match name {
                "return" => Statement::Return(self.next_expression()),
                "break" => {
                    if let Some(simple_token) = self.peek() {

                    }
                    Statement::Break(None)
                },
                "continue" => Statement::Continue,

                "use" => Statement::Use(None),
                "package" => Statement::Package(None),
                "throw" => Statement::Throw(None),

                "var" => Statement::Var(None),
                "let" => Statement::Let(None),

                "function" => BlockHeader::Function(None),
                "class" => BlockHeader::Class(None),
                "enum" => BlockHeader::Enum(None),

                "if" => BlockHeader::If(None),
                "else" => {
                    // TODO: see if else if here (?)
                    BlockHeader::Else
                }
                "match" => BlockHeader::Match(None),

                "for" => BlockHeader::For(None),
                "while" => BlockHeader::While(None),
                "loop" => BlockHeader::Loop,

                "catch" => BlockHeader::Catch(None),
                // TODO :test

                _ => return None
            }
        )
    }

    pub fn lookup(simple_token: SimpleToken) -> Option<(SimpleToken, usize)> {
        match simple_token.0 {
            SimpleToken::
        }
        // The "..._tuple" variables has the type (char, SimpleToken::Symbol).
        // The macro tries to match the char in "m_tuple" to the char in "second_char_option".
        macro_rules! match_symbol {
            ( $first_char_tuple:expr, $second_char_option:expr, $( $m_tuple:expr ),+ ) => {
                match $second_char_option {
                    $( Some(second_char) if second_char == $m_tuple.0 => (SimpleToken::Symbol($m_tuple.1), 2), )+
                    None | Some(_) => (SimpleToken::Symbol($first_char_tuple.1), 1)
                }
            }
        }

        Some(
            match c {
                '(' => (SimpleToken::Symbol(ParenthesisBegin), 1),
                ')' => (SimpleToken::Symbol(ParenthesisEnd), 1),
                '[' => (SimpleToken::Symbol(SquareBracketBegin), 1),
                ']' => (SimpleToken::Symbol(SquareBracketEnd), 1),
                '{' => (SimpleToken::Symbol(CurlyBracketBegin), 1),
                '}' => (SimpleToken::Symbol(CurlyBracketEnd), 1),
                ',' => (SimpleToken::Symbol(Comma), 1),
                '?' => (SimpleToken::Symbol(QuestionMark), 1),
                '\"' => (SimpleToken::Symbol(DoubleQuote), 1),
                '\'' => (SimpleToken::Symbol(SingleQuote), 1),
                ':' => (SimpleToken::Symbol(Colon), 1),
                ';' => (SimpleToken::Symbol(SemiColon), 1),
                '%' => (SimpleToken::Symbol(Modulus), 1),
                '&' => (SimpleToken::Symbol(BitAnd), 1),
                '^' => (SimpleToken::Symbol(BitXor), 1),
                '~' => (SimpleToken::Symbol(BitCompliment), 1),
                '#' => (SimpleToken::Symbol(Annotation), 1),

                // The out-commented symbols underneath are "matched" in other ways.
                //"\n" => SimpleToken::Symbol(Symbol::LineBreak),
                //"\r\n" => SimpleToken::Symbol(Symbol::LineBreak),
                //" " => SimpleToken::Symbol(Symbol::WhiteSpace(1)),
                //"\t" => SimpleToken::Symbol(Symbol::WhiteSpace(1)),

                //  !  !=
                '!' => {
                    match_symbol!(
                         (c, ExclamationMark),
                         c_next,
                         ('=', NotEquals)
                    )
                }

                //  |  |>
                '|' => {
                    match_symbol!(
                         (c, BitOr),
                         c_next,
                         ('>', Pipe)
                    )
                }


                //  .  ..
                '.' => {
                    match_symbol!(
                        (c, Dot),
                        c_next,
                        ('.', Range)
                    )
                }

                //  =  ==
                '=' => {
                    match_symbol!(
                        (c, Equals),
                        c_next,
                        ('=', EqualsOperator)
                    )
                }

                //  +  ++
                '+' => {
                    match_symbol!(
                        (c, Addition),
                        c_next,
                        ('+', Increment)
                    )
                }

                //  <  <=  <<
                '<' => {
                    match_symbol!(
                        (c, PointyBracketBegin),
                        c_next,
                        ('=', LessThanOrEquals),
                        ('<', ShiftLeft)
                    )
                }

                //  >  >=  >>
                '>' => {
                    match_symbol!(
                        (c, PointyBracketEnd),
                        c_next,
                        ('=', GreaterThanOrEquals),
                        ('>', ShiftRight)
                    )
                }

                //  -  --  ->
                '-' => {
                    match_symbol!(
                        (c, Subtraction),
                        c_next,
                        ('-', Decrement),
                        ('>', Arrow)
                    )
                }

                //  *  */ **
                '*' => {
                    match_symbol!(
                        (c, Multiplication),
                        c_next,
                        ('/', CommentMultiLineEnd),
                        ('*', Power)
                    )
                }

                //  /  //  /*
                '/' => {
                    match_symbol!(
                        (c, Division),
                        c_next,
                        ('/', CommentSingleLine),
                        ('*', CommentMultiLineBegin)
                    )
                }

                _ => return None
            }
        )
    }
}