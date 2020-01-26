use crate::lexer::simple_token::Symbol::{ParenthesisBegin, ParenthesisEnd, SquareBracketBegin, SquareBracketEnd, CurlyBracketBegin, CurlyBracketEnd, PointyBracketBegin, PointyBracketEnd, Dot, Comma, QuestionMark, ExclamationMark, Equals, DoubleQuote, SingleQuote, Colon, SemiColon, LineBreak, WhiteSpace, Pipe, Range, Arrow, EqualsOperator, NotEquals, LessThan, GreaterThan, LessThanOrEquals, GreaterThanOrEquals, Addition, Subtraction, Multiplication, Division, Modulus, Power, BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight, BitCompliment, Increment, Decrement, CommentSingleLine, CommentMultiLineBegin, CommentMultiLineEnd, BoolNot, BoolAnd, BoolOr, Pound, At, Dollar, In, Is, As, AssignAddition, AssignSubtraction, AssignMultiplication, AssignDivision, AssignModulus, AssignPower, AssignBitAnd, AssignBitOr, AssignBitXor, AssignShiftLeft, AssignShiftRight, Of};

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleToken {
    Identifier(String),
    Number(String),
    Literal(Literal),
    Symbol(Symbol),
    EndOfFile,
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    ParenthesisBegin,
    ParenthesisEnd,
    SquareBracketBegin,
    SquareBracketEnd,
    CurlyBracketBegin,
    CurlyBracketEnd,
    PointyBracketBegin,
    PointyBracketEnd,

    Dot,
    Comma,
    QuestionMark,
    ExclamationMark,
    Equals,
    DoubleQuote,
    SingleQuote,
    Colon,
    SemiColon,
    Pound,
    At,
    Dollar,
    LineBreak,
    WhiteSpace(usize),

    // Pipe: |>  Range: ..  Arrow: ->
    Pipe,
    Range,
    Arrow,

    EqualsOperator,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,

    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Power,

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    BitCompliment,

    AssignAddition,
    AssignSubtraction,
    AssignMultiplication,
    AssignDivision,
    AssignModulus,
    AssignPower,

    AssignBitAnd,
    AssignBitOr,
    AssignBitXor,
    AssignShiftLeft,
    AssignShiftRight,

    Increment,
    Decrement,

    CommentSingleLine,
    CommentMultiLineBegin,
    CommentMultiLineEnd,

    BoolNot,
    BoolAnd,
    BoolOr,

    In,
    // Is == pattern matching
    Is,
    // cast
    As,
    // Of == instanceof
    Of,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    StringLiteral(String),
    CharLiteral(String),
}

impl Symbol {
    // Linebreak, manual linebreak (";") or EndOfFile.
    pub fn is_break_symbol(simple_token: &SimpleToken) -> bool {
        match simple_token {
            | SimpleToken::Symbol(Symbol::LineBreak)
            | SimpleToken::Symbol(Symbol::SemiColon)
            | SimpleToken::EndOfFile => true,
            _ => false
        }
    }

    pub fn lookup_identifier(name: &str) -> Option<SimpleToken> {
        Some(
            match name {
                "not" => SimpleToken::Symbol(BoolNot),
                "and" => SimpleToken::Symbol(BoolAnd),
                "or" => SimpleToken::Symbol(BoolOr),

                "in" => SimpleToken::Symbol(In),
                "is" => SimpleToken::Symbol(Is),
                "as" => SimpleToken::Symbol(As),
                "of" => SimpleToken::Symbol(Of),

                _ => return None
            }
        )
    }

    pub fn lookup_one(c1: char) -> Option<(SimpleToken, usize)> {
        Symbol::lookup(c1, None, None)
    }

    pub fn lookup_three(c1: char, c2: Option<char>, c3: Option<char>) -> Option<(SimpleToken, usize)> {
        Symbol::lookup(c1, c2, c3)
    }

    fn lookup(c1: char, c2: Option<char>, c3: Option<char>) -> Option<(SimpleToken, usize)> {
        let mut tmp_chars = Vec::with_capacity(3);
        tmp_chars.push(c1);
        if c2.is_some() {
            tmp_chars.push(c2.expect("Unable to unwrap c3"));
            if c3.is_some() {
                tmp_chars.push(c3.expect("Unable to unwrap c3"));
            }
        }
        let real_string: String = tmp_chars.into_iter().collect();

        match c1 {
            '(' => Symbol::ret_single_lookup(ParenthesisBegin),
            ')' => Symbol::ret_single_lookup(ParenthesisEnd),
            '[' => Symbol::ret_single_lookup(SquareBracketBegin),
            ']' => Symbol::ret_single_lookup(SquareBracketEnd),
            '{' => Symbol::ret_single_lookup(CurlyBracketBegin),
            '}' => Symbol::ret_single_lookup(CurlyBracketEnd),
            ',' => Symbol::ret_single_lookup(Comma),
            '?' => Symbol::ret_single_lookup(QuestionMark),
            '\"' => Symbol::ret_single_lookup(DoubleQuote),
            '\'' => Symbol::ret_single_lookup(SingleQuote),
            ':' => Symbol::ret_single_lookup(Colon),
            ';' => Symbol::ret_single_lookup(SemiColon),
            '~' => Symbol::ret_single_lookup(BitCompliment),
            '#' => Symbol::ret_single_lookup(Pound),
            '@' => Symbol::ret_single_lookup(At),
            '$' => Symbol::ret_single_lookup(Dollar),

            '%' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Modulus),
                        ("%=", AssignModulus)
                    ],
                )
            }

            '&' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), BitAnd),
                        ("&=", AssignBitAnd)
                    ],
                )
            }

            '^' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), BitXor),
                        ("^=", AssignBitXor)
                    ],
                )
            }

            '!' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), ExclamationMark),
                        ("!=", NotEquals)
                    ],
                )
            }

            '|' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), BitOr),
                        ("|>", Pipe),
                        ("|=", AssignBitOr)
                    ],
                )
            }

            '.' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Dot),
                        ("..", Range)
                    ],
                )
            }

            '=' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Equals),
                        ("==", EqualsOperator)
                    ],
                )
            }

            '+' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Addition),
                        ("++", Increment),
                        ("+=", AssignAddition)
                    ],
                )
            }

            '<' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), PointyBracketBegin),
                        ("<=", LessThanOrEquals),
                        ("<<", ShiftLeft),
                        ("<<=", AssignShiftLeft)
                    ],
                )
            }

            '>' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), PointyBracketEnd),
                        (">=", GreaterThanOrEquals),
                        (">>", ShiftRight),
                        (">>=", AssignShiftRight)
                    ],
                )
            }

            '-' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Subtraction),
                        ("--", Decrement),
                        ("->", Arrow),
                        ("-=", AssignSubtraction)
                    ],
                )
            }

            '*' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Multiplication),
                        ("*/", CommentMultiLineEnd),
                        ("**", Power),
                        ("*=", AssignMultiplication),
                        ("**=", AssignPower)
                    ],
                )
            }

            '/' => {
                Symbol::match_symbol(
                    &real_string,
                    vec![
                        (&c1.to_string(), Division),
                        ("//", CommentSingleLine),
                        ("/*", CommentMultiLineBegin),
                        ("/=", AssignDivision)
                    ],
                )
            }

            _ => None
        }
    }

    fn ret_single_lookup(symbol: Symbol) -> Option<(SimpleToken, usize)> {
        Some((SimpleToken::Symbol(symbol), 1))
    }


    fn match_symbol(
        real_string: &str, mut string_symbol_tuples: Vec<(&str, Symbol)>,
    ) -> Option<(SimpleToken, usize)> {
        // Sort the tuples by the length of their "strings".
        // This ensures that the longest symbols are matched
        // (ex. "==" is always matched instead of "=").
        string_symbol_tuples.sort_unstable_by(
            |a, b| b.0.len().cmp(&a.0.len())
        );

        for tuple in string_symbol_tuples {
            let current_string = tuple.0;
            let current_symbol = tuple.1;

            if real_string.starts_with(current_string) {
                return Some((SimpleToken::Symbol(current_symbol), current_string.len()));
            }
        }

        None
    }
}