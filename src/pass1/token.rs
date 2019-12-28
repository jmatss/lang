use crate::lexer::CustomResult;
use crate::error::CustomError::LexError;
use crate::pass1::token::Symbol::{ParenthesisBegin, ParenthesisEnd, SquareBracketBegin, SquareBracketEnd,
                                  CurlyBracketBegin, CurlyBracketEnd, PointyBracketBegin, PointyBracketEnd,
                                  Dot, Comma, Equals, DoubleQuote, SingleQuote, Colon, SemiColon, LineBreak,
                                  WhiteSpace, Pipe, Range, Arrow, EqualsOperator, NotEquals, LessThan, GreaterThan,
                                  LessThanOrEquals, GreaterThanOrEquals, Addition, Subtraction, Multiplication,
                                  Division, Modulus, Power, BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight,
                                  BitCompliment, Increment, Decrement, CommentSingleLine, CommentMultiLineBegin,
                                  CommentMultiLineEnd, Annotation, BoolNot, BoolAnd, BoolOr};

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Number(String),
    Symbol(Symbol),
    EndOfFile,
    Unknown(String),
}

#[derive(Debug, Clone)]
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
    Equals,
    DoubleQuote,
    SingleQuote,
    Colon,
    SemiColon,
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

    Increment,
    Decrement,

    CommentSingleLine,
    CommentMultiLineBegin,
    CommentMultiLineEnd,
    Annotation,

    BoolNot,
    BoolAnd,
    BoolOr,
}

impl Symbol {
    pub fn lookup_identifier(name: &str) -> Option<Token> {
        Some(
            match name {
                "not" => Token::Symbol(Symbol::BoolNot),
                "and" => Token::Symbol(Symbol::BoolAnd),
                "or" => Token::Symbol(Symbol::BoolOr),

                _ => return None
            }
        )
    }

    pub fn lookup(c: char, c_next: Option<char>) -> Option<Token> {
        // The "..._tuple" variables has the type (char, Token::Symbol).
        // The macro tries to match the char in "m_tuple" to the char in "second_char_option"
        // and returns that "m_tuple"'s Token::Symbol if it matches.
        macro_rules! match_symbol {
            ( $first_char_tuple:expr, $second_char_option:expr, $( $m_tuple:expr ),+ ) => {
                Token::Symbol(
                    match $second_char_option {
                        $( Some(second_char) if second_char == $m_tuple.0 => $m_tuple.1, )+
                        None | Some(_) => $first_char_tuple.1
                    }
                )
            }
        }

        Some(
            match c {
                '(' => Token::Symbol(Symbol::ParenthesisBegin),
                ')' => Token::Symbol(Symbol::ParenthesisEnd),
                '[' => Token::Symbol(Symbol::SquareBracketBegin),
                ']' => Token::Symbol(Symbol::SquareBracketEnd),
                '{' => Token::Symbol(Symbol::CurlyBracketBegin),
                '}' => Token::Symbol(Symbol::CurlyBracketEnd),
                ',' => Token::Symbol(Symbol::Comma),
                '\"' => Token::Symbol(Symbol::DoubleQuote),
                '\'' => Token::Symbol(Symbol::SingleQuote),
                ':' => Token::Symbol(Symbol::Colon),
                ';' => Token::Symbol(Symbol::SemiColon),
                '%' => Token::Symbol(Symbol::Modulus),
                '&' => Token::Symbol(Symbol::BitAnd),
                '^' => Token::Symbol(Symbol::BitXor),
                '~' => Token::Symbol(Symbol::BitCompliment),
                '#' => Token::Symbol(Symbol::Annotation),

                // The out-commented symbols underneath are "matched" in other ways.
                //"\n" => Token::Symbol(Symbol::LineBreak),
                //"\r\n" => Token::Symbol(Symbol::LineBreak),
                //" " => Token::Symbol(Symbol::WhiteSpace(1)),
                //"\t" => Token::Symbol(Symbol::WhiteSpace(1)),

                //  |  |>
                '|' => {
                    if let Some('>') = c_next {
                        Token::Symbol(Symbol::Pipe)
                    } else {
                        Token::Symbol(Symbol::BitOr)
                    }
                }

                //  !=
                '!' => {
                    if let Some('=') = c_next {
                        Token::Symbol(Symbol::NotEquals)
                    } else {
                        return None;
                    }
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

        /*
            if let Some('+') = c_next {
                Token::Symbol(Symbol::Increment)
            } else {
                Token::Symbol(Symbol::Addition)
            }
        */

        /*
            if let Some(c_next_val) = c_next {
                if c_next_val == '/' {
                    Token::Symbol(Symbol::CommentMultiLineEnd)
                } else if c_next_val == '*' {
                    Token::Symbol(Symbol::Power)
                } else {
                    Token::Symbol(Symbol::Multiplication)
                }
            } else {
                Token::Symbol(Symbol::Multiplication)
            }
        */

        /*
        Some(
            match name {
                "(" => Token::Symbol(Symbol::ParenthesisBegin),
                ")" => Token::Symbol(Symbol::ParenthesisEnd),
                "[" => Token::Symbol(Symbol::SquareBracketBegin),
                "]" => Token::Symbol(Symbol::SquareBracketEnd),
                "{" => Token::Symbol(Symbol::CurlyBracketBegin),
                "}" => Token::Symbol(Symbol::CurlyBracketEnd),
                "<" => Token::Symbol(Symbol::PointyBracketBegin),
                ">" => Token::Symbol(Symbol::PointyBracketEnd),

                "." => Token::Symbol(Symbol::Dot),
                "," => Token::Symbol(Symbol::Comma),
                "=" => Token::Symbol(Symbol::Equals),
                "\"" => Token::Symbol(Symbol::DoubleQuote),
                "'" => Token::Symbol(Symbol::SingleQuote),
                ":" => Token::Symbol(Symbol::Colon),
                ";" => Token::Symbol(Symbol::SemiColon),

                // The symbols underneath are "matched" in other ways.
                //"\n" => Token::Symbol(Symbol::LineBreak),
                //"\r\n" => Token::Symbol(Symbol::LineBreak),
                //" " => Token::Symbol(Symbol::WhiteSpace(1)),
                //"\t" => Token::Symbol(Symbol::WhiteSpace(1)),

                "|>" => Token::Symbol(Symbol::Pipe),
                ".." => Token::Symbol(Symbol::Range),
                "->" => Token::Symbol(Symbol::Arrow),

                "==" => Token::Symbol(Symbol::EqualsOperator),
                "!=" => Token::Symbol(Symbol::NotEquals),
                //"<" => Token::Symbol(Symbol::LessThan),
                //">" => Token::Symbol(Symbol::GreaterThan),
                "<=" => Token::Symbol(Symbol::LessThanOrEquals),
                ">=" => Token::Symbol(Symbol::GreaterThanOrEquals),

                "+" => Token::Symbol(Symbol::Addition),
                "-" => Token::Symbol(Symbol::Subtraction),
                "*" => Token::Symbol(Symbol::Multiplication),
                "/" => Token::Symbol(Symbol::Division),
                "%" => Token::Symbol(Symbol::Modulus),
                "**" => Token::Symbol(Symbol::Power),

                "&" => Token::Symbol(Symbol::BitAnd),
                "|" => Token::Symbol(Symbol::BitOr),
                "^" => Token::Symbol(Symbol::BitXor),
                "<<" => Token::Symbol(Symbol::ShiftLeft),
                ">>" => Token::Symbol(Symbol::ShiftRight),
                "~" => Token::Symbol(Symbol::BitCompliment),

                "++" => Token::Symbol(Symbol::Increment),
                "--" => Token::Symbol(Symbol::Decrement),

                "//" => Token::Symbol(Symbol::CommentSingleLine),
                "/*" => Token::Symbol(Symbol::CommentMultiLineBegin),
                "*/" => Token::Symbol(Symbol::CommentMultiLineEnd),
                "#" => Token::Symbol(Symbol::Annotation),

                _ => return None
            }
        )
        */
    }
    /*
        // TODO: make this macro with variadic input.
        fn match_three(
            c_next: Option<char>,
            (c, c_symbol): (char, Symbol),
            (c2, c2_symbol): (char, Symbol),
            (c3, c3_symbol): (char, Symbol),
        ) -> Token {
            Token::Symbol(
                if let Some(c_next_val) = c_next {
                    if c_next_val == c2 {
                        c2_symbol
                    } else if c_next_val == c3 {
                        c3_symbol
                    } else {
                        c_symbol
                    }
                } else {
                    c_symbol
                }
            )
        }
        */
}