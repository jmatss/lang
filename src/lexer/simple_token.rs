use crate::lexer::simple_token::Symbol::{
    Address, Arrow, As, AssignAddition, AssignBitAnd, AssignBitOr, AssignBitXor, AssignDivision,
    AssignModulus, AssignMultiplication, AssignPower, AssignShiftLeft, AssignShiftRight,
    AssignSubtraction, At, BitAnd, BitCompliment, BitOr, BitXor, BoolAnd, BoolNot, BoolOr, Colon,
    Comma, CommentMultiLineBegin, CommentMultiLineEnd, CommentSingleLine, CurlyBracketBegin,
    CurlyBracketEnd, Decrement, Deref, Division, Dollar, Dot, DoubleQuote, Equals, EqualsOperator,
    ExclamationMark, GreaterThanOrEquals, In, Increment, Is, LessThanOrEquals, LineBreak, Minus,
    Modulus, Multiplication, NotEquals, Of, ParenthesisBegin, ParenthesisEnd, Pipe, Plus,
    PointyBracketBegin, PointyBracketEnd, Pound, Power, QuestionMark, Range, RangeInclusive,
    SemiColon, ShiftLeft, ShiftRight, SingleQuote, SquareBracketBegin, SquareBracketEnd,
    WhiteSpace,
};

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

    // Pipe: |>  Range: ..  RangeInclusive: ..=  Arrow: ->  Deref: .*  Address: .&
    Pipe,
    Range,
    RangeInclusive,
    Arrow,
    Deref,
    Address,

    EqualsOperator,
    NotEquals,
    //LessThan, == PointyBracketBegin
    //GreaterThan, == PointyBracketEnd
    LessThanOrEquals,
    GreaterThanOrEquals,

    Plus,
    Minus,
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

impl SimpleToken {
    // Linebreak, manual linebreak (";") or EndOfFile.
    pub fn is_break_symbol(simple_token: &SimpleToken) -> bool {
        match simple_token {
            SimpleToken::Symbol(Symbol::LineBreak)
            | SimpleToken::Symbol(Symbol::SemiColon)
            | SimpleToken::EndOfFile => true,
            _ => false,
        }
    }

    pub fn lookup_identifier(name: &str) -> Option<SimpleToken> {
        Some(match name {
            "not" => SimpleToken::Symbol(BoolNot),
            "and" => SimpleToken::Symbol(BoolAnd),
            "or" => SimpleToken::Symbol(BoolOr),

            "in" => SimpleToken::Symbol(In),
            "is" => SimpleToken::Symbol(Is),
            "as" => SimpleToken::Symbol(As),
            "of" => SimpleToken::Symbol(Of),

            _ => return None,
        })
    }

    pub fn lookup_one(c1: char) -> Option<(SimpleToken, usize)> {
        SimpleToken::lookup(c1, None, None)
    }

    pub fn lookup_three(
        c1: char,
        c2: Option<char>,
        c3: Option<char>,
    ) -> Option<(SimpleToken, usize)> {
        SimpleToken::lookup(c1, c2, c3)
    }

    fn lookup(c1: char, c2: Option<char>, c3: Option<char>) -> Option<(SimpleToken, usize)> {
        let mut tmp_chars = Vec::with_capacity(3);
        tmp_chars.push(c1);
        if c2.is_some() {
            tmp_chars.push(c2.expect("Unable to unwrap c2"));
            if c3.is_some() {
                tmp_chars.push(c3.expect("Unable to unwrap c3"));
            }
        }
        let real_string: String = tmp_chars.into_iter().collect();

        match c1 {
            '(' => SimpleToken::ret_single_lookup(ParenthesisBegin),
            ')' => SimpleToken::ret_single_lookup(ParenthesisEnd),
            '[' => SimpleToken::ret_single_lookup(SquareBracketBegin),
            ']' => SimpleToken::ret_single_lookup(SquareBracketEnd),
            '{' => SimpleToken::ret_single_lookup(CurlyBracketBegin),
            '}' => SimpleToken::ret_single_lookup(CurlyBracketEnd),
            ',' => SimpleToken::ret_single_lookup(Comma),
            '?' => SimpleToken::ret_single_lookup(QuestionMark),
            '\"' => SimpleToken::ret_single_lookup(DoubleQuote),
            '\'' => SimpleToken::ret_single_lookup(SingleQuote),
            ':' => SimpleToken::ret_single_lookup(Colon),
            ';' => SimpleToken::ret_single_lookup(SemiColon),
            '~' => SimpleToken::ret_single_lookup(BitCompliment),
            '#' => SimpleToken::ret_single_lookup(Pound),
            '@' => SimpleToken::ret_single_lookup(At),
            '$' => SimpleToken::ret_single_lookup(Dollar),

            '%' => {
                SimpleToken::match_symbol(&real_string, vec![("%", Modulus), ("%=", AssignModulus)])
            }

            '&' => {
                SimpleToken::match_symbol(&real_string, vec![("&", BitAnd), ("&=", AssignBitAnd)])
            }

            '^' => {
                SimpleToken::match_symbol(&real_string, vec![("^", BitXor), ("^=", AssignBitXor)])
            }

            '!' => SimpleToken::match_symbol(
                &real_string,
                vec![("!", ExclamationMark), ("!=", NotEquals)],
            ),

            '|' => SimpleToken::match_symbol(
                &real_string,
                vec![("|", BitOr), ("|>", Pipe), ("|=", AssignBitOr)],
            ),

            '.' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    (".", Dot),
                    ("..", Range),
                    (".*", Deref),
                    (".&", Address),
                    ("..=", RangeInclusive),
                ],
            ),

            '=' => {
                SimpleToken::match_symbol(&real_string, vec![("=", Equals), ("==", EqualsOperator)])
            }

            '+' => SimpleToken::match_symbol(
                &real_string,
                vec![("+", Plus), ("++", Increment), ("+=", AssignAddition)],
            ),

            '<' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    ("<", PointyBracketBegin),
                    ("<=", LessThanOrEquals),
                    ("<<", ShiftLeft),
                    ("<<=", AssignShiftLeft),
                ],
            ),

            '>' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    (">", PointyBracketEnd),
                    (">=", GreaterThanOrEquals),
                    (">>", ShiftRight),
                    (">>=", AssignShiftRight),
                ],
            ),

            '-' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    ("-", Minus),
                    ("--", Decrement),
                    ("->", Arrow),
                    ("-=", AssignSubtraction),
                ],
            ),

            '*' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    ("*", Multiplication),
                    ("*/", CommentMultiLineEnd),
                    ("**", Power),
                    ("*=", AssignMultiplication),
                    ("**=", AssignPower),
                ],
            ),

            '/' => SimpleToken::match_symbol(
                &real_string,
                vec![
                    ("/", Division),
                    ("//", CommentSingleLine),
                    ("/*", CommentMultiLineBegin),
                    ("/=", AssignDivision),
                ],
            ),

            _ => None,
        }
    }

    fn ret_single_lookup(symbol: Symbol) -> Option<(SimpleToken, usize)> {
        Some((SimpleToken::Symbol(symbol), 1))
    }

    fn match_symbol(
        real_string: &str,
        mut string_symbol_tuples: Vec<(&str, Symbol)>,
    ) -> Option<(SimpleToken, usize)> {
        // Sort the tuples by the length of their "strings".
        // This ensures that the longest symbols are matched
        // (ex. "==" is always matched instead of "=").
        string_symbol_tuples.sort_unstable_by(|a, b| b.0.len().cmp(&a.0.len()));

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
