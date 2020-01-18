use crate::pass1::simple_token::Symbol::{ParenthesisBegin, ParenthesisEnd, SquareBracketBegin, SquareBracketEnd, CurlyBracketBegin, CurlyBracketEnd, PointyBracketBegin, PointyBracketEnd, Dot, Comma, QuestionMark, ExclamationMark, Equals, DoubleQuote, SingleQuote, Colon, SemiColon, LineBreak, WhiteSpace, Pipe, Range, Arrow, EqualsOperator, NotEquals, LessThan, GreaterThan, LessThanOrEquals, GreaterThanOrEquals, Addition, Subtraction, Multiplication, Division, Modulus, Power, BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight, BitCompliment, Increment, Decrement, CommentSingleLine, CommentMultiLineBegin, CommentMultiLineEnd, BoolNot, BoolAnd, BoolOr, Pound, At, Dollar};

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleToken {
    Identifier(String),
    Number(String),
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

    Increment,
    Decrement,

    CommentSingleLine,
    CommentMultiLineBegin,
    CommentMultiLineEnd,

    BoolNot,
    BoolAnd,
    BoolOr,
}

impl Symbol {
    pub fn lookup_identifier(name: &str) -> Option<SimpleToken> {
        Some(
            match name {
                "not" => SimpleToken::Symbol(BoolNot),
                "and" => SimpleToken::Symbol(BoolAnd),
                "or" => SimpleToken::Symbol(BoolOr),

                _ => return None
            }
        )
    }

    pub fn lookup(c: char, c_next: Option<char>) -> Option<(SimpleToken, usize)> {
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
                '#' => (SimpleToken::Symbol(Pound), 1),
                '@' => (SimpleToken::Symbol(At), 1),
                '$' => (SimpleToken::Symbol(Dollar), 1),


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