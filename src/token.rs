use crate::error::CustomError::LexError;
use crate::error::CustomError;
use crate::token::Control::{ParenthesisBegin, ParenthesisEnd, SquareBracketBegin, SquareBracketEnd, CurlyBracketBegin, CurlyBracketEnd, PointyBracketBegin, PointyBracketEnd, NewLine, WhiteSpace, Comma, Dot, QuoteString, QuoteChar, Escape};
use crate::token::Header::{Function, HeaderEnd, Class, If, ElseIf, Else, Match, For, While, Loop, Test};
use crate::token::Comparator::{Equals, NotEquals, LessThan, GreaterThan, LessThanOrEquals, GreaterThanOrEquals};
use crate::token::Keyword::{Use, Package, Return, Yield, Break, Continue, Implements, Extends, This, Separator, Static, Throw, Catch};
use crate::token::BinaryOperator::{Assignment, In, Is, As, Addition, Subtraction, Multiplication, Division, Modulus, Power, BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight, BoolAnd, BoolOr};
use crate::token::UnaryOperator::{IncrementPrefix, IncrementPostfix, DecrementPrefix, DecrementPostfix, BitCompliment, BoolNot};
use crate::token::Fluff::{CommentSingleLine, CommentMultiLineBegin, CommentMultiLineEnd, Annotation};

#[derive(Debug)]
pub enum Token {
    Control(Control),
    Header(Header),
    Comparator(Comparator),
    Keyword(Keyword),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    Fluff(Fluff),
    Identifier(String),
    Type(String),
}

#[derive(Debug)]
pub enum Control {
    ParenthesisBegin,
    ParenthesisEnd,
    SquareBracketBegin,
    SquareBracketEnd,
    CurlyBracketBegin,
    CurlyBracketEnd,
    PointyBracketBegin,
    PointyBracketEnd,
    // TODO: Move string, char & escape to other category.
    QuoteString,
    QuoteChar,
    Escape,
    NewLine,
    WhiteSpace,
    Comma,
    // "Control" might not be a suitable category for dot, TODO: pipe (|>) operator.
    Dot,
}

// "Headers" for blocks.
#[derive(Debug)]
pub enum Header {
    // HeaderEnd == Body start (':' for now)
    HeaderEnd,
    Function,
    Class,
    If,
    ElseIf,
    Else,
    Match,
    For,
    While,
    Loop,
    Test,
}

#[derive(Debug)]
pub enum Comparator {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
}

#[derive(Debug)]
pub enum Keyword {
    /* GENERAL */
    // == import
    Use,
    Package,

    /* FUNCTION */
    Return,
    // Maybe(?)
    Yield,
    // Either takes an integer X to break X layers, or returns a value.
    Break,
    Continue,

    /* CLASS */
    Implements,
    Extends,
    This,
    // Static context
    // TODO: Class,

    /* VARIABLE */
    Separator,
    Static,

    /* EXCEPTION */
    Throw,
    Catch,
}

#[derive(Debug)]
pub enum BinaryOperator {
    /* GENERAL */
    Assignment,
    // Used in for loops etc.
    In,
    // instanceOf
    Is,
    // cast
    As,

    /* NUMBERS */
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    Power,
    /* NUMBERS (BIT) */
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    /* BOOL */
    BoolAnd,
    BoolOr,
}

#[derive(Debug)]
pub enum UnaryOperator {
    /* NUMBERS */
    IncrementPrefix,
    IncrementPostfix,
    DecrementPrefix,
    DecrementPostfix,
    /* NUMBERS (BIT) */
    BitCompliment,

    /* BOOL */
    BoolNot,
}

#[derive(Debug)]
pub enum Fluff {
    CommentSingleLine,
    CommentMultiLineBegin,
    CommentMultiLineEnd,
    // maybe (?)
    Annotation,
}

// TODO: binary/unary operators for char/text/byte as well(?)
//  ex concat.


impl Token {
    pub fn lookup(string: &str) -> Result<Token, CustomError> {
        Ok(
            match string {
                "(" => Token::Control(ParenthesisBegin),
                ")" => Token::Control(ParenthesisEnd),
                "[" => Token::Control(SquareBracketBegin),
                "]" => Token::Control(SquareBracketEnd),
                "{" => Token::Control(CurlyBracketBegin),
                "}" => Token::Control(CurlyBracketEnd),
                "<" => Token::Control(PointyBracketBegin),
                ">" => Token::Control(PointyBracketEnd),
                "\"" => Token::Control(QuoteString),
                "'" => Token::Control(QuoteChar),
                "\\" => Token::Control(Escape),
                "\n" => Token::Control(NewLine),
                "\r\n" => Token::Control(NewLine),
                " " => Token::Control(WhiteSpace),
                "\t" => Token::Control(WhiteSpace),
                "," => Token::Control(Comma),
                // "Control" might not be a suitable category for dot, TODO: pipe (|>) operator.
                "." => Token::Control(Dot),

                ":" => Token::Header(HeaderEnd),
                "function" => Token::Header(Function),
                "class" => Token::Header(Class),
                "if" => Token::Header(If),
                "else if" => Token::Header(ElseIf),
                "else" => Token::Header(Else),
                "match" => Token::Header(Match),
                "for" => Token::Header(For),
                "while" => Token::Header(While),
                "loop" => Token::Header(Loop),
                "test" => Token::Header(Test),

                "==" => Token::Comparator(Equals),
                "!=" => Token::Comparator(NotEquals),
                "<" => Token::Comparator(LessThan),
                ">" => Token::Comparator(GreaterThan),
                "<=" => Token::Comparator(LessThanOrEquals),
                ">=" => Token::Comparator(GreaterThanOrEquals),

                "use" => Token::Keyword(Use),
                "package" => Token::Keyword(Package),
                "return" => Token::Keyword(Return),
                "yield" => Token::Keyword(Yield),
                "break" => Token::Keyword(Break),
                "continue" => Token::Keyword(Continue),
                "implements" => Token::Keyword(Implements),
                "extends" => Token::Keyword(Extends),
                "this" => Token::Keyword(This),
                // TODO: "class" => Token::Keyword(Class),
                ":" => Token::Keyword(Separator),
                "static" => Token::Keyword(Static),
                "throw" => Token::Keyword(Throw),
                "catch" => Token::Keyword(Catch),

                "=" => Token::BinaryOperator(Assignment),
                "in" => Token::BinaryOperator(In),
                "is" => Token::BinaryOperator(Is),
                "as" => Token::BinaryOperator(As),
                "+" => Token::BinaryOperator(Addition),
                "-" => Token::BinaryOperator(Subtraction),
                "*" => Token::BinaryOperator(Multiplication),
                "/" => Token::BinaryOperator(Division),
                "%" => Token::BinaryOperator(Modulus),
                "**" => Token::BinaryOperator(Power),
                "&" => Token::BinaryOperator(BitAnd),
                "|" => Token::BinaryOperator(BitOr),
                "^" => Token::BinaryOperator(BitXor),
                "<<" => Token::BinaryOperator(ShiftLeft),
                ">>" => Token::BinaryOperator(ShiftRight),
                "and" => Token::BinaryOperator(BoolAnd),
                "or" => Token::BinaryOperator(BoolOr),

                "++" => Token::UnaryOperator(IncrementPrefix),
                "++" => Token::UnaryOperator(IncrementPostfix),
                "--" => Token::UnaryOperator(DecrementPrefix),
                "--" => Token::UnaryOperator(DecrementPostfix),
                "~" => Token::UnaryOperator(BitCompliment),
                "not" => Token::UnaryOperator(BoolNot),

                "#" => Token::Fluff(CommentSingleLine),
                "" => Token::Fluff(CommentMultiLineBegin),
                "" => Token::Fluff(CommentMultiLineEnd),
                "" => Token::Fluff(Annotation),
                _ => return Err(LexError("Incorrect string parsed."))
            }
        )
    }
}

pub struct Variable {
    id: String,
    var_type: String,
}