use crate::lexer::CustomResult;
use crate::error::CustomError::LexError;
use crate::token::Control::{ParenthesisBegin, ParenthesisEnd, SquareBracketBegin, SquareBracketEnd, CurlyBracketBegin, CurlyBracketEnd, PointyBracketBegin, PointyBracketEnd, NewLine, WhiteSpace, Comma, Dot, QuoteString, QuoteChar, Escape};
use crate::token::Header::{HeaderEnd, If, ElseIf, Else, Match, For, While, Loop, Test, Enum, Function, Class, Let, Var};
use crate::token::Comparator::{Equals, NotEquals, LessThan, GreaterThan, LessThanOrEquals, GreaterThanOrEquals};
use crate::token::Keyword::{Use, Package, Return, Yield, Break, Continue, Implements, Extends, This, Separator, Static, Throw, Catch};
use crate::token::BinaryOperator::{Assignment, In, Is, As, Addition, Subtraction, Multiplication, Division, Modulus, Power, BitAnd, BitOr, BitXor, ShiftLeft, ShiftRight, BoolAnd, BoolOr, Range};
use crate::token::UnaryOperator::{IncrementPrefix, IncrementPostfix, DecrementPrefix, DecrementPostfix, BitCompliment, BoolNot};
use crate::token::Fluff::{CommentSingleLine, CommentMultiLineBegin, CommentMultiLineEnd, Annotation};

#[derive(Debug, Clone)]
pub enum Token {
    Control(Control),
    Header(Header),
    Comparator(Comparator),
    Keyword(Keyword),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    Fluff(Fluff),
    Variable(Variable),
    ShortHand(ShortHand),
    FunctionCall(String),
    Name(String),
    Type(String),
    ReturnType(String),
    Expression(Vec<Token>),
    Unknown(String)
}

#[derive(Debug, Clone)]
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
    WhiteSpace(usize),
    Comma,
    // "Control" might not be a suitable category for dot, TODO: pipe (|>) operator.
    Dot,
}

#[derive(Debug, Clone)]
pub enum Header {
    // HeaderEnd == Body start (':' for now)
    HeaderEnd,
    Function,
    Class,
    Enum,
    Var,
    Let,
    If,
    ElseIf,
    Else,
    Match,
    For,
    While,
    Loop,
    Test,
}

#[derive(Debug, Clone)]
pub enum Comparator {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ShortHand {
    Integer(i128),
    Float(f64),
    String(String),
    Char(char),
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    /* GENERAL */
    Assignment,
    // Used in for loops etc.
    In,
    // instanceOf
    Is,
    // cast
    As,
    Range,

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    pub fn lookup(name: String) -> CustomResult<Token> {
        Ok(
            match name.as_ref() {
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
                " " => Token::Control(WhiteSpace(1)),
                "\t" => Token::Control(WhiteSpace(1)),
                "," => Token::Control(Comma),
                // "Control" might not be a suitable category for dot, TODO: pipe (|>) operator.
                "." => Token::Control(Dot),

                ":" => Token::Header(HeaderEnd),
                "function" => Token::Header(Function),
                "class" => Token::Header(Class),
                "enum" => Token::Header(Enum),
                "var" => Token::Header(Var),
                "let" => Token::Header(Let),
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
                ".." => Token::BinaryOperator(Range),
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
                "#*" => Token::Fluff(CommentMultiLineBegin),
                "*#" => Token::Fluff(CommentMultiLineEnd),
                "@" => Token::Fluff(Annotation),
                _ => Token::Unknown(name)
            }
        )
    }
}
/*
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Variable>,
    pub return_type:  String,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub implements: Vec<String>,
    pub extends: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
}
*/

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: String,
}

#[derive(Debug, Clone)]
pub struct Indent {
    pub size: usize
}