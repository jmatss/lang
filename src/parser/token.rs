use crate::parser::token::NonBlockHeader::{Var, Let, Set};

#[derive(Debug, Clone)]
pub enum Token {
    // TODO: Rust block (statement/expression (?)).
    // EndBreak == LineBreak/ManualBreak (;).
    Expression(Expression),
    Statement(Statement),

    Block(BlockHeader, Vec<Token>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<Expression>),
    Break(Option<Expression>),
    Yield(Option<Expression>),
    // Continue == Next == Skip
    Continue,
    Next,
    Skip,

    // TODO: Maybe something else other that String.
    Use(Option<String>),
    Package(Option<String>),
    // TODO: throw
    Throw(Option<Expression>),

    BlockHeader(BlockHeader),
    BlockEnd,

    // Headers that isn't a real block, and doesn't end with ":". (var, let, set)
    NonBlockHeader(NonBlockHeader),
    NonBlockEnd,

    // Special cases
    // Init: Used in constructor to initialize fields with same name as parameters of constructor.
    Init,
}

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(Option<String>),
    CharLiteral(Option<char>),
    Integer(Option<String>),
    Float(Option<String>),
    Variable(Option<Variable>),

    FunctionCall(Option<FunctionCall>),

    BinaryOperation(Option<BinaryOperation>),
    UnaryOperation(Option<UnaryOperation>),
}

#[derive(Debug, Clone)]
pub enum BlockHeader {
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,
    Function(Option<Function>),
    Class(Option<Class>),
    // TODO: change inner of enum to something else.
    Enum(Option<Vec<Variable>>),

    If(Option<Expression>),
    ElseIf(Option<Expression>),
    Else,

    // BinaryOperation == MatchCa+se
    Match(Option<Vec<BinaryOperation>>),

    // BinaryOperation == In
    For(Option<BinaryOperation>),
    // For == Iterate
    Iterate(Option<BinaryOperation>),
    While(Option<Expression>),
    Loop,

    // Example:
    //      catch e: EOFException:
    // or
    //      catch e as EOFException:
    Catch(Option<Variable>),

    // TODO: (?)
    Test(Option<Function>),
}

#[derive(Debug, Clone)]
pub enum NonBlockHeader {
    // BinaryOperation == Assignment
    // Var can be either assignment or declaration.
    Var(Option<VarEnum>),
    Let(Option<BinaryOperation>),
    Set(Option<BinaryOperation>),
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

    // TODO: Make left-hand-side a pattern.
    /* Example of MatchCase where ("a": ; do_something()) is a MatchCase.
        match "a":
            "a":
                do_something()
            _:
                do_nothing()
        .
    */
    MatchCase,

    // Access function/fields ex. list.add(), tuple.0
    Dot,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,

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

    // Example ExpressionAnd:
    //  for i in 0..1 and j 0..1:
    //  with s = Scanner(stdin) and x = Abc():
    ExpressionAnd,
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

    // Declare a variable, ex.:
    //  var text_name @ Text
    // or
    //  var text_name
    Declaration,
}

#[derive(Debug, Clone)]
pub enum VarEnum {
    // BinaryOperation == Assignment
    BinaryOperation(BinaryOperation),
    // UnaryOperation == Declaration
    UnaryOperation(UnaryOperation),
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    operator: BinaryOperator,
    left: Box<Expression>,
    right: Box<Expression>,
}

impl BinaryOperation {
    pub fn new(operator: BinaryOperator, left: Box<Expression>, right: Box<Expression>) -> Self {
        BinaryOperation { operator, left, right }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    operator: UnaryOperator,
    value: Box<Expression>,
}

impl UnaryOperation {
    pub fn new(operator: UnaryOperator, value: Box<Expression>) -> Self {
        UnaryOperation { operator, value }
    }
}


#[derive(Debug, Clone)]
pub struct Class {
    name: String,
    implements: Vec<Type>,
    // TODO: extends: Vec<Type>
}

impl Class {
    pub fn new(name: String, implements: Vec<Type>) -> Self {
        Class { name, implements }
    }
}

// TODO: Add generic types for functions.
#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    parameters: Vec<Variable>,
    return_type: Type,
}

impl Function {
    pub fn new(name: String, parameters: Vec<Variable>, return_type: Type) -> Self {
        Function { name, parameters, return_type }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    name: String,
    arguments: Vec<Variable>,
}

impl FunctionCall {
    pub fn new(name: String, arguments: Vec<Variable>) -> Self {
        FunctionCall { name, arguments }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    var_type: Option<Type>,
    // value ~= default
    value: Option<Box<Expression>>,
}

impl Variable {
    pub fn new(name: String, var_type: Option<Type>, value: Option<Box<Expression>>) -> Self {
        Variable { name, value, var_type }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    t: String,
}

impl Type {
    pub fn new(t: String) -> Self {
        Type { t }
    }
}

impl Token {
    pub fn lookup_identifier(name: &str) -> Option<Statement> {
        Some(
            match name {
                "return" => Statement::Return(None),
                "break" => Statement::Break(None),
                "yield" => Statement::Yield(None),
                "continue" => Statement::Continue,
                "next" => Statement::Next,
                "skip" => Statement::Skip,

                "use" => Statement::Use(None),
                "package" => Statement::Package(None),
                "throw" => Statement::Throw(None),

                "var" => Statement::NonBlockHeader(Var(None)),
                "let" => Statement::NonBlockHeader(Let(None)),
                "set" => Statement::NonBlockHeader(Set(None)),

                "function" => Statement::BlockHeader(BlockHeader::Function(None)),
                "class" => Statement::BlockHeader(BlockHeader::Class(None)),
                "enum" => Statement::BlockHeader(BlockHeader::Enum(None)),

                "if" => Statement::BlockHeader(BlockHeader::If(None)),
                "else" => {
                    // TODO: see if else if here (?)
                    Statement::BlockHeader(BlockHeader::Else)
                }
                "match" => Statement::BlockHeader(BlockHeader::Match(None)),

                "for" => Statement::BlockHeader(BlockHeader::For(None)),
                "while" => Statement::BlockHeader(BlockHeader::While(None)),
                "loop" => Statement::BlockHeader(BlockHeader::Loop),

                "catch" => Statement::BlockHeader(BlockHeader::Catch(None)),
                // TODO :test

                _ => return None
            }
        )
    }
}

/*
    StringLiteral,
    CharLiteral,
    Break, // LineBreak/ManualBreak (;)
    Expression,
    Statement,  // return, break, continue, var, let
    BlockExpression // if/elseif/else, for, while, match
    BlockHeader,
    Block,

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

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: String,
}

#[derive(Debug, Clone)]
pub struct Indent {
    pub size: usize
}
*/