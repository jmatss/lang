use crate::parser::token::Modifier::{Static, Private, Public};
use crate::lexer::simple_token::{SimpleToken, Symbol};

#[derive(Debug, Clone)]
pub enum Token {
    // TODO: Rust block (statement/expression (?)).
    Expression(Expression),
    Statement(Statement),
    BlockHeader(BlockHeader),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Option<Expression>),
    // Yield ~= Break with a value
    Yield(Option<Expression>),
    Break,
    // Continue == Next
    Continue,

    // TODO: Maybe something else other that String.
    Use(Option<Path>),
    Package(Option<Path>),
    // TODO: throw
    Throw(Option<Expression>),

    // static, private etc.
    Modifier(Modifier),

    // Special cases
    // Init: Used in constructor to initialize fields with same name as parameters of constructor.
    Init,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Option<Literal>),
    Integer(Option<String>),
    Float(Option<String>),

    Variable(Option<Variable>),
    ArrayAccess(Option<ArrayAccess>),
    FunctionCall(Option<FunctionCall>),

    Operation(Operation),
}

#[derive(Debug, Clone)]
pub enum Operation {
    BinaryOperation(Option<BinaryOperation>),
    UnaryOperation(Option<UnaryOperation>),
}

#[derive(Debug, Clone)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,
    Function(Option<Function>),
    Class(Option<Class>),
    Enum(Option<Enum>),
    Interface(Option<Interface>),

    If(Option<Expression>),
    ElseIf(Option<Expression>),
    // FIXME: Only use Else and no ElseIf (?)
    Else(Option<Expression>),

    // BinaryOperation == MatchCase
    // TODO: Check match cases somewhere else
    Match(Option<Expression>),
    // Defer == After
    Defer,

    // For == Iterate
    // BinaryOperation == In
    For(Option<BinaryOperation>),
    // TODO: Maybe merge while and loop (?)
    While(Option<Expression>),
    Loop,

    // Example:
    //      catch e: EOFException:
    // or
    //      catch e as EOFException:
    Catch(Option<Variable>),

    // TODO: (?)
    Test(Option<Function>),

    // Headers that isn't "real blocks". Only does declaration/assignment and doesn't end with ":".
    // (var, let, set)
    AssignBlockHeader(AssignBlockHeader),
}

#[derive(Debug, Clone)]
pub enum AssignBlockHeader {
    // BinaryOperation == Assignment
    // Var can be either assignment or declaration.
    Var,
    Let,
    Set,
}

#[derive(Debug, Clone)]
pub enum Modifier {
    Static,
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub enum Literal {
    StringLiteral(String),
    CharLiteral(String),
}

#[derive(Debug)]
pub enum Output {
    Operator(Operator),
    Value(Expression),
}

#[derive(Debug, Clone)]
pub enum Operator {
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    ParenthesisBegin,
    ParenthesisEnd,
}

// (evaluate_left_to_right, precedence)
impl Operator {
    // Precedence according to:
    //      https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html
    // and "power"  ~over "mul/div/mod" according to pythons:
    //      https://www.mathcs.emory.edu/~valerie/courses/fall10/155/resources/op_precedence.html
    // and old rust docs:
    //      https://web.archive.org/web/20160304121349/https://doc.rust-lang.org/reference.html#unary-operator-expressions

    /*
        Precedence:
            1   x++ x--
            2   ++x --x ~ !
            3   . (function calls etc.)
            4   as
            5   ** (power)
            6   * / %
            7   + -
            8   << >>
            9   < > <= >= is of
            10  == !=
            11  &
            12  ^
            13  |
            14  and (bool)
            15  or (bool)
            16  ..
            17  in
            18  = += -? *= /= %= **= &= |= ^= <<= >>=
    */
    fn lookup(&self) -> Option<(bool, usize)> {
        Some(
            match *self {
                Operator::UnaryOperator(UnaryOperator::IncrementPrefix) =>
                    (true, 2),
                Operator::UnaryOperator(UnaryOperator::IncrementPostfix) =>
                    (true, 1),
                Operator::UnaryOperator(UnaryOperator::DecrementPrefix) =>
                    (true, 2),
                Operator::UnaryOperator(UnaryOperator::DecrementPostfix) =>
                    (true, 1),

                Operator::UnaryOperator(UnaryOperator::BitCompliment) =>
                    (true, 2),
                Operator::UnaryOperator(UnaryOperator::BoolNot) =>
                    (true, 2),

                Operator::BinaryOperator(BinaryOperator::Assignment) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::In) =>
                    (false, 17),
                Operator::BinaryOperator(BinaryOperator::Is) =>
                    (true, 9),
                Operator::BinaryOperator(BinaryOperator::As) =>
                    (true, 4),
                Operator::BinaryOperator(BinaryOperator::Of) =>
                    (true, 9),
                Operator::BinaryOperator(BinaryOperator::Range) =>
                    (true, 16),
                Operator::BinaryOperator(BinaryOperator::Dot) =>
                    (true, 3),

                Operator::BinaryOperator(BinaryOperator::Equals) =>
                    (true, 10),
                Operator::BinaryOperator(BinaryOperator::NotEquals) =>
                    (true, 10),
                Operator::BinaryOperator(BinaryOperator::LessThan) =>
                    (true, 9),
                Operator::BinaryOperator(BinaryOperator::GreaterThan) =>
                    (true, 9),
                Operator::BinaryOperator(BinaryOperator::LessThanOrEquals) =>
                    (true, 9),
                Operator::BinaryOperator(BinaryOperator::GreaterThanOrEquals) =>
                    (true, 9),

                Operator::BinaryOperator(BinaryOperator::Addition) =>
                    (true, 7),
                Operator::BinaryOperator(BinaryOperator::Subtraction) =>
                    (true, 7),
                Operator::BinaryOperator(BinaryOperator::Multiplication) =>
                    (true, 6),
                Operator::BinaryOperator(BinaryOperator::Division) =>
                    (true, 6),
                Operator::BinaryOperator(BinaryOperator::Modulus) =>
                    (true, 6),
                Operator::BinaryOperator(BinaryOperator::Power) =>
                    (false, 5),

                Operator::BinaryOperator(BinaryOperator::BitAnd) =>
                    (true, 11),
                Operator::BinaryOperator(BinaryOperator::BitOr) =>
                    (true, 13),
                Operator::BinaryOperator(BinaryOperator::BitXor) =>
                    (true, 12),
                Operator::BinaryOperator(BinaryOperator::ShiftLeft) =>
                    (true, 8),
                Operator::BinaryOperator(BinaryOperator::ShiftRight) =>
                    (true, 8),

                Operator::BinaryOperator(BinaryOperator::BoolAnd) =>
                    (true, 14),
                Operator::BinaryOperator(BinaryOperator::BoolOr) =>
                    (true, 15),

                Operator::BinaryOperator(BinaryOperator::AssignAddition) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignSubtraction) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignMultiplication) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignDivision) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignModulus) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignPower) =>
                    (false, 18),

                Operator::BinaryOperator(BinaryOperator::AssignBitAnd) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignBitOr) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignBitXor) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignShiftLeft) =>
                    (false, 18),
                Operator::BinaryOperator(BinaryOperator::AssignShiftRight) =>
                    (false, 18),

                _ => return None
            }
        )
    }

    pub fn precedence(&self) -> Option<usize> {
        Some(self.lookup()?.1)
    }

    pub fn evaluate_left_to_right(&self) -> Option<bool> {
        Some(self.lookup()?.0)
    }
}

// TODO: Maybe add "of" (instanceof) and use "is" as pattern matching.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum BinaryOperator {
    /* GENERAL */
    Assignment,
    // Used in for loops etc.
    In,
    // pattern matching
    Is,
    // cast
    As,
    Of,
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

    // Example ExpressionAnd:
    //  for i in 0..1 and j in 0..1:
    //  with s = Scanner(stdin) and x = Abc():
    // Might use Comma instead:
    //  for i in 0..1, j in 0..1:
    //  with s = Scanner(stdin), x = Abc():
    ExpressionAnd,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    // Used to indicate that this is a increment or decrement operator.
    // Some extra logic needs to be run to figure put if it is pre- or postfix.
    IncrementIndicator,
    DecrementIndicator,

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
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryOperation {
    pub fn new(operator: BinaryOperator, left: Box<Expression>, right: Box<Expression>) -> Self {
        BinaryOperation { operator, left, right }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub value: Box<Expression>,
}

impl UnaryOperation {
    pub fn new(operator: UnaryOperator, value: Box<Expression>) -> Self {
        UnaryOperation { operator, value }
    }
}


#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub generics: Vec<Type>,
    pub implements: Vec<Type>,
    // TODO: extends: Vec<Type>
}

impl Class {
    pub fn new(name: String, generics: Vec<Type>, implements: Vec<Type>) -> Self {
        Class { name, generics, implements }
    }
}

// TODO: Add generic types for functions.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<Type>,
    pub parameters: Vec<Variable>,
    pub return_type: Type,
}

impl Function {
    pub fn new(name: String, generics: Vec<Type>, parameters: Vec<Variable>, return_type: Type) -> Self {
        Function { name, generics, parameters, return_type }
    }
}

// TODO: Add generic types for functions.
#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<Type>,
}

impl Enum {
    pub fn new(name: String, generics: Vec<Type>) -> Self {
        Enum { name, generics }
    }
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<Type>,
}

impl Interface {
    pub fn new(name: String, generics: Vec<Type>) -> Self {
        Interface { name, generics }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Argument>,
}

impl FunctionCall {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        FunctionCall { name, arguments }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    // Named used for named arguments.
    pub name: Option<String>,
    pub value: Expression,
}

impl Argument {
    pub fn new(name: Option<String>, value: Expression) -> Self {
        Argument { name, value }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Option<Type>,
    // value ~= default
    pub value: Option<Box<Expression>>,
    pub modifiers: Vec<Modifier>,
}

impl Variable {
    pub fn new(name: String) -> Self {
        Variable { name, var_type: None, value: None, modifiers: Vec::new() }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub variable: Variable,
    pub index: Box<Expression>,
}

impl ArrayAccess {
    pub fn new(variable: Variable, index: Box<Expression>) -> Self {
        ArrayAccess { variable, index }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    // None == void
    pub t: Option<String>,
    pub generics: Vec<Type>,
}

impl Type {
    pub fn new(t: Option<String>, generics: Vec<Type>) -> Self {
        Type { t, generics }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Path {
    pub identifiers: Vec<String>,
}

impl Path {
    pub fn new() -> Self {
        Path { identifiers: Vec::new() }
    }

    pub fn push(&mut self, identifier: String) {
        self.identifiers.push(identifier);
    }
}

impl Token {
    pub fn lookup_operator(symbol: Symbol) -> Option<Operator> {
        Some(
            match symbol {
                Symbol::ParenthesisBegin =>
                    Operator::ParenthesisBegin,
                Symbol::ParenthesisEnd =>
                    Operator::ParenthesisEnd,
                /*
                Symbol::SquareBracketBegin,
                Symbol::SquareBracketEnd,
                Symbol::CurlyBracketBegin,
                Symbol::CurlyBracketEnd,
                Symbol::PointyBracketBegin,
                Symbol::PointyBracketEnd,
                */

                Symbol::Dot =>
                    Operator::BinaryOperator(BinaryOperator::Dot),
                //Symbol::Comma,
                //Symbol::QuestionMark,
                //Symbol::ExclamationMark,
                Symbol::Equals =>
                    Operator::BinaryOperator(BinaryOperator::Assignment),
                //Symbol::DoubleQuote,
                //Symbol::SingleQuote,
                //Symbol::Colon,
                //Symbol::SemiColon,
                //Symbol::Pound,
                //Symbol::At,
                //Symbol::Dollar,
                //Symbol::LineBreak,
                //Symbol::WhiteSpace(usize),

                // Pipe: |>  Range: ..  Arrow: ->
                //Symbol::Pipe,
                Symbol::Range =>
                    Operator::BinaryOperator(BinaryOperator::Range),
                //Symbol::Arrow,

                Symbol::EqualsOperator =>
                    Operator::BinaryOperator(BinaryOperator::Equals),
                Symbol::NotEquals =>
                    Operator::BinaryOperator(BinaryOperator::NotEquals),
                Symbol::LessThan =>
                    Operator::BinaryOperator(BinaryOperator::LessThan),
                Symbol::GreaterThan =>
                    Operator::BinaryOperator(BinaryOperator::GreaterThan),
                Symbol::LessThanOrEquals =>
                    Operator::BinaryOperator(BinaryOperator::LessThanOrEquals),
                Symbol::GreaterThanOrEquals =>
                    Operator::BinaryOperator(BinaryOperator::GreaterThanOrEquals),

                Symbol::Addition =>
                    Operator::BinaryOperator(BinaryOperator::Addition),
                Symbol::Subtraction =>
                    Operator::BinaryOperator(BinaryOperator::Subtraction),
                Symbol::Multiplication =>
                    Operator::BinaryOperator(BinaryOperator::Multiplication),
                Symbol::Division =>
                    Operator::BinaryOperator(BinaryOperator::Division),
                Symbol::Modulus =>
                    Operator::BinaryOperator(BinaryOperator::Modulus),
                Symbol::Power =>
                    Operator::BinaryOperator(BinaryOperator::Power),

                Symbol::BitAnd =>
                    Operator::BinaryOperator(BinaryOperator::BitAnd),
                Symbol::BitOr =>
                    Operator::BinaryOperator(BinaryOperator::BitOr),
                Symbol::BitXor =>
                    Operator::BinaryOperator(BinaryOperator::BitXor),
                Symbol::ShiftLeft =>
                    Operator::BinaryOperator(BinaryOperator::ShiftLeft),
                Symbol::ShiftRight =>
                    Operator::BinaryOperator(BinaryOperator::ShiftRight),
                Symbol::BitCompliment =>
                    Operator::UnaryOperator(UnaryOperator::BitCompliment),

                // Special case
                // The caller needs to run some extra logic to figure out if is is pre- or postfix.
                Symbol::Increment =>
                    Operator::UnaryOperator(UnaryOperator::IncrementIndicator),
                Symbol::Decrement =>
                    Operator::UnaryOperator(UnaryOperator::DecrementIndicator),

                /*
                Symbol::CommentSingleLine),
                Symbol::CommentMultiLineBegin),
                Symbol::CommentMultiLineEnd,
                */

                Symbol::BoolNot =>
                    Operator::UnaryOperator(UnaryOperator::BoolNot),
                Symbol::BoolAnd =>
                    Operator::BinaryOperator(BinaryOperator::BoolAnd),
                Symbol::BoolOr =>
                    Operator::BinaryOperator(BinaryOperator::BoolOr),

                Symbol::In =>
                    Operator::BinaryOperator(BinaryOperator::In),
                Symbol::Is =>
                    Operator::BinaryOperator(BinaryOperator::Is),
                Symbol::As =>
                    Operator::BinaryOperator(BinaryOperator::As),
                Symbol::Of =>
                    Operator::BinaryOperator(BinaryOperator::Of),

                _ => return None,
            }
        )
    }

    pub fn lookup_identifier(name: &str) -> Option<Token> {
        Some(
            match name {
                "return" => Token::ret_statement(Statement::Return(None)),
                "yield" => Token::ret_statement(Statement::Yield(None)),
                "break" => Token::ret_statement(Statement::Break),
                "continue" => Token::ret_statement(Statement::Continue),
                "next" => Token::ret_statement(Statement::Continue),

                "use" => Token::ret_statement(Statement::Use(None)),
                "package" => Token::ret_statement(Statement::Package(None)),
                "throw" => Token::ret_statement(Statement::Throw(None)),

                "static" => Token::ret_statement(Statement::Modifier(Static)),
                "private" => Token::ret_statement(Statement::Modifier(Private)),
                "public" => Token::ret_statement(Statement::Modifier(Public)),

                "var" => Token::ret_assign_block_header(AssignBlockHeader::Var),
                "let" => Token::ret_assign_block_header(AssignBlockHeader::Let),
                "set" => Token::ret_assign_block_header(AssignBlockHeader::Set),

                "function" => Token::ret_block_header(BlockHeader::Function(None)),
                "class" => Token::ret_block_header(BlockHeader::Class(None)),
                "enum" => Token::ret_block_header(BlockHeader::Enum(None)),
                "interface" => Token::ret_block_header(BlockHeader::Interface(None)),

                "if" => Token::ret_block_header(BlockHeader::If(None)),
                "else" => {
                    // TODO: see if "else if" here (?)
                    Token::ret_block_header(BlockHeader::Else(None))
                }
                "match" => Token::ret_block_header(BlockHeader::Match(None)),
                "defer" => Token::ret_block_header(BlockHeader::Defer),
                "after" => Token::ret_block_header(BlockHeader::Defer),

                //"for" => Token::ret_block_header(BlockHeader::For(None)),
                "iterate" => Token::ret_block_header(BlockHeader::For(None)),
                "while" => Token::ret_block_header(BlockHeader::While(None)),
                "loop" => Token::ret_block_header(BlockHeader::Loop),

                "catch" => Token::ret_block_header(BlockHeader::Catch(None)),
                // TODO: test

                _ => return None
            }
        )
    }

    fn ret_statement(statement: Statement) -> Token {
        Token::Statement(statement)
    }

    fn ret_assign_block_header(assign_block_header: AssignBlockHeader) -> Token {
        Token::BlockHeader(BlockHeader::AssignBlockHeader(assign_block_header))
    }

    fn ret_block_header(block_header: BlockHeader) -> Token {
        Token::BlockHeader(block_header)
    }
}