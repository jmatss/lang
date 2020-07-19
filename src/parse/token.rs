use crate::error::CustomError::ParseError;
use crate::{common::variable_type::Type, lex, CustomResult};

/// A unique number given to every block.
pub type BlockId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseToken {
    pub kind: ParseTokenKind,
    pub line_nr: u64,
    pub column_nr: u64,
}

impl ParseToken {
    pub fn new(kind: ParseTokenKind, line_nr: u64, column_nr: u64) -> Self {
        Self {
            kind,
            line_nr,
            column_nr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseTokenKind {
    // TODO: Rust/C block (statement/expression (?)).
    Expression(Expression),
    Statement(Statement),
    Block(BlockHeader, BlockId, Vec<ParseToken>),
    EndOfFile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Option<Expression>),
    // Yield ~= Break with a value
    Yield(Expression),
    Break,
    // Continue == Next
    Continue,

    Use(Path),
    Package(Path),

    // The expr will contain the assigned value if this is a initialization.
    // Used both for "var" and "const" variables.
    VariableDecl(Variable, Option<Expression>),
    // static, private etc.
    Modifier(Modifier),

    // Special cases
    // Init: Used in constructor to initialize fields with same name as parameters of constructor.
    Init,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Literal(lex::token::Literal, Option<TypeStruct>),

    Variable(Variable),
    //ArrayAccess(Option<ArrayAccess>),
    FunctionCall(FunctionCall),
    //MacroCall(Option<MacroCall>),
    Operation(Operation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Interface(Interface),

    // Any `IfCase` blocks should be grouped together under one `If`.
    // A `Ifcase` is a if/elif/else with its corresponding eval expression.
    //
    // Example 1:
    //   if(x) {} else {}
    // =>
    //   Block(If, [ Block(IfCase(x), ...), Block(IfCase(_), ...) ])
    //
    // Example 2:
    //   if (x) {
    //     if (y) {
    //       ...
    //     }
    //   } else (z) {
    //     ...
    //   } else {
    //     ...
    //   }
    // =>
    //   Block(
    //     header: If,
    //     body: [
    //        Block(
    //          header: IfCase(x),
    //          body: [
    //            Block(
    //              header: If,
    //              body: [
    //                Block(
    //                  header: IfCase(y),
    //                  body: [ ... ]
    //                )
    //              ]
    //            )
    //          ]
    //        ),
    //        Block(
    //          header: IfCase(z),
    //          body: [ ... ]
    //        ),
    //        Block(
    //          header: IfCase(_),
    //          body: [ ... ]
    //        )
    //     ]
    //   )
    If,
    IfCase(Option<Expression>),

    // Any `MatchCase` blocks should be grouped together under one `Match`.
    // See the example above for `If`, should be structured ~similarly.
    Match(Expression),
    MatchCase(Expression),

    For(Variable, Expression),
    // TODO: Maybe merge while and loop (?)
    While(Option<Expression>),

    // AutoClosable.
    With(Option<Expression>),
    // Defer -> Run this expression at the end of the current block.
    Defer(Option<Expression>),

    // Function for testing, allows strings as test names with spaces etc.
    Test(Option<Function>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Modifier {
    Var,
    Const,
    Static,
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub operator: BinaryOperator,
    pub ret_type: Option<TypeStruct>,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryOperation {
    pub fn new(operator: BinaryOperator, left: Box<Expression>, right: Box<Expression>) -> Self {
        BinaryOperation {
            operator,
            ret_type: None,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub ret_type: Option<TypeStruct>,
    pub value: Box<Expression>,
}

impl UnaryOperation {
    pub fn new(operator: UnaryOperator, value: Box<Expression>) -> Self {
        UnaryOperation {
            operator,
            ret_type: None,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<TypeStruct>,
    pub implements: Vec<TypeStruct>,
    // TODO: extends: Vec<Type>
}

impl Struct {
    pub fn new(name: String, generics: Vec<TypeStruct>, implements: Vec<TypeStruct>) -> Self {
        Struct {
            name,
            generics,
            implements,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub generics: Option<Vec<TypeStruct>>,
    pub parameters: Option<Vec<Variable>>,
    pub ret_type: Option<TypeStruct>,
}

impl Function {
    pub fn new(
        name: String,
        generics: Option<Vec<TypeStruct>>,
        parameters: Option<Vec<Variable>>,
        ret_type: Option<TypeStruct>,
    ) -> Self {
        Function {
            name,
            generics,
            parameters,
            ret_type,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<TypeStruct>,
}

impl Enum {
    pub fn new(name: String, generics: Vec<TypeStruct>) -> Self {
        Enum { name, generics }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub generics: Vec<TypeStruct>,
}

impl Interface {
    pub fn new(name: String, generics: Vec<TypeStruct>) -> Self {
        Interface { name, generics }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Argument>,
}

impl FunctionCall {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        FunctionCall { name, arguments }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub ret_type: Option<TypeStruct>,
    pub modifiers: Option<Vec<Modifier>>,
    pub const_: bool,
}

impl Variable {
    pub fn new(
        name: String,
        ret_type: Option<TypeStruct>,
        modifiers: Option<Vec<Modifier>>,
        const_: bool,
    ) -> Self {
        Variable {
            name,
            ret_type,
            modifiers,
            const_,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeStruct {
    // None == void
    pub t: Type,
    pub generics: Option<Vec<TypeStruct>>,
}

impl TypeStruct {
    pub fn new(t: Type, generics: Option<Vec<TypeStruct>>) -> Self {
        TypeStruct { t, generics }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    pub idents: Vec<String>,
}

impl Path {
    pub fn new(idents: Vec<String>) -> Self {
        Path { idents }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericHeader {
    pub name: Option<String>,
    pub generics: Option<Vec<TypeStruct>>,
    pub parameters: Option<Vec<Variable>>,
    pub ret_type: Option<TypeStruct>,
}

impl GenericHeader {
    pub fn new() -> Self {
        GenericHeader {
            name: None,
            generics: None,
            parameters: None,
            ret_type: None,
        }
    }
}

/// Used during parsing of expression so that both operators and operand can be
/// stored together in the same container.
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Operator(Operator),
    Operand(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),

    // Special operators that needs extra logic.
    // Need to figure out if plus/minus is Addition/Subtraction(binary) or
    // Positive/Minus(unary).
    ParenthesisBegin,
    ParenthesisEnd,
    Plus,
    Minus,
}

pub struct OperatorInfo {
    pub eval_ltor: bool, // eval_left_to_right
    pub prec: usize,     // precedence
    pub fix: Fix,
}

/// Either prefix or postfix, should only be used for unary operators. It will
/// be set to Dummy for every item non unary operators.
pub enum Fix {
    Prefix,
    Postfix,
    Dummy,
}

// TODO: Clean up.
// (evaluate_left_to_right, precedence, prefix/postfix)
impl Operator {
    // Precedence according to:
    //      https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html
    // and "power"  ~over "mul/div/mod" according to pythons:
    //      https://www.mathcs.emory.edu/~valerie/courses/fall10/155/resources/op_precedence.html
    // and old rust docs:
    //      https://web.archive.org/web/20160304121349/https://doc.rust-lang.org/reference.html#unary-operator-expressions

    /*
        Precedence:
            0   ( )      (precedence for parenthesis always highest)
            1   +x -x
            2   x++ x--  (only postfix)
            3   ~ !
            4   . .* .&  (function calls, deref, address etc.)
            5   as
            6   **       (power)
            7   * / %
            8   + -
            9   << >>
            10   < > <= >= is of
            11  == !=
            12  &
            13  ^
            14  |
            15  and (bool)
            16  or (bool)
            17  .. ..=
            18  in
            19  = += -= *= /= %= **= &= |= ^= <<= >>=
    */
    fn lookup(&self) -> Option<(bool, usize, Fix)> {
        if let Operator::ParenthesisBegin = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::ParenthesisEnd = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::UnaryOperator(unary_op) = self {
            Some(match unary_op {
                UnaryOperator::Positive => (true, 1, Fix::Prefix),
                UnaryOperator::Negative => (true, 1, Fix::Prefix),
                UnaryOperator::Increment => (true, 2, Fix::Postfix),
                UnaryOperator::Decrement => (true, 2, Fix::Postfix),

                UnaryOperator::BitCompliment => (true, 3, Fix::Prefix),
                UnaryOperator::BoolNot => (true, 3, Fix::Prefix),
                UnaryOperator::Deref => (true, 4, Fix::Postfix),
                UnaryOperator::Address => (true, 4, Fix::Postfix),
            })
        } else if let Operator::BinaryOperator(binary_op) = self {
            Some(match binary_op {
                BinaryOperator::Assignment => (false, 19, Fix::Dummy),
                BinaryOperator::In => (false, 18, Fix::Dummy),
                BinaryOperator::Is => (true, 10, Fix::Dummy),
                BinaryOperator::As => (true, 5, Fix::Dummy),
                BinaryOperator::Of => (true, 10, Fix::Dummy),
                BinaryOperator::Range => (true, 17, Fix::Dummy),
                BinaryOperator::RangeInclusive => (true, 17, Fix::Dummy),
                BinaryOperator::Dot => (true, 4, Fix::Dummy),

                BinaryOperator::Equals => (true, 11, Fix::Dummy),
                BinaryOperator::NotEquals => (true, 11, Fix::Dummy),
                BinaryOperator::LessThan => (true, 10, Fix::Dummy),
                BinaryOperator::GreaterThan => (true, 10, Fix::Dummy),
                BinaryOperator::LessThanOrEquals => (true, 10, Fix::Dummy),
                BinaryOperator::GreaterThanOrEquals => (true, 10, Fix::Dummy),

                BinaryOperator::Addition => (true, 8, Fix::Dummy),
                BinaryOperator::Subtraction => (true, 8, Fix::Dummy),
                BinaryOperator::Multiplication => (true, 7, Fix::Dummy),
                BinaryOperator::Division => (true, 7, Fix::Dummy),
                BinaryOperator::Modulus => (true, 7, Fix::Dummy),
                BinaryOperator::Power => (false, 6, Fix::Dummy),

                BinaryOperator::BitAnd => (true, 12, Fix::Dummy),
                BinaryOperator::BitOr => (true, 14, Fix::Dummy),
                BinaryOperator::BitXor => (true, 13, Fix::Dummy),
                BinaryOperator::ShiftLeft => (true, 9, Fix::Dummy),
                BinaryOperator::ShiftRight => (true, 9, Fix::Dummy),

                BinaryOperator::BoolAnd => (true, 15, Fix::Dummy),
                BinaryOperator::BoolOr => (true, 16, Fix::Dummy),

                BinaryOperator::AssignAddition => (false, 19, Fix::Dummy),
                BinaryOperator::AssignSubtraction => (false, 19, Fix::Dummy),
                BinaryOperator::AssignMultiplication => (false, 19, Fix::Dummy),
                BinaryOperator::AssignDivision => (false, 19, Fix::Dummy),
                BinaryOperator::AssignModulus => (false, 19, Fix::Dummy),
                BinaryOperator::AssignPower => (false, 19, Fix::Dummy),

                BinaryOperator::AssignBitAnd => (false, 19, Fix::Dummy),
                BinaryOperator::AssignBitOr => (false, 19, Fix::Dummy),
                BinaryOperator::AssignBitXor => (false, 19, Fix::Dummy),
                BinaryOperator::AssignShiftLeft => (false, 19, Fix::Dummy),
                BinaryOperator::AssignShiftRight => (false, 19, Fix::Dummy),

                _ => return None,
            })
        } else {
            None
        }
    }

    pub fn info(&self) -> CustomResult<OperatorInfo> {
        if let Some(info) = self.lookup() {
            Ok(OperatorInfo {
                eval_ltor: info.0,
                prec: info.1,
                fix: info.2,
            })
        } else {
            Err(ParseError(format!(
                "Invalid operator, unable to get info: {:?}.",
                self
            )))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    /* GENERAL */
    Assignment,
    // Used in for loops etc.
    // TODO: Maybe make keyword instead and only allow use it in "for" loops.
    In,
    // pattern matching
    Is,
    // cast
    As,
    Of,
    Range,
    RangeInclusive,

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

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    /* NUMBERS */
    Increment,
    Decrement,

    // Dereference pointer and take address of value (.* & .&).
    Deref,
    Address,

    // ex: (+a + -b)  =>  { Positive(a) + Negative(b) }
    Positive,
    Negative,

    /* NUMBERS (BIT) */
    BitCompliment,

    /* BOOL */
    BoolNot,
}

impl ParseToken {
    /// Returns some Operator if the given symbol is a valid operator, returns
    /// None otherwise.
    pub fn get_if_operator(symbol: &lex::token::Symbol) -> Option<Operator> {
        Some(match symbol {
            lex::token::Symbol::ParenthesisBegin => Operator::ParenthesisBegin,
            lex::token::Symbol::ParenthesisEnd => Operator::ParenthesisEnd,
            lex::token::Symbol::Plus => Operator::Plus,
            lex::token::Symbol::Minus => Operator::Minus,
            /*
            lex::token::Symbol::SquareBracketBegin,
            lex::token::Symbol::SquareBracketEnd,
            lex::token::Symbol::CurlyBracketBegin,
            lex::token::Symbol::CurlyBracketEnd,
            lex::token::Symbol::PointyBracketBegin,
            lex::token::Symbol::PointyBracketEnd,
            */
            lex::token::Symbol::Increment => Operator::UnaryOperator(UnaryOperator::Increment),
            lex::token::Symbol::Decrement => Operator::UnaryOperator(UnaryOperator::Decrement),

            lex::token::Symbol::Dot => Operator::BinaryOperator(BinaryOperator::Dot),
            //lex::token::Symbol::Comma,
            //lex::token::Symbol::QuestionMark,
            //lex::token::Symbol::ExclamationMark,
            lex::token::Symbol::Equals => Operator::BinaryOperator(BinaryOperator::Assignment),
            //lex::token::Symbol::DoubleQuote,
            //lex::token::Symbol::SingleQuote,
            //lex::token::Symbol::Colon,
            //lex::token::Symbol::SemiColon,
            //lex::token::Symbol::Pound,
            //lex::token::Symbol::At,
            //lex::token::Symbol::Dollar,
            //lex::token::Symbol::LineBreak,
            //lex::token::Symbol::WhiteSpace(usize),

            //lex::token::Symbol::Pipe,
            lex::token::Symbol::Range => Operator::BinaryOperator(BinaryOperator::Range),
            lex::token::Symbol::RangeInclusive => {
                Operator::BinaryOperator(BinaryOperator::RangeInclusive)
            }
            //lex::token::Symbol::Arrow,
            lex::token::Symbol::Deref => Operator::UnaryOperator(UnaryOperator::Deref),
            lex::token::Symbol::Address => Operator::UnaryOperator(UnaryOperator::Address),

            lex::token::Symbol::EqualsOperator => Operator::BinaryOperator(BinaryOperator::Equals),
            lex::token::Symbol::NotEquals => Operator::BinaryOperator(BinaryOperator::NotEquals),
            lex::token::Symbol::SquareBracketBegin => {
                Operator::BinaryOperator(BinaryOperator::LessThan)
            }
            lex::token::Symbol::SquareBracketEnd => {
                Operator::BinaryOperator(BinaryOperator::GreaterThan)
            }
            lex::token::Symbol::LessThanOrEquals => {
                Operator::BinaryOperator(BinaryOperator::LessThanOrEquals)
            }
            lex::token::Symbol::GreaterThanOrEquals => {
                Operator::BinaryOperator(BinaryOperator::GreaterThanOrEquals)
            }

            lex::token::Symbol::Multiplication => {
                Operator::BinaryOperator(BinaryOperator::Multiplication)
            }
            lex::token::Symbol::Division => Operator::BinaryOperator(BinaryOperator::Division),
            lex::token::Symbol::Modulus => Operator::BinaryOperator(BinaryOperator::Modulus),
            lex::token::Symbol::Power => Operator::BinaryOperator(BinaryOperator::Power),

            lex::token::Symbol::BitAnd => Operator::BinaryOperator(BinaryOperator::BitAnd),
            lex::token::Symbol::BitOr => Operator::BinaryOperator(BinaryOperator::BitOr),
            lex::token::Symbol::BitXor => Operator::BinaryOperator(BinaryOperator::BitXor),
            lex::token::Symbol::ShiftLeft => Operator::BinaryOperator(BinaryOperator::ShiftLeft),
            lex::token::Symbol::ShiftRight => Operator::BinaryOperator(BinaryOperator::ShiftRight),
            lex::token::Symbol::BitCompliment => {
                Operator::UnaryOperator(UnaryOperator::BitCompliment)
            }

            /*
            lex::token::Symbol::CommentSingleLine,
            lex::token::Symbol::CommentMultiLineBegin,
            lex::token::Symbol::CommentMultiLineEnd,
            */
            lex::token::Symbol::BoolNot => Operator::UnaryOperator(UnaryOperator::BoolNot),
            lex::token::Symbol::BoolAnd => Operator::BinaryOperator(BinaryOperator::BoolAnd),
            lex::token::Symbol::BoolOr => Operator::BinaryOperator(BinaryOperator::BoolOr),

            lex::token::Symbol::AssignAddition => {
                Operator::BinaryOperator(BinaryOperator::AssignAddition)
            }
            lex::token::Symbol::AssignSubtraction => {
                Operator::BinaryOperator(BinaryOperator::AssignSubtraction)
            }
            lex::token::Symbol::AssignMultiplication => {
                Operator::BinaryOperator(BinaryOperator::AssignMultiplication)
            }
            lex::token::Symbol::AssignDivision => {
                Operator::BinaryOperator(BinaryOperator::AssignDivision)
            }
            lex::token::Symbol::AssignModulus => {
                Operator::BinaryOperator(BinaryOperator::AssignModulus)
            }
            lex::token::Symbol::AssignPower => {
                Operator::BinaryOperator(BinaryOperator::AssignPower)
            }

            lex::token::Symbol::AssignBitAnd => {
                Operator::BinaryOperator(BinaryOperator::AssignBitAnd)
            }
            lex::token::Symbol::AssignBitOr => {
                Operator::BinaryOperator(BinaryOperator::AssignBitOr)
            }
            lex::token::Symbol::AssignBitXor => {
                Operator::BinaryOperator(BinaryOperator::AssignBitXor)
            }
            lex::token::Symbol::AssignShiftLeft => {
                Operator::BinaryOperator(BinaryOperator::AssignShiftLeft)
            }
            lex::token::Symbol::AssignShiftRight => {
                Operator::BinaryOperator(BinaryOperator::AssignShiftRight)
            }

            lex::token::Symbol::In => Operator::BinaryOperator(BinaryOperator::In),
            lex::token::Symbol::Is => Operator::BinaryOperator(BinaryOperator::Is),
            lex::token::Symbol::As => Operator::BinaryOperator(BinaryOperator::As),
            lex::token::Symbol::Of => Operator::BinaryOperator(BinaryOperator::Of),

            _ => return None,
        })
    }
}
