use crate::error::{LangError, LangErrorKind::ParseError};
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

    // AutoClosable.
    With(Expression),
    // Defer -> Run this expression at the end of the current block.
    Defer(Expression),

    // The lhs can't be a "Variable" directly since it needs to support
    // ex. array indexing and dereferencing. But evaluationg the lhs expressions
    // MUST evaluate to a variable.
    // The valid lhs expressions are (Variable or wrapping a Variable):
    //   Variable
    //   bin op:
    //     Dot (both lhs and rhs as Variables)
    //   un op:
    //     Deref
    //     Address
    // The "middle expr" is the lhs and the "right expr" is the rhs of the assignment.
    Assignment(AssignOperator, Expression, Expression),

    // Used both for "var" and "const" variables. The expr options will be Some
    // if this var decl also has han initializer.
    VariableDecl(Variable, Option<Expression>),

    // TODO: Implement extern for variables as well.
    // Declaration of extern functions.
    ExternalDecl(Function),

    // static, private etc.
    Modifier(Modifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    // TODO: Unnecessary to have type here? Bool, string and char types are
    //       implied. For numbers the postfix notation might be converted to a
    //       "As" so that the type doesn't need to be stored in the literal,
    //       it will be stored in the surrounding expression.
    Literal(lex::token::Literal, Option<TypeStruct>),

    // TODO: Is it ok to have type as a expression? This lets one handle binary
    //       operators like ex. "as" in a simple way.
    Type(TypeStruct),

    // TODO: FIXME: The "Variable" struct contains a type. Should the type be
    //              in this enum instead?
    Variable(Variable),
    FunctionCall(FunctionCall),
    StructInit(StructInit),
    ArrayInit(Vec<Argument>),
    //MacroCall(Option<MacroCall>),
    Operation(Operation),
}

// TODO: Rethink this approach of using "AccessType"s, should not do it.
//       This is because sometimes it is just ignored, as for example with
//       "StructAccess" since that is handled in another way.
#[derive(Debug)]
pub enum AccessType {
    Regular,
    Deref,
    Address,
    // StructAccess, Struct access is currently handled in other ways
    ArrayAccess,
}

impl Expression {
    pub fn is_var(&self) -> bool {
        match self {
            Expression::Variable(_) => true,
            Expression::Operation(op) => match op {
                Operation::BinaryOperation(bin_op) => match bin_op.operator {
                    BinaryOperator::Dot => bin_op.left.is_var() && bin_op.right.is_var(),
                    _ => false,
                },
                Operation::UnaryOperation(un_op) => match un_op.operator {
                    UnaryOperator::Deref
                    | UnaryOperator::Address
                    | UnaryOperator::ArrayAccess(_) => true,
                    _ => false,
                },
            },
            _ => false,
        }
    }

    pub fn get_access_type(&self) -> Option<AccessType> {
        match self {
            Expression::Variable(_) => Some(AccessType::Regular),
            Expression::Operation(op) => match op {
                Operation::BinaryOperation(bin_op) => match bin_op.operator {
                    BinaryOperator::Dot => {
                        if bin_op.right.is_deref() {
                            Some(AccessType::Deref)
                        } else if bin_op.right.is_address() {
                            Some(AccessType::Address)
                        } else if bin_op.right.is_array_access() {
                            Some(AccessType::ArrayAccess)
                        } else if bin_op.left.is_var() && bin_op.right.is_var() {
                            Some(AccessType::Regular)
                        } else {
                            panic!("TODO: lhs & rhs isn't both var, might be func call.");
                        }
                    }
                    _ => None,
                },
                Operation::UnaryOperation(un_op) => match un_op.operator {
                    UnaryOperator::Deref => Some(AccessType::Deref),
                    UnaryOperator::Address => Some(AccessType::Address),
                    UnaryOperator::ArrayAccess(_) => Some(AccessType::ArrayAccess),
                    _ => None,
                },
            },
            _ => None,
        }
    }

    pub fn is_deref(&self) -> bool {
        if let Expression::Operation(Operation::UnaryOperation(un_op)) = self {
            if let UnaryOperator::Deref = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_address(&self) -> bool {
        if let Expression::Operation(Operation::UnaryOperation(un_op)) = self {
            if let UnaryOperator::Address = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_array_access(&self) -> bool {
        if let Expression::Operation(Operation::UnaryOperation(un_op)) = self {
            if let UnaryOperator::ArrayAccess(_) = un_op.operator {
                return true;
            }
        }
        false
    }

    pub fn is_struct_access(&self) -> bool {
        if let Expression::Operation(Operation::BinaryOperation(bin_op)) = self {
            if let BinaryOperator::Dot = bin_op.operator {
                return bin_op.left.is_var() && bin_op.right.is_var();
            }
        }
        false
    }

    /// If this is a Dot operation, this function will return the variable from
    /// the rhs.
    pub fn eval_to_var(&mut self) -> Option<&mut Variable> {
        match self {
            Expression::Variable(var) => Some(var),
            Expression::Operation(op) => match op {
                Operation::BinaryOperation(bin_op) => match bin_op.operator {
                    BinaryOperator::Dot => {
                        if bin_op.left.is_var() && bin_op.right.is_var() {
                            bin_op.right.eval_to_var()
                        } else {
                            None
                        }
                    }
                    _ => None,
                },
                Operation::UnaryOperation(un_op) => match un_op.operator {
                    // TODO: This returns the variable that this un op is applied
                    // on and not the variable resulting from the addr/deref op.
                    UnaryOperator::Deref
                    | UnaryOperator::Address
                    | UnaryOperator::ArrayAccess(_) => un_op.value.eval_to_var(),
                    _ => None,
                },
            },
            _ => None,
        }
    }
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

    // A anonymous block "{ ... }" that can be used to limit the scope.
    Anonymous,

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

    // Function for testing, allows strings as test names with spaces etc.
    Test(Function),
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
    pub is_const: bool,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl BinaryOperation {
    pub fn new(operator: BinaryOperator, left: Box<Expression>, right: Box<Expression>) -> Self {
        BinaryOperation {
            operator,
            ret_type: None,
            is_const: false,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub ret_type: Option<TypeStruct>,
    pub is_const: bool,
    pub value: Box<Expression>,
}

impl UnaryOperation {
    pub fn new(operator: UnaryOperator, value: Box<Expression>) -> Self {
        UnaryOperation {
            operator,
            ret_type: None,
            is_const: false,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub generics: Option<Vec<TypeStruct>>,
    pub implements: Option<Vec<TypeStruct>>,
    pub members: Option<Vec<Variable>>, // TODO: extends: Vec<Type>
}

impl Struct {
    pub fn new(
        name: String,
        generics: Option<Vec<TypeStruct>>,
        implements: Option<Vec<TypeStruct>>,
        members: Option<Vec<Variable>>,
    ) -> Self {
        Self {
            name,
            generics,
            implements,
            members,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub generics: Option<Vec<TypeStruct>>,
    pub parameters: Option<Vec<Variable>>,
    pub ret_type: Option<TypeStruct>,
    pub is_var_arg: bool,
}

impl Function {
    pub fn new(
        name: String,
        generics: Option<Vec<TypeStruct>>,
        parameters: Option<Vec<Variable>>,
        ret_type: Option<TypeStruct>,
        is_var_arg: bool,
    ) -> Self {
        Function {
            name,
            generics,
            parameters,
            ret_type,
            is_var_arg,
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
        Self { name, arguments }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    pub name: String,
    pub arguments: Vec<Argument>,
}

impl StructInit {
    pub fn new(name: String, arguments: Vec<Argument>) -> Self {
        Self { name, arguments }
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
pub enum AccessInstruction {
    /// The first string is the name of the struct that this var is a member of.
    /// The second string is the name of this member/variable and the u32 is the
    /// position/index of this var in the struct.
    StructMember(String, String, Option<u32>),

    Deref,
    Address,

    /// The expression if the index of the array to access.
    ArrayAccess(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RootVariable {
    pub name: String,
    pub decl_block_id: BlockId,
    pub is_struct: bool,
}

impl RootVariable {
    pub fn new(name: String, decl_block_id: BlockId, is_struct: bool) -> Self {
        Self {
            name,
            decl_block_id,
            is_struct,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub ret_type: Option<TypeStruct>,
    pub modifiers: Option<Vec<Modifier>>,
    pub is_const: bool,

    /// If `access_instrs` is set, access this Variable is NOT a "basic" variable
    /// that can be accessed directly. It will either be a struct member,
    /// some sort of deref/address/array access or a combination of them. This
    /// will require extra instructions when accessing the contents of the variable
    /// that will be "reproduced" when generating the code to access this var.
    /// The "RootVariable" is the "root" variable that will is the entry point
    /// and to get this specific self "variable" instace, one will apply
    /// the "AccessInstruction"s on it.
    pub access_instrs: Option<(RootVariable, Vec<AccessInstruction>)>,
}

impl Variable {
    pub fn new(
        name: String,
        ret_type: Option<TypeStruct>,
        modifiers: Option<Vec<Modifier>>,
        is_const: bool,
    ) -> Self {
        Variable {
            name,
            ret_type,
            modifiers,
            is_const,
            access_instrs: None,
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
    const EXTENSION: &'static str = ".ren";

    pub fn new(idents: Vec<String>) -> Self {
        Path { idents }
    }

    pub fn to_file_path(&self) -> Option<String> {
        if let Some(path_str) = std::path::Path::new(&self.idents.join("/")).to_str() {
            let mut path = path_str.to_string();
            path.push_str(Path::EXTENSION);
            Some(path)
        } else {
            None
        }
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
    // and c:
    //      https://en.cppreference.com/w/c/language/operator_precedence

    /*
        Precedence:
            0   ( )          (precedence for parenthesis always highest)
            1   . .* .& .[]  (function calls, deref, address, indexing etc.)
            2   +x -x
            3   x++ x--      (only postfix)
            4   ~ !
            5   as
            6   **           (power)
            7   * / %
            8   + -
            9   << >>
            10  < > <= >= is of
            11  == !=
            12  &
            13  ^
            14  |
            15  and          (bool)
            16  or           (bool)
            17  .. ..=
            18  in

            (Currently assignments aren't counted as expression, but they would
            have the lowest precedence if they were)
            19  = += -= *= /= %= **= &= |= ^= <<= >>=
    */
    fn lookup(&self) -> Option<(bool, usize, Fix)> {
        if let Operator::ParenthesisBegin = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::ParenthesisEnd = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::UnaryOperator(unary_op) = self {
            Some(match unary_op {
                UnaryOperator::Positive => (true, 2, Fix::Prefix),
                UnaryOperator::Negative => (true, 2, Fix::Prefix),
                UnaryOperator::Increment => (true, 3, Fix::Postfix),
                UnaryOperator::Decrement => (true, 3, Fix::Postfix),

                UnaryOperator::BitComplement => (true, 4, Fix::Prefix),
                UnaryOperator::BoolNot => (true, 4, Fix::Prefix),
                UnaryOperator::Deref => (true, 1, Fix::Postfix),
                UnaryOperator::Address => (true, 1, Fix::Postfix),
                UnaryOperator::ArrayAccess(_) => (true, 1, Fix::Postfix),
            })
        } else if let Operator::BinaryOperator(binary_op) = self {
            Some(match binary_op {
                BinaryOperator::In => (false, 18, Fix::Dummy),
                BinaryOperator::Is => (true, 10, Fix::Dummy),
                BinaryOperator::As => (true, 5, Fix::Dummy),
                BinaryOperator::Of => (true, 10, Fix::Dummy),
                BinaryOperator::Range => (true, 17, Fix::Dummy),
                BinaryOperator::RangeInclusive => (true, 17, Fix::Dummy),
                BinaryOperator::Dot => (true, 1, Fix::Dummy),

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
            Err(LangError::new(
                format!("Invalid operator, unable to get info: {:?}.", self),
                ParseError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOperator {
    Assignment, // "Normal" assignment ("x = y").
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    /* GENERAL */
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

    // Dereference pointer and take address of value (.* and .&).
    Deref,
    Address,

    // ex: (+a + -b)  =>  { Positive(a) + Negative(b) }
    Positive,
    Negative,

    // TODO: Slice/slicing.
    // The expression is the dimension.
    ArrayAccess(Box<Expression>),

    /* NUMBERS (BIT) */
    BitComplement,

    /* BOOL */
    BoolNot,
}

impl ParseToken {
    /// Returns some Operator if the given symbol is a valid operator inside a
    /// expression, returns None otherwise.
    pub fn get_if_expr_op(symbol: &lex::token::Symbol) -> Option<Operator> {
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
            */
            lex::token::Symbol::PointyBracketBegin => {
                Operator::BinaryOperator(BinaryOperator::LessThan)
            }
            lex::token::Symbol::PointyBracketEnd => {
                Operator::BinaryOperator(BinaryOperator::GreaterThan)
            }

            lex::token::Symbol::Increment => Operator::UnaryOperator(UnaryOperator::Increment),
            lex::token::Symbol::Decrement => Operator::UnaryOperator(UnaryOperator::Decrement),

            lex::token::Symbol::Dot => Operator::BinaryOperator(BinaryOperator::Dot),
            //lex::token::Symbol::Comma,
            //lex::token::Symbol::QuestionMark,
            //lex::token::Symbol::ExclamationMark,
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
                Operator::UnaryOperator(UnaryOperator::BitComplement)
            }

            /*
            lex::token::Symbol::CommentSingleLine,
            lex::token::Symbol::CommentMultiLineBegin,
            lex::token::Symbol::CommentMultiLineEnd,
            */
            lex::token::Symbol::BoolNot => Operator::UnaryOperator(UnaryOperator::BoolNot),
            lex::token::Symbol::BoolAnd => Operator::BinaryOperator(BinaryOperator::BoolAnd),
            lex::token::Symbol::BoolOr => Operator::BinaryOperator(BinaryOperator::BoolOr),

            lex::token::Symbol::In => Operator::BinaryOperator(BinaryOperator::In),
            lex::token::Symbol::Is => Operator::BinaryOperator(BinaryOperator::Is),
            lex::token::Symbol::As => Operator::BinaryOperator(BinaryOperator::As),
            lex::token::Symbol::Of => Operator::BinaryOperator(BinaryOperator::Of),

            _ => return None,
        })
    }

    /// Returns some Operator if the given symbol is a valid operator inside a
    /// expression, returns None otherwise.
    pub fn get_if_stmt_op(lex_token: &lex::token::LexToken) -> Option<AssignOperator> {
        if let lex::token::LexTokenKind::Symbol(ref symbol) = lex_token.kind {
            Some(match symbol {
                lex::token::Symbol::Equals => AssignOperator::Assignment,
                lex::token::Symbol::AssignAddition => AssignOperator::AssignAddition,
                lex::token::Symbol::AssignSubtraction => AssignOperator::AssignSubtraction,
                lex::token::Symbol::AssignMultiplication => AssignOperator::AssignMultiplication,
                lex::token::Symbol::AssignDivision => AssignOperator::AssignDivision,
                lex::token::Symbol::AssignModulus => AssignOperator::AssignModulus,
                lex::token::Symbol::AssignPower => AssignOperator::AssignPower,
                lex::token::Symbol::AssignBitAnd => AssignOperator::AssignBitAnd,
                lex::token::Symbol::AssignBitOr => AssignOperator::AssignBitOr,
                lex::token::Symbol::AssignBitXor => AssignOperator::AssignBitXor,
                lex::token::Symbol::AssignShiftLeft => AssignOperator::AssignShiftLeft,
                lex::token::Symbol::AssignShiftRight => AssignOperator::AssignShiftRight,

                _ => return None,
            })
        } else {
            None
        }
    }
}
