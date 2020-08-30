use super::expr::Expression;
use crate::variable_type::TypeStruct;

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    BinaryOperation(BinaryOperation),
    UnaryOperation(UnaryOperation),
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
