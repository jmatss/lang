use common::{
    error::{CustomResult, LangError, LangErrorKind::ParseError},
    token::{
        block::BlockHeader,
        expr::Expression,
        op::{AssignOperator, BinaryOperator, UnaryOperator},
        stmt::Statement,
    },
    BlockId,
};

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
            4   ~
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
            15  not          (!)
            16  and          (bool)
            17  or           (bool)
            18  .. ..=
            19  in

            (Currently assignments aren't counted as expression, but they would
            have the lowest precedence if they were)
            20  = += -= *= /= %= **= &= |= ^= <<= >>=
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
                UnaryOperator::BoolNot => (true, 15, Fix::Prefix),
                UnaryOperator::Deref => (true, 1, Fix::Postfix),
                UnaryOperator::Address => (true, 1, Fix::Postfix),
                UnaryOperator::ArrayAccess(_) => (true, 1, Fix::Postfix),
            })
        } else if let Operator::BinaryOperator(binary_op) = self {
            Some(match binary_op {
                BinaryOperator::In => (false, 19, Fix::Dummy),
                BinaryOperator::Is => (true, 10, Fix::Dummy),
                BinaryOperator::As => (true, 5, Fix::Dummy),
                BinaryOperator::Of => (true, 10, Fix::Dummy),
                BinaryOperator::Range => (true, 18, Fix::Dummy),
                BinaryOperator::RangeInclusive => (true, 18, Fix::Dummy),
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

                BinaryOperator::BoolAnd => (true, 16, Fix::Dummy),
                BinaryOperator::BoolOr => (true, 17, Fix::Dummy),

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
