use common::{
    error::{CustomResult, LangError, LangErrorKind::ParseError},
    token::{
        expr::Expr,
        op::{AssignOperator, BinOperator, UnOperator},
    },
};

/// Returns some Operator if the given symbol is a valid operator inside a
/// expression, returns None otherwise.
pub fn get_if_expr_op(symbol: &lex::token::Sym) -> Option<Operator> {
    Some(match symbol {
        lex::token::Sym::ParenthesisBegin => Operator::ParenthesisBegin,
        lex::token::Sym::ParenthesisEnd => Operator::ParenthesisEnd,
        lex::token::Sym::Plus => Operator::Plus,
        lex::token::Sym::Minus => Operator::Minus,
        /*
        lex::token::Symbol::SquareBracketBegin,
        lex::token::Symbol::SquareBracketEnd,
        lex::token::Symbol::CurlyBracketBegin,
        lex::token::Symbol::CurlyBracketEnd,
        */
        lex::token::Sym::PointyBracketBegin => Operator::BinaryOperator(BinOperator::LessThan),
        lex::token::Sym::PointyBracketEnd => Operator::BinaryOperator(BinOperator::GreaterThan),

        lex::token::Sym::Dot => Operator::BinaryOperator(BinOperator::Dot),
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
        lex::token::Sym::Range => Operator::BinaryOperator(BinOperator::Range),
        lex::token::Sym::RangeInclusive => Operator::BinaryOperator(BinOperator::RangeInclusive),
        //lex::token::Symbol::Arrow,
        lex::token::Sym::Deref => Operator::UnaryOperator(UnOperator::Deref),
        lex::token::Sym::Address => Operator::UnaryOperator(UnOperator::Address),

        lex::token::Sym::EqualsOperator => Operator::BinaryOperator(BinOperator::Equals),
        lex::token::Sym::NotEquals => Operator::BinaryOperator(BinOperator::NotEquals),
        lex::token::Sym::SquareBracketBegin => Operator::BinaryOperator(BinOperator::LessThan),
        lex::token::Sym::SquareBracketEnd => Operator::BinaryOperator(BinOperator::GreaterThan),
        lex::token::Sym::LessThanOrEquals => {
            Operator::BinaryOperator(BinOperator::LessThanOrEquals)
        }
        lex::token::Sym::GreaterThanOrEquals => {
            Operator::BinaryOperator(BinOperator::GreaterThanOrEquals)
        }

        lex::token::Sym::Multiplication => Operator::BinaryOperator(BinOperator::Multiplication),
        lex::token::Sym::Division => Operator::BinaryOperator(BinOperator::Division),
        lex::token::Sym::Modulus => Operator::BinaryOperator(BinOperator::Modulus),

        lex::token::Sym::BitAnd => Operator::BinaryOperator(BinOperator::BitAnd),
        lex::token::Sym::BitOr => Operator::BinaryOperator(BinOperator::BitOr),
        lex::token::Sym::BitXor => Operator::BinaryOperator(BinOperator::BitXor),
        lex::token::Sym::ShiftLeft => Operator::BinaryOperator(BinOperator::ShiftLeft),
        lex::token::Sym::ShiftRight => Operator::BinaryOperator(BinOperator::ShiftRight),
        lex::token::Sym::BitCompliment => Operator::UnaryOperator(UnOperator::BitComplement),

        /*
        lex::token::Symbol::CommentSingleLine,
        lex::token::Symbol::CommentMultiLineBegin,
        lex::token::Symbol::CommentMultiLineEnd,
        */
        lex::token::Sym::BoolNot => Operator::UnaryOperator(UnOperator::BoolNot),
        lex::token::Sym::BoolAnd => Operator::BinaryOperator(BinOperator::BoolAnd),
        lex::token::Sym::BoolOr => Operator::BinaryOperator(BinOperator::BoolOr),

        lex::token::Sym::In => Operator::BinaryOperator(BinOperator::In),
        lex::token::Sym::Is => Operator::BinaryOperator(BinOperator::Is),
        lex::token::Sym::As => Operator::BinaryOperator(BinOperator::As),
        lex::token::Sym::Of => Operator::BinaryOperator(BinOperator::Of),

        _ => return None,
    })
}

/// Returns some Operator if the given symbol is a valid operator inside a
/// expression, returns None otherwise.
pub fn get_if_stmt_op(lex_token: &lex::token::LexToken) -> Option<AssignOperator> {
    if let lex::token::LexTokenKind::Sym(ref symbol) = lex_token.kind {
        Some(match symbol {
            lex::token::Sym::Equals => AssignOperator::Assignment,
            lex::token::Sym::AssignAddition => AssignOperator::AssignAdd,
            lex::token::Sym::AssignSubtraction => AssignOperator::AssignSub,
            lex::token::Sym::AssignMultiplication => AssignOperator::AssignMul,
            lex::token::Sym::AssignDivision => AssignOperator::AssignDiv,
            lex::token::Sym::AssignModulus => AssignOperator::AssignMod,
            lex::token::Sym::AssignBitAnd => AssignOperator::AssignBitAnd,
            lex::token::Sym::AssignBitOr => AssignOperator::AssignBitOr,
            lex::token::Sym::AssignBitXor => AssignOperator::AssignBitXor,
            lex::token::Sym::AssignShiftLeft => AssignOperator::AssignShl,
            lex::token::Sym::AssignShiftRight => AssignOperator::AssignShr,

            _ => return None,
        })
    } else {
        None
    }
}

/// Used during parsing of expression so that both operators and operand can be
/// stored together in the same container.
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Operator(Operator),
    Operand(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    BinaryOperator(BinOperator),
    UnaryOperator(UnOperator),

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
            1   . .* .& .[]  (func/method calls, deref, address, indexing etc.)
            2   +x -x        (negative/positive)
            3   ~
            4   as
            5   * / %
            6   + -
            7   << >>
            8   < > <= >= is of
            9   == !=
            10  &
            11  ^
            12  |
            13  not          (!)
            14  and          (bool)
            15  or           (bool)
            16  .. ..=
            17  in

            (Currently assignments aren't counted as expression, but they would
            have the lowest precedence if they were)
            18  = += -= *= /= %= *= &= |= ^= <<= >>=
    */
    fn lookup(&self) -> Option<(bool, usize, Fix)> {
        if let Operator::ParenthesisBegin = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::ParenthesisEnd = self {
            Some((true, 0, Fix::Dummy))
        } else if let Operator::UnaryOperator(unary_op) = self {
            Some(match unary_op {
                UnOperator::Positive => (true, 2, Fix::Prefix),
                UnOperator::Negative => (true, 2, Fix::Prefix),

                UnOperator::BitComplement => (true, 3, Fix::Prefix),
                UnOperator::BoolNot => (true, 13, Fix::Prefix),
                UnOperator::Deref => (true, 1, Fix::Postfix),
                UnOperator::Address => (true, 1, Fix::Postfix),
                UnOperator::ArrayAccess(_) => (true, 1, Fix::Postfix),
                UnOperator::StructAccess(..) => (true, 1, Fix::Postfix),
            })
        } else if let Operator::BinaryOperator(binary_op) = self {
            Some(match binary_op {
                BinOperator::In => (false, 17, Fix::Dummy),
                BinOperator::Is => (true, 8, Fix::Dummy),
                BinOperator::As => (true, 4, Fix::Dummy),
                BinOperator::Of => (true, 8, Fix::Dummy),
                BinOperator::Range => (true, 16, Fix::Dummy),
                BinOperator::RangeInclusive => (true, 16, Fix::Dummy),
                BinOperator::Dot => (true, 1, Fix::Dummy),

                BinOperator::Equals => (true, 9, Fix::Dummy),
                BinOperator::NotEquals => (true, 9, Fix::Dummy),
                BinOperator::LessThan => (true, 8, Fix::Dummy),
                BinOperator::GreaterThan => (true, 8, Fix::Dummy),
                BinOperator::LessThanOrEquals => (true, 8, Fix::Dummy),
                BinOperator::GreaterThanOrEquals => (true, 8, Fix::Dummy),

                BinOperator::Addition => (true, 6, Fix::Dummy),
                BinOperator::Subtraction => (true, 6, Fix::Dummy),
                BinOperator::Multiplication => (true, 5, Fix::Dummy),
                BinOperator::Division => (true, 5, Fix::Dummy),
                BinOperator::Modulus => (true, 5, Fix::Dummy),

                BinOperator::BitAnd => (true, 10, Fix::Dummy),
                BinOperator::BitOr => (true, 12, Fix::Dummy),
                BinOperator::BitXor => (true, 11, Fix::Dummy),
                BinOperator::ShiftLeft => (true, 7, Fix::Dummy),
                BinOperator::ShiftRight => (true, 7, Fix::Dummy),

                BinOperator::BoolAnd => (true, 14, Fix::Dummy),
                BinOperator::BoolOr => (true, 15, Fix::Dummy),
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
