use common::{
    error::{LangError, LangErrorKind::ParseError, LangResult},
    token::stmt::Modifier,
    token::{
        expr::Expr,
        op::{AssignOperator, BinOperator, UnOperator},
    },
};
use lex::token::{Kw, LexToken, LexTokenKind, Sym};

pub fn get_modifier_token(keyword: &Kw) -> Option<Modifier> {
    Some(match keyword {
        Kw::Const => Modifier::Const,
        Kw::External => Modifier::External,
        Kw::Static => Modifier::Static,
        Kw::Private => Modifier::Private,
        Kw::Public => Modifier::Public,
        _ => return None,
    })
}

/// Returns some Operator if the given symbol is a valid operator inside a
/// expression, returns None otherwise.
pub fn get_if_expr_op(symbol: &Sym) -> Option<Operator> {
    Some(match symbol {
        Sym::ParenthesisBegin => Operator::ParenthesisBegin,
        Sym::ParenthesisEnd => Operator::ParenthesisEnd,
        Sym::PointyBracketBegin => Operator::BinaryOperator(BinOperator::Lt),
        Sym::PointyBracketEnd => Operator::BinaryOperator(BinOperator::Gt),

        Sym::Plus => Operator::Plus,
        Sym::Minus => Operator::Minus,
        Sym::Increment => Operator::UnaryOperator(UnOperator::Increment),
        Sym::Decrement => Operator::UnaryOperator(UnOperator::Decrement),

        Sym::Dot => Operator::BinaryOperator(BinOperator::Dot),
        Sym::Range => Operator::BinaryOperator(BinOperator::Range),
        Sym::RangeInclusive => Operator::BinaryOperator(BinOperator::RangeInclusive),
        Sym::Deref => Operator::UnaryOperator(UnOperator::Deref),
        Sym::Address => Operator::UnaryOperator(UnOperator::Address),

        Sym::DoubleEquals => Operator::BinaryOperator(BinOperator::Eq),
        Sym::NotEquals => Operator::BinaryOperator(BinOperator::Neq),
        Sym::SquareBracketBegin => Operator::BinaryOperator(BinOperator::Lt),
        Sym::SquareBracketEnd => Operator::BinaryOperator(BinOperator::Gt),
        Sym::Lte => Operator::BinaryOperator(BinOperator::Lte),
        Sym::Gte => Operator::BinaryOperator(BinOperator::Gte),

        Sym::Mul => Operator::BinaryOperator(BinOperator::Mul),
        Sym::Div => Operator::BinaryOperator(BinOperator::Div),
        Sym::Mod => Operator::BinaryOperator(BinOperator::Mod),

        Sym::BitAnd => Operator::BinaryOperator(BinOperator::BitAnd),
        Sym::BitOr => Operator::BinaryOperator(BinOperator::BitOr),
        Sym::BitXor => Operator::BinaryOperator(BinOperator::BitXor),
        Sym::ShiftLeft => Operator::BinaryOperator(BinOperator::ShiftLeft),
        Sym::ShiftRight => Operator::BinaryOperator(BinOperator::ShiftRight),
        Sym::BitCompliment => Operator::UnaryOperator(UnOperator::BitComplement),

        Sym::BoolNot => Operator::UnaryOperator(UnOperator::BoolNot),
        Sym::BoolAnd => Operator::BinaryOperator(BinOperator::BoolAnd),
        Sym::BoolOr => Operator::BinaryOperator(BinOperator::BoolOr),

        Sym::In => Operator::BinaryOperator(BinOperator::In),
        Sym::Is => Operator::BinaryOperator(BinOperator::Is),
        Sym::As => Operator::BinaryOperator(BinOperator::As),
        Sym::Of => Operator::BinaryOperator(BinOperator::Of),

        _ => return None,
    })
}

/// Returns some Operator if the given symbol is a valid operator inside a
/// expression, returns None otherwise.
pub fn get_if_stmt_op(lex_token: &LexToken) -> Option<AssignOperator> {
    if let LexTokenKind::Sym(ref symbol) = lex_token.kind {
        Some(match symbol {
            Sym::Equals => AssignOperator::Assignment,
            Sym::AssignAdd => AssignOperator::AssignAdd,
            Sym::AssignSub => AssignOperator::AssignSub,
            Sym::AssignMul => AssignOperator::AssignMul,
            Sym::AssignDiv => AssignOperator::AssignDiv,
            Sym::AssignMod => AssignOperator::AssignMod,
            Sym::AssignBitAnd => AssignOperator::AssignBitAnd,
            Sym::AssignBitOr => AssignOperator::AssignBitOr,
            Sym::AssignBitXor => AssignOperator::AssignBitXor,
            Sym::AssignShiftLeft => AssignOperator::AssignShl,
            Sym::AssignShiftRight => AssignOperator::AssignShr,

            _ => return None,
        })
    } else {
        None
    }
}

/// Used during parsing of expression so that both operators and operand can be
/// stored together in the same container.
#[derive(Debug, Clone)]
pub enum Output {
    Operator(Operator),
    Operand(Expr),
}

#[derive(Debug, Clone)]
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
    // and old rust docs:
    //      https://web.archive.org/web/20160304121349/https://doc.rust-lang.org/reference.html#unary-operator-expressions
    // and c:
    //      https://en.cppreference.com/w/c/language/operator_precedence

    /*
        Precedence:
            0   ( )          (precedence for parenthesis always highest)
            1   . :: .* .& .[] .++ .-- (func/method calls, deref, address, indexing, inc, dec etc.)
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
        match self {
            Operator::ParenthesisBegin | Operator::ParenthesisEnd => Some((true, 0, Fix::Dummy)),
            Operator::Plus | Operator::Minus => None,
            Operator::UnaryOperator(un_op) => Some(match un_op {
                UnOperator::Positive => (true, 2, Fix::Prefix),
                UnOperator::Negative => (true, 2, Fix::Prefix),
                UnOperator::Increment => (true, 1, Fix::Postfix),
                UnOperator::Decrement => (true, 1, Fix::Postfix),
                UnOperator::BitComplement => (true, 3, Fix::Prefix),
                UnOperator::BoolNot => (true, 13, Fix::Prefix),
                UnOperator::Deref => (true, 1, Fix::Postfix),
                UnOperator::Address => (true, 1, Fix::Postfix),
                UnOperator::ArrayAccess(_) => (true, 1, Fix::Postfix),
                UnOperator::UnionIs(..) => unreachable!("UnionIs"),
                UnOperator::AdtAccess(..) => (true, 1, Fix::Postfix),
                UnOperator::EnumAccess(..) => (true, 1, Fix::Postfix),
            }),
            Operator::BinaryOperator(bin_op) => Some(match bin_op {
                BinOperator::In => (false, 17, Fix::Dummy),
                BinOperator::Is => (true, 8, Fix::Dummy),
                BinOperator::As => (true, 4, Fix::Dummy),
                BinOperator::Of => (true, 8, Fix::Dummy),
                BinOperator::Range => (true, 16, Fix::Dummy),
                BinOperator::RangeInclusive => (true, 16, Fix::Dummy),
                BinOperator::Dot => (true, 1, Fix::Dummy),

                BinOperator::Eq => (true, 9, Fix::Dummy),
                BinOperator::Neq => (true, 9, Fix::Dummy),
                BinOperator::Lt => (true, 8, Fix::Dummy),
                BinOperator::Gt => (true, 8, Fix::Dummy),
                BinOperator::Lte => (true, 8, Fix::Dummy),
                BinOperator::Gte => (true, 8, Fix::Dummy),

                BinOperator::Add => (true, 6, Fix::Dummy),
                BinOperator::Sub => (true, 6, Fix::Dummy),
                BinOperator::Mul => (true, 5, Fix::Dummy),
                BinOperator::Div => (true, 5, Fix::Dummy),
                BinOperator::Mod => (true, 5, Fix::Dummy),

                BinOperator::BitAnd => (true, 10, Fix::Dummy),
                BinOperator::BitOr => (true, 12, Fix::Dummy),
                BinOperator::BitXor => (true, 11, Fix::Dummy),
                BinOperator::ShiftLeft => (true, 7, Fix::Dummy),
                BinOperator::ShiftRight => (true, 7, Fix::Dummy),

                BinOperator::BoolAnd => (true, 14, Fix::Dummy),
                BinOperator::BoolOr => (true, 15, Fix::Dummy),
            }),
        }
    }

    // TODO: FilePosition.
    pub fn info(&self) -> LangResult<OperatorInfo> {
        if let Some(info) = self.lookup() {
            Ok(OperatorInfo {
                eval_ltor: info.0,
                prec: info.1,
                fix: info.2,
            })
        } else {
            Err(LangError::new(
                format!("Invalid operator, unable to get info: {:?}.", self),
                ParseError,
                None,
            ))
        }
    }
}
