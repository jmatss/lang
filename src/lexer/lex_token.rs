#[derive(Debug, Clone, PartialEq)]
pub struct LexToken {
    pub t: LexTokenType,
    pub lineNr: u64,
    pub columnNr: u64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexTokenType {
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    EndOfFile,
    Unknown(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    StringLiteral(String),
    CharLiteral(String),
    Number(String, u32), // u32 => radix
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    If,
    Else,
    Match,

    For,
    While,

    Return,
    Yield,
    Break,
    Continue,

    Use,
    Package,

    Var,
    Const,
    Private,
    Public,

    Function,
    Struct,
    Enum,
    Interface,
    //Macro,
    Defer,
    With,

    Test,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    ParenthesisBegin,
    ParenthesisEnd,
    SquareBracketBegin,
    SquareBracketEnd,
    CurlyBracketBegin,
    CurlyBracketEnd,
    PointyBracketBegin,
    PointyBracketEnd,

    Dot,
    Comma,
    QuestionMark,
    ExclamationMark,
    Equals,
    DoubleQuote,
    SingleQuote,
    Colon,
    SemiColon,
    Pound,
    At,
    Dollar,
    LineBreak,
    WhiteSpace(usize),

    Pipe,           // |>
    Range,          // ..
    RangeInclusive, // ..=
    Arrow,          // ->
    FatArrow,       // =>
    Deref,          // .*
    Address,        // .&

    EqualsOperator,
    NotEquals,
    //LessThan, == PointyBracketBegin
    //GreaterThan, == PointyBracketEnd
    LessThanOrEquals,
    GreaterThanOrEquals,

    Plus,
    Minus,
    Multiplication,
    Division,
    Modulus,
    Power,

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    BitCompliment,

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

    Increment,
    Decrement,

    CommentSingleLine,
    CommentMultiLineBegin,
    CommentMultiLineEnd,

    BoolNot,
    BoolAnd,
    BoolOr,

    In,
    // Is == pattern matching
    Is,
    // cast
    As,
    // Of == instanceof
    Of,
}

impl LexToken {
    // Linebreak, manual linebreak (";") or EndOfFile.
    pub fn is_break_symbol(lex_token: &LexToken) -> bool {
        match lex_token.t {
            LexTokenType::Symbol(Symbol::LineBreak)
            | LexTokenType::Symbol(Symbol::SemiColon)
            | LexTokenType::EndOfFile => true,
            _ => false,
        }
    }

    /// Sees if the given `identifer` is a valid symbol. If it is, returns
    /// the symbol as a token type, otherwise returns None.
    pub fn get_if_symbol(identifier: &str) -> Option<LexTokenType> {
        Some(match identifier {
            "not" => LexTokenType::Symbol(Symbol::BoolNot),
            "and" => LexTokenType::Symbol(Symbol::BoolAnd),
            "or" => LexTokenType::Symbol(Symbol::BoolOr),

            "in" => LexTokenType::Symbol(Symbol::In),
            "is" => LexTokenType::Symbol(Symbol::Is),
            "as" => LexTokenType::Symbol(Symbol::As),
            "of" => LexTokenType::Symbol(Symbol::Of),

            _ => return None,
        })
    }

    /// Sees if the given `identifer` is a valid keyword. If it is, returns
    /// the keyword as a token type, otherwise returns None.
    pub fn get_if_keyword(identifier: &str) -> Option<LexTokenType> {
        Some(match identifier {
            "if" => LexTokenType::Keyword(Keyword::If),
            "else" => LexTokenType::Keyword(Keyword::Else),
            "match" => LexTokenType::Keyword(Keyword::Match),

            "for" => LexTokenType::Keyword(Keyword::For),
            "while" => LexTokenType::Keyword(Keyword::While),

            "return" => LexTokenType::Keyword(Keyword::Return),
            "yield" => LexTokenType::Keyword(Keyword::Yield),
            "break" => LexTokenType::Keyword(Keyword::Break),
            "continue" => LexTokenType::Keyword(Keyword::Continue),

            "use" => LexTokenType::Keyword(Keyword::Use),
            "package" => LexTokenType::Keyword(Keyword::Package),

            "var" => LexTokenType::Keyword(Keyword::Var),
            "const" => LexTokenType::Keyword(Keyword::Const),
            "private" => LexTokenType::Keyword(Keyword::Private),
            "public" => LexTokenType::Keyword(Keyword::Public),

            "function" => LexTokenType::Keyword(Keyword::Function),
            "struct" => LexTokenType::Keyword(Keyword::Struct),
            "enum" => LexTokenType::Keyword(Keyword::Enum),
            "interface" => LexTokenType::Keyword(Keyword::Interface),

            "defer" => LexTokenType::Keyword(Keyword::Defer),
            "with" => LexTokenType::Keyword(Keyword::With),

            "test" => LexTokenType::Keyword(Keyword::Test),

            _ => return None,
        })
    }

    /// Sees if the given character `c1` is a valid symbol. Returns the
    /// symbol as a LexTokenType if this character is a symbol, otherwise None
    /// is returned.
    pub fn get_if_symbol_char(c1: char) -> Option<(LexTokenType, usize)> {
        LexToken::symbol_lookup_chars(c1, None, None)
    }

    /// Sees if the given characters `c1`, `c2` and `c3` forms a valid symbol.
    /// Returns the symbol as a LexTokenType if this character is a symbol together
    /// with how many characters long the symbol is (1-3), otherwise None
    /// is returned.
    pub fn get_if_symbol_three_chars(
        c1: char,
        c2: Option<char>,
        c3: Option<char>,
    ) -> Option<(LexTokenType, usize)> {
        LexToken::symbol_lookup_chars(c1, c2, c3)
    }

    fn symbol_lookup_chars(
        c1: char,
        c2: Option<char>,
        c3: Option<char>,
    ) -> Option<(LexTokenType, usize)> {
        let mut tmp_chars = Vec::with_capacity(3);

        // Put the characters that exists (is Some) into a string so that it can
        // be matched easier.
        tmp_chars.push(c1);
        if c2.is_some() {
            tmp_chars.push(c2.expect("Unable to unwrap c2"));
            if c3.is_some() {
                tmp_chars.push(c3.expect("Unable to unwrap c3"));
            }
        }
        let real_string: String = tmp_chars.into_iter().collect();

        match c1 {
            '(' => LexToken::ret_single_lookup(Symbol::ParenthesisBegin),
            ')' => LexToken::ret_single_lookup(Symbol::ParenthesisEnd),
            '[' => LexToken::ret_single_lookup(Symbol::SquareBracketBegin),
            ']' => LexToken::ret_single_lookup(Symbol::SquareBracketEnd),
            '{' => LexToken::ret_single_lookup(Symbol::CurlyBracketBegin),
            '}' => LexToken::ret_single_lookup(Symbol::CurlyBracketEnd),
            ',' => LexToken::ret_single_lookup(Symbol::Comma),
            '?' => LexToken::ret_single_lookup(Symbol::QuestionMark),
            '\"' => LexToken::ret_single_lookup(Symbol::DoubleQuote),
            '\'' => LexToken::ret_single_lookup(Symbol::SingleQuote),
            ':' => LexToken::ret_single_lookup(Symbol::Colon),
            ';' => LexToken::ret_single_lookup(Symbol::SemiColon),
            '~' => LexToken::ret_single_lookup(Symbol::BitCompliment),
            '#' => LexToken::ret_single_lookup(Symbol::Pound),
            '@' => LexToken::ret_single_lookup(Symbol::At),
            '$' => LexToken::ret_single_lookup(Symbol::Dollar),

            '%' => LexToken::match_symbol(
                &real_string,
                vec![("%", Symbol::Modulus), ("%=", Symbol::AssignModulus)],
            ),

            '&' => LexToken::match_symbol(
                &real_string,
                vec![("&", Symbol::BitAnd), ("&=", Symbol::AssignBitAnd)],
            ),

            '^' => LexToken::match_symbol(
                &real_string,
                vec![("^", Symbol::BitXor), ("^=", Symbol::AssignBitXor)],
            ),

            '!' => LexToken::match_symbol(
                &real_string,
                vec![("!", Symbol::ExclamationMark), ("!=", Symbol::NotEquals)],
            ),

            '|' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("|", Symbol::BitOr),
                    ("|>", Symbol::Pipe),
                    ("|=", Symbol::AssignBitOr),
                ],
            ),

            '.' => LexToken::match_symbol(
                &real_string,
                vec![
                    (".", Symbol::Dot),
                    ("..", Symbol::Range),
                    (".*", Symbol::Deref),
                    (".&", Symbol::Address),
                    ("..=", Symbol::RangeInclusive),
                ],
            ),

            '=' => LexToken::match_symbol(
                &real_string,
                vec![("=", Symbol::Equals), ("==", Symbol::EqualsOperator)],
            ),

            '+' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("+", Symbol::Plus),
                    ("++", Symbol::Increment),
                    ("+=", Symbol::AssignAddition),
                ],
            ),

            '<' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("<", Symbol::PointyBracketBegin),
                    ("<=", Symbol::LessThanOrEquals),
                    ("<<", Symbol::ShiftLeft),
                    ("<<=", Symbol::AssignShiftLeft),
                ],
            ),

            '>' => LexToken::match_symbol(
                &real_string,
                vec![
                    (">", Symbol::PointyBracketEnd),
                    (">=", Symbol::GreaterThanOrEquals),
                    (">>", Symbol::ShiftRight),
                    (">>=", Symbol::AssignShiftRight),
                ],
            ),

            '-' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("-", Symbol::Minus),
                    ("--", Symbol::Decrement),
                    ("->", Symbol::Arrow),
                    ("-=", Symbol::AssignSubtraction),
                ],
            ),

            '*' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("*", Symbol::Multiplication),
                    ("*/", Symbol::CommentMultiLineEnd),
                    ("**", Symbol::Power),
                    ("*=", Symbol::AssignMultiplication),
                    ("**=", Symbol::AssignPower),
                ],
            ),

            '/' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("/", Symbol::Division),
                    ("//", Symbol::CommentSingleLine),
                    ("/*", Symbol::CommentMultiLineBegin),
                    ("/=", Symbol::AssignDivision),
                ],
            ),

            _ => None,
        }
    }

    fn ret_single_lookup(symbol: Symbol) -> Option<(LexTokenType, usize)> {
        Some((LexTokenType::Symbol(symbol), 1))
    }

    /// Sees if the given `real_string` which is created with the "peeked"
    /// characters forms a valid symbol and in that case returns it.
    fn match_symbol(
        real_string: &str,
        mut string_symbol_tuples: Vec<(&str, Symbol)>,
    ) -> Option<(LexTokenType, usize)> {
        // Sort the tuples by the length of their "strings".
        // This ensures that the longest symbols are matched
        // (ex. "==" is always matched instead of "=").
        string_symbol_tuples.sort_unstable_by(|a, b| b.0.len().cmp(&a.0.len()));

        for tuple in string_symbol_tuples {
            let current_string = tuple.0;
            let current_symbol = tuple.1;

            if real_string.starts_with(current_string) {
                return Some((LexTokenType::Symbol(current_symbol), current_string.len()));
            }
        }

        None
    }
}
