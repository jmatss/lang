#[derive(Debug, Clone, PartialEq)]
pub struct LexToken {
    pub kind: LexTokenKind,
    pub line_nr: u64,
    pub column_nr: u64,
}

impl LexToken {
    pub fn new(kind: LexTokenKind, line_nr: u64, column_nr: u64) -> Self {
        Self {
            kind,
            line_nr,
            column_nr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexTokenKind {
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
    Bool(bool),
    Integer(String, u32), // u32 => radix
    Float(String),
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
    Static,
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
    UnderScore,
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
    /// Linebreak, manual linebreak (";") or EndOfFile.
    pub fn is_break_symbol(lex_token: &LexToken) -> bool {
        match lex_token.kind {
            LexTokenKind::Symbol(Symbol::LineBreak)
            | LexTokenKind::Symbol(Symbol::SemiColon)
            | LexTokenKind::EndOfFile => true,
            _ => false,
        }
    }

    /// Sees if the given `ident` is a valid bool literal. If it is, returns
    /// the literal as a token type, otherwise returns None.
    pub fn get_if_bool(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "true" => LexTokenKind::Literal(Literal::Bool(true)),
            "false" => LexTokenKind::Literal(Literal::Bool(false)),
            _ => return None,
        })
    }

    /// Sees if the given `ident` is a valid symbol. If it is, returns
    /// the symbol as a token type, otherwise returns None.
    pub fn get_if_symbol(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "not" => LexTokenKind::Symbol(Symbol::BoolNot),
            "and" => LexTokenKind::Symbol(Symbol::BoolAnd),
            "or" => LexTokenKind::Symbol(Symbol::BoolOr),

            "in" => LexTokenKind::Symbol(Symbol::In),
            "is" => LexTokenKind::Symbol(Symbol::Is),
            "as" => LexTokenKind::Symbol(Symbol::As),
            "of" => LexTokenKind::Symbol(Symbol::Of),

            _ => return None,
        })
    }

    /// Sees if the given `ident` is a valid keyword. If it is, returns
    /// the keyword as a token type, otherwise returns None.
    pub fn get_if_keyword(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "if" => LexTokenKind::Keyword(Keyword::If),
            "else" => LexTokenKind::Keyword(Keyword::Else),
            "match" => LexTokenKind::Keyword(Keyword::Match),

            "for" => LexTokenKind::Keyword(Keyword::For),
            "while" => LexTokenKind::Keyword(Keyword::While),

            "return" => LexTokenKind::Keyword(Keyword::Return),
            "yield" => LexTokenKind::Keyword(Keyword::Yield),
            "break" => LexTokenKind::Keyword(Keyword::Break),
            "continue" => LexTokenKind::Keyword(Keyword::Continue),

            "use" => LexTokenKind::Keyword(Keyword::Use),
            "package" => LexTokenKind::Keyword(Keyword::Package),

            "var" => LexTokenKind::Keyword(Keyword::Var),
            "const" => LexTokenKind::Keyword(Keyword::Const),
            "static" => LexTokenKind::Keyword(Keyword::Static),
            "private" => LexTokenKind::Keyword(Keyword::Private),
            "public" => LexTokenKind::Keyword(Keyword::Public),

            "function" => LexTokenKind::Keyword(Keyword::Function),
            "struct" => LexTokenKind::Keyword(Keyword::Struct),
            "enum" => LexTokenKind::Keyword(Keyword::Enum),
            "interface" => LexTokenKind::Keyword(Keyword::Interface),

            "defer" => LexTokenKind::Keyword(Keyword::Defer),
            "with" => LexTokenKind::Keyword(Keyword::With),

            "test" => LexTokenKind::Keyword(Keyword::Test),

            _ => return None,
        })
    }

    /// Sees if the given character `c1` is a valid symbol. Returns the
    /// symbol as a LexTokenType if this character is a symbol, otherwise None
    /// is returned.
    pub fn get_if_symbol_char(c1: char) -> Option<(LexTokenKind, usize)> {
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
    ) -> Option<(LexTokenKind, usize)> {
        LexToken::symbol_lookup_chars(c1, c2, c3)
    }

    fn symbol_lookup_chars(
        c1: char,
        c2: Option<char>,
        c3: Option<char>,
    ) -> Option<(LexTokenKind, usize)> {
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
            '_' => LexToken::ret_single_lookup(Symbol::UnderScore),
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
                vec![
                    ("=", Symbol::Equals),
                    ("==", Symbol::EqualsOperator),
                    ("=>", Symbol::FatArrow),
                ],
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

    fn ret_single_lookup(symbol: Symbol) -> Option<(LexTokenKind, usize)> {
        Some((LexTokenKind::Symbol(symbol), 1))
    }

    /// Sees if the given `real_string` which is created with the "peeked"
    /// characters forms a valid symbol and in that case returns it.
    fn match_symbol(
        real_string: &str,
        mut string_symbol_tuples: Vec<(&str, Symbol)>,
    ) -> Option<(LexTokenKind, usize)> {
        // Sort the tuples by the length of their "strings".
        // This ensures that the longest symbols are matched
        // (ex. "==" is always matched instead of "=").
        string_symbol_tuples.sort_unstable_by(|a, b| b.0.len().cmp(&a.0.len()));

        for tuple in string_symbol_tuples {
            let current_string = tuple.0;
            let current_symbol = tuple.1;

            if real_string.starts_with(current_string) {
                return Some((LexTokenKind::Symbol(current_symbol), current_string.len()));
            }
        }

        None
    }
}
