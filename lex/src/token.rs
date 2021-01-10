use common::{file::FilePosition, token::lit::Lit};

#[derive(Debug, Clone, PartialEq)]
pub struct LexToken {
    pub kind: LexTokenKind,
    pub file_pos: FilePosition,
}

impl LexToken {
    pub fn new(kind: LexTokenKind, file_pos: FilePosition) -> Self {
        Self { kind, file_pos }
    }

    #[allow(clippy::match_like_matches_macro)]
    pub fn is_eof(&self) -> bool {
        if let LexTokenKind::EOF = self.kind {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexTokenKind {
    Ident(String),
    Lit(Lit),
    Kw(Kw),
    Sym(Sym),
    Comment(String, bool), // (true => single line), (false => multi line)
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Kw {
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
    External,

    Var,
    Const,
    Static,
    Private,
    Public,

    Function,
    Struct,
    Enum,
    Trait,
    Implement,
    Implements,
    Where,
    //Macro,
    Defer,

    Test,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sym {
    ParenthesisBegin,
    ParenthesisEnd,
    SquareBracketBegin,
    SquareBracketEnd,
    CurlyBracketBegin,
    CurlyBracketEnd,
    PointyBracketBegin,
    PointyBracketEnd,

    Dot,
    TripleDot,
    Comma,
    UnderScore,
    QuestionMark,
    ExclamationMark,
    Equals,
    DoubleQuote,
    SingleQuote,
    Colon,
    DoubleColon,
    SemiColon,
    Pound,
    At,
    Dollar,
    LineBreak,
    WhiteSpace(usize),

    Pipe,            // |>
    Range,           // ..
    RangeInclusive,  // ..=
    Arrow,           // ->
    FatArrow,        // =>
    Deref,           // .*
    Address,         // .&
    ArrayIndexBegin, // .[

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
    #[allow(clippy::match_like_matches_macro)]
    pub fn is_break_symbol(&self) -> bool {
        match self.kind {
            LexTokenKind::Sym(Sym::LineBreak) | LexTokenKind::Sym(Sym::SemiColon) => true,
            _ => false,
        }
    }

    /// Sees if the given `ident` is a valid bool literal. If it is, returns
    /// the literal as a token type, otherwise returns None.
    pub fn get_if_bool(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "true" => LexTokenKind::Lit(Lit::Bool(true)),
            "false" => LexTokenKind::Lit(Lit::Bool(false)),
            _ => return None,
        })
    }

    /// Sees if the given `ident` is a valid symbol. If it is, returns
    /// the symbol as a token type, otherwise returns None.
    pub fn get_if_symbol(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "not" => LexTokenKind::Sym(Sym::BoolNot),
            "and" => LexTokenKind::Sym(Sym::BoolAnd),
            "or" => LexTokenKind::Sym(Sym::BoolOr),

            "in" => LexTokenKind::Sym(Sym::In),
            "is" => LexTokenKind::Sym(Sym::Is),
            "as" => LexTokenKind::Sym(Sym::As),
            "of" => LexTokenKind::Sym(Sym::Of),

            _ => return None,
        })
    }

    /// Sees if the given `ident` is a valid keyword. If it is, returns
    /// the keyword as a token type, otherwise returns None.
    pub fn get_if_keyword(ident: &str) -> Option<LexTokenKind> {
        Some(match ident {
            "if" => LexTokenKind::Kw(Kw::If),
            "else" => LexTokenKind::Kw(Kw::Else),
            "match" => LexTokenKind::Kw(Kw::Match),

            "for" => LexTokenKind::Kw(Kw::For),
            "while" => LexTokenKind::Kw(Kw::While),

            "return" => LexTokenKind::Kw(Kw::Return),
            "yield" => LexTokenKind::Kw(Kw::Yield),
            "break" => LexTokenKind::Kw(Kw::Break),
            "continue" => LexTokenKind::Kw(Kw::Continue),

            "use" => LexTokenKind::Kw(Kw::Use),
            "package" => LexTokenKind::Kw(Kw::Package),
            "external" => LexTokenKind::Kw(Kw::External),

            "var" => LexTokenKind::Kw(Kw::Var),
            "const" => LexTokenKind::Kw(Kw::Const),
            "static" => LexTokenKind::Kw(Kw::Static),
            "private" => LexTokenKind::Kw(Kw::Private),
            "public" => LexTokenKind::Kw(Kw::Public),

            "function" => LexTokenKind::Kw(Kw::Function),
            "struct" => LexTokenKind::Kw(Kw::Struct),
            "enum" => LexTokenKind::Kw(Kw::Enum),
            "implement" => LexTokenKind::Kw(Kw::Implement),
            "implements" => LexTokenKind::Kw(Kw::Implements),
            "trait" => LexTokenKind::Kw(Kw::Trait),
            "where" => LexTokenKind::Kw(Kw::Where),

            "defer" => LexTokenKind::Kw(Kw::Defer),

            "test" => LexTokenKind::Kw(Kw::Test),

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
            '(' => LexToken::ret_single_lookup(Sym::ParenthesisBegin),
            ')' => LexToken::ret_single_lookup(Sym::ParenthesisEnd),
            '[' => LexToken::ret_single_lookup(Sym::SquareBracketBegin),
            ']' => LexToken::ret_single_lookup(Sym::SquareBracketEnd),
            '{' => LexToken::ret_single_lookup(Sym::CurlyBracketBegin),
            '}' => LexToken::ret_single_lookup(Sym::CurlyBracketEnd),
            ',' => LexToken::ret_single_lookup(Sym::Comma),
            '_' => LexToken::ret_single_lookup(Sym::UnderScore),
            '?' => LexToken::ret_single_lookup(Sym::QuestionMark),
            '\"' => LexToken::ret_single_lookup(Sym::DoubleQuote),
            '\'' => LexToken::ret_single_lookup(Sym::SingleQuote),
            ';' => LexToken::ret_single_lookup(Sym::SemiColon),
            '~' => LexToken::ret_single_lookup(Sym::BitCompliment),
            '#' => LexToken::ret_single_lookup(Sym::Pound),
            '@' => LexToken::ret_single_lookup(Sym::At),
            '$' => LexToken::ret_single_lookup(Sym::Dollar),

            ':' => LexToken::match_symbol(
                &real_string,
                vec![(":", Sym::Colon), ("::", Sym::DoubleColon)],
            ),

            '%' => LexToken::match_symbol(
                &real_string,
                vec![("%", Sym::Modulus), ("%=", Sym::AssignModulus)],
            ),

            '&' => LexToken::match_symbol(
                &real_string,
                vec![("&", Sym::BitAnd), ("&=", Sym::AssignBitAnd)],
            ),

            '^' => LexToken::match_symbol(
                &real_string,
                vec![("^", Sym::BitXor), ("^=", Sym::AssignBitXor)],
            ),

            '!' => LexToken::match_symbol(
                &real_string,
                vec![("!", Sym::ExclamationMark), ("!=", Sym::NotEquals)],
            ),

            '|' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("|", Sym::BitOr),
                    ("|>", Sym::Pipe),
                    ("|=", Sym::AssignBitOr),
                ],
            ),

            '.' => LexToken::match_symbol(
                &real_string,
                vec![
                    (".", Sym::Dot),
                    ("..", Sym::Range),
                    (".*", Sym::Deref),
                    (".&", Sym::Address),
                    (".[", Sym::ArrayIndexBegin),
                    ("..=", Sym::RangeInclusive),
                    ("...", Sym::TripleDot),
                ],
            ),

            '=' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("=", Sym::Equals),
                    ("==", Sym::EqualsOperator),
                    ("=>", Sym::FatArrow),
                ],
            ),

            '+' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("+", Sym::Plus),
                    ("++", Sym::Increment),
                    ("+=", Sym::AssignAddition),
                ],
            ),

            '<' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("<", Sym::PointyBracketBegin),
                    ("<=", Sym::LessThanOrEquals),
                    ("<<", Sym::ShiftLeft),
                    ("<<=", Sym::AssignShiftLeft),
                ],
            ),

            '>' => LexToken::match_symbol(
                &real_string,
                vec![
                    (">", Sym::PointyBracketEnd),
                    (">=", Sym::GreaterThanOrEquals),
                    (">>", Sym::ShiftRight),
                    (">>=", Sym::AssignShiftRight),
                ],
            ),

            '-' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("-", Sym::Minus),
                    ("--", Sym::Decrement),
                    ("->", Sym::Arrow),
                    ("-=", Sym::AssignSubtraction),
                ],
            ),

            '*' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("*", Sym::Multiplication),
                    ("*/", Sym::CommentMultiLineEnd),
                    ("*=", Sym::AssignMultiplication),
                ],
            ),

            '/' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("/", Sym::Division),
                    ("//", Sym::CommentSingleLine),
                    ("/*", Sym::CommentMultiLineBegin),
                    ("/=", Sym::AssignDivision),
                ],
            ),

            _ => None,
        }
    }

    fn ret_single_lookup(symbol: Sym) -> Option<(LexTokenKind, usize)> {
        Some((LexTokenKind::Sym(symbol), 1))
    }

    /// Sees if the given `real_string` which is created with the "peeked"
    /// characters forms a valid symbol and in that case returns it.
    fn match_symbol(
        real_string: &str,
        mut string_symbol_tuples: Vec<(&str, Sym)>,
    ) -> Option<(LexTokenKind, usize)> {
        // Sort the tuples by the length of their "strings".
        // This ensures that the longest symbols are matched
        // (ex. "==" is always matched instead of "=").
        string_symbol_tuples.sort_unstable_by(|a, b| b.0.len().cmp(&a.0.len()));

        for tuple in string_symbol_tuples {
            let current_string = tuple.0;
            let current_symbol = tuple.1;

            if real_string.starts_with(current_string) {
                return Some((LexTokenKind::Sym(current_symbol), current_string.len()));
            }
        }

        None
    }
}
