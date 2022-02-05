use common::{
    file::FilePosition,
    token::{ast::CommentType, lit::Lit},
};

#[derive(Debug, Clone, PartialEq)]
pub struct LexToken {
    pub kind: LexTokenKind,
    pub file_pos: FilePosition,
}

impl LexToken {
    pub fn new(kind: LexTokenKind, file_pos: FilePosition) -> Self {
        Self { kind, file_pos }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, LexTokenKind::EOF)
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq)]
pub enum LexTokenKind {
    Ident(String),
    Lit(Lit),
    Kw(Kw),
    Sym(Sym),
    Comment(String, CommentType),
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
    Module,
    External,

    Var,
    Const,
    Final,
    Static,
    /// Only accessable from the same file (or "class" ?).
    Private,
    /// Will be a exposed symbol, can be linked to from other "modules".
    Public,
    /// Only accessable from the current module (not exposed as a symbol).
    Hidden,

    Struct,
    Enum,
    Union,

    Trait,
    Implement,

    Where,
    //Macro,
    Defer,

    Function,
    FunctionPointer,
    Test,
}

impl Kw {
    pub fn is_modifier(&self) -> bool {
        matches!(self, Kw::Private | Kw::Public | Kw::Hidden)
    }
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
    TupleIndexBegin, // .(
    Increment,       // .++
    Decrement,       // .--
    DoubleQuoteC,    // c"
    DoubleQuoteS,    // s"
    DoubleQuoteF,    // f"

    DoubleEquals,
    NotEquals,
    //LessThan, == PointyBracketBegin
    //GreaterThan, == PointyBracketEnd
    Lte,
    Gte,

    Plus,
    Minus,
    Mul,
    Div,
    Mod,

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    BitCompliment,

    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,

    AssignBitAnd,
    AssignBitOr,
    AssignBitXor,
    AssignShiftLeft,
    AssignShiftRight,

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
    pub fn is_break_symbol(&self) -> bool {
        matches!(
            self.kind,
            LexTokenKind::Sym(Sym::LineBreak) | LexTokenKind::Sym(Sym::SemiColon)
        )
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
            "mod" => LexTokenKind::Kw(Kw::Module),
            "ext" => LexTokenKind::Kw(Kw::External),

            "var" => LexTokenKind::Kw(Kw::Var),
            "const" => LexTokenKind::Kw(Kw::Const),
            "final" => LexTokenKind::Kw(Kw::Final),
            "static" => LexTokenKind::Kw(Kw::Static),

            "priv" => LexTokenKind::Kw(Kw::Private),
            "pub" => LexTokenKind::Kw(Kw::Public),
            "hid" => LexTokenKind::Kw(Kw::Hidden),

            "struct" => LexTokenKind::Kw(Kw::Struct),
            "enum" => LexTokenKind::Kw(Kw::Enum),
            "union" => LexTokenKind::Kw(Kw::Union),

            "trait" => LexTokenKind::Kw(Kw::Trait),
            "impl" => LexTokenKind::Kw(Kw::Implement),

            "where" => LexTokenKind::Kw(Kw::Where),
            "defer" => LexTokenKind::Kw(Kw::Defer),

            "fn" => LexTokenKind::Kw(Kw::Function),
            "fnptr" => LexTokenKind::Kw(Kw::FunctionPointer),
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
        match (c2, c3) {
            (None, Some(c)) | (Some(c), None) => tmp_chars.push(c),
            (Some(c2), Some(c3)) => tmp_chars.extend_from_slice(&[c2, c3]),
            (None, None) => (),
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

            '%' => {
                LexToken::match_symbol(&real_string, vec![("%", Sym::Mod), ("%=", Sym::AssignMod)])
            }

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
                    (".(", Sym::TupleIndexBegin),
                    (".++", Sym::Increment),
                    (".--", Sym::Decrement),
                    ("..=", Sym::RangeInclusive),
                    ("...", Sym::TripleDot),
                ],
            ),

            '=' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("=", Sym::Equals),
                    ("==", Sym::DoubleEquals),
                    ("=>", Sym::FatArrow),
                ],
            ),

            '+' => {
                LexToken::match_symbol(&real_string, vec![("+", Sym::Plus), ("+=", Sym::AssignAdd)])
            }

            '<' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("<", Sym::PointyBracketBegin),
                    ("<=", Sym::Lte),
                    ("<<", Sym::ShiftLeft),
                    ("<<=", Sym::AssignShiftLeft),
                ],
            ),

            '>' => LexToken::match_symbol(
                &real_string,
                vec![
                    (">", Sym::PointyBracketEnd),
                    (">=", Sym::Gte),
                    (">>", Sym::ShiftRight),
                    (">>=", Sym::AssignShiftRight),
                ],
            ),

            '-' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("-", Sym::Minus),
                    ("->", Sym::Arrow),
                    ("-=", Sym::AssignSub),
                ],
            ),

            '*' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("*", Sym::Mul),
                    ("*/", Sym::CommentMultiLineEnd),
                    ("*=", Sym::AssignMul),
                ],
            ),

            '/' => LexToken::match_symbol(
                &real_string,
                vec![
                    ("/", Sym::Div),
                    ("//", Sym::CommentSingleLine),
                    ("/*", Sym::CommentMultiLineBegin),
                    ("/=", Sym::AssignDiv),
                ],
            ),

            'c' => LexToken::match_symbol(&real_string, vec![("c\"", Sym::DoubleQuoteC)]),
            's' => LexToken::match_symbol(&real_string, vec![("s\"", Sym::DoubleQuoteS)]),
            'f' => LexToken::match_symbol(&real_string, vec![("f\"", Sym::DoubleQuoteF)]),

            _ => None,
        }
    }

    #[allow(clippy::unnecessary_wraps)]
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
