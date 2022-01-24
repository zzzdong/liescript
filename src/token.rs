#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Whitespace(String),
    Ident(String),
    BoolLit(bool),
    CharLit(char),
    IntegerLit(i64),
    FloatLit(f64),
    StringLit(String),
    Comment(String),
    Keywrod(Keyword),
    Delimiter(Delimiter),
    Punctuation(Punctuation),
    Unknown(String),
    Error(String),
}

impl Token {
    pub fn ident(ident: impl ToString) -> Token {
        Token::Ident(ident.to_string())
    }
    pub fn whitespace(ws: impl ToString) -> Token {
        Token::Whitespace(ws.to_string())
    }
    pub fn int(i: i64) -> Token {
        Token::IntegerLit(i)
    }
    pub fn float(f: f64) -> Token {
        Token::FloatLit(f)
    }
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    As,
    Break,
    Const,
    Continue,
    Crate,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Move,
    Pub,
    Ref,
    Return,
    SelfValue,
    SelfType,
    Static,
    Struct,
    Super,
    Trait,
    Type,
    Unsafe,
    Use,
    Where,
    While,
    Await,
    Async,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimiter {
    LParen,   // (
    RParen,   // )
    LSquare,  // [
    RSquare,  // ]
    LBracket, // {
    RBracker, // }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    Not,        // !
    And,        // &
    Or,         // |
    AndAnd,     // &&
    OrOr,       // ||
    LShift,     // <<
    RShift,     // >>
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=
    CaretEq,    // ^=
    AndEq,      // &=
    OrEq,       // |=
    ShlEq,      // <<=
    ShrEq,      // >>=
    Eq,         // =
    EqEq,       // ==
    NotEq,      // !=
    Lt,         // <
    LtE,        // <=
    Gt,         // >
    GtE,        // >=
    At,         // @
    Underscore, // _
    Dot,        // .
    DotDot,     // ..
    DotDotDot,  // ...
    DotDotEq,   // ..=
    Comman,     // ,
    Semicolon,  // ;
    Colon,      // :
    PathSep,    // ::
    RArrow,     // ->
    FatArrow,   // =>
    Pound,      // #
    Dollar,     // $
    Question,   // ?
}
