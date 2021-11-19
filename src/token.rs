#[derive(Debug)]
pub enum Token {
    Ident(String),
    BoolLit(bool),
    CharLit(char),
    IntegerLit(i64),
    FloatLit(f64),
    StringLit(String),
    Comment(String),
    Keywrod(Keyword),
    Delimiter(Delimiter),
    Operator(Operator),
    Unknown(String),
    Error(String)
}

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy)]
pub enum Delimiter {
    Comman,    // ,
    Semicolon, // ;
    Dot,       // .
    Assign,    // =
    Reture,    // ->
    LParen,    // (
    RParen,    // )
    LSquare,   // [
    RSquare,   // ]
    LBracket,  // {
    RBracker,  // }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Not,      // !
    Question, // ?
    Plus,     // +
    Minus,    // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    Pow,      // ^
    LShift,   // <<
    RShift,   // >>
    BitOr,    // |
    BitAnd,   // &
    And,      // &&
    Or,       // ||
    Eq,       // ==
    NotEq,    // !=
    Lt,       // <
    LtE,      // <=
    Gt,       // >
    GtE,      // >=
}
