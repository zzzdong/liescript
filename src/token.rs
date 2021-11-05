
#[derive(Debug)]
pub enum Token<'s> {
    Ident(&'s str),
    BoolLit(bool),
    CharLit(char),
    IntegerLit(i64),
    FloatLit(f64),
    StringLit(String),
    Comment(&'s str),
    Keywrod(Keyword),
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
