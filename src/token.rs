use std::{borrow::Cow, ops::Deref,};

pub use crate::ast::{Ident, Keyword, Literal, Symbol};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Token { span, kind }
    }
}

impl Deref for Token {
    type Target = TokenKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    start: LineCol,
    end: LineCol,
}

impl Span {
    pub fn new(start: LineCol, end: LineCol) -> Self {
        Span {
            start,
            end,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

impl LineCol {
    pub fn new() -> Self {
        LineCol { line: 1, column: 1 }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Ident(Ident),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    ParenTree(Vec<Token>),
    SquareTree(Vec<Token>),
    BracketTree(Vec<Token>),
    Whitespace,
    Comment,
    Eof,
}



#[derive(Debug)]
pub struct TokenError {
    pub(crate) detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> TokenError {
        TokenError {
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

impl TokenKind {
    pub(crate) fn ident(ident: impl ToString) -> TokenKind {
        TokenKind::Ident(Ident::new(ident))
    }
    pub(crate) fn int(i: i64) -> TokenKind {
        TokenKind::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> TokenKind {
        TokenKind::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> TokenKind {
        TokenKind::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn symbol(s: &str) -> TokenKind {
        TokenKind::Symbol(Symbol::from_str(s).unwrap())
    }
}



