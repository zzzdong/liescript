use std::borrow::Cow;

use crate::ast::{Ident, Keyword, Literal, Punctuation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    filename: String,
    line: usize,
    column: usize,
}

impl Location {
    pub fn new(filename: impl ToString, line: usize, column: usize) -> Self {
        Location {
            filename: filename.to_string(),
            line,
            column,
        }
    }
}

#[derive(Debug)]
pub struct TokenError {
    location: Location,
    detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(location: Location, detail: D) -> TokenError {
        TokenError {
            location: location,
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

impl PartialEq for TokenError {
    fn eq(&self, other: &Self) -> bool {
        unreachable!()
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Whitespace(String),
    Ident(Ident),
    Literal(Literal),
    Comment(String),
    Keywrod(Keyword),
    Punctuation(Punctuation),
    Unknown(char),
    Error(TokenError),
}

impl Token {
    pub(crate) fn ident(ident: impl ToString) -> Token {
        Token::Ident(Ident::new(ident))
    }
    pub(crate) fn whitespace(ws: impl ToString) -> Token {
        Token::Whitespace(ws.to_string())
    }
    pub(crate) fn int(i: i64) -> Token {
        Token::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> Token {
        Token::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> Token {
        Token::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn punctuation(s: &str) -> Token {
        Token::Punctuation(Punctuation::from_str(s).unwrap())
    }
}
