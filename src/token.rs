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
pub enum Token<'i> {
    Eof,
    Whitespace(&'i str),
    Ident(Ident),
    Literal(Literal),
    Comment(&'i str),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Group(GroupType, Vec<Token<'i>>),
    Unknown(char),
    Error(TokenError),
}

impl<'i> Token<'i> {
    pub(crate) fn ident(ident: impl ToString) -> Token<'i> {
        Token::Ident(Ident::new(ident))
    }
    pub(crate) fn whitespace(ws: &'i str) -> Token<'i> {
        Token::Whitespace(ws)
    }
    pub(crate) fn int(i: i64) -> Token<'i> {
        Token::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> Token<'i> {
        Token::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> Token<'i> {
        Token::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn punctuation(s: &str) -> Token {
        Token::Punctuation(Punctuation::from_str(s).unwrap())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GroupType {
    Paren,
    Square,
    Bracket,
}
