use std::{fmt, slice};

use crate::ast::*;
use crate::diagnostic::{Spanned};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(Identifier),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    Whitespace(String),
    Comment(String),
    Tree(Vec<Spanned<Token>>),
    Eof,
}

impl Token {
    pub fn is_whitespace(&self) -> bool {
        matches!(self, Token::Whitespace(_))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(ident) => {
                write!(f, "{}", ident.as_str())
            }
            Token::Literal(lit) => {
                write!(f, "{}", lit)
            }
            Token::Keyword(kw) => {
                write!(f, "{}", kw.as_str())
            }
            Token::Symbol(sym) => {
                write!(f, "{}", sym.as_str())
            }
            Token::Whitespace(ws) => {
                write!(f, "{}", ws.as_str())
            }
            Token::Comment(c) => {
                write!(f, "{}", c.as_str())
            }
            Token::Tree(t) => {
                write!(f, "Tree()")
            }
            Token::Eof => {
                write!(f, "EOF")
            }
        }
    }
}

impl Token {
    pub(crate) fn ident(ident: impl ToString) -> Token {
        Token::Ident(Identifier::new(ident))
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
    pub(crate) fn symbol(s: &str) -> Token {
        Token::Symbol(Symbol::from_str(s).unwrap())
    }
    pub(crate) fn whitespace(s: &str) -> Token {
        Token::Whitespace(s.into())
    }
}

#[derive(Clone, Debug)]
pub struct TokenStream {
    iter: <Vec<Spanned<Token>> as IntoIterator>::IntoIter,
}

impl<'i> TokenStream {
    pub fn new(iter: Vec<Spanned<Token>>) -> Self {
        TokenStream {
            iter: iter.into_iter(),
        }
    }

    pub fn next_token(&mut self) -> Option<Spanned<Token>> {
        self.next()
    }
}

impl<'i> Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|token| match token.inner {
            Token::Comment(_) | Token::Whitespace(_) => self.next(),
            _ => Some(token),
        })
    }
}

/// A token stream iterator.
#[derive(Debug, Clone)]
pub struct TokenStreamIter<'i> {
    iter: slice::Iter<'i, Token>,
}

impl<'i> Iterator for TokenStreamIter<'i> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().cloned()
    }
}
