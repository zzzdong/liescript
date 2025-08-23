use std::{fmt, slice};

use crate::ast::ident::Identifier;
use crate::ast::keyword::Keyword;
use crate::ast::literal::Literal;
use crate::ast::symbol::Symbol;
use crate::diagnostic::Spanned;

pub type TokenSpan = Spanned<Token>;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(Identifier),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    Whitespace(String),
    Comment(String),
    Tree(Vec<TokenSpan>),
    Eof,
}

impl Token {
    pub fn is_ident(&self) -> bool {
        matches!(self, Token::Ident(_))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Token::Literal(_))
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self, Token::Keyword(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Token::Symbol(_))
    }
    pub fn is_whitespace(&self) -> bool {
        matches!(self, Token::Whitespace(_))
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Token::Comment(_))
    }

    pub fn is_eof(&self) -> bool {
        matches!(self, Token::Eof)
    }

    pub fn into_ident(self) -> Identifier {
        match self {
            Token::Ident(ident) => ident,
            _ => panic!("Not an identifier"),
        }
    }

    pub fn into_literal(self) -> Literal {
        match self {
            Token::Literal(lit) => lit,
            _ => panic!("Not a literal"),
        }
    }

    pub fn into_keyword(self) -> Keyword {
        match self {
            Token::Keyword(kw) => kw,
            _ => panic!("Not a keyword"),
        }
    }

    pub fn into_symbol(self) -> Symbol {
        match self {
            Token::Symbol(sym) => sym,
            _ => panic!("Not a symbol"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(ident) => {
                write!(f, "{}", ident.as_str())
            }
            Token::Literal(lit) => {
                write!(f, "{lit}")
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
                write!(f, "Tree({t:?})")
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

    pub(crate) fn keyword(s: &str) -> Token {
        Token::Keyword(Keyword::from_str(s).unwrap())
    }

    pub(crate) fn whitespace(s: &str) -> Token {
        Token::Whitespace(s.into())
    }
}

/// `[]`
pub struct Bracket {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

/// `()`
pub struct Paren {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

/// `{}`
pub struct Brace {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

pub struct Punctuated<T> {
    pub items: Vec<(T, TokenSpan)>,
    pub last: Option<Box<T>>,
}

// #[derive(Clone, Debug)]
// pub struct TokenStream {
//     iter: <Vec<TokenSpan> as IntoIterator>::IntoIter,
// }

// impl TokenStream {
//     pub fn new(iter: Vec<TokenSpan>) -> Self {
//         TokenStream {
//             iter: iter.into_iter(),
//         }
//     }

//     pub fn next_token(&mut self) -> Option<TokenSpan> {
//         self.next()
//     }
// }

// impl Iterator for TokenStream {
//     type Item = TokenSpan;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter.next().and_then(|token| match token.value {
//             Token::Comment(_) | Token::Whitespace(_) => self.next(),
//             _ => Some(token),
//         })
//     }
// }

// /// A token stream iterator.
// #[derive(Debug, Clone)]
// pub struct TokenStreamIter<'i> {
//     iter: slice::Iter<'i, Token>,
// }

// impl<'i> Iterator for TokenStreamIter<'i> {
//     type Item = Token;

//     fn next(&mut self) -> Option<Self::Item> {
//         self.iter.next().cloned()
//     }
// }
