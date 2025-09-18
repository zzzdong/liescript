use std::{fmt, slice, str::FromStr};

use super::ident::Identifier;
use super::keyword::Keyword;
use super::literal::Literal;
use super::symbol::Symbol;
use crate::diagnostic::{HasSpan, Span, Spanned};

#[macro_export]
macro_rules! token {
    ($s:expr) => {{
        use $crate::lexical::Token;
        use $crate::lexical::keyword::Keyword;

        match $s {
            // 处理关键字
            s if Keyword::from_str(s).is_ok() => Token::Keyword(Keyword::from_str(s).unwrap()),
            // 处理字面量
            s if s.starts_with('\"') => {
                Token::Literal(Literal::String(s.trim_matches('\"').to_string()))
            }
            s if s.parse::<i64>().is_ok() => Token::Literal(Literal::Integer(s.parse().unwrap())),
            s if s.parse::<f64>().is_ok() => Token::Literal(Literal::Float(s.parse().unwrap())),
            // 处理标点符号
            s if Symbol::from_str(s).is_some() => Token::Symbol(Punct::from_str(s).unwrap()),
            // 默认作为标识符
            s => Token::Ident(Identifier::new(s)),
        }
    }};
}

pub type TokenSpan = Spanned<Token>;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    /// 标识符: [a-zA-Z_][a-zA-Z0-9_]*
    Ident(Identifier),

    /// 字面量: 数字/字符串/字符等
    Literal(Literal),

    /// 关键字: if/else/fn等
    Keyword(Keyword),

    /// 标点符号
    Symbol(Symbol),

    /// 空白: 空格/制表符/换行等
    Whitespace(String),

    /// 注释: // 或 /* */
    Comment(String),

    /// 树结构: 用于分组
    Tree(Vec<TokenSpan>),

    /// 文件结束标记
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
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Literal(lit) => write!(f, "{}", lit),
            Token::Keyword(kw) => write!(f, "{}", kw),
            Token::Symbol(p) => write!(f, "{}", p),
            Token::Whitespace(_) => write!(f, " "),
            Token::Comment(c) => write!(f, "{}", c),
            Token::Tree(tokens) => {
                write!(f, "(")?;
                for token in tokens {
                    write!(f, "{} ", token)?;
                }
                write!(f, ")")
            }
            Token::Eof => write!(f, ""),
        }
    }
}

impl Token {
    pub fn ident(ident: impl ToString) -> Token {
        Token::Ident(Identifier::new(ident))
    }

    pub fn int(i: i64) -> Token {
        Token::Literal(Literal::Integer(i))
    }

    pub fn float(f: f64) -> Token {
        Token::Literal(Literal::Float(f))
    }

    pub fn string(s: impl ToString) -> Token {
        Token::Literal(Literal::String(s.to_string()))
    }

    pub fn char(c: char) -> Token {
        Token::Literal(Literal::Char(c))
    }

    pub fn keyword(s: &str) -> Token {
        Token::Keyword(Keyword::from_str(s).unwrap())
    }

    pub fn symbol(s: &str) -> Option<Token> {
        Symbol::from_str(s).map(Token::Symbol)
    }

    pub fn whitespace(s: &str) -> Token {
        Token::Whitespace(s.into())
    }

    pub fn comment(s: &str) -> Token {
        Token::Comment(s.into())
    }

    pub fn eof() -> Token {
        Token::Eof
    }
}

impl From<Identifier> for Token {
    fn from(ident: Identifier) -> Self {
        Token::Ident(ident)
    }
}

impl From<Literal> for Token {
    fn from(literal: Literal) -> Self {
        Token::Literal(literal)
    }
}

impl From<Keyword> for Token {
    fn from(keyword: Keyword) -> Self {
        Token::Keyword(keyword)
    }
}

/// `[]`
#[derive(Debug, PartialEq)]
pub struct Bracket {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

impl Bracket {
    pub fn new(open: TokenSpan, close: TokenSpan) -> Self {
        Self { open, close }
    }

    pub fn span(&self) -> Span {
        Span::new(self.open.span().start, self.close.span().end)
    }
}

/// `()`
#[derive(Debug, PartialEq)]
pub struct Paren {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

impl Paren {
    pub fn new(open: TokenSpan, close: TokenSpan) -> Self {
        Self { open, close }
    }

    pub fn span(&self) -> Span {
        Span::new(self.open.span().start, self.close.span().end)
    }
}

/// `{}`
#[derive(Debug, PartialEq)]
pub struct Brace {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

impl Brace {
    pub fn new(open: TokenSpan, close: TokenSpan) -> Self {
        Self { open, close }
    }

    pub fn span(&self) -> Span {
        Span::new(self.open.span().start, self.close.span().end)
    }
}

/// `<>`
#[derive(Debug, PartialEq)]
pub struct Angle {
    pub open: TokenSpan,
    pub close: TokenSpan,
}

impl Angle {
    pub fn new(open: TokenSpan, close: TokenSpan) -> Self {
        Self { open, close }
    }

    pub fn span(&self) -> Span {
        Span::new(self.open.span().start, self.close.span().end)
    }
}

pub trait Bracketed {
    type Output;

    fn bracketed(open: TokenSpan, close: TokenSpan) -> Self::Output;
}

impl Bracketed for Bracket {
    type Output = Self;

    fn bracketed(open: TokenSpan, close: TokenSpan) -> Self {
        Self::new(open, close)
    }
}

impl Bracketed for Paren {
    type Output = Self;

    fn bracketed(open: TokenSpan, close: TokenSpan) -> Self {
        Self::new(open, close)
    }
}

impl Bracketed for Brace {
    type Output = Self;

    fn bracketed(open: TokenSpan, close: TokenSpan) -> Self {
        Self::new(open, close)
    }
}

#[derive(Debug, PartialEq)]
pub struct Punctuated<T> {
    pub items: Vec<(T, TokenSpan)>,
    pub last: Option<Box<T>>,
}

impl<T> Punctuated<T> {
    pub fn new() -> Self {
        Punctuated {
            items: Vec::new(),
            last: None,
        }
    }

    pub fn push(&mut self, item: T, punct: TokenSpan) {
        self.items.push((item, punct));
    }

    pub fn push_last(&mut self, item: T) {
        self.last = Some(Box::new(item));
    }

    pub fn len(&self) -> usize {
        self.items.len() + self.last.is_some() as usize
    }

    pub fn last(&self) -> Option<&T> {
        match &self.last {
            Some(last) => Some(last),
            None => self.items.last().map(|(item, _)| item),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty() && self.last.is_none()
    }
}

impl<T: HasSpan> Punctuated<T> {
    pub fn span(&self) -> Span {
        let span = match (self.items.first(), self.items.last()) {
            (Some((start, _)), Some((_, end))) => Span::new(start.span().start, end.span().end),

            _ => Span::default(),
        };

        match &self.last {
            Some(last) => Span::new(span.start, last.span().end),
            None => span,
        }
    }
}

impl<T: HasSpan> HasSpan for Punctuated<T> {
    fn span(&self) -> Span {
        self.items
            .iter()
            .map(|(item, _)| item.span())
            .chain(self.last.iter().map(|item| item.span()))
            .fold(Span::default(), |span, item| span.join(item))
    }
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
