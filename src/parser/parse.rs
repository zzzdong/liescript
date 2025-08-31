use std::{borrow::Cow, iter::Peekable};

use crate::{
    diagnostic::{Span, Spanned},
    lexical::{
        Brace, Bracket, IdentSpan, Keyword, KeywordSpan, Literal, LiteralSpan, Paren, Punctuated,
        Symbol, SymbolSpan, Token, TokenSpan, TokenStream, Tokenizer,
    },
    syntax::Visibility,
};

/// 用于匹配Token的trait
pub trait TokenMatcher: std::fmt::Debug {
    fn matches(&self, token: &Token) -> bool;
}

impl TokenMatcher for Token {
    fn matches(&self, token: &Token) -> bool {
        self == token
    }
}

impl TokenMatcher for &Token {
    fn matches(&self, token: &Token) -> bool {
        *self == token
    }
}

impl TokenMatcher for Symbol {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Symbol(s) if s == self)
    }
}

impl TokenMatcher for Keyword {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Keyword(k) if k == self)
    }
}

impl TokenMatcher for &[Token] {
    fn matches(&self, token: &Token) -> bool {
        self.contains(token)
    }
}

// impl<F> TokenMatcher for F
// where
//     F: Fn(&Token) -> bool + std::fmt::Debug,
// {
//     fn matches(&self, token: &Token) -> bool {
//         self(token)
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    message: String,
    span: Option<Span>,
    expected: Vec<String>,
    found: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>) -> Self {
        ParseError {
            message: message.into(),
            span: None,
            expected: Vec::new(),
            found: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
        self
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn expected(&self) -> &[String] {
        &self.expected
    }

    pub fn found(&self) -> Option<&str> {
        self.found.as_deref()
    }

    pub fn eof() -> Self {
        ParseError::new("unexpected end of input")
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;

        if !self.expected.is_empty() {
            write!(f, ", expected {}", self.expected.join(", "))?;
        }

        if let Some(found) = &self.found {
            write!(f, ", but found {}", found)?;
        }

        Ok(())
    }
}

impl std::error::Error for ParseError {}

pub struct ParseStream<'i> {
    iter: Peekable<std::slice::Iter<'i, TokenSpan>>,
}

impl<'i> ParseStream<'i> {
    pub fn new(token_stream: &'i TokenStream) -> Result<Self, ParseError> {
        Ok(ParseStream {
            iter: token_stream.iter().peekable(),
        })
    }

    pub fn lookahead1(&mut self) -> Result<&'i TokenSpan, ParseError> {
        self.iter.peek().map(|v| *v).ok_or(ParseError::eof())
    }

    pub fn peek(&mut self) -> Option<&TokenSpan> {
        self.iter.peek().cloned()
    }

    pub fn is_empty(&mut self) -> bool {
        self.iter.peek().is_none()
    }

    pub fn next_is<M: TokenMatcher>(&mut self, matcher: M) -> bool {
        matches!(self.iter.peek(), Some(t) if matcher.matches(&t.value))
    }

    pub fn consume(&mut self) -> Result<TokenSpan, ParseError> {
        self.iter.next().cloned().ok_or(ParseError::eof())
    }

    pub fn next_is_identifier(&mut self) -> bool {
        matches!(self.iter.peek(), Some(t) if t.value.is_ident())
    }

    pub fn expect_identifier(&mut self) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;

        if let Token::Ident(_) = &tok.value {
            return self.consume();
        }

        Err(ParseError::new("Expected identifier")
            .with_span(tok.span)
            .with_expected("identifier"))
    }

    pub fn expect_symbol(&mut self, symbol: Symbol) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;
        if let Token::Symbol(s) = &tok.value {
            if *s == symbol {
                return self.consume();
            }
        }
        Err(ParseError::new("Expected symbol")
            .with_span(tok.span)
            .with_expected(symbol.as_str()))
    }

    pub fn expect(&mut self, expected: &Token) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;
        if expected.matches(&tok.value) {
            return self.consume();
        }

        Err(ParseError::new("Expected token")
            .with_span(tok.span)
            .with_expected(format!("{:?}", expected)))
    }

    pub fn parse_optional<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        let checkpoint = self.checkpoint();
        match T::parse(self) {
            Ok(value) => Some(value),
            Err(_) => {
                self.restore(checkpoint);
                None
            }
        }
    }

    /// Try to parse a value, but if it fails, reset the stream to the original state
    pub fn try_parse<T, F>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut ParseStream) -> Result<T, ParseError>,
    {
        let checkpoint = self.checkpoint();

        match f(self) {
            Ok(result) => Some(result),
            Err(e) => {
                self.restore(checkpoint);
                None
            }
        }
    }

    pub fn parse_punctuated<T>(&mut self, sep: &Token) -> Result<Punctuated<T>, ParseError>
    where
        T: Parse,
    {
        let mut items = Vec::new();
        let mut last = None;

        loop {
            match self.try_parse(T::parse) {
                Ok(item) => {
                    if self.next_is(sep) {
                        items.push((item, self.consume()?));
                    } else {
                        last = Some(Box::new(item));
                        break;
                    }
                }
                Err(_) => {
                    break;
                }
            }
        }

        Ok(Punctuated { items, last })
    }

    pub fn parse_punctuated_with<T, F>(
        &mut self,
        f: F,
        sep: &Token,
    ) -> Result<Punctuated<T>, ParseError>
    where
        T: Parse,
        F: Fn(&mut ParseStream) -> Result<T, ParseError>,
    {
        let mut items = Vec::new();
        let mut last = None;

        loop {
            match self.try_parse(&f) {
                Ok(item) => {
                    if self.next_is(sep) {
                        items.push((item, self.consume()?));
                    } else {
                        last = Some(Box::new(item));
                        break;
                    }
                }
                Err(_) => {
                    break;
                }
            }
        }

        Ok(Punctuated { items, last })
    }

    pub fn checkpoint(&self) -> Checkpoint<'i> {
        Checkpoint {
            iter: self.iter.clone(),
        }
    }

    pub fn restore(&mut self, checkpoint: Checkpoint<'i>) {
        self.iter = checkpoint.iter;
    }
}

pub struct Checkpoint<'i> {
    iter: Peekable<std::slice::Iter<'i, TokenSpan>>,
}

pub trait Parse
where
    Self: Sized,
{
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError>;
}

impl<T: Parse> Parse for Spanned<T> {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let start_token = stream.lookahead1()?;
        let value = T::parse(stream)?;
        let end_token = stream.lookahead1().map(|t| t).unwrap_or(start_token);

        Ok(Spanned {
            value,
            span: Span {
                start: start_token.span.start,
                end: end_token.span.end,
            },
        })
    }
}

impl Parse for LiteralSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_literal() {
            return Err(ParseError::new("Expected literal value")
                .with_span(tok.span)
                .with_expected("valid literal")
                .with_found(format!("{:?}", tok.value)));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_literal()))
    }
}

impl Parse for IdentSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_ident() {
            return Err(ParseError::new("Expected identifier")
                .with_span(tok.span)
                .with_expected("valid identifier")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_ident()))
    }
}

impl Parse for KeywordSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_keyword() {
            return Err(ParseError::new("Expected keyword")
                .with_span(tok.span)
                .with_expected("valid keyword")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_keyword()))
    }
}

impl Parse for SymbolSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.consume()?;

        if !tok.is_symbol() {
            return Err(ParseError::new("Expected symbol")
                .with_span(tok.span)
                .with_expected("valid symbol")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_symbol()))
    }
}

impl Parse for usize {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_literal() {
            return Err(ParseError::new("Expected literal")
                .with_span(tok.span)
                .with_expected("valid literal")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        let literal = token.value.into_literal();

        // TODO: Support other numeric types
        if let Literal::Integer(lit) = literal {
            return Ok(lit as usize);
        }

        Err(ParseError::new("Expected literal")
            .with_span(tok.span)
            .with_expected("integer literal")
            .with_found(tok.value.to_string()))
    }
}
