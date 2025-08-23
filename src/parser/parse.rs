use std::{borrow::Cow, iter::Peekable};

use crate::{
    ast::{
        token::{Punctuated, Token, TokenSpan}, Ident, Keyword, Lit, Symbol
    },
    diagnostic::{Span, Spanned},
    parser::tokenizer::{TokenStream, Tokenizer},
};

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

    pub fn next_is(&mut self, token: &Token) -> bool {
        matches!(self.iter.peek(), Some(t) if &t.value == token)
    }

    pub fn consume(&mut self) -> Result<TokenSpan, ParseError> {
        self.iter.next().cloned().ok_or(ParseError::eof())
    }

    pub fn expect(&mut self, token: &Token) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;
        if tok.value == *token {
            return self.consume();
        }

        Err(ParseError::new("Expected token")
            .with_span(tok.span)
            .with_expected(token.to_string()))
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

    /// Try to parse a value, but if it fails, reset the stream to the original state
    pub fn try_parse<T, F>(&mut self, f: F) -> Result<T, ParseError>
    where
        F: FnOnce(&mut ParseStream) -> Result<T, ParseError>,
    {
        let mut stream = self.fork();
        match f(&mut stream) {
            Ok(result) => {
                self.reset(stream);
                Ok(result)
            }
            Err(e) => Err(e),
        }
    }

    pub fn fork(&self) -> ParseStream<'i> {
        ParseStream {
            iter: self.iter.clone(),
        }
    }

    pub fn reset(&mut self, other: ParseStream<'i>) {
        self.iter = other.iter;
    }
}

pub trait Parse
where
    Self: Sized,
{
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError>;
}

impl Parse for Lit {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_literal() {
            return Err(ParseError::new("Expected literal value")
                .with_span(tok.span)
                .with_expected("number, string or boolean literal")
                .with_found(tok.value.to_string()));
        }

        match stream.consume() {
            Ok(token) => Ok(token.map(|t| t.into_literal())),
            Err(e) => Err(e.with_expected("literal value")),
        }
    }
}

impl Parse for Ident {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_ident() {
            return Err(ParseError::new("Expected identifier")
                .with_span(tok.span)
                .with_expected("valid identifier")
                .with_found(tok.value.to_string()));
        }

        match stream.consume() {
            Ok(token) => Ok(token.map(|t| t.into_ident())),
            Err(e) => Err(e.with_expected("identifier")),
        }
    }
}

impl Parse for Keyword {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_keyword() {
            return Err(ParseError::new("Expected keyword")
                .with_span(tok.span)
                .with_expected("valid keyword")
                .with_found(tok.value.to_string()));
        }

        match stream.consume() {
            Ok(token) => Ok(token.map(|t| t.into_keyword())),
            Err(e) => Err(e.with_expected("keyword")),
        }
    }
}

impl Parse for Symbol {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.consume()?;

        if !tok.is_symbol() {
            return Err(ParseError::new("Expected symbol")
                .with_span(tok.span)
                .with_expected("valid symbol")
                .with_found(tok.value.to_string()));
        }

        match stream.consume() {
            Ok(token) => Ok(token.map(|t| t.into_symbol())),
            Err(e) => Err(e.with_expected("symbol")),
        }
    }
}
