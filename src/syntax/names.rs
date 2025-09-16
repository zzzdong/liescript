use std::default;

use super::{BinOp, Pattern, RangeLimits, Statement, Type, UnOp};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};

/// 路径，用于表示命名空间中的项
#[derive(Debug, PartialEq)]
pub struct Path {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<PathSegment>,
}

impl Path {
    pub fn span(&self) -> Span {
        let start = if let Some(leading_colon) = &self.leading_colon {
            leading_colon.span().start
        } else if let Some((first, _)) = self.segments.items.first() {
            first.ident.span().start
        } else {
            return Span::default();
        };

        let end = if let Some((last, _)) = self.segments.items.last() {
            last.ident.span().end
        } else {
            return Span::default();
        };

        Span::new(start, end)
    }
}

impl HasSpan for Path {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 路径段
#[derive(Debug, PartialEq)]
pub struct PathSegment {
    pub ident: IdentSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(Option<TokenSpan>),
    Private(Option<TokenSpan>),
    Inherited,
}

impl Visibility {
    pub fn span(&self) -> Span {
        match self {
            Self::Public(Some(span)) => span.span(),
            Self::Private(Some(span)) => span.span(),
            _ => Span::default(),
        }
    }
}

impl HasSpan for Visibility {
    fn span(&self) -> Span {
        self.span()
    }
}

