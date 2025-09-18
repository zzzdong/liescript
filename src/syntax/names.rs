use std::default;

use super::{BinOp, Pattern, RangeLimits, Statement, Type, UnOp};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Angle, Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};

#[derive(Debug, PartialEq)]
pub struct SimplePath {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<SimplePathSegment>,
}

impl SimplePath {
    pub fn span(&self) -> Span {
        self.segments.span()
    }

    pub fn is_ends_with_colon(&self) -> bool {
        self.segments.last.is_none()
    }
}

impl HasSpan for SimplePath {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub enum SimplePathSegment {
    Ident(IdentSpan),
    Super(TokenSpan),
    Self_(TokenSpan),
    Crate(TokenSpan),
}

impl SimplePathSegment {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(ident) => ident.span(),
            Self::Super(span) => span.span(),
            Self::Self_(span) => span.span(),
            Self::Crate(span) => span.span(),
        }
    }
}

impl HasSpan for SimplePathSegment {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub struct PathInExpression {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<PathExprSegment>,
}

impl PathInExpression {
    pub fn span(&self) -> Span {
        Span::new(
            self.leading_colon
                .as_ref()
                .map(|t| t.span().start)
                .unwrap_or(self.segments.span().start),
            self.segments.span().end,
        )
    }
}

impl HasSpan for PathInExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub struct PathExprSegment {
    pub ident: PathIdentSegment,
    pub args: Option<(TokenSpan, GenericArgs)>,
}

impl PathExprSegment {
    pub fn span(&self) -> Span {
        Span::new(
            self.ident.span().start,
            self.args
                .as_ref()
                .map(|(span, _)| span.span().end)
                .unwrap_or(self.ident.span().end),
        )
    }
}

impl HasSpan for PathExprSegment {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub enum PathIdentSegment {
    Ident(IdentSpan),
    /// `super`
    Super(TokenSpan),
    /// `self`
    SelfValue(TokenSpan),
    /// `Self`
    SelfType(TokenSpan),
    /// `crate`
    Crate(TokenSpan),
}

impl PathIdentSegment {
    pub fn span(&self) -> Span {
        match self {
            Self::Ident(ident) => ident.span(),
            Self::Super(span) => span.span(),
            Self::SelfValue(span) => span.span(),
            Self::SelfType(span) => span.span(),
            Self::Crate(span) => span.span(),
        }
    }
}

impl HasSpan for PathIdentSegment {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub struct GenericArgs {
    pub angle_token: Angle,
    pub args: Punctuated<GenericArg>,
}

impl GenericArgs {
    pub fn span(&self) -> Span {
        self.angle_token.span()
    }
}

impl HasSpan for GenericArgs {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub enum GenericArg {
    Type(Type),
}

#[derive(Debug, PartialEq)]
pub struct TypePath {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<TypePathSegment>,
}

impl TypePath {
    pub fn span(&self) -> Span {
        Span::new(
            self.leading_colon
                .as_ref()
                .map(|t| t.span().start)
                .unwrap_or(self.segments.span().start),
            self.segments.span().end,
        )
    }
}

impl HasSpan for TypePath {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug, PartialEq)]
pub struct TypePathSegment {
    pub ident: PathIdentSegment,
    pub args: Option<(Option<TokenSpan>, GenericArgs)>,
}

impl TypePathSegment {
    pub fn span(&self) -> Span {
        Span::new(
            self.ident.span().start,
            self.args
                .as_ref()
                .map(|(_, args)| args.span().end)
                .unwrap_or(self.ident.span().end),
        )
    }
}

impl HasSpan for TypePathSegment {
    fn span(&self) -> Span {
        self.span()
    }
}

// /// 路径，用于表示命名空间中的项
// #[derive(Debug, PartialEq)]
// pub struct Path {
//     pub leading_colon: Option<TokenSpan>,
//     pub segments: Punctuated<PathSegment>,
// }

// impl Path {
//     pub fn span(&self) -> Span {
//         let start = if let Some(leading_colon) = &self.leading_colon {
//             leading_colon.span().start
//         } else if let Some((first, _)) = self.segments.items.first() {
//             first.ident.span().start
//         } else {
//             return Span::default();
//         };

//         let end = if let Some((last, _)) = self.segments.items.last() {
//             last.ident.span().end
//         } else {
//             return Span::default();
//         };

//         Span::new(start, end)
//     }
// }

// impl HasSpan for Path {
//     fn span(&self) -> Span {
//         self.span()
//     }
// }

// /// 路径段
// #[derive(Debug, Clone, PartialEq)]
// pub struct PathSegment {
//     pub ident: IdentSpan,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public(TokenSpan),
    Private(TokenSpan),
}

impl Visibility {
    pub fn span(&self) -> Span {
        match self {
            Self::Public(span) => span.span(),
            Self::Private(span) => span.span(),
        }
    }
}

impl HasSpan for Visibility {
    fn span(&self) -> Span {
        self.span()
    }
}
