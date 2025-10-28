//! 名称解析模块

use crate::{
    lexical::{Symbol, Token, TokenSpan},
    syntax::names::{PathInExpression, SimplePath},
};

use super::{
    context::ParseContext,
    diagnostic::{ParseError, ParseResult},
    Parse,
};

impl Parse for SimplePath {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_simple_path(cx)
    }
}

impl Parse for PathInExpression {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_path_in_expression(cx)
    }
}

/// 解析简单路径
pub fn parse_simple_path(cx: &mut ParseContext) -> ParseResult<SimplePath> {
    let leading_colon = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon))) {
        Some(cx.consume()?)
    } else {
        None
    };
    
    let mut segments = crate::lexical::Punctuated::new();
    
    // 解析第一个段
    let first_segment_token = cx.expect_identifier()?;
    let ident = first_segment_token.value.clone().into_ident();
    let segment = crate::syntax::names::SimplePathSegment::Ident(ident.into());
    segments.push_last(segment);
    
    // 解析后续段（如果有::分隔符）
    while cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon))) {
        let colon_token = cx.consume()?; // 消耗::
        let segment_token = cx.expect_identifier()?;
        let ident = segment_token.value.clone().into_ident();
        let segment = crate::syntax::names::SimplePathSegment::Ident(ident.into());
        segments.push(segment, colon_token);
    }
    
    Ok(SimplePath { leading_colon, segments })
}

/// 解析表达式中的路径
pub fn parse_path_in_expression(cx: &mut ParseContext) -> ParseResult<PathInExpression> {
    let leading_colon = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon))) {
        Some(cx.consume()?)
    } else {
        None
    };
    
    let mut segments = crate::lexical::Punctuated::new();
    
    // 解析第一个段
    let first_segment_token = cx.expect_identifier()?;
    let ident = first_segment_token.value.clone().into_ident();
    let segment = crate::syntax::names::PathExprSegment {
        ident: crate::syntax::names::PathIdentSegment::Ident(ident.into()),
        args: None,
    };
    segments.push_last(segment);
    
    // 解析后续段（如果有::分隔符）
    while cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon))) {
        let colon_token = cx.consume()?; // 消耗::
        let segment_token = cx.expect_identifier()?;
        let ident = segment_token.value.clone().into_ident();
        let segment = crate::syntax::names::PathExprSegment {
            ident: crate::syntax::names::PathIdentSegment::Ident(ident.into()),
            args: None,
        };
        segments.push(segment, colon_token);
    }
    
    Ok(PathInExpression { leading_colon, segments })
}