use liescript_ast::expressions::Expression;
use liescript_ast::patterns::*;
use liescript_lexical::{
    keyword::Keyword, symbol::Symbol, token::{Brace, Paren, Punctuated, Token}, Span, Spanned
};

use crate::names::parse_path_in_expression;

use super::{
    Parse,
    context::ParseContext,
    diagnostic::{ParseError, ParseResult},
};

impl Parse for Pattern {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_pattern(cx)
    }
}

/// 解析模式
pub fn parse_pattern(cx: &mut ParseContext) -> ParseResult<Pattern> {
    if cx.is_eof() {
        return Err(cx.create_eof_error("pattern"));
    }

    // 先检查各种模式类型的起始标记，避免多次借用cx
    if cx.next_is(|t: &Token| matches!(t, Token::Ident(_))) {
        // 检查是否是结构体模式或元组结构体模式
        if cx
            .lookahead_token(1)
            .map_or(false, |t| matches!(t, Token::Symbol(Symbol::LBrace)))
        {
            parse_struct_pattern(cx).map(Pattern::Struct)
        } else if cx
            .lookahead_token(1)
            .map_or(false, |t| matches!(t, Token::Symbol(Symbol::LParen)))
        {
            parse_tuple_struct_pattern(cx).map(Pattern::TupleStruct)
        } else {
            parse_identifier_pattern(cx).map(Pattern::Identifier)
        }
    } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Underscore))) {
        parse_wildcard_pattern(cx).map(Pattern::Wildcard)
    } else if cx.next_is(|t: &Token| matches!(t, Token::Literal(_))) {
        parse_literal_pattern(cx).map(Pattern::Literal)
    } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::And))) {
        parse_reference_pattern(cx).map(Pattern::Reference)
    } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LParen))) {
        parse_tuple_pattern(cx).map(Pattern::Tuple)
    } else {
        // 尝试解析范围模式
        cx.push_recovery_point(super::context::RecoveryType::Expression);

        if let Ok(_expr) = Expression::parse(cx) {
            if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::DotDot | Symbol::DotDotEq)))
            {
                // 回退并解析范围模式
                cx.recover_to_point();
                parse_range_pattern(cx).map(Pattern::Range)
            } else {
                // 回退并报错
                cx.recover_to_point();
                if let Some(token) = cx.peek() {
                    let token_span = token.span;
                    let token_value = token.value.clone();
                    return Err(cx.create_error(
                        "Unsupported pattern type".to_string(),
                        token_span,
                        Some(
                            "identifier, _, literal, &, path{...}, path(...), (...), or range"
                                .to_string(),
                        ),
                        Some(format!("{:?}", token_value)),
                    ));
                } else {
                    return Err(cx.create_eof_error("pattern"));
                }
            }
        } else {
            // 回退并报错
            cx.recover_to_point();
            if let Some(token) = cx.peek() {
                let token_span = token.span;
                let token_value = token.value.clone();
                return Err(cx.create_error(
                    "Unsupported pattern type".to_string(),
                    token_span,
                    Some(
                        "identifier, _, literal, &, path{...}, path(...), (...), or range"
                            .to_string(),
                    ),
                    Some(format!("{:?}", token_value)),
                ));
            } else {
                return Err(cx.create_eof_error("pattern"));
            }
        }
    }
}

/// 解析标识符模式
fn parse_identifier_pattern(cx: &mut ParseContext) -> ParseResult<IdentifierPattern> {
    let identifier_token = cx.expect_identifier()?;
    let ident = identifier_token.value.clone().into_ident();
    Ok(IdentifierPattern {
        ident: Spanned::new(ident, identifier_token.span),
        by_ref: None,
        is_mut: None,
        subpat: None,
    })
}

/// 解析通配符模式
fn parse_wildcard_pattern(cx: &mut ParseContext) -> ParseResult<WildcardPattern> {
    let underscore_token = cx.expect_symbol(Symbol::Underscore)?;
    Ok(WildcardPattern::new(underscore_token))
}

/// 解析字面量模式
fn parse_literal_pattern(cx: &mut ParseContext) -> ParseResult<LiteralPattern> {
    if let Some(token) = cx.peek() {
        let token_value = token.value.clone();
        let token_span = token.span;

        if let Token::Literal(lit) = &token_value {
            cx.consume();
            Ok(LiteralPattern {
                lit: lit.clone().into(),
            })
        } else {
            Err(cx.create_error(
                "Expected literal for pattern".to_string(),
                token_span,
                Some("literal value".to_string()),
                Some(format!("{:?}", token_value)),
            ))
        }
    } else {
        Err(cx.create_eof_error("literal pattern"))
    }
}

/// 解析范围模式
fn parse_range_pattern(cx: &mut ParseContext) -> ParseResult<RangePattern> {
    let lo = Expression::parse(cx)?;

    let current_span = cx.peek().map(|t| t.span).unwrap_or_else(|| Span::dummy());

    let limits = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::DotDot))) {
        let token = cx.consume()?;
        Spanned::new(RangeLimits::HalfOpen, token.span)
    } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::DotDotEq))) {
        let token = cx.consume()?;
        Spanned::new(RangeLimits::Closed, token.span)
    } else {
        return Err(cx.create_error(
            "Expected range operator".to_string(),
            current_span,
            Some(".. or ..=".to_string()),
            None,
        ));
    };

    let hi = Expression::parse(cx)?;

    Ok(RangePattern {
        lo: Box::new(lo),
        limits,
        hi: Box::new(hi),
    })
}

/// 解析引用模式
fn parse_reference_pattern(cx: &mut ParseContext) -> ParseResult<ReferencePattern> {
    let and_token = cx.expect_symbol(Symbol::And)?;

    let is_mut = if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::Mut))) {
        Some(cx.consume()?)
    } else {
        None
    };

    let pat = Pattern::parse(cx)?;

    Ok(ReferencePattern {
        and_token,
        is_mut,
        pat: Box::new(pat),
    })
}

/// 解析结构体模式
fn parse_struct_pattern(cx: &mut ParseContext) -> ParseResult<StructPattern> {
    let path = parse_path_in_expression(cx)?;
    let lbrace = cx.expect_symbol(Symbol::LBrace)?;

    let mut fields = Vec::new();
    let mut rest = None;

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::DotDot))) {
            let dot2_token = cx.consume()?;
            rest = Some(RestPattern { dot2_token });
            break;
        }

        let member = cx.expect_identifier()?;

        let colon_token = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Colon))) {
            Some(cx.consume()?)
        } else {
            None
        };

        let member_ident = member.value.clone().into_ident();

        let pat = if colon_token.is_some() {
            Pattern::parse(cx)?
        } else {
            Pattern::Identifier(IdentifierPattern {
                ident: Spanned::new(member_ident.clone(), member.span),
                by_ref: None,
                is_mut: None,
                subpat: None,
            })
        };

        fields.push(FieldPattern {
            member: Spanned::new(member_ident, member.span),
            colon_token,
            pat: Box::new(pat),
        });

        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
            cx.consume();
        } else {
            break;
        }
    }

    let rbrace = cx.expect_symbol(Symbol::RBrace)?;

    Ok(StructPattern {
        path,
        brace_token: Brace::new(lbrace, rbrace),
        fields: {
            let mut punctuated = Punctuated::new();
            for field in fields {
                punctuated.push_last(field);
            }
            punctuated
        },
        rest,
    })
}

/// 解析元组结构体模式
fn parse_tuple_struct_pattern(cx: &mut ParseContext) -> ParseResult<TupleStructPattern> {
    let path = parse_path_in_expression(cx)?;
    let lparen = cx.expect_symbol(Symbol::LParen)?;

    let mut elems = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen))) && !cx.is_eof() {
        elems.push(Pattern::parse(cx)?);

        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
            cx.consume();
        } else {
            break;
        }
    }

    let rparen = cx.expect_symbol(Symbol::RParen)?;

    Ok(TupleStructPattern {
        path,
        paren_token: Paren::new(lparen, rparen),
        elems: {
            let mut punctuated = Punctuated::new();
            for elem in elems {
                punctuated.push_last(elem);
            }
            punctuated
        },
    })
}

/// 解析元组模式
fn parse_tuple_pattern(cx: &mut ParseContext) -> ParseResult<TuplePattern> {
    let lparen = cx.expect_symbol(Symbol::LParen)?;

    let mut elems = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen))) && !cx.is_eof() {
        elems.push(Pattern::parse(cx)?);

        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
            cx.consume();
        } else {
            break;
        }
    }

    let rparen = cx.expect_symbol(Symbol::RParen)?;

    Ok(TuplePattern {
        paren_token: Paren::new(lparen, rparen),
        elems: {
            let mut punctuated = Punctuated::new();
            for elem in elems {
                punctuated.push_last(elem);
            }
            punctuated
        },
    })
}

/// 解析剩余模式
fn parse_rest_pattern(cx: &mut ParseContext) -> ParseResult<RestPattern> {
    let dot2_token = cx.expect_symbol(Symbol::DotDot)?;
    Ok(RestPattern { dot2_token })
}
