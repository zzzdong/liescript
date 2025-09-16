use super::parse::{Parse, ParseError, ParseStream};
use crate::diagnostic::Span;
use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::{
    expressions::{Expression, LiteralExpression, PathExpression},
    names::{Path, PathSegment},
    patterns::*,
};

// Pattern 的 Parse 实现
impl Parse for Pattern {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // 1. 尝试解析引用模式 &x 或 &mut x
        if let Some(pat) = stream.try_parse(parse_reference_pattern) {
            return Ok(Pattern::Reference(pat));
        }

        // 2. 尝试解析字面量模式 (需要先于范围模式检查)
        if let Some(pat) = stream.try_parse(parse_literal_pattern) {
            // 检查是否是范围模式的一部分 (如 1..5)
            if stream.next_is(Symbol::DotDot) || stream.next_is(Symbol::DotDotEq) {
                let lo = Box::new(Expression::Literal(LiteralExpression { lit: pat.lit }));
                return parse_range_pattern_rest(stream, lo);
            }
            return Ok(Pattern::Literal(pat));
        }

        // 3. 尝试解析标识符模式 (可能是范围模式的一部分)
        if let Some(pat) = stream.try_parse(parse_identifier_pattern) {
            // 检查是否是范围模式的一部分 (如 x..y)
            if stream.next_is(Symbol::DotDot) || stream.next_is(Symbol::DotDotEq) {
                let path = Path {
                    leading_colon: None,
                    segments: Punctuated {
                        items: vec![(
                            PathSegment { ident: pat.ident },
                            TokenSpan {
                                value: Token::Symbol(Symbol::Colon),
                                span: Span::default(),
                            },
                        )],
                        last: None,
                    },
                };
                let lo = Box::new(Expression::Path(PathExpression { path }));
                return parse_range_pattern_rest(stream, lo);
            }
            return Ok(Pattern::Identifier(pat));
        }

        // 4. 尝试解析通配符模式 _
        if let Some(pat) = stream.try_parse(parse_wildcard_pattern) {
            return Ok(Pattern::Wildcard(pat));
        }

        // 5. 尝试解析剩余模式 ..
        if let Some(pat) = stream.try_parse(parse_rest_pattern) {
            return Ok(Pattern::Rest(pat));
        }

        // 6. 尝试解析结构体模式 Point { x, y }
        if let Some(pat) = stream.try_parse(parse_struct_pattern) {
            return Ok(Pattern::Struct(pat));
        }

        // 7. 尝试解析元组结构体模式 Some(x)
        if let Some(pat) = stream.try_parse(parse_tuple_struct_pattern) {
            return Ok(Pattern::TupleStruct(pat));
        }

        // 8. 尝试解析元组模式 (x, y)
        if let Some(pat) = stream.try_parse(parse_tuple_pattern) {
            return Ok(Pattern::Tuple(pat));
        }

        // 9. 尝试解析分组模式 (x | y)
        if let Some(pat) = stream.try_parse(parse_grouped_pattern) {
            return Ok(Pattern::Grouped(pat));
        }

        // 10. 尝试解析切片模式 [x, y]
        if let Some(pat) = stream.try_parse(parse_slice_pattern) {
            return Ok(Pattern::Slice(pat));
        }

        // 11. 尝试解析路径模式 None
        if let Some(pat) = stream.try_parse(parse_path_pattern) {
            return Ok(Pattern::Path(pat));
        }

        Err(ParseError::new("expected a pattern"))
    }
}

/// 解析范围模式的剩余部分 (.. 或 ..=)
fn parse_range_pattern_rest(
    stream: &mut ParseStream,
    lo: Box<Expression>,
) -> Result<Pattern, ParseError> {
    let limits = if stream.next_is(Symbol::DotDot) {
        stream.consume()?.map(|_| RangeLimits::HalfOpen)
    } else {
        stream.consume()?.map(|_| RangeLimits::Closed)
    };

    // 解析右边界
    let hi = if stream.next_is(Symbol::DotDot) || stream.next_is(Symbol::DotDotEq) {
        // 开区间如 1.. 或 1..=
        Box::new(Expression::Literal(LiteralExpression {
            lit: LiteralSpan::parse(stream)?,
        }))
    } else {
        Box::new(Expression::parse(stream)?)
    };

    Ok(Pattern::Range(RangePattern { lo, limits, hi }))
}

/// 解析字面量模式
fn parse_literal_pattern(stream: &mut ParseStream) -> Result<LiteralPattern, ParseError> {
    let lit = LiteralSpan::parse(stream)?;
    Ok(LiteralPattern { lit })
}

/// 解析标识符模式
fn parse_identifier_pattern(stream: &mut ParseStream) -> Result<IdentifierPattern, ParseError> {
    let mut by_ref = None;
    let mut is_mut = None;

    // 检查是否有 ref 或 mut 关键字
    if stream.next_is(&Token::Keyword(Keyword::Ref)) {
        by_ref = Some(stream.consume()?);
    }
    if stream.next_is(&Token::Keyword(Keyword::Mut)) {
        is_mut = Some(stream.consume()?);
    }

    let ident = IdentSpan::parse(stream)?;

    let subpat = if stream.next_is(Symbol::At) {
        let at_token = stream.expect_symbol(Symbol::At)?;
        let pattern = Box::new(Pattern::parse(stream)?);
        Some((at_token, pattern))
    } else {
        None
    };

    Ok(IdentifierPattern {
        ident,
        by_ref,
        is_mut,
        subpat,
    })
}

/// 解析通配符模式
fn parse_wildcard_pattern(stream: &mut ParseStream) -> Result<WildcardPattern, ParseError> {
    let underscore_token = stream.expect_symbol(Symbol::Underscore)?;
    Ok(WildcardPattern { underscore_token })
}

/// 解析剩余模式
fn parse_rest_pattern(stream: &mut ParseStream) -> Result<RestPattern, ParseError> {
    let dot2_token = stream.expect_symbol(Symbol::DotDot)?;
    Ok(RestPattern { dot2_token })
}

/// 解析引用模式
fn parse_reference_pattern(stream: &mut ParseStream) -> Result<ReferencePattern, ParseError> {
    let and_token = stream.expect_symbol(Symbol::And)?;
    let is_mut = if stream.next_is(&Token::Keyword(Keyword::Mut)) {
        Some(stream.consume()?)
    } else {
        None
    };
    let pat = Box::new(Pattern::parse(stream)?);

    Ok(ReferencePattern {
        and_token,
        is_mut,
        pat,
    })
}

/// 解析结构体模式
fn parse_struct_pattern(stream: &mut ParseStream) -> Result<StructPattern, ParseError> {
    let path = Path::parse(stream)?;
    let brace_token = stream.expect_symbol(Symbol::LBrace)?;

    let mut fields = Vec::new();
    let mut rest = None;

    while !stream.is_empty() && !stream.next_is(Symbol::RBrace) {
        if stream.next_is(Symbol::DotDot) {
            rest = Some(parse_rest_pattern(stream)?);
            break;
        }

        let field = parse_field_pattern(stream)?;
        fields.push(field);

        if stream.next_is(Symbol::Comma) {
            stream.consume()?;
        } else {
            break;
        }
    }

    let close_brace = stream.expect_symbol(Symbol::RBrace)?;

    Ok(StructPattern {
        path,
        brace_token: Brace {
            open: brace_token,
            close: close_brace,
        },
        fields: Punctuated {
            items: fields
                .into_iter()
                .map(|f| {
                    (
                        f,
                        TokenSpan {
                            value: Token::Symbol(Symbol::Comma),
                            span: Span::default(),
                        },
                    )
                })
                .collect(),
            last: None,
        },
        rest,
    })
}

/// 解析字段模式
fn parse_field_pattern(stream: &mut ParseStream) -> Result<FieldPattern, ParseError> {
    let member = IdentSpan::parse(stream)?;
    let colon_token = if stream.next_is(Symbol::Colon) {
        Some(stream.consume()?)
    } else {
        None
    };
    let pat = Box::new(Pattern::parse(stream)?);

    Ok(FieldPattern {
        member,
        colon_token,
        pat,
    })
}

/// 解析元组结构体模式
fn parse_tuple_struct_pattern(stream: &mut ParseStream) -> Result<TupleStructPattern, ParseError> {
    let path = Path::parse(stream)?;
    let open_paren = stream.expect_symbol(Symbol::LParen)?;

    let comma_token = Token::Symbol(Symbol::Comma);
    let elems = stream.parse_punctuated_with(Pattern::parse, &comma_token)?;

    let close_paren = stream.expect_symbol(Symbol::RParen)?;

    Ok(TupleStructPattern {
        path,
        paren_token: Paren {
            open: open_paren,
            close: close_paren,
        },
        elems,
    })
}

/// 解析元组模式
fn parse_tuple_pattern(stream: &mut ParseStream) -> Result<TuplePattern, ParseError> {
    let open_paren = stream.expect_symbol(Symbol::LParen)?;

    let comma_token = Token::Symbol(Symbol::Comma);
    let elems = stream.parse_punctuated_with(Pattern::parse, &comma_token)?;

    let close_paren = stream.expect_symbol(Symbol::RParen)?;

    Ok(TuplePattern {
        paren_token: Paren {
            open: open_paren,
            close: close_paren,
        },
        elems,
    })
}

/// 解析分组模式
fn parse_grouped_pattern(stream: &mut ParseStream) -> Result<GroupedPattern, ParseError> {
    let open_paren = stream.expect_symbol(Symbol::LParen)?;
    let pat = Box::new(Pattern::parse(stream)?);
    let close_paren = stream.expect_symbol(Symbol::RParen)?;

    Ok(GroupedPattern {
        paren_token: Paren {
            open: open_paren,
            close: close_paren,
        },
        pat,
    })
}

/// 解析切片模式
fn parse_slice_pattern(stream: &mut ParseStream) -> Result<SlicePattern, ParseError> {
    let open_bracket = stream.expect_symbol(Symbol::LBracket)?;

    let comma_token = Token::Symbol(Symbol::Comma);
    let elems = stream.parse_punctuated_with(Pattern::parse, &comma_token)?;

    let close_bracket = stream.expect_symbol(Symbol::RBracket)?;

    Ok(SlicePattern {
        bracket_token: Bracket {
            open: open_bracket,
            close: close_bracket,
        },
        elems,
    })
}

/// 解析路径模式
fn parse_path_pattern(stream: &mut ParseStream) -> Result<PathPattern, ParseError> {
    let path = Path::parse(stream)?;
    Ok(PathPattern { path })
}
