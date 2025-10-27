use liescript_ast::items::Item;
use liescript_ast::names::{PathIdentSegment, TypePath, TypePathSegment};
use liescript_ast::statements::*;
use liescript_ast::types::*;
use liescript_ast::{expressions::Expression, patterns::Pattern};
use liescript_lexical::token::{Bracket, TokenSpan};
use liescript_lexical::{
    Span, Spanned,
    keyword::Keyword,
    symbol::Symbol,
    token::{Brace, Paren, Punctuated, Token},
};

use crate::{Parse, ParseContext, ParseResult};

impl Parse for Type {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_type(cx)
    }
}

/// 解析类型
pub fn parse_type(cx: &mut ParseContext) -> ParseResult<Type> {
    if cx.is_eof() {
        return Err(cx.create_eof_error("type"));
    }

    // 避免同时进行可变和不可变借用
    let is_keyword = cx.next_is(|t: &Token| matches!(t, Token::Keyword(_)));
    let is_ident = cx.next_is(|t: &Token| matches!(t, Token::Ident(_)));
    let is_symbol = cx.next_is(|t: &Token| matches!(t, Token::Symbol(_)));

    if is_keyword {
        if let Some(token) = cx.peek() {
            if let Token::Keyword(keyword) = &token.value {
                // 解析原生类型
                match keyword {
                    Keyword::Any => parse_primitive_type(cx, Primitive::Any),
                    Keyword::Bool => parse_primitive_type(cx, Primitive::Boolean),
                    Keyword::Byte => parse_primitive_type(cx, Primitive::Byte),
                    Keyword::Int => parse_primitive_type(cx, Primitive::Integer),
                    Keyword::Float => parse_primitive_type(cx, Primitive::Float),
                    Keyword::Char => parse_primitive_type(cx, Primitive::Char),
                    Keyword::String => parse_primitive_type(cx, Primitive::String),
                    _ => parse_path_type(cx), // 其他关键字作为路径类型处理
                }
            } else {
                unreachable!()
            }
        } else {
            Err(cx.create_eof_error("type"))
        }
    } else if is_ident {
        parse_path_type(cx)
    } else if is_symbol {
        if let Some(token) = cx.peek() {
            if let Token::Symbol(symbol) = &token.value {
                match symbol {
                    Symbol::And => parse_reference_type(cx),
                    Symbol::LParen => parse_tuple_type(cx),
                    Symbol::LBracket => parse_array_or_slice_type(cx),
                    Symbol::Not => parse_never_type(cx),
                    Symbol::Underscore => parse_inferred_type(cx),
                    _ => {
                        let token_span = token.span;
                        let token_value = token.value.clone();
                        Err(cx.create_error(
                            "Unexpected symbol for type".to_string(),
                            token_span,
                            Some("type".to_string()),
                            Some(format!("{:?}", token_value)),
                        ))
                    }
                }
            } else {
                unreachable!()
            }
        } else {
            Err(cx.create_eof_error("type"))
        }
    } else {
        if let Some(token) = cx.peek() {
            let token_span = token.span;
            let token_value = token.value.clone();
            Err(cx.create_error(
                "Unexpected token for type".to_string(),
                token_span,
                Some("type".to_string()),
                Some(format!("{:?}", token_value)),
            ))
        } else {
            Err(cx.create_eof_error("type"))
        }
    }
}

/// 解析原生类型
fn parse_primitive_type(
    cx: &mut ParseContext,
    primitive: fn(TokenSpan) -> Primitive,
) -> ParseResult<Type> {
    let token = cx.consume()?;
    Ok(Type::Primitive(primitive(token)))
}

/// 解析路径类型
fn parse_path_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let leading_colon = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon))) {
        Some(cx.consume().unwrap())
    } else {
        None
    };

    let mut segments = Punctuated::new();

    // 解析第一个段
    let first_segment_token = cx.expect_identifier()?;
    let ident = first_segment_token.value.clone().into_ident();
    let segment = PathIdentSegment::Ident(ident.into());
    let type_segment = TypePathSegment {
        ident: segment,
        args: None,
    };
    segments.push_last(type_segment);

    // 解析后续段（如果有::分隔符）
    loop {
        let has_colon = cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::ColonColon)));
        if !has_colon {
            break;
        }

        let colon_token = cx.consume()?; // 消耗::
        let segment_token = cx.expect_identifier()?;
        let ident = segment_token.value.clone().into_ident();
        let segment = PathIdentSegment::Ident(ident.into());
        let type_segment = TypePathSegment {
            ident: segment,
            args: None,
        };
        segments.push(type_segment, colon_token);
    }

    Ok(Type::Path(TypePath {
        leading_colon,
        segments,
    }))
}

/// 解析引用类型
fn parse_reference_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let and_token = cx.expect_symbol(Symbol::And)?;
    let is_mut = if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::Mut))) {
        Some(cx.consume().unwrap())
    } else {
        None
    };
    let ty = parse_type(cx)?;

    Ok(Type::Reference(ReferenceType {
        and_token,
        is_mut,
        ty: Box::new(ty),
    }))
}

/// 解析元组类型
fn parse_tuple_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let lparen = cx.expect_symbol(Symbol::LParen)?;
    let mut elems = Vec::new();

    loop {
        let is_rparen = cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen)));
        if is_rparen || cx.is_eof() {
            break;
        }

        elems.push(parse_type(cx)?);

        let has_comma = cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma)));
        if has_comma {
            cx.consume();
        } else {
            break;
        }
    }

    let rparen = cx.expect_symbol(Symbol::RParen)?;

    let mut punctuated_elems = Punctuated::new();
    for elem in elems {
        punctuated_elems.push_last(elem);
    }

    Ok(Type::Tuple(TupleType {
        paren_token: Paren::new(lparen, rparen),
        elems: punctuated_elems,
    }))
}

/// 解析数组或切片类型
fn parse_array_or_slice_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let lbracket = cx.expect_symbol(Symbol::LBracket)?;
    let elem = parse_type(cx)?;

    if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) {
        // 数组类型 [T; N]
        let semi_token = cx.consume()?;
        let len = Expression::parse(cx)?;
        let rbracket = cx.expect_symbol(Symbol::RBracket)?;

        Ok(Type::Array(ArrayType {
            bracket_token: Bracket::new(lbracket, rbracket),
            elem: Box::new(elem),
            semi_token,
            len: Box::new(len),
        }))
    } else {
        // 切片类型 [T]
        let rbracket = cx.expect_symbol(Symbol::RBracket)?;

        Ok(Type::Slice(SliceType {
            bracket_token: Bracket::new(lbracket, rbracket),
            elem: Box::new(elem),
        }))
    }
}

/// 解析Never类型
fn parse_never_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let bang_token = cx.expect_symbol(Symbol::Not)?;
    Ok(Type::Never(NeverType { bang_token }))
}

/// 解析推断类型
fn parse_inferred_type(cx: &mut ParseContext) -> ParseResult<Type> {
    let underscore_token = cx.expect_symbol(Symbol::Underscore)?;
    Ok(Type::Inferred(InferredType { underscore_token }))
}
