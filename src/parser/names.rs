use super::parse::{Parse, ParseError, ParseStream};

use crate::lexical::{
    Angle, Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol,
    Token, TokenSpan,
};
use crate::syntax::{Type, names::*};

impl Parse for Visibility {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let peek = stream.peek();
        match peek.map(|v| v.as_ref()) {
            Some(Token::Keyword(Keyword::Pub)) => {
                let pub_token = stream.expect(&Token::Keyword(Keyword::Pub))?;
                Ok(Visibility::Public(pub_token))
            }
            Some(Token::Keyword(Keyword::Priv)) => {
                let priv_token = stream.expect(&Token::Keyword(Keyword::Priv))?;
                Ok(Visibility::Private(priv_token))
            }
            Some(t) => Err(ParseError::new("expected visibility".to_string())
                .with_expected(Keyword::Pub)
                .with_expected(Keyword::Priv)
                .with_found(t)
                .with_span(peek.map(|t| t.span()).unwrap_or_default())),
            None => Err(ParseError::eof()),
        }
    }
}

fn parse_simple_path(stream: &mut ParseStream) -> Result<SimplePath, ParseError> {
    let leading_colon = if stream.next_is(Symbol::ColonColon) {
        Some(stream.consume()?)
    } else {
        None
    };

    let segments = stream.parse_punctuated_with(
        parse_simple_path_segment,
        &Token::Symbol(Symbol::ColonColon),
    )?;

    Ok(SimplePath {
        leading_colon,
        segments,
    })
}

impl Parse for SimplePath {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_simple_path(stream)
    }
}

fn parse_simple_path_segment(stream: &mut ParseStream) -> Result<SimplePathSegment, ParseError> {
    let token = stream.consume()?;

    match token.as_ref() {
        Token::Ident(ident) => Ok(SimplePathSegment::Ident(
            token.clone().map(|_| ident.clone()),
        )),
        Token::Keyword(Keyword::Super) => Ok(SimplePathSegment::Super(token)),
        Token::Keyword(Keyword::SelfValue) => Ok(SimplePathSegment::Self_(token)),
        Token::Keyword(Keyword::Crate) => Ok(SimplePathSegment::Crate(token)),
        t => Err(ParseError::new("expected simple path segment")
            .with_expected(Keyword::Super)
            .with_expected(Keyword::SelfValue)
            .with_expected(Keyword::Crate)
            .with_found(t)
            .with_span(token.span())),
    }
}

fn parse_path_in_expression(stream: &mut ParseStream) -> Result<PathInExpression, ParseError> {
    let leading_colon = if stream.next_is(Symbol::ColonColon) {
        Some(stream.consume()?)
    } else {
        None
    };

    let segments = stream
        .parse_punctuated_with(parse_path_expr_segment, &Token::Symbol(Symbol::ColonColon))?;

    Ok(PathInExpression {
        leading_colon,
        segments,
    })
}

impl Parse for PathInExpression {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_path_in_expression(stream)
    }
}

fn parse_path_expr_segment(stream: &mut ParseStream) -> Result<PathExprSegment, ParseError> {
    let ident = parse_path_ident_segment(stream)?;

    let args = stream.try_parse(|s| {
        let colon_token = s.expect(&Token::Symbol(Symbol::ColonColon))?;
        let args = parse_generic_args(s)?;

        Ok((colon_token, args))
    });

    Ok(PathExprSegment { ident, args })
}

fn parse_path_ident_segment(stream: &mut ParseStream) -> Result<PathIdentSegment, ParseError> {
    let token = stream.consume()?;

    match token.as_ref() {
        Token::Ident(ident) => Ok(PathIdentSegment::Ident(
            token.clone().map(|_| ident.clone()),
        )),
        Token::Keyword(Keyword::Super) => Ok(PathIdentSegment::Super(token)),
        Token::Keyword(Keyword::SelfValue) => Ok(PathIdentSegment::SelfValue(token)),
        Token::Keyword(Keyword::SelfType) => Ok(PathIdentSegment::SelfType(token)),
        Token::Keyword(Keyword::Crate) => Ok(PathIdentSegment::Crate(token)),
        t => Err(ParseError::new("expected path ident segment")
            .with_expected(Keyword::Super)
            .with_expected(Keyword::SelfValue)
            .with_expected(Keyword::SelfType)
            .with_expected(Keyword::Crate)
            .with_found(t)
            .with_span(token.span())),
    }
}

fn parse_type_path(stream: &mut ParseStream) -> Result<TypePath, ParseError> {
    let leading_colon = if stream.next_is(Symbol::ColonColon) {
        Some(stream.consume()?)
    } else {
        None
    };

    let segments = stream
        .parse_punctuated_with(parse_type_path_segment, &Token::Symbol(Symbol::ColonColon))?;

    Ok(TypePath {
        leading_colon,
        segments,
    })
}

impl Parse for TypePath {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_type_path(stream)
    }
}

fn parse_type_path_segment(stream: &mut ParseStream) -> Result<TypePathSegment, ParseError> {
    let ident = parse_path_ident_segment(stream)?;

    let args = stream.try_parse(|s| {
        let colon_token = if s.next_is(Symbol::ColonColon) {
            Some(s.consume()?)
        } else {
            None
        };
        let args = parse_generic_args(s)?;

        Ok((colon_token, args))
    });

    Ok(TypePathSegment { ident, args })
}

fn parse_generic_args(stream: &mut ParseStream) -> Result<GenericArgs, ParseError> {
    let open = stream.expect(&Token::Symbol(Symbol::Lt))?;
    let args = stream.parse_punctuated_with(parse_generic_arg, &Token::Symbol(Symbol::Comma))?;
    let close = stream.expect(&Token::Symbol(Symbol::Gt))?;

    Ok(GenericArgs {
        angle_token: Angle::new(open, close),
        args,
    })
}

fn parse_generic_arg(stream: &mut ParseStream) -> Result<GenericArg, ParseError> {
    // FIXME: not only type arguments
    let ty = Type::parse(stream)?;

    Ok(GenericArg::Type(ty))
}
