use super::parse::{Parse, ParseError, ParseStream};

use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::names::*;

impl Parse for Visibility {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.peek().map(|v| v.as_ref()) {
            Some(Token::Keyword(Keyword::Pub)) => {
                let pub_token = stream.expect(&Token::Keyword(Keyword::Pub))?;
                Ok(Visibility::Public(Some(pub_token)))
            }
            Some(Token::Keyword(Keyword::Priv)) => {
                let priv_token = stream.expect(&Token::Keyword(Keyword::Priv))?;
                Ok(Visibility::Private(Some(priv_token)))
            }
            _ => Ok(Visibility::Inherited),
        }
    }
}

// 辅助类型的 Parse 实现
impl Parse for Path {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let leading_colon = if stream.next_is(Symbol::ColonColon) {
            Some(stream.consume()?)
        } else {
            None
        };

        let segments = stream.parse_punctuated(&Token::Symbol(Symbol::ColonColon))?;
        Ok(Path {
            leading_colon,
            segments,
        })
    }
}

impl Parse for PathSegment {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let ident = IdentSpan::parse(stream)?;
        Ok(PathSegment { ident })
    }
}


