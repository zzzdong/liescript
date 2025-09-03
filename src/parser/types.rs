use crate::lexical::{
    Brace, Bracket, Keyword, LiteralSpan, Paren, Punctuated, Symbol, Token, TokenSpan,
};
use crate::parser::parse::{Parse, ParseError, ParseStream};
use crate::syntax::Expression;
use crate::syntax::types::*;

impl Parse for Type {
    fn parse(stream: &mut ParseStream) -> Result<Type, ParseError> {
        let peek = stream.peek().ok_or(ParseError::eof())?;
        match peek.value() {
            Token::Symbol(Symbol::Underscore) => {
                let underscore_token = stream.consume()?;
                Ok(Type::Inferred(InferredType { underscore_token }))
            }
            Token::Symbol(Symbol::Not) => Type::parse_never(stream),
            Token::Symbol(Symbol::LParen) => Type::parse_tuple(stream),
            Token::Symbol(Symbol::LBracket) => match stream.try_parse(Type::parse_array) {
                Some(arr) => Ok(arr),
                None => Type::parse_slice(stream),
            },
            Token::Symbol(Symbol::And) => Type::parse_reference(stream),
            Token::Keyword(Keyword::Fn) => Type::parse_fn(stream),
            _ => Type::parse_primitive(stream),
        }
    }
}

impl Type {
    fn parse_never(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let bang_token = stream.expect(&Token::Symbol(Symbol::Not))?;
        Ok(Type::Never(NeverType { bang_token }))
    }

    fn parse_primitive(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;

        match token.value() {
            Token::Keyword(Keyword::Any) => Ok(Type::Primitive(Primitive::Any(token))),
            Token::Keyword(Keyword::Bool) => Ok(Type::Primitive(Primitive::Boolean(token))),
            Token::Keyword(Keyword::Byte) => Ok(Type::Primitive(Primitive::Byte(token))),
            Token::Keyword(Keyword::Int) => Ok(Type::Primitive(Primitive::Integer(token))),
            Token::Keyword(Keyword::Float) => Ok(Type::Primitive(Primitive::Float(token))),
            Token::Keyword(Keyword::Char) => Ok(Type::Primitive(Primitive::Char(token))),
            Token::Keyword(Keyword::String) => Ok(Type::Primitive(Primitive::String(token))),
            _ => Err(ParseError::new("Expected primitive type")
                .with_span(token.span())
                .with_expected("primitive type keyword")),
        }
    }

    fn parse_tuple(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LParen))?;
        let mut elems = Punctuated::new();

        while !stream.next_is(&Token::Symbol(Symbol::RParen)) {
            let item = Type::parse(stream)?;
            elems.push(item, stream.consume()?); // Push item and following punctuation

            if !stream.next_is(&Token::Symbol(Symbol::Comma)) {
                break;
            }
            stream.consume()?; // consume comma
        }

        let close = stream.expect(&Token::Symbol(Symbol::RParen))?;
        Ok(Type::Tuple(TupleType {
            paren_token: Paren::new(open, close),
            elems,
        }))
    }

    fn parse_slice(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LBracket))?;
        let elem = Type::parse(stream)?;
        let close = stream.expect(&Token::Symbol(Symbol::RBracket))?;
        Ok(Type::Slice(SliceType {
            bracket_token: Bracket::new(open, close),
            elem: Box::new(elem),
        }))
    }

    fn parse_array(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LBracket))?;
        let elem = Type::parse(stream)?;
        let semi = stream.expect(&Token::Symbol(Symbol::Semi))?;
        let len = Expression::parse(stream)?;
        let close = stream.expect(&Token::Symbol(Symbol::RBracket))?;
        Ok(Type::Array(ArrayType {
            bracket_token: Bracket::new(open, close),
            elem: Box::new(elem),
            semi_token: semi,
            len: Box::new(len),
        }))
    }

    fn parse_reference(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let and_token = stream.expect(&Token::Symbol(Symbol::And))?;
        let is_mut = stream
            .try_parse(|s| s.expect(&Token::Keyword(Keyword::Mut)))
            .is_some();
        let ty = Type::parse(stream)?;
        Ok(Type::Reference(ReferenceType {
            and_token,
            is_mut: if is_mut {
                Some(stream.consume()?)
            } else {
                None
            },
            ty: Box::new(ty),
        }))
    }

    fn parse_fn(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
        let open = stream.expect(&Token::Symbol(Symbol::LParen))?;

        let inputs = stream.parse_punctuated::<Type>(&Token::Symbol(Symbol::Comma))?;

        let close = stream.expect(&Token::Symbol(Symbol::RParen))?;

        let output = if stream.next_is(&Token::Symbol(Symbol::RArrow)) {
            let rarrow = stream.consume()?;
            Some((rarrow, Box::new(Type::parse(stream)?)))
        } else {
            None
        };

        Ok(Type::BareFn(BareFunctionType {
            fn_token,
            paren_token: Paren::new(open, close),
            inputs,
            output,
        }))
    }
}
