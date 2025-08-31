use crate::lexical::{
    Brace, Bracket, Keyword, LiteralSpan, Paren, Punctuated, Symbol, Token, TokenSpan,
};
use crate::parser::parse::{Parse, ParseError, ParseStream};
use crate::syntax::types::*;

impl Parse for Type {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.peek() {
            Some(Token::Symbol(Symbol::Not)) => Ok(Type::Never(NeverType {
                bang_token: stream.consume()?,
            })),
            Some(Token::Symbol(Symbol::And)) => Ok(Type::Reference(ReferenceType {
                and_token: stream.consume()?,
                is_mut: stream
                    .peek()
                    .is_mut()
                    .then(|| stream.consume())
                    .transpose()?,
                ty: Box::new(stream.parse()?),
            })),
            Some(Token::Ident(ident)) if ident == "_" => Ok(Type::Inferred(InferredType {
                underscore_token: stream.consume()?,
            })),
            Some(Token::Keyword(Keyword::Any)) => Ok(Type::Any(AnyType {
                any_token: stream.consume()?,
            })),
            Some(Token::Keyword(Keyword::Fn)) => Ok(Type::BareFn(BareFunctionType {
                fn_token: stream.consume()?,
                paren_token: stream.consume()?,
                inputs: stream.parse_terminated(Parse::parse)?,
                output: stream
                    .peek()
                    .is_r_arrow()
                    .then(|| Ok((stream.consume()?, Box::new(stream.parse()?))))
                    .transpose()?,
            })),
            Some(Token::Symbol(Symbol::LBracket)) => {
                let bracket_token = stream.consume()?;
                let elem = Box::new(stream.parse()?);

                if stream.peek().is_semi() {
                    let semi_token = stream.consume()?;
                    let len = Box::new(stream.parse()?);
                    let _ = stream.consume()?; // RBracket
                    Ok(Type::Array(ArrayType {
                        bracket_token,
                        elem,
                        semi_token,
                        len,
                    }))
                } else {
                    let _ = stream.consume()?; // RBracket
                    Ok(Type::Slice(SliceType {
                        bracket_token,
                        elem,
                    }))
                }
            }
            Some(Token::Symbol(Symbol::LParen)) => {
                let paren_token = stream.consume()?;
                let lookahead = stream.lookahead();

                if lookahead.peek().is_comma() || lookahead.peek().is_r_paren() {
                    let elems = stream.parse_terminated(Parse::parse)?;
                    let _ = stream.consume()?; // RParen
                    Ok(Type::Tuple(TupleType { paren_token, elems }))
                } else {
                    let ty = Box::new(stream.parse()?);
                    let _ = stream.consume()?; // RParen
                    Ok(Type::Parenthesized(ParenthesizedType { paren_token, ty }))
                }
            }
            _ => Ok(Type::Path(TypePath {
                path: stream.parse()?,
            })),
        }
    }
}
