use crate::lexical::{Symbol, Token};
use crate::parser::parse::{Parse, ParseError, ParseStream};
use crate::syntax::patterns::*;

impl Parse for Pattern {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.peek() {
            Some(Token::Literal(_)) => Ok(Pattern::Literal(LiteralPattern {
                lit: stream.consume()?,
            })),
            Some(Token::Ident(_)) => {
                let ident = stream.consume()?;

                if stream.peek().is_at() {
                    let at_token = stream.consume()?;
                    let subpat = Box::new(stream.parse()?);
                    Ok(Pattern::Identifier(IdentifierPattern {
                        ident,
                        by_ref: None,
                        is_mut: None,
                        subpat: Some((at_token, subpat)),
                    }))
                } else {
                    Ok(Pattern::Identifier(IdentifierPattern {
                        ident,
                        by_ref: None,
                        is_mut: None,
                        subpat: None,
                    }))
                }
            }
            Some(Token::Symbol(Symbol::Underscore)) => Ok(Pattern::Wildcard(WildcardPattern {
                underscore_token: stream.consume()?,
            })),
            Some(Token::Symbol(Symbol::Dot2)) => Ok(Pattern::Rest(RestPattern {
                dot2_token: stream.consume()?,
            })),
            Some(Token::Symbol(Symbol::And)) => {
                let and_token = stream.consume()?;
                let is_mut = stream
                    .peek()
                    .is_mut()
                    .then(|| stream.consume())
                    .transpose()?;
                let pat = Box::new(stream.parse()?);
                Ok(Pattern::Reference(ReferencePattern {
                    and_token,
                    is_mut,
                    pat,
                }))
            }
            Some(Token::Symbol(Symbol::LBrace)) => {
                let path = stream.parse()?;
                let brace_token = stream.consume()?;
                let fields = stream.parse_terminated(Parse::parse)?;
                let rest = if stream.peek().is_dot2() {
                    Some(stream.parse()?)
                } else {
                    None
                };
                let _ = stream.consume()?; // RBrace
                Ok(Pattern::Struct(StructPattern {
                    path,
                    brace_token,
                    fields,
                    rest,
                }))
            }
            Some(Token::Symbol(Symbol::LParen)) => {
                let path = if stream.lookahead().peek().is_ident() {
                    Some(stream.parse()?)
                } else {
                    None
                };

                let paren_token = stream.consume()?;
                let elems = stream.parse_terminated(Parse::parse)?;
                let _ = stream.consume()?; // RParen

                if let Some(path) = path {
                    Ok(Pattern::TupleStruct(TupleStructPattern {
                        path,
                        paren_token,
                        elems,
                    }))
                } else {
                    Ok(Pattern::Tuple(TuplePattern { paren_token, elems }))
                }
            }
            Some(Token::Symbol(Symbol::LBracket)) => {
                let bracket_token = stream.consume()?;
                let elems = stream.parse_terminated(Parse::parse)?;
                let _ = stream.consume()?; // RBracket
                Ok(Pattern::Slice(SlicePattern {
                    bracket_token,
                    elems,
                }))
            }
            Some(Token::Symbol(Symbol::LParen)) => {
                let paren_token = stream.consume()?;
                let pat = Box::new(stream.parse()?);
                let _ = stream.consume()?; // RParen
                Ok(Pattern::Grouped(GroupedPattern { paren_token, pat }))
            }
            Some(Token::Symbol(Symbol::Dot2)) => {
                let lo = Box::new(stream.parse()?);
                let limits = stream.parse()?;
                let hi = Box::new(stream.parse()?);
                Ok(Pattern::Range(RangePattern { lo, limits, hi }))
            }
            _ => Ok(Pattern::Path(PathPattern {
                path: stream.parse()?,
            })),
        }
    }
}

impl Parse for FieldPattern {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let member = stream.parse()?;
        let colon_token = if stream.peek().is_colon() {
            Some(stream.consume()?)
        } else {
            None
        };
        let pat = Box::new(stream.parse()?);
        Ok(FieldPattern {
            member,
            colon_token,
            pat,
        })
    }
}
