use std::borrow::Cow;

use crate::{
    ast::nodes::{ImportStmt, PathSegment},
    token::{Ident, Keyword, Literal, Punctuation, Token},
};

#[derive(Debug)]
pub enum ParseError {
    Incomplete,
    Unexpect,
    Failure(ParseFailure),
}

#[derive(Debug)]
pub struct ParseFailure {
    detail: Cow<'static, str>,
}

type IResult<I, O> = Result<(I, O), ParseError>;

trait AstParser: Sized {
    fn name(&self) -> &'static str;
    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self>;
}

impl AstParser for Ident {
    fn name(&self) -> &'static str {
        "Ident"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self> {
        let mut input = i.clone();
        match input.next() {
            Some(Token::Ident(ident)) => Ok((input, ident)),
            Some(_) => Err(ParseError::Unexpect),
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Keyword {
    fn name(&self) -> &'static str {
        "Keyword"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self> {
        let mut input = i.clone();
        match input.next() {
            Some(Token::Keyword(kw)) => Ok((input, kw)),
            Some(_) => Err(ParseError::Unexpect),
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Literal {
    fn name(&self) -> &'static str {
        "Literal"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(mut i: I) -> IResult<I, Self> {
        let input = i.clone();
        match i.next() {
            Some(Token::Literal(lit)) => Ok((input, lit)),
            Some(_) => Err(ParseError::Unexpect),
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Punctuation {
    fn name(&self) -> &'static str {
        "Punctuation"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(mut i: I) -> IResult<I, Self> {
        let input = i.clone();
        match i.next() {
            Some(Token::Punctuation(p)) => Ok((input, p)),
            Some(_) => Err(ParseError::Unexpect),
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for PathSegment {
    fn name(&self) -> &'static str {
        "PathSegment"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self> {
        let mut input = i.clone();
        match input.next() {
            Some(Token::Ident(ident)) => Ok((input, PathSegment::Ident(ident))),
            Some(Token::Keyword(Keyword::Super)) => Ok((input, PathSegment::PathSuper)),
             Some(Token::Keyword(Keyword::SelfValue)) => Ok((input, PathSegment::PathSelf)),
             Some(Token::Keyword(Keyword::Crate)) => Ok((input, PathSegment::PathCrate)),
             Some(_) => Err(ParseError::Unexpect),
             None => Err(ParseError::Incomplete),
        }
    }
}


impl AstParser for ImportStmt {
    fn name(&self) -> &'static str {
        "ImportStmt"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self> {
        let input = i.clone();
        let (i, _) = Keyword::parse(input).and_then(|(i, kw)| {
            if kw != Keyword::Use {
                Err(ParseError::Unexpect)
            } else {
                Ok((i, kw))
            }
        })?;

        unimplemented!();

    }
}

#[cfg(test)]
mod test {
    use crate::{token::Ident, tokenizer::Tokenizer};

    use super::{AstParser, ParseError};

    #[test]
    fn ident() {
        let inputs = [
            ("a", Ok(Ident::new("a"))),
            ("use", Err(ParseError::Unexpect)),
        ];

        for input in inputs {
            let mut i = Tokenizer::new("", input.0);

            println!("=> {:?}", Ident::parse(i));
        }
    }
}
