use std::borrow::Cow;

use crate::{
    ast::nodes::{ImportStmt, PathSegment, UseTree},
    token::{Ident, Keyword, Literal, Punctuation, Token, TreeType},
    tokenizer::StrippedTokenizer,
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
        let (i, _) = expect(i, |kw: &Keyword| kw == &Keyword::Use)?;
        let (i, tree) = UseTree::parse(i)?;

        let ret = ImportStmt { items: tree.flat() };

        Ok((i, ret))
    }
}

impl AstParser for UseTree {
    fn name(&self) -> &'static str {
        "ImportItem"
    }

    fn parse<'i, I: Iterator<Item = Token<'i>> + Clone>(i: I) -> IResult<I, Self> {
        let mut ret = UseTree {
            path: Vec::new(),
            alias: None,
            children: Vec::new(),
        };

        let (mut input, seg) = PathSegment::parse(i)?;
        ret.path.push(seg);

        loop {
            let i = input.clone();
            match input.next() {
                Some(Token::Punctuation(Punctuation::PathSep)) => {}
                Some(Token::Keyword(Keyword::As)) => {
                    let (i, alias) = expect(input, |p: &Ident| true)?;
                    ret.alias = Some(alias);

                    return Ok((i, ret));
                }
                Some(Token::Punctuation(Punctuation::Comma))
                | Some(Token::Punctuation(Punctuation::Semicolon)) => {
                    return Ok((i, ret));
                }
                Some(t) => {
                    println!("Unexpect: {:?}", &t);
                    return Err(ParseError::Unexpect);
                }
                None => {
                    return Ok((input, ret));
                }
            };

            let tmp = input.clone();
            if let Ok((ii, item)) = PathSegment::parse(tmp) {
                input = ii;
                ret.path.push(item);
            } else if let Some(Token::TokenTree(ty, tree)) = input.next() {
                if ty != TreeType::Block {
                    return Err(ParseError::Unexpect);
                }

                let iter = tree.into_iter();
                let mut ii = StrippedTokenizer::new(iter);

                loop {
                    let (mut i, item) = UseTree::parse(ii)?;
                    ret.children.push(item);
                    match i.next() {
                        Some(Token::Punctuation(Punctuation::Comma)) => {
                            ii = i;
                            continue;
                        }
                        Some(t) => {
                            println!("Unexpect => {:?}", t);
                            return Err(ParseError::Unexpect);
                        }
                        None => {
                            break;
                        }
                    }
                }
            }
        }
    }
}

fn expect<'i, I, T, F>(i: I, f: F) -> IResult<I, T>
where
    F: FnOnce(&T) -> bool,
    I: Iterator<Item = Token<'i>> + Clone,
    T: AstParser,
{
    T::parse(i).and_then(|(i, t)| {
        if f(&t) {
            Ok((i, t))
        } else {
            Err(ParseError::Unexpect)
        }
    })
}

#[cfg(test)]
mod test {
    use crate::{ast::nodes::ImportStmt, token::Ident, tokenizer::Tokenizer};

    use super::{AstParser, ParseError};

    #[test]
    fn ident() {
        let inputs = [
            ("a", Ok(Ident::new("a"))),
            ("use", Err(ParseError::Unexpect)),
        ];

        for input in inputs {
            let mut i = Tokenizer::new("", input.0).stripped();

            println!("=> {:?}", Ident::parse(i));
        }
    }

    #[test]
    fn import_item() {
        let inputs = [
            ("use std::net::TcpStream;", true),
            ("use std::net::TcpStream as TcpSocket;", true),
            (
                "use std::{net::TcpStream as TcpSocket, encoding::json};",
                true,
            ),
            ("use crate::parser;", true),
            ("use std::net:TcpStream;", false),
        ];

        for input in inputs {
            let mut i = Tokenizer::new("", input.0).stripped();

            println!("=> {:?}", ImportStmt::parse(i));
        }
    }
}
