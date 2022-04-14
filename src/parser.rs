use std::borrow::Cow;
use std::fmt::Debug;

use crate::{
    ast::{
        nodes::{ImportStmt, PathSegment, UseTree},
        Ident, Keyword, Literal, Symbol,
    },
    tokenizer::{Token, TokenError, TokenStream},
};

#[derive(Debug)]
pub enum ParseError {
    Incomplete,
    Unexpect(Token),
    Expect(Token),
    Failure(ParseFailure),
}

impl ParseError {
    pub(crate) fn failure<D: Into<Cow<'static, str>>>(detail: D) -> Self {
        ParseError::Failure(ParseFailure {
            detail: detail.into(),
        })
    }

    pub(crate) fn unexpect<D: Into<Cow<'static, str>>>(detail: D) -> Self {
        ParseError::Failure(ParseFailure {
            detail: detail.into(),
        })
    }
}

#[derive(Debug)]
pub struct ParseFailure {
    detail: Cow<'static, str>,
}

impl From<TokenError> for ParseError {
    fn from(e: TokenError) -> Self {
        ParseError::Failure(ParseFailure {
            detail: e.detail.unwrap(),
        })
    }
}

type IResult<I, O> = Result<(I, O), ParseError>;

trait AstParser: Debug + Sized {
    fn name(&self) -> &'static str;
    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self>;
}

impl AstParser for Ident {
    fn name(&self) -> &'static str {
        "Ident"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let mut input = i.clone();

        match input.next() {
            Some(t) => match t.0 {
                Token::Ident(ident) => Ok((input, ident)),
                _ => Err(ParseError::Unexpect(t.0)),
            },
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Keyword {
    fn name(&self) -> &'static str {
        "Keyword"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let mut input = i.clone();

        match input.next() {
            Some(t) => match t.0 {
                Token::Keyword(kw) => Ok((input, kw)),
                _ => Err(ParseError::Unexpect(t.0)),
            },
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Literal {
    fn name(&self) -> &'static str {
        "Literal"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let mut input = i.clone();

        match input.next() {
            Some(t) => match t.0 {
                Token::Literal(lit) => Ok((input, lit)),
                _ => Err(ParseError::Unexpect(t.0)),
            },
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for PathSegment {
    fn name(&self) -> &'static str {
        "PathSegment"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let mut input = i.clone();
        match input.next() {
            Some(t) => match t.0 {
                Token::Ident(ident) => Ok((input, PathSegment::Ident(ident))),
                Token::Keyword(Keyword::Super) => Ok((input, PathSegment::PathSuper)),
                Token::Keyword(Keyword::SelfValue) => Ok((input, PathSegment::PathSelf)),
                Token::Keyword(Keyword::Crate) => Ok((input, PathSegment::PathCrate)),
                _ => Err(ParseError::Unexpect(t.0)),
            },

            None => Err(ParseError::Incomplete),
        }
    }
}

macro_rules! expect_token {
    ($i:expr, $token:ident, $match:pat) => {{
        let input = $i.clone();
        match $token::parse(input) {
            Ok((i, t)) => {
                if matches!(t, $match) {
                    Ok((input, t))
                } else {
                    return Err(ParseError::Unexpect(t));
                }
            }
            Err(e) => return Err(e),
        }
    }};
}

impl AstParser for ImportStmt {
    fn name(&self) -> &'static str {
        "ImportStmt"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let (i, _) = expect_token(i, Token::Keyword(Keyword::Use))?;

        let (i, tree) = UseTree::parse(i)?;

        let ret = ImportStmt { items: tree.flat() };

        let (i, _) = expect_token(i, Token::Symbol(Symbol::Semicolon))?;

        Ok((i, ret))
    }
}

impl AstParser for UseTree {
    fn name(&self) -> &'static str {
        "ImportItem"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        parse_usetree(i, false)
    }
}

struct Parser {
    input: TokenStream,
}

impl Parser {
    fn parse_usetree(i: TokenStream, sub: bool) -> IResult<TokenStream, UseTree> {
        let mut ret = UseTree {
            path: Vec::new(),
            alias: None,
            children: Vec::new(),
        };

        let (i, seg) = PathSegment::parse(i)?;
        ret.path.push(seg);

        let mut input = i.clone();

        loop {
            let input_cloned = input.clone();

            match input.next() {
                Some(token) => match token.0 {
                    Token::Symbol(Symbol::PathSep) => match input.next() {
                        Some(token) => match token.0 {
                            Token::Symbol(Symbol::LBracket) => {
                                let ts = input.clone();
                                loop {
                                    let (mut ts, item) = parse_usetree(ts, true)?;
                                    ret.children.push(item);

                                    match ts.next() {
                                        Some(token) => match token.0 {
                                            Token::Symbol(Symbol::Comma) => {
                                                continue;
                                            }
                                            Token::Symbol(Symbol::LBracket) => {
                                                break;
                                            }
                                            _ => {
                                                return Err(ParseError::Unexpect(token.0));
                                            }
                                        },

                                        None => {
                                            break;
                                        }
                                    }
                                }
                            }

                            // try PathSegment
                            _ => {
                                let token_cloned = token.clone();
                                let ts = TokenStream::new(vec![token]);
                                match PathSegment::parse(ts) {
                                    Ok((i, p)) => {
                                        ret.path.push(p);
                                    }
                                    Err(_e) => return Err(ParseError::Unexpect(token_cloned.0)),
                                }
                            }
                        },

                        None => {
                            return Err(ParseError::Incomplete);
                        }
                    },
                    Token::Keyword(Keyword::As) => {
                        let (i, alias) = Ident::parse(input)?;

                        ret.alias = Some(alias);

                        return Ok((i, ret));
                    }
                    Token::Symbol(Symbol::Comma) | Token::Symbol(Symbol::Semicolon) => {
                        return Ok((input_cloned, ret));
                    }
                    _ => {
                        return Err(ParseError::Unexpect(token.0));
                    }
                },
                None => {
                    if sub {
                        return Ok((input, ret));
                    } else {
                        return Err(ParseError::failure("unexpect Eof"));
                    }
                }
            };
        }
    }
}

fn expect_token(i: TokenStream, expected: Token) -> IResult<TokenStream, Token> {
    let mut input = i.clone();

    match input.next_token() {
        Some(token) if token == expected => Ok((input, token)),
        Some(token) => Err(ParseError::unexpect(format!(
            "expect{expected:?}, but found{token:?}"
        ))),
        None => Err(ParseError::failure("unexpect EOF")),
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::nodes::ImportStmt, ast::Ident, tokenizer::Tokenizer};

    use super::{AstParser, ParseError};

    #[test]
    fn ident() {
        let inputs = [
            ("a", Ok(Ident::new("a"))),
            ("use", Err(ParseError::Unexpect)),
        ];

        for input in inputs {
            let mut i = Tokenizer::new("", input.0).token_stream().unwrap();

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
            (
                "use std::{net::TcpStream as TcpSocket, encoding::json, fs::{open, close}, io::{read as r, write as w}};",
                true,
            ),
        ];

        for input in inputs {
            let mut i = Tokenizer::new("", input.0).token_stream().unwrap();

            println!("=> {:?}", ImportStmt::parse(i));
        }
    }
}
