use std::borrow::Cow;
use std::fmt::Debug;

use crate::{
    ast::nodes::{ImportStmt, PathSegment, UseTree},
    token::{Ident, Keyword, Literal, Symbol, Token, TokenError, TokenKind},
    tokenizer::TokenStream,
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
            Some(t) => match t.kind {
                TokenKind::Ident(ident) => Ok((input, ident)),
                _ => Err(ParseError::Unexpect(t)),
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
            Some(t) => match t.kind {
                TokenKind::Keyword(kw) => Ok((input, kw)),
                _ => Err(ParseError::Unexpect(t)),
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
            Some(t) => match t.kind {
                TokenKind::Literal(lit) => Ok((input, lit)),
                _ => Err(ParseError::Unexpect(t)),
            },
            None => Err(ParseError::Incomplete),
        }
    }
}

impl AstParser for Symbol {
    fn name(&self) -> &'static str {
        "Symbol"
    }

    fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
        let mut input = i.clone();

        match input.next() {
            Some(t) => match t.kind {
                TokenKind::Symbol(sym) => Ok((input, sym)),
                _ => Err(ParseError::Unexpect(t)),
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
            Some(t) => match t.kind {
                TokenKind::Ident(ident) => Ok((input, PathSegment::Ident(ident))),
                TokenKind::Keyword(Keyword::Super) => Ok((input, PathSegment::PathSuper)),
                TokenKind::Keyword(Keyword::SelfValue) => Ok((input, PathSegment::PathSelf)),
                TokenKind::Keyword(Keyword::Crate) => Ok((input, PathSegment::PathCrate)),
                _ => Err(ParseError::Unexpect(t)),
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
        let (i, _) = expect_keyword(i, Keyword::Use)?;

        let (i, tree) = UseTree::parse(i)?;

        let ret = ImportStmt { items: tree.flat() };

        let (i, _) = expect_symbol(i, Symbol::Semicolon)?;

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
            Some(token) => match token.kind {
                TokenKind::Symbol(Symbol::PathSep) => match input.next() {
                    Some(token) => match token.kind {
                        TokenKind::BracketTree(tree) => {
                            let tree = TokenStream::new(tree);
                            let mut ts = tree;
                            // loop tree
                            loop {
                                let (i, item) = parse_usetree(ts, true)?;
                                ret.children.push(item);
                                ts = i;

                                match ts.next() {
                                    Some(token) => match token.kind {
                                        TokenKind::Symbol(Symbol::Comma) => {
                                            continue;
                                        }
                                        _ => {
                                            return Err(ParseError::Unexpect(token));
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
                                Err(_e) => return Err(ParseError::Unexpect(token_cloned)),
                            }
                        }
                    },

                    None => {
                        return Err(ParseError::Incomplete);
                    }
                },
                TokenKind::Keyword(Keyword::As) => {
                    let (i, alias) = Ident::parse(input)?;

                    ret.alias = Some(alias);

                    return Ok((i, ret));
                }
                TokenKind::Symbol(Symbol::Comma) | TokenKind::Symbol(Symbol::Semicolon) => {
                    return Ok((input_cloned, ret));
                }
                _ => {
                    return Err(ParseError::Unexpect(token));
                }
            },
            None => {
                if sub {
                    return Ok((input, ret));
                } else {
                    return Err(ParseError::failure("unexpect Eof"))
                }
                
            }
        };
    }
}




fn expect_keyword(i: TokenStream, keyword: Keyword) -> IResult<TokenStream, Keyword> {
    let input = i.clone();

    Keyword::parse(input).and_then(|(i, kw)| {
        if keyword == kw {
            Ok((i, kw))
        } else {
            Err(ParseError::failure(format!(
                "expect Keyword::{keyword:?}, but found Keyword::{kw:?}"
            )))
        }
    })
}

fn expect_symbol(i: TokenStream, symbol: Symbol) -> IResult<TokenStream, Symbol> {
    let input = i.clone();

    Symbol::parse(input).and_then(|(i, sym)| {
        if symbol == sym {
            Ok((i, sym))
        } else {
            Err(ParseError::failure(format!(
                "expect Keyword::{symbol:?}, but found Keyword::{sym:?}"
            )))
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
