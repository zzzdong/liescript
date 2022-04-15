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
pub struct ParseError {
    detail: Cow<'static, str>,
}

impl ParseError {
    pub(crate) fn failure<D: Into<Cow<'static, str>>>(detail: D) -> Self {
        ParseError {
            detail: detail.into(),
        }
    }

    pub(crate) fn unexpect(expected: impl std::fmt::Display, found: Token) -> Self {
        ParseError {
            detail: Cow::Owned(format!("expected {expected}, but found {found:?}")),
        }
    }

    pub(crate) fn eof() -> Self {
        ParseError {
            detail: Cow::Borrowed("unexpect EOF"),
        }
    }
}

impl From<TokenError> for ParseError {
    fn from(e: TokenError) -> Self {
        ParseError {
            detail: e.detail.unwrap(),
        }
    }
}

// type IResult<I, O> = Result<(I, O), ParseError>;

// trait AstParser: Debug + Sized {
//     fn name(&self) -> &'static str;
//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self>;
// }

// impl AstParser for Ident {
//     fn name(&self) -> &'static str {
//         "Ident"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         let mut input = i.clone();

//         match input.next() {
//             Some(t) => match t.0 {
//                 Token::Ident(ident) => Ok((input, ident)),
//                 _ => Err(ParseError::Unexpect(t.0)),
//             },
//             None => Err(ParseError::Incomplete),
//         }
//     }
// }

// impl AstParser for Keyword {
//     fn name(&self) -> &'static str {
//         "Keyword"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         let mut input = i.clone();

//         match input.next() {
//             Some(t) => match t.0 {
//                 Token::Keyword(kw) => Ok((input, kw)),
//                 _ => Err(ParseError::Unexpect(t.0)),
//             },
//             None => Err(ParseError::Incomplete),
//         }
//     }
// }

// impl AstParser for Literal {
//     fn name(&self) -> &'static str {
//         "Literal"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         let mut input = i.clone();

//         match input.next() {
//             Some(t) => match t.0 {
//                 Token::Literal(lit) => Ok((input, lit)),
//                 _ => Err(ParseError::Unexpect(t.0)),
//             },
//             None => Err(ParseError::Incomplete),
//         }
//     }
// }

// impl AstParser for PathSegment {
//     fn name(&self) -> &'static str {
//         "PathSegment"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         let mut input = i.clone();
//         match input.next() {
//             Some(t) => match t.0 {
//                 Token::Ident(ident) => Ok((input, PathSegment::Ident(ident))),
//                 Token::Keyword(Keyword::Super) => Ok((input, PathSegment::PathSuper)),
//                 Token::Keyword(Keyword::SelfValue) => Ok((input, PathSegment::PathSelf)),
//                 Token::Keyword(Keyword::Crate) => Ok((input, PathSegment::PathCrate)),
//                 _ => Err(ParseError::Unexpect(t.0)),
//             },

//             None => Err(ParseError::Incomplete),
//         }
//     }
// }

// macro_rules! expect_token {
//     ($i:expr, $token:ident, $match:pat) => {{
//         let input = $i.clone();
//         match $token::parse(input) {
//             Ok((i, t)) => {
//                 if matches!(t, $match) {
//                     Ok((input, t))
//                 } else {
//                     return Err(ParseError::Unexpect(t));
//                 }
//             }
//             Err(e) => return Err(e),
//         }
//     }};
// }

// impl AstParser for ImportStmt {
//     fn name(&self) -> &'static str {
//         "ImportStmt"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         let (i, _) = expect_token(i, Token::Keyword(Keyword::Use))?;

//         let (i, tree) = UseTree::parse(i)?;

//         let ret = ImportStmt { items: tree.flat() };

//         let (i, _) = expect_token(i, Token::Symbol(Symbol::Semicolon))?;

//         Ok((i, ret))
//     }
// }

// impl AstParser for UseTree {
//     fn name(&self) -> &'static str {
//         "ImportItem"
//     }

//     fn parse<'i>(i: TokenStream) -> IResult<TokenStream, Self> {
//         parse_usetree(i, false)
//     }
// }

struct Node {
    value: PathSegment,
    children: Vec<Node>,
    alias: Option<Ident>,
}

impl Node {
    fn new(seg: PathSegment) -> Self {
        Node { value: seg, children: Vec::new(), alias: None }
    }
}


#[derive(Debug)]
struct Parser {
    input: TokenStream,
}

impl Parser {
    pub fn new(input: TokenStream) -> Self {
        Parser { input }
    }

    pub fn parse_import_item(&mut self) -> Result<ImportStmt, ParseError> {
        self.expect_token(Token::Keyword(Keyword::Use))?;
        let use_tree = self.parse_usetree()?;

        Ok(ImportStmt {
            items: use_tree.flat(),
        })
    }

    fn parse_path_segment(&mut self) -> Result<PathSegment, ParseError> {
        match self.consume_token()? {
            Token::Ident(ident) => Ok(PathSegment::Ident(ident)),
            Token::Keyword(Keyword::Super) => Ok(PathSegment::PathSuper),
            Token::Keyword(Keyword::SelfValue) => Ok(PathSegment::PathSelf),
            Token::Keyword(Keyword::Crate) => Ok(PathSegment::PathCrate),
            t => Err(ParseError::unexpect("PathSegment", t)),
        }
    }

    fn parse_usetree(&mut self) -> Result<Node, ParseError> {
        // self.parse_sub_usetree(&[Token::Symbol(Symbol::Semicolon)])
    }

    fn parse_sub_usetree(&mut self) -> Result<Node, ParseError> {
        let mut ret = UseTree {
            path: Vec::new(),
            alias: None,
            children: Vec::new(),
        };

        self.expect_token(Token::Symbol(Symbol::LBracket))?;

        let seg = self.parse_path_segment()?;

        let mut tmp = Node::new(seg);

        loop {
            match self.peek_token()? {
                Token::Symbol(Symbol::PathSep) => {
                    self.consume_token();
                }
                Token::Symbol(Symbol::LBracket) => {
                    let tree = self.parse_sub_usetree()?;
                    tmp.children.push(tree);
                }
                Token::Symbol(Symbol::RBracket) => {
                    break;
                }
                Token::Symbol(Symbol::Comma) => {
                    self.parse_usetree()?;
                }
            



            }
        }

        // loop {
        //     let seg = self.parse_path_segment()?;
        //     ret.path.push(seg);

        //     match self.consume_token()? {
        //         Token::Symbol(Symbol::PathSep) => {
        //             match self.peek_token()? {
        //                 // parse sub tree
        //                 Token::Symbol(Symbol::LBracket) => {
        //                     self.expect_token(Token::Symbol(Symbol::LBracket))?;
        //                     let sub_tree =
        //                         self.parse_sub_usetree(&[Token::Symbol(Symbol::RBracket), Token::Symbol(Symbol::Comma),])?;
        //                     ret.children.push(sub_tree);
        //                 }
        //                 // try PathSegment
        //                 _ => {

        //                 }
        //             }
        //         }
        //         Token::Keyword(Keyword::As) => {
        //             let ident = self.parse_ident()?;
        //             ret.alias = Some(ident);
        //             break;
        //         }
        //         tok if delimited.contains(&tok) => {
        //             break;
        //         }
        //         tok => {
        //             println!("{ret:?}");
        //             return Err(ParseError::unexpect("path_segment or delimit", tok));
        //         }
        //     };
        // }

        return Ok(ret);
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        match self.consume_token()? {
            Token::Ident(ident) => Ok(ident),
            token => Err(ParseError::unexpect("ident", token)),
        }
    }

    fn peek_token(&mut self) -> Result<Token, ParseError> {
        self.input.clone().next_token().ok_or(ParseError::eof())
    }

    fn consume_token(&mut self) -> Result<Token, ParseError> {
        self.input.next_token().ok_or(ParseError::eof())
    }

    fn expect_token(&mut self, expected: Token) -> Result<Token, ParseError> {
        match self.consume_token()? {
            token if token == expected => Ok(token),
            token => Err(ParseError::unexpect(format!("{expected:?}"), token)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{ast::nodes::ImportStmt, ast::Ident, parser::Parser, tokenizer::Tokenizer};

    use super::ParseError;

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
            let i = Tokenizer::new("", input.0).token_stream().unwrap();

            let mut parser = Parser::new(i);

            println!("=> {:?}", parser.parse_import_item());
        }
    }
}
