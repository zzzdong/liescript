use std::borrow::Cow;
use std::fmt;

use crate::{
    ast::{
        nodes::{PathSegment, StructField, StructItem, TypePath, UseItem, UseStmt, UseTree},
        Ident, Keyword, Literal, Symbol,
    },
    tokenizer::{Span, Token, TokenError, TokenStream},
};

#[derive(Debug)]
pub enum ParseError {
    /// The parser had an error (recoverable)
    Error(Cow<'static, str>),
    /// The parser had an unrecoverable error
    Failure(Cow<'static, str>),
}

impl ParseError {
    pub(crate) fn failure<D: Into<Cow<'static, str>>>(detail: D) -> Self {
        ParseError::Failure(detail.into())
    }

    pub(crate) fn unexpect(expected: impl std::fmt::Display, found: Token) -> Self {
        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found {found:?}"
        )))
    }

    pub(crate) fn eof() -> Self {
        ParseError::Failure(Cow::Borrowed("unexpect EOF"))
    }
}

impl From<TokenError> for ParseError {
    fn from(e: TokenError) -> Self {
        ParseError::Failure(e.detail.unwrap())
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

#[derive(Debug)]
struct UsePathNode {
    value: PathSegment,
    children: Vec<UsePathNode>,
    alias: Option<Ident>,
}

impl UsePathNode {
    fn new(seg: PathSegment) -> Self {
        UsePathNode {
            value: seg,
            children: Vec::new(),
            alias: None,
        }
    }

    fn flat(self, mut stack: Vec<PathSegment>) -> Vec<UseItem> {
        stack.push(self.value);

        if self.children.is_empty() {
            vec![UseItem {
                path: stack,
                alias: self.alias,
            }]
        } else {
            let mut ret = Vec::new();

            for child in self.children {
                let path = stack.clone();
                ret.extend(child.flat(path));
            }

            ret
        }
    }

    fn print(node: &UsePathNode, offset: usize) {
        println!(
            "{:width$}{:?} {:?}",
            "",
            node.value,
            node.alias,
            width = offset,
        );
        for child in &node.children {
            Self::print(child, offset + 2);
        }
    }
}

#[derive(Debug)]
enum Node {
    Seg((PathSegment, Option<Ident>)),
    Tree(Vec<Vec<Node>>),
}

impl Node {
    fn flat(nodes: Vec<Node>) -> Vec<UseItem> {
        let mut alias = None;
        let mut stack = Vec::new();

        for node in nodes {
            match node {
                Node::Seg((seg, a)) => {
                    stack.push(seg);
                    alias = a;
                }
                Node::Tree(subs) => {
                    let mut ret = Vec::new();

                    for s in subs {
                        let ss = stack.clone();
                        let n = Node::flat(s);
                        for mut nn in n {
                            let mut p = ss.clone();
                            p.extend(nn.path);
                            nn.path = p;
                            ret.push(nn);
                        }
                    }

                    return ret;
                }
            }
        }

        vec![UseItem { path: stack, alias }]
    }
}

#[derive(Debug)]
struct Parser {
    input: TokenStream,
    last_span: Option<Span>,
}

impl Parser {
    pub fn new(input: TokenStream) -> Self {
        Parser {
            input,
            last_span: None,
        }
    }

    pub fn parse_use_stmt(&mut self) -> Result<UseStmt, ParseError> {
        self.expect_token(Token::Keyword(Keyword::Use))?;
        let use_tree = self.parse_usetree()?;

        let path = Vec::new();

        Ok(UseStmt {
            items: use_tree.flat(path),
        })
    }

    pub fn parse_use_stmt2(&mut self) -> Result<UseStmt, ParseError> {
        self.expect_token(Token::Keyword(Keyword::Use))?;
        let use_tree = self.parse_use_tree()?;

        Ok(UseStmt {
            items: Node::flat(use_tree),
        })
    }

    pub fn parse_node(&mut self) -> Result<Node, ParseError> {
        let tok = self.peek_token()?;
        match tok {
            Token::Symbol(Symbol::LBracket) => {
                self.next_token()?;
                let tree = self.separated_list(Symbol::Comma, Parser::parse_use_tree)?;
                self.expect_token(Token::Symbol(Symbol::RBracket))?;
                Ok(Node::Tree(tree))
            }
            _ => {
                let seg = self.parse_path_segment()?;

                let alias = if let Ok(Token::Keyword(Keyword::As)) = self.peek_token() {
                    self.next_token()?;
                    Some(self.parse_ident()?)
                } else {
                    None
                };

                Ok(Node::Seg((seg, alias)))
            }
        }
    }

    pub fn parse_use_tree(&mut self) -> Result<Vec<Node>, ParseError> {
        self.separated_list(Symbol::PathSep, Parser::parse_node)
    }

    // pub fn parse_struct_item(&mut self) -> Result<StructItem, ParseError> {
    //     self.expect_token(Token::Keyword(Keyword::Struct))?;

    //     let name = self.parse_ident()?;

    //     self.expect_token(Token::Symbol(Symbol::LBracket))?;

    //     self.parse_struct_fileds()?;

    //     self.expect_token(Token::Symbol(Symbol::RBracket))?;

    // }

    // fn parse_struct_fileds(&mut self) -> Result<Vec<StructField>, ParseError> {
    //     let tok = self.peek_token()?;

    //     match tok {
    //         Token::Symbol(Symbol::RBracket) => {
    //             break;
    //         }
    //         Token::
    //     }

    // }

    // fn parse_struct_field(&mut self) -> Result<StructField, ParseError> {
    //     let ident = self.parse_ident()?;
    //     self.expect_token(Token::Symbol(Symbol::Colon))?;
    //     self.parse_type_path()?;
    // }

    fn parse_type_path(&mut self) -> Result<TypePath, ParseError> {
        let path = self.separated_list(Symbol::PathSep, Parser::parse_path_segment)?;

        Ok(TypePath { path })
    }

    fn parse_path_segment(&mut self) -> Result<PathSegment, ParseError> {
        match self.next_token()? {
            Token::Ident(ident) => Ok(PathSegment::Ident(ident)),
            Token::Keyword(Keyword::Super) => Ok(PathSegment::PathSuper),
            Token::Keyword(Keyword::SelfValue) => Ok(PathSegment::PathSelf),
            Token::Keyword(Keyword::Crate) => Ok(PathSegment::PathCrate),
            t => Err(ParseError::unexpect("PathSegment", t)),
        }
    }

    fn parse_usetree(&mut self) -> Result<UsePathNode, ParseError> {
        self.parse_sub_usetree(&[Token::Symbol(Symbol::Semicolon)])
    }

    fn parse_sub_usetree(&mut self, delimited: &[Token]) -> Result<UsePathNode, ParseError> {
        let seg = self.parse_path_segment()?;

        let mut root = UsePathNode::new(seg);

        let mut tmp = &mut root;

        loop {
            match self.peek_token()? {
                Token::Symbol(Symbol::PathSep) => {
                    self.next_token()?;
                }
                Token::Keyword(Keyword::As) => {
                    self.next_token()?;
                    let ident = self.parse_ident()?;
                    tmp.alias = Some(ident);

                    let tok = self.peek_token()?;
                    if !delimited.contains(&tok) {
                        return Err(ParseError::unexpect("delimited", tok));
                    }
                    break;
                }
                tok if delimited.contains(&tok) => break,
                tok => {
                    return Err(ParseError::unexpect("path_segment1", tok));
                }
            }

            match self.peek_token()? {
                Token::Symbol(Symbol::LBracket) => {
                    self.expect_token(Token::Symbol(Symbol::LBracket))?;
                    let tree = self.parse_sub_usetree(&[
                        Token::Symbol(Symbol::Comma),
                        Token::Symbol(Symbol::RBracket),
                    ])?;
                    tmp.children.push(tree);
                    loop {
                        match self.next_token()? {
                            Token::Symbol(Symbol::Comma) => {}
                            Token::Symbol(Symbol::RBracket) => {
                                break;
                            }
                            tok => {
                                return Err(ParseError::unexpect("path_segment2", tok));
                            }
                        }

                        let tree = self.parse_sub_usetree(&[
                            Token::Symbol(Symbol::Comma),
                            Token::Symbol(Symbol::RBracket),
                        ])?;
                        tmp.children.push(tree);
                    }
                }
                tok => {
                    let seg = self.parse_path_segment()?;
                    let node = UsePathNode::new(seg);
                    tmp.children.push(node);
                    tmp = tmp.children.last_mut().unwrap();
                }
            }
        }

        return Ok(root);
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        match self.next_token()? {
            Token::Ident(ident) => Ok(ident),
            token => Err(ParseError::unexpect("ident", token)),
        }
    }

    /// Look for an expected symbol and consume it if it exists
    fn parse_symbol(&mut self, expected: Symbol) -> bool {
        match self.peek_token() {
            Ok(Token::Symbol(sym)) if expected == sym => {
                self.next_token().unwrap();
                true
            }
            _ => false,
        }
    }

    fn separated_list<T, F>(&mut self, sep: Symbol, f: F) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            values.push(f(self)?);
            if !self.parse_symbol(sep) {
                break;
            }
        }
        Ok(values)
    }

    /// Consume the next token if it matches the expected token, otherwise return false
    #[must_use]
    fn consume_token(&mut self, expected: &Token) -> bool {
        match self.peek_token() {
            Ok(tok) if tok == *expected => {
                self.input.next_token().unwrap();
                true
            }
            _ => false,
        }
    }

    fn expect_token<T, P>(&mut self, pat: P) -> Result<T, ParseError>
    where
        P: Pattern<Output = T>,
    {
        let tok = self.next_token()?;
        pat.parse(tok)
    }

    fn peek_token(&mut self) -> Result<Token, ParseError> {
        self.input.clone().next_token().ok_or(ParseError::eof())
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        self.input.next_token().ok_or(ParseError::eof())
    }
}

trait Pattern {
    type Output;
    fn parse(self, tok: Token) -> Result<Self::Output, ParseError>;
}

impl Pattern for Token {
    type Output = Token;

    fn parse(self, tok: Token) -> Result<Token, ParseError> {
        if tok == self {
            Ok(tok)
        } else {
            Err(ParseError::unexpect(format!("{self:?}"), tok))
        }
    }
}

impl<F> Pattern for F
where
    F: Fn(Token) -> Result<Token, ParseError>,
{
    type Output = Token;

    fn parse(self, tok: Token) -> Result<Token, ParseError> {
        self(tok)
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::nodes::UseStmt,
        ast::Ident,
        parser::{Node, Parser},
        tokenizer::Tokenizer,
    };

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

            let mut parser = Parser::new(i.clone());

            println!("=> {:?}", parser.parse_use_stmt());

            let mut parser = Parser::new(i);
            println!("-> {:?}", parser.parse_use_stmt2())
        }
    }

    #[test]
    fn parse_use_tree() {
        let inputs = [
            ("std::net::TcpStream;", true),
            ("crate::parser;", true),
            (
                "std::{net::TcpStream, encoding::json, fs::{open, close}, io::{read, write}};",
                true,
            ),
            (
                "std::{net::TcpStream as TcpSocket, encoding::json, fs::{open, close}, io::{read as r, write as w}};",
                true,
            ),
        ];

        for input in inputs {
            let i = Tokenizer::new("", input.0).token_stream().unwrap();

            let mut parser = Parser::new(i);

            let ret = parser.parse_use_tree();

            for item in Node::flat(ret.unwrap()) {
                println!("->{:?}", item);
            }

            println!("======");
        }
    }
}
