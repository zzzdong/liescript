use std::borrow::Cow;
use std::fmt;

use crate::{
    ast::{
        nodes::{
            PathSegment, PrimitiveTy, StructField, StructItem, Ty, TypePath, UseItem, UseStmt,
            UseTree, Visibility, expr::Expr,
        },
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

#[derive(Debug)]
enum PathNode {
    Seg((PathSegment, Option<Ident>)),
    Tree(Vec<Vec<PathNode>>),
}

impl PathNode {
    fn flat(nodes: Vec<PathNode>) -> Vec<UseItem> {
        let mut alias = None;
        let mut stack = Vec::new();

        for node in nodes {
            match node {
                PathNode::Seg((seg, a)) => {
                    stack.push(seg);
                    alias = a;
                }
                PathNode::Tree(subs) => {
                    let mut ret = Vec::new();

                    for s in subs {
                        let ss = stack.clone();
                        let n = PathNode::flat(s);
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
        let use_tree = self.parse_use_tree()?;

        Ok(UseStmt {
            items: PathNode::flat(use_tree),
        })
    }

    pub fn parse_use_tree(&mut self) -> Result<Vec<PathNode>, ParseError> {
        self.separated_list(Symbol::PathSep, Parser::parse_node)
    }

    pub fn parse_node(&mut self) -> Result<PathNode, ParseError> {
        let tok = self.peek_token()?;
        match tok {
            Token::Symbol(Symbol::LBracket) => {
                self.consume_token()?;
                let tree = self.separated_list(Symbol::Comma, Parser::parse_use_tree)?;
                self.expect_token(Token::Symbol(Symbol::RBracket))?;
                Ok(PathNode::Tree(tree))
            }
            _ => {
                let seg = self.parse_path_segment()?;

                let alias = if let Ok(Token::Keyword(Keyword::As)) = self.peek_token() {
                    self.consume_token()?;
                    Some(self.parse_ident()?)
                } else {
                    None
                };

                Ok(PathNode::Seg((seg, alias)))
            }
        }
    }

    pub fn parse_struct_item(&mut self) -> Result<StructItem, ParseError> {
        self.expect_token(Token::Keyword(Keyword::Struct))?;

        let name = self.parse_ident()?;

        self.expect_token(Token::Symbol(Symbol::LBracket))?;

        let fields =
            self.separated_list0(Symbol::Comma, Parser::parse_struct_field, Symbol::RBracket)?;

        self.expect_token(Token::Symbol(Symbol::RBracket))?;

        Ok(StructItem { name, fields })
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParseError> {
        let visibility = self.try_visibility().unwrap_or_default();

        let name = self.parse_ident()?;
        self.expect_token(Token::Symbol(Symbol::Colon))?;

        let ty = match self.try_primitive() {
            Some(ty) => Ty::Primitive(ty),
            None => Ty::TypePath(self.parse_type_path()?),
        };

        Ok(StructField {
            visibility,
            name,
            ty,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        unimplemented!()
    }



    fn parse_type_path(&mut self) -> Result<TypePath, ParseError> {
        let path = self.separated_list(Symbol::PathSep, Parser::parse_path_segment)?;

        Ok(TypePath { path })
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

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        match self.consume_token()? {
            Token::Ident(ident) => Ok(ident),
            token => Err(ParseError::unexpect("ident", token)),
        }
    }

    /// Look for visibility and consume it if it exists
    fn try_visibility(&mut self) -> Option<Visibility> {
        self.try_next(|tok| match tok {
            Token::Keyword(kw) => match kw {
                Keyword::Pub => Some(Visibility::Pub),
                Keyword::Priv => Some(Visibility::Priv),
                _ => return None,
            },
            _ => None,
        })
    }

    /// Look for primitive and consume it if it exists
    fn try_primitive(&mut self) -> Option<PrimitiveTy> {
        self.try_next(|tok| match tok {
            Token::Keyword(kw) => match kw {
                Keyword::Byte => Some(PrimitiveTy::Byte),
                Keyword::Char => Some(PrimitiveTy::Char),
                Keyword::Int => Some(PrimitiveTy::Int),
                Keyword::Float => Some(PrimitiveTy::Float),
                Keyword::Str => Some(PrimitiveTy::Str),
                _ => return None,
            },
            _ => None,
        })
    }

    /// Look for an expected symbol and consume it if it exists
    fn try_symbol(&mut self, expected: Symbol) -> bool {
        self.next_token(&Token::Symbol(expected))
    }

    fn separated_list<T, F>(&mut self, sep: Symbol, f: F) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            values.push(f(self)?);
            if !self.try_symbol(sep) {
                break;
            }
        }
        Ok(values)
    }

    fn separated_list0<T, F>(
        &mut self,
        sep: Symbol,
        f: F,
        terminal: Symbol,
    ) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            if self.test_next(&Token::Symbol(terminal)) {
                break;
            }
            values.push(f(self)?);
            if !self.try_symbol(sep) {
                break;
            }
        }
        Ok(values)
    }

    /// Peek and test next token
    fn test_next(&mut self, expected: &Token) -> bool {
        match self.peek_token() {
            Ok(tok) => &tok == expected,
            _ => false,
        }
    }

    /// Check next token, consume it if ok
    fn try_next<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(Token) -> Option<T>,
    {
        match self.peek_token() {
            Ok(tok) => f(tok).map(|t| {
                self.consume_token().unwrap();
                t
            }),
            _ => None,
        }
    }

    /// Consume and return the next token
    #[must_use]
    fn consume_token(&mut self) -> Result<Token, ParseError> {
        self.input.next_token().ok_or(ParseError::eof())
    }

    /// Consume next token, and check it with pattern
    fn expect_token<T, P>(&mut self, pat: P) -> Result<T, ParseError>
    where
        P: Pattern<Output = T>,
    {
        let tok = self.consume_token()?;
        pat.parse(tok)
    }

    /// Peek next token without cunsume it
    fn peek_token(&mut self) -> Result<Token, ParseError> {
        self.input.clone().next_token().ok_or(ParseError::eof())
    }

    /// Consume the next token if it matches the expected token, otherwise return false
    #[must_use]
    fn next_token(&mut self, expected: &Token) -> bool {
        match self.peek_token() {
            Ok(tok) if tok == *expected => {
                self.input.next_token().unwrap();
                true
            }
            _ => false,
        }
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
        parser::{Parser, PathNode},
        tokenizer::Tokenizer,
    };

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

            for item in PathNode::flat(ret.unwrap()) {
                println!("->{:?}", item);
            }

            println!("======");
        }
    }

    #[test]
    fn use_item() {
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
        }
    }

    #[test]
    fn struct_item() {
        let inputs = [
            (r#"struct AAA {}"#, true),
            (
                r"struct A { a: int, pub b: float, priv c:byte, priv d: AAA}",
                true,
            ),
        ];

        for input in inputs {
            let i = Tokenizer::new("", input.0).token_stream().unwrap();

            let mut parser = Parser::new(i.clone());

            println!("=> {:?}", parser.parse_struct_item());
        }
    }
}
