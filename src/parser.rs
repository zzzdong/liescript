use log::debug;
use std::borrow::Cow;

use crate::ast::*;
use crate::tokenizer::{Span, Token, TokenError, TokenKind, TokenStream, Tokenizer};

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

    pub(crate) fn unexpect(expected: impl std::fmt::Display, found: &Token) -> Self {
        let span = found.span;
        let tok = &found.kind;

        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found `{tok}` @ {}",
            span
        )))
    }

    pub(crate) fn unexpect_kind(expected: impl std::fmt::Display, found: &TokenKind) -> Self {
        ParseError::Error(Cow::Owned(format!(
            "expected {expected}, but found `{found:?}`"
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
    fn flat(nodes: Vec<PathNode>) -> Vec<UsePath> {
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

        vec![UsePath { path: stack, alias }]
    }
}

/// https://doc.rust-lang.org/reference/expressions.html#expression-precedence
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest = 0,
    Assign,
    Range,
    LogicOr,
    LogicAnd,
    Equal,
    Compare,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    Term,
    Factor,
    As,
    Prefix,
    Postfix,
    Call,
    Path,
}

#[derive(Debug)]
struct Parser {
    input: TokenStream,
    last_span: Option<Span>,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let ts = Tokenizer::new(input).token_stream().unwrap();

        Parser {
            input: ts,
            last_span: None,
        }
    }

    pub fn parse_top_level(&mut self) -> Result<Ast, ParseError> {
        let mut stmts = Vec::new();

        loop {
            if self.is_eof() {
                break;
            }

            let tok = self.peek_token()?;

            match tok.kind {
                TokenKind::Keyword(Keyword::Let) => {
                    let local = self.parse_let_stmt()?;
                    stmts.push(Statement::Let(local));
                }
                TokenKind::Keyword(Keyword::Use) => {
                    let item = self.parse_use_stmt()?;
                    self.expect_token(TokenKind::Symbol(Symbol::Semicolon))?;
                    stmts.push(Statement::Item(Item::Use(item)));
                }
                TokenKind::Keyword(Keyword::Struct) => {
                    let item = self.parse_struct_item()?;
                    stmts.push(Statement::Item(Item::Struct(item)));
                }
                TokenKind::Keyword(Keyword::Fn) => {
                    let item = self.parse_fn_item()?;
                    stmts.push(Statement::Item(Item::Fn(item)));
                }
                TokenKind::Symbol(Symbol::Semicolon) => {
                    stmts.push(Statement::Empty);
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if self.next_token(&TokenKind::Symbol(Symbol::Semicolon)) {
                        stmts.push(Statement::Semi(expr));
                    } else {
                        stmts.push(Statement::Expr(expr));
                    }
                }
            }
        }

        Ok(Ast { stmts })
    }

    pub fn parse_use_stmt(&mut self) -> Result<ItemUse, ParseError> {
        self.expect_token(TokenKind::Keyword(Keyword::Use))?;
        let use_tree = self.parse_use_tree()?;

        Ok(ItemUse {
            items: PathNode::flat(use_tree),
        })
    }

    pub fn parse_use_tree(&mut self) -> Result<Vec<PathNode>, ParseError> {
        self.separated_list(Symbol::PathSep, Parser::parse_node)
    }

    pub fn parse_node(&mut self) -> Result<PathNode, ParseError> {
        let tok = self.peek_token()?;
        match tok.kind {
            TokenKind::Symbol(Symbol::LBrace) => {
                self.consume_token()?;
                let tree = self.separated_list(Symbol::Comma, Parser::parse_use_tree)?;
                self.expect_token(TokenKind::Symbol(Symbol::RBrace))?;
                Ok(PathNode::Tree(tree))
            }
            _ => {
                let seg = self.parse_path_segment()?;

                let alias = if self.next_token(&TokenKind::Keyword(Keyword::As)) {
                    Some(self.parse_ident()?)
                } else {
                    None
                };

                Ok(PathNode::Seg((seg, alias)))
            }
        }
    }

    pub fn parse_fn_item(&mut self) -> Result<ItemFn, ParseError> {
        let sig = self.parse_fn_signature()?;
        let block = self.parse_block()?;

        Ok(ItemFn {
            vis: Visibility::Pub,
            sig,
            block,
        })
    }

    pub fn parse_fn_signature(&mut self) -> Result<Signature, ParseError> {
        self.expect_token(TokenKind::Keyword(Keyword::Fn))?;

        let name = self.parse_ident()?;

        self.expect_token(TokenKind::Symbol(Symbol::LParen))?;

        let inputs = self.separated_list0(Symbol::Comma, Parser::parse_fn_arg, Symbol::RParen)?;

        self.expect_token(TokenKind::Symbol(Symbol::RParen))?;

        let output = if self.next_token(&TokenKind::Symbol(Symbol::RArrow)) {
            let ty = self.parse_type()?;
            Some(ty)
        } else {
            None
        };

        Ok(Signature {
            name,
            inputs,
            output,
        })
    }

    pub fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect_token(TokenKind::Symbol(Symbol::LBrace))?;

        let mut stmts = Vec::new();

        loop {
            let tok = self.peek_token()?;

            match tok.kind {
                TokenKind::Keyword(Keyword::Let) => {
                    let local = self.parse_let_stmt()?;
                    stmts.push(Statement::Let(local));
                }
                TokenKind::Keyword(Keyword::Use) => {
                    let item = self.parse_use_stmt()?;
                    self.expect_token(TokenKind::Symbol(Symbol::Semicolon))?;
                    stmts.push(Statement::Item(Item::Use(item)));
                }
                TokenKind::Keyword(Keyword::Struct) => {
                    let item = self.parse_struct_item()?;
                    stmts.push(Statement::Item(Item::Struct(item)));
                }
                TokenKind::Keyword(Keyword::Fn) => {
                    let item = self.parse_fn_item()?;
                    stmts.push(Statement::Item(Item::Fn(item)));
                }
                TokenKind::Symbol(Symbol::Semicolon) => {
                    stmts.push(Statement::Empty);
                }
                TokenKind::Symbol(Symbol::RBrace) => {
                    break;
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if self.next_token(&TokenKind::Symbol(Symbol::Semicolon)) {
                        stmts.push(Statement::Semi(expr));
                    } else {
                        stmts.push(Statement::Expr(expr));
                    }
                }
            }
        }

        self.expect_token(TokenKind::Symbol(Symbol::RBrace))?;

        Ok(Block { stmts })
    }

    fn parse_fn_arg(&mut self) -> Result<FnArg, ParseError> {
        let tok = self.peek_token()?;

        Ok(match tok.kind {
            TokenKind::Keyword(Keyword::SelfValue) => {
                self.consume_token()?;
                FnArg::Receiver(Receiver { reference: false })
            }
            TokenKind::Symbol(Symbol::And) => {
                self.consume_token()?;
                self.expect_token(TokenKind::Keyword(Keyword::SelfValue))?;
                FnArg::Receiver(Receiver { reference: true })
            }
            _ => {
                let name = self.parse_ident()?;
                self.expect_token(TokenKind::Symbol(Symbol::Colon))?;
                let ty = self.parse_type()?;
                FnArg::Typed(PatType { name, ty })
            }
        })
    }

    pub fn parse_struct_item(&mut self) -> Result<ItemStruct, ParseError> {
        self.expect_token(TokenKind::Keyword(Keyword::Struct))?;

        let name = self.parse_ident()?;

        self.expect_token(TokenKind::Symbol(Symbol::LBrace))?;

        let fields =
            self.separated_list0(Symbol::Comma, Parser::parse_struct_field, Symbol::RBrace)?;

        self.expect_token(TokenKind::Symbol(Symbol::RBrace))?;

        Ok(ItemStruct { name, fields })
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParseError> {
        let visibility = self.try_visibility().unwrap_or_default();

        let name = self.parse_ident()?;
        self.expect_token(TokenKind::Symbol(Symbol::Colon))?;

        let ty = self.parse_type()?;

        Ok(StructField {
            visibility,
            name,
            ty,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let tok = self.peek_token()?;

        Ok(match tok.kind {
            TokenKind::Keyword(kw) => {
                self.consume_token()?;
                match kw {
                    Keyword::Bool => Type::Primitive(PrimitiveTy::Bool),
                    Keyword::Byte => Type::Primitive(PrimitiveTy::Byte),
                    Keyword::Char => Type::Primitive(PrimitiveTy::Char),
                    Keyword::Int => Type::Primitive(PrimitiveTy::Int),
                    Keyword::Float => Type::Primitive(PrimitiveTy::Float),
                    Keyword::Str => Type::Primitive(PrimitiveTy::Str),
                    _ => return Err(ParseError::unexpect("type", &tok)),
                }
            }
            TokenKind::Symbol(Symbol::And) => {
                self.consume_token()?;
                let ty = self.parse_type()?;
                Type::Reference(Box::new(ty))
            }
            TokenKind::Symbol(Symbol::LBracket) => {
                self.consume_token()?;
                let ty = self.parse_type()?;
                self.expect_token(TokenKind::Symbol(Symbol::Semicolon))?;
                let len = Box::new(self.parse_expr()?);
                self.expect_token(TokenKind::Symbol(Symbol::RBracket))?;

                Type::Array(TypeArray {
                    elem: Box::new(ty),
                    len,
                })
            }
            _ => Type::Path(self.parse_type_path()?),
        })
    }

    fn parse_let_stmt(&mut self) -> Result<LetStmt, ParseError> {
        self.expect_token(TokenKind::Keyword(Keyword::Let))?;

        let var = self.parse_ident()?;

        let ty = if self.next_token(&TokenKind::Symbol(Symbol::Colon)) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let expr = if self.next_token(&TokenKind::Symbol(Symbol::Eq)) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        Ok(LetStmt { var, ty, expr })
    }

    /// reference: https://github.com/sqlparser-rs/sqlparser-rs/blob/main/src/parser.rs
    /// reference: https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_subexpr(Precedence::Lowest)
    }

    fn parse_subexpr(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        debug!("parsing expr");

        let mut expr = self.parse_prefix()?;
        debug!("prefix: {expr:?}");

        loop {
            let next_precedence = self.next_precedence()?;
            debug!("next precedence: {next_precedence:?}");

            if precedence >= next_precedence {
                break;
            }

            expr = self.parse_infix(expr, next_precedence)?;
        }

        Ok(expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.try_prefixop() {
            Some(op) => Ok(Expr::PrefixOp(PrefixOpExpr {
                op,
                rhs: Box::new(self.parse_subexpr(Precedence::Prefix)?),
            })),
            None => self.parse_primary(),
        }
    }

    fn parse_infix(&mut self, expr: Expr, precedence: Precedence) -> Result<Expr, ParseError> {
        let tok = self.consume_token()?;

        debug!("parse_infix, first token {tok:?}");

        match tok.kind {
            TokenKind::Symbol(sym) => match sym {
                Symbol::LParen => {
                    let args: Vec<Expr> =
                        self.separated_list0(Symbol::Comma, Parser::parse_expr, Symbol::RParen)?;
                    self.expect_token(TokenKind::Symbol(Symbol::RParen))?;
                    Ok(Expr::FuncCall(FuncCallExpr {
                        name: Box::new(expr),
                        args,
                    }))
                }
                Symbol::LBracket => {
                    let index = self.parse_expr()?;
                    self.expect_token(TokenKind::Symbol(Symbol::RBracket))?;
                    Ok(Expr::Index(IndexExpr {
                        name: Box::new(expr),
                        rhs: Box::new(index),
                    }))
                }
                Symbol::Question => Ok(Expr::PostfixOp(PostfixOpExpr {
                    op: PostfixOp::Try,
                    lhs: Box::new(expr),
                })),
                _ => {
                    if let Ok(op) = BinOp::from_symbol(sym) {
                        Ok(Expr::BinOp(BinOpExpr {
                            op,
                            lhs: Box::new(expr),
                            rhs: Box::new(self.parse_subexpr(precedence)?),
                        }))
                    } else {
                        unreachable!()
                    }
                }
            },
            _ => {
                unreachable!()
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek_token()?;

        match tok.kind {
            TokenKind::Literal(lit) => {
                self.consume_token()?;
                Ok(Expr::Literal(lit.into()))
            }
            TokenKind::Ident(ident) => {
                self.consume_token()?;
                Ok(Expr::Ident(ident.into()))
            }
            TokenKind::Symbol(Symbol::LParen) => {
                self.consume_token()?;
                let expr = self.parse_subexpr(Precedence::Lowest)?;
                self.expect_token(TokenKind::Symbol(Symbol::RParen))?;
                Ok(expr)
            }
            TokenKind::Symbol(Symbol::LBracket) => {
                self.consume_token()?;
                let items =
                    self.separated_list0(Symbol::Comma, Parser::parse_expr, Symbol::RBracket)?;
                self.expect_token(TokenKind::Symbol(Symbol::RBracket))?;
                Ok(Expr::Array(ArrayExpr { elems: items }))
            }
            TokenKind::Symbol(Symbol::LBrace) => self.parse_expr_block().map(Expr::Block),
            TokenKind::Keyword(Keyword::If) => self.parse_expr_if().map(Expr::If),

            _ => Err(ParseError::unexpect("primary", &tok)),
        }
    }

    fn parse_expr_if(&mut self) -> Result<ExprIf, ParseError> {
        self.expect_token(TokenKind::Keyword(Keyword::If))?;
        let cond = Box::new(self.parse_expr()?);
        let then_branch = self.parse_block()?;
        let else_branch = if self.next_token(&TokenKind::Keyword(Keyword::Else)) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        Ok(ExprIf {
            cond,
            then_branch,
            else_branch,
        })
    }

    fn parse_expr_block(&mut self) -> Result<ExprBlock, ParseError> {
        let block = self.parse_block()?;

        Ok(ExprBlock { block })
    }

    fn next_precedence(&mut self) -> Result<Precedence, ParseError> {
        let tok = self.input.clone().next_token();

        debug!("next_precedence() {tok:?}");

        let p = match tok.map(|t| t.kind) {
            Some(TokenKind::Symbol(sym)) => match sym {
                Symbol::Plus | Symbol::Minus => Precedence::Term,
                Symbol::Star | Symbol::Slash | Symbol::Percent => Precedence::Factor,
                Symbol::LParen | Symbol::LBracket => Precedence::Call,
                Symbol::Dot => Precedence::Call,
                Symbol::Eq => Precedence::Equal,
                Symbol::Gt | Symbol::GtE | Symbol::Lt | Symbol::LtE | Symbol::EqEq => {
                    Precedence::Compare
                }
                Symbol::Question => Precedence::Postfix,
                Symbol::PathSep => Precedence::Path,
                _ => Precedence::Lowest,
            },
            _ => Precedence::Lowest,
        };

        Ok(p)
    }

    fn parse_type_path(&mut self) -> Result<TypePath, ParseError> {
        let path = self.separated_list(Symbol::PathSep, Parser::parse_path_segment)?;

        Ok(TypePath { path })
    }

    fn parse_path_segment(&mut self) -> Result<PathSegment, ParseError> {
        let tok = self.consume_token()?;

        match tok.kind {
            TokenKind::Ident(ident) => Ok(PathSegment::Ident(ident)),
            TokenKind::Keyword(Keyword::Super) => Ok(PathSegment::PathSuper),
            TokenKind::Keyword(Keyword::SelfValue) => Ok(PathSegment::PathSelf),
            TokenKind::Keyword(Keyword::Crate) => Ok(PathSegment::PathCrate),
            _ => Err(ParseError::unexpect("PathSegment", &tok)),
        }
    }

    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let tok = self.consume_token()?;

        match tok.kind {
            TokenKind::Ident(ident) => Ok(ident),
            _ => Err(ParseError::unexpect("ident", &tok)),
        }
    }

    fn try_prefixop(&mut self) -> Option<PrefixOp> {
        self.try_next(|tok| match tok {
            TokenKind::Symbol(sym) => PrefixOp::from_symbol(sym).ok(),
            _ => None,
        })
    }

    fn try_binop(&mut self) -> Option<BinOp> {
        self.try_next(|tok| match tok {
            TokenKind::Symbol(sym) => BinOp::from_symbol(sym).ok(),
            _ => None,
        })
    }

    /// Look for visibility and consume it if it exists
    fn try_visibility(&mut self) -> Option<Visibility> {
        self.try_next(|tok| match tok {
            TokenKind::Keyword(kw) => match kw {
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
            TokenKind::Keyword(kw) => match kw {
                Keyword::Bool => Some(PrimitiveTy::Bool),
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
        self.next_token(&TokenKind::Symbol(expected))
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
        terminated: Symbol,
    ) -> Result<Vec<T>, ParseError>
    where
        F: Fn(&mut Parser) -> Result<T, ParseError>,
    {
        let mut values = Vec::new();
        loop {
            if self.test_next(&TokenKind::Symbol(terminated)) {
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
    fn test_next(&mut self, expected: &TokenKind) -> bool {
        match self.peek_token() {
            Ok(tok) => &tok.kind == expected,
            _ => false,
        }
    }

    /// Check next token, consume it if ok
    fn try_next<T, F>(&mut self, f: F) -> Option<T>
    where
        F: Fn(TokenKind) -> Option<T>,
    {
        match self.peek_token() {
            Ok(tok) => f(tok.kind).map(|t| {
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
    fn expect_token(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        let tok = self.consume_token()?;
        if tok.kind == kind {
            Ok(tok)
        } else {
            Err(ParseError::unexpect(kind, &tok))
        }
    }

    /// Peek next token without cunsume it
    fn peek_token(&self) -> Result<Token, ParseError> {
        self.input.clone().next_token().ok_or(ParseError::eof())
    }

    /// Consume the next token if it matches the expected token, otherwise return false
    #[must_use]
    fn next_token(&mut self, expected: &TokenKind) -> bool {
        match self.peek_token() {
            Ok(tok) if tok.kind == *expected => {
                self.input.next_token().unwrap();
                true
            }
            _ => false,
        }
    }

    fn is_eof(&self) -> bool {
        self.input.clone().next_token().is_none()
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{Parser, PathNode};

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
            let mut parser = Parser::new(input.0);

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
            let mut parser = Parser::new(input.0);

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
            (
                r"struct A { 
                    a: int,
                    pub b: float,
                    priv c: AAA,
                    priv d: std::json,
                    pub e: &std::json,
                    pub f: [int; 2],
                    pub g: &[float; 4]
                }",
                true,
            ),
        ];

        for input in inputs {
            let mut parser = Parser::new(input.0);

            println!("=> {:?}", parser.parse_struct_item());
        }
    }

    #[test]
    fn parse_expr() {
        let inputs = [
            "a+b",
            "a+b*c",
            "a*b+c",
            "(a+b)*c",
            "a(b,c)",
            "a()",
            "a(b+c, d*e)",
            "a[b]",
            "a.b.c(d)",
            "a.b[c]",
            "a+b>c",
            "a+b<=c*d",
            "-a+b*c",
            "&a*b-c",
            "*a.b+c",
            "a()?",
            "a.b?",
            "-a.b?",
            "a=b",
        ];

        for input in &inputs {
            let mut parser = Parser::new(input);

            println!("=> {:?}", parser.parse_expr());
        }
    }

    #[test]
    fn test_parse_let_stmt() {
        let inputs = [
            ("let a = a * b"),
            ("let a : int = a * b"),
            ("let a = [a, b[c], d(), e(f)]"),
            ("let a"),
        ];

        for input in &inputs {
            let mut parser = Parser::new(input);

            let ret = parser.parse_let_stmt();

            println!("=> {ret:?}");
        }
    }

    #[test]
    fn test_parse() {
        use std::fs;

        env_logger::builder()
            .filter_level(log::LevelFilter::Debug)
            .init();

        let content = fs::read_to_string("scripts/hello.lie").unwrap();

        let mut parser = Parser::new(&content);

        let ret = parser.parse_top_level();

        println!("=> {ret:?}");

        ret.unwrap().print();
    }
}
