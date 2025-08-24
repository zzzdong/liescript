use std::{borrow::Cow, iter::Peekable};

use crate::{
    ast::{
        Arm, BareFnArg, Block, Expr, ExprArray, ExprAssign, ExprBinary, ExprBlock, ExprBreak,
        ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprGroup, ExprIf,
        ExprIndex, ExprInfer, ExprLit, ExprLoop, ExprMatch, ExprMethodCall, ExprPath, ExprRange,
        ExprReference, ExprRepeat, ExprReturn, ExprSpan, ExprStruct, ExprTry, ExprTuple, ExprUnary,
        ExprWhile, Field, FieldPat, FieldValue, Fields, FieldsNamed, FieldsUnnamed, FnArg,
        IdentSpan, ImplItem, ImplItemConst, ImplItemFn, Item, ItemEnum, ItemFn, ItemImpl,
        ItemStruct, ItemType, ItemUse, Label, LetStmt, LiteralSpan, Pat, PatRest, PatSpan,
        PatStruct, PatTuple, PatType, PatWild, Path, PathSegment, RangeLimits, Receiver,
        RetureType, Signature, Stmt, StmtSpan, Type, TypeAny, TypeArray, TypeBareFn, TypeInfer,
        TypeNever, TypeParamBound, TypeParen, TypeReference, TypeSlice, TypeSpan, TypeTraitObject,
        TypeTuple, UseGlob, UseGroup, UseName, UsePath, UseRename, UseTree, Variant, Visibility,
        ident::Identifier,
        keyword::Keyword,
        literal::Literal,
        op::{BinOp, UnOp},
        precedence::Precedence,
        symbol::Symbol,
        token::{Brace, Bracket, Paren, Punctuated, Token, TokenSpan},
    },
    diagnostic::{Span, Spanned},
    parser::tokenizer::{TokenStream, Tokenizer},
};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    message: String,
    span: Option<Span>,
    expected: Vec<String>,
    found: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>) -> Self {
        ParseError {
            message: message.into(),
            span: None,
            expected: Vec::new(),
            found: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
        self
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn expected(&self) -> &[String] {
        &self.expected
    }

    pub fn found(&self) -> Option<&str> {
        self.found.as_deref()
    }

    pub fn eof() -> Self {
        ParseError::new("unexpected end of input")
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;

        if !self.expected.is_empty() {
            write!(f, ", expected {}", self.expected.join(", "))?;
        }

        if let Some(found) = &self.found {
            write!(f, ", but found {}", found)?;
        }

        Ok(())
    }
}

impl std::error::Error for ParseError {}

pub struct ParseStream<'i> {
    iter: Peekable<std::slice::Iter<'i, TokenSpan>>,
}

impl<'i> ParseStream<'i> {
    pub fn new(token_stream: &'i TokenStream) -> Result<Self, ParseError> {
        Ok(ParseStream {
            iter: token_stream.iter().peekable(),
        })
    }

    pub fn lookahead1(&mut self) -> Result<&'i TokenSpan, ParseError> {
        self.iter.peek().map(|v| *v).ok_or(ParseError::eof())
    }

    pub fn next_is(&mut self, token: &Token) -> bool {
        matches!(self.iter.peek(), Some(t) if &t.value == token)
    }

    pub fn consume(&mut self) -> Result<TokenSpan, ParseError> {
        self.iter.next().cloned().ok_or(ParseError::eof())
    }

    pub fn next_is_identifier(&mut self) -> bool {
        matches!(self.iter.peek(), Some(t) if t.value.is_ident())
    }

    pub fn expect_identifier(&mut self) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;

        if let Token::Ident(_) = &tok.value {
            return self.consume();
        }

        Err(ParseError::new("Expected identifier")
            .with_span(tok.span)
            .with_expected("identifier"))
    }

    pub fn expect_symbol(&mut self, symbol: Symbol) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;
        if let Token::Symbol(s) = &tok.value {
            if *s == symbol {
                return self.consume();
            }
        }
        Err(ParseError::new("Expected symbol")
            .with_span(tok.span)
            .with_expected(symbol.as_str()))
    }

    pub fn next_is_symbol(&mut self, symbol: Symbol) -> bool {
        matches!(self.iter.peek(), Some(t) if matches!(&t.value, Token::Symbol(s) if *s == symbol))
    }

    pub fn expect(&mut self, token: &Token) -> Result<TokenSpan, ParseError> {
        let tok = self.lookahead1()?;
        if tok.value == *token {
            return self.consume();
        }

        Err(ParseError::new("Expected token")
            .with_span(tok.span)
            .with_expected(token.to_string()))
    }

    pub fn parse_punctuated<T>(&mut self, sep: &Token) -> Result<Punctuated<T>, ParseError>
    where
        T: Parse,
    {
        let mut items = Vec::new();
        let mut last = None;

        loop {
            match self.try_parse(T::parse) {
                Ok(item) => {
                    if self.next_is(sep) {
                        items.push((item, self.consume()?));
                    } else {
                        last = Some(Box::new(item));
                        break;
                    }
                }
                Err(_) => {
                    break;
                }
            }
        }

        Ok(Punctuated { items, last })
    }

    pub fn parse_optional<T>(&mut self) -> Option<T>
    where
        T: Parse,
    {
        let mut stream = self.fork();
        match T::parse(&mut stream) {
            Ok(value) => {
                self.reset(stream);
                Some(value)
            }
            Err(_) => None,
        }
    }

    /// Try to parse a value, but if it fails, reset the stream to the original state
    pub fn try_parse<T, F>(&mut self, f: F) -> Result<T, ParseError>
    where
        F: FnOnce(&mut ParseStream) -> Result<T, ParseError>,
    {
        let mut stream = self.fork();
        match f(&mut stream) {
            Ok(result) => {
                self.reset(stream);
                Ok(result)
            }
            Err(e) => Err(e),
        }
    }

    pub fn fork(&self) -> ParseStream<'i> {
        ParseStream {
            iter: self.iter.clone(),
        }
    }

    pub fn reset(&mut self, other: ParseStream<'i>) {
        self.iter = other.iter;
    }
}

pub trait Parse
where
    Self: Sized,
{
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError>;
}

impl<T: Parse> Parse for Spanned<T> {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let start_token = stream.lookahead1()?;
        let value = T::parse(stream)?;
        let end_token = stream.lookahead1().map(|t| t).unwrap_or(start_token);

        Ok(Spanned {
            value,
            span: Span {
                start: start_token.span.start,
                end: end_token.span.end,
            },
        })
    }
}

impl Parse for Literal {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        match &tok.value {
            Token::Literal(lit) => {
                let _ = stream.consume()?;
                Ok(lit.clone())
            }
            _ => Err(ParseError::new("Expected literal value")
                .with_span(tok.span)
                .with_expected("number, string or boolean literal")
                .with_found(format!("{:?}", tok.value))),
        }
    }
}

impl Parse for Identifier {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_ident() {
            return Err(ParseError::new("Expected identifier")
                .with_span(tok.span)
                .with_expected("valid identifier")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.value.into_ident())
    }
}

impl Parse for Keyword {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_keyword() {
            return Err(ParseError::new("Expected keyword")
                .with_span(tok.span)
                .with_expected("valid keyword")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.value.into_keyword())
    }
}

impl Parse for Symbol {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.consume()?;

        if !tok.is_symbol() {
            return Err(ParseError::new("Expected symbol")
                .with_span(tok.span)
                .with_expected("valid symbol")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.value.into_symbol())
    }
}

impl Parse for ExprLit {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let lit = LiteralSpan::parse(stream)?;
        Ok(ExprLit { lit })
    }
}

impl Parse for ExprInfer {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let underscore_token = stream.expect_symbol(Symbol::Underscore)?;
        Ok(ExprInfer { underscore_token })
    }
}

impl Parse for ExprPath {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let path = Path::parse(stream)?;
        Ok(ExprPath { path })
    }
}

impl Parse for ExprGroup {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(ExprGroup {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            expr,
        })
    }
}

impl Parse for ExprArray {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let elems = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RBracket)?;

        Ok(ExprArray {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            elems,
        })
    }
}

impl Parse for ExprTuple {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let elems = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(ExprTuple {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            elems,
        })
    }
}

impl Parse for ExprUnary {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let op = Spanned::<UnOp>::parse(stream)?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        Ok(ExprUnary { op, expr })
    }
}

impl Parse for ExprBinary {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let lhs = Box::new(ExprSpan::parse(stream)?);
        let op = Spanned::<BinOp>::parse(stream)?;
        let rhs = Box::new(ExprSpan::parse(stream)?);
        Ok(ExprBinary { op, lhs, rhs })
    }
}

impl Parse for ExprAssign {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let left = Box::new(ExprSpan::parse(stream)?);
        let eq_token = stream.expect_symbol(Symbol::Equal)?;
        let right = Box::new(ExprSpan::parse(stream)?);
        Ok(ExprAssign {
            left,
            eq_token,
            right,
        })
    }
}

impl Parse for ExprField {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let expr = Box::new(ExprSpan::parse(stream)?);
        let dot_token = stream.expect_symbol(Symbol::Dot)?;
        let member = IdentSpan::parse(stream)?;
        Ok(ExprField {
            expr,
            dot_token,
            member,
        })
    }
}

impl Parse for ExprIndex {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let expr = Box::new(ExprSpan::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let index = Box::new(ExprSpan::parse(stream)?);
        let close_token = stream.expect_symbol(Symbol::RBracket)?;

        Ok(ExprIndex {
            expr,
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            index,
        })
    }
}

impl Parse for ExprCall {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let func = Box::new(ExprSpan::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(ExprCall {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            func,
            args,
        })
    }
}

impl Parse for ExprMethodCall {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let receiver = Box::new(ExprSpan::parse(stream)?);
        let method = IdentSpan::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(ExprMethodCall {
            receiver,
            method,
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            args,
        })
    }
}

impl Parse for ExprCast {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let expr = Box::new(ExprSpan::parse(stream)?);
        let as_token = stream.expect(&Token::Keyword(Keyword::As))?;
        let ty = Box::new(TypeSpan::parse(stream)?);
        Ok(ExprCast { expr, as_token, ty })
    }
}

impl Parse for ExprTry {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let expr = Box::new(ExprSpan::parse(stream)?);
        let question_token = stream.expect_symbol(Symbol::Question)?;
        Ok(ExprTry {
            expr,
            question_token,
        })
    }
}

impl Parse for ExprReference {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let and_token = stream.expect_symbol(Symbol::And)?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        Ok(ExprReference { and_token, expr })
    }
}

impl Parse for ExprRepeat {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        let semicolon_token = stream.expect_symbol(Symbol::Semicolon)?;
        let len = Box::new(ExprSpan::parse(stream)?);
        let close_token = stream.expect_symbol(Symbol::RBracket)?;

        Ok(ExprRepeat {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            expr,
            semicolon_token,
            len,
        })
    }
}

impl Parse for ExprRange {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let start = if stream.next_is_symbol(Symbol::DotDot)
            || stream.next_is_symbol(Symbol::DotDotEqual)
        {
            None
        } else {
            Some(Box::new(ExprSpan::parse(stream)?))
        };

        let limits = if stream.next_is_symbol(Symbol::DotDot) {
            stream.consume()?;
            Spanned::new(RangeLimits::HalfOpen, stream.lookahead1()?.span)
        } else if stream.next_is_symbol(Symbol::DotDotEqual) {
            stream.consume()?;
            Spanned::new(RangeLimits::Closed, stream.lookahead1()?.span)
        } else {
            return Err(ParseError::new("Expected range operator (.. or ..=)"));
        };

        let end = if stream.next_is_symbol(Symbol::Semicolon)
            || stream.next_is_symbol(Symbol::Comma)
            || stream.next_is_symbol(Symbol::RParen)
        {
            None
        } else {
            Some(Box::new(ExprSpan::parse(stream)?))
        };

        Ok(ExprRange { start, end, limits })
    }
}

impl Parse for ExprBlock {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = stream.parse_optional();
        let block = Block::parse(stream)?;
        Ok(ExprBlock { label, block })
    }
}

impl Parse for ExprIf {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let if_token = stream.expect(&Token::Keyword(Keyword::If))?;
        let cond = Box::new(ExprSpan::parse(stream)?);
        let then_branch = Block::parse(stream)?;

        let else_branch = if stream.next_is(&Token::Keyword(Keyword::Else)) {
            stream.consume()?;
            Some(Box::new(ExprSpan::parse(stream)?))
        } else {
            None
        };

        Ok(ExprIf {
            if_token,
            cond,
            then_branch,
            else_branch,
        })
    }
}

impl Parse for ExprWhile {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = stream.parse_optional();
        let while_token = stream.expect(&Token::Keyword(Keyword::While))?;
        let cond = Box::new(ExprSpan::parse(stream)?);
        let body = Block::parse(stream)?;
        Ok(ExprWhile {
            label,
            while_token,
            cond,
            body,
        })
    }
}

impl Parse for ExprLoop {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = stream.parse_optional();
        let loop_token = stream.expect(&Token::Keyword(Keyword::Loop))?;
        let body = Block::parse(stream)?;
        Ok(ExprLoop {
            label,
            loop_token,
            body,
        })
    }
}

impl Parse for ExprForLoop {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = stream.parse_optional();
        let for_token = stream.expect(&Token::Keyword(Keyword::For))?;
        let pat = Box::new(PatSpan::parse(stream)?);
        let in_token = stream.expect(&Token::Keyword(Keyword::In))?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        let body = Block::parse(stream)?;
        Ok(ExprForLoop {
            label,
            for_token,
            pat,
            in_token,
            expr,
            body,
        })
    }
}

impl Parse for ExprBreak {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let break_token = stream.expect(&Token::Keyword(Keyword::Break))?;
        let label = if stream.next_is_identifier() {
            Some(Label::parse(stream)?)
        } else {
            None
        };
        let expr = if !stream.next_is_symbol(Symbol::Semicolon) {
            Some(Box::new(ExprSpan::parse(stream)?))
        } else {
            None
        };
        Ok(ExprBreak {
            break_token,
            label,
            expr,
        })
    }
}

impl Parse for ExprContinue {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let continue_token = stream.expect(&Token::Keyword(Keyword::Continue))?;
        let label = if stream.next_is_identifier() {
            Some(Label::parse(stream)?)
        } else {
            None
        };
        Ok(ExprContinue {
            continue_token,
            label,
        })
    }
}

impl Parse for ExprReturn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let return_token = stream.expect(&Token::Keyword(Keyword::Return))?;
        let expr = if !stream.next_is_symbol(Symbol::Semicolon) {
            Some(Box::new(ExprSpan::parse(stream)?))
        } else {
            None
        };
        Ok(ExprReturn { return_token, expr })
    }
}

impl Parse for ExprClosure {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let or1_token = stream.expect_symbol(Symbol::Or)?;

        let inputs = if !stream.next_is_symbol(Symbol::Or) {
            let punctuated = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
            let mut items = Vec::with_capacity(
                punctuated.items.len() + if punctuated.last.is_some() { 1 } else { 0 },
            );
            for (item, _) in punctuated.items {
                items.push(item);
            }
            if let Some(item) = punctuated.last {
                items.push(*item);
            }
            items
        } else {
            Vec::new()
        };

        let or2_token = stream.expect_symbol(Symbol::Or)?;

        let output = if stream.next_is_symbol(Symbol::RArrow) {
            let arrow_token = stream.expect_symbol(Symbol::RArrow)?;
            RetureType::Typed(arrow_token, Box::new(TypeSpan::parse(stream)?))
        } else {
            RetureType::Default
        };

        let body = Box::new(ExprSpan::parse(stream)?);
        Ok(ExprClosure {
            or1_token,
            inputs,
            or2_token,
            output,
            body,
        })
    }
}

impl Parse for ExprMatch {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let match_token = stream.expect(&Token::Keyword(Keyword::Match))?;
        let expr = Box::new(ExprSpan::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut arms = Vec::new();
        while !stream.next_is_symbol(Symbol::RBrace) {
            let pat = Box::new(PatSpan::parse(stream)?);

            let guard = if stream.next_is(&Token::Keyword(Keyword::If)) {
                stream.consume()?;
                Some(Box::new(ExprSpan::parse(stream)?))
            } else {
                None
            };

            let arrow_token = stream.expect_symbol(Symbol::FatArrow)?;
            let body = Box::new(ExprSpan::parse(stream)?);

            let comma = stream.next_is_symbol(Symbol::Comma);
            if comma {
                stream.consume()?;
            }

            arms.push(Arm {
                pat,
                guard,
                body,
                comma,
            });
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;
        Ok(ExprMatch {
            match_token,
            expr,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            arms,
        })
    }
}

impl Parse for ExprStruct {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let path = Path::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut fields = Punctuated::new();
        let mut dot2_token = None;
        let mut rest = None;

        while !stream.next_is_symbol(Symbol::RBrace) {
            if stream.next_is(&Token::Symbol(Symbol::DotDot)) {
                dot2_token = Some(stream.consume()?);
                rest = Some(IdentSpan::parse(stream)?);
                break;
            }

            let member = IdentSpan::parse(stream)?;
            let colon_token = stream.expect(&Token::Symbol(Symbol::Colon))?;
            let expr = Box::new(ExprSpan::parse(stream)?);

            let comma = stream.next_is_symbol(Symbol::Comma);
            if comma {
                stream.consume()?;
            }

            fields
                .items
                .push((FieldValue { member, expr }, colon_token));
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;
        Ok(ExprStruct {
            path,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            fields,
            dot2_token,
            rest,
        })
    }
}

/// 解析表达式 (使用Pratt parser算法)
fn parse_expr(stream: &mut ParseStream, prec: Precedence) -> Result<ExprSpan, ParseError> {
    let mut expr = parse_prefix(stream)?;

    loop {
        let next_prec = get_next_precedence(stream)?;
        if next_prec < prec {
            break;
        }

        expr = parse_infix(stream, expr, next_prec)?;
    }

    Ok(expr)
}

/// 解析主表达式 (字面量、标识符、括号表达式等)
fn parse_primary(stream: &mut ParseStream) -> Result<ExprSpan, ParseError> {
    let start_token = stream.lookahead1()?;

    let expr = match &start_token.value {
        // 字面量
        Token::Literal(_) => Expr::Lit(ExprLit::parse(stream)?),

        // 标识符
        Token::Ident(_) => Expr::Path(ExprPath::parse(stream)?),

        // 下划线
        Token::Symbol(Symbol::Underscore) => Expr::Infer(ExprInfer::parse(stream)?),

        // 括号表达式
        Token::Symbol(Symbol::LParen) => Expr::Group(ExprGroup::parse(stream)?),

        // 数组表达式
        Token::Symbol(Symbol::LBracket) => Expr::Array(ExprArray::parse(stream)?),

        // 控制流表达式
        Token::Keyword(Keyword::If) => Expr::If(ExprIf::parse(stream)?),
        Token::Keyword(Keyword::While) => Expr::While(ExprWhile::parse(stream)?),
        Token::Keyword(Keyword::Loop) => Expr::Loop(ExprLoop::parse(stream)?),
        Token::Keyword(Keyword::For) => Expr::ForLoop(ExprForLoop::parse(stream)?),
        Token::Keyword(Keyword::Break) => Expr::Break(ExprBreak::parse(stream)?),
        Token::Keyword(Keyword::Continue) => Expr::Continue(ExprContinue::parse(stream)?),
        Token::Keyword(Keyword::Return) => Expr::Return(ExprReturn::parse(stream)?),
        Token::Keyword(Keyword::Match) => Expr::Match(ExprMatch::parse(stream)?),

        // 闭包表达式
        Token::Symbol(Symbol::Or) => Expr::Closure(ExprClosure::parse(stream)?),

        _ => {
            return Err(ParseError::new("Expected primary expression")
                .with_span(start_token.span)
                .with_expected("valid primary expression"));
        }
    };

    let end_token = stream.lookahead1().unwrap_or(start_token);
    Ok(Spanned::new(
        expr,
        Span::new(start_token.span.start, end_token.span.end),
    ))
}

/// 解析前缀表达式
fn parse_prefix(stream: &mut ParseStream) -> Result<ExprSpan, ParseError> {
    let start_token = stream.lookahead1()?;
    let start_span = start_token.span;

    let expr = match &start_token.value {
        // 引用表达式
        Token::Symbol(Symbol::And) => {
            let and_token = stream.consume()?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary)?);
            let span = start_span.join(expr.span());
            Spanned::new(Expr::Reference(ExprReference { and_token, expr }), span)
        }

        // 一元运算符
        Token::Symbol(Symbol::Not) | Token::Symbol(Symbol::Minus) => {
            let op = Spanned::<UnOp>::parse(stream)?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary)?);
            let span = start_span.join(expr.span());
            Spanned::new(Expr::Unary(ExprUnary { op, expr }), span)
        }

        // 主表达式
        _ => parse_primary(stream)?,
    };

    Ok(expr)
}

/// 获取下一个运算符的优先级
fn get_next_precedence(stream: &mut ParseStream) -> Result<Precedence, ParseError> {
    let token = match stream.lookahead1() {
        Ok(t) => t,
        Err(_) => return Ok(Precedence::None),
    };

    match &token.value {
        // 二元运算符
        Token::Symbol(sym) if BinOp::from_symbol(*sym).is_ok() => {
            let op = BinOp::from_symbol(*sym).unwrap();
            Ok(Precedence::for_binop(&op))
        }

        // 赋值运算符
        Token::Symbol(Symbol::Equal) => Ok(Precedence::Assignment),

        // 范围运算符
        Token::Symbol(Symbol::DotDot) | Token::Symbol(Symbol::DotDotEqual) => Ok(Precedence::Range),

        // 调用表达式
        Token::Symbol(Symbol::LParen) | Token::Symbol(Symbol::LBracket) => Ok(Precedence::Call),

        // 成员访问
        Token::Symbol(Symbol::Dot) => Ok(Precedence::Call),

        // 问号运算符
        Token::Symbol(Symbol::Question) => Ok(Precedence::Call),

        _ => Ok(Precedence::None),
    }
}

/// 解析中缀表达式
fn parse_infix(
    stream: &mut ParseStream,
    left: ExprSpan,
    prec: Precedence,
) -> Result<ExprSpan, ParseError> {
    let start_token = stream.lookahead1()?;
    let start_span = left.span();

    let expr = match &start_token.value {
        // 赋值表达式
        Token::Symbol(Symbol::Equal) => {
            let eq_token = stream.consume()?;
            let rhs = Box::new(parse_expr(stream, Precedence::None)?);
            Expr::Assign(ExprAssign {
                left: Box::new(left),
                eq_token,
                right: rhs,
            })
        }

        // 二元运算符
        Token::Symbol(sym) if BinOp::from_symbol(*sym).is_ok() => {
            let op = Spanned::<BinOp>::parse(stream)?;
            let rhs = Box::new(parse_expr(stream, prec)?);
            Expr::Binary(ExprBinary {
                op,
                lhs: Box::new(left),
                rhs,
            })
        }

        // 极调用表达式
        Token::Symbol(Symbol::LParen) => {
            let open_token = stream.consume()?;
            let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
            let close_token = stream.expect_symbol(Symbol::RParen)?;

            Expr::Call(ExprCall {
                paren_token: Paren {
                    open: open_token,
                    close: close_token,
                },
                func: Box::new(left),
                args,
            })
        }

        // 数组索引
        Token::Symbol(Symbol::LBracket) => {
            let open_token = stream.consume()?;
            let index = Box::new(parse_expr(stream, Precedence::Primary)?);
            let close_token = stream.expect_symbol(Symbol::RBracket)?;

            Expr::Index(ExprIndex {
                expr: Box::new(left),
                bracket_token: Bracket {
                    open: open_token,
                    close: close_token,
                },
                index,
            })
        }

        // 成员访问
        Token::Symbol(Symbol::Dot) => {
            let dot_token = stream.consume()?;
            let member = IdentSpan::parse(stream)?;
            Expr::Field(ExprField {
                expr: Box::new(left),
                dot_token,
                member,
            })
        }

        // 范围表达式
        Token::Symbol(Symbol::DotDot) | Token::Symbol(Symbol::DotDotEqual) => {
            let limits = Spanned::<RangeLimits>::parse(stream)?;
            let end = if stream.next_is_symbol(Symbol::Semicolon)
                || stream.next_is_symbol(Symbol::Comma)
                || stream.next_is_symbol(Symbol::RParen)
            {
                None
            } else {
                Some(Box::new(parse_expr(stream, Precedence::Range - 1)?))
            };

            Expr::Range(ExprRange {
                start: Some(Box::new(left)),
                end,
                limits,
            })
        }

        // 问号运算符
        Token::Symbol(Symbol::Question) => {
            let question_token = stream.consume()?;
            Expr::Try(ExprTry {
                expr: Box::new(left),
                question_token,
            })
        }

        _ => {
            return Err(ParseError::new("Expected infix operator")
                .with_span(start_token.span)
                .with_expected("binary operator, call, index or field access"));
        }
    };

    let span = start_span.join(expr.span());
    Ok(Spanned::new(
        expr,
        span,
    ))
}

impl Parse for ExprSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_expr(stream, Precedence::None)
    }
}

// 辅助类型的 Parse 实现
impl Parse for Path {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let leading_colon = if stream.next_is_symbol(Symbol::ColonColon) {
            Some(stream.consume()?)
        } else {
            None
        };

        let segments = stream.parse_punctuated(&Token::Symbol(Symbol::ColonColon))?;
        Ok(Path {
            leading_colon,
            segments,
        })
    }
}

impl Parse for PathSegment {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let ident = IdentSpan::parse(stream)?;
        Ok(PathSegment { ident })
    }
}

impl Parse for Label {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        Ok(Label { name, colon_token })
    }
}

impl Parse for Block {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let mut stmts = Vec::new();

        while !stream.next_is_symbol(Symbol::RBrace) {
            stmts.push(StmtSpan::parse(stream)?);
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;
        Ok(Block {
            brac_token: Brace {
                open: open_token,
                close: close_token,
            },
            stmts,
        })
    }
}

impl Parse for RetureType {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        if stream.next_is(&Token::Symbol(Symbol::RArrow)) {
            let arrow_token = stream.expect_symbol(Symbol::RArrow)?;
            let ty = Box::new(TypeSpan::parse(stream)?);
            Ok(RetureType::Typed(arrow_token, ty))
        } else {
            Ok(RetureType::Default)
        }
    }
}

impl Parse for RangeLimits {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        if stream.next_is_symbol(Symbol::DotDot) {
            stream.consume()?;
            Ok(RangeLimits::HalfOpen)
        } else if stream.next_is_symbol(Symbol::DotDotEqual) {
            stream.consume()?;
            Ok(RangeLimits::Closed)
        } else {
            Err(ParseError::new("Expected range operator (.. or ..=)"))
        }
    }
}

impl Parse for UnOp {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;
        match token.value {
            Token::Symbol(Symbol::Not) => Ok(UnOp::Not),
            Token::Symbol(Symbol::Minus) => Ok(UnOp::Neg),
            Token::Symbol(Symbol::And) => Ok(UnOp::Deref),
            _ => Err(ParseError::new("Expected unary operator")
                .with_span(token.span)
                .with_expected("!, -, or &")),
        }
    }
}

impl Parse for BinOp {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;
        match &token.value {
            Token::Symbol(sym) => match BinOp::from_symbol(*sym) {
                Ok(op) => Ok(op),
                Err(_) => Err(ParseError::new("Expected binary operator")
                    .with_span(token.span)
                    .with_expected(
                        "+, -, *, /, %, ==, !=, <, <=, >, >=, &&, ||, &, |, ^, <<, or >>",
                    )
                    .with_found(sym.as_str())),
            },
            _ => Err(ParseError::new("Expected binary operator")
                .with_span(token.span)
                .with_expected("+, -, *, /, %, ==, !=, <, <=, >, >=, &&, ||, &, |, ^, <<, or >>")
                .with_found(format!("{:?}", token.value))),
        }
    }
}

// Pat 相关的 Parse 实现
impl Parse for Pat {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Literal(_) => Ok(Pat::Lit(LiteralSpan::parse(stream)?)),
            Token::Ident(_) => Ok(Pat::Path(Path::parse(stream)?)),
            Token::Symbol(Symbol::Underscore) => Ok(Pat::Wild(PatWild::parse(stream)?)),
            Token::Symbol(Symbol::DotDot) => Ok(Pat::Rest(PatRest::parse(stream)?)),
            _ => Err(ParseError::new("Expected pattern")
                .with_span(token.span)
                .with_expected("literal, identifier, _, or ..")),
        }
    }
}

impl Parse for PatWild {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let underscore_token = stream.expect_symbol(Symbol::Underscore)?;
        Ok(PatWild { underscore_token })
    }
}

impl Parse for PatRest {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let dot2_token = stream.expect_symbol(Symbol::DotDot)?;
        Ok(PatRest { dot2_token })
    }
}

impl Parse for PatStruct {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let path = Path::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut fields = Vec::new();
        let mut rest = None;

        while !stream.next_is_symbol(Symbol::RBrace) {
            if stream.next_is_symbol(Symbol::DotDot) {
                stream.consume()?;
                rest = Some(IdentSpan::parse(stream)?);
                break;
            }

            let member = IdentSpan::parse(stream)?;
            let colon_token = stream.expect_symbol(Symbol::Colon)?;
            let pat = Box::new(PatSpan::parse(stream)?);

            fields.push(FieldPat { member, pat });

            if stream.next_is_symbol(Symbol::Comma) {
                stream.consume()?;
            }
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;
        Ok(PatStruct { path, fields, rest })
    }
}

impl Parse for PatTuple {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let mut elts = Vec::new();

        while !stream.next_is_symbol(Symbol::RParen) {
            elts.push(PatSpan::parse(stream)?);

            if stream.next_is_symbol(Symbol::Comma) {
                stream.consume()?;
            }
        }

        let close_token = stream.expect_symbol(Symbol::RParen)?;
        Ok(PatTuple { elts })
    }
}

impl Parse for PatType {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let pat = Box::new(PatSpan::parse(stream)?);
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Box::new(TypeSpan::parse(stream)?);
        Ok(PatType {
            pat,
            colon_token,
            ty,
        })
    }
}

// Type 相关的 Parse 实现
impl Parse for Type {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Symbol(Symbol::Underscore) => Ok(Type::Infer(TypeInfer::parse(stream)?)),
            Token::Symbol(Symbol::Not) => Ok(Type::Never(TypeNever::parse(stream)?)),
            Token::Symbol(Symbol::LParen) => Ok(Type::Tuple(TypeTuple::parse(stream)?)),
            Token::Symbol(Symbol::LBracket) => {
                if stream.next_is_symbol(Symbol::Semicolon) {
                    Ok(Type::Array(TypeArray::parse(stream)?))
                } else {
                    Ok(Type::Slice(TypeSlice::parse(stream)?))
                }
            }
            Token::Symbol(Symbol::And) => Ok(Type::Reference(TypeReference::parse(stream)?)),
            Token::Ident(_) => Ok(Type::Path(Path::parse(stream)?)),
            _ => Err(ParseError::new("Expected type")
                .with_span(token.span)
                .with_expected("identifier, _, never, (, [, or &")),
        }
    }
}

impl Parse for TypeInfer {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let underscore_token = stream.expect_symbol(Symbol::Underscore)?;
        Ok(TypeInfer { underscore_token })
    }
}

impl Parse for TypeNever {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let never_token = stream.expect_symbol(Symbol::Not)?;
        Ok(TypeNever { never_token })
    }
}

impl Parse for TypeTuple {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let elems = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(TypeTuple {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            elems,
        })
    }
}

impl Parse for TypeArray {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let elem = Box::new(TypeSpan::parse(stream)?);
        let semicolon = stream.expect_symbol(Symbol::Semicolon)?;
        let len = ExprSpan::parse(stream)?;
        let close_token = stream.expect_symbol(Symbol::RBracket)?;

        Ok(TypeArray {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            elem,
            semicolon,
            len,
        })
    }
}

impl Parse for TypeSlice {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let elem = Box::new(TypeSpan::parse(stream)?);
        let close_token = stream.expect_symbol(Symbol::RBracket)?;

        Ok(TypeSlice {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            elem,
        })
    }
}

impl Parse for TypeReference {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let and_token = stream.expect_symbol(Symbol::And)?;
        let elem = Box::new(TypeSpan::parse(stream)?);
        Ok(TypeReference { and_token, elem })
    }
}

impl Parse for TypeParen {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let elem = Box::new(TypeSpan::parse(stream)?);
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(TypeParen {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            elem,
        })
    }
}

impl Parse for TypeBareFn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let inputs = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
        let close_token = stream.expect_symbol(Symbol::RParen)?;
        let output = RetureType::parse(stream)?;

        Ok(TypeBareFn {
            fn_token,
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            inputs,
            output,
        })
    }
}

impl Parse for TypeTraitObject {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let dyn_token = stream.expect(&Token::Keyword(Keyword::Dyn))?;
        let bounds = stream.parse_punctuated(&Token::Symbol(Symbol::Plus))?;

        Ok(TypeTraitObject { dyn_token, bounds })
    }
}

impl Parse for TypeParamBound {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        Ok(TypeParamBound { name })
    }
}

impl Parse for BareFnArg {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = if stream.next_is_identifier() {
            let ident = IdentSpan::parse(stream)?;
            let colon_token = stream.expect_symbol(Symbol::Colon)?;
            Some((ident, colon_token))
        } else {
            None
        };

        let ty = TypeSpan::parse(stream)?;
        Ok(BareFnArg { name, ty })
    }
}

impl Parse for TypeAny {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let any_token = stream.expect(&Token::Keyword(Keyword::Any))?;
        Ok(TypeAny { any_token })
    }
}

// Stmt 相关的 Parse 实现
impl Parse for Stmt {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Keyword(Keyword::Let) => Ok(Stmt::Let(LetStmt::parse(stream)?)),
            Token::Symbol(Symbol::Semicolon) => {
                stream.consume()?;
                Ok(Stmt::Empty)
            }
            _ => {
                let expr = ExprSpan::parse(stream)?;
                if stream.next_is_symbol(Symbol::Semicolon) {
                    stream.consume()?;
                    Ok(Stmt::Expr(expr))
                } else {
                    Ok(Stmt::Expr(expr))
                }
            }
        }
    }
}

impl Parse for LetStmt {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let let_token = stream.expect(&Token::Keyword(Keyword::Let))?;
        let pat = PatSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = if !stream.next_is_symbol(Symbol::Equal) {
            Some(TypeSpan::parse(stream)?)
        } else {
            None
        };
        let eq_token = stream.expect_symbol(Symbol::Equal)?;
        let expr = ExprSpan::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semicolon)?;

        Ok(LetStmt {
            let_token,
            pat,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            ident::Identifier,
            keyword::Keyword,
            literal::Literal,
            op::{BinOp, UnOp},
            symbol::Symbol,
            token::{Brace, Bracket, Paren, Token, TokenSpan},
        },
        diagnostic::Span,
    };

    fn create_token_stream(tokens: Vec<Token>) -> TokenStream {
        let token_spans = tokens
            .into_iter()
            .map(|t| TokenSpan {
                value: t,
                span: Span::default(),
            })
            .collect();
        TokenStream::new(token_spans)
    }

    #[test]
    fn test_parse_literal() {
        let stream = create_token_stream(vec![Token::Literal(Literal::Integer(42))]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let expr = ExprSpan::parse(&mut parse_stream).unwrap();
        assert!(matches!(expr.as_ref(), Expr::Lit(_)));
    }

    #[test]
    fn test_parse_binary_expr() {
        let stream = create_token_stream(vec![
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Plus),
            Token::Literal(Literal::Integer(2)),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let expr = ExprSpan::parse(&mut parse_stream).unwrap();
        println!("expr: {:?}", &expr);
        assert!(matches!(expr.as_ref(), Expr::Binary(_)));
    }

    #[test]
    fn test_parse_let_stmt() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Let),
            Token::Ident("x".into()),
            Token::Symbol(Symbol::Colon),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Equal),
            Token::Literal(Literal::Integer(42)),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let stmt = Stmt::parse(&mut parse_stream).unwrap();
        assert!(matches!(stmt, Stmt::Let(_)));
    }

    #[test]
    fn test_parse_pat_struct() {
        let stream = create_token_stream(vec![
            Token::Ident("Point".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Ident("x".into()),
            Token::Symbol(Symbol::Colon),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Comma),
            Token::Ident("y".into()),
            Token::Symbol(Symbol::Colon),
            Token::Literal(Literal::Integer(2)),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let pat = Pat::parse(&mut parse_stream).unwrap();
        println!("{:?}", &pat);
        assert!(matches!(pat, Pat::Struct(_)));
    }

    #[test]
    fn test_parse_type_reference() {
        let stream =
            create_token_stream(vec![Token::Symbol(Symbol::And), Token::Ident("i32".into())]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let ty = Type::parse(&mut parse_stream).unwrap();
        assert!(matches!(ty, Type::Reference(_)));
    }

    #[test]
    fn test_parse_empty_stmt() {
        let stream = create_token_stream(vec![Token::Symbol(Symbol::Semicolon)]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let stmt = Stmt::parse(&mut parse_stream).unwrap();
        assert!(matches!(stmt, Stmt::Empty));
    }
}
