use std::{borrow::Cow, iter::Peekable};

use crate::{
    ast::{
        Arm, BareFnArg, Block, Expression, ExprArray, ExprAssign, ExprBinary, ExprBlock, ExprBreak,
        ExprCall, ExprCast, ExprClosure, ExprContinue, ExprField, ExprForLoop, ExprGroup, ExprIf,
        ExprIndex, ExprInfer, ExprLit, ExprLoop, ExprMatch, ExprMethodCall, ExprPath, ExprRange,
        ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTuple, ExprUnary,
        ExprWhile, Field, FieldPat, FieldValue, Fields, FieldsNamed, FieldsUnnamed, FnArg,
        IdentSpan, ImplItem, ImplItemConst, ImplItemFn, Item, ItemEnum, ItemFn, ItemImpl,
        ItemStruct, ItemType, ItemUse, KeywordSpan, Label, LetStmt, LiteralSpan, LocalInit, Pat,
        PatRest, PatStruct, PatTuple, PatType, PatWild, Path, PathSegment, RangeLimits, Receiver,
        RetureType, Signature, Stmt, SymbolSpan, Type, TypeAny, TypeArray, TypeBareFn, TypeInfer,
        TypeNever, TypeParamBound, TypeParen, TypeReference, TypeSlice, TypeTraitObject, TypeTuple,
        UseGlob, UseGroup, UseName, UsePath, UseRename, UseTree, Variant, Visibility,
        ident::Identifier,
        keyword::Keyword,
        literal::Literal,
        op::{BinOp, UnOp},
        precedence::Precedence,
        symbol::Symbol,
        token::{self, Brace, Bracket, Paren, Punctuated, Token, TokenSpan},
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

    pub fn peek(&mut self) -> Option<&TokenSpan> {
        self.iter.peek().cloned()
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

    pub fn parse_punctuated_with<T, F>(
        &mut self,
        f: F,
        sep: &Token,
    ) -> Result<Punctuated<T>, ParseError>
    where
        T: Parse,
        F: Fn(&mut ParseStream) -> Result<T, ParseError>,
    {
        let mut items = Vec::new();
        let mut last = None;

        loop {
            match self.try_parse(&f) {
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

impl Parse for LiteralSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_literal() {
            return Err(ParseError::new("Expected literal value")
                .with_span(tok.span)
                .with_expected("valid literal")
                .with_found(format!("{:?}", tok.value)));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_literal()))
    }
}

impl Parse for IdentSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_ident() {
            return Err(ParseError::new("Expected identifier")
                .with_span(tok.span)
                .with_expected("valid identifier")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_ident()))
    }
}

impl Parse for KeywordSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.lookahead1()?;

        if !tok.is_keyword() {
            return Err(ParseError::new("Expected keyword")
                .with_span(tok.span)
                .with_expected("valid keyword")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_keyword()))
    }
}

impl Parse for SymbolSpan {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let tok = stream.consume()?;

        if !tok.is_symbol() {
            return Err(ParseError::new("Expected symbol")
                .with_span(tok.span)
                .with_expected("valid symbol")
                .with_found(tok.value.to_string()));
        }

        let token = stream.consume()?;
        Ok(token.map(|t| t.into_symbol()))
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
        let expr = Box::new(Expression::parse(stream)?);
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

// impl Parse for ExprUnary {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let op = Spanned::<UnOp>::parse(stream)?;
//         let expr = Box::new(Expr::parse(stream)?);
//         Ok(ExprUnary { op, expr })
//     }
// }

// impl Parse for ExprBinary {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let lhs = Box::new(Expr::parse(stream)?);
//         let op = Spanned::<BinOp>::parse(stream)?;
//         let rhs = Box::new(Expr::parse(stream)?);
//         Ok(ExprBinary { op, lhs, rhs })
//     }
// }

// impl Parse for ExprAssign {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let left = Box::new(Expr::parse(stream)?);
//         let eq_token = stream.expect_symbol(Symbol::Equal)?;
//         let right = Box::new(Expr::parse(stream)?);
//         Ok(ExprAssign {
//             left,
//             eq_token,
//             right,
//         })
//     }
// }

impl Parse for ExprField {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let expr = Box::new(Expression::parse(stream)?);
        let dot_token = stream.expect_symbol(Symbol::Dot)?;
        let member = IdentSpan::parse(stream)?;
        Ok(ExprField {
            expr,
            dot_token,
            member,
        })
    }
}

// impl Parse for ExprIndex {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let expr = Box::new(Expr::parse(stream)?);
//         let open_token = stream.expect_symbol(Symbol::LBracket)?;
//         let index = Box::new(Expr::parse(stream)?);
//         let close_token = stream.expect_symbol(Symbol::RBracket)?;

//         Ok(ExprIndex {
//             expr,
//             bracket_token: Bracket {
//                 open: open_token,
//                 close: close_token,
//             },
//             index,
//         })
//     }
// }

// impl Parse for ExprCall {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let func = Box::new(Expr::parse(stream)?);
//         let open_token = stream.expect_symbol(Symbol::LParen)?;
//         let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
//         let close_token = stream.expect_symbol(Symbol::RParen)?;

//         Ok(ExprCall {
//             paren_token: Paren {
//                 open: open_token,
//                 close: close_token,
//             },
//             func,
//             args,
//         })
//     }
// }

// impl Parse for ExprMethodCall {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let receiver = Box::new(Expr::parse(stream)?);
//         let method = IdentSpan::parse(stream)?;
//         let open_token = stream.expect_symbol(Symbol::LParen)?;
//         let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
//         let close_token = stream.expect_symbol(Symbol::RParen)?;

//         Ok(ExprMethodCall {
//             receiver,
//             method,
//             paren_token: Paren {
//                 open: open_token,
//                 close: close_token,
//             },
//             args,
//         })
//     }
// }

// impl Parse for ExprCast {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let expr = Box::new(Expr::parse(stream)?);
//         let as_token = stream.expect(&Token::Keyword(Keyword::As))?;
//         let ty = Box::new(Type::parse(stream)?);
//         Ok(ExprCast { expr, as_token, ty })
//     }
// }

// impl Parse for ExprTry {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let expr = Box::new(Expr::parse(stream)?);
//         let question_token = stream.expect_symbol(Symbol::Question)?;
//         Ok(ExprTry {
//             expr,
//             question_token,
//         })
//     }
// }

// impl Parse for ExprReference {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let and_token = stream.expect_symbol(Symbol::And)?;
//         let expr = Box::new(Expr::parse(stream)?);
//         Ok(ExprReference { and_token, expr })
//     }
// }

impl Parse for ExprRepeat {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBracket)?;
        let expr = Box::new(Expression::parse(stream)?);
        let semicolon_token = stream.expect_symbol(Symbol::Semicolon)?;
        let len = Box::new(Expression::parse(stream)?);
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

// impl Parse for ExprRange {
//     fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
//         let start = if stream.next_is_symbol(Symbol::DotDot)
//             || stream.next_is_symbol(Symbol::DotDotEqual)
//         {
//             None
//         } else {
//             Some(Box::new(Expr::parse(stream)?))
//         };

//         let limits = if stream.next_is_symbol(Symbol::DotDot) {
//             stream.consume()?;
//             Spanned::new(RangeLimits::HalfOpen, stream.lookahead1()?.span)
//         } else if stream.next_is_symbol(Symbol::DotDotEqual) {
//             stream.consume()?;
//             Spanned::new(RangeLimits::Closed, stream.lookahead1()?.span)
//         } else {
//             return Err(ParseError::new("Expected range operator (.. or ..=)"));
//         };

//         let end = if stream.next_is_symbol(Symbol::Semicolon)
//             || stream.next_is_symbol(Symbol::Comma)
//             || stream.next_is_symbol(Symbol::RParen)
//         {
//             None
//         } else {
//             Some(Box::new(Expr::parse(stream)?))
//         };

//         Ok(ExprRange { start, end, limits })
//     }
// }

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
        let cond = Box::new(Expression::parse(stream)?);
        let then_branch = Block::parse(stream)?;

        let else_branch = if stream.next_is(&Token::Keyword(Keyword::Else)) {
            stream.consume()?;
            Some(Box::new(Expression::parse(stream)?))
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
        let cond = Box::new(Expression::parse(stream)?);
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
        let pat = Box::new(Pat::parse(stream)?);
        let in_token = stream.expect(&Token::Keyword(Keyword::In))?;
        let expr = Box::new(Expression::parse(stream)?);
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

        let expr = if !stream.next_is_symbol(Symbol::Semicolon) && stream.peek().is_some() {
            Some(Box::new(Expression::parse(stream)?))
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
        let expr = if !stream.next_is_symbol(Symbol::Semicolon) && stream.peek().is_some() {
            Some(Box::new(Expression::parse(stream)?))
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
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        } else {
            Punctuated::new()
        };

        let or2_token = stream.expect_symbol(Symbol::Or)?;

        println!("inputs: {inputs:?}");

        let output = if stream.next_is_symbol(Symbol::RArrow) {
            let arrow_token = stream.expect_symbol(Symbol::RArrow)?;
            RetureType::Typed(arrow_token, Box::new(Type::parse(stream)?))
        } else {
            RetureType::Default
        };

        println!("output: {output:?}");

        let body = Box::new(Expression::parse(stream)?);
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
        let expr = Box::new(Expression::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut arms = Vec::new();
        while !stream.next_is_symbol(Symbol::RBrace) {
            let pat = Box::new(Pat::parse(stream)?);

            let guard = if stream.next_is(&Token::Keyword(Keyword::If)) {
                stream.consume()?;
                Some(Box::new(Expression::parse(stream)?))
            } else {
                None
            };

            let fat_arrow_token = stream.expect_symbol(Symbol::FatArrow)?;
            let body = Box::new(Expression::parse(stream)?);

            let comma = stream.next_is_symbol(Symbol::Comma);
            if comma {
                stream.consume()?;
            }

            arms.push(Arm {
                pat,
                fat_arrow_token,
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
            let expr = Box::new(Expression::parse(stream)?);

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
fn parse_expr(stream: &mut ParseStream, prec: Precedence) -> Result<Expression, ParseError> {
    let mut expr = parse_prefix(stream)?;

    while let Some(peek) = stream.peek() {
        let next_prec = get_next_precedence(peek)?;
        if next_prec <= prec {
            break;
        }

        expr = parse_infix(stream, expr, next_prec)?;

        println!("{expr:?}");
    }

    println!("-> {expr:?}");

    Ok(expr)
}

/// 解析主表达式 (字面量、标识符、括号表达式等)
fn parse_primary(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let start_token = stream.peek().ok_or(ParseError::eof())?;

    let expr = match &start_token.value {
        // 字面量
        Token::Literal(_) => Expression::Lit(ExprLit::parse(stream)?),

        // 标识符
        Token::Ident(_) => Expression::Path(ExprPath::parse(stream)?),

        // 下划线
        Token::Symbol(Symbol::Underscore) => Expression::Infer(ExprInfer::parse(stream)?),

        // 括号表达式
        Token::Symbol(Symbol::LParen) => Expression::Group(ExprGroup::parse(stream)?),

        // 数组表达式
        Token::Symbol(Symbol::LBracket) => Expression::Array(ExprArray::parse(stream)?),

        // 块表达式
        Token::Symbol(Symbol::LBrace) => Expression::Block(ExprBlock::parse(stream)?),

        // 控制流表达式
        Token::Keyword(Keyword::If) => Expression::If(ExprIf::parse(stream)?),
        Token::Keyword(Keyword::While) => Expression::While(ExprWhile::parse(stream)?),
        Token::Keyword(Keyword::Loop) => Expression::Loop(ExprLoop::parse(stream)?),
        Token::Keyword(Keyword::For) => Expression::ForLoop(ExprForLoop::parse(stream)?),
        Token::Keyword(Keyword::Break) => Expression::Break(ExprBreak::parse(stream)?),
        Token::Keyword(Keyword::Continue) => Expression::Continue(ExprContinue::parse(stream)?),
        Token::Keyword(Keyword::Return) => Expression::Return(ExprReturn::parse(stream)?),
        Token::Keyword(Keyword::Match) => Expression::Match(ExprMatch::parse(stream)?),

        // 闭包表达式
        Token::Symbol(Symbol::Or) => Expression::Closure(ExprClosure::parse(stream)?),

        _ => {
            return Err(ParseError::new("Expected primary expression")
                .with_span(start_token.span)
                .with_expected("valid primary expression"));
        }
    };

    Ok(expr)
}

/// 解析前缀表达式
fn parse_prefix(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let start_token = stream.lookahead1()?;

    let expr = match &start_token.value {
        // 引用表达式
        Token::Symbol(Symbol::And) => {
            let and_token = stream.consume()?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary)?);

            Expression::Reference(ExprReference { and_token, expr })
        }

        // 一元运算符
        Token::Symbol(Symbol::Not) | Token::Symbol(Symbol::Minus) => {
            let op = Spanned::<UnOp>::parse(stream)?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary)?);

            Expression::Unary(ExprUnary { op, expr })
        }

        // 主表达式
        _ => parse_primary(stream)?,
    };

    Ok(expr)
}

/// 获取下一个运算符的优先级
fn get_next_precedence(token: &TokenSpan) -> Result<Precedence, ParseError> {
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
fn parse_infix(stream: &mut ParseStream, left: Expression, prec: Precedence) -> Result<Expression, ParseError> {
    let start_token = stream.lookahead1()?;

    let expr = match &start_token.value {
        // 赋值表达式
        Token::Symbol(Symbol::Equal) => {
            let eq_token = stream.consume()?;
            let rhs = Box::new(parse_expr(stream, Precedence::None)?);
            Expression::Assign(ExprAssign {
                left: Box::new(left),
                eq_token,
                right: rhs,
            })
        }

        // 二元运算符
        Token::Symbol(sym) if BinOp::from_symbol(*sym).is_ok() => {
            let op = Spanned::<BinOp>::parse(stream)?;
            let rhs = Box::new(parse_expr(stream, prec)?);
            Expression::Binary(ExprBinary {
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

            Expression::Call(ExprCall {
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

            Expression::Index(ExprIndex {
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
            Expression::Field(ExprField {
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

            Expression::Range(ExprRange {
                start: Some(Box::new(left)),
                end,
                limits,
            })
        }

        // 问号运算符
        Token::Symbol(Symbol::Question) => {
            let question_token = stream.consume()?;
            Expression::Try(ExprTry {
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

    Ok(expr)
}

impl Parse for Expression {
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
            stmts.push(Stmt::parse(stream)?);
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

impl Parse for UseName {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        Ok(UseName { name })
    }
}

impl Parse for UseRename {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        let as_token = stream.expect(&Token::Keyword(Keyword::As))?;
        let alias = IdentSpan::parse(stream)?;
        Ok(UseRename {
            name,
            as_token,
            alias,
        })
    }
}

impl Parse for RetureType {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        if stream.next_is(&Token::Symbol(Symbol::RArrow)) {
            let arrow_token = stream.expect_symbol(Symbol::RArrow)?;
            let ty = Box::new(Type::parse(stream)?);
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

impl Parse for Spanned<UnOp> {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;
        match token.value {
            Token::Symbol(Symbol::Not) => Ok(token.map(|_| UnOp::Not)),
            Token::Symbol(Symbol::Minus) => Ok(token.map(|_| UnOp::Neg)),
            Token::Symbol(Symbol::And) => Ok(token.map(|_| UnOp::Deref)),
            _ => Err(ParseError::new("Expected unary operator")
                .with_span(token.span)
                .with_expected("!, -, or &")),
        }
    }
}

impl Parse for Spanned<BinOp> {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;
        match &token.value {
            Token::Symbol(sym) => match BinOp::from_symbol(*sym) {
                Ok(op) => Ok(token.map(|_| op)),
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

        let pat = match &token.value {
            Token::Literal(_) => Pat::Lit(LiteralSpan::parse(stream)?),
            Token::Ident(_) => {
                if let Ok(pat) = stream.try_parse(PatStruct::parse) {
                    Pat::Struct(pat)
                } else {
                    Pat::Path(Path::parse(stream)?)
                }
            }
            Token::Symbol(Symbol::Underscore) => Pat::Wild(PatWild::parse(stream)?),
            Token::Symbol(Symbol::DotDot) => Pat::Rest(PatRest::parse(stream)?),
            Token::Symbol(Symbol::LParen) => Pat::Tuple(PatTuple::parse(stream)?),
            _ => {
                return Err(ParseError::new("Expected pattern")
                    .with_span(token.span)
                    .with_expected("literal, identifier, _, or .."));
            }
        };

        // when there is a colon, it's a type annotation
        if let Some(peek) = stream.peek()
            && peek == &Token::Symbol(Symbol::Colon)
        {
            let colon_token = stream.expect_symbol(Symbol::Colon)?;
            let pat = Box::new(pat);
            let ty = Box::new(Type::parse(stream)?);
            return Ok(Pat::Type(PatType {
                pat,
                colon_token,
                ty,
            }));
        }

        Ok(pat)
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

        let fields = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;

        let rest = stream.try_parse(PatRest::parse).ok();

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        let brace_token = Brace::new(open_token, close_token);

        Ok(PatStruct {
            path,
            brace_token,
            fields,
            rest,
        })
    }
}

impl Parse for FieldPat {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let member = IdentSpan::parse(stream)?;
        if let Some(peek) = stream.peek()
            && peek.value == Token::Symbol(Symbol::Colon)
        {
            // eg. `name: name`
            let colon_token = Some(stream.expect_symbol(Symbol::Colon)?);

            let pat = Box::new(Pat::parse(stream)?);

            return Ok(FieldPat {
                member,
                colon_token,
                pat,
            });
        } else {
            let pat = Pat::Ident(member.clone());

            Ok(FieldPat {
                member,
                colon_token: None,
                pat: Box::new(pat),
            })
        }
    }
}

impl Parse for PatTuple {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let mut elts = Vec::new();

        while !stream.next_is_symbol(Symbol::RParen) {
            elts.push(Pat::parse(stream)?);

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
        let pat = Box::new(Pat::parse(stream)?);
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Box::new(Type::parse(stream)?);
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
        let elem = Box::new(Type::parse(stream)?);
        let semicolon = stream.expect_symbol(Symbol::Semicolon)?;
        let len = Expression::parse(stream)?;
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
        let elem = Box::new(Type::parse(stream)?);
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
        let elem = Box::new(Type::parse(stream)?);
        Ok(TypeReference { and_token, elem })
    }
}

impl Parse for TypeParen {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let elem = Box::new(Type::parse(stream)?);
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

        let ty = Type::parse(stream)?;
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
                let expr = Expression::parse(stream)?;
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
        let pat = Pat::parse(stream)?;

        let init = if stream.next_is(&Token::Symbol(Symbol::Equal)) {
            Some(stream.try_parse(LocalInit::parse)?)
        } else {
            None
        };

        let semi_token = stream.expect_symbol(Symbol::Semicolon)?;

        Ok(LetStmt {
            let_token,
            pat,
            init,
            semi_token,
        })
    }
}

impl Parse for LocalInit {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let eq_token = stream.expect_symbol(Symbol::Equal)?;
        let expr = Expression::parse(stream)?;
        Ok(LocalInit { eq_token, expr })
    }
}

// Item 相关的 Parse 实现
impl Parse for Item {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Keyword(Keyword::Fn) => Ok(Item::Fn(ItemFn::parse(stream)?)),
            Token::Keyword(Keyword::Struct) => Ok(Item::Struct(ItemStruct::parse(stream)?)),
            Token::Keyword(Keyword::Enum) => Ok(Item::Enum(ItemEnum::parse(stream)?)),
            Token::Keyword(Keyword::Impl) => Ok(Item::Impl(ItemImpl::parse(stream)?)),
            Token::Keyword(Keyword::Type) => Ok(Item::Type(ItemType::parse(stream)?)),
            Token::Keyword(Keyword::Use) => Ok(Item::Use(ItemUse::parse(stream)?)),
            _ => Err(ParseError::new("Expected item")
                .with_span(token.span)
                .with_expected("fn, struct, enum, impl, type, or use")),
        }
    }
}

impl Parse for ItemFn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;
        let sig = Signature::parse(stream)?;
        let block = Block::parse(stream)?;
        Ok(ItemFn { vis, sig, block })
    }
}

impl Parse for Signature {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
        let name = IdentSpan::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LParen)?;

        let inputs = if stream.next_is_symbol(Symbol::RParen) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };

        let close_token = stream.expect_symbol(Symbol::RParen)?;

        let output = RetureType::parse(stream)?;

        Ok(Signature {
            fn_token,
            name,
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            inputs,
            output,
        })
    }
}

impl Parse for FnArg {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // Try to parse as receiver first
        if let Ok(receiver) = stream.try_parse(Receiver::parse) {
            return Ok(FnArg::Receiver(receiver));
        }

        // Otherwise parse as typed argument
        let pat = Pat::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;

        Ok(FnArg::Typed(PatType {
            pat: Box::new(pat),
            colon_token,
            ty: Box::new(ty),
        }))
    }
}

impl Parse for Receiver {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        // Check for &self or &mut self
        let and_token = if stream.next_is_symbol(Symbol::And) {
            Some(stream.expect_symbol(Symbol::And)?)
        } else {
            None
        };

        let self_token = stream.expect(&Token::Keyword(Keyword::SelfValue))?;

        Ok(Receiver {
            and_token,
            self_token,
        })
    }
}

impl Parse for ItemStruct {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let struct_token = stream.expect(&Token::Keyword(Keyword::Struct))?;
        let name = IdentSpan::parse(stream)?;

        // Parse fields
        let fields = if stream.next_is_symbol(Symbol::LBrace) {
            Fields::Named(FieldsNamed::parse(stream)?)
        } else if stream.next_is_symbol(Symbol::LParen) {
            Fields::Unnamed(FieldsUnnamed::parse(stream)?)
        } else {
            Fields::Unit
        };

        let semi_token = if matches!(fields, Fields::Unit) || matches!(fields, Fields::Unnamed(_)) {
            Some(stream.expect_symbol(Symbol::Semicolon)?)
        } else {
            None
        };

        Ok(ItemStruct {
            vis,
            struct_token,
            name,
            fields,
            semi_token,
        })
    }
}

impl Parse for FieldsNamed {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let named = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated_with(Field::parse_named, &Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(FieldsNamed {
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            named,
        })
    }
}

impl Parse for FieldsUnnamed {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let unnamed = if stream.next_is_symbol(Symbol::RParen) {
            Punctuated::new()
        } else {
            stream.parse_punctuated_with(Field::parse_unnamed, &Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(FieldsUnnamed {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            unnamed,
        })
    }
}

impl Parse for Field {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        if let Ok(field) = stream.try_parse(Field::parse_unnamed) {
            return Ok(field);
        }

        Field::parse_named(stream)
    }
}

impl Field {
    fn parse_named(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;
        let name = IdentSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;

        Ok(Field {
            vis,
            name: Some(name),
            colon_token: Some(colon_token),
            ty,
        })
    }

    fn parse_unnamed(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let ty = Type::parse(stream)?;

        Ok(Field {
            vis: Visibility::Inherited,
            name: None,
            colon_token: None,
            ty,
        })
    }
}

impl Parse for ItemEnum {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let enum_token = stream.expect(&Token::Keyword(Keyword::Enum))?;
        let name = IdentSpan::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let variants = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(ItemEnum {
            vis,
            enum_token,
            name,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            variants,
        })
    }
}

impl Parse for Variant {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;

        // Parse fields if present
        let fields = if stream.next_is_symbol(Symbol::LBrace) {
            Fields::Named(FieldsNamed::parse(stream)?)
        } else if stream.next_is_symbol(Symbol::LParen) {
            Fields::Unnamed(FieldsUnnamed::parse(stream)?)
        } else {
            Fields::Unit
        };

        // Parse discriminant if present
        let discriminant = if stream.next_is_symbol(Symbol::Equal) {
            let eq_token = stream.expect_symbol(Symbol::Equal)?;
            let expr = Expression::parse(stream)?;
            Some((eq_token, expr))
        } else {
            None
        };

        Ok(Variant {
            name,
            fields,
            discriminant,
        })
    }
}

impl Parse for ItemImpl {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let impl_token = stream.expect(&Token::Keyword(Keyword::Impl))?;
        let self_ty = Box::new(Type::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut items = Vec::new();
        while !stream.next_is_symbol(Symbol::RBrace) {
            items.push(ImplItem::parse(stream)?);
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(ItemImpl {
            impl_token,
            trait_: None, // For simplicity, not parsing trait impls like `Trait for Type`
            self_ty,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            items,
        })
    }
}

impl Parse for ImplItem {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Keyword(Keyword::Fn) => Ok(ImplItem::Fn(ImplItemFn::parse(stream)?)),
            Token::Keyword(Keyword::Const) => Ok(ImplItem::Const(ImplItemConst::parse(stream)?)),
            _ => Err(ParseError::new("Expected impl item")
                .with_span(token.span)
                .with_expected("fn or const")),
        }
    }
}

impl Parse for ImplItemFn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let sig = Signature::parse(stream)?;
        let block = Block::parse(stream)?;
        Ok(ImplItemFn { vis, sig, block })
    }
}

impl Parse for ImplItemConst {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let const_token = stream.expect(&Token::Keyword(Keyword::Const))?;
        let name = IdentSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;
        let eq_token = stream.expect_symbol(Symbol::Equal)?;
        let expr = Expression::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semicolon)?;

        Ok(ImplItemConst {
            const_token,
            name,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        })
    }
}

impl Parse for ItemType {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let type_token = stream.expect(&Token::Keyword(Keyword::Type))?;
        let ident = IdentSpan::parse(stream)?;
        let eq_token = stream.expect_symbol(Symbol::Equal)?;
        let ty = Type::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semicolon)?;

        Ok(ItemType {
            vis,
            type_token,
            ident,
            eq_token,
            ty,
            semi_token,
        })
    }
}

impl Parse for ItemUse {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let use_token = stream.expect(&Token::Keyword(Keyword::Use))?;

        let leading_colon = if stream.next_is_symbol(Symbol::ColonColon) {
            Some(stream.expect_symbol(Symbol::ColonColon)?)
        } else {
            None
        };

        let tree = UseTree::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semicolon)?;

        Ok(ItemUse {
            vis,
            use_token,
            leading_colon,
            tree,
            semi_token,
        })
    }
}

impl Parse for UseTree {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // Try to parse as path
        if let Ok(path) = stream.try_parse(UsePath::parse) {
            return Ok(UseTree::Path(path));
        }

        // Try to parse as group
        if let Ok(group) = stream.try_parse(UseGroup::parse) {
            return Ok(UseTree::Group(group));
        }

        // Try to parse as name
        if let Ok(name) = stream.try_parse(UseName::parse) {
            return Ok(UseTree::Name(name));
        }

        // Try to parse as rename
        if let Ok(rename) = stream.try_parse(UseRename::parse) {
            return Ok(UseTree::Rename(rename));
        }

        // Try to parse as glob
        if let Ok(glob) = stream.try_parse(UseGlob::parse) {
            return Ok(UseTree::Glob(glob));
        }

        let token = stream.lookahead1()?;
        Err(ParseError::new("Expected use tree")
            .with_span(token.span)
            .with_expected("path, group, name, rename, or glob"))
    }
}

impl Parse for UsePath {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        let colon2_token = stream.expect_symbol(Symbol::ColonColon)?;
        let tree = UseTree::parse(stream)?;
        Ok(UsePath {
            name,
            colon2_token,
            tree: Box::new(tree),
        })
    }
}

impl Parse for UseGroup {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let items = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(UseGroup {
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            items,
        })
    }
}

impl Parse for UseGlob {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let star_token = stream.expect_symbol(Symbol::Star)?;
        Ok(UseGlob { star_token })
    }
}

impl Parse for Visibility {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        match stream.peek().map(|v| v.as_ref()) {
            Some(Token::Keyword(Keyword::Pub)) => {
                let pub_token = stream.expect(&Token::Keyword(Keyword::Pub))?;
                Ok(Visibility::Public(pub_token))
            }
            Some(Token::Keyword(Keyword::Priv)) => {
                let priv_token = stream.expect(&Token::Keyword(Keyword::Priv))?;
                Ok(Visibility::Private(priv_token))
            }
            _ => Ok(Visibility::Inherited),
        }
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
        let expr = Expression::parse(&mut parse_stream).unwrap();
        assert!(matches!(expr, Expression::Lit(_)));
    }

    #[test]
    fn test_parse_binary_expr() {
        let stream = create_token_stream(vec![
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Plus),
            Token::Literal(Literal::Integer(2)),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let expr = Expression::parse(&mut parse_stream).unwrap();
        assert!(matches!(expr, Expression::Binary(_)));
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

    #[test]
    fn test_parse_function() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Fn),
            Token::Ident("foo".into()),
            Token::Symbol(Symbol::LParen),
            Token::Symbol(Symbol::RParen),
            Token::Symbol(Symbol::LBrace),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Fn(_)));
    }

    #[test]
    fn test_parse_struct_named() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Struct),
            Token::Ident("Point".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Ident("x".into()),
            Token::Symbol(Symbol::Colon),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("y".into()),
            Token::Symbol(Symbol::Colon),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Struct(_)));
    }

    #[test]
    fn test_parse_struct_unnamed() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Struct),
            Token::Ident("Point".into()),
            Token::Symbol(Symbol::LParen),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::RParen),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Struct(_)));
    }

    #[test]
    fn test_parse_enum() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Enum),
            Token::Ident("Color".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Ident("Red".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("Green".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("Blue".into()),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Enum(_)));
    }

    #[test]
    fn test_parse_impl() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Impl),
            Token::Ident("Point".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Impl(_)));
    }

    #[test]
    fn test_parse_type_alias() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Type),
            Token::Ident("MyInt".into()),
            Token::Symbol(Symbol::Equal),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Type(_)));
    }

    #[test]
    fn test_parse_use_simple() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Use),
            Token::Ident("std".into()),
            Token::Symbol(Symbol::ColonColon),
            Token::Ident("io".into()),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Use(_)));
    }

    #[test]
    fn test_parse_use_glob() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Use),
            Token::Ident("std".into()),
            Token::Symbol(Symbol::ColonColon),
            Token::Symbol(Symbol::Star),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut parse_stream = ParseStream::new(&stream).unwrap();
        let item = Item::parse(&mut parse_stream).unwrap();
        assert!(matches!(item, Item::Use(_)));
    }

    // ---------- 通用 Spanned ----------
    #[test]
    fn test_spanned_literal() {
        let stream = create_token_stream(vec![Token::Literal(Literal::Integer(123))]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Spanned::<LiteralSpan>::parse(&mut s).unwrap();
    }

    // ---------- 基本 token ----------
    #[test]
    fn test_ident_span() {
        let stream = create_token_stream(vec![Token::Ident("foo".into())]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = IdentSpan::parse(&mut s).unwrap();
    }

    #[test]
    fn test_keyword_span() {
        let stream = create_token_stream(vec![Token::Keyword(Keyword::Fn)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = KeywordSpan::parse(&mut s).unwrap();
    }

    #[test]
    fn test_symbol_span() {
        let stream = create_token_stream(vec![Token::Symbol(Symbol::Star)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = SymbolSpan::parse(&mut s).unwrap();
    }

    // ---------- 表达式 ----------
    #[test]
    fn test_expr_array() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LBracket),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Comma),
            Token::Literal(Literal::Integer(2)),
            Token::Symbol(Symbol::RBracket),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprArray::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_tuple() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LParen),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Comma),
            Token::Literal(Literal::Integer(2)),
            Token::Symbol(Symbol::RParen),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprTuple::parse(&mut s).unwrap();
    }

    // #[test]
    // fn test_expr_unary() {
    //     let stream = create_token_stream(vec![
    //         Token::Symbol(Symbol::Minus),
    //         Token::Literal(Literal::Integer(1)),
    //     ]);
    //     let mut s = ParseStream::new(&stream).unwrap();
    //     let _ = ExprUnary::parse(&mut s).unwrap();
    // }

    // #[test]
    // fn test_expr_assign() {
    //     let stream = create_token_stream(vec![
    //         Token::Ident("x".into()),
    //         Token::Symbol(Symbol::Equal),
    //         Token::Literal(Literal::Integer(1)),
    //     ]);
    //     let mut s = ParseStream::new(&stream).unwrap();
    //     let _ = ExprAssign::parse(&mut s).unwrap();
    // }

    #[test]
    fn test_expr_field() {
        let stream = create_token_stream(vec![
            Token::Ident("obj".into()),
            Token::Symbol(Symbol::Dot),
            Token::Ident("field".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_index() {
        let stream = create_token_stream(vec![
            Token::Ident("arr".into()),
            Token::Symbol(Symbol::LBracket),
            Token::Literal(Literal::Integer(0)),
            Token::Symbol(Symbol::RBracket),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_call() {
        let stream = create_token_stream(vec![
            Token::Ident("f".into()),
            Token::Symbol(Symbol::LParen),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::RParen),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_method_call() {
        let stream = create_token_stream(vec![
            Token::Ident("obj".into()),
            Token::Symbol(Symbol::Dot),
            Token::Ident("method".into()),
            Token::Symbol(Symbol::LParen),
            Token::Symbol(Symbol::RParen),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_cast() {
        let stream = create_token_stream(vec![
            Token::Literal(Literal::Integer(1)),
            Token::Keyword(Keyword::As),
            Token::Ident("i64".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_try() {
        let stream = create_token_stream(vec![
            Token::Ident("x".into()),
            Token::Symbol(Symbol::Question),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_reference() {
        let stream =
            create_token_stream(vec![Token::Symbol(Symbol::And), Token::Ident("x".into())]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = Expression::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_repeat() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LBracket),
            Token::Literal(Literal::Integer(0)),
            Token::Symbol(Symbol::Semicolon),
            Token::Literal(Literal::Integer(3)),
            Token::Symbol(Symbol::RBracket),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprRepeat::parse(&mut s).unwrap();
    }

    // #[test]
    // fn test_expr_range() {
    //     let stream = create_token_stream(vec![
    //         Token::Literal(Literal::Integer(1)),
    //         Token::Symbol(Symbol::DotDot),
    //         Token::Literal(Literal::Integer(5)),
    //     ]);
    //     let mut s = ParseStream::new(&stream).unwrap();
    //     let _ = ExprRange::parse(&mut s).unwrap();
    // }

    #[test]
    fn test_expr_closure() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::Or),
            Token::Symbol(Symbol::Or),
            Token::Symbol(Symbol::RArrow),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprClosure::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_break() {
        let stream = create_token_stream(vec![Token::Keyword(Keyword::Break)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprBreak::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_continue() {
        let stream = create_token_stream(vec![Token::Keyword(Keyword::Continue)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprContinue::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_return() {
        let stream = create_token_stream(vec![Token::Keyword(Keyword::Return)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprReturn::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_match() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Match),
            Token::Ident("x".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Ident("Some".into()),
            Token::Symbol(Symbol::FatArrow),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::Comma),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprMatch::parse(&mut s).unwrap();
    }

    #[test]
    fn test_expr_struct() {
        let stream = create_token_stream(vec![
            Token::Ident("S".into()),
            Token::Symbol(Symbol::LBrace),
            Token::Ident("a".into()),
            Token::Symbol(Symbol::Colon),
            Token::Literal(Literal::Integer(1)),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ExprStruct::parse(&mut s).unwrap();
    }

    // ---------- 模式 ----------
    #[test]
    fn test_pat_wild() {
        let stream = create_token_stream(vec![Token::Symbol(Symbol::Underscore)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = PatWild::parse(&mut s).unwrap();
    }

    #[test]
    fn test_pat_rest() {
        let stream = create_token_stream(vec![Token::Symbol(Symbol::DotDot)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = PatRest::parse(&mut s).unwrap();
    }

    #[test]
    fn test_pat_tuple() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LParen),
            Token::Ident("a".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("b".into()),
            Token::Symbol(Symbol::RParen),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = PatTuple::parse(&mut s).unwrap();
    }

    #[test]
    fn test_pat_type() {
        let stream = create_token_stream(vec![
            Token::Ident("x".into()),
            Token::Symbol(Symbol::Colon),
            Token::Ident("i32".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = PatType::parse(&mut s).unwrap();
    }

    // ---------- 类型 ----------
    #[test]
    fn test_type_array() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LBracket),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Semicolon),
            Token::Literal(Literal::Integer(4)),
            Token::Symbol(Symbol::RBracket),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeArray::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_slice() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LBracket),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::RBracket),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeSlice::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_bare_fn() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Fn),
            Token::Symbol(Symbol::LParen),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::RParen),
            Token::Symbol(Symbol::RArrow),
            Token::Ident("i32".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeBareFn::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_trait_object() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Dyn),
            Token::Ident("Clone".into()),
            Token::Symbol(Symbol::Plus),
            Token::Ident("Debug".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeTraitObject::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_param_bound() {
        let stream = create_token_stream(vec![Token::Ident("Clone".into())]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeParamBound::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_paren() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LParen),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::RParen),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeParen::parse(&mut s).unwrap();
    }

    #[test]
    fn test_type_any() {
        let stream = create_token_stream(vec![Token::Keyword(Keyword::Any)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = TypeAny::parse(&mut s).unwrap();
    }

    // ---------- use 相关 ----------
    #[test]
    fn test_use_path() {
        let stream = create_token_stream(vec![
            Token::Ident("a".into()),
            Token::Symbol(Symbol::ColonColon),
            Token::Ident("b".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = UsePath::parse(&mut s).unwrap();
    }

    #[test]
    fn test_use_group() {
        let stream = create_token_stream(vec![
            Token::Symbol(Symbol::LBrace),
            Token::Ident("a".into()),
            Token::Symbol(Symbol::Comma),
            Token::Ident("b".into()),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = UseGroup::parse(&mut s).unwrap();
    }

    #[test]
    fn test_use_rename() {
        let stream = create_token_stream(vec![
            Token::Ident("a".into()),
            Token::Keyword(Keyword::As),
            Token::Ident("b".into()),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = UseRename::parse(&mut s).unwrap();
    }

    #[test]
    fn test_use_glob() {
        let stream = create_token_stream(vec![Token::Symbol(Symbol::Star)]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = UseGlob::parse(&mut s).unwrap();
    }

    // ---------- impl 相关 ----------
    #[test]
    fn test_impl_item_const() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Const),
            Token::Ident("X".into()),
            Token::Symbol(Symbol::Colon),
            Token::Ident("i32".into()),
            Token::Symbol(Symbol::Equal),
            Token::Literal(Literal::Integer(42)),
            Token::Symbol(Symbol::Semicolon),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ImplItemConst::parse(&mut s).unwrap();
    }

    #[test]
    fn test_impl_item_fn() {
        let stream = create_token_stream(vec![
            Token::Keyword(Keyword::Fn),
            Token::Ident("foo".into()),
            Token::Symbol(Symbol::LParen),
            Token::Symbol(Symbol::RParen),
            Token::Symbol(Symbol::LBrace),
            Token::Symbol(Symbol::RBrace),
        ]);
        let mut s = ParseStream::new(&stream).unwrap();
        let _ = ImplItemFn::parse(&mut s).unwrap();
    }
}
