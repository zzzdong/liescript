//! 解析上下文模块

use std::iter::Peekable;
use std::slice::Iter;

use crate::{
    diagnostic::{Diagnostics, Span, Spanned},
    lexical::{Literal, Punctuated, Token, TokenSpan},
    parser::Parse,
};

use super::{
    config::ParserConfig,
    diagnostic::{ParseError, ParseResult},
};

/// 错误恢复类型
#[derive(Debug, Clone, Copy)]
pub enum RecoveryType {
    /// 语句级别恢复
    Statement,
    /// 表达式级别恢复
    Expression,
    /// 类型级别恢复
    Type,
    /// 项级别恢复
    Item,
}

/// 错误恢复点
#[derive(Debug, Clone)]
struct RecoveryPoint<'i> {
    /// 保存的迭代器状态
    iter: Peekable<std::slice::Iter<'i, TokenSpan>>,
    /// 保存的当前位置
    current_position: Option<Span>,
    /// 恢复类型
    recovery_type: RecoveryType,
}

/// LSP状态信息
#[derive(Debug, Clone)]
pub struct LspState {
    /// 光标位置
    pub cursor_position: Option<Span>,
    /// 增量解析的变更信息
    pub incremental_changes: Vec<IncrementalChange>,
}

/// 增量解析变更
#[derive(Debug, Clone)]
pub struct IncrementalChange {
    /// 变更范围
    pub span: Span,
    /// 新的内容
    pub new_content: String,
}

/// 解析上下文 - 核心结构
pub struct ParseContext<'i> {
    /// Token迭代器
    iter: Peekable<Iter<'i, TokenSpan>>,
    /// 当前解析位置（最近消耗的token的span）
    current_position: Option<Span>,
    /// 解析器配置
    config: &'i ParserConfig,
    /// 诊断信息收集器
    diagnostics: &'i mut Diagnostics,
    /// 错误恢复栈
    recovery_stack: Vec<RecoveryPoint<'i>>,
    /// LSP状态信息
    lsp_state: Option<LspState>,
}

impl<'i> ParseContext<'i> {
    /// 创建新的解析上下文
    pub fn new(
        tokens: &'i [TokenSpan],
        config: &'i ParserConfig,
        diagnostics: &'i mut Diagnostics,
    ) -> Self {
        // 使用TokenStream的iter()方法获取迭代器
        let iter = tokens.iter().peekable();
        Self {
            iter,
            current_position: None,
            config,
            diagnostics,
            recovery_stack: Vec::new(),
            lsp_state: None,
        }
    }

    /// Peek
    pub fn peek(&mut self) -> Option<&TokenSpan> {
        self.iter.peek().copied()
    }

    /// Peek Token
    pub fn peek_token(&mut self) -> Option<&Token> {
        self.peek().map(|ts| &ts.value)
    }

    /// 向前查看Token
    pub fn lookahead(&mut self, offset: usize) -> Option<&TokenSpan> {
        let mut iter = self.iter.clone();
        for _ in 0..offset {
            iter.next();
        }
        iter.next()
    }

    /// 向前查看Token的值
    pub fn lookahead_token(&mut self, offset: usize) -> Option<&Token> {
        self.lookahead(offset).map(|ts| &ts.value)
    }

    /// 检查是否到达文件末尾
    pub fn is_eof(&mut self) -> bool {
        self.iter.peek().is_none()
    }

    /// 消耗当前Token并前进
    pub fn consume(&mut self) -> ParseResult<TokenSpan> {
        if let Some(token) = self.iter.next() {
            self.current_position = Some(token.span);
            Ok(token.clone())
        } else {
            Err(self.create_eof_error("token"))
        }
    }

    /// 获取当前位置的Span
    pub fn current_span(&self) -> Option<Span> {
        self.current_position
    }

    /// 获取下一个token的span
    pub fn next_span(&mut self) -> Option<Span> {
        self.iter.peek().map(|ts| ts.span)
    }

    /// 创建错误时自动包含当前位置信息
    pub fn create_error_with_current_position(
        &self,
        message: String,
        expected: Option<String>,
        found: Option<String>,
    ) -> ParseError {
        let span = self.current_position.unwrap_or(Span::dummy());
        self.create_error(message, span, expected, found)
    }

    /// 检查下一个Token是否匹配
    pub fn next_is<M: TokenMatcher>(&mut self, matcher: M) -> bool {
        self.peek()
            .map(|token| matcher.matches(token))
            .unwrap_or(false)
    }

    /// 期望特定的Token
    pub fn expect<M: TokenMatcher>(
        &mut self,
        matcher: M,
        expected: &str,
    ) -> ParseResult<TokenSpan> {
        if let Some(token) = self.iter.peek() {
            if matcher.matches(&token.value) {
                self.consume()
            } else {
                let span = token.span;
                let found_value = format!("{:?}", token.value);
                let error = self.create_error(
                    format!("Expected {}", expected),
                    span,
                    Some(expected.to_string()),
                    Some(found_value),
                );
                Err(error)
            }
        } else {
            Err(self.create_eof_error(expected))
        }
    }

    /// 期望标识符
    pub fn expect_identifier(&mut self) -> ParseResult<TokenSpan> {
        self.expect(
            |token: &Token| matches!(token, Token::Ident(_)),
            "identifier",
        )
    }

    /// 期望符号
    pub fn expect_symbol(&mut self, symbol: crate::lexical::Symbol) -> ParseResult<TokenSpan> {
        self.expect(
            |token: &Token| matches!(token, Token::Symbol(s) if *s == symbol),
            &format!("symbol '{}'", symbol.as_str()),
        )
    }

    /// 期望关键字
    pub fn expect_keyword(&mut self, keyword: crate::lexical::Keyword) -> ParseResult<TokenSpan> {
        self.expect(
            |token: &Token| matches!(token, Token::Keyword(k) if *k == keyword),
            &format!("keyword '{}'", keyword.as_str()),
        )
    }

    /// 进入错误恢复模式
    pub fn push_recovery_point(&mut self, recovery_type: RecoveryType) {
        self.recovery_stack.push(RecoveryPoint {
            iter: self.iter.clone(),
            current_position: self.current_position,
            recovery_type,
        });
    }

    /// 退出错误恢复模式
    pub fn pop_recovery_point(&mut self) -> Option<RecoveryPoint<'i>> {
        self.recovery_stack.pop()
    }

    /// 恢复到最近的恢复点
    pub fn recover_to_point(&mut self) -> bool {
        if let Some(point) = self.recovery_stack.last() {
            // 直接使用保存的迭代器状态和位置
            self.iter = point.iter.clone();
            self.current_position = point.current_position;
            true
        } else {
            false
        }
    }

    /// 尝试解析，支持错误恢复
    pub fn try_parse<T, F>(&mut self, parse_fn: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
    {
        // 保存解析前的状态
        let saved_iter = self.iter.clone();

        match parse_fn(self) {
            Ok(result) => Some(result),
            Err(error) => {
                // 记录错误
                let diagnostic: crate::diagnostic::Diagnostic = error.clone().into();
                self.diagnostics.add(diagnostic);

                // 如果启用错误恢复，恢复到解析前的位置
                if self.config.enable_error_recovery {
                    self.iter = saved_iter;
                }
                None
            }
        }
    }

    /// 解析逗号分隔的列表
    pub fn parse_punctuated<T, F>(
        &mut self,
        parse_fn: F,
        separator: crate::lexical::Symbol,
    ) -> ParseResult<Punctuated<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        let mut items = Vec::new();
        let mut last = None;

        loop {
            match self.try_parse(&parse_fn) {
                Some(item) => {
                    if self.next_is(
                        |token: &Token| matches!(token, Token::Symbol(s) if *s == separator),
                    ) {
                        items.push((item, self.expect_symbol(separator)?));
                    } else {
                        last = Some(Box::new(item));
                        break;
                    }
                }
                None => break,
            }
        }

        Ok(Punctuated { items, last })
    }

    /// 解析可选的内容
    pub fn parse_optional<T, F>(&mut self, parse_fn: F) -> Option<T>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.try_parse(parse_fn)
    }

    /// 创建诊断错误
    pub fn create_error(
        &self,
        message: String,
        span: Span,
        expected: Option<String>,
        found: Option<String>,
    ) -> ParseError {
        ParseError::new(message)
            .with_span(span)
            .with_expected(expected.unwrap_or_default())
            .with_found(found.unwrap_or_default())
    }

    /// 创建EOF错误
    pub fn create_eof_error(&self, expected: &str) -> ParseError {
        let span = if let Some(last_token) = self.iter.clone().last() {
            last_token.span
        } else {
            Span::dummy()
        };

        ParseError::new("Unexpected end of input")
            .with_span(span)
            .with_expected(expected.to_string())
            .with_found("end of file".to_string())
    }

    /// 设置LSP状态
    pub fn set_lsp_state(&mut self, lsp_state: LspState) {
        self.lsp_state = Some(lsp_state);
    }

    /// 检查是否靠近光标位置（用于LSP优化）
    pub fn is_near_cursor(&self, span: Span) -> bool {
        self.lsp_state
            .as_ref()
            .and_then(|state| state.cursor_position)
            .map_or(false, |cursor| {
                // 简化实现：检查光标是否在span范围内
                cursor.start >= span.start && cursor.end <= span.end
            })
    }
}

/// Token匹配trait
pub trait TokenMatcher {
    fn matches(&self, token: &Token) -> bool;
}

impl TokenMatcher for Token {
    fn matches(&self, token: &Token) -> bool {
        self == token
    }
}

impl TokenMatcher for crate::lexical::Symbol {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Symbol(s) if s == self)
    }
}

impl TokenMatcher for crate::lexical::Keyword {
    fn matches(&self, token: &Token) -> bool {
        matches!(token, Token::Keyword(k) if k == self)
    }
}

impl<F> TokenMatcher for F
where
    F: Fn(&Token) -> bool,
{
    fn matches(&self, token: &Token) -> bool {
        self(token)
    }
}

impl Parse for u32 {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        if let Some(Token::Literal(Literal::Integer(i))) = cx.peek_token() {
            let index = *i;
            let peek = cx.consume()?;
            if index < 0 || index > u32::MAX as i64 {
                return Err(cx.create_error(
                    format!("integer literal {index} is out of range for `u32`"),
                    peek.span,
                    Some("u32".to_string()),
                    Some(peek.value().to_string()),
                ));
            }
            return Ok(index as u32);
        }

        match cx.peek() {
            Some(peek) => {
                let peek = peek.clone();
                Err(cx.create_error(
                    "Invalid u32".to_string(),
                    peek.span(),
                    Some("u32".to_string()),
                    Some(peek.value().to_string()),
                ))
            }
            None => Err(cx.create_eof_error("u32")),
        }
    }
}
