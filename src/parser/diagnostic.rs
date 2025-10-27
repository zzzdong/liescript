//! 解析器诊断和错误处理模块

use crate::diagnostic::{Diagnostic, Span};

/// 解析错误类型
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    /// 错误消息
    pub message: String,
    /// 错误位置
    pub span: Option<Span>,
    /// 期望的Token类型
    pub expected: Vec<String>,
    /// 实际找到的Token
    pub found: Option<String>,
}

impl ParseError {
    /// 创建新的解析错误
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            expected: Vec::new(),
            found: None,
        }
    }

    /// 设置错误位置
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    /// 添加期望的Token类型
    pub fn with_expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    /// 设置实际找到的Token
    pub fn with_found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
        self
    }

    /// 创建EOF错误
    pub fn eof() -> Self {
        Self::new("unexpected end of input")
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

/// 解析结果类型
pub type ParseResult<T> = Result<T, ParseError>;

/// 从ParseError转换为Diagnostic
impl From<ParseError> for Diagnostic {
    fn from(error: ParseError) -> Self {
        let mut diagnostic = Diagnostic {
            severity: crate::diagnostic::Severity::Error,
            phase: crate::diagnostic::Phase::Parse,
            code: "PARSE_ERROR".to_string(),
            message: error.message,
            span: error.span.unwrap_or_else(Span::dummy),
            context: None,
        };

        if !error.expected.is_empty() {
            diagnostic.message = format!(
                "{}, expected {}",
                diagnostic.message,
                error.expected.join(", ")
            );
        }

        if let Some(found) = error.found {
            diagnostic.message = format!("{}, but found {}", diagnostic.message, found);
        }

        diagnostic
    }
}