//! 解析器诊断和错误处理模块

use liescript_diagnostic::diagnostic::{Diagnostic, DiagnosticBuilder, Phase, Severity};
use liescript_lexical::Span;

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
    /// 错误代码
    pub code: String,
    /// 详细描述
    pub description: Option<String>,
    /// 修复建议
    pub suggestion: Option<String>,
}

impl ParseError {
    /// 创建新的解析错误
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            expected: Vec::new(),
            found: None,
            code: "E001".to_string(),
            description: None,
            suggestion: None,
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

    /// 设置错误代码
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = code.into();
        self
    }

    /// 设置详细描述
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// 设置修复建议
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// 创建EOF错误
    pub fn eof() -> Self {
        Self::new("unexpected end of input")
            .with_code("E010")
            .with_description("在期望更多输入时遇到了文件结束")
            .with_suggestion("检查是否缺少分号、括号或其他语法元素")
    }

    /// 创建语法错误
    pub fn syntax_error(message: impl Into<String>, span: Span) -> Self {
        Self::new(message)
            .with_span(span)
            .with_code("E001")
            .with_description("语法结构不符合LieScript语言规范")
            .with_suggestion("检查语法规则，确保代码结构正确")
    }

    /// 创建括号不匹配错误
    pub fn bracket_error(message: impl Into<String>, span: Span) -> Self {
        Self::new(message)
            .with_span(span)
            .with_code("E002")
            .with_description("括号、大括号或方括号不匹配")
            .with_suggestion("检查所有括号是否成对出现")
    }

    /// 创建Token不匹配错误
    pub fn token_mismatch(expected: &str, found: &str, span: Span) -> Self {
        Self::syntax_error(
            format!("expected '{}', but found '{}'", expected, found),
            span,
        )
        .with_expected(expected.to_string())
        .with_found(found.to_string())
    }

    /// 创建括号不匹配错误
    pub fn bracket_mismatch(open_bracket: &str, close_bracket: &str, span: Span) -> Self {
        Self::bracket_error(
            format!(
                "mismatched brackets: expected '{}' but found '{}'",
                open_bracket, close_bracket
            ),
            span,
        )
    }

    /// 创建操作符错误
    pub fn operator_error(message: impl Into<String>, span: Span) -> Self {
        Self::syntax_error(message, span)
            .with_code("E003")
            .with_description("操作符使用不符合语法规则")
            .with_suggestion("检查操作符的优先级和结合性")
    }

    /// 创建表达式错误
    pub fn expression_error(message: impl Into<String>, span: Span) -> Self {
        Self::syntax_error(message, span)
            .with_code("E004")
            .with_description("表达式结构不符合语法规则")
            .with_suggestion("检查表达式的语法结构")
    }

    /// 创建语句错误
    pub fn statement_error(message: impl Into<String>, span: Span) -> Self {
        Self::syntax_error(message, span)
            .with_code("E005")
            .with_description("语句结构不符合语法规则")
            .with_suggestion("检查语句的语法结构")
    }

    /// 创建声明错误
    pub fn declaration_error(message: impl Into<String>, span: Span) -> Self {
        Self::syntax_error(message, span)
            .with_code("E006")
            .with_description("声明语法不符合规则")
            .with_suggestion("检查变量、函数或其他声明的语法")
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
        let mut builder = DiagnosticBuilder::new(
            Severity::Error,
            Phase::Parse,
            &error.code,
            &error.message,
            error.span.unwrap_or_else(Span::dummy),
        );

        // 构建完整的错误消息
        let mut full_message = error.message.clone();
        if !error.expected.is_empty() {
            full_message.push_str(&format!(", expected {}", error.expected.join(", ")));
        }
        if let Some(found) = &error.found {
            full_message.push_str(&format!(", but found {}", found));
        }

        builder = builder.description(&full_message);

        if let Some(description) = &error.description {
            builder = builder.description(description);
        }

        if let Some(suggestion) = &error.suggestion {
            builder = builder.suggestion(suggestion);
        }

        builder.build()
    }
}
