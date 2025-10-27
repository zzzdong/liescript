//! 诊断信息核心数据结构
//! 
//! 定义诊断信息的核心类型，包括错误分级、错误类型、
//! 诊断信息构建器等。

use std::fmt;
use serde::{Serialize, Deserialize};
use liescript_lexical::span::{FileId, Span};

/// 错误严重级别
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Severity {
    /// 错误 - 阻止编译继续
    Error = 1,
    /// 警告 - 可能有问题但可以继续编译
    Warning = 2,
    /// 信息 - 提供额外信息
    Info = 3,
    /// 提示 - 代码改进建议
    Hint = 4,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Info => write!(f, "info"),
            Severity::Hint => write!(f, "hint"),
        }
    }
}

/// 错误来源阶段
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Phase {
    /// 词法分析阶段
    Lexical,
    /// 语法分析阶段
    Parse,
    /// 类型检查阶段
    TypeCheck,
    /// 语义分析阶段
    Semantic,
    /// 代码生成阶段
    CodeGen,
    /// 链接阶段
    Linking,
    /// 运行时错误
    Runtime,
    /// 其他错误
    Other,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Phase::Lexical => write!(f, "lexical"),
            Phase::Parse => write!(f, "parse"),
            Phase::TypeCheck => write!(f, "type-check"),
            Phase::Semantic => write!(f, "semantic"),
            Phase::CodeGen => write!(f, "codegen"),
            Phase::Linking => write!(f, "linking"),
            Phase::Runtime => write!(f, "runtime"),
            Phase::Other => write!(f, "other"),
        }
    }
}

/// 诊断信息类型
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    /// 错误严重级别
    pub severity: Severity,
    /// 错误来源阶段
    pub phase: Phase,
    /// 错误代码 (e.g., "E001")
    pub code: String,
    /// 主要错误消息
    pub message: String,
    /// 源代码位置
    pub span: Span,
    /// 详细说明
    pub description: Option<String>,
    /// 修复建议
    pub suggestion: Option<String>,
    /// 相关文档链接
    pub help_url: Option<String>,
    /// 相关诊断信息（错误链）
    pub related: Vec<Diagnostic>,
    /// 标签信息（用于分类）
    pub labels: Vec<String>,
}

impl Diagnostic {
    /// 创建新的诊断信息
    pub fn new(
        severity: Severity,
        phase: Phase,
        code: impl Into<String>,
        message: impl Into<String>,
        span: Span,
    ) -> Self {
        Self {
            severity,
            phase,
            code: code.into(),
            message: message.into(),
            span,
            description: None,
            suggestion: None,
            help_url: None,
            related: Vec::new(),
            labels: Vec::new(),
        }
    }

    /// 设置详细说明
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// 设置修复建议
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// 设置帮助文档链接
    pub fn with_help_url(mut self, help_url: impl Into<String>) -> Self {
        self.help_url = Some(help_url.into());
        self
    }

    /// 添加相关诊断信息
    pub fn with_related(mut self, related: Diagnostic) -> Self {
        self.related.push(related);
        self
    }

    /// 添加标签
    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.labels.push(label.into());
        self
    }

    /// 检查是否为错误级别
    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    /// 检查是否为警告级别
    pub fn is_warning(&self) -> bool {
        self.severity == Severity::Warning
    }

    /// 获取文件ID
    pub fn file_id(&self) -> FileId {
        self.span.file
    }
}

/// 诊断信息构建器（流畅接口）
#[derive(Debug)]
pub struct DiagnosticBuilder {
    severity: Severity,
    phase: Phase,
    code: String,
    message: String,
    span: Span,
    description: Option<String>,
    suggestion: Option<String>,
    help_url: Option<String>,
    related: Vec<Diagnostic>,
    labels: Vec<String>,
}

impl DiagnosticBuilder {
    /// 开始构建诊断信息
    pub fn new(
        severity: Severity,
        phase: Phase,
        code: impl Into<String>,
        message: impl Into<String>,
        span: Span,
    ) -> Self {
        Self {
            severity,
            phase,
            code: code.into(),
            message: message.into(),
            span,
            description: None,
            suggestion: None,
            help_url: None,
            related: Vec::new(),
            labels: Vec::new(),
        }
    }

    /// 设置详细说明
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// 设置修复建议
    pub fn suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// 设置帮助文档链接
    pub fn help_url(mut self, help_url: impl Into<String>) -> Self {
        self.help_url = Some(help_url.into());
        self
    }

    /// 添加相关诊断信息
    pub fn related(mut self, related: Diagnostic) -> Self {
        self.related.push(related);
        self
    }

    /// 添加标签
    pub fn label(mut self, label: impl Into<String>) -> Self {
        self.labels.push(label.into());
        self
    }

    /// 完成构建
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            severity: self.severity,
            phase: self.phase,
            code: self.code,
            message: self.message,
            span: self.span,
            description: self.description,
            suggestion: self.suggestion,
            help_url: self.help_url,
            related: self.related,
            labels: self.labels,
        }
    }
}

/// 预定义的诊断类型
#[derive(Debug, Clone, Copy)]
pub enum DiagnosticKind {
    /// 语法错误
    SyntaxError,
    /// 类型错误
    TypeError,
    /// 未定义变量
    UndefinedVariable,
    /// 未定义函数
    UndefinedFunction,
    /// 类型不匹配
    TypeMismatch,
    /// 参数数量不匹配
    ArgumentCountMismatch,
    /// 重复定义
    DuplicateDefinition,
    /// 不可达代码
    UnreachableCode,
    /// 未使用变量
    UnusedVariable,
    /// 除零错误
    DivisionByZero,
    /// 内存错误
    MemoryError,
}

impl DiagnosticKind {
    /// 获取错误代码
    pub fn code(&self) -> &'static str {
        match self {
            DiagnosticKind::SyntaxError => "E001",
            DiagnosticKind::TypeError => "E002",
            DiagnosticKind::UndefinedVariable => "E003",
            DiagnosticKind::UndefinedFunction => "E004",
            DiagnosticKind::TypeMismatch => "E005",
            DiagnosticKind::ArgumentCountMismatch => "E006",
            DiagnosticKind::DuplicateDefinition => "E007",
            DiagnosticKind::UnreachableCode => "W001",
            DiagnosticKind::UnusedVariable => "W002",
            DiagnosticKind::DivisionByZero => "E008",
            DiagnosticKind::MemoryError => "E009",
        }
    }

    /// 获取默认严重级别
    pub fn severity(&self) -> Severity {
        match self {
            DiagnosticKind::SyntaxError => Severity::Error,
            DiagnosticKind::TypeError => Severity::Error,
            DiagnosticKind::UndefinedVariable => Severity::Error,
            DiagnosticKind::UndefinedFunction => Severity::Error,
            DiagnosticKind::TypeMismatch => Severity::Error,
            DiagnosticKind::ArgumentCountMismatch => Severity::Error,
            DiagnosticKind::DuplicateDefinition => Severity::Error,
            DiagnosticKind::UnreachableCode => Severity::Warning,
            DiagnosticKind::UnusedVariable => Severity::Warning,
            DiagnosticKind::DivisionByZero => Severity::Error,
            DiagnosticKind::MemoryError => Severity::Error,
        }
    }

    /// 获取默认阶段
    pub fn phase(&self) -> Phase {
        match self {
            DiagnosticKind::SyntaxError => Phase::Parse,
            DiagnosticKind::TypeError => Phase::TypeCheck,
            DiagnosticKind::UndefinedVariable => Phase::Semantic,
            DiagnosticKind::UndefinedFunction => Phase::Semantic,
            DiagnosticKind::TypeMismatch => Phase::TypeCheck,
            DiagnosticKind::ArgumentCountMismatch => Phase::Semantic,
            DiagnosticKind::DuplicateDefinition => Phase::Semantic,
            DiagnosticKind::UnreachableCode => Phase::Semantic,
            DiagnosticKind::UnusedVariable => Phase::Semantic,
            DiagnosticKind::DivisionByZero => Phase::Runtime,
            DiagnosticKind::MemoryError => Phase::Runtime,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use liescript_lexical::Span;

    #[test]
    fn test_diagnostic_creation() {
        let span = Span::new(FileId::new(1), 10, 20);
        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            span,
        )
        .with_description("在表达式末尾缺少分号")
        .with_suggestion("在表达式末尾添加分号");

        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.phase, Phase::Parse);
        assert_eq!(diagnostic.code, "E001");
        assert_eq!(diagnostic.message, "语法错误");
        assert!(diagnostic.description.is_some());
        assert!(diagnostic.suggestion.is_some());
    }

    #[test]
    fn test_diagnostic_builder() {
        let span = Span::new(FileId::new(1), 10, 20);
        let diagnostic = DiagnosticBuilder::new(
            Severity::Warning,
            Phase::Semantic,
            "W001",
            "未使用的变量",
            span,
        )
        .description("变量'x'被声明但从未使用")
        .suggestion("考虑删除此变量或使用它")
        .build();

        assert!(diagnostic.is_warning());
        assert_eq!(diagnostic.phase, Phase::Semantic);
    }

    #[test]
    fn test_diagnostic_kind() {
        let kind = DiagnosticKind::SyntaxError;
        assert_eq!(kind.code(), "E001");
        assert_eq!(kind.severity(), Severity::Error);
        assert_eq!(kind.phase(), Phase::Parse);
    }
}