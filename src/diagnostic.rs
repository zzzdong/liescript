use std::ops::Range;
use std::{fmt, ops::Deref};

use crate::source::FileId;

/// 错误严重级别
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

/// 错误来源阶段
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    Lexical,   // 词法分析
    Parse,     // 语法分析
    TypeCheck, // 类型检查
    Semantic,  // 语义分析
    CodeGen,   // 代码生成
    Linking,   // 链接阶段
    Runtime,   // 运行时错误
    Other,     // 其他错误
}

/// 源代码位置范围
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub file: FileId,
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// 创建新Span
    pub fn new(file: FileId, start: u32, end: u32) -> Self {
        Span { file, start, end }
    }

    pub fn dummy() -> Self {
        Span {
            file: FileId::new(0),
            start: 0,
            end: 0,
        }
    }

    pub fn split(&self, offset: u32) -> (Span, Span) {
        (
            Span {
                file: self.file,
                start: self.start,
                end: self.start + offset,
            },
            Span {
                file: self.file,
                start: self.start + offset,
                end: self.end,
            },
        )
    }

    /// 转换为Range<usize>
    pub fn to_range(&self) -> Range<usize> {
        self.start as usize..self.end as usize
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Spanned { value, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn file(&self) -> FileId {
        self.span.file
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
}

impl<T: PartialEq> PartialEq<Spanned<T>> for Spanned<T> {
    fn eq(&self, other: &Spanned<T>) -> bool {
        self.value == other.value
    }
}

impl<T: PartialEq> PartialEq<T> for Spanned<T> {
    fn eq(&self, other: &T) -> bool {
        &self.value == other
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> From<T> for Spanned<T> {
    fn from(value: T) -> Self {
        Spanned::new(value, Span::dummy())
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}

/// 统一诊断信息结构
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// 错误严重级别
    pub severity: Severity,
    /// 错误来源阶段
    pub phase: Phase,
    /// 错误代码 (e.g., "E001")
    pub code: String,
    /// 人类可读的错误消息
    pub message: String,
    /// 源代码位置范围
    pub span: Span,
    /// 相关上下文信息
    pub context: Option<String>,
}

impl Diagnostic {
    /// 创建新诊断
    pub fn new(severity: Severity, phase: Phase, code: &str, message: &str, span: Span) -> Self {
        Diagnostic {
            severity,
            phase,
            code: code.to_string(),
            message: message.to_string(),
            span,
            context: None,
        }
    }

    /// 添加上下文信息
    pub fn with_context(mut self, context: &str) -> Self {
        self.context = Some(context.to_string());
        self
    }

    /// 转换为编译器控制台输出格式
    pub fn to_console_string(&self, source: &str) -> String {
        let severity_str = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        };

        let phase_str = match self.phase {
            Phase::Lexical => "lexical",
            Phase::Parse => "parse",
            Phase::TypeCheck => "type",
            Phase::Semantic => "semantic",
            Phase::CodeGen => "codegen",
            Phase::Linking => "linking",
            Phase::Runtime => "runtime",
            Phase::Other => "other",
        };

        let mut output = format!(
            "{}[{}]: {}\n  --> {}:{}",
            severity_str, self.code, self.message, phase_str, self.code
        );

        if let Some(context) = &self.context {
            output.push_str(&format!("\n  context: {}", context));
        }

        // 添加源代码片段
        let range = self.span.to_range();
        if range.start < source.len() && range.end <= source.len() {
            let snippet = &source[range.clone()];
            output.push_str(&format!("\n  snippet: '{}'", snippet));
        }

        output
    }

    /// 转换为LSP兼容格式 (简化版)
    pub fn to_lsp_format(&self) -> LspDiagnostic {
        LspDiagnostic {
            range: (self.span.start, self.span.end),
            severity: match self.severity {
                Severity::Error => 1,
                Severity::Warning => 2,
                Severity::Note => 3,
            },
            code: self.code.clone(),
            message: self.message.clone(),
            source: match self.phase {
                Phase::Lexical => "lexer".to_string(),
                Phase::Parse => "parser".to_string(),
                Phase::TypeCheck => "typechecker".to_string(),
                Phase::Semantic => "semantic".to_string(),
                Phase::CodeGen => "codegen".to_string(),
                Phase::Linking => "linker".to_string(),
                Phase::Runtime => "runtime".to_string(),
                Phase::Other => "compiler".to_string(),
            },
        }
    }
}

/// LSP兼容的诊断结构
#[derive(Debug, Clone)]
pub struct LspDiagnostic {
    pub range: (u32, u32),
    pub severity: u8,
    pub code: String,
    pub message: String,
    pub source: String,
}

/// 诊断收集器
#[derive(Debug, Default)]
pub struct Diagnostics {
    errors: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics { errors: Vec::new() }
    }

    /// 添加新诊断
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.errors.push(diagnostic);
    }

    /// 检查是否有错误
    pub fn has_errors(&self) -> bool {
        self.errors.iter().any(|d| d.severity == Severity::Error)
    }

    /// 获取所有诊断
    pub fn all(&self) -> &[Diagnostic] {
        &self.errors
    }

    /// 转换为控制台输出
    pub fn to_console_output(&self, source: &str) -> String {
        self.errors
            .iter()
            .map(|d| d.to_console_string(source))
            .collect::<Vec<_>>()
            .join("\n\n")
    }

    /// 转换为LSP诊断列表
    pub fn to_lsp_diagnostics(&self) -> Vec<LspDiagnostic> {
        self.errors.iter().map(|d| d.to_lsp_format()).collect()
    }
}
