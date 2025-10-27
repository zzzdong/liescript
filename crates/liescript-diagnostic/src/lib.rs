//! LieScript诊断系统
//!
//! 提供现代化的错误诊断和报告功能，支持多阶段错误处理、
//! 源代码高亮显示、LSP集成等功能。

#![warn(missing_docs)]

pub mod diagnostic;
pub mod lsp;
pub mod reporter;
pub mod source;

// 重新导出主要类型
pub use diagnostic::{Diagnostic, DiagnosticBuilder, DiagnosticKind, Phase, Severity};
pub use reporter::{ConsoleReporter, DiagnosticReporter, LspReporter};
pub use source::SourceMap;

use liescript_lexical::span::{FileId, Span};

/// 诊断系统错误类型
#[derive(Debug, thiserror::Error)]
pub enum DiagnosticError {
    /// 文件未找到错误
    #[error("文件未找到: {file_id}")]
    FileNotFound { file_id: FileId },

    /// 源代码位置无效错误
    #[error("无效的源代码位置: {span:?}")]
    InvalidSpan { span: Span },

    /// 诊断报告错误
    #[error("诊断报告失败: {message}")]
    ReportingError { message: String },
}

/// 诊断系统结果类型
pub type Result<T> = std::result::Result<T, DiagnosticError>;
