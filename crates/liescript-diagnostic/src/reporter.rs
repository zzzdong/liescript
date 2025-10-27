//! 诊断报告器
//! 
//! 提供多种诊断信息输出格式，包括终端彩色输出和LSP兼容格式。

use std::io::{self, Write};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use liescript_lexical::span::{Span};

use crate::diagnostic::{Diagnostic, Severity};
use crate::source::{SourceMap};
use crate::Result;

/// 诊断报告器接口
pub trait DiagnosticReporter {
    /// 报告诊断信息
    fn report(&mut self, diagnostic: &Diagnostic, source_map: &SourceMap) -> Result<()>;
    
    /// 报告多个诊断信息
    fn report_all(&mut self, diagnostics: &[Diagnostic], source_map: &SourceMap) -> Result<()> {
        for diagnostic in diagnostics {
            self.report(diagnostic, source_map)?;
        }
        Ok(())
    }
}

/// 控制台诊断报告器
pub struct ConsoleReporter {
    /// 输出流
    stream: StandardStream,
    /// 是否显示详细信息
    verbose: bool,
}

impl ConsoleReporter {
    /// 创建新的控制台报告器
    pub fn new(verbose: bool) -> Self {
        Self {
            stream: StandardStream::stdout(ColorChoice::Auto),
            verbose,
        }
    }

    /// 设置颜色
    fn set_color(&mut self, color: Color) -> io::Result<()> {
        let mut spec = ColorSpec::new();
        spec.set_fg(Some(color));
        spec.set_bold(true);
        self.stream.set_color(&spec)
    }

    /// 重置颜色
    fn reset_color(&mut self) -> io::Result<()> {
        self.stream.reset()
    }

    /// 格式化位置信息
    fn format_location(&self, span: Span, source_map: &SourceMap) -> String {
        if let Some((path, line, col)) = source_map.position(span) {
            format!("{}:{}:{}", path, line + 1, col + 1)
        } else {
            "<unknown location>".to_string()
        }
    }

    /// 格式化源代码片段
    fn format_snippet(&self, diagnostic: &Diagnostic, source_map: &SourceMap) -> String {
        if let Some(snippet) = source_map.snippet(diagnostic.span) {
            if let Some((_path, line, _)) = source_map.position(diagnostic.span) {
                let line_num = line + 1;
                let line_content = source_map
                    .line_content(diagnostic.file_id(), line)
                    .unwrap_or_default();
                
                let mut result = format!("{} | {}\n", line_num, line_content.trim_end());
                
                // 添加指示器
                let indicator_len = diagnostic.span.len() as usize;
                let indicator = "^".repeat(indicator_len.max(1));
                result.push_str(&format!("  | {}{}", " ".repeat(line_content.len() - snippet.len()), indicator));
                
                result
            } else {
                format!("  {}", snippet)
            }
        } else {
            "<no source available>".to_string()
        }
    }
}

impl DiagnosticReporter for ConsoleReporter {
    fn report(&mut self, diagnostic: &Diagnostic, source_map: &SourceMap) -> Result<()> {
        // 设置颜色
        let color = match diagnostic.severity {
            Severity::Error => Color::Red,
            Severity::Warning => Color::Yellow,
            Severity::Info => Color::Blue,
            Severity::Hint => Color::Cyan,
        };

        self.set_color(color).map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("设置颜色失败: {}", e),
        })?;

        // 输出错误标题
        write!(
            &mut self.stream,
            "{}[{}]: {}\n",
            diagnostic.severity, diagnostic.code, diagnostic.message
        )
        .map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("写入失败: {}", e),
        })?;

        self.reset_color().map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("重置颜色失败: {}", e),
        })?;

        // 输出位置信息
        let location = self.format_location(diagnostic.span, source_map);
        writeln!(&mut self.stream, "  --> {}", location).map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("写入失败: {}", e),
        })?;

        // 输出源代码片段
        let snippet = self.format_snippet(diagnostic, source_map);
        writeln!(&mut self.stream, "{}", snippet).map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("写入失败: {}", e),
        })?;

        // 输出详细信息（如果启用）
        if self.verbose {
            if let Some(description) = &diagnostic.description {
                writeln!(&mut self.stream, "  note: {}", description).map_err(|e| crate::DiagnosticError::ReportingError {
                    message: format!("写入失败: {}", e),
                })?;
            }

            if let Some(suggestion) = &diagnostic.suggestion {
                self.set_color(Color::Green).map_err(|e| crate::DiagnosticError::ReportingError {
                    message: format!("设置颜色失败: {}", e),
                })?;
                writeln!(&mut self.stream, "  help: {}", suggestion).map_err(|e| crate::DiagnosticError::ReportingError {
                    message: format!("写入失败: {}", e),
                })?;
                self.reset_color().map_err(|e| crate::DiagnosticError::ReportingError {
                    message: format!("重置颜色失败: {}", e),
                })?;
            }

            if let Some(help_url) = &diagnostic.help_url {
                writeln!(&mut self.stream, "  see: {}", help_url).map_err(|e| crate::DiagnosticError::ReportingError {
                    message: format!("写入失败: {}", e),
                })?;
            }
        }

        // 输出相关诊断信息
        for related in &diagnostic.related {
            writeln!(&mut self.stream, "  related:").map_err(|e| crate::DiagnosticError::ReportingError {
                message: format!("写入失败: {}", e),
            })?;
            self.report(related, source_map)?;
        }

        writeln!(&mut self.stream).map_err(|e| crate::DiagnosticError::ReportingError {
            message: format!("写入失败: {}", e),
        })?;

        Ok(())
    }
}

/// LSP诊断报告器
pub struct LspReporter {
    /// 收集的诊断信息
    diagnostics: Vec<lsp_types::Diagnostic>,
}

impl LspReporter {
    /// 创建新的LSP报告器
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// 获取所有诊断信息
    pub fn into_diagnostics(self) -> Vec<lsp_types::Diagnostic> {
        self.diagnostics
    }

    /// 转换为LSP严重级别
    fn to_lsp_severity(severity: Severity) -> Option<lsp_types::DiagnosticSeverity> {
        match severity {
            Severity::Error => Some(lsp_types::DiagnosticSeverity::ERROR),
            Severity::Warning => Some(lsp_types::DiagnosticSeverity::WARNING),
            Severity::Info => Some(lsp_types::DiagnosticSeverity::INFORMATION),
            Severity::Hint => Some(lsp_types::DiagnosticSeverity::HINT),
        }
    }

    /// 转换为LSP位置
    fn to_lsp_range(&self, span: Span, source_map: &SourceMap) -> lsp_types::Range {
        if let Some((_, start_line, start_col)) = source_map.position(span) {
            let end_pos = if let Some((_, end_line, end_col)) = source_map.position(Span::new(span.file, span.end, span.end)) {
                (end_line, end_col)
            } else {
                (start_line, start_col + 1)
            };

            lsp_types::Range {
                start: lsp_types::Position {
                    line: start_line as u32,
                    character: start_col as u32,
                },
                end: lsp_types::Position {
                    line: end_pos.0 as u32,
                    character: end_pos.1 as u32,
                },
            }
        } else {
            // 默认范围
            lsp_types::Range {
                start: lsp_types::Position { line: 0, character: 0 },
                end: lsp_types::Position { line: 0, character: 1 },
            }
        }
    }
}

impl Default for LspReporter {
    fn default() -> Self {
        Self::new()
    }
}

impl DiagnosticReporter for LspReporter {
    fn report(&mut self, diagnostic: &Diagnostic, source_map: &SourceMap) -> Result<()> {
        let lsp_diagnostic = lsp_types::Diagnostic {
            range: self.to_lsp_range(diagnostic.span, source_map),
            severity: Self::to_lsp_severity(diagnostic.severity),
            code: Some(lsp_types::NumberOrString::String(diagnostic.code.clone())),
            code_description: diagnostic.help_url.as_ref().map(|url| lsp_types::CodeDescription {
                href: lsp_types::Url::parse(url).unwrap_or_else(|_| lsp_types::Url::parse("https://docs.liescript.dev").unwrap()),
            }),
            source: Some(diagnostic.phase.to_string()),
            message: diagnostic.message.clone(),
            related_information: if diagnostic.related.is_empty() {
                None
            } else {
                Some(
                    diagnostic
                        .related
                        .iter()
                        .map(|related| lsp_types::DiagnosticRelatedInformation {
                            location: lsp_types::Location {
                                uri: lsp_types::Url::parse(&format!("file://{}", source_map.get_file_path(related.file_id()).unwrap_or("unknown"))).unwrap(),
                                range: self.to_lsp_range(related.span, source_map),
                            },
                            message: related.message.clone(),
                        })
                        .collect(),
                )
            },
            tags: None,
            data: None,
        };

        self.diagnostics.push(lsp_diagnostic);
        Ok(())
    }
}

/// 诊断收集器
#[derive(Debug)]
pub struct DiagnosticCollector {
    /// 诊断信息列表
    diagnostics: Vec<Diagnostic>,
    /// 源代码管理器
    source_map: SourceMap,
}

impl DiagnosticCollector {
    /// 创建新的诊断收集器
    pub fn new(source_map: SourceMap) -> Self {
        Self {
            diagnostics: Vec::new(),
            source_map,
        }
    }

    /// 添加诊断信息
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// 检查是否有错误
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_error())
    }

    /// 获取所有诊断信息
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// 获取源代码管理器
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// 报告到控制台
    pub fn report_to_console(&self, verbose: bool) -> Result<()> {
        let mut reporter = ConsoleReporter::new(verbose);
        reporter.report_all(&self.diagnostics, &self.source_map)
    }

    /// 转换为LSP诊断信息
    pub fn to_lsp_diagnostics(&self) -> Result<Vec<lsp_types::Diagnostic>> {
        let mut reporter = LspReporter::new();
        reporter.report_all(&self.diagnostics, &self.source_map)?;
        Ok(reporter.into_diagnostics())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::{Diagnostic, Phase, Severity};
    use liescript_lexical::{FileId, Span};

    #[test]
    fn test_console_reporter() {
        let mut source_map = SourceMap::new();
        let file_id = source_map.add_file("test.lie".to_string(), "let x = 42;".to_string());
        let span = Span::new(file_id, 0, 3);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            span,
        );

        let mut reporter = ConsoleReporter::new(false);
        let result = reporter.report(&diagnostic, &source_map);
        assert!(result.is_ok());
    }

    #[test]
    fn test_lsp_reporter() {
        let mut source_map = SourceMap::new();
        let file_id = source_map.add_file("test.lie".to_string(), "let x = 42;".to_string());
        let span = Span::new(file_id, 0, 3);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            span,
        );

        let mut reporter = LspReporter::new();
        let result = reporter.report(&diagnostic, &source_map);
        assert!(result.is_ok());

        let diagnostics = reporter.into_diagnostics();
        assert_eq!(diagnostics.len(), 1);
    }

    #[test]
    fn test_diagnostic_collector() {
        let source_map = SourceMap::new();
        let mut collector = DiagnosticCollector::new(source_map);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            Span::dummy(),
        );

        collector.add(diagnostic);
        assert!(collector.has_errors());
        assert_eq!(collector.diagnostics().len(), 1);
    }
}