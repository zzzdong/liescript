//! LSP协议集成模块
//!
//! 提供Language Server Protocol兼容的诊断信息格式转换和集成功能。

use liescript_lexical::span::Span;
use lsp_types;
use serde::{Deserialize, Serialize};

use crate::diagnostic::{Diagnostic, Severity};
use crate::source::SourceMap;

/// LSP诊断适配器
#[derive(Debug)]
pub struct LspDiagnosticAdapter {
    /// 源代码管理器
    source_map: SourceMap,
}

impl LspDiagnosticAdapter {
    /// 创建新的LSP诊断适配器
    pub fn new(source_map: SourceMap) -> Self {
        Self { source_map }
    }

    /// 将诊断信息转换为LSP格式
    pub fn to_lsp_diagnostic(&self, diagnostic: &Diagnostic) -> lsp_types::Diagnostic {
        lsp_types::Diagnostic {
            range: self.to_lsp_range(diagnostic.span),
            severity: self.to_lsp_severity(diagnostic.severity),
            code: Some(lsp_types::NumberOrString::String(diagnostic.code.clone())),
            code_description: diagnostic
                .help_url
                .as_ref()
                .map(|url| lsp_types::CodeDescription {
                    href: lsp_types::Url::parse(url).unwrap_or_else(|_| {
                        lsp_types::Url::parse("https://docs.liescript.dev").unwrap()
                    }),
                }),
            source: Some(diagnostic.phase.to_string()),
            message: self.build_message(diagnostic),
            related_information: self.build_related_information(diagnostic),
            tags: self.build_tags(diagnostic),
            data: None,
        }
    }

    /// 构建完整的错误消息
    fn build_message(&self, diagnostic: &Diagnostic) -> String {
        let mut message = diagnostic.message.clone();

        if let Some(description) = &diagnostic.description {
            message.push_str(&format!("\n\n{}", description));
        }

        if let Some(suggestion) = &diagnostic.suggestion {
            message.push_str(&format!("\n\n建议: {}", suggestion));
        }

        message
    }

    /// 构建相关错误信息
    fn build_related_information(
        &self,
        diagnostic: &Diagnostic,
    ) -> Option<Vec<lsp_types::DiagnosticRelatedInformation>> {
        if diagnostic.related.is_empty() {
            return None;
        }

        let related_info: Vec<_> = diagnostic
            .related
            .iter()
            .filter_map(|related| {
                let file_path = self.source_map.get_file_path(related.file_id())?;
                let uri = lsp_types::Url::parse(&format!("file://{}", file_path)).ok()?;

                Some(lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location {
                        uri,
                        range: self.to_lsp_range(related.span),
                    },
                    message: related.message.clone(),
                })
            })
            .collect();

        if related_info.is_empty() {
            None
        } else {
            Some(related_info)
        }
    }

    /// 构建标签信息
    fn build_tags(&self, diagnostic: &Diagnostic) -> Option<Vec<lsp_types::DiagnosticTag>> {
        let mut tags = Vec::new();

        // 根据严重级别添加标签
        match diagnostic.severity {
            Severity::Warning => {
                if diagnostic.labels.iter().any(|l| l.contains("unused")) {
                    tags.push(lsp_types::DiagnosticTag::UNNECESSARY);
                }
                if diagnostic.labels.iter().any(|l| l.contains("deprecated")) {
                    tags.push(lsp_types::DiagnosticTag::DEPRECATED);
                }
            }
            Severity::Error => {
                // 错误级别可以添加特定标签
                if diagnostic.labels.iter().any(|l| l.contains("fatal")) {
                    // 可以添加自定义标签
                }
            }
            _ => {}
        }

        if tags.is_empty() { None } else { Some(tags) }
    }

    /// 转换为LSP范围
    fn to_lsp_range(&self, span: Span) -> lsp_types::Range {
        if let Some((_, start_line, start_col)) = self.source_map.position(span) {
            let end_pos = if let Some((_, end_line, end_col)) = self
                .source_map
                .position(Span::new(span.file, span.end, span.end))
            {
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
                start: lsp_types::Position {
                    line: 0,
                    character: 0,
                },
                end: lsp_types::Position {
                    line: 0,
                    character: 1,
                },
            }
        }
    }

    /// 转换为LSP严重级别
    fn to_lsp_severity(&self, severity: Severity) -> Option<lsp_types::DiagnosticSeverity> {
        match severity {
            Severity::Error => Some(lsp_types::DiagnosticSeverity::ERROR),
            Severity::Warning => Some(lsp_types::DiagnosticSeverity::WARNING),
            Severity::Info => Some(lsp_types::DiagnosticSeverity::INFORMATION),
            Severity::Hint => Some(lsp_types::DiagnosticSeverity::HINT),
        }
    }
}

/// LSP诊断发布器
#[derive(Debug)]
pub struct LspDiagnosticPublisher {
    /// 诊断适配器
    adapter: LspDiagnosticAdapter,
    /// 已发布的诊断信息
    published_diagnostics: Vec<lsp_types::Diagnostic>,
}

impl LspDiagnosticPublisher {
    /// 创建新的LSP诊断发布器
    pub fn new(source_map: SourceMap) -> Self {
        Self {
            adapter: LspDiagnosticAdapter::new(source_map),
            published_diagnostics: Vec::new(),
        }
    }

    /// 发布诊断信息
    pub fn publish(&mut self, diagnostic: &Diagnostic) {
        let lsp_diagnostic = self.adapter.to_lsp_diagnostic(diagnostic);
        self.published_diagnostics.push(lsp_diagnostic);
    }

    /// 发布多个诊断信息
    pub fn publish_all(&mut self, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            self.publish(diagnostic);
        }
    }

    /// 清除所有已发布的诊断信息
    pub fn clear(&mut self) {
        self.published_diagnostics.clear();
    }

    /// 获取已发布的诊断信息
    pub fn get_diagnostics(&self) -> &[lsp_types::Diagnostic] {
        &self.published_diagnostics
    }

    /// 转换为LSP发布诊断参数
    pub fn to_publish_diagnostics_params(
        &self,
        uri: lsp_types::Url,
    ) -> lsp_types::PublishDiagnosticsParams {
        lsp_types::PublishDiagnosticsParams {
            uri,
            diagnostics: self.published_diagnostics.clone(),
            version: None,
        }
    }
}

/// LSP诊断配置
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspDiagnosticConfig {
    /// 是否启用诊断
    pub enabled: bool,
    /// 最大诊断数量
    pub max_diagnostics: usize,
    /// 是否包含信息级别诊断
    pub include_info: bool,
    /// 是否包含提示级别诊断
    pub include_hints: bool,
    /// 诊断延迟（毫秒）
    pub delay_ms: u64,
}

impl Default for LspDiagnosticConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            max_diagnostics: 100,
            include_info: true,
            include_hints: false,
            delay_ms: 100,
        }
    }
}

/// LSP诊断管理器
#[derive(Debug)]
pub struct LspDiagnosticManager {
    /// 配置
    config: LspDiagnosticConfig,
    /// 诊断发布器
    publisher: LspDiagnosticPublisher,
    /// 待处理的诊断信息
    pending_diagnostics: Vec<Diagnostic>,
}

impl LspDiagnosticManager {
    /// 创建新的LSP诊断管理器
    pub fn new(source_map: SourceMap, config: LspDiagnosticConfig) -> Self {
        Self {
            config,
            publisher: LspDiagnosticPublisher::new(source_map),
            pending_diagnostics: Vec::new(),
        }
    }

    /// 添加诊断信息
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        if !self.config.enabled {
            return;
        }

        // 过滤不需要的诊断级别
        if !self.should_include(&diagnostic) {
            return;
        }

        self.pending_diagnostics.push(diagnostic);

        // 限制诊断数量
        if self.pending_diagnostics.len() > self.config.max_diagnostics {
            self.pending_diagnostics
                .truncate(self.config.max_diagnostics);
        }
    }

    /// 检查是否应该包含该诊断
    fn should_include(&self, diagnostic: &Diagnostic) -> bool {
        match diagnostic.severity {
            Severity::Error | Severity::Warning => true,
            Severity::Info => self.config.include_info,
            Severity::Hint => self.config.include_hints,
        }
    }

    /// 发布所有待处理的诊断信息
    pub fn publish_all(&mut self) {
        self.publisher.publish_all(&self.pending_diagnostics);
        self.pending_diagnostics.clear();
    }

    /// 获取诊断发布器
    pub fn publisher(&self) -> &LspDiagnosticPublisher {
        &self.publisher
    }

    /// 获取配置
    pub fn config(&self) -> &LspDiagnosticConfig {
        &self.config
    }

    /// 更新配置
    pub fn update_config(&mut self, config: LspDiagnosticConfig) {
        self.config = config;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::{Diagnostic, Phase, Severity};
    use liescript_lexical::{FileId, Span};

    #[test]
    fn test_lsp_diagnostic_adapter() {
        let source_map = SourceMap::new();
        let adapter = LspDiagnosticAdapter::new(source_map);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            Span::dummy(),
        );

        let lsp_diagnostic = adapter.to_lsp_diagnostic(&diagnostic);
        assert_eq!(
            lsp_diagnostic.code,
            Some(lsp_types::NumberOrString::String("E001".to_string()))
        );
        assert_eq!(lsp_diagnostic.message, "语法错误");
    }

    #[test]
    fn test_lsp_diagnostic_publisher() {
        let source_map = SourceMap::new();
        let mut publisher = LspDiagnosticPublisher::new(source_map);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            Span::dummy(),
        );

        publisher.publish(&diagnostic);
        assert_eq!(publisher.get_diagnostics().len(), 1);

        publisher.clear();
        assert!(publisher.get_diagnostics().is_empty());
    }

    #[test]
    fn test_lsp_diagnostic_manager() {
        let source_map = SourceMap::new();
        let config = LspDiagnosticConfig::default();
        let mut manager = LspDiagnosticManager::new(source_map, config);

        let diagnostic = Diagnostic::new(
            Severity::Error,
            Phase::Parse,
            "E001",
            "语法错误",
            Span::dummy(),
        );

        manager.add_diagnostic(diagnostic);
        manager.publish_all();

        assert_eq!(manager.publisher().get_diagnostics().len(), 1);
    }
}
