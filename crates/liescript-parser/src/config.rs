//! 解析器配置模块

use liescript_lexical::FileId;



/// 解析器配置选项
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// 是否启用错误恢复机制
    pub enable_error_recovery: bool,
    /// 是否启用LSP支持
    pub enable_lsp_support: bool,
    /// 最大错误数量限制
    pub max_errors: usize,
    /// 文件ID，用于诊断信息
    pub file_id: FileId,
    /// 是否启用增量解析（LSP优化）
    pub enable_incremental_parsing: bool,
    /// 是否记录详细的解析日志
    pub enable_debug_logging: bool,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            enable_error_recovery: true,
            enable_lsp_support: false,
            max_errors: 100,
            file_id: FileId::new(0),
            enable_incremental_parsing: false,
            enable_debug_logging: false,
        }
    }
}

impl ParserConfig {
    /// 创建新的解析器配置
    pub fn new(file_id: FileId) -> Self {
        Self {
            file_id,
            ..Default::default()
        }
    }
    
    /// 启用LSP支持
    pub fn with_lsp_support(mut self) -> Self {
        self.enable_lsp_support = true;
        self.enable_incremental_parsing = true;
        self
    }
    
    /// 禁用错误恢复
    pub fn without_error_recovery(mut self) -> Self {
        self.enable_error_recovery = false;
        self
    }
    
    /// 设置最大错误数量
    pub fn with_max_errors(mut self, max_errors: usize) -> Self {
        self.max_errors = max_errors;
        self
    }
}