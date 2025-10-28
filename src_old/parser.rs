//! 解析器模块 - 新的Parser架构

pub mod config;
pub mod context;
pub mod diagnostic;
pub mod expression;
pub mod item;
pub mod name;
pub mod pattern;
pub mod statement;
pub mod r#type;

// 重新导出主要类型
pub use config::ParserConfig;
pub use context::ParseContext;
pub use diagnostic::{ParseError, ParseResult};

use crate::diagnostic::Spanned;

/// 主解析器结构
pub struct Parser {
    config: ParserConfig,
}

impl Parser {
    /// 创建新的解析器
    pub fn new(config: ParserConfig) -> Self {
        Self { config }
    }
    
    /// 解析Token流为AST
    pub fn parse(&self, tokens: &crate::lexical::TokenStream) -> ParseResult<()> {
        // TODO: 实现文件解析逻辑
        unimplemented!()
    }
    
    /// 解析单个表达式（用于REPL或LSP）
    pub fn parse_expression(&self, tokens: &crate::lexical::TokenStream) -> ParseResult<()> {
        // TODO: 实现表达式解析逻辑
        unimplemented!()
    }
}

/// Parse trait - 用于语法组件的解析
pub trait Parse where Self: Sized {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self>;
}
