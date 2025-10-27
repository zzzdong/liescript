//! LieScript解析器模块
//! 
//! 提供语法分析功能，将Token流转换为抽象语法树（AST）。

pub mod config;
pub mod context;
pub mod expressions;
pub mod diagnostic;
pub mod items;
pub mod names;
pub mod patterns;
pub mod statements;
pub mod types;

// 重新导出主要类型
pub use config::ParserConfig;
pub use context::{ParseContext, TokenMatcher};
pub use diagnostic::{ParseError, ParseResult};

/// 解析trait - 用于实现各种语法结构的解析
pub trait Parse {
    /// 从解析上下文中解析语法结构
    fn parse(cx: &mut ParseContext) -> ParseResult<Self>
    where
        Self: Sized;
}

/// 用于解析可选内容的trait
pub trait ParseOptional {
    /// 尝试解析，如果失败则返回None
    fn parse_optional(cx: &mut ParseContext) -> Option<Self>
    where
        Self: Sized;
}

impl<T: Parse> ParseOptional for T {
    fn parse_optional(cx: &mut ParseContext) -> Option<Self> {
        cx.try_parse(Self::parse)
    }
}
