//! 模式测试模块
//!
//! 测试各种模式的解析，包括标识符模式、通配符模式等

use liescript_ast::Pattern;
use liescript_diagnostic::{SourceMap, reporter::DiagnosticCollector};
use liescript_lexer::TokenStream;
use liescript_parser::{Parse, ParseContext, ParseResult, ParserConfig};

/// 创建测试用的ParseContext
fn test_parse<T: Parse>(input: &str) -> ParseResult<T> {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("test.lie".to_string(), input.to_string());
    let config = ParserConfig::default();
    let mut diagnostic_collector = DiagnosticCollector::new(source_map);
    let tokens = TokenStream::parse(file_id, input).unwrap();
    let mut context = ParseContext::new(&tokens, &config, &mut diagnostic_collector);
    T::parse(&mut context)
}

#[test]
fn test_identifier_pattern() {
    let input = "x";

    let parsed = test_parse::<Pattern>(input).unwrap();
    assert!(
        matches!(parsed, Pattern::Identifier(_)),
        "输入 '{}' 应该解析为标识符模式",
        input
    );
}

#[test]
fn test_wildcard_pattern() {
    let input = "_";

    let parsed = test_parse::<Pattern>(input).unwrap();
    assert!(
        matches!(parsed, Pattern::Wildcard(_)),
        "输入 '{}' 应该解析为通配符模式",
        input
    );
}