//! 类型测试模块
//!
//! 测试各种类型的解析，包括基本类型、泛型类型等

use liescript_ast::Type;
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
fn test_primitive_type_parsing() {
    let input = "i32";

    let parsed = test_parse::<Type>(input).unwrap();
    assert!(
        matches!(parsed, Type::Path(_)),
        "输入 '{}' 应该解析为类型",
        input
    );
}

#[test]
fn test_generic_type_parsing() {
    let input = "Vec<i32>";

    let parsed = test_parse::<Type>(input).unwrap();
    assert!(
        matches!(parsed, Type::Path(_)),
        "输入 '{}' 应该解析为类型",
        input
    );
}