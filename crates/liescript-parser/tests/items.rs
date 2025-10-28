//! 项（Item）测试模块
//!
//! 测试各种项的解析，包括函数声明、结构体声明等

use liescript_ast::Item;
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
fn test_simple_function_declaration() {
    let input = "fn main() {}";

    let parsed = test_parse::<Item>(input).unwrap();
    assert!(
        matches!(parsed, Item::Function(_)),
        "输入 '{}' 应该解析为函数声明",
        input
    );
}

#[test]
fn test_parameterized_function_declaration() {
    let input = "fn add(a: i32, b: i32) -> i32 { a + b }";

    let parsed = test_parse::<Item>(input).unwrap();
    assert!(
        matches!(parsed, Item::Function(_)),
        "输入 '{}' 应该解析为函数声明",
        input
    );
}

#[test]
fn test_struct_with_fields() {
    let input = "struct Point { x: i32, y: i32 }";

    let parsed = test_parse::<Item>(input).unwrap();
    assert!(
        matches!(parsed, Item::Struct(_)),
        "输入 '{}' 应该解析为结构体声明",
        input
    );
}

#[test]
fn test_empty_struct() {
    let input = "struct Empty;";

    let parsed = test_parse::<Item>(input).unwrap();
    assert!(
        matches!(parsed, Item::Struct(_)),
        "输入 '{}' 应该解析为结构体声明",
        input
    );
}