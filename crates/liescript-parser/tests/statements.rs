//! 语句测试模块
//!
//! 测试各种语句的解析，包括空语句、let语句等

use liescript_ast::Statement;
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
fn test_empty_statements() {
    let input = ";";

    let parsed = test_parse::<Statement>(input).unwrap();
    assert!(
        matches!(parsed, Statement::Empty(_)),
        "输入 '{}' 应该解析为 Empty 语句",
        input
    );
}

#[test]
fn test_let_statements() {
    let input = "let x = 42;";

    let parsed = test_parse::<Statement>(input).unwrap();
    assert!(
        matches!(parsed, Statement::Let(_)),
        "输入 '{}' 应该解析为 let 语句",
        input
    );

    let input = "let name = \"Alice\";";

    let parsed = test_parse::<Statement>(input).unwrap();
    assert!(
        matches!(parsed, Statement::Let(_)),
        "输入 '{}' 应该解析为 let 语句",
        input
    );
}

#[test]
fn test_expression_statements() {
    let input = "x = 10;";

    let parsed = test_parse::<Statement>(input).unwrap();
    assert!(
        matches!(parsed, Statement::Expression(_)),
        "输入 '{}' 应该解析为 Expression 语句",
        input
    );
}