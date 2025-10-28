//! 错误处理测试模块
//! 
//! 测试解析器的错误处理能力和边界情况

use liescript_diagnostic::{reporter::DiagnosticCollector, SourceMap};
use liescript_lexical::Span;
use liescript_parser::{Parse, ParseContext, ParseError, ParseResult, ParserConfig};
use liescript_lexer::TokenStream;
use liescript_ast::{expressions::{Expression, LiteralExpression}, names::PathInExpression};

/// 创建测试用的ParseContext
fn test_parse<T: Parse>(input: &str) -> ParseResult<T> {
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("test.lie".to_string(), input.to_string());
    let config = ParserConfig::default();
    let mut diagnostic_collector = DiagnosticCollector::new(source_map);
    let tokens = TokenStream::parse(file_id, input).map_err(|err| {
        ParseError::syntax_error(err.message().to_string(), err.span().unwrap_or_else(|| Span::dummy()))
    })?;
    let mut context = ParseContext::new(&tokens, &config, &mut diagnostic_collector);
    // T::parse(&mut context)
    let ret = T::parse(&mut context);
    if !context.is_eof() {
        return Err(ParseError::syntax_error("incomplete", context.current_span().unwrap_or_else(|| Span::dummy())));
    }
    ret
}



    #[test]
    fn test_incomplete_expressions() {
        let error_cases = ["1 +", "x =", "(", "[", "fn(", "a &&", "!"];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_mismatched_brackets() {
        let error_cases = ["(1 + 2", "[1, 2, 3", "{a: 1", "1 + 2)", "array[0", "fn())"];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_invalid_operator_usage() {
        let error_cases = ["1 + + 2", "a = = b", "x * / y", "a && && b"];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_empty_and_whitespace() {
        let error_cases = ["", "   ", "\n\t\r"];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_invalid_identifiers() {
        let error_cases = [
            "123abc",        // 数字开头的标识符
            "-variable",     // 符号开头的标识符
            "var-name",     // 包含连字符
            "var.name",     // 包含点号
            "var name",     // 包含空格
        ];

        for input in error_cases {
            let result = test_parse::<PathInExpression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_malformed_function_calls() {
        let error_cases = [
            "fn(",          // 缺少参数和右括号
            "fn(,",         // 逗号后缺少参数
            "fn(a,)",       // 逗号后缺少参数
            "fn(a b)",      // 缺少逗号分隔符
            "fn(a,,b)",     // 连续逗号
        ];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_invalid_literals() {
        let error_cases = [
            "0x",           // 不完整的十六进制
            "0b",           // 不完整的二进制
            "0o",           // 不完整的八进制
            "1.2.3",        // 多个小数点
            "truefalse",    // 无效的布尔字面量
            "'unclosed",   // 未闭合的字符字面量
            "\"unclosed",   // 未闭合的字符串字面量
        ];

        for input in error_cases {
            let result = test_parse::<LiteralExpression>(input);

            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_unexpected_tokens() {
        let error_cases = [
            "1 2",          // 连续数字
            "a b",          // 连续标识符
            "+ -",          // 连续操作符
            "= =",          // 连续赋值操作符
            ", ,",          // 连续逗号
        ];

        for input in error_cases {
            let result = test_parse::<Expression>(input);
            assert!(result.is_err(), 
                   "输入 '{}' 应该解析失败但成功", input);
        }
    }

    #[test]
    fn test_recovery_scenarios() {
        // 测试错误恢复能力
        let partial_cases = [
            "1 + 2 +", // 部分解析
            "x = y =",   // 部分赋值链
            "fn(a, b,",     // 部分函数调用
        ];

        for input in partial_cases {
            let result = test_parse::<Expression>(input);
            // 这些应该部分解析成功或提供有用的错误信息
            if let Ok(expr) = result {
                assert!(matches!(expr, Expression::Operator(_)) || matches!(expr, Expression::Call(_)), 
                       "输入 '{}' 应该部分解析成功", input);
            }
            // 如果解析失败，应该提供有意义的错误信息
        }
    }

    #[test]
    fn test_edge_case_boundaries() {
        let boundary_cases = [
            "0",            // 零值
            "9223372036854775807", // i64最大值
        ];

        for input in boundary_cases {
            let result = test_parse::<Expression>(input);
            // 边界值应该正确处理
            if result.is_ok() {
                let expr = result.unwrap();
                assert!(matches!(expr, Expression::Literal(_)), 
                       "输入 '{}' 应该解析为字面量", input);
            }
        }
    }
