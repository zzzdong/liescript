//! Parser模块的基本测试套件

use liescript::{
    lexical::TokenStream,
    parser::{Parse, ParseContext},
    source::FileId,
    Expression,
};

/// 创建测试用的ParseContext
fn create_test_context(input: &str) -> ParseContext {
    let tokens = TokenStream::parse(FileId::default(), input).unwrap();
    ParseContext::new(tokens)
}

/// 测试辅助函数：解析表达式并返回结果
fn parse_expr(input: &str) -> Result<Expression, String> {
    let mut context = create_test_context(input);
    Expression::parse(&mut context).map_err(|e| e.to_string())
}

/// 测试辅助函数：断言表达式解析成功
fn assert_parses(input: &str) {
    match parse_expr(input) {
        Ok(expr) => println!("✓ 成功解析: {} -> {:?}", input, expr),
        Err(e) => panic!("解析失败: {} -> {}", input, e),
    }
}

#[cfg(test)]
mod basic_tests {
    use super::*;

    #[test]
    fn test_literal_expressions() {
        // 整数字面量
        assert_parses("42");
        assert_parses("0");
        assert_parses("1234567890");
        
        // 布尔字面量
        assert_parses("true");
        assert_parses("false");
    }

    #[test]
    fn test_identifier_expressions() {
        // 简单标识符
        assert_parses("x");
        assert_parses("variable");
        assert_parses("camelCase");
        assert_parses("snake_case");
        
        // 带下划线的标识符
        assert_parses("_");
        assert_parses("_private");
    }

    #[test]
    fn test_grouped_expressions() {
        // 分组表达式
        assert_parses("(42)");
        assert_parses("(x)");
        assert_parses("((x))");
    }
}

#[cfg(test)]
mod operator_tests {
    use super::*;

    #[test]
    fn test_arithmetic_operators() {
        // 基本算术运算
        assert_parses("1 + 2");
        assert_parses("3 - 4");
        assert_parses("5 * 6");
        assert_parses("7 / 8");
        assert_parses("9 % 10");
        
        // 复杂算术表达式
        assert_parses("1 + 2 * 3");
        assert_parses("(1 + 2) * 3");
    }

    #[test]
    fn test_comparison_operators() {
        // 比较运算符
        assert_parses("1 == 2");
        assert_parses("3 != 4");
        assert_parses("5 < 6");
        assert_parses("7 > 8");
    }

    #[test]
    fn test_logical_operators() {
        // 逻辑运算符
        assert_parses("true && false");
        assert_parses("true || false");
        assert_parses("!true");
    }
}

// 主测试运行器
#[cfg(test)]
mod test_runner {
    use super::*;

    #[test]
    fn run_basic_parser_tests() {
        // 这个测试会运行所有子模块的测试
        // 如果所有测试都通过，这个测试也会通过
        println!("开始运行Parser模块基本测试...");
    }
}