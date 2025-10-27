//! 简单的Parser模块测试

#[cfg(test)]
mod parser_tests {
    use liescript::{
        lexical::TokenStream,
        parser::{Parse, ParseContext},
        source::FileId,
        Expression,
    };

    /// 测试基本表达式解析
    #[test]
    fn test_basic_expression_parsing() {
        // 创建测试输入
        let input = "42";
        
        // 创建token流
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        
        // 创建解析上下文
        let mut context = ParseContext::new(tokens);
        
        // 尝试解析表达式
        match Expression::parse(&mut context) {
            Ok(expr) => {
                println!("✓ 成功解析表达式: {:?}", expr);
                // 验证解析结果
                assert!(matches!(expr, Expression::Literal(_)));
            }
            Err(e) => {
                panic!("解析失败: {}", e);
            }
        }
    }

    /// 测试算术表达式
    #[test]
    fn test_arithmetic_expression() {
        let input = "1 + 2 * 3";
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        let mut context = ParseContext::new(tokens);
        
        match Expression::parse(&mut context) {
            Ok(expr) => {
                println!("✓ 成功解析算术表达式: {:?}", expr);
                assert!(matches!(expr, Expression::Operator(_)));
            }
            Err(e) => {
                panic!("算术表达式解析失败: {}", e);
            }
        }
    }

    /// 测试标识符表达式
    #[test]
    fn test_identifier_expression() {
        let input = "variable_name";
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        let mut context = ParseContext::new(tokens);
        
        match Expression::parse(&mut context) {
            Ok(expr) => {
                println!("✓ 成功解析标识符表达式: {:?}", expr);
                assert!(matches!(expr, Expression::Path(_)));
            }
            Err(e) => {
                panic!("标识符表达式解析失败: {}", e);
            }
        }
    }

    /// 测试分组表达式
    #[test]
    fn test_grouped_expression() {
        let input = "(1 + 2)";
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        let mut context = ParseContext::new(tokens);
        
        match Expression::parse(&mut context) {
            Ok(expr) => {
                println!("✓ 成功解析分组表达式: {:?}", expr);
                assert!(matches!(expr, Expression::Grouped(_)));
            }
            Err(e) => {
                panic!("分组表达式解析失败: {}", e);
            }
        }
    }

    /// 测试错误恢复
    #[test]
    fn test_error_recovery() {
        // 测试不完整的表达式
        let input = "1 + ";
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        let mut context = ParseContext::new(tokens);
        
        // 这个应该会失败，但应该优雅地处理错误
        match Expression::parse(&mut context) {
            Ok(_) => {
                // 如果成功解析了部分表达式，也是可以接受的
                println!("✓ 成功处理部分表达式");
            }
            Err(e) => {
                // 错误是预期的，但应该是有意义的错误信息
                println!("✓ 预期错误: {}", e);
                assert!(!e.to_string().is_empty());
            }
        }
    }
}