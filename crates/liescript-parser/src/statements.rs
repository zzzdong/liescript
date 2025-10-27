use liescript_ast::items::Item;
use liescript_ast::types::Type;
use liescript_ast::{expressions::Expression, patterns::Pattern};
use liescript_ast::statements::*;
use liescript_lexical::{
    keyword::Keyword, symbol::Symbol, token::{Brace, Paren, Punctuated, Token}, Span, Spanned
};

use crate::{Parse, ParseContext, ParseResult};

impl Parse for Statement {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_statement(cx)
    }
}

/// 解析语句
pub fn parse_statement(cx: &mut ParseContext) -> ParseResult<Statement> {
    if cx.is_eof() {
        return Err(cx.create_eof_error("statement"));
    }

    // 检查是否是项声明
    if let Some(token) = cx.peek() {
        if let Token::Keyword(keyword) = &token.value {
            if matches!(
                keyword,
                Keyword::Fn
                    | Keyword::Struct
                    | Keyword::Enum
                    | Keyword::Type
                    | Keyword::Const
                    | Keyword::Static
                    | Keyword::Mod
                    | Keyword::Use
            ) {
                return Item::parse(cx).map(Statement::Item);
            }
        }
    }

    // 检查是否是let语句
    if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::Let))) {
        return parse_let_statement(cx).map(Statement::Let);
    }

    // 检查是否是空语句（仅分号）
    if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) {
        return parse_empty_statement(cx).map(Statement::Empty);
    }

    // 默认解析为表达式语句
    parse_expression_statement(cx).map(Statement::Expression)
}

/// 解析空语句
fn parse_empty_statement(cx: &mut ParseContext) -> ParseResult<EmptyStatement> {
    let semi_token = cx.expect_symbol(Symbol::Semi)?;
    Ok(EmptyStatement { semi_token })
}

/// 解析表达式语句
fn parse_expression_statement(cx: &mut ParseContext) -> ParseResult<ExpressionStatement> {
    let expr = Expression::parse(cx)?;

    // 检查是否需要分号
    let semi_token = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) {
        Some(cx.consume()?)
    } else {
        None
    };

    Ok(ExpressionStatement { expr, semi_token })
}

/// 解析let语句
fn parse_let_statement(cx: &mut ParseContext) -> ParseResult<LetStatement> {
    let let_token = cx.expect_keyword(Keyword::Let)?;
    let pattern = Pattern::parse(cx)?;

    // 解析可选的类型注解
    let type_annotation = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Colon))) {
        let colon_token = cx.consume()?;
        let ty = Type::parse(cx)?;
        Some((colon_token, Box::new(ty)))
    } else {
        None
    };

    // 解析可选的初始化表达式
    let initializer = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Eq))) {
        let eq_token = cx.consume()?;
        let expr = Expression::parse(cx)?;
        Some((eq_token, Box::new(expr)))
    } else {
        None
    };

    let semi_token = cx.expect_symbol(Symbol::Semi)?;

    Ok(LetStatement {
        let_token,
        pattern,
        type_annotation,
        initializer,
        semi_token,
    })
}