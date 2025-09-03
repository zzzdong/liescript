use crate::syntax::patterns::Pattern;
use crate::syntax::statements::Statement;
use crate::syntax::statements::*;
use crate::syntax::types::Type;

use crate::lexical::{Keyword, Symbol, Token};

use super::parse::{Parse, ParseError, ParseStream};
use super::expression::parse_expression;

impl Parse for Statement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // 检查空语句
        if stream.next_is(&Token::Symbol(Symbol::Semi)) {
            return Ok(Statement::Empty(EmptyStatement::parse(stream)?));
        }
        
        // 检查声明语句 (let)
        if stream.next_is(&Token::Keyword(Keyword::Let)) {
            return Ok(Statement::Declaration(DeclarationStatement::parse(stream)?));
        }
        
        // 检查表达式语句
        // 先尝试解析表达式，然后检查是否以分号结尾
        let checkpoint = stream.checkpoint();
        if let Ok(expr) = parse_expression(stream) {
            if stream.next_is(&Token::Symbol(Symbol::Semi)) {
                // 表达式后跟分号
                let semi_token = stream.expect(&Token::Symbol(Symbol::Semi))?;
                let expr_semi_stmt = ExpressionSemiStatement { expr, semi_token };
                return Ok(Statement::Expression(ExpressionStatement::Semi(expr_semi_stmt)));
            } else {
                // 不带分号的表达式
                return Ok(Statement::Expression(ExpressionStatement::NoSemi(expr)));
            }
        }
        
        // 如果上面都不匹配，恢复检查点并返回错误
        stream.restore(checkpoint);
        Err(ParseError::new("Expected statement"))
    }
}

impl Parse for EmptyStatement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let semi_token = stream.expect(&Token::Symbol(Symbol::Semi))?;
        Ok(EmptyStatement { semi_token })
    }
}

impl Parse for DeclarationStatement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // 目前只支持 let 语句
        Ok(DeclarationStatement::Let(LetStatement::parse(stream)?))
    }
}

impl Parse for LetStatement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let let_token = stream.expect(&Token::Keyword(Keyword::Let))?;
        let pattern = Pattern::parse(stream)?;
        
        let type_annotation = if stream.next_is(&Token::Symbol(Symbol::Colon)) {
            let colon_token = stream.expect(&Token::Symbol(Symbol::Colon))?;
            let ty = Box::new(Type::parse(stream)?);
            Some((colon_token, ty))
        } else {
            None
        };
        
        let initializer = if stream.next_is(&Token::Symbol(Symbol::Eq)) {
            let eq_token = stream.expect(&Token::Symbol(Symbol::Eq))?;
            let expr = Box::new(parse_expression(stream)?);
            Some((eq_token, expr))
        } else {
            None
        };
        
        let semi_token = stream.expect(&Token::Symbol(Symbol::Semi))?;
        
        Ok(LetStatement {
            let_token,
            pattern,
            type_annotation,
            initializer,
            semi_token,
        })
    }
}
