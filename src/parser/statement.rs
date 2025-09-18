use crate::syntax::patterns::Pattern;
use crate::syntax::statements::Statement;
use crate::syntax::types::Type;
use crate::syntax::{Item, statements::*};

use crate::lexical::{Keyword, Symbol, Token};

use super::expression::parse_expression;
use super::parse::{Parse, ParseError, ParseStream};

impl Parse for Statement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // 检查空语句
        if stream.next_is(&Token::Symbol(Symbol::Semi)) {
            return Ok(Statement::Empty(EmptyStatement::parse(stream)?));
        }

        if let Some(item) = stream.try_parse(Item::parse) {
            return Ok(Statement::Item(item));
        }

        if stream.next_is(Token::Keyword(Keyword::Let)) {
            return Ok(Statement::Let(parse_let_stmt(stream)?));
        }

        // 检查表达式语句
        // 先尝试解析表达式，然后检查是否以分号结尾
        let checkpoint = stream.checkpoint();
        if let Ok(expr) = parse_expression(stream) {
            if stream.next_is(&Token::Symbol(Symbol::Semi)) {
                // 表达式后跟分号
                let semi_token = stream.expect(&Token::Symbol(Symbol::Semi))?;
                return Ok(Statement::Expression(ExpressionStatement {
                    expr,
                    semi_token: Some(semi_token),
                }));
            } else {
                // 不带分号的表达式
                return Ok(Statement::Expression(ExpressionStatement {
                    expr,
                    semi_token: None,
                }));
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

fn parse_let_stmt(stream: &mut ParseStream) -> Result<LetStatement, ParseError> {
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

impl Parse for LetStatement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_let_stmt(stream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Span;
    use crate::lexical::{Brace, Identifier, Literal, Paren, Punctuated, Token, TokenStream};
    use crate::syntax::patterns::{IdentifierPattern, ReferencePattern, TuplePattern};
    use crate::syntax::{
        PathExprSegment, PathIdentSegment, PathInExpression, Primitive, expressions::*,
    };

    /// 解析语句并忽略Span信息进行比较
    macro_rules! assert_stmt_eq {
        ($input:literal, $expected:expr) => {{
            let actual = parse_stmt($input).unwrap();
            assert_eq!(actual, $expected);
        }};
    }

    fn parse_stmt(input: &str) -> Result<Statement, ParseError> {
        let tokens = TokenStream::parse(input).unwrap();
        let mut stream = ParseStream::new(&tokens);
        Statement::parse(&mut stream)
    }

    fn ident_pattern(name: &str) -> Pattern {
        Pattern::Identifier(IdentifierPattern {
            ident: Identifier::new(name).into(),
            by_ref: None,
            is_mut: None,
            subpat: None,
        })
    }

    fn literal_expr(value: i64) -> Expression {
        Expression::Literal(LiteralExpression {
            lit: Literal::Integer(value).into(),
        })
    }

    fn path_expr(paths: &[&str]) -> PathExpression {
        PathExpression { path: path(paths) }
    }

    fn path(paths: &[&str]) -> PathInExpression {
        let mut segments = Punctuated::new();
        if let Some((last, rest)) = paths.split_last() {
            for path in rest {
                segments.push(
                    PathExprSegment {
                        ident: PathIdentSegment::Ident(Identifier::new(path).into()),
                        args: None,
                    },
                    Token::Symbol(Symbol::ColonColon).into(),
                );
            }
            segments.push_last(PathExprSegment {
                ident: PathIdentSegment::Ident(Identifier::new(last).into()),
                args: None,
            });
        }

        PathInExpression {
            leading_colon: None,
            segments,
        }
    }

    #[test]
    fn test_empty_statement() {
        assert_stmt_eq!(
            ";",
            Statement::Empty(EmptyStatement {
                semi_token: Token::Symbol(Symbol::Semi).into()
            })
        );
    }

    #[test]
    fn test_let_statement() {
        assert_stmt_eq!(
            "let x = 42;",
            Statement::Let(LetStatement {
                let_token: Token::Keyword(Keyword::Let).into(),
                pattern: ident_pattern("x"),
                type_annotation: None,
                initializer: Some((Token::Symbol(Symbol::Eq).into(), Box::new(literal_expr(42)))),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_let_statement_with_type() {
        assert_stmt_eq!(
            "let x: int = 42;",
            Statement::Let(LetStatement {
                let_token: Token::Keyword(Keyword::Let).into(),
                pattern: ident_pattern("x"),
                type_annotation: Some((
                    Token::Symbol(Symbol::Colon).into(),
                    Box::new(Type::Primitive(Primitive::Integer(
                        Token::Keyword(Keyword::Int).into()
                    )))
                )),
                initializer: Some((Token::Symbol(Symbol::Eq).into(), Box::new(literal_expr(42)))),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_let_statement_without_initializer() {
        assert_stmt_eq!(
            "let x: int;",
            Statement::Let(LetStatement {
                let_token: Token::Keyword(Keyword::Let).into(),
                pattern: ident_pattern("x"),
                type_annotation: Some((
                    Token::Symbol(Symbol::Colon).into(),
                    Box::new(Type::Primitive(Primitive::Integer(
                        Token::Keyword(Keyword::Int).into()
                    )))
                )),
                initializer: None,
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_expression_statement() {
        assert_stmt_eq!(
            "42;",
            Statement::Expression(ExpressionStatement {
                expr: literal_expr(42),
                semi_token: Some(Token::Symbol(Symbol::Semi).into()),
            })
        );
    }

    #[test]
    fn test_expression_statement_without_semi() {
        assert_stmt_eq!(
            "42",
            Statement::Expression(ExpressionStatement {
                expr: literal_expr(42),
                semi_token: None,
            })
        );
    }

    #[test]
    fn test_block_expression_statement() {
        assert_stmt_eq!(
            "{ let x = 1; }",
            Statement::Expression(ExpressionStatement {
                expr: Expression::Block(BlockExpression {
                    brace_token: Brace {
                        open: Token::Symbol(Symbol::LBrace).into(),
                        close: Token::Symbol(Symbol::RBrace).into(),
                    },
                    stmts: vec![Statement::Let(LetStatement {
                        let_token: Token::Keyword(Keyword::Let).into(),
                        pattern: ident_pattern("x"),
                        type_annotation: None,
                        initializer: Some((
                            Token::Symbol(Symbol::Eq).into(),
                            Box::new(literal_expr(1))
                        )),
                        semi_token: Token::Symbol(Symbol::Semi).into(),
                    })],
                }),
                semi_token: None,
            })
        );
    }

    #[test]
    fn test_let_statement_with_complex_pattern() {
        assert_stmt_eq!(
            "let (a, b) = (1, 2);",
            Statement::Let(LetStatement {
                let_token: Token::Keyword(Keyword::Let).into(),
                pattern: Pattern::Tuple(TuplePattern {
                    paren_token: Paren {
                        open: Token::Symbol(Symbol::LParen).into(),
                        close: Token::Symbol(Symbol::RParen).into(),
                    },
                    elems: {
                        let mut elems = Punctuated::new();
                        elems.push(ident_pattern("a"), Token::Symbol(Symbol::Comma).into());
                        elems.last = Some(Box::new(ident_pattern("b")));
                        elems
                    },
                }),
                type_annotation: None,
                initializer: Some((
                    Token::Symbol(Symbol::Eq).into(),
                    Box::new(Expression::Tuple(TupleExpression {
                        paren_token: Paren {
                            open: Token::Symbol(Symbol::LParen).into(),
                            close: Token::Symbol(Symbol::RParen).into(),
                        },
                        elems: {
                            let mut elems = Punctuated::new();
                            elems.push(literal_expr(1), Token::Symbol(Symbol::Comma).into());
                            elems.push_last(literal_expr(2));
                            elems
                        },
                    }))
                )),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_let_statement_with_reference_pattern() {
        assert_stmt_eq!(
            "let &x = &42;",
            Statement::Let(LetStatement {
                let_token: Token::Keyword(Keyword::Let).into(),
                pattern: Pattern::Reference(ReferencePattern {
                    and_token: Token::Symbol(Symbol::And).into(),
                    is_mut: None,
                    pat: Box::new(ident_pattern("x")),
                }),
                type_annotation: None,
                initializer: Some((
                    Token::Symbol(Symbol::Eq).into(),
                    Box::new(Expression::Operator(OperatorExpression::Borrow {
                        and_token: Token::Symbol(Symbol::And).into(),
                        is_mut: None,
                        expr: Box::new(literal_expr(42)),
                    }))
                )),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_nested_block_statement() {
        assert_stmt_eq!(
            "{ { let x = 1; } }",
            Statement::Expression(ExpressionStatement {
                expr: Expression::Block(BlockExpression {
                    brace_token: Brace {
                        open: Token::Symbol(Symbol::LBrace).into(),
                        close: Token::Symbol(Symbol::RBrace).into(),
                    },
                    stmts: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Block(BlockExpression {
                            brace_token: Brace {
                                open: Token::Symbol(Symbol::LBrace).into(),
                                close: Token::Symbol(Symbol::RBrace).into(),
                            },
                            stmts: vec![Statement::Let(LetStatement {
                                let_token: Token::Keyword(Keyword::Let).into(),
                                pattern: ident_pattern("x"),
                                type_annotation: None,
                                initializer: Some((
                                    Token::Symbol(Symbol::Eq).into(),
                                    Box::new(literal_expr(1))
                                )),
                                semi_token: Token::Symbol(Symbol::Semi).into(),
                            })],
                        }),
                        semi_token: None,
                    })],
                }),
                semi_token: None,
            })
        );
    }
}
