use crate::diagnostic::Spanned;
use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};

use crate::syntax::expressions::*;
use crate::syntax::names::Path;
use crate::syntax::patterns::Pattern;
use crate::syntax::statements::Statement;
use crate::syntax::types::Type;
use crate::syntax::{BinOp, RangeLimits, UnOp};

use super::parse::{Parse, ParseError, ParseStream};

pub fn parse_expression(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    parse_expr(stream, 0)
}

/// 解析表达式 (使用Pratt parser算法)
fn parse_expr(stream: &mut ParseStream, min_bp: u8) -> Result<Expression, ParseError> {
    let mut expr = parse_prefix(stream)?;

    while let Some(peek) = stream.peek() {
        match BinOp::from_token(&peek) {
            Some(bin_op) => {
                let (left_bp, right_bp) = bin_op.get_binding_power();
                if left_bp < min_bp {
                    break;
                }

                expr = parse_infix(stream, expr, right_bp)?;
            }
            None => {
                // postfix
                expr = parse_postfix(stream, expr)?;
            }
        }
    }

    Ok(expr)
}

/// 解析主表达式 (字面量、标识符、括号表达式等)
fn parse_primary(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let start_token = stream.peek().ok_or(ParseError::eof())?;

    match &start_token.value {
        Token::Literal(_) => parse_literal(stream),
        Token::Ident(_) => parse_path(stream),
        Token::Symbol(Symbol::Underscore) => parse_underscore(stream),
        Token::Symbol(Symbol::LParen) => parse_paren(stream),
        Token::Symbol(Symbol::LBracket) => parse_array(stream)
            .map(|expr| Expression::WithoutBlock(ExpressionWithoutBlock::Array(expr))),
        Token::Symbol(Symbol::LBrace) => {
            parse_block(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::Block(expr)))
        }
        Token::Keyword(Keyword::If) => {
            parse_if(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::If(expr)))
        }
        Token::Keyword(Keyword::While) => {
            parse_while(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::While(expr)))
        }
        Token::Keyword(Keyword::Loop) => {
            parse_loop(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::Loop(expr)))
        }
        Token::Keyword(Keyword::For) => {
            parse_for(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::For(expr)))
        }
        Token::Keyword(Keyword::Break) => parse_break(stream)
            .map(|expr| Expression::WithoutBlock(ExpressionWithoutBlock::Break(expr))),
        Token::Keyword(Keyword::Continue) => parse_continue(stream)
            .map(|expr| Expression::WithoutBlock(ExpressionWithoutBlock::Continue(expr))),
        Token::Keyword(Keyword::Return) => parse_return(stream)
            .map(|expr| Expression::WithoutBlock(ExpressionWithoutBlock::Return(expr))),
        Token::Keyword(Keyword::Match) => {
            parse_match(stream).map(|expr| Expression::WithBlock(ExpressionWithBlock::Match(expr)))
        }
        Token::Symbol(Symbol::Or) => parse_closure(stream)
            .map(|expr| Expression::WithoutBlock(ExpressionWithoutBlock::Closure(expr))),
        _ => Err(ParseError::new("Expected primary expression")
            .with_span(start_token.span)
            .with_expected("valid primary expression")),
    }
}

/// 解析前缀表达式
fn parse_prefix(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    while let Some(start_token) = stream.peek() {
        match &start_token.value {
            // 引用表达式
            Token::Symbol(Symbol::And) => {
                let and_token = stream.consume()?;
                let is_mut = if stream.next_is(Keyword::Mut) {
                    Some(stream.consume()?)
                } else {
                    None
                };
                return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                    OperatorExpression::Borrow {
                        and_token,
                        is_mut,
                        expr: Box::new(parse_prefix(stream)?),
                    },
                )));
            }

            // 解引用表达式
            Token::Symbol(Symbol::Star) => {
                let star_token = stream.consume()?;
                return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                    OperatorExpression::Deref {
                        star_token,
                        expr: Box::new(parse_prefix(stream)?),
                    },
                )));
            }

            // 一元运算符
            Token::Symbol(Symbol::Not) | Token::Symbol(Symbol::Minus) => {
                let op = Spanned::<UnOp>::parse(stream)?;
                return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                    OperatorExpression::Neg {
                        op,
                        expr: Box::new(parse_prefix(stream)?),
                    },
                )));
            }

            // 不是前缀操作符，退出循环
            _ => return parse_primary(stream),
        }
    }

    Err(ParseError::eof())
}

/// 解析中缀表达式
fn parse_infix(
    stream: &mut ParseStream,
    left: Expression,
    right_bp: u8,
) -> Result<Expression, ParseError> {
    let start_token = stream.peek().ok_or(ParseError::eof())?.clone();

    let op = <Spanned<BinOp>>::parse(stream)?;

    let right = Box::new(parse_expr(stream, op.get_binding_power().0)?);

    let expr = match op.value() {
        BinOp::Assign => Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
            OperatorExpression::Assign {
                left: Box::new(left),
                eq_token: start_token.clone(),
                right,
            },
        )),
        BinOp::AddAssign
        | BinOp::SubAssign
        | BinOp::MulAssign
        | BinOp::DivAssign
        | BinOp::RemAssign => Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
            OperatorExpression::AssignOp {
                left: Box::new(left),
                op,
                right,
            },
        )),
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Rem
        | BinOp::BitAnd
        | BinOp::BitOr
        | BinOp::BitXor
        | BinOp::BitShl
        | BinOp::BitShr => Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
            OperatorExpression::Arithmetic {
                left: Box::new(left),
                op,
                right,
            },
        )),
        BinOp::LogicAnd | BinOp::LogicOr => Expression::WithoutBlock(
            ExpressionWithoutBlock::Operator(OperatorExpression::Logical {
                left: Box::new(left),
                op,
                right,
            }),
        ),
        BinOp::Eq
        | BinOp::NotEq
        | BinOp::LessThen
        | BinOp::GreaterThen
        | BinOp::LessThenOrEq
        | BinOp::GreaterThenOrEq => Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
            OperatorExpression::Comparison {
                left: Box::new(left),
                op,
                right,
            },
        )),
        _ => unreachable!(),
    };

    Ok(expr)
}

fn parse_postfix(stream: &mut ParseStream, expr: Expression) -> Result<Expression, ParseError> {
    let mut expr = expr;

    while let Some(peek) = stream.peek() {
        match peek.value() {
            Token::Symbol(Symbol::Dot) => {
                let dot_token = stream.consume()?;
                let peek = stream.consume()?;

                match peek.value() {
                    Token::Keyword(Keyword::Await) => {
                        let await_token = stream.consume()?;
                        expr = Expression::WithoutBlock(ExpressionWithoutBlock::Await(
                            AwaitExpression {
                                expr: Box::new(expr),
                                dot_token,
                                await_token,
                            },
                        ));
                    }
                    Token::Ident(_name) => {
                        let field = IdentSpan::parse(stream)?;

                        expr = Expression::WithoutBlock(ExpressionWithoutBlock::Field(
                            FieldExpression {
                                expr: Box::new(expr),
                                dot_token,
                                field,
                            },
                        ));
                    }

                    Token::Literal(Literal::Integer(_i)) => {
                        let index = <Spanned<u32>>::parse(stream)?;

                        expr = Expression::WithoutBlock(ExpressionWithoutBlock::TupleIndex(
                            TupleIndexingExpression {
                                expr: Box::new(expr),
                                dot_token,
                                index,
                            },
                        ));
                    }

                    _ => {
                        return Err(ParseError::new("Expected field access or await")
                            .with_span(peek.span)
                            .with_expected("identifier or await"));
                    }
                }
            }

            Token::Symbol(Symbol::LParen) => {
                let open_token = stream.consume()?;
                let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
                let close_token = stream.expect_symbol(Symbol::RParen)?;

                // when expr is FieldExpression, it should be changed to MethodCallExpression
                expr = match expr {
                    Expression::WithoutBlock(ExpressionWithoutBlock::Field(FieldExpression {
                        expr,
                        dot_token,
                        field,
                    })) => Expression::WithoutBlock(ExpressionWithoutBlock::MethodCall(
                        MethodCallExpression {
                            expr,
                            dot_token,
                            method: field,
                            paren_token: Paren {
                                open: open_token,
                                close: close_token,
                            },
                            args,
                        },
                    )),
                    _ => Expression::WithoutBlock(ExpressionWithoutBlock::Call(CallExpression {
                        expr: Box::new(expr),
                        paren_token: Paren {
                            open: open_token,
                            close: close_token,
                        },
                        args,
                    })),
                }
            }

            Token::Symbol(Symbol::LBracket) => {
                let open_token = stream.consume()?;
                let index = Box::new(parse_expression(stream)?);
                let close_token = stream.expect_symbol(Symbol::RBracket)?;

                expr = Expression::WithoutBlock(ExpressionWithoutBlock::Index(IndexExpression {
                    expr: Box::new(expr),
                    bracket_token: Bracket {
                        open: open_token,
                        close: close_token,
                    },
                    index,
                }));
            }

            Token::Symbol(Symbol::Question) => {
                let question_token = stream.consume()?;
                expr = Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                    OperatorExpression::Try {
                        expr: Box::new(expr),
                        question_token,
                    },
                ));
            }

            _ => break,
        }
    }

    Ok(expr)
}

impl Parse for Expression {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_expression(stream)
    }
}

// 新增的解析函数
fn parse_literal(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let lit = LiteralSpan::parse(stream)?;
    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Literal(
        LiteralExpression { lit },
    )))
}

fn parse_path(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let path = Path::parse(stream)?;
    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Path(
        PathExpression { path },
    )))
}

fn parse_underscore(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let underscore_token = stream.consume()?;
    Ok(Expression::WithoutBlock(
        ExpressionWithoutBlock::Underscore(UnderscoreExpression { underscore_token }),
    ))
}

fn parse_paren(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let open_token = stream.consume()?;

    // 空元组
    if stream.next_is(Symbol::RParen) {
        let close_token = stream.consume()?;
        return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Tuple(
            TupleExpression {
                paren_token: Paren {
                    open: open_token,
                    close: close_token,
                },
                elems: Punctuated::new(),
            },
        )));
    }

    // 尝试解析为分组表达式（单个表达式后跟右括号）
    let result = stream.try_parse(|s| {
        let expr = parse_expression(s)?;

        // 如果下一个token是右括号，则这是一个分组表达式
        let close_token = s.expect_symbol(Symbol::RParen)?;

        Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Grouped(
            GroupedExpression {
                paren_token: Paren {
                    open: open_token.clone(),
                    close: close_token,
                },
                expr: Box::new(expr),
            },
        )))
    });

    if let Some(grouped) = result {
        return Ok(grouped);
    }

    // 如果不是分组表达式，则尝试解析为元组表达式
    let elems =
        stream.parse_punctuated_with(|s| parse_expression(s), &Token::Symbol(Symbol::Comma))?;
    let close_token = stream.expect_symbol(Symbol::RParen)?;

    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Tuple(
        TupleExpression {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            elems,
        },
    )))
}

fn parse_array(stream: &mut ParseStream) -> Result<ArrayExpression, ParseError> {
    let open_token = stream.consume()?;

    // 空数组
    if stream.next_is(Symbol::RBracket) {
        let close_token = stream.consume()?;
        return Ok(ArrayExpression::Elements {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            elems: Punctuated::new(),
        });
    }

    // 尝试解析为重复表达式 [expr; count]
    let result = stream.try_parse(|s| {
        // 解析第一个表达式
        let value = parse_expression(s)?;

        // 如果后面跟着分号，则可能是重复表达式
        let semi_token = s.expect_symbol(Symbol::Semi)?;
        let count = <Spanned<u32>>::parse(s)?;
        let close_token = s.expect_symbol(Symbol::RBracket)?;

        Ok(ArrayExpression::Repeat {
            bracket_token: Bracket {
                open: open_token.clone(),
                close: close_token,
            },
            value: Box::new(value),
            semi_token,
            count,
        })
    });

    if let Some(repeat_expr) = result {
        return Ok(repeat_expr);
    }

    // 如果不是重复表达式，则解析为普通数组表达式
    let elems =
        stream.parse_punctuated_with(|s| parse_expression(s), &Token::Symbol(Symbol::Comma))?;
    let close_token = stream.expect_symbol(Symbol::RBracket)?;

    Ok(ArrayExpression::Elements {
        bracket_token: Bracket {
            open: open_token,
            close: close_token,
        },
        elems,
    })
}

fn parse_block(stream: &mut ParseStream) -> Result<BlockExpression, ParseError> {
    let open_token = stream.consume()?;
    let mut stmts = Vec::new();

    while !stream.next_is(Symbol::RBrace) {
        if stream.is_empty() {
            return Err(
                ParseError::new("Unexpected end of input in block").with_span(open_token.span)
            );
        }
        stmts.push(Statement::parse(stream)?);
    }

    let close_token = stream.expect_symbol(Symbol::RBrace)?;
    Ok(BlockExpression {
        brace_token: Brace {
            open: open_token,
            close: close_token,
        },
        stmts,
    })
}

fn parse_if(stream: &mut ParseStream) -> Result<IfExpression, ParseError> {
    let if_token = stream.consume()?;
    let cond = Box::new(parse_expression(stream)?);
    let then_branch = BlockExpression::parse(stream)?;

    let else_branch = if stream.next_is(Keyword::Else) {
        let else_token = stream.consume()?;
        if stream.next_is(Keyword::If) {
            Some((
                else_token,
                Box::new(ElseBranch::If(Box::new(parse_if(stream)?))),
            ))
        } else if stream.next_is(Symbol::LBrace) {
            Some((
                else_token,
                Box::new(ElseBranch::Block(BlockExpression::parse(stream)?)),
            ))
        } else {
            return Err(ParseError::new("Expected block or if after else")
                .with_span(stream.lookahead1()?.span));
        }
    } else {
        None
    };

    Ok(IfExpression {
        if_token,
        cond,
        then_branch,
        else_branch,
    })
}

fn parse_while(stream: &mut ParseStream) -> Result<WhileLoopExpression, ParseError> {
    let while_token = stream.consume()?;
    let cond = Box::new(parse_expression(stream)?);
    let body = BlockExpression::parse(stream)?;

    Ok(WhileLoopExpression {
        label: None,
        while_token,
        cond,
        body,
    })
}

fn parse_loop(stream: &mut ParseStream) -> Result<LoopExpression, ParseError> {
    let loop_token = stream.consume()?;
    let body = BlockExpression::parse(stream)?;

    Ok(LoopExpression {
        label: None,
        loop_token,
        body,
    })
}

fn parse_for(stream: &mut ParseStream) -> Result<ForLoopExpression, ParseError> {
    let for_token = stream.consume()?;
    let pat = Pattern::parse(stream)?;
    let in_token = stream.expect(&Token::Keyword(Keyword::In))?;
    let expr = Box::new(parse_expression(stream)?);
    let body = BlockExpression::parse(stream)?;

    Ok(ForLoopExpression {
        label: None,
        for_token,
        pat,
        in_token,
        expr,
        body,
    })
}

fn parse_break(stream: &mut ParseStream) -> Result<BreakExpression, ParseError> {
    let break_token = stream.consume()?;
    let label = stream.try_parse(|s| IdentSpan::parse(s));

    // 如果下一个token不是分号或右大括号，则尝试解析表达式
    if !stream.next_is(Symbol::Semi) && !stream.next_is(Symbol::RBrace) {
        let expr = Box::new(parse_expression(stream)?);
        return Ok(BreakExpression {
            break_token,
            label,
            expr: Some(expr),
        });
    }

    Ok(BreakExpression {
        break_token,
        label,
        expr: None,
    })
}

fn parse_continue(stream: &mut ParseStream) -> Result<ContinueExpression, ParseError> {
    let continue_token = stream.consume()?;
    let label = stream.try_parse(|s| IdentSpan::parse(s));

    Ok(ContinueExpression {
        continue_token,
        label,
    })
}

fn parse_return(stream: &mut ParseStream) -> Result<ReturnExpression, ParseError> {
    let return_token = stream.consume()?;

    if !stream.next_is(Symbol::Semi) && !stream.next_is(Symbol::RBrace) {
        let expr = Box::new(parse_expression(stream)?);
        return Ok(ReturnExpression {
            return_token,
            expr: Some(expr),
        });
    }

    Ok(ReturnExpression {
        return_token,
        expr: None,
    })
}

fn parse_match(stream: &mut ParseStream) -> Result<MatchExpression, ParseError> {
    let match_token = stream.consume()?;
    let expr = Box::new(parse_expression(stream)?);
    let open_brace = stream.expect_symbol(Symbol::LBrace)?;
    let mut arms = Vec::new();

    while !stream.next_is(Symbol::RBrace) {
        if stream.is_empty() {
            return Err(
                ParseError::new("Unexpected end of input in match expression")
                    .with_span(match_token.span),
            );
        }

        let pat = Pattern::parse(stream)?;
        let guard = if stream.next_is(Keyword::If) {
            Some(MatchArmGuard {
                if_token: stream.consume()?,
                expr: Box::new(parse_expression(stream)?),
            })
        } else {
            None
        };

        let fat_arrow_token = stream.expect_symbol(Symbol::FatArrow)?;
        let body = Box::new(parse_expression(stream)?);
        let body_span = body.span();
        let comma_token = stream
            .next_is(Symbol::Comma)
            .then(|| stream.consume())
            .transpose()?;

        arms.push(MatchArm {
            pat,
            guard,
            fat_arrow_token,
            body,
            comma_token: comma_token.clone(),
        });

        if comma_token.is_none() && !stream.next_is(Symbol::RBrace) {
            return Err(ParseError::new("Expected comma after match arm").with_span(body_span));
        }
    }

    let close_brace = stream.expect_symbol(Symbol::RBrace)?;

    Ok(MatchExpression {
        match_token,
        expr,
        brace_token: Brace {
            open: open_brace,
            close: close_brace,
        },
        arms,
    })
}

fn parse_closure(stream: &mut ParseStream) -> Result<ClosureExpression, ParseError> {
    let or1_token = stream.consume()?;

    // 尝试解析无参数闭包 || expr
    let result = stream.try_parse(|s| {
        let or2_token = s.expect_symbol(Symbol::Or)?;
        let body = Box::new(parse_expression(s)?);
        Ok(ClosureExpression {
            move_token: None,
            or_token: (or1_token.clone(), or2_token),
            inputs: Punctuated::new(),
            output: None,
            body,
        })
    });

    if let Some(closure) = result {
        return Ok(closure);
    }

    // 解析带参数的闭包
    let inputs =
        stream.parse_punctuated_with(|s| Pattern::parse(s), &Token::Symbol(Symbol::Comma))?;
    let or2_token = stream.expect_symbol(Symbol::Or)?;

    // 可选的返回类型
    let output = stream
        .next_is(Symbol::RArrow)
        .then(|| Ok((stream.consume()?, Box::new(Type::parse(stream)?))))
        .transpose()?;

    let body = Box::new(parse_expression(stream)?);

    Ok(ClosureExpression {
        move_token: None,
        or_token: (or1_token, or2_token),
        inputs,
        output,
        body,
    })
}

impl Parse for BlockExpression {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut stmts = Vec::new();

        while !stream.next_is(Symbol::RBrace) {
            if stream.is_empty() {
                return Err(
                    ParseError::new("Unexpected end of input in block").with_span(open_token.span)
                );
            }
            stmts.push(Statement::parse(stream)?);
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(BlockExpression {
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            stmts,
        })
    }
}

impl Parse for UnOp {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;

        match UnOp::from_token(&token) {
            Some(op) => Ok(op),
            None => {
                return Err(ParseError::new("Expected unary operator").with_span(token.span));
            }
        }
    }
}

impl Parse for BinOp {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;

        match BinOp::from_token(&token) {
            Some(op) => Ok(op),
            None => {
                return Err(ParseError::new("Expected binary operator").with_span(token.span));
            }
        }
    }
}

impl Parse for RangeLimits {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;

        match token.value() {
            Token::Symbol(Symbol::DotDot) => Ok(RangeLimits::HalfOpen),
            Token::Symbol(Symbol::DotDotEq) => Ok(RangeLimits::Closed),
            _ => {
                return Err(ParseError::new("Expected range limits").with_span(token.span));
            }
        }
    }
}

impl Parse for Label {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = IdentSpan::parse(stream)?;
        let colon_token = stream.consume()?;

        Ok(Label { colon_token, label })
    }
}
