use crate::diagnostic::Spanned;
use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::expressions::*;
use crate::syntax::names::Path;
use crate::syntax::patterns::Pattern;
use crate::syntax::precedence::Precedence;
use crate::syntax::statements::Statement;
use crate::syntax::types::Type;
use crate::syntax::{BinOp, RangeLimits, UnOp};

use super::parse::{Parse, ParseError, ParseStream};

fn parse_expression(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    parse_expr(stream, Precedence::None.binding_powers().0)
}

/// 解析表达式 (使用Pratt parser算法)
fn parse_expr(stream: &mut ParseStream, min_bp: u8) -> Result<Expression, ParseError> {
    let mut expr = parse_prefix(stream)?;

    while let Some(peek) = stream.peek() {
        let (left_bp, right_bp) = get_next_binding_power(peek)?;
        if left_bp < min_bp {
            break;
        }

        expr = parse_infix(stream, expr, right_bp)?;
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
        Token::Symbol(Symbol::LBracket) => parse_array(stream),
        Token::Symbol(Symbol::LBrace) => parse_block(stream),
        Token::Keyword(Keyword::If) => parse_if(stream),
        Token::Keyword(Keyword::While) => parse_while(stream),
        Token::Keyword(Keyword::Loop) => parse_loop(stream),
        Token::Keyword(Keyword::For) => parse_for(stream),
        Token::Keyword(Keyword::Break) => parse_break(stream),
        Token::Keyword(Keyword::Continue) => parse_continue(stream),
        Token::Keyword(Keyword::Return) => parse_return(stream),
        Token::Keyword(Keyword::Match) => parse_match(stream),
        Token::Symbol(Symbol::Or) => parse_closure(stream),
        _ => Err(ParseError::new("Expected primary expression")
            .with_span(start_token.span)
            .with_expected("valid primary expression")),
    }
}

/// 解析前缀表达式
fn parse_prefix(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let start_token = stream.lookahead1()?;

    let expr = match &start_token.value {
        // 引用表达式
        Token::Symbol(Symbol::And) => {
            let and_token = stream.consume()?;
            let is_mut = if stream.next_is(Keyword::Mut) {
                Some(stream.consume()?)
            } else {
                None
            };
            let expr = Box::new(parse_expr(stream, Precedence::Unary.binding_powers().0)?);

            Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                OperatorExpression::Borrow {
                    and_token,
                    is_mut,
                    expr,
                },
            ))
        }

        // 解引用表达式
        Token::Symbol(Symbol::Star) => {
            let star_token = stream.consume()?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary.binding_powers().0)?);

            Expression::WithoutBlock(ExpressionWithoutBlock::Operator(
                OperatorExpression::Deref { star_token, expr },
            ))
        }

        // 一元运算符
        Token::Symbol(Symbol::Not) | Token::Symbol(Symbol::Minus) => {
            let op = Spanned::<UnOp>::parse(stream)?;
            let expr = Box::new(parse_expr(stream, Precedence::Unary.binding_powers().0)?);

            Expression::WithoutBlock(ExpressionWithoutBlock::Operator(OperatorExpression::Neg {
                op,
                expr,
            }))
        }

        // 主表达式
        _ => parse_primary(stream)?,
    };

    Ok(expr)
}

/// 获取下一个运算符的结合性和优先级
fn get_next_binding_power(token: &TokenSpan) -> Result<(u8, u8), ParseError> {
    Precedence::from_token(&token.value)
        .ok_or_else(|| {
            ParseError::new("Expected operator")
                .with_span(token.span)
                .with_expected("valid operator")
        })
        .or_else(|_| Ok((0, 0))) // 最低优先级
}

/// 解析中缀表达式
fn parse_infix(
    stream: &mut ParseStream,
    left: Expression,
    right_bp: u8,
) -> Result<Expression, ParseError> {
    let start_token = stream.lookahead1()?;

    let expr = match &start_token.value {
        // 成员访问
        Token::Symbol(Symbol::Dot) => {
            let dot_token = stream.consume()?;

            let next_token = stream.consume()?;

            match next_token.value() {
                Token::Keyword(Keyword::Await) => {
                    Expression::WithoutBlock(ExpressionWithoutBlock::Await(AwaitExpression {
                        expr: Box::new(left),
                        dot_token,
                        await_token: next_token,
                    }))
                }
                Token::Literal(Literal::Integer(idx)) => {
                    // TODO: index must be a `Spanned<usize>`
                    Expression::WithoutBlock(ExpressionWithoutBlock::TupleIndex(
                        TupleIndexingExpression {
                            expr: Box::new(left),
                            dot_token,
                            index: Spanned::new(*idx as u32, next_token.span),
                        },
                    ))
                }
                Token::Ident(ident) => {
                    let dot_token = stream.consume()?;
                    Expression::WithoutBlock(ExpressionWithoutBlock::Field(FieldExpression {
                        expr: Box::new(left),
                        dot_token,
                        ident: next_token.map(|_| ident.clone()),
                    }))
                }
                _ => {
                    return Err(ParseError::new("Expected identifier or integer literal")
                        .with_span(next_token.span)
                        .with_expected("identifier or integer literal"));
                }
            }
        }

        // 范围表达式
        Token::Symbol(Symbol::DotDot) | Token::Symbol(Symbol::DotDotEq) => {
            let limits = Spanned::<RangeLimits>::parse(stream)?;
            let end = if stream.next_is(Symbol::Semi)
                || stream.next_is(Symbol::Comma)
                || stream.next_is(Symbol::RParen)
                || stream.next_is(Symbol::RBrace)
                || stream.next_is(Symbol::RBracket)
            {
                None
            } else {
                // 范围表达式是右结合的
                Some(Box::new(parse_expr(
                    stream,
                    Precedence::Range.binding_powers().1,
                )?))
            };

            Expression::WithoutBlock(ExpressionWithoutBlock::Range(RangeExpression {
                start: Some(Box::new(left)),
                limits,
                end,
            }))
        }

        Token::Keyword(Keyword::As) => {
            let as_token = stream.consume()?;
            let type_span = Type::parse(stream)?;

            Expression::WithoutBlock(ExpressionWithoutBlock::Operator(OperatorExpression::Cast {
                expr: Box::new(left),
                as_token: start_token.clone(),
                ty: Box::new(type_span),
            }))
        }

        tok if BinOp::from_token(tok).is_some() => {
            let op = BinOp::from_token(&tok).unwrap();
            let right = Box::new(parse_expr(stream, Precedence::from_token(&tok).unwrap().1)?);

            match &op {
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
                        op: start_token.map(|_| op),
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
                        op: start_token.map(|_| op),
                        right,
                    },
                )),
                BinOp::LogicAnd | BinOp::LogicOr => Expression::WithoutBlock(
                    ExpressionWithoutBlock::Operator(OperatorExpression::Logical {
                        left: Box::new(left),
                        op: start_token.map(|_| op),
                        right,
                    }),
                ),
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::LessThen
                | BinOp::GreaterThen
                | BinOp::LessThenOrEq
                | BinOp::GreaterThenOrEq => Expression::WithoutBlock(
                    ExpressionWithoutBlock::Operator(OperatorExpression::Comparison {
                        left: Box::new(left),
                        op: start_token.map(|_| op),
                        right,
                    }),
                ),
                _ => unreachable!(),
            }
        }

        // 函数调用表达式
        Token::Symbol(Symbol::LParen) => {
            let open_token = stream.consume()?;
            let args = stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?;
            let close_token = stream.expect_symbol(Symbol::RParen)?;

            Expression::WithoutBlock(ExpressionWithoutBlock::Call(CallExpression {
                expr: Box::new(left),
                paren_token: Paren {
                    open: open_token,
                    close: close_token,
                },
                args,
            }))
        }

        // 数组索引
        Token::Symbol(Symbol::LBracket) => {
            let open_token = stream.consume()?;
            let index = Box::new(parse_expression(stream)?);
            let close_token = stream.expect_symbol(Symbol::RBracket)?;

            Expression::WithoutBlock(ExpressionWithoutBlock::Index(IndexExpression {
                expr: Box::new(left),
                bracket_token: Bracket {
                    open: open_token,
                    close: close_token,
                },
                index,
            }))
        }

        // 问号运算符
        Token::Symbol(Symbol::Question) => {
            let question_token = stream.consume()?;
            Expression::WithoutBlock(ExpressionWithoutBlock::Operator(OperatorExpression::Try {
                expr: Box::new(left),
                question_token,
            }))
        }

        _ => {
            return Err(ParseError::new("Expected infix operator")
                .with_span(start_token.span)
                .with_expected("binary operator, call, index or field access"));
        }
    };

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

    if let Ok(grouped) = stream.try_parse(|s| {
        let expr = parse_expression(s)?;
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
    }) {
        return Ok(grouped);
    }

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

    if let Ok(repeat_expr) = stream.try_parse(|s| {
        let value = parse_expression(s)?;
        let semi_token = s.expect_symbol(Symbol::Semi)?;
        let count = <Spanned<usize>>::parse(s)?;
        let close_token = s.expect_symbol(Symbol::RBracket)?;
        Ok(ArrayExpression::Repeat {
            bracket_token: Bracket {
                open: open_token,
                close: close_token,
            },
            value: Box::new(value),
            semi_token,
            count,
        })
    }) {
        return Ok(repeat_expr);
    }

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

fn parse_while(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let while_token = stream.consume()?;
    let cond = Box::new(parse_expression(stream)?);
    let body = BlockExpression::parse(stream)?;

    Ok(Expression::WithBlock(ExpressionWithBlock::While(
        WhileLoopExpression {
            label: None,
            while_token,
            cond,
            body,
        },
    )))
}

fn parse_loop(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let loop_token = stream.consume()?;
    let body = BlockExpression::parse(stream)?;

    Ok(Expression::WithBlock(ExpressionWithBlock::Loop(
        LoopExpression {
            label: None,
            loop_token,
            body,
        },
    )))
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

fn parse_break(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let break_token = stream.consume()?;
    let label = if let Ok(label) = stream.try_parse(|s| IdentSpan::parse(s)) {
        Some(label)
    } else {
        None
    };

    if !stream.next_is(Symbol::Semi) && !stream.next_is(Symbol::RBrace) {
        let expr = Box::new(parse_expression(stream)?);
        return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Break(
            BreakExpression {
                break_token,
                label,
                expr: Some(expr),
            },
        )));
    }

    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Break(
        BreakExpression {
            break_token,
            label,
            expr: None,
        },
    )))
}

fn parse_continue(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let continue_token = stream.consume()?;
    let label = if let Ok(label) = stream.try_parse(|s| IdentSpan::parse(s)) {
        Some(label)
    } else {
        None
    };

    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Continue(
        ContinueExpression {
            continue_token,
            label,
        },
    )))
}

fn parse_return(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let return_token = stream.consume()?;

    if !stream.next_is(Symbol::Semi) && !stream.next_is(Symbol::RBrace) {
        let expr = Box::new(parse_expression(stream)?);
        return Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Return(
            ReturnExpression {
                return_token,
                expr: Some(expr),
            },
        )));
    }

    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Return(
        ReturnExpression {
            return_token,
            expr: None,
        },
    )))
}

fn parse_match(stream: &mut ParseStream) -> Result<Expression, ParseError> {
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
        let comma_token = stream
            .next_is(Symbol::Comma)
            .then(|| stream.consume())
            .transpose()?;

        arms.push(MatchArm {
            pat,
            guard,
            fat_arrow_token,
            body,
            comma_token,
        });

        if comma_token.is_none() && !stream.next_is(Symbol::RBrace) {
            return Err(ParseError::new("Expected comma after match arm").with_span(body.span()));
        }
    }

    let close_brace = stream.expect_symbol(Symbol::RBrace)?;

    Ok(Expression::WithBlock(ExpressionWithBlock::Match(
        MatchExpression {
            match_token,
            expr,
            brace_token: Brace {
                open: open_brace,
                close: close_brace,
            },
            arms,
        },
    )))
}

fn parse_closure(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let or1_token = stream.consume()?;

    if let Some(closure) = stream.try_parse(|s| {
        let or2_token = s.expect_symbol(Symbol::Or)?;
        let body = Box::new(parse_expr(s, Precedence::None)?);
        Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Closure(
            ClosureExpression {
                move_token: None,
                or_token: (or1_token, or2_token),
                inputs: Punctuated::new(),
                output: None,
                body,
            },
        )))
    }) {
        return closure;
    }

    let inputs = stream.parse_punctuated_with(|s| Pattern::parse(s), Symbol::Comma)?;
    let or2_token = stream.expect_symbol(Symbol::Or)?;
    let output = stream
        .next_is_symbol(Symbol::RArrow)
        .then(|| Ok((stream.consume()?, Box::new(Type::parse(stream)?))))
        .transpose()?;
    let body = Box::new(parse_expression(stream)?);

    Ok(Expression::WithoutBlock(ExpressionWithoutBlock::Closure(
        ClosureExpression {
            move_token: None,
            or_token: (or1_token, or2_token),
            inputs,
            output,
            body,
        },
    )))
}

impl Parse for BlockExpression {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let stmts = stream.parse_punctuated_with(Statement::parse, &Token::Symbol(Symbol::Semi))?;
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
