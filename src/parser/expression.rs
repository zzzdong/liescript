use crate::diagnostic::Spanned;
use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};

use crate::parser::parse;
use crate::syntax::names::Path;
use crate::syntax::patterns::Pattern;
use crate::syntax::statements::Statement;
use crate::syntax::types::Type;
use crate::syntax::{BinOp, RangeLimits, UnOp};
use crate::syntax::{PostfixOp, expressions::*};

use super::parse::{Parse, ParseError, ParseStream};



pub fn parse_expression(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    parse_expr(stream, 0)
}

/// 解析表达式 (使用Pratt parser算法)
fn parse_expr(stream: &mut ParseStream, min_bp: u8) -> Result<Expression, ParseError> {
    // 处理以 .. 开头的范围表达式（如 ..10 或 ..=10）
    if let Some(expr) = stream.try_parse(|s| parse_range(s, None)) {
        return Ok(Expression::Range(expr));
    }

    let mut expr = parse_prefix(stream)?;

    while let Some(peek) = stream.peek() {
        // 处理范围表达式（如 1..10 或 1..=10）
        if matches!(
            peek.value(),
            Token::Symbol(Symbol::DotDot) | Token::Symbol(Symbol::DotDotEq)
        ) {
            let range = parse_range(stream, Some(expr))?;
            expr = Expression::Range(range);
            continue;
        }

        // Check if token is a binary operator
        if let Some(bin_op) = BinOp::from_token(&peek) {
            let (left_bp, right_bp) = bin_op.get_binding_power();
            if left_bp < min_bp {
                break;
            }
            expr = parse_infix(stream, expr, right_bp)?;
            continue;
        }

        // Check if token is a postfix operator
        if PostfixOp::from_token(&peek).is_some() {
            expr = parse_postfix(stream, expr)?;
            continue;
        }

        // Not an operator token, end expression parsing

        break;
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
        Token::Symbol(Symbol::LBracket) => parse_array(stream).map(|expr| Expression::Array(expr)),
        Token::Symbol(Symbol::LBrace) => parse_block(stream).map(|expr| Expression::Block(expr)),
        Token::Keyword(Keyword::If) => parse_if(stream).map(|expr| Expression::If(expr)),
        Token::Keyword(Keyword::While) => parse_while(stream).map(|expr| Expression::While(expr)),
        Token::Keyword(Keyword::Loop) => parse_loop(stream).map(|expr| Expression::Loop(expr)),
        Token::Keyword(Keyword::For) => parse_for(stream).map(|expr| Expression::For(expr)),
        Token::Keyword(Keyword::Break) => parse_break(stream).map(|expr| Expression::Break(expr)),
        Token::Keyword(Keyword::Continue) => {
            parse_continue(stream).map(|expr| Expression::Continue(expr))
        }
        Token::Keyword(Keyword::Return) => {
            parse_return(stream).map(|expr| Expression::Return(expr))
        }
        Token::Keyword(Keyword::Match) => parse_match(stream).map(|expr| Expression::Match(expr)),
        Token::Keyword(Keyword::Async) => {
            parse_async_block(stream).map(|expr| Expression::Async(expr))
        }
        Token::Symbol(Symbol::Or) | Token::Symbol(Symbol::OrOr) => {
            parse_closure(stream).map(|expr| Expression::Closure(expr))
        }

        _ => Err(ParseError::new("Expected primary expression")
            .with_span(start_token.span)
            .with_expected("valid primary expression")
            .with_found(&start_token)),
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
                return Ok(Expression::Operator(OperatorExpression::Borrow {
                    and_token,
                    is_mut,
                    expr: Box::new(parse_prefix(stream)?),
                }));
            }

            // 解引用表达式
            Token::Symbol(Symbol::Star) => {
                let star_token = stream.consume()?;
                return Ok(Expression::Operator(OperatorExpression::Deref {
                    star_token,
                    expr: Box::new(parse_prefix(stream)?),
                }));
            }

            // 一元运算符
            Token::Symbol(Symbol::Not) | Token::Symbol(Symbol::Minus) => {
                let op = Spanned::<UnOp>::parse(stream)?;
                return Ok(Expression::Operator(OperatorExpression::Neg {
                    op,
                    expr: Box::new(parse_prefix(stream)?),
                }));
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

    let right = Box::new(parse_expr(stream, right_bp)?);

    let expr = match op.value() {
        BinOp::Assign => Expression::Operator(OperatorExpression::Assign {
            left: Box::new(left),
            eq_token: start_token.clone(),
            right,
        }),
        BinOp::AddAssign
        | BinOp::SubAssign
        | BinOp::MulAssign
        | BinOp::DivAssign
        | BinOp::RemAssign => Expression::Operator(OperatorExpression::AssignOp {
            left: Box::new(left),
            op,
            right,
        }),
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Rem
        | BinOp::BitAnd
        | BinOp::BitOr
        | BinOp::BitXor
        | BinOp::BitShl
        | BinOp::BitShr => Expression::Operator(OperatorExpression::Arithmetic {
            left: Box::new(left),
            op,
            right,
        }),
        BinOp::LogicAnd | BinOp::LogicOr => Expression::Operator(OperatorExpression::Logical {
            left: Box::new(left),
            op,
            right,
        }),
        BinOp::Eq
        | BinOp::NotEq
        | BinOp::LessThen
        | BinOp::GreaterThen
        | BinOp::LessThenOrEq
        | BinOp::GreaterThenOrEq => Expression::Operator(OperatorExpression::Comparison {
            left: Box::new(left),
            op,
            right,
        }),
        _ => unreachable!("unexpected operator: {op:?}"),
    };

    Ok(expr)
}

fn parse_postfix(stream: &mut ParseStream, expr: Expression) -> Result<Expression, ParseError> {
    let mut expr = expr;

    while let Some(peek) = stream.peek() {
        match peek.value() {
            Token::Symbol(Symbol::Dot) => {
                let dot_token = stream.consume()?;
                let peek = stream.peek().ok_or(ParseError::eof())?;

                match peek.value() {
                    Token::Keyword(Keyword::Await) => {
                        let await_token = stream.consume()?;
                        expr = Expression::Await(AwaitExpression {
                            expr: Box::new(expr),
                            dot_token,
                            await_token,
                        });
                    }
                    Token::Ident(_name) => {
                        let field = IdentSpan::parse(stream)?;

                        expr = Expression::Field(FieldExpression {
                            expr: Box::new(expr),
                            dot_token,
                            field,
                        });
                    }

                    Token::Literal(Literal::Integer(_i)) => {
                        let index = <Spanned<u32>>::parse(stream)?;

                        expr = Expression::TupleIndex(TupleIndexingExpression {
                            expr: Box::new(expr),
                            dot_token,
                            index,
                        });
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
                    Expression::Field(FieldExpression {
                        expr,
                        dot_token,
                        field,
                    }) => Expression::MethodCall(MethodCallExpression {
                        expr,
                        dot_token,
                        method: field,
                        paren_token: Paren {
                            open: open_token,
                            close: close_token,
                        },
                        args,
                    }),
                    _ => Expression::Call(CallExpression {
                        expr: Box::new(expr),
                        paren_token: Paren {
                            open: open_token,
                            close: close_token,
                        },
                        args,
                    }),
                }
            }

            Token::Symbol(Symbol::LBracket) => {
                let open_token = stream.consume()?;
                let index = Box::new(parse_expression(stream)?);
                let close_token = stream.expect_symbol(Symbol::RBracket)?;

                expr = Expression::Index(IndexExpression {
                    expr: Box::new(expr),
                    bracket_token: Bracket {
                        open: open_token,
                        close: close_token,
                    },
                    index,
                });
            }

            Token::Symbol(Symbol::Question) => {
                let question_token = stream.consume()?;
                expr = Expression::Operator(OperatorExpression::Try {
                    expr: Box::new(expr),
                    question_token,
                });
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
    Ok(Expression::Literal(LiteralExpression { lit }))
}

fn parse_path(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let path = Path::parse(stream)?;
    Ok(Expression::Path(PathExpression { path }))
}

fn parse_underscore(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let underscore_token = stream.consume()?;
    Ok(Expression::Underscore(UnderscoreExpression {
        underscore_token,
    }))
}

fn parse_paren(stream: &mut ParseStream) -> Result<Expression, ParseError> {
    let open_token = stream.consume()?;

    // 空元组
    if stream.next_is(Symbol::RParen) {
        let close_token = stream.consume()?;
        return Ok(Expression::Tuple(TupleExpression {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            elems: Punctuated::new(),
        }));
    }

    // 尝试解析为分组表达式（单个表达式后跟右括号）
    let result = stream.try_parse(|s| {
        let expr = parse_expression(s)?;

        // 如果下一个token是右括号，则这是一个分组表达式
        let close_token = s.expect_symbol(Symbol::RParen)?;

        Ok(Expression::Grouped(GroupedExpression {
            paren_token: Paren {
                open: open_token.clone(),
                close: close_token,
            },
            expr: Box::new(expr),
        }))
    });

    if let Some(grouped) = result {
        return Ok(grouped);
    }

    // 如果不是分组表达式，则尝试解析为元组表达式
    let elems =
        stream.parse_punctuated_with(|s| parse_expression(s), &Token::Symbol(Symbol::Comma))?;

    let close_token = stream.expect_symbol(Symbol::RParen)?;

    Ok(Expression::Tuple(TupleExpression {
        paren_token: Paren {
            open: open_token,
            close: close_token,
        },
        elems,
    }))
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

    // Parse condition
    let cond = Box::new(parse_expression(stream)?);

    // Parse then branch
    let then_branch = BlockExpression::parse(stream)?;

    // Parse else branch if present
    let else_branch = if stream.next_is(Keyword::Else) {
        let else_token = stream.consume()?;

        if stream.next_is(Keyword::If) {
            let else_if_expr = parse_if(stream)?;
            Some((else_token, Box::new(ElseBranch::If(Box::new(else_if_expr)))))
        } else {
            let else_block = BlockExpression::parse(stream)?;
            Some((else_token, Box::new(ElseBranch::Block(else_block))))
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

fn parse_async_block(stream: &mut ParseStream) -> Result<AsyncBlockExpression, ParseError> {
    let async_token = stream.consume()?;
    let block = BlockExpression::parse(stream)?;

    Ok(AsyncBlockExpression { async_token, block })
}

fn parse_closure(stream: &mut ParseStream) -> Result<ClosureExpression, ParseError> {
    // try parse || expr, `||` will be parsed as a binary operator
    if let Some(expr) = stream.try_parse(|s| {
        let oror_token = s.expect_symbol(Symbol::OrOr)?;
        let body = Box::new(parse_expression(s)?);
        Ok(ClosureExpression {
            move_token: None,
            or_token: (oror_token.clone(), oror_token),
            inputs: Punctuated::new(),
            output: None,
            body,
        })
    }) {
        return Ok(expr);
    }

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

impl PartialEq for BinOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BinOp::Add, BinOp::Add) => true,
            (BinOp::Sub, BinOp::Sub) => true,
            (BinOp::Mul, BinOp::Mul) => true,
            (BinOp::Div, BinOp::Div) => true,
            (BinOp::Rem, BinOp::Rem) => true,
            (BinOp::BitAnd, BinOp::BitAnd) => true,
            (BinOp::BitOr, BinOp::BitOr) => true,
            (BinOp::BitXor, BinOp::BitXor) => true,
            (BinOp::BitShl, BinOp::BitShl) => true,
            (BinOp::BitShr, BinOp::BitShr) => true,
            (BinOp::LogicAnd, BinOp::LogicAnd) => true,
            (BinOp::LogicOr, BinOp::LogicOr) => true,
            (BinOp::Eq, BinOp::Eq) => true,
            (BinOp::NotEq, BinOp::NotEq) => true,
            (BinOp::LessThen, BinOp::LessThen) => true,
            (BinOp::GreaterThen, BinOp::GreaterThen) => true,
            (BinOp::LessThenOrEq, BinOp::LessThenOrEq) => true,
            (BinOp::GreaterThenOrEq, BinOp::GreaterThenOrEq) => true,
            (BinOp::Assign, BinOp::Assign) => true,
            (BinOp::AddAssign, BinOp::AddAssign) => true,
            (BinOp::SubAssign, BinOp::SubAssign) => true,
            (BinOp::MulAssign, BinOp::MulAssign) => true,
            (BinOp::DivAssign, BinOp::DivAssign) => true,
            (BinOp::RemAssign, BinOp::RemAssign) => true,
            _ => false,
        }
    }
}

impl PartialEq for UnOp {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (UnOp::Neg, UnOp::Neg) => true,
            (UnOp::Not, UnOp::Not) => true,
            _ => false,
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

fn parse_range(
    stream: &mut ParseStream,
    start: Option<Expression>,
) -> Result<RangeExpression, ParseError> {
    let limits = <Spanned<RangeLimits>>::parse(stream)?;

    // 使用 try_parse 来尝试解析右条件，如果失败则返回 None
    let end = stream.try_parse(parse_expression).map(Box::new);

    Ok(RangeExpression {
        start: start.map(Box::new),
        limits,
        end,
    })
}

impl Parse for Label {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let label = IdentSpan::parse(stream)?;
        let colon_token = stream.consume()?;

        Ok(Label { colon_token, label })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::{Pos, Span};
    use crate::lexical::{IdentSpan, Identifier, Literal, Token, TokenStream};
    use crate::syntax::{PathSegment, expressions::*};

    /// 解析表达式并忽略Span信息进行比较
    fn assert_expr_eq(input: &str, expected: Expression) {
        let actual = parse_expr(input).unwrap();
        assert_eq!(actual, expected);
    }

    fn path_expr(paths: &[&str]) -> PathExpression {
        let mut segments = Punctuated::new();
        if let Some((last, rest)) = paths.split_last() {
            for path in rest {
                segments.push(
                    PathSegment {
                        ident: IdentSpan::new(Identifier::new(path), Span::dummy()),
                    },
                    Spanned::new(Token::Symbol(Symbol::ColonColon), Span::dummy()),
                );
            }
            segments.last = Some(Box::new(PathSegment {
                ident: IdentSpan::new(Identifier::new(last), Span::dummy()),
            }));
        }

        PathExpression {
            path: Path {
                leading_colon: None,
                segments,
            },
        }
    }

    fn parse_expr(input: &str) -> Result<Expression, ParseError> {
        let tokens = TokenStream::parse(input).unwrap();
        let mut stream = ParseStream::new(&tokens);
        parse_expression(&mut stream)
    }

    #[test]
    fn test_literal_expression() {
        assert_expr_eq(
            "1",
            Expression::Literal(LiteralExpression {
                lit: Spanned::new(Literal::Integer(1), Span::dummy()),
            }),
        );
        assert!(parse_expr("\"string\"").is_ok());
        assert!(parse_expr("true").is_ok());
    }

    #[test]
    fn test_identifier_expression() {
        assert_expr_eq("foo", Expression::Path(path_expr(&["foo"])));
        assert_expr_eq(
            "_bar",
            Expression::Underscore(UnderscoreExpression {
                underscore_token: TokenSpan::new(Token::Symbol(Symbol::Underscore), Span::dummy()),
            }),
        );
    }

    #[test]
    fn test_binary_expression() {
        assert_expr_eq(
            "1 + 2",
            Expression::Operator(OperatorExpression::Arithmetic {
                left: Box::new(Expression::Literal(LiteralExpression {
                    lit: Spanned::new(Literal::Integer(1), Span::dummy()),
                })),
                op: Spanned::new(BinOp::Add, Span::dummy()),
                right: Box::new(Expression::Literal(LiteralExpression {
                    lit: Spanned::new(Literal::Integer(2), Span::dummy()),
                })),
            }),
        );
        assert_expr_eq(
            "a == b",
            Expression::Operator(OperatorExpression::Comparison {
                left: Box::new(Expression::Path(path_expr(&["a"]))),
                op: Spanned::new(BinOp::Eq, Span::dummy()),
                right: Box::new(Expression::Path(path_expr(&["b"]))),
            }),
        );
        assert_expr_eq(
            "x && y",
            Expression::Operator(OperatorExpression::Logical {
                left: Box::new(Expression::Path(path_expr(&["x"]))),
                op: Spanned::new(BinOp::LogicAnd, Span::dummy()),
                right: Box::new(Expression::Path(path_expr(&["y"]))),
            }),
        );
    }

    #[test]
    fn test_unary_expression() {
        assert_expr_eq(
            "-1",
            Expression::Operator(OperatorExpression::Neg {
                op: Spanned::new(UnOp::Neg, Span::dummy()),
                expr: Box::new(Expression::Literal(LiteralExpression {
                    lit: Spanned::new(Literal::Integer(1), Span::dummy()),
                })),
            }),
        );
        assert_expr_eq(
            "!flag",
            Expression::Operator(OperatorExpression::Neg {
                op: Spanned::new(UnOp::Not, Span::dummy()),
                expr: Box::new(Expression::Path(path_expr(&["flag"]))),
            }),
        );
        assert_expr_eq(
            "*ptr",
            Expression::Operator(OperatorExpression::Deref {
                star_token: TokenSpan::new(Token::Symbol(Symbol::Star), Span::dummy()),
                expr: Box::new(Expression::Path(path_expr(&["ptr"]))),
            }),
        );
    }

    #[test]
    fn test_grouping_expression() {
        assert_expr_eq(
            "(1 + 2)",
            Expression::Grouped(GroupedExpression {
                paren_token: Paren {
                    open: TokenSpan::new(Token::Symbol(Symbol::LParen), Span::dummy()),
                    close: TokenSpan::new(Token::Symbol(Symbol::RParen), Span::dummy()),
                },
                expr: Box::new(Expression::Operator(OperatorExpression::Arithmetic {
                    left: Box::new(Expression::Literal(LiteralExpression {
                        lit: Spanned::new(Literal::Integer(1), Span::dummy()),
                    })),
                    op: Spanned::new(BinOp::Add, Span::dummy()),
                    right: Box::new(Expression::Literal(LiteralExpression {
                        lit: Spanned::new(Literal::Integer(2), Span::dummy()),
                    })),
                })),
            }),
        );
        assert_expr_eq(
            "((a))",
            Expression::Grouped(GroupedExpression {
                paren_token: Paren {
                    open: TokenSpan::new(Token::Symbol(Symbol::LParen), Span::dummy()),
                    close: TokenSpan::new(Token::Symbol(Symbol::RParen), Span::dummy()),
                },
                expr: Box::new(Expression::Grouped(GroupedExpression {
                    paren_token: Paren {
                        open: TokenSpan::new(Token::Symbol(Symbol::LParen), Span::dummy()),
                        close: TokenSpan::new(Token::Symbol(Symbol::RParen), Span::dummy()),
                    },
                    expr: Box::new(Expression::Path(path_expr(&["a"]))),
                })),
            }),
        );
    }

    #[test]
    fn test_if_expression() {
        let result = parse_expr("if x { 1 } else { 2 }");
        assert!(result.is_ok());

        let result = parse_expr("if a { b } else if c { d } else { e }");
        assert!(result.is_ok());
    }

    #[test]
    fn test_loop_expressions() {
        let result = parse_expr("while x { y }");

        assert!(result.is_ok());

        let result = parse_expr("loop { break }");

        assert!(result.is_ok());

        let result = parse_expr("for i in 0..10 { i }");

        assert!(result.is_ok());
    }

    #[test]
    fn test_match_expression() {
        let result = parse_expr("match x { 1 => 2, _ => 3 }");

        assert!(result.is_ok());
    }

    #[test]
    fn test_error_handling() {
        assert!(parse_expr("1 +").is_err());
        assert!(parse_expr("if x {").is_err());
        assert!(parse_expr("match x { 1 =>").is_err());
    }

    #[test]
    fn test_operator_precedence() {
        // 乘法优先级高于加法
        assert!(parse_expr("1 + 2 * 3").is_ok());
        assert!(parse_expr("1 * 2 + 3").is_ok());

        // 比较运算符优先级高于逻辑运算符
        assert!(parse_expr("a == b && c != d").is_ok());
        assert!(parse_expr("x > y || z < w").is_ok());

        // 一元运算符优先级最高
        assert!(parse_expr("-a * b").is_ok());
        assert!(parse_expr("!flag && condition").is_ok());
    }

    #[test]
    fn test_range_expressions() {
        // 各种范围表达式形式
        assert!(parse_expr("1..10").is_ok());
        assert!(parse_expr("1..=10").is_ok());
        assert!(parse_expr("..10").is_ok());
        assert!(parse_expr("1..").is_ok());
        assert!(parse_expr("..").is_ok());

        // 范围表达式在复杂表达式中的使用
        assert!(parse_expr("for i in 0..10 { i }").is_ok());
        assert!(parse_expr("x >= 1..10").is_ok());
    }

    #[test]
    fn test_complex_expressions() {
        // 嵌套表达式
        assert!(parse_expr("(1 + 2) * (3 - 4)").is_ok());
        assert!(parse_expr("a + b * c - d / e").is_ok());

        // 混合运算符
        assert!(parse_expr("a && b || c && d").is_ok());
        assert!(parse_expr("x == y && z != w").is_ok());

        // // 函数调用与运算符组合
        // assert!(parse_expr("foo() + bar()").is_ok());
        let ret = parse_expr("a.method() * 2");

        assert!(ret.is_ok());
    }

    #[test]
    fn test_assignment_expressions() {
        // 各种赋值表达式
        assert!(parse_expr("x = 1").is_ok());
        assert!(parse_expr("y += 2").is_ok());
        assert!(parse_expr("z *= a + b").is_ok());
    }

    #[test]
    fn test_field_and_method_access() {
        // 字段访问
        assert!(parse_expr("obj.field").is_ok());

        let ret = parse_expr("object.field.method()");

        assert!(ret.is_ok());

        // 方法调用
        assert!(parse_expr("obj.method()").is_ok());
        assert!(parse_expr("obj.method(arg1, arg2)").is_ok());
    }

    #[test]
    fn test_array_and_index_expressions() {
        // 数组表达式
        assert!(parse_expr("[1, 2, 3]").is_ok());
        assert!(parse_expr("[0; 10]").is_ok());

        // 索引访问
        assert!(parse_expr("arr[0]").is_ok());
        assert!(parse_expr("matrix[1][2]").is_ok());
    }

    #[test]
    fn test_closure_expressions() {
        // 闭包表达式
        assert!(parse_expr("|| x + 1").is_ok());
        assert!(parse_expr("|x| x * 2").is_ok());
        assert!(parse_expr("|x, y| x + y").is_ok());
        assert!(parse_expr("|| { x + 1 }").is_ok());
    }
}
