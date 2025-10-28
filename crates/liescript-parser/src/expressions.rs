//! 表达式解析模块 (使用Pratt parser)

use liescript_ast::{
    Type, expressions::{
        ArrayExpression, BlockExpression, BreakExpression, CallExpression, ClosureExpression, ClosureParam, ContinueExpression, ElseBranch, Expression, FieldExpression, ForLoopExpression, GroupedExpression, IfExpression, IndexExpression, LiteralExpression, LoopExpression, MatchExpression, MethodCallExpression, OperatorExpression, PathExpression, RangeExpression, ReturnExpression, TupleIndexingExpression, UnderscoreExpression, WhileLoopExpression
    }, operators::{BinOp, UnOp}, patterns::{Pattern, RangeLimits}, statements::Statement
};
use liescript_lexical::{
    Spanned,
    keyword::Keyword,
    literal::Literal,
    symbol::Symbol,
    token::{Brace, Bracket, Paren, Punctuated, Token, TokenSpan},
};

use super::{
    Parse,
    context::ParseContext,
    diagnostic::{ParseError, ParseResult},
};

/// 解析表达式 (使用Pratt parser算法)
pub fn parse_expression(cx: &mut ParseContext) -> ParseResult<Expression> {
    parse_expr(cx, 0)
}

/// 解析表达式 (Pratt parser核心算法)
fn parse_expr(cx: &mut ParseContext, min_bp: u8) -> ParseResult<Expression> {
    // 处理以 .. 开头的范围表达式（如 ..10 或 ..=10）
    if let Some(expr) = try_parse_range_start(cx)? {
        return Ok(Expression::Range(expr));
    }

    let mut expr = parse_prefix(cx)?;

    while let Some(token) = cx.peek_token() {
        // 处理范围表达式（如 1..10 或 1..=10）
        if matches!(
            token,
            &Token::Symbol(Symbol::DotDot) | &Token::Symbol(Symbol::DotDotEq)
        ) {
            let range = parse_range(cx, Some(expr))?;
            expr = Expression::Range(range);
            continue;
        }

        // 检查是否是二元运算符
        if let Some(bin_op) = get_bin_op(token) {
            let (left_bp, right_bp) = get_binding_power(&bin_op);
            if left_bp < min_bp {
                break;
            }
            expr = parse_infix(cx, expr, right_bp)?;
            continue;
        }

        // 检查是否是后缀运算符
        if is_postfix_op(token) {
            expr = parse_postfix(cx, expr)?;
            continue;
        }

        // 不是运算符token，结束表达式解析
        break;
    }

    Ok(expr)
}

/// 尝试解析以..开头的范围表达式
fn try_parse_range_start(cx: &mut ParseContext) -> ParseResult<Option<RangeExpression>> {
    // 使用try_parse机制而不是位置保存
    if let Some(token) = cx.peek_token() {
        if matches!(
            token,
            &Token::Symbol(Symbol::DotDot) | &Token::Symbol(Symbol::DotDotEq)
        ) {
            let range = parse_range(cx, None)?;
            return Ok(Some(range));
        }
    }
    Ok(None)
}

/// 解析主表达式 (字面量、标识符、括号表达式等)
fn parse_primary(cx: &mut ParseContext) -> ParseResult<Expression> {
    if let Some(token) = cx.peek() {
        let token_value = token.value.clone();
        let token_span = token.span;

        match token_value {
            Token::Literal(_) => parse_literal(cx).map(Into::into),
            Token::Ident(_) => parse_path_expression(cx).map(Into::into),
            Token::Keyword(keyword) => match keyword {
                Keyword::If => parse_if_expression(cx).map(Into::into),
                Keyword::Match => parse_match_expression(cx).map(Into::into),
                Keyword::Loop => parse_loop_expression(cx).map(Into::into),
                Keyword::While => parse_while_expression(cx).map(Into::into),
                Keyword::For => parse_for_expression(cx).map(Into::into),
                Keyword::Return => parse_return_expression(cx).map(Into::into),
                Keyword::Break => parse_break_expression(cx).map(Into::into),
                Keyword::Continue => parse_continue_expression(cx).map(Into::into),
                Keyword::True | Keyword::False => parse_literal(cx).map(Into::into), // 布尔字面量
                _ => parse_path_expression(cx).map(Into::into), // 其他关键字作为路径处理
            },
            Token::Symbol(symbol) => match symbol {
                Symbol::LParen => parse_grouped_expression(cx).map(Into::into),
                Symbol::LBracket => parse_array_expression(cx).map(Into::into),
                Symbol::LBrace => parse_block_expression(cx).map(Into::into),
                Symbol::Underscore => parse_underscore_expression(cx).map(Into::into),
                Symbol::Or | Symbol::OrOr => {
                    parse_closure(cx).map(|expr| Expression::Closure(expr))
                }
                _ => Err(cx.create_error(
                    "Unexpected symbol".to_string(),
                    token_span,
                    Some("expression".to_string()),
                    Some(format!("{:?}", token_value)),
                )),
            },
            // 处理其他Token类型
            _ => Err(cx.create_error(
                "Unexpected token".to_string(),
                token_span,
                Some("expression".to_string()),
                Some(format!("{:?}", token_value)),
            )),
        }
    } else {
        Err(cx.create_eof_error("expression"))
    }
}

/// 解析前缀表达式
fn parse_prefix(cx: &mut ParseContext) -> ParseResult<Expression> {
    if let Some(token) = cx.peek_token() {
        match token {
            // 引用表达式
            &Token::Symbol(Symbol::And) => {
                let and_token = cx.consume()?;
                let is_mut = if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::Mut))) {
                    Some(cx.consume()?)
                } else {
                    None
                };
                return Ok(Expression::Operator(OperatorExpression::Borrow {
                    and_token,
                    is_mut,
                    expr: Box::new(parse_prefix(cx)?),
                }));
            }

            // 一元运算符
            &Token::Symbol(Symbol::Not) | &Token::Symbol(Symbol::Minus) => {
                let op_token = cx.consume()?;
                let op = match op_token.value {
                    Token::Symbol(Symbol::Not) => UnOp::Not,
                    Token::Symbol(Symbol::Minus) => UnOp::Neg,
                    _ => unreachable!(),
                };
                return Ok(Expression::Operator(OperatorExpression::Neg {
                    op: Spanned::new(op, op_token.span),
                    expr: Box::new(parse_prefix(cx)?),
                }));
            }

            // 不是前缀操作符，解析主表达式
            _ => parse_primary(cx),
        }
    } else {
        Err(cx.create_eof_error("expression"))
    }
}

/// 解析中缀表达式
fn parse_infix(cx: &mut ParseContext, left: Expression, right_bp: u8) -> ParseResult<Expression> {
    let token = cx.consume()?;

    let bin_op = get_bin_op(&token.value).unwrap();

    let right = Box::new(parse_expr(cx, right_bp)?);

    let expr = match bin_op {
        BinOp::Assign => Expression::Operator(OperatorExpression::Assign {
            left: Box::new(left),
            eq_token: token,
            right,
        }),
        _ => Expression::Operator(OperatorExpression::Arithmetic {
            left: Box::new(left),
            op: Spanned::new(bin_op, token.span),
            right,
        }),
    };

    Ok(expr)
}

/// 解析后缀表达式
fn parse_postfix(cx: &mut ParseContext, expr: Expression) -> ParseResult<Expression> {
    let mut expr = expr;

    while let Some(token) = cx.peek_token() {
        match token {
            &Token::Symbol(Symbol::Dot) => {
                let dot_token = cx.consume()?;

                if let Some(next_token) = cx.peek_token() {
                    match next_token {
                        Token::Ident(ident) => {
                            // 字段访问
                            let ident = ident.clone();
                            let field = cx.expect_identifier()?;
                            expr = Expression::Field(FieldExpression {
                                expr: Box::new(expr),
                                dot_token,
                                field: field.map(|_| ident.clone()),
                            });
                        }
                        &Token::Literal(_) => {
                            // 元组索引访问
                            let index_token = cx.consume()?;
                            if let Token::Literal(literal) = &index_token.value {
                                if let Literal::Integer(index) = literal {
                                    let index = *index;
                                    if index < 0 || index > u32::MAX as i64 {
                                        return Err(cx.create_error(
                                            "Invalid tuple index".to_string(),
                                            index_token.span,
                                            Some("integer".to_string()),
                                            Some(index.to_string()),
                                        ));
                                    }
                                    expr = Expression::TupleIndex(TupleIndexingExpression {
                                        expr: Box::new(expr),
                                        dot_token,
                                        index: Spanned::new(index as u32, index_token.span),
                                    });
                                } else {
                                    return Err(cx.create_error(
                                        "Expected integer literal for tuple index".to_string(),
                                        index_token.span,
                                        Some("integer literal".to_string()),
                                        Some(format!("{:?}", index_token.value)),
                                    ));
                                }
                            } else {
                                unreachable!()
                            }
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }

            &Token::Symbol(Symbol::LParen) => {
                let lparen = cx.expect_symbol(Symbol::LParen)?;

                let args = cx.parse_punctuated(parse_expression, Symbol::Comma)?;

                let rparen = cx.expect_symbol(Symbol::RParen)?;

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
                            open: lparen,
                            close: rparen,
                        },
                        args,
                    }),
                    _ => Expression::Call(CallExpression {
                        expr: Box::new(expr),
                        paren_token: Paren {
                            open: lparen,
                            close: rparen,
                        },
                        args,
                    }),
                }
            }

            &Token::Symbol(Symbol::LBracket) => {
                let lbracket = cx.expect_symbol(Symbol::LBracket)?;
                let index = parse_expression(cx)?;
                let rbracket = cx.expect_symbol(Symbol::RBracket)?;

                expr = Expression::Index(IndexExpression {
                    expr: Box::new(expr),
                    bracket_token: Bracket::new(lbracket, rbracket),
                    index: Box::new(index),
                });
            }

            &Token::Symbol(Symbol::Question) => {
                let question_token = cx.expect_symbol(Symbol::Question)?;
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

/// 从token获取二元运算符
fn get_bin_op(token: &Token) -> Option<BinOp> {
    match token {
        Token::Symbol(Symbol::Eq) => Some(BinOp::Assign),
        Token::Symbol(Symbol::Plus) => Some(BinOp::Add),
        Token::Symbol(Symbol::Minus) => Some(BinOp::Sub),
        Token::Symbol(Symbol::Star) => Some(BinOp::Mul),
        Token::Symbol(Symbol::Slash) => Some(BinOp::Div),
        Token::Symbol(Symbol::Percent) => Some(BinOp::Rem),
        Token::Symbol(Symbol::And) => Some(BinOp::BitAnd),
        Token::Symbol(Symbol::Or) => Some(BinOp::BitOr),
        Token::Symbol(Symbol::Caret) => Some(BinOp::BitXor),
        Token::Symbol(Symbol::Shl) => Some(BinOp::BitShl),
        Token::Symbol(Symbol::Shr) => Some(BinOp::BitShr),
        Token::Symbol(Symbol::AndAnd) => Some(BinOp::LogicAnd),
        Token::Symbol(Symbol::OrOr) => Some(BinOp::LogicOr),
        Token::Symbol(Symbol::EqEq) => Some(BinOp::Eq),
        Token::Symbol(Symbol::Ne) => Some(BinOp::NotEq),
        Token::Symbol(Symbol::Lt) => Some(BinOp::LessThen),
        Token::Symbol(Symbol::Gt) => Some(BinOp::GreaterThen),
        Token::Symbol(Symbol::Le) => Some(BinOp::LessThenOrEq),
        Token::Symbol(Symbol::Ge) => Some(BinOp::GreaterThenOrEq),
        _ => None,
    }
}

/// 获取运算符的绑定力
fn get_binding_power(op: &BinOp) -> (u8, u8) {
    match op {
        BinOp::Assign => (2, 1),
        BinOp::AddAssign
        | BinOp::SubAssign
        | BinOp::MulAssign
        | BinOp::DivAssign
        | BinOp::RemAssign => (2, 1),
        BinOp::LogicOr => (4, 5),
        BinOp::LogicAnd => (6, 7),
        BinOp::Eq
        | BinOp::NotEq
        | BinOp::LessThen
        | BinOp::GreaterThen
        | BinOp::LessThenOrEq
        | BinOp::GreaterThenOrEq => (8, 9),
        BinOp::BitOr => (10, 11),
        BinOp::BitXor => (12, 13),
        BinOp::BitAnd => (14, 15),
        BinOp::BitShl | BinOp::BitShr => (16, 17),
        BinOp::Add | BinOp::Sub => (18, 19),
        BinOp::Mul | BinOp::Div | BinOp::Rem => (20, 21),
        BinOp::Range | BinOp::RangeInclusive => (22, 23),
        BinOp::Cast => (24, 25),
    }
}

/// 检查是否是后缀运算符
fn is_postfix_op(token: &Token) -> bool {
    matches!(
        token,
        Token::Symbol(Symbol::Dot)
            | Token::Symbol(Symbol::LParen)
            | Token::Symbol(Symbol::LBracket)
            | Token::Symbol(Symbol::Question)
    )
}

// 具体的表达式解析函数

/// 解析字面量表达式
fn parse_literal(cx: &mut ParseContext) -> ParseResult<LiteralExpression> {
    let token = cx.consume()?;
    if let Token::Literal(lit) = &token.value {
        Ok(LiteralExpression {
            lit: Spanned {
                value: lit.clone(),
                span: token.span,
            },
        })
    } else {
        Err(cx.create_error(
            "Expected literal".to_string(),
            token.span,
            Some("literal".to_string()),
            Some(format!("{:?}", token.value)),
        ))
    }
}

/// 解析路径表达式
fn parse_path_expression(cx: &mut ParseContext) -> ParseResult<PathExpression> {
    crate::names::parse_path_in_expression(cx).map(|path| PathExpression::new(path))
}

/// 解析分组表达式
fn parse_grouped_expression(cx: &mut ParseContext) -> ParseResult<Expression> {
    let lparen = cx.expect_symbol(Symbol::LParen)?;
    let expr = parse_expression(cx)?;
    let rparen = cx.expect_symbol(Symbol::RParen)?;

    Ok(Expression::Grouped(GroupedExpression {
        paren_token: Paren::new(lparen, rparen),
        expr: Box::new(expr),
    }))
}

/// 解析数组表达式
fn parse_array_expression(cx: &mut ParseContext) -> ParseResult<ArrayExpression> {
    let lbracket = cx.expect_symbol(Symbol::LBracket)?;

    if cx.next_is(Symbol::RBracket) {
        let rbracket = cx.expect_symbol(Symbol::RBracket)?;
        return Ok(ArrayExpression::Elements {
            bracket_token: Bracket::new(lbracket, rbracket),
            elems: Punctuated::new(),
        });
    }

    // try repeat array expression
    if let Some(arr) = cx.try_parse(|cx| {
        let elem = parse_expression(cx)?;
        let semi = cx.expect_symbol(Symbol::Semi)?;
        let count = parse_u32(cx)?;
        let rbracket = cx.expect_symbol(Symbol::RBracket)?;

        return Ok(ArrayExpression::Repeat {
            bracket_token: Bracket::new(lbracket.clone(), rbracket),
            value: Box::new(elem),
            semi_token: semi,
            count,
        });
    }) {
        return Ok(arr);
    }

    // 普通数组表达式 [elem1, elem2, ...]
    let elems = cx.parse_punctuated(parse_expression, Symbol::Comma)?;
    let rbracket = cx.expect_symbol(Symbol::RBracket)?;

    Ok(ArrayExpression::Elements {
        bracket_token: Bracket::new(lbracket, rbracket),
        elems,
    })
}

/// 解析块表达式
fn parse_block_expression(cx: &mut ParseContext) -> ParseResult<BlockExpression> {
    let lbrace = cx.expect_symbol(Symbol::LBrace)?;
    let mut stmts = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
        stmts.push(Statement::parse(cx)?);
    }

    let rbrace = cx.expect_symbol(Symbol::RBrace)?;

    Ok(BlockExpression {
        brace_token: Brace::new(lbrace, rbrace),
        stmts,
    })
}

/// 解析下划线表达式
fn parse_underscore_expression(cx: &mut ParseContext) -> ParseResult<UnderscoreExpression> {
    let underscore_token = cx.expect_symbol(Symbol::Underscore)?;
    Ok(UnderscoreExpression { underscore_token })
}

/// 解析闭包表达式
fn parse_closure(cx: &mut ParseContext) -> ParseResult<ClosureExpression> {
    // try parse || expr, `||` will be parsed as a binary operator
    if let Some(expr) = cx.try_parse(|s| {
        let oror_token = s.expect_symbol(Symbol::OrOr)?;
        // split ||
        let span = oror_token.span.clone();
        let (span1, span2) = span.split(1);
        let or1_token = TokenSpan::new(Token::Symbol(Symbol::Or), span1);
        let or2_token = TokenSpan::new(Token::Symbol(Symbol::Or), span2);

        let body = Box::new(parse_expression(s)?);
        Ok(ClosureExpression {
            move_token: None,
            or_token: (or1_token, or2_token),
            inputs: Punctuated::new(),
            output: None,
            body,
        })
    }) {
        return Ok(expr);
    }

    let or1_token = cx.consume()?;
    // 尝试解析无参数闭包 || expr
    let result = cx.try_parse(|s| {
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
        cx.parse_punctuated(parse_closure_param, Symbol::Comma)?;
    let or2_token = cx.expect_symbol(Symbol::Or)?;

    // 可选的返回类型
    let output = cx
        .next_is(Symbol::RArrow)
        .then(|| Ok((cx.consume()?, Box::new(Type::parse(cx)?))))
        .transpose()?;

    let body = Box::new(parse_expression(cx)?);

    Ok(ClosureExpression {
        move_token: None,
        or_token: (or1_token, or2_token),
        inputs,
        output,
        body,
    })
}

fn parse_closure_param(cx: &mut ParseContext) -> ParseResult<ClosureParam> {
    let pattern = Pattern::parse(cx)?;

    if cx.next_is(Symbol::Colon) {
        return Ok(ClosureParam {
            pattern,
            colon_token: Some(cx.consume()?),
            ty: Some(Box::new(Type::parse(cx)?)),
        });
    }

    Ok(ClosureParam {
        pattern,
        colon_token: None,
        ty: None,
    })
}

/// 解析if表达式
fn parse_if_expression(cx: &mut ParseContext) -> ParseResult<IfExpression> {
    let if_token = cx.expect_keyword(Keyword::If)?;
    let cond = parse_expression(cx)?;
    let then_branch = parse_block_expression(cx)?;

    let else_branch = if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::Else))) {
        let else_token = cx.consume()?;
        let else_expr = if cx.next_is(|t: &Token| matches!(t, Token::Keyword(Keyword::If))) {
            parse_if_expression(cx).map(|expr| ElseBranch::If(Box::new(expr)))?
        } else {
            parse_block_expression(cx).map(|expr| ElseBranch::Block(expr))?
        };
        Some((else_token, Box::new(else_expr)))
    } else {
        None
    };

    Ok(IfExpression {
        if_token,
        cond: Box::new(cond),
        then_branch,
        else_branch,
    })
}

/// 解析match表达式
fn parse_match_expression(cx: &mut ParseContext) -> ParseResult<MatchExpression> {
    let match_token = cx.expect_keyword(Keyword::Match)?;
    let expr = parse_expression(cx)?;
    let lbrace = cx.expect_symbol(Symbol::LBrace)?;

    let mut arms = Vec::new();
    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
        // TODO: 实现match分支解析
        cx.consume(); // 跳过当前token
    }

    let rbrace = cx.expect_symbol(Symbol::RBrace)?;

    Ok(MatchExpression {
        match_token,
        expr: Box::new(expr),
        brace_token: Brace::new(lbrace, rbrace),
        arms,
    })
}

/// 解析loop表达式
fn parse_loop_expression(cx: &mut ParseContext) -> ParseResult<LoopExpression> {
    let loop_token = cx.expect_keyword(Keyword::Loop)?;
    let body = parse_block_expression(cx)?;

    Ok(LoopExpression {
        label: None, // TODO: 支持标签
        loop_token,
        body,
    })
}

/// 解析while表达式
fn parse_while_expression(cx: &mut ParseContext) -> ParseResult<WhileLoopExpression> {
    let while_token = cx.expect_keyword(Keyword::While)?;
    let cond = parse_expression(cx)?;
    let body = parse_block_expression(cx)?;

    Ok(WhileLoopExpression {
        label: None, // TODO: 支持标签
        while_token,
        cond: Box::new(cond),
        body,
    })
}

/// 解析for表达式
fn parse_for_expression(cx: &mut ParseContext) -> ParseResult<ForLoopExpression> {
    let for_token = cx.expect_keyword(Keyword::For)?;
    let pat = Pattern::parse(cx)?;
    let in_token = cx.expect_keyword(Keyword::In)?;
    let expr = parse_expression(cx)?;
    let body = parse_block_expression(cx)?;

    Ok(ForLoopExpression {
        label: None, // TODO: 支持标签
        for_token,
        pat,
        in_token,
        expr: Box::new(expr),
        body,
    })
}

/// 解析return表达式
fn parse_return_expression(cx: &mut ParseContext) -> ParseResult<ReturnExpression> {
    let return_token = cx.expect_keyword(Keyword::Return)?;
    let expr = if !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) && !cx.is_eof()
    {
        Some(Box::new(parse_expression(cx)?))
    } else {
        None
    };

    Ok(ReturnExpression { return_token, expr })
}

/// 解析break表达式
fn parse_break_expression(cx: &mut ParseContext) -> ParseResult<BreakExpression> {
    let break_token = cx.expect_keyword(Keyword::Break)?;
    let expr = if !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) && !cx.is_eof()
    {
        Some(Box::new(parse_expression(cx)?))
    } else {
        None
    };

    Ok(BreakExpression {
        break_token,
        label: None, // TODO: 支持标签
        expr,
    })
}

/// 解析continue表达式
fn parse_continue_expression(cx: &mut ParseContext) -> ParseResult<ContinueExpression> {
    let continue_token = cx.expect_keyword(Keyword::Continue)?;

    Ok(ContinueExpression {
        continue_token,
        label: None, // TODO: 支持标签
    })
}

/// 解析范围表达式
fn parse_range(cx: &mut ParseContext, start: Option<Expression>) -> ParseResult<RangeExpression> {
    let token = cx.consume()?;
    let limits = match token.value {
        Token::Symbol(Symbol::DotDot) => RangeLimits::HalfOpen,
        Token::Symbol(Symbol::DotDotEq) => RangeLimits::Closed,
        _ => unreachable!(),
    };

    let end = if !cx.is_eof()
        && !cx.next_is(|t: &Token| {
            matches!(
                t,
                Token::Symbol(Symbol::Comma)
                    | Token::Symbol(Symbol::Semi)
                    | Token::Symbol(Symbol::RParen)
                    | Token::Symbol(Symbol::RBracket)
                    | Token::Symbol(Symbol::RBrace)
            )
        }) {
        Some(Box::new(parse_expression(cx)?))
    } else {
        None
    };

    Ok(RangeExpression {
        start: start.map(Box::new),
        limits: Spanned::new(limits, token.span),
        end,
    })
}

// 只为Expression实现Parse trait
impl Parse for Expression {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_expression(cx)
    }
}

impl Parse for LiteralExpression {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_literal(cx)
    }
}



fn parse_u32(cx: &mut ParseContext) -> ParseResult<Spanned<u32>> {
    let token = cx
        .consume()
        .map_err(|_| cx.create_eof_error("u32 literal"))?;

    if let Token::Literal(Literal::Integer(value)) = &token.value {
        // 检查值是否在u32范围内
        if *value >= 0 && *value <= u32::MAX as i64 {
            Ok(Spanned {
                value: *value as u32,
                span: token.span,
            })
        } else {
            Err(cx.create_error(
                format!("integer literal {} is out of range for u32 type", value),
                token.span,
                Some("u32 literal".to_string()),
                Some(format!("{} (must be between 0 and {})", value, u32::MAX)),
            ))
        }
    } else {
        Err(cx.create_error(
            "expected literal".to_string(),
            token.span,
            Some("u32 literal".to_string()),
            Some(format!("{:?}", token.value)),
        ))
    }
}
