use std::iter::Peekable;
use std::{borrow::Cow, convert::identity};

use crate::token::{Delimiter, Keyword, Operator, Token};
use crate::tokenizer::{StrippedTokenizer, Tokenizer};
use crate::{
    ast::{Ast, BinOpExpr, Expr, LetStmt, PrefixOpExpr},
    token::TokenKind,
};

#[derive(Debug)]
pub struct ParseError {
    detail: Cow<'static, str>,
}

impl ParseError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> ParseError {
        ParseError {
            detail: detail.into(),
        }
    }
}

// macro_rules! expect_token {
//     ($tokenizer:expr, $expect:expr) => {{
//         match $tokenizer.next() {
//             Some(t) if t == $expect => t,
//             Some(t) => {
//                 return Err(ParseError::new(format!(
//                     "expect {:?}, but found {:?}",
//                     $expect, t
//                 )))
//             }
//             None => {
//                 return Err(ParseError::new(format!(
//                     "expect {:?}, but found EOF",
//                     $expect
//                 )))
//             }
//         }
//     }};
// }

// macro_rules! expect_kind {
//     ($tokenizer:expr, $expect:expr) => {{
//         match $tokenizer.next() {
//             Some(t) if t.kind() == $expect => t,
//             Some(t) => {
//                 return Err(ParseError::new(format!(
//                     "expect {:?}, but found {:?}",
//                     $expect, t
//                 )))
//             }
//             None => {
//                 return Err(ParseError::new(format!(
//                     "expect {:?}, but found EOF",
//                     $expect
//                 )))
//             }
//         }
//     }};
// }

pub struct Parser {}

impl Parser {
    pub fn parse_str(input: &str) -> Result<Ast, ParseError> {
        let mut tokenizer = Tokenizer::new("", input);

        let mut ast = Ast::new();

        // loop {
        //     let token = tokenizer.next_token()?;

        //     if token == Token::Eof {
        //         break;
        //     }

        //     match token {
        //         Token::Keywrod(crate::token::Keyword::Let) => {}
        //     }
        // }

        unimplemented!()
    }

    // fn parse_let_stmt(tokenizer: impl Iterator<Item = Token>) -> Result<LetStmt, ParseError> {
    //     let eq = Token::Delimiter(crate::token::Delimiter::Eq);
    //     let t = expect_token!(tokenizer, eq);
    //     unimplemented!()
    // }

    // 1. func call
    // 2. literal
    // 3. ident
    // 4. prefix op
    // 5. bin op
    fn parse_expr(
        tokenizer: &mut Peekable<StrippedTokenizer>,
        prev_priority: u8,
    ) -> Result<Expr, ParseError> {
        let token = tokenizer.next();

        let mut lhs = match token {
            Some(Token::Literal(lit)) => Expr::Literal(lit),
            Some(Token::Ident(ident)) => Expr::Ident(ident),
            Some(Token::Operator(op)) => {
                let priority = prefix_op_priority(op);
                let rhs = match priority {
                    Some(p) => Self::parse_expr(tokenizer, p)?,
                    None => {
                        return Err(ParseError::new(format!(
                            "unexpect prefix operator: {token:?}"
                        )))
                    }
                };
                Expr::Prefix(PrefixOpExpr {
                    op,
                    rhs: Box::new(rhs),
                })
            }
            Some(_) => {
                return Err(ParseError::new(format!("unexpect token {token:?}")));
            }
            None => {
                return Ok(Expr::Eof);
            }
        };

        loop {
            let token = tokenizer.peek();
            match token {
                Some(Token::Operator(op)) => match infix_op_priority(op.clone()) {
                    Some(p) => {
                        if prev_priority <= p {
                            break;
                        } else {
                            let op = op.clone();
                            tokenizer.next();

                            let rhs = Self::parse_expr(tokenizer, p)?;
                            lhs = Expr::BinOp(BinOpExpr {
                                op,
                                lhs: Box::new(lhs),
                                rhs: Box::new(rhs),
                            });
                            continue;
                        }
                    }
                    None => {
                        return Err(ParseError::new(format!("unexpect infix op {op:?}")));
                    }
                },
                Some(Token::Delimiter(Delimiter::LSquare))
                | Some(Token::Delimiter(Delimiter::LParen)) => {
                    unimplemented!();
                }
                Some(_) => {
                    return Err(ParseError::new(format!(
                        "unexpect token in binop expr {token:?}"
                    )));
                }
                None => {
                    return Ok(lhs);
                }
            }

            break;
        }

        Ok(lhs)
    }

    fn parse_expr2(mut tokenizer: impl Iterator<Item = Token>) -> Result<Expr, ParseError> {
        unimplemented!();

        let mut top = Frame {
            min_bp: 0,
            lhs: None,
            token: None,
        };

        let mut stack = Vec::new();

        loop {
            let token = tokenizer.next();

            let token = token.as_ref();

            let (token, r_bp) = loop {
                match binding_power(token, top.lhs.is_none()) {
                    Some((t, (l_bp, r_bp))) if top.min_bp <= l_bp => break (t, r_bp),
                    _ => {
                        let res = top;
                        top = match stack.pop() {
                            Some(it) => it,
                            None => {
                                eprintln!();
                                return res.lhs.ok_or(ParseError::new("stack empty"));
                            }
                        };

                        let op = res.token.unwrap();
                        match op {
                            Token::Operator(op) => {
                                top.lhs = Some(match top.lhs {
                                    Some(lhs) => Expr::BinOp(BinOpExpr {
                                        op,
                                        lhs: Box::new(lhs),
                                        rhs: Box::new(res.lhs.unwrap()),
                                    }),
                                    None => Expr::Prefix(PrefixOpExpr {
                                        op,
                                        rhs: Box::new(res.lhs.unwrap()),
                                    }),
                                });
                            }
                            _ => {
                                unreachable!()
                            }
                        }
                    }
                };
            };

            if token == &Token::Delimiter(Delimiter::RParen) {
                let res = top;
                top = stack.pop().unwrap();
                top.lhs = res.lhs;
                continue;
            }

            stack.push(top);
            top = Frame {
                min_bp: r_bp,
                lhs: None,
                token: Some(*token),
            };
        }
    }
}

/// https://doc.rust-lang.org/reference/expressions.html#expression-precedence

fn prefix_op_priority(op: Operator) -> Option<u8> {
    match op {
        Operator::Plus | Operator::Minus | Operator::Not => Some(90),
        _ => None,
    }
}

fn infix_op_priority(op: Operator) -> Option<u8> {
    match op {
        Operator::Assign => Some(10),
        Operator::DotDot => Some(20),
        Operator::Or => Some(30),
        Operator::And => Some(31),
        Operator::Eq | Operator::NotEq 
        | Operator::Lt | Operator::Gt 
        |Operator::LtE | Operator::GtE  => Some(40),
        // bit op (| ^ & << >>) 50
        Operator::Plus | Operator::Minus => Some(60),
        Operator::Mul | Operator::Div | Operator::Mod => Some(70),
        _ => None,
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::And | Operator::Minus | Operator::Dot => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::Question => (11, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: Operator) -> Option<(u8, u8)> {
    let res = match op {
        Operator::And | Operator::Minus => (5, 6),
        Operator::Mul | Operator::Div | Operator::Mod => (7, 8),
        Operator::Dot => (14, 13),
        _ => return None,
    };
    Some(res)
}

fn binding_power(op: Option<&Token>, prefix: bool) -> Option<(&Token, (u8, u8))> {
    let op = op?;

    let res = match op {
        Token::Literal(_) => (99, 100),
        Token::Delimiter(Delimiter::LParen) => (99, 0),
        Token::Delimiter(Delimiter::RParen) => (0, 100),
        Token::Operator(Operator::Eq) => (2, 1),
        Token::Operator(Operator::Plus) | Token::Operator(Operator::Minus) if prefix => (99, 9),
        Token::Operator(Operator::Plus) | Token::Operator(Operator::Minus) => (5, 6),
        Token::Operator(Operator::Mul)
        | Token::Operator(Operator::Div)
        | Token::Operator(Operator::Mod) => (7, 8),
        Token::Operator(Operator::Not) => (11, 100),
        Token::Operator(Operator::Dot) => (14, 13),
        _ => return None,
    };

    Some((op, res))
}

struct Frame {
    pub min_bp: u8,
    pub lhs: Option<Expr>,
    pub token: Option<Token>,
}

#[cfg(test)]
mod test {
    use crate::tokenizer::StrippedTokenizer;

    use super::*;

    #[test]
    fn test_parser_expr() {
        let inputs = [
            "a+b-c*d",
            "a*b-c+d",
            "a*b/c%d",
            "a-b*c+d",
            "-a*b-c+d",

        ];

        for input in &inputs {
            let tokenizer = StrippedTokenizer::new("", input);

            let mut tokenizer = tokenizer.peekable();
    
            let ret = Parser::parse_expr(&mut tokenizer, 0).unwrap();
    
            println!("{}=>\n {}", input,  ret);
        }


    }
}
