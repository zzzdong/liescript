use std::borrow::Cow;
use std::iter::Peekable;

use crate::ast::ast::{Ast, BinOpExpr, Expr, IndexExpr, PrefixOpExpr, FuncCallExpr};
use crate::ast::op::{BinOp, BitOp, LogOp, NumOp, PostfixOp, PrefixOp};
use crate::ast::Punctuation;
use crate::token::Token;
use crate::tokenizer::Tokenizer;

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
        tokenizer: &mut Peekable<impl Iterator<Item = Token>>,
        prev_priority: u8,
    ) -> Result<Expr, ParseError> {
        let token = tokenizer.next();

        let mut lhs = match token {
            Some(Token::Literal(lit)) => Expr::Literal(lit.into()),
            Some(Token::Ident(ident)) => Expr::Ident(ident.into()),
            Some(Token::Punctuation(op)) => {
                let op = PrefixOp::from_punctuation(op).map_err(|_| {
                    ParseError::new(format!("expect prefix operator, but found {token:?}"))
                })?;

                let priority = prefix_op_priority(op);

                let rhs = Self::parse_expr(tokenizer, priority)?;

                Expr::PrefixOp(PrefixOpExpr {
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
                Some(Token::Punctuation(Punctuation::LSquare)) => {
                    tokenizer.next();
                    let rhs = Self::parse_expr(tokenizer, 0)?;
                    assert_eq!(
                        tokenizer.next(),
                        Some(Token::Punctuation(Punctuation::RSquare))
                    );
                    lhs = Expr::Index(IndexExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                    continue;
                }

                Some(Token::Punctuation(Punctuation::LParen)) => {
                    // TODO: func call ("(")
                    let args = Self::parse_func_call_args(tokenizer)?;
                    println!("args: {args:?}");
                    lhs = Expr::FuncCall(FuncCallExpr{
                        name: Box::new(lhs),
                        params: args,
                    });
                    continue;
                }
                Some(Token::Punctuation(Punctuation::RSquare)) 
                | Some(Token::Punctuation(Punctuation::Comma)) 
                | Some(Token::Punctuation(Punctuation::Semicolon)) => {
                    break;
                }
                Some(Token::Punctuation(op)) => {
                    let op = BinOp::from_punctuation(*op).map_err(|_| {
                        ParseError::new(format!("expect binary operator, but found {token:?}"))
                    })?;

                    let priority = binary_op_priority(op);

                    if prev_priority >= priority {
                        break;
                    } else {
                        let op = op.clone();
                        tokenizer.next();

                        let rhs = Self::parse_expr(tokenizer, priority)?;
                        lhs = Expr::BinOp(BinOpExpr {
                            op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        });
                        continue;
                    }
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
        }

        Ok(lhs)
    }

    fn parse_func_call_args(
        tokenizer: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        tokenizer.next(); // eat "("

        loop {
            let arg = Self::parse_expr(tokenizer, 0)?;

            args.push(arg);

            let token = tokenizer.peek();
            
            match token {
                Some(Token::Punctuation(Punctuation::Comma)) => {
                    tokenizer.next();
                    continue;
                }
                Some(Token::Punctuation(Punctuation::RSquare)) => {
                    tokenizer.next();
                    println!("=>args: {args:?}");
                    break;
                }
                None => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(format!(
                        "unexpect token between call args {token:?}"
                    )));
                }
            };
        }

        Ok(args)
    }


}

/// https://doc.rust-lang.org/reference/expressions.html#expression-precedence

fn prefix_op_priority(op: PrefixOp) -> u8 {
    match op {
        PrefixOp::Neg | PrefixOp::Not => 90,
    }
}

fn postfix_op_priority(op: PostfixOp) -> u8 {
    match op {
        PostfixOp::Question => 100,
    }
}

fn binary_op_priority(op: BinOp) -> u8 {
    match op {
        BinOp::Assign(_) => 10,
        BinOp::Range(_) => 20,
        BinOp::Log(LogOp::Or) => 30,
        BinOp::Log(LogOp::And) => 31,
        BinOp::Comp(_) => 40,
        BinOp::Bit(BitOp::Or) => 50,
        BinOp::Bit(BitOp::And) => 51,
        BinOp::Bit(BitOp::Xor) => 52,
        BinOp::Bit(_) => 53, // <<  >>
        BinOp::Num(NumOp::Add) | BinOp::Num(NumOp::Sub) => 60,
        BinOp::Num(NumOp::Mul) | BinOp::Num(NumOp::Div) | BinOp::Num(NumOp::Mod) => 61,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parser_expr() {
        let inputs = ["a+b-c*d", "a*b-c+d", "a*b/c%d", "a-b*c+d", "-a*b-c+d", "a+b[c[d[e]]]*f", "a+b()-c", "a+b(c,d,e,f)-g*h"];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input).stripped();

            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_expr(&mut tokenizer, 0).unwrap();

            println!("{}=>\n {}", input, ret);
        }
    }

    #[test]
    fn test_parser_math_expr() {
        let inputs = [
            ("1+2-3+4-5", -1),
            ("1*4/2*3", 6),
            ("1+2*3-4", 3),
            ("1+3*4*5/2-6", 25),
        ];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input.0).stripped();

            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_expr(&mut tokenizer, 0).unwrap();

            print!("{ret}:\n");

            assert_eq!(Expr::try_do_math(&ret), Ok(input.1));
        }
    }
}
