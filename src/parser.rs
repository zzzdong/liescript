use std::borrow::Cow;
use std::iter::Peekable;

use crate::ast::nodes::expr::{BinOpExpr, Expr, FuncCallExpr, IndexExpr, PrefixOpExpr};
use crate::ast::nodes::stmt::LetStmt;
use crate::ast::nodes::{Ast, AstNode};
use crate::ast::op::{AccessOp, AssignOp, BinOp, BitOp, LogOp, NumOp, PostfixOp, PrefixOp};
use crate::ast::Punctuation;
use crate::token::{self, Token};
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

    pub fn expect_but_found(expect: Token, found: &Token) -> ParseError {
        ParseError::new(format!("expect {expect:?}, but found {found:?}"))
    }
}

fn is_stmt_end(token: &Token) -> bool {
    token == &Token::Punctuation(Punctuation::Semicolon)
}

pub struct Parser {}

impl Parser {
    pub fn parse_str(input: &str) -> Result<Ast, ParseError> {
        let mut tokenizer = Tokenizer::new("", input).stripped().peekable();

        let mut ast = Ast::new();

        loop {
            let token = tokenizer.peek();

            match token {
                Some(Token::Keyword(crate::ast::Keyword::Let)) => {
                    let let_stmt = Self::parse_let_stmt(&mut tokenizer)?;
                    ast.children.push(AstNode::Let(let_stmt));
                }
                Some(t) => {
                    return Err(ParseError::new("unknown token {t:?}"));
                }
                None => {
                    break;
                }
            }
        }

        return Ok(ast);
    }

    fn parse_let_stmt(
        tokenizer: &mut Peekable<impl Iterator<Item = Token>>,
    ) -> Result<LetStmt, ParseError> {
        let t = tokenizer.next();
        assert_eq!(t, Some(Token::Keyword(crate::ast::Keyword::Let)));

        let ident = match tokenizer.next() {
            Some(Token::Ident(ident)) => ident,
            t => {
                return Err(ParseError::new(format!("expect ident, but found {t:?}")));
            }
        };

        match tokenizer.next() {
            Some(Token::Punctuation(Punctuation::Eq)) => {}
            t => {
                return Err(ParseError::new(format!("expect `=`, but found {t:?}")));
            }
        }

        let expr = Self::parse_expr_until(tokenizer, 0, is_stmt_end)?;

        match expr {
            Expr::BinOp(BinOpExpr { op, .. }) if op.kind() == crate::ast::op::OpKind::Assign => {
                return Err(ParseError::new(format!(
                    "assign expr is not supported in let stmt"
                )));
            }
            // TODO, need to check the return value when if expr or block expr
            _ => {}
        };

        assert_eq!(
            tokenizer.next(),
            Some(Token::Punctuation(Punctuation::Semicolon))
        );

        Ok(LetStmt { ident, expr })
    }

    // 1. func call
    // 2. literal
    // 3. ident
    // 4. prefix op
    // 5. bin op
    fn parse_expr(
        tokenizer: &mut Peekable<impl Iterator<Item = Token>>,
        prev_priority: u8,
    ) -> Result<Expr, ParseError> {
        fn not(_token: &Token) -> bool {
            false
        }

        Self::parse_expr_until(tokenizer, prev_priority, not)
    }

    fn parse_expr_until(
        tokenizer: &mut Peekable<impl Iterator<Item = Token>>,
        prev_priority: u8,
        predicate: impl Fn(&Token) -> bool + Copy,
    ) -> Result<Expr, ParseError> {
        if let Some(token) = tokenizer.peek() {
            if predicate(token) {
                return Ok(Expr::Completed);
            }
        }

        let token = tokenizer.next();

        let mut lhs = match token {
            Some(Token::Literal(lit)) => Expr::Literal(lit.into()),
            Some(Token::Ident(ident)) => Expr::Ident(ident.into()),
            // group
            Some(Token::Punctuation(Punctuation::LParen)) => {
                fn is_group_dilimite(token: &Token) -> bool {
                    token == &Token::Punctuation(Punctuation::RParen)
                }

                let group = Self::parse_expr_until(tokenizer, 0, is_group_dilimite)?;
                match tokenizer.peek() {
                    Some(Token::Punctuation(Punctuation::RParen)) => {
                        tokenizer.next();
                    }
                    t => {
                        return Err(ParseError::new(format!(
                            "expect `)` for group end, but found {t:?}"
                        )));
                    }
                }

                group
            }
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
                Some(token) if predicate(token) => {
                    return Ok(lhs);
                }
                // Index Expr
                Some(Token::Punctuation(Punctuation::LSquare)) => {
                    tokenizer.next();

                    fn is_rsquare(token: &Token) -> bool {
                        token == &Token::Punctuation(Punctuation::RSquare)
                    }

                    let rhs = Self::parse_expr_until(tokenizer, 0, is_rsquare)?;

                    match tokenizer.next() {
                        Some(Token::Punctuation(Punctuation::RSquare)) => {
                            tokenizer.next();
                        }
                        t => {
                            return Err(ParseError::new(format!(
                                "expect `)` for group end, but found {t:?}"
                            )));
                        }
                    }

                    lhs = Expr::Index(IndexExpr {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });

                    continue;
                }
                // FuncCall Expr
                Some(Token::Punctuation(Punctuation::LParen)) => {
                    let args = Self::parse_func_call_args(tokenizer)?;

                    lhs = Expr::FuncCall(FuncCallExpr {
                        name: Box::new(lhs),
                        params: args,
                    });
                    continue;
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

                        let rhs = Self::parse_expr_until(tokenizer, priority, predicate)?;
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
        fn is_args_delimit(token: &Token) -> bool {
            matches!(
                token,
                Token::Punctuation(Punctuation::Comma) | Token::Punctuation(Punctuation::RParen)
            )
        }

        let mut args = Vec::new();

        tokenizer.next(); // eat "("

        if let Some(Token::Punctuation(Punctuation::RParen)) = tokenizer.peek() {
            tokenizer.next();
            return Ok(args);
        }

        loop {
            let arg = Self::parse_expr_until(tokenizer, 0, is_args_delimit)?;

            args.push(arg);

            let token = tokenizer.peek();

            match token {
                Some(Token::Punctuation(Punctuation::Comma)) => {
                    tokenizer.next();
                    continue;
                }
                Some(Token::Punctuation(Punctuation::RParen)) => {
                    tokenizer.next();
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
        BinOp::Access(AccessOp::Field) => 110,
        BinOp::Access(AccessOp::Path) => 120,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let inputs = [
            "a+b-c*d",
            "a*b-c+d",
            "a*b/c%d",
            "a-b*c+d",
            "-a*b-c+d",
            "a+b[c[d[e]]]*f",
            "a+b()-c",
            "a+b(c,d,e,f[g])-h*i",
            "a+b(c(d,e),f[g])-h*i",
            "(a+b)*c-d*e",
            "a*(b+c)-d*e",
            "(((a)))",
            "a[(b+c)*d]",
        ];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input).stripped();

            let mut tokenizer = tokenizer.peekable();

            println!("{}=>", input);

            let ret = Parser::parse_expr(&mut tokenizer, 0).unwrap();

            println!("{}", ret);

            let mut graph = petgraph::Graph::new();
            Expr::expr_graph(&ret, &mut graph);
            println!("{}", petgraph::dot::Dot::new(&graph));
        }
    }

    #[test]
    fn test_parse_math_expr() {
        let inputs = [
            ("1+2-3+4-5", -1),
            ("1*4/2*3", 6),
            ("1+2*3-4", 3),
            ("1+3*4*5/2-6", 25),
            ("1*(2+3)-4*5", -15),
        ];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input.0).stripped();

            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_expr(&mut tokenizer, 0).unwrap();

            print!("{ret}:\n");

            assert_eq!(Expr::try_do_math(&ret), Ok(input.1));
        }
    }

    #[test]
    fn test_parse_let_stmt() {
        let inputs = ["let a = a * b;", "let a.b = a*b;"];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input).stripped();

            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_let_stmt(&mut tokenizer);

            print!("=> {ret:?}:\n");
        }
    }
}
