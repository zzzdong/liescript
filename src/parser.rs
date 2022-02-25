use std::borrow::Cow;
use std::iter::Peekable;

use crate::ast::nodes::expr::{ArrayExpr, BinOpExpr, Expr, FuncCallExpr, IndexExpr, PrefixOpExpr};
use crate::ast::nodes::stmt::LetStmt;
use crate::ast::nodes::{Ast, AstNode, PostfixOpExpr};
use crate::ast::op::{AccessOp, AssignOp, BinOp, BitOp, LogOp, NumOp, OpKind, PostfixOp, PrefixOp};
use crate::ast::Punctuation;
use crate::token::{Token, TreeType};
use crate::tokenizer::{StrippedTokenizer, Tokenizer};

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

fn is_eq(token: &Token) -> bool {
    token == &Token::Punctuation(Punctuation::Eq)
}

fn is_comma(token: &Token) -> bool {
    token == &Token::Punctuation(Punctuation::Comma)
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

    fn parse_block<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
    ) -> Result<Vec<AstNode>, ParseError> {
        let mut nodes = Vec::new();

        loop {
            let token = tokenizer.peek();

            match token {
                Some(Token::Keyword(crate::ast::Keyword::Let)) => {
                    let let_stmt = Self::parse_let_stmt(tokenizer)?;
                    nodes.push(AstNode::Let(let_stmt));
                }
                Some(t) => {
                    return Err(ParseError::new("unknown token {t:?}"));
                }
                None => {
                    break;
                }
            }
        }

        Ok(nodes)
    }


    fn parse_let_stmt<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
    ) -> Result<LetStmt, ParseError> {
        let t = tokenizer.next();
        assert_eq!(t, Some(Token::Keyword(crate::ast::Keyword::Let)));

        let var = Self::parse_expr_until(tokenizer, Precedence::Lowest, is_eq)?;

        let var = match var {
            Expr::Ident(ident) => (ident.ident(), None),
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) if op.kind() == OpKind::Decl => {
                if let Expr::Ident(lident) = lhs.as_ref() {
                    if let Expr::Ident(rident) = rhs.as_ref() {
                        (lident.clone().ident(), Some(rident.clone().ident()))
                    } else {
                        return Err(ParseError::new(format!("expect decl, but found {rhs:?}")));
                    }
                } else {
                    return Err(ParseError::new(format!("expect var, but found {lhs:?}")));
                }
            }
            _ => {
                return Err(ParseError::new(format!(
                    "expect var decl, but found {var:?}"
                )));
            }
        };

        match tokenizer.next() {
            Some(Token::Punctuation(Punctuation::Eq)) => {}
            t => {
                return Err(ParseError::new(format!("expect `=`, but found {t:?}")));
            }
        }

        let expr = Self::parse_expr_until(tokenizer, Precedence::Lowest, is_stmt_end)?;

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

        Ok(LetStmt {
            var: var.0,
            decl: var.1,
            expr,
        })
    }

    // 1. func call
    // 2. literal
    // 3. ident
    // 4. prefix op
    // 5. bin op
    fn parse_expr<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
        prev_precedence: Precedence,
    ) -> Result<Expr, ParseError> {
        fn not(_token: &Token) -> bool {
            false
        }

        Self::parse_expr_until(tokenizer, prev_precedence, not)
    }

    fn parse_expr_until<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
        prev_precedence: Precedence,
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
            Some(Token::TokenTree(ty, group)) => {
                let mut tokenizer = StrippedTokenizer::new(group.into_iter()).peekable();
                match ty {
                    TreeType::Paren => Self::parse_expr(&mut tokenizer, Precedence::Lowest)?,

                    // parse block
                    TreeType::Bracket => {
                        unimplemented!();
                    }

                    // parse array define
                    TreeType::Square => {
                        let items = Self::parse_comma_delimited(&mut tokenizer)?;

                        Expr::Array(ArrayExpr { items })
                    }
                }
            }
            Some(Token::Punctuation(op)) => {
                let op = PrefixOp::from_punctuation(op).map_err(|_| {
                    ParseError::new(format!("expect prefix operator, but found {token:?}"))
                })?;

                let precedence = prefix_op_precedence(op);

                let rhs = Self::parse_expr(tokenizer, precedence)?;

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

                Some(Token::TokenTree(ty, group)) => {
                    if let Some(Token::TokenTree(ty, group)) = tokenizer.next() {
                        let mut tokenizer = StrippedTokenizer::new((group).into_iter()).peekable();
                        match ty {
                            // Index Expr
                            TreeType::Square => {
                                let rhs = Self::parse_expr(&mut tokenizer, Precedence::Lowest)?;
                                lhs = Expr::Index(IndexExpr {
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                });
                                continue;
                            }
                            // func call
                            TreeType::Paren => {
                                let args = Self::parse_comma_delimited(&mut tokenizer)?;

                                lhs = Expr::FuncCall(FuncCallExpr {
                                    name: Box::new(lhs),
                                    args,
                                });
                                continue;
                            }
                            TreeType::Bracket => {
                                unimplemented!("GroupType::Bracket")
                            }
                        }
                    } else {
                        break;
                    }
                }
                Some(Token::Punctuation(op)) if op == &Punctuation::Question => {
                    tokenizer.next();
                    lhs = Expr::PostfixOp(PostfixOpExpr {
                        op: PostfixOp::Try,
                        lhs: Box::new(lhs),
                    });
                    continue;
                }
                Some(Token::Punctuation(op)) => {
                    let op = BinOp::from_punctuation(*op).map_err(|_| {
                        ParseError::new(format!("expect binary operator, but found {token:?}"))
                    })?;
                    match Self::parse_binop(tokenizer, op, prev_precedence, lhs.clone(), predicate)?
                    {
                        Some(expr) => {
                            lhs = expr;
                            continue;
                        }
                        None => break,
                    }
                }
                Some(Token::Keyword(crate::ast::Keyword::As)) => {
                    let op = BinOp::CastOp;
                    match Self::parse_binop(tokenizer, op, prev_precedence, lhs.clone(), predicate)?
                    {
                        Some(expr) => {
                            lhs = expr;
                            continue;
                        }
                        None => break,
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

    fn parse_binop<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
        op: BinOp,
        prev_precedence: Precedence,
        lhs: Expr,
        predicate: impl Fn(&Token) -> bool + Copy,
    ) -> Result<Option<Expr>, ParseError> {
        let precedence = binary_op_precedence(op);

        if prev_precedence >= precedence {
            Ok(None)
        } else {
            let op = op.clone();
            tokenizer.next();

            let rhs = Self::parse_expr_until(tokenizer, precedence, predicate)?;
            let lhs = Expr::BinOp(BinOpExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

            Ok(Some(lhs))
        }
    }

    fn parse_comma_delimited<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
    ) -> Result<Vec<Expr>, ParseError> {
        let mut items = Vec::new();

        loop {
            let item = Self::parse_expr_until(tokenizer, Precedence::Lowest, is_comma)?;

            if item == Expr::Eof {
                break;
            }

            items.push(item);

            let token = tokenizer.peek();

            match token {
                Some(Token::Punctuation(Punctuation::Comma)) => {
                    tokenizer.next();
                    continue;
                }
                None => {
                    break;
                }
                _ => {
                    return Err(ParseError::new(format!(
                        "unexpect token between comma delimited {token:?}"
                    )));
                }
            };
        }

        Ok(items)
    }
}

/// https://doc.rust-lang.org/reference/expressions.html#expression-precedence

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    Lowest = 0,
    Assign,
    Range,
    LogicOr,
    LogicAnd,
    Equal,
    Compare,
    BitOr,
    BitXor,
    BitAnd,
    BitShift,
    Term,
    Factor,
    As,
    Prefix,
    Postfix,
    Call,
    Path,
}

fn prefix_op_precedence(op: PrefixOp) -> Precedence {
    match op {
        PrefixOp::Neg | PrefixOp::Not => Precedence::Prefix,
    }
}

fn postfix_op_precedence(op: PostfixOp) -> Precedence {
    match op {
        PostfixOp::Try => Precedence::Postfix,
    }
}

fn binary_op_precedence(op: BinOp) -> Precedence {
    match op {
        BinOp::Assign(_) => Precedence::As,
        BinOp::Range(_) => Precedence::Range,
        BinOp::Log(LogOp::Or) => Precedence::LogicOr,
        BinOp::Log(LogOp::And) => Precedence::LogicAnd,
        BinOp::Comp(_) => Precedence::Compare,
        BinOp::Bit(BitOp::Or) => Precedence::BitOr,
        BinOp::Bit(BitOp::Xor) => Precedence::BitXor,
        BinOp::Bit(BitOp::And) => Precedence::BitAnd,
        BinOp::Bit(_) => Precedence::BitShift, // <<  >>
        BinOp::Num(NumOp::Add) | BinOp::Num(NumOp::Sub) => Precedence::Term,
        BinOp::Num(NumOp::Mul) | BinOp::Num(NumOp::Div) | BinOp::Num(NumOp::Mod) => {
            Precedence::Factor
        }
        BinOp::CastOp | BinOp::DeclOp => Precedence::As,
        BinOp::Access(AccessOp::Field) => Precedence::Call,
        BinOp::Access(AccessOp::Path) => Precedence::Path,
    }
}

trait AstParser {
    type Item;

    fn parse<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
    ) -> Result<Self::Item, ParseError>;
}

impl AstParser for LetStmt {
    type Item = LetStmt;

    fn parse<'i>(
        tokenizer: &mut Peekable<impl Iterator<Item = Token<'i>>>,
    ) -> Result<Self::Item, ParseError> {
        let t = tokenizer.next();
        assert_eq!(t, Some(Token::Keyword(crate::ast::Keyword::Let)));

        let var = Parser::parse_expr_until(tokenizer, Precedence::Lowest, is_eq)?;

        let var = match var {
            Expr::Ident(ident) => (ident.ident(), None),
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) if op.kind() == OpKind::Decl => {
                if let Expr::Ident(lident) = lhs.as_ref() {
                    if let Expr::Ident(rident) = rhs.as_ref() {
                        (lident.clone().ident(), Some(rident.clone().ident()))
                    } else {
                        return Err(ParseError::new(format!("expect decl, but found {rhs:?}")));
                    }
                } else {
                    return Err(ParseError::new(format!("expect var, but found {lhs:?}")));
                }
            }
            _ => {
                return Err(ParseError::new(format!(
                    "expect var decl, but found {var:?}"
                )));
            }
        };

        match tokenizer.next() {
            Some(Token::Punctuation(Punctuation::Eq)) => {}
            t => {
                return Err(ParseError::new(format!("expect `=`, but found {t:?}")));
            }
        }

        let expr = Parser::parse_expr_until(tokenizer, Precedence::Lowest, is_stmt_end)?;

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

        Ok(LetStmt {
            var: var.0,
            decl: var.1,
            expr,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_expr() {
        let inputs = [
            "a?+b-c*d",
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
            "[a, b[c], d(), e(f)]",
            "a: A",
            "a as u8",
        ];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input).stripped();

            let mut tokenizer = tokenizer.peekable();

            println!("{}=>", input);

            let ret = Parser::parse_expr(&mut tokenizer, Precedence::Lowest).unwrap();

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

            let ret = Parser::parse_expr(&mut tokenizer, Precedence::Lowest).unwrap();

            print!("{ret}:\n");

            assert_eq!(Expr::try_do_math(&ret), Ok(input.1));
        }
    }

    #[test]
    fn test_parse_group() {
        let inputs: Vec<&str> = vec!["a[b[c]]", "a[b[c[d[e[f]]]]]", "a(b(c(d(e()))))"];

        for input in inputs {
            let tokenizer = Tokenizer::new("", input).stripped();
            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_expr(&mut tokenizer, Precedence::Lowest).unwrap();

            print!("=> {ret:?}:\n");
        }
    }

    #[test]
    fn test_parse_let_stmt() {
        let inputs = [
            ("let a = a * b;", true),
            ("let a : u8 = a * b;", true),
            ("let a = [a, b[c], d(), e(f)];", true),
            ("let a.b = a*b;", false),
        ];

        for input in &inputs {
            let tokenizer = Tokenizer::new("", input.0).stripped();

            let mut tokenizer = tokenizer.peekable();

            let ret = Parser::parse_let_stmt(&mut tokenizer);

            print!("=> {ret:?}:\n");

            assert_eq!(ret.is_ok(), input.1);
        }
    }
}
