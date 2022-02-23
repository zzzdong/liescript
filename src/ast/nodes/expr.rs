use crate::ast::ident::Ident;
use crate::ast::literal::Literal;
use crate::ast::op::{BinOp, NumOp, PostfixOp, PrefixOp};
use std::fmt;

use super::AstNode;

const LEVEL_INDENT: usize = 2;

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(IdentExpr),
    Literal(LiteralExpr),
    PrefixOp(PrefixOpExpr),
    PostfixOp(PostfixOpExpr),
    BinOp(BinOpExpr),
    Index(IndexExpr),
    FuncCall(FuncCallExpr),
    Block(BlockExpr),
    Completed,
    Eof,
}

impl Expr {
    fn traval_expr(expr: &Expr, mut level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match expr {
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::Index(IndexExpr { lhs, rhs }) => {
                writeln!(
                    f,
                    "{:indent$}{}",
                    "",
                    "IndexOp",
                    indent = level * LEVEL_INDENT
                )?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::PrefixOp(PrefixOpExpr { op, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::PostfixOp(PostfixOpExpr { op, lhs: rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::FuncCall(FuncCallExpr { name, params }) => {
                writeln!(
                    f,
                    "{:indent$}FunctionCall",
                    "",
                    indent = level * LEVEL_INDENT
                )?;
                level += 1;
                writeln!(f, "{:indent$}Name", "", indent = level * LEVEL_INDENT)?;
                level += 1;
                Self::traval_expr(name, level, f)?;
                writeln!(f, "{:indent$}Params", "", indent = level * LEVEL_INDENT)?;
                level += 1;
                for param in params {
                    Self::traval_expr(param, level, f)?;
                }
            }
            Expr::Literal(lit) => {
                writeln!(f, "{:indent$}{:?}", "", lit, indent = level * LEVEL_INDENT)?;
            }
            Expr::Ident(ident) => {
                writeln!(
                    f,
                    "{:indent$}{:?}",
                    "",
                    ident,
                    indent = level * LEVEL_INDENT
                )?;
            }
            _ => {}
        }

        Ok(())
    }

    pub(crate) fn try_do_math(expr: &Expr) -> Result<i64, &'static str> {
        match expr {
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) => {
                let lhs = Self::try_do_math(lhs)?;

                let rhs = Self::try_do_math(rhs)?;

                match op {
                    &BinOp::Num(NumOp::Add) => Ok(lhs + rhs),
                    &BinOp::Num(NumOp::Sub) => Ok(lhs - rhs),
                    &BinOp::Num(NumOp::Mul) => Ok(lhs * rhs),
                    &BinOp::Num(NumOp::Div) => Ok(lhs / rhs),
                    &BinOp::Num(NumOp::Mod) => Ok(lhs % rhs),
                    _ => Err("unsupport op"),
                }
            }
            Expr::PrefixOp(PrefixOpExpr { op, rhs }) => {
                let rhs = Self::try_do_math(rhs)?;

                match op {
                    &PrefixOp::Neg => Ok(-rhs),
                    _ => Err("unsupport op"),
                }
            }

            Expr::Literal(LiteralExpr(lit)) => match lit {
                &Literal::Integer(i) => Ok(i),
                _ => Err("unsupport literal"),
            },

            _ => Err("unsupport expr"),
        }
    }

    pub fn expr_graph(
        expr: &Expr,
        graph: &mut petgraph::Graph<String, u32>,
    ) -> petgraph::graph::NodeIndex {
        match expr {
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let lhs = Self::expr_graph(lhs, graph);

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, lhs, 200);
                graph.add_edge(node, rhs, 100);

                node
            }
            Expr::Index(IndexExpr { lhs, rhs }) => {
                let node = graph.add_node("IndexOpExpr".into());

                let lhs = Self::expr_graph(lhs, graph);

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, lhs, 200);
                graph.add_edge(node, rhs, 100);

                node
            }
            Expr::PrefixOp(PrefixOpExpr { op, rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, rhs, 100);

                node
            }
            Expr::PostfixOp(PostfixOpExpr { op, lhs: rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, rhs, 100);

                node
            }
            Expr::FuncCall(FuncCallExpr { name, params }) => {
                let node = graph.add_node("FuncCallExpr".into());

                let lhs = Self::expr_graph(name, graph);

                graph.add_edge(node, lhs, 100);

                for param in params {
                    let p = Self::expr_graph(param, graph);
                    graph.add_edge(node, p, 50);
                }
                node
            }
            Expr::Literal(lit) => {
                let node = graph.add_node(format!("{lit:?}"));

                node
            }
            Expr::Ident(ident) => {
                let node = graph.add_node(format!("{ident:?}"));
                node
            }
            _ => {
                unimplemented!()
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Self::traval_expr(self, 0, f)
    }
}

fn write_ident(level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for _i in 0..level * LEVEL_INDENT {
        write!(f, "")?;
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub block: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FuncCallExpr {
    pub name: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct PrefixOpExpr {
    pub op: PrefixOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostfixOpExpr {
    pub op: PostfixOp,
    pub lhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr(Literal);

impl From<Literal> for LiteralExpr {
    fn from(lit: Literal) -> Self {
        LiteralExpr(lit)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr(Ident);

impl IdentExpr {
    pub fn ident(self) -> Ident {
        self.0
    }
}



impl From<Ident> for IdentExpr {
    fn from(i: Ident) -> Self {
        IdentExpr(i)
    }
}
