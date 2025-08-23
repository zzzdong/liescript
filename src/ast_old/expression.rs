use std::fmt;

use crate::{
    ast::{
        op::{BinOp, PostfixOp, PrefixOp}, statement::Block, Identifier, Literal
    },
    diagnostic::{Span, Spanned},
};







const LEVEL_INDENT: usize = 2;


pub type ExpressionNode = Spanned<Expression>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    PrefixOp(PrefixOpExpression),
    PostfixOp(PostfixOpExpression),
    BinOp(BinOpExpression),
    Index(IndexExpression),
    Array(ArrayExpression),
    FuncCall(FuncCallExpression),
    Block(BlockExpression),
    If(IfExpression),
}

impl Expression {
    fn traval_expr(expr: &Expression, mut level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match expr {
            Expression::BinOp(BinOpExpression { op, lhs, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;

                Self::traval_expr(rhs, level, f)?;
            }
            Expression::Index(IndexExpression { name: lhs, rhs }) => {
                writeln!(f, "{:indent$}IndexOp", "", indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;

                Self::traval_expr(rhs, level, f)?;
            }
            Expression::PrefixOp(PrefixOpExpression { op, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expression::PostfixOp(PostfixOpExpression { op, lhs: rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expression::FuncCall(FuncCallExpression { name, args: params }) => {
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
            Expression::Literal(lit) => {
                writeln!(f, "{:indent$}{:?}", "", lit, indent = level * LEVEL_INDENT)?;
            }
            Expression::Identifier(ident) => {
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

    pub fn expr_graph(
        expr: &Expression,
        graph: &mut petgraph::Graph<String, &str>,
    ) -> petgraph::graph::NodeIndex {
        match expr {
            Expression::BinOp(BinOpExpression { op, lhs, rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let lhs = Self::expr_graph(lhs, graph);

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, lhs, "lhs");
                graph.add_edge(node, rhs, "rhs");

                node
            }
            Expression::Index(IndexExpression { name: lhs, rhs }) => {
                let node = graph.add_node("IndexOpExpr".into());

                let lhs = Self::expr_graph(lhs, graph);

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, lhs, "name");
                graph.add_edge(node, rhs, "index");

                node
            }
            Expression::PrefixOp(PrefixOpExpression { op, rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, rhs, "value");

                node
            }
            Expression::PostfixOp(PostfixOpExpression { op, lhs: rhs }) => {
                let node = graph.add_node(format!("{op}"));

                let rhs = Self::expr_graph(rhs, graph);

                graph.add_edge(node, rhs, "value");

                node
            }
            Expression::FuncCall(FuncCallExpression { name, args }) => {
                let node = graph.add_node("FuncCallExpr".into());

                let lhs = Self::expr_graph(name, graph);

                graph.add_edge(node, lhs, "name");

                for arg in args {
                    let p = Self::expr_graph(arg, graph);
                    graph.add_edge(node, p, "arg");
                }
                node
            }
            Expression::Array(ArrayExpression { elems: items }) => {
                let node = graph.add_node("ArrayExpr".into());

                for item in items {
                    let p = Self::expr_graph(item, graph);
                    graph.add_edge(node, p, "item");
                }
                node
            }
            Expression::Literal(lit) => graph.add_node(format!("{lit:?}")),
            Expression::Identifier(ident) => graph.add_node(format!("{ident:?}")),
            _ => {
                unimplemented!("{expr:?}")
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Self::traval_expr(self, 0, f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression {
    pub elems: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub name: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallExpression {
    pub name: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrefixOpExpression {
    pub op: PrefixOp,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PostfixOpExpression {
    pub op: PostfixOp,
    pub lhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOpExpression {
    pub op: BinOp,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr(Literal);

impl From<Literal> for LiteralExpr {
    fn from(lit: Literal) -> Self {
        LiteralExpr(lit)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub cond: Box<Expression>,
    pub then_branch: Block,
    pub else_branch: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockExpression {
    pub block: Block,
}
