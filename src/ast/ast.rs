use std::{fmt, path::Prefix};

use super::{
    literal::Literal,
    op::{BinOp, PostfixOp, PrefixOp},
    Ident,
};

const LEVEL_INDENT: usize = 2;

#[derive(Debug, Clone)]
pub struct Ast {
    pub children: Vec<AstNode>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Import(ImportStmt),
    Let(LetStmt),
    StructDef(StructDefStmt),
    FuncDef(FuncDefStmt),
    While(WhileStmt),
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub path: String,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub ident: String,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct StructDefStmt {
    pub name: String,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone)]
pub struct FuncDefStmt {
    pub name: String,
    pub params: Vec<FieldDef>,
    pub ret_type: String,
    pub block: BlockExpr,
}

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
                writeln!(f, "{:indent$}{:?}", "", "IndexOp", indent = level * LEVEL_INDENT)?;

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
        use super::op::NumOp;

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

impl From<Ident> for IdentExpr {
    fn from(i: Ident) -> Self {
        IdentExpr(i)
    }
}
