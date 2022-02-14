use std::fmt;

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
    Literal(LiteralExpr),
    Ident(IdentExpr),
    BinOp(BinOpExpr),
    Prefix(PrefixOpExpr),
    Postfix(PostfixOpExpr),
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
            Expr::Prefix(PrefixOpExpr { op, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::Postfix(PostfixOpExpr { op, lhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent = level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;
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
pub struct FuncCallExpr {
    pub name: Box<Expr>,
    pub params: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct PrefixOpExpr {
    pub op: crate::token::Operator,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct PostfixOpExpr {
    pub op: crate::token::Operator,
    pub lhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr {
    pub op: crate::token::Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Char(char),
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentExpr {
    pub ident: String,
}

impl IdentExpr {
    pub fn new(ident: impl ToString) -> Self {
        IdentExpr {
            ident: ident.to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Neg, // -
    Not, // !
}

#[derive(Debug, Clone, Copy)]
pub enum PostfixOp {
    Question, // ?
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Not, // !
    Question, // ?
    Plus,   // +
    Minus,  // -
    Mul,    // *
    Div,    // /
    Mod,    // %
    Pow,    // ^
    LShift, // <<
    RShift, // >>
    BitOr,  // |
    BitAnd, // &
    And,    // &&
    Or,     // ||
    As,     // as
    Eq,     // ==
    NotEq,  // !=
    Lt,     // <
    LtE,    // <=
    Gt,     // >
    GtE,    // >=
    Dot,    // .
}
