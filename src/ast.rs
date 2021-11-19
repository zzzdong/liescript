use std::fmt;

const LEVEL_INDENT: usize = 2;

#[derive(Debug, Clone)]
pub enum Ast {
    Let(LetStmt),
    Expression(Expr),
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub ident: String,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(BinOpExpr),
    Prefix(PrefixOpExpr),
    FuncCall(FuncCallExpr),
    Block(BlockExpr),
    Literal(LiteralExpr),
    Ident(IdentExpr),
}

impl Expr {
    fn traval_expr(expr: &Expr, mut level: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match expr {
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent=level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(lhs, level, f)?;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::Prefix(PrefixOpExpr { op, rhs }) => {
                writeln!(f, "{:indent$}{:?}", "", op, indent=level * LEVEL_INDENT)?;

                level += 1;

                Self::traval_expr(rhs, level, f)?;
            }
            Expr::FuncCall(FuncCallExpr{name, params}) => {
                writeln!(f, "{:indent$}FunctionCall", "", indent=level * LEVEL_INDENT)?;
                level += 1;
                writeln!(f, "{:indent$}Name", "", indent=level * LEVEL_INDENT)?;
                level += 1;
                Self::traval_expr(name, level, f)?;
                writeln!(f, "{:indent$}Params", "", indent=level * LEVEL_INDENT)?;
                level += 1;
                for param in params {
                    Self::traval_expr(param, level, f)?;
                }
            }
            Expr::Literal(lit) => {
                writeln!(f, "{:indent$}{:?}", "", lit, indent=level * LEVEL_INDENT)?;
            }
            Expr::Ident(ident) => {
                writeln!(f, "{:indent$}{:?}", "", ident, indent=level * LEVEL_INDENT)?;
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
    pub block: Vec<Ast>
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
pub struct BinOpExpr {
    pub op: Operator,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Char(char),
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub struct IdentExpr(String);

impl IdentExpr {
    pub fn new(ident: &str) -> Self {
        IdentExpr(ident.to_string())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Neg, // -
    Not, // !
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
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
    // As, // as
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    LtE,   // <=
    Gt,    // >
    GtE,   // >=
    Dot,   // .
}
