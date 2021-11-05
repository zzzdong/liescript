use std::fmt;

#[derive(Debug)]
pub enum Ast {
    Let(LetStmt),
}

#[derive(Debug)]
pub struct LetStmt {
    pub ident: String,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinOp(BinOpExpr),
    Prefix(PrefixOpExpr),
    Literal(LiteralExpr),
    Ident(IdentExpr),
}

impl Expr {
    fn traval_expr(expr: &Expr, mut indent: u32, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match expr {
            Expr::BinOp(BinOpExpr { op, lhs, rhs }) => {
                for _i in 0..indent {
                    write!(f, " ")?;
                }

                writeln!(f, "{:?}", op)?;

                indent += 2;

                Self::traval_expr(lhs, indent, f)?;

                Self::traval_expr(rhs, indent, f)?;
            }
            Expr::Prefix(PrefixOpExpr { op, rhs }) => {
                for _i in 0..indent {
                    write!(f, " ")?;
                }

                writeln!(f, "{:?}", op)?;

                indent += 2;

                Self::traval_expr(rhs, indent, f)?;
            }
            Expr::Literal(lit) => {
                for _i in 0..indent {
                    write!(f, " ")?;
                }
                writeln!(f, "{:?}", lit)?;
            }
            Expr::Ident(lit) => {
                for _i in 0..indent {
                    write!(f, " ")?;
                }
                writeln!(f, "{:?}", lit)?;
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
