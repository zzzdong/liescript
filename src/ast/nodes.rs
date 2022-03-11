// pub mod expr;
pub mod stmt;

// pub use expr::*;
pub use stmt::*;

// use self::expr::Expr;
// use self::stmt::{FuncDefStmt, ImportStmt, LetStmt, StructDefStmt, WhileStmt};

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
    Statement(Statement),
}
