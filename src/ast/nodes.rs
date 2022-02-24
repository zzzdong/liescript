pub mod expr;
pub mod stmt;

pub use expr::*;
pub use stmt::*;

use self::expr::Expr;
use self::stmt::{FuncDefStmt, ImportStmt, LetStmt, StructDefStmt, WhileStmt};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Import(ImportStmt),
    Let(LetStmt),
    StructDef(StructDefStmt),
    FuncDef(FuncDefStmt),
    While(WhileStmt),
    Expression(Expr),
}
