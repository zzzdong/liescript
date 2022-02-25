use crate::ast::Ident;

use super::expr::{BlockExpr, Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: Expr,
    pub alias: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub var: Ident,
    pub decl: Option<Ident>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStmt {
    pub block: BlockExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: BlockExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ifstmt {
    pub condition: Expr,
    pub block: BlockExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefStmt {
    pub name: Ident,
    pub fields: Vec<VarDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub name: Ident,
    pub ty: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDefStmt {
    pub name: String,
    pub params: Vec<VarDecl>,
    pub ret_type: String,
    pub block: BlockExpr,
}
