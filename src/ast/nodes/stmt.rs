use crate::ast::Ident;

use super::expr::{BlockExpr, Expr};

#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub path: String,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub ident: Ident,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct StructDefStmt {
    pub name: Ident,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: Ident,
    pub ty: Ident,
}

#[derive(Debug, Clone)]
pub struct FuncDefStmt {
    pub name: String,
    pub params: Vec<FieldDef>,
    pub ret_type: String,
    pub block: BlockExpr,
}
