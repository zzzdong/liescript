use crate::ast::Ident;

use super::expr::{BlockExpr, Expr};

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub var: Ident,
    pub decl: Option<Ident>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block: BlockExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefStmt {
    pub name: Ident,
    pub fields: Vec<FieldDef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub name: Ident,
    pub ty: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncDefStmt {
    pub name: String,
    pub params: Vec<FieldDef>,
    pub ret_type: String,
    pub block: BlockExpr,
}
