use std::fmt::Display;

use crate::ast::Ident;

use super::expr::Expr;

#[derive(Debug, Clone)]
pub enum Statement {
    Empty,
    Item(Item),
    Let(LetStmt),
    Expr,
}

#[derive(Debug, Clone)]
pub enum Item {
    Use(UseStmt),
    Struct(StructItem),
}

#[derive(Debug, Clone)]
pub struct UseStmt {
    pub items: Vec<UseItem>,
}

#[derive(Debug, Clone)]
pub struct UseItem {
    pub path: Vec<PathSegment>,
    pub alias: Option<Ident>,
}

#[derive(Debug, Clone)]
pub struct UseTree {
    pub path: Vec<PathSegment>,
    pub alias: Option<Ident>,
    pub children: Vec<UseTree>,
}

impl UseTree {
    pub fn flat(self) -> Vec<UseItem> {
        let mut ret = Vec::new();

        if self.children.is_empty() {
            ret.push(UseItem {
                path: self.path,
                alias: self.alias,
            });
            return ret;
        }

        let UseTree { path, children, .. } = self;

        for child in children {
            for mut c in child.flat() {
                let mut x = path.clone();
                x.extend(c.path);
                c.path = x;
                ret.push(c);
            }
        }

        ret
    }
}

#[derive(Debug, Clone)]
pub enum PathSegment {
    Ident(Ident),
    PathSuper,
    PathSelf,
    PathCrate,
}

impl Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{ident:?}"),
            Self::PathSuper => write!(f, "super"),
            Self::PathSelf => write!(f, "self"),
            Self::PathCrate => write!(f, "crate"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructItem {
    pub name: Ident,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum Visibility {
    Pub,
    Priv,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Pub
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Primitive(PrimitiveTy),
    TypePath(TypePath),
}

#[derive(Debug, Clone)]
pub enum PrimitiveTy {
    Bool,
    Byte,
    Char,
    Int,
    Float,
    Str,
}

#[derive(Debug, Clone)]
pub struct TypePath {
    pub path: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub var: Ident,
    pub ty: Option<Ty>,
    pub expr: Option<Expr>,
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct LoopStmt {
//     pub block: BlockExpr,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct WhileStmt {
//     pub condition: Expr,
//     pub block: BlockExpr,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct IfStmt {
//     pub condition: Expr,
//     pub block: BlockExpr,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct StructDefStmt {
//     pub name: Ident,
//     pub fields: Vec<VarDecl>,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct VarDecl {
//     pub name: Ident,
//     pub ty: Ident,
// }

// #[derive(Debug, Clone, PartialEq)]
// pub struct FuncDefStmt {
//     pub name: String,
//     pub params: Vec<VarDecl>,
//     pub ret_type: String,
//     pub block: BlockExpr,
// }
