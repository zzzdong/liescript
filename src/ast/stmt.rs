use std::fmt::Display;

use crate::ast::Ident;

use super::expr::Expr;

#[derive(Debug, Clone)]
pub enum Statement {
    Empty,
    /// A local (let) binding.
    Let(LetStmt),
    /// An item definition.
    Item(Item),
    /// Expression without trailing semicolon.
    Expr(Expr),
    /// Expression with trailing semicolon.
    Semi(Expr),
}

#[derive(Debug, Clone)]
pub enum Item {
    Use(ItemUse),
    Struct(ItemStruct),
    Fn(ItemFn),
}

#[derive(Debug, Clone)]
pub struct ItemUse {
    pub items: Vec<UsePath>,
}

#[derive(Debug, Clone)]
pub struct UsePath {
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
    pub fn flat(self) -> Vec<UsePath> {
        let mut ret = Vec::new();

        if self.children.is_empty() {
            ret.push(UsePath {
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

#[derive(Debug, Clone, PartialEq)]
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
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ItemFn {
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub name: Ident,
    pub inputs: Vec<FnArg>,
    pub output: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum FnArg {
    Receiver(Receiver),
    Typed(PatType),
}

#[derive(Debug, Clone)]
pub struct Receiver {
    pub reference: bool,
}

#[derive(Debug, Clone)]
pub struct PatType {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct ItemStruct {
    pub name: Ident,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub visibility: Visibility,
    pub name: Ident,
    pub ty: Type,
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
pub enum Type {
    Primitive(PrimitiveTy),
    Array(TypeArray),
    Path(TypePath),
    Reference(Box<Type>),
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

/// A fixed size array type: `[T; n]`.
#[derive(Debug, Clone)]
pub struct TypeArray {
    pub elem: Box<Type>,
    pub len: Box<Expr>,
}

/// A path like `std::slice::Iter`
#[derive(Debug, Clone)]
pub struct TypePath {
    pub path: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub var: Ident,
    pub ty: Option<Type>,
    pub expr: Option<Box<Expr>>,
}
