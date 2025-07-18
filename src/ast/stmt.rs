use std::fmt::Display;

use crate::ast::Identifier;

use super::expression::Expression;

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    /// An empty statement.
    Empty,
    /// A break statement.
    Break,
    /// A continue statement.
    Continue,
    /// A local (let) binding.
    Let(LetStmt),
    /// A while loop.
    While(WhileStmt),
    /// A return statement.
    Return(ReturnStmt),
    /// An item definition.
    Item(Item),
    /// Expr without trailing semicolon.
    Expr(Expression),
    /// Expression with trailing semicolon.
    ExpressionSmt(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Use(ItemUse),
    Struct(ItemStruct),
    Fn(ItemFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemUse {
    pub items: Vec<UsePath>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UsePath {
    pub path: Vec<PathSegment>,
    pub alias: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseTree {
    pub path: Vec<PathSegment>,
    pub alias: Option<Identifier>,
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
    Ident(Identifier),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<TopLevel>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemFn {
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub name: Identifier,
    pub inputs: Vec<FnArg>,
    pub output: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnArg {
    Receiver(Receiver),
    Typed(PatType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Receiver {
    pub reference: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatType {
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ItemStruct {
    pub name: Identifier,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub visibility: Visibility,
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Pub,
    Priv,
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Pub
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTy),
    Array(TypeArray),
    Path(TypePath),
    Reference(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveTy {
    Bool,
    Byte,
    Char,
    Int,
    Float,
    Str,
}

/// A fixed size array type: `[T; n]`.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeArray {
    pub elem: Box<Type>,
    pub len: Box<Expression>,
}

/// A path like `std::slice::Iter`
#[derive(Debug, Clone, PartialEq)]
pub struct TypePath {
    pub path: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub var: Identifier,
    pub ty: Option<Type>,
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub cond: Box<Expression>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt;

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStmt;
