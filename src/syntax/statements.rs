use super::{BinOp, Expression, Pattern, RangeLimits, Type, UnOp};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};
use crate::syntax::{GenericParams, Item};

/// Statement 是一种用于执行操作的语法结构
///
/// 在LieScript中，Statement用于多种上下文：
/// - 声明变量和常量
/// - 执行表达式
/// - 控制流程
/// - 项定义
///
/// 根据Rust标准文档，Statement的顶级定义为：
/// Statement → EmptyStatement | ItemDeclaration | ExpressionStatement | MacroInvocationSemi
///
/// 参考：https://doc.rust-lang.org/reference/statements.html
#[derive(Debug, PartialEq)]
pub enum Statement {
    /// 空语句，仅包含分号
    Empty(EmptyStatement),

    /// 项声明语句，用于声明模块级别的项
    Item(Item),

    /// Let语句，用于声明变量等
    Let(LetStatement),

    /// 表达式语句，用于执行表达式
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Empty(stmt) => stmt.span(),
            Statement::Let(stmt) => stmt.span(),
            Statement::Expression(stmt) => stmt.span(),
            Statement::Item(stmt) => stmt.span(),
        }
    }
}

impl HasSpan for Statement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 空语句，仅包含分号
///
/// 根据Rust规范文档，空语句的定义为：
/// EmptyStatement → ;
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#expression-statements
#[derive(Debug, PartialEq)]
pub struct EmptyStatement {
    pub semi_token: TokenSpan,
}

impl EmptyStatement {
    pub fn span(&self) -> Span {
        self.semi_token.span()
    }
}

impl HasSpan for EmptyStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 声明语句，用于声明变量、常量等
///
/// 根据Rust规范文档，声明语句包括：
/// - let语句
/// - const语句
/// - static语句
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#declaration-statements
#[derive(Debug, PartialEq)]
pub enum DeclarationStatement {
    /// let语句，用于声明变量
    Let(LetStatement),
    /// const语句，用于声明常量
    Const(ConstStatement),
    /// static语句，用于声明静态变量
    Static(StaticStatement),
}

impl DeclarationStatement {
    pub fn span(&self) -> Span {
        match self {
            DeclarationStatement::Let(stmt) => stmt.span(),
            DeclarationStatement::Const(stmt) => stmt.span(),
            DeclarationStatement::Static(stmt) => stmt.span(),
        }
    }
}

impl HasSpan for DeclarationStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// let语句，用于声明变量
///
/// 根据Rust规范文档，let语句的定义为：
/// LetStatement → let Pattern TypeAnnotation? Initializer? ;
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#let-statements
#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub let_token: TokenSpan,
    pub pattern: Pattern,
    pub type_annotation: Option<(TokenSpan, Box<Type>)>, // (: token, type)
    pub initializer: Option<(TokenSpan, Box<Expression>)>, // (= token, expression)
    pub semi_token: TokenSpan,
}

impl LetStatement {
    pub fn span(&self) -> Span {
        let end = self.semi_token.span().end;
        Span::new(self.let_token.span().start, end)
    }
}

impl HasSpan for LetStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// const语句，用于声明常量
///
/// 根据Rust规范文档，const语句的定义为：
/// ConstStatement → const Identifier : Type = Expression ;
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#const-statements
#[derive(Debug, PartialEq)]
pub struct ConstStatement {
    pub const_token: TokenSpan,
    pub ident: IdentSpan,
    pub colon_token: TokenSpan,
    pub eq_token: TokenSpan,
    pub expr: Expression,
    pub semi_token: TokenSpan,
}

impl ConstStatement {
    pub fn span(&self) -> Span {
        let end = self.semi_token.span().end;
        Span::new(self.const_token.span().start, end)
    }
}

impl HasSpan for ConstStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// static语句，用于声明静态变量
///
/// 根据Rust规范文档，static语句的定义为：
/// StaticStatement → static Identifier : Type = Expression ;
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#static-statements
#[derive(Debug, PartialEq)]
pub struct StaticStatement {
    pub static_token: TokenSpan,
    pub ident: IdentSpan,
    pub colon_token: TokenSpan,
    pub eq_token: TokenSpan,
    pub expr: Expression,
    pub semi_token: TokenSpan,
}

impl StaticStatement {
    pub fn span(&self) -> Span {
        let end = self.semi_token.span().end;
        Span::new(self.static_token.span().start, end)
    }
}

impl HasSpan for StaticStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 表达式语句，用于执行表达式
///
/// 根据Rust规范文档，表达式语句的定义为：
/// ExpressionStatement → ExpressionWithoutBlock ;
///                     | ExpressionWithBlock
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#expression-statements
#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub semi_token: Option<TokenSpan>,
}

impl ExpressionStatement {
    pub fn span(&self) -> Span {
        let end = self
            .semi_token
            .as_ref()
            .map_or(self.expr.span().end, |semi| semi.span().end);
        Span::new(self.expr.span().start, end)
    }
}

impl HasSpan for ExpressionStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 表达式后跟分号的语句
#[derive(Debug, PartialEq)]
pub struct ExpressionSemiStatement {
    pub expr: Expression,
    pub semi_token: TokenSpan,
}

impl ExpressionSemiStatement {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for ExpressionSemiStatement {
    fn span(&self) -> Span {
        self.span()
    }
}
