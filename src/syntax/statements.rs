use super::{BinOp, Expression, Path, Pattern, RangeLimits, Type, UnOp};
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
#[derive(Debug)]
pub enum Statement {
    /// 空语句，仅包含分号
    Empty(EmptyStatement),

    /// 声明语句，用于声明变量、常量等
    Declaration(DeclarationStatement),

    /// 表达式语句，用于执行表达式
    Expression(ExpressionStatement),

    /// 项声明语句，用于声明模块级别的项
    Item(Item),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Empty(stmt) => stmt.span(),
            Statement::Declaration(stmt) => stmt.span(),
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
#[derive(Debug)]
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
#[derive(Debug)]
pub enum DeclarationStatement {
    /// let语句，用于声明变量
    Let(LetStatement),
}

impl DeclarationStatement {
    pub fn span(&self) -> Span {
        match self {
            DeclarationStatement::Let(stmt) => stmt.span(),
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
#[derive(Debug)]
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

/// 表达式语句，用于执行表达式
///
/// 根据Rust规范文档，表达式语句的定义为：
/// ExpressionStatement → ExpressionWithoutBlock ;
///                     | ExpressionWithBlock
///
/// 参考：https://doc.rust-lang.org/reference/statements.html#expression-statements
#[derive(Debug)]
pub enum ExpressionStatement {
    /// 表达式后跟分号
    Semi(ExpressionSemiStatement),

    /// 不带分号的表达式（通常是块表达式的最后一个表达式）
    NoSemi(Expression),
}

impl ExpressionStatement {
    pub fn span(&self) -> Span {
        match self {
            ExpressionStatement::Semi(stmt) => stmt.span(),
            ExpressionStatement::NoSemi(expr) => expr.span(),
        }
    }
}

impl HasSpan for ExpressionStatement {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 表达式后跟分号的语句
#[derive(Debug)]
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


