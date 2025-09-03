use super::{BinOp, Path, Pattern, RangeLimits, Type, UnOp, Statement};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};

/// Expression 是一种用于计算值的语法结构
///
/// 在LieScript中，Expression用于多种上下文：
/// - 变量赋值
/// - 函数调用
/// - 控制流条件
/// - 返回值
///
/// 根据Rust标准文档，Expression的顶级定义为：
/// Expression → ExpressionWithoutBlock | ExpressionWithBlock
/// Expression 表示Rust语言中的表达式，严格遵循官方规范
///
/// 参考：https://doc.rust-lang.org/reference/expressions.html
#[derive(Debug)]
pub enum Expression {
    /// 无块表达式 (ExpressionWithoutBlock)
    WithoutBlock(ExpressionWithoutBlock),
    /// 块表达式 (ExpressionWithBlock)
    WithBlock(ExpressionWithBlock),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::WithoutBlock(expr) => expr.span(),
            Expression::WithBlock(expr) => expr.span(),
        }
    }

    pub fn is_field_expr(&self) -> bool {
        match self {
            Expression::WithoutBlock(expr) => match expr {
                ExpressionWithoutBlock::Field(_) => true,
                _ => false,
            },
            Expression::WithBlock(_) => false,
        }
    }
}

impl HasSpan for Expression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 无块表达式 (ExpressionWithoutBlock)
#[derive(Debug)]
pub enum ExpressionWithoutBlock {
    /// 字面量表达式 (LiteralExpression)
    Literal(LiteralExpression),
    /// 路径表达式 (PathExpression)
    Path(PathExpression),
    /// 运算符表达式 (OperatorExpression)
    Operator(OperatorExpression),
    /// 分组表达式 (GroupedExpression)
    Grouped(GroupedExpression),
    /// 数组表达式 (ArrayExpression)
    Array(ArrayExpression),
    /// await表达式 (AwaitExpression)
    Await(AwaitExpression),
    /// 索引表达式 (IndexExpression)
    Index(IndexExpression),
    /// 元组表达式 (TupleExpression)
    Tuple(TupleExpression),
    /// 元组索引表达式 (TupleIndexingExpression)
    TupleIndex(TupleIndexingExpression),
    /// 结构体表达式 (StructExpression)
    Struct(StructExpression),
    /// 调用表达式 (CallExpression)
    Call(CallExpression),
    /// 方法调用表达式 (MethodCallExpression)
    MethodCall(MethodCallExpression),
    /// 字段表达式 (FieldExpression)
    Field(FieldExpression),
    /// 闭包表达式 (ClosureExpression)
    Closure(ClosureExpression),
    /// continue表达式 (ContinueExpression)
    Continue(ContinueExpression),
    /// break表达式 (BreakExpression)
    Break(BreakExpression),
    /// 范围表达式 (RangeExpression)
    Range(RangeExpression),
    /// return表达式 (ReturnExpression)
    Return(ReturnExpression),
    /// 下划线表达式 (UnderscoreExpression)
    Underscore(UnderscoreExpression),
}

impl ExpressionWithoutBlock {
    pub fn span(&self) -> Span {
        match self {
            ExpressionWithoutBlock::Literal(expr) => expr.span(),
            ExpressionWithoutBlock::Path(expr) => expr.span(),
            ExpressionWithoutBlock::Operator(expr) => expr.span(),
            ExpressionWithoutBlock::Grouped(expr) => expr.span(),
            ExpressionWithoutBlock::Array(expr) => expr.span(),
            ExpressionWithoutBlock::Await(expr) => expr.span(),
            ExpressionWithoutBlock::Index(expr) => expr.span(),
            ExpressionWithoutBlock::Tuple(expr) => expr.span(),
            ExpressionWithoutBlock::TupleIndex(expr) => expr.span(),
            ExpressionWithoutBlock::Struct(expr) => expr.span(),
            ExpressionWithoutBlock::Call(expr) => expr.span(),
            ExpressionWithoutBlock::MethodCall(expr) => expr.span(),
            ExpressionWithoutBlock::Field(expr) => expr.span(),
            ExpressionWithoutBlock::Closure(expr) => expr.span(),
            ExpressionWithoutBlock::Continue(expr) => expr.span(),
            ExpressionWithoutBlock::Break(expr) => expr.span(),
            ExpressionWithoutBlock::Range(expr) => expr.span(),
            ExpressionWithoutBlock::Return(expr) => expr.span(),
            ExpressionWithoutBlock::Underscore(expr) => expr.span(),
        }
    }
}

impl HasSpan for ExpressionWithoutBlock {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 块表达式 (ExpressionWithBlock)
#[derive(Debug)]
pub enum ExpressionWithBlock {
    /// 块表达式 (BlockExpression)
    Block(BlockExpression),
    /// 循环表达式 (LoopExpression)
    Loop(LoopExpression),
    /// while循环表达式 (WhileLoopExpression)
    While(WhileLoopExpression),
    /// for循环表达式 (ForLoopExpression)
    For(ForLoopExpression),
    /// if表达式 (IfExpression)
    If(IfExpression),
    /// match表达式 (MatchExpression)
    Match(MatchExpression),
    /// 异步块表达式 (AsyncBlockExpression)
    Async(AsyncBlockExpression),
}

impl ExpressionWithBlock {
    pub fn span(&self) -> Span {
        match self {
            ExpressionWithBlock::Block(expr) => expr.span(),
            ExpressionWithBlock::Loop(expr) => expr.span(),
            ExpressionWithBlock::While(expr) => expr.span(),
            ExpressionWithBlock::For(expr) => expr.span(),
            ExpressionWithBlock::If(expr) => expr.span(),
            ExpressionWithBlock::Match(expr) => expr.span(),
            ExpressionWithBlock::Async(expr) => expr.span(),
        }
    }
}

impl HasSpan for ExpressionWithBlock {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 块表达式
///
/// 根据Rust规范文档，块表达式的定义为：
/// BlockExpression : { InnerAttribute* Statements? }
///
/// 参考：https://doc.rust-lang.org/reference/expressions/block-expr.html
#[derive(Debug)]
pub struct BlockExpression {
    pub brace_token: Brace,
    pub stmts: Vec<Statement>,
}

impl BlockExpression {
    pub fn span(&self) -> Span {
        self.brace_token.span()
    }
}

impl HasSpan for BlockExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 循环表达式
///
/// 根据Rust规范文档，循环表达式的定义为：
/// LoopExpression : [LoopLabel] loop BlockExpression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html#infinite-loops
#[derive(Debug)]
pub struct LoopExpression {
    pub label: Option<Label>,
    pub loop_token: TokenSpan,
    pub body: BlockExpression,
}

impl LoopExpression {
    pub fn span(&self) -> Span {
        let start = if let Some(label) = &self.label {
            label.span().start
        } else {
            self.loop_token.span().start
        };

        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for LoopExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// while循环表达式
///
/// 根据Rust规范文档，while循环表达式的定义为：
/// PredicateLoopExpression : [LoopLabel] while Expression BlockExpression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html#predicate-loops
#[derive(Debug)]
pub struct WhileLoopExpression {
    pub label: Option<Label>,
    pub while_token: TokenSpan,
    pub cond: Box<Expression>,
    pub body: BlockExpression,
}

impl WhileLoopExpression {
    pub fn span(&self) -> Span {
        let start = if let Some(label) = &self.label {
            label.span().start
        } else {
            self.while_token.span().start
        };

        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for WhileLoopExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// for循环表达式
///
/// 根据Rust规范文档，for循环表达式的定义为：
/// IteratorLoopExpression : [LoopLabel] for Pattern in Expression BlockExpression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html#iterator-loops
#[derive(Debug)]
pub struct ForLoopExpression {
    pub label: Option<Label>,
    pub for_token: TokenSpan,
    pub pat: Pattern,
    pub in_token: TokenSpan,
    pub expr: Box<Expression>,
    pub body: BlockExpression,
}

impl ForLoopExpression {
    pub fn span(&self) -> Span {
        let start = if let Some(label) = &self.label {
            label.span().start
        } else {
            self.for_token.span().start
        };

        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for ForLoopExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// if表达式
///
/// 根据Rust规范文档，if表达式的定义为：
/// IfExpression : if Expression BlockExpression (else (BlockExpression | IfExpression))?
///
/// 参考：https://doc.rust-lang.org/reference/expressions/if-expr.html
#[derive(Debug)]
pub struct IfExpression {
    pub if_token: TokenSpan,
    pub cond: Box<Expression>,
    pub then_branch: BlockExpression,
    pub else_branch: Option<(TokenSpan, Box<ElseBranch>)>, // (else_token, else_branch)
}

impl IfExpression {
    pub fn span(&self) -> Span {
        let end = if let Some((_else_token, else_branch)) = &self.else_branch {
            else_branch.span().end
        } else {
            self.then_branch.span().end
        };

        Span::new(self.if_token.span().start, end)
    }
}

impl HasSpan for IfExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// match表达式
///
/// 根据Rust规范文档，match表达式的定义为：
/// MatchExpression : match Expression { InnerAttribute* MatchArms? }
///
/// 参考：https://doc.rust-lang.org/reference/expressions/match-expr.html
#[derive(Debug)]
pub struct MatchExpression {
    pub match_token: TokenSpan,
    pub expr: Box<Expression>,
    pub brace_token: Brace,
    pub arms: Vec<MatchArm>,
}

impl MatchExpression {
    pub fn span(&self) -> Span {
        Span::new(self.match_token.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for MatchExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// else分支
///
/// 根据Rust规范文档，else分支可以是块表达式或另一个if表达式
///
/// 参考：https://doc.rust-lang.org/reference/expressions/if-expr.html
#[derive(Debug)]
pub enum ElseBranch {
    /// else if分支
    If(Box<IfExpression>),
    /// else块
    Block(BlockExpression),
}

impl ElseBranch {
    pub fn span(&self) -> Span {
        match self {
            ElseBranch::If(expr) => expr.span(),
            ElseBranch::Block(expr) => expr.span(),
        }
    }
}

impl HasSpan for ElseBranch {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 运算符表达式实现
///
/// 根据Rust规范文档，运算符表达式包括多种类型：
/// - 借用和解引用：https://doc.rust-lang.org/reference/expressions/operator-expr.html#borrow-operators
/// - 错误传播：https://doc.rust-lang.org/reference/expressions/operator-expr.html#the-question-mark-operator
/// - 否定：https://doc.rust-lang.org/reference/expressions/operator-expr.html#negation-operators
/// - 算术和逻辑：https://doc.rust-lang.org/reference/expressions/operator-expr.html#arithmetic-and-logical-binary-operators
/// - 比较：https://doc.rust-lang.org/reference/expressions/operator-expr.html#comparison-operators
/// - 惰性布尔：https://doc.rust-lang.org/reference/expressions/operator-expr.html#lazy-boolean-operators
/// - 类型转换：https://doc.rust-lang.org/reference/expressions/operator-expr.html#type-cast-expressions
/// - 赋值：https://doc.rust-lang.org/reference/expressions/operator-expr.html#assignment-expressions
#[derive(Debug)]
pub enum OperatorExpression {
    /// 引用表达式 & / &mut
    Borrow {
        and_token: TokenSpan,
        is_mut: Option<TokenSpan>,
        expr: Box<Expression>,
    },
    /// 解引用表达式 *
    Deref {
        star_token: TokenSpan,
        expr: Box<Expression>,
    },
    /// 错误传播表达式 ?
    Try {
        expr: Box<Expression>,
        question_token: TokenSpan,
    },
    /// 否定表达式 ! / -
    Neg {
        op: Spanned<UnOp>,
        expr: Box<Expression>,
    },
    /// 算术/逻辑表达式 + - * / % & | ^ << >>
    Arithmetic {
        left: Box<Expression>,
        op: Spanned<BinOp>,
        right: Box<Expression>,
    },
    /// 比较表达式 == != < > <= >=
    Comparison {
        left: Box<Expression>,
        op: Spanned<BinOp>,
        right: Box<Expression>,
    },
    /// 惰性布尔表达式 && ||
    Logical {
        left: Box<Expression>,
        op: Spanned<BinOp>,
        right: Box<Expression>,
    },
    /// 类型转换表达式 as
    Cast {
        expr: Box<Expression>,
        as_token: TokenSpan,
        ty: Box<Type>,
    },
    /// 赋值表达式 =
    Assign {
        left: Box<Expression>,
        eq_token: TokenSpan,
        right: Box<Expression>,
    },
    /// 复合赋值表达式 += -= *= /= %=
    AssignOp {
        left: Box<Expression>,
        op: Spanned<BinOp>,
        right: Box<Expression>,
    },
}

impl OperatorExpression {
    pub fn span(&self) -> Span {
        match self {
            OperatorExpression::Borrow {
                and_token, expr, ..
            } => Span::new(and_token.span().start, expr.span().end),
            OperatorExpression::Deref { star_token, expr } => {
                Span::new(star_token.span().start, expr.span().end)
            }
            OperatorExpression::Try {
                expr,
                question_token,
            } => Span::new(expr.span().start, question_token.span().end),
            OperatorExpression::Neg { op, expr } => Span::new(op.span().start, expr.span().end),
            OperatorExpression::Arithmetic { left, right, .. } => {
                Span::new(left.span().start, right.span().end)
            }
            OperatorExpression::Comparison { left, right, .. } => {
                Span::new(left.span().start, right.span().end)
            }
            OperatorExpression::Logical { left, right, .. } => {
                Span::new(left.span().start, right.span().end)
            }
            OperatorExpression::Cast { expr, ty, .. } => {
                Span::new(expr.span().start, ty.span().end)
            }
            OperatorExpression::Assign { left, right, .. } => {
                Span::new(left.span().start, right.span().end)
            }
            OperatorExpression::AssignOp { left, right, .. } => {
                Span::new(left.span().start, right.span().end)
            }
        }
    }
}

impl HasSpan for OperatorExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 字面量表达式
///
/// 根据Rust规范文档，字面量表达式的定义为：
/// LiteralExpression : CHAR_LITERAL | STRING_LITERAL | RAW_STRING_LITERAL | BYTE_LITERAL | BYTE_STRING_LITERAL | RAW_BYTE_STRING_LITERAL | INTEGER_LITERAL | FLOAT_LITERAL | BOOLEAN_LITERAL
///
/// 参考：https://doc.rust-lang.org/reference/expressions/literal-expr.html
#[derive(Debug)]
pub struct LiteralExpression {
    pub lit: LiteralSpan,
}

impl LiteralExpression {
    pub fn span(&self) -> Span {
        self.lit.span()
    }
}

impl HasSpan for LiteralExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 路径表达式
///
/// 根据Rust规范文档，路径表达式用于引用项、变量、函数等
///
/// 参考：https://doc.rust-lang.org/reference/expressions/path-expr.html
#[derive(Debug)]
pub struct PathExpression {
    pub path: Path,
}

impl PathExpression {
    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl HasSpan for PathExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// Await表达式
///
/// 根据Rust规范文档，await表达式的定义为：
/// AwaitExpression : Expression . await
///
/// 参考：https://doc.rust-lang.org/reference/expressions/await-expr.html
#[derive(Debug)]
pub struct AwaitExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub await_token: TokenSpan,
}

impl AwaitExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.await_token.span().end)
    }
}

impl HasSpan for AwaitExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 结构体表达式
///
/// 根据Rust规范文档，结构体表达式的定义为：
/// StructExpression : PathInExpression { StructExprFields? } | PathInExpression ( TupleFields? ) | PathInExpression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/struct-expr.html
#[derive(Debug)]
pub struct StructExpression {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldInitializer>,
    pub rest: Option<(TokenSpan, Box<Expression>)>, // (.., expr)
}

impl StructExpression {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for StructExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 异步块表达式
///
/// 根据Rust规范文档，异步块表达式的定义为：
/// AsyncBlockExpression : async [move] BlockExpression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/block-expr.html#async-blocks
#[derive(Debug)]
pub struct AsyncBlockExpression {
    pub async_token: TokenSpan,
    pub move_token: Option<TokenSpan>,
    pub block: BlockExpression,
}

impl AsyncBlockExpression {
    pub fn span(&self) -> Span {
        Span::new(self.async_token.span().start, self.block.span().end)
    }
}

impl HasSpan for AsyncBlockExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 字段初始化器
///
/// 根据Rust规范文档，字段初始化器的定义为：
/// StructExprField : Identifier | (Identifier | INTEGER_LITERAL) : Expression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/struct-expr.html
#[derive(Debug)]
pub struct FieldInitializer {
    pub member: IdentSpan,
    pub colon_token: Option<TokenSpan>,
    pub expr: Box<Expression>,
}

impl FieldInitializer {
    pub fn span(&self) -> Span {
        Span::new(self.member.span().start, self.expr.span().end)
    }
}

impl HasSpan for FieldInitializer {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 标签
///
/// 根据Rust规范文档，标签的定义为：
/// LoopLabel : LIFETIME_OR_LABEL :
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html
#[derive(Debug)]
pub struct Label {
    pub label: IdentSpan,
    pub colon_token: TokenSpan,
}

impl Label {
    pub fn span(&self) -> Span {
        Span::new(self.label.span().start, self.colon_token.span().end)
    }
}

impl HasSpan for Label {
    fn span(&self) -> Span {
        self.span()
    }
}

/// match分支
///
/// 根据Rust规范文档，match分支的定义为：
/// MatchArm : OuterAttribute* Pattern MatchArmGuard?
/// MatchArmGuard : if Expression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/match-expr.html
#[derive(Debug)]
pub struct MatchArm {
    pub pat: Pattern,
    pub guard: Option<MatchArmGuard>,
    pub fat_arrow_token: TokenSpan,
    pub body: Box<Expression>,
    pub comma_token: Option<TokenSpan>,
}

impl MatchArm {
    pub fn span(&self) -> Span {
        let start = self.pat.span().start;
        let end = if let Some(comma) = &self.comma_token {
            comma.span().end
        } else {
            self.body.span().end
        };
        Span::new(start, end)
    }
}

impl HasSpan for MatchArm {
    fn span(&self) -> Span {
        self.span()
    }
}

/// match分支守卫
#[derive(Debug)]
pub struct MatchArmGuard {
    pub if_token: TokenSpan,
    pub expr: Box<Expression>,
}

impl MatchArmGuard {
    pub fn span(&self) -> Span {
        Span::new(self.if_token.span().start, self.expr.span().end)
    }
}

impl HasSpan for MatchArmGuard {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 字段表达式
///
/// 根据Rust规范文档，字段表达式的定义为：
/// FieldExpression : Expression . IDENTIFIER
///
/// 参考：https://doc.rust-lang.org/reference/expressions/field-expr.html
#[derive(Debug)]
pub struct FieldExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub field: IdentSpan,
}

impl FieldExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.field.span().end)
    }
}

impl HasSpan for FieldExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组索引表达式
///
/// 根据Rust规范文档，元组索引表达式的定义为：
/// TupleIndexingExpression : Expression . INTEGER_LITERAL
///
/// 参考：https://doc.rust-lang.org/reference/expressions/tuple-expr.html#tuple-indexing-expressions
#[derive(Debug)]
pub struct TupleIndexingExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub index: Spanned<u32>,
}

impl TupleIndexingExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.index.span().end)
    }
}

impl HasSpan for TupleIndexingExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 调用表达式
///
/// 根据Rust规范文档，调用表达式的定义为：
/// CallExpression : Expression ( CallParams? )
///
/// 参考：https://doc.rust-lang.org/reference/expressions/call-expr.html
#[derive(Debug)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    pub paren_token: Paren,
    pub args: Punctuated<Expression>,
}

impl CallExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for CallExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 方法调用表达式
///
/// 根据Rust规范文档，方法调用表达式的定义为：
/// MethodCallExpression : Expression . PathExprSegment ( CallParams? )
///
/// 参考：https://doc.rust-lang.org/reference/expressions/method-call-expr.html
#[derive(Debug)]
pub struct MethodCallExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub method: IdentSpan,
    pub paren_token: Paren,
    pub args: Punctuated<Expression>,
}

impl MethodCallExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for MethodCallExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 索引表达式
///
/// 根据Rust规范文档，索引表达式的定义为：
/// IndexExpression : Expression [ Expression ]
///
/// 参考：https://doc.rust-lang.org/reference/expressions/array-expr.html#array-and-slice-indexing-expressions
#[derive(Debug)]
pub struct IndexExpression {
    pub expr: Box<Expression>,
    pub bracket_token: Bracket,
    pub index: Box<Expression>,
}

impl IndexExpression {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.bracket_token.span().end)
    }
}

impl HasSpan for IndexExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 分组表达式
///
/// 根据Rust规范文档，分组表达式的定义为：
/// GroupedExpression : ( Expression )
///
/// 参考：https://doc.rust-lang.org/reference/expressions/grouped-expr.html
#[derive(Debug)]
pub struct GroupedExpression {
    pub paren_token: Paren,
    pub expr: Box<Expression>,
}

impl GroupedExpression {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for GroupedExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 数组表达式
///
/// 根据Rust规范文档，数组表达式的定义为：
/// ArrayExpression : [ ArrayElements? ]
///
/// 参考：https://doc.rust-lang.org/reference/expressions/array-expr.html
#[derive(Debug)]
pub enum ArrayExpression {
    /// 元素列表，如 [1, 2, 3]
    Elements {
        bracket_token: Bracket,
        elems: Punctuated<Expression>,
    },
    /// 重复表达式，如 [1; 3]
    Repeat {
        bracket_token: Bracket,
        value: Box<Expression>,
        semi_token: TokenSpan,
        count: Spanned<u32>,
    },
}

impl ArrayExpression {
    pub fn span(&self) -> Span {
        match self {
            ArrayExpression::Elements { bracket_token, .. } => bracket_token.span(),
            ArrayExpression::Repeat { bracket_token, .. } => bracket_token.span(),
        }
    }
}

impl HasSpan for ArrayExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组表达式
///
/// 根据Rust规范文档，元组表达式的定义为：
/// TupleExpression : ( TupleElements? )
///
/// 参考：https://doc.rust-lang.org/reference/expressions/tuple-expr.html
#[derive(Debug)]
pub struct TupleExpression {
    pub paren_token: Paren,
    pub elems: Punctuated<Expression>,
}

impl TupleExpression {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TupleExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 范围表达式
///
/// 根据Rust规范文档，范围表达式的定义为：
/// RangeExpression : RangeExpr | RangeFromExpr | RangeToExpr | RangeFullExpr | RangeInclusiveExpr | RangeToInclusiveExpr
///
/// 参考：https://doc.rust-lang.org/reference/expressions/range-expr.html
#[derive(Debug)]
pub struct RangeExpression {
    pub start: Option<Box<Expression>>,
    pub limits: Spanned<RangeLimits>,
    pub end: Option<Box<Expression>>,
}

impl RangeExpression {
    pub fn span(&self) -> Span {
        let start = if let Some(start) = &self.start {
            start.span().start
        } else {
            self.limits.span().start
        };

        let end = if let Some(end) = &self.end {
            end.span().end
        } else {
            self.limits.span().end
        };

        Span::new(start, end)
    }
}

impl HasSpan for RangeExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 闭包表达式
///
/// 根据Rust规范文档，闭包表达式的定义为：
/// ClosureExpression : move? ( || | | ClosureParameters? | ) (-> TypeNoBounds)? Expression
///
/// 参考：https://doc.rust-lang.org/reference/expressions/closure-expr.html
#[derive(Debug)]
pub struct ClosureExpression {
    pub move_token: Option<TokenSpan>,
    pub or_token: (TokenSpan, TokenSpan), // (|, |)
    pub inputs: Punctuated<Pattern>,
    pub output: Option<(TokenSpan, Box<Type>)>, // (-> token, type)
    pub body: Box<Expression>,
}

impl ClosureExpression {
    pub fn span(&self) -> Span {
        let start = if let Some(move_token) = &self.move_token {
            move_token.span().start
        } else {
            self.or_token.0.span().start
        };

        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for ClosureExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// break表达式
///
/// 根据Rust规范文档，break表达式的定义为：
/// BreakExpression : break LIFETIME_OR_LABEL? Expression?
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html#break-expressions
#[derive(Debug)]
pub struct BreakExpression {
    pub break_token: TokenSpan,
    pub label: Option<IdentSpan>,
    pub expr: Option<Box<Expression>>,
}

impl BreakExpression {
    pub fn span(&self) -> Span {
        let end = if let Some(expr) = &self.expr {
            expr.span().end
        } else if let Some(label) = &self.label {
            label.span().end
        } else {
            self.break_token.span().end
        };

        Span::new(self.break_token.span().start, end)
    }
}

impl HasSpan for BreakExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// continue表达式
///
/// 根据Rust规范文档，continue表达式的定义为：
/// ContinueExpression : continue LIFETIME_OR_LABEL?
///
/// 参考：https://doc.rust-lang.org/reference/expressions/loop-expr.html#continue-expressions
#[derive(Debug)]
pub struct ContinueExpression {
    pub continue_token: TokenSpan,
    pub label: Option<IdentSpan>,
}

impl ContinueExpression {
    pub fn span(&self) -> Span {
        let end = if let Some(label) = &self.label {
            label.span().end
        } else {
            self.continue_token.span().end
        };

        Span::new(self.continue_token.span().start, end)
    }
}

impl HasSpan for ContinueExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// return表达式
///
/// 根据Rust规范文档，return表达式的定义为：
/// ReturnExpression : return Expression?
///
/// 参考：https://doc.rust-lang.org/reference/expressions/return-expr.html
#[derive(Debug)]
pub struct ReturnExpression {
    pub return_token: TokenSpan,
    pub expr: Option<Box<Expression>>,
}

impl ReturnExpression {
    pub fn span(&self) -> Span {
        let end = if let Some(expr) = &self.expr {
            expr.span().end
        } else {
            self.return_token.span().end
        };

        Span::new(self.return_token.span().start, end)
    }
}

impl HasSpan for ReturnExpression {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 下划线表达式
///
/// 根据Rust规范文档，下划线表达式用于占位符
///
/// 参考：https://doc.rust-lang.org/reference/expressions.html
#[derive(Debug)]
pub struct UnderscoreExpression {
    pub underscore_token: TokenSpan,
}

impl UnderscoreExpression {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for UnderscoreExpression {
    fn span(&self) -> Span {
        self.span()
    }
}
