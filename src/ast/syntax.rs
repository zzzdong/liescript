use crate::{
    ast::{
        ident::Identifier,
        keyword::Keyword,
        literal::Literal,
        op::{BinOp, UnOp},
        symbol::Symbol,
        token::{Brace, Bracket, Paren, Punctuated, TokenSpan},
    },
    diagnostic::{Span, Spanned, HasSpan},
};


pub type IdentSpan = Spanned<Identifier>;

pub type LiteralSpan = Spanned<Literal>;

pub type KeywordSpan = Spanned<Keyword>;

pub type SymbolSpan = Spanned<Symbol>;

#[derive(Debug)]
pub enum Statement {
}

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
    /// 异步块表达式 (AsyncBlockExpression)
    Async(AsyncBlockExpression),
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

/// 块表达式 (ExpressionWithBlock)
#[derive(Debug)]
pub enum ExpressionWithBlock {
    /// 块表达式 (BlockExpression)
    Block(BlockExpression),
    /// const块表达式 (ConstBlockExpression)
    Const(ConstBlockExpression),
    /// 不安全块表达式 (UnsafeBlockExpression)
    Unsafe(UnsafeBlockExpression),
    /// 循环表达式 (LoopExpression)
    Loop(LoopExpression),
    /// if表达式 (IfExpression)
    If(IfExpression),
    /// match表达式 (MatchExpression)
    Match(MatchExpression),
}

/// 块表达式
#[derive(Debug)]
pub struct BlockExpression {
    pub brace_token: Brace,
    pub stmts: Vec<Statement>,
}

/// const块表达式
#[derive(Debug)]
pub struct ConstBlockExpression {
    pub const_token: TokenSpan,
    pub block: BlockExpression,
}

/// 不安全块表达式
#[derive(Debug)]
pub struct UnsafeBlockExpression {
    pub unsafe_token: TokenSpan,
    pub block: BlockExpression,
}

/// 循环表达式
#[derive(Debug)]
pub struct LoopExpression {
    pub label: Option<Label>,
    pub loop_token: TokenSpan,
    pub body: BlockExpression,
}

/// if表达式
#[derive(Debug)]
pub struct IfExpression {
    pub if_token: TokenSpan,
    pub cond: Box<Expression>,
    pub then_branch: BlockExpression,
    pub else_branch: Option<ElseBranch>,
}

/// match表达式
#[derive(Debug)]
pub struct MatchExpression {
    pub match_token: TokenSpan,
    pub expr: Box<Expression>,
    pub brace_token: Brace,
    pub arms: Punctuated<Arm>,
}

/// else分支
#[derive(Debug)]
pub enum ElseBranch {
    /// else if分支
    If(Box<IfExpression>),
    /// else块
    Block(BlockExpression),
}

/// 运算符表达式实现
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
        question_token: TokenSpan,
        expr: Box<Expression>,
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
}

/// 字面量表达式
#[derive(Debug)]
pub struct LiteralExpression {
    pub lit: LiteralSpan,
}

/// 路径表达式
#[derive(Debug)]
pub struct PathExpression {
    pub path: Path,
}

/// Await表达式
#[derive(Debug)]
pub struct AwaitExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub await_token: TokenSpan,
}

/// 结构体表达式
#[derive(Debug)]
pub struct StructExpression {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldInitializer>,
    pub dot2_token: Option<TokenSpan>,
}

/// 异步块表达式
#[derive(Debug)]
pub struct AsyncBlockExpression {
    pub async_token: TokenSpan,
    pub block: BlockExpression,
}

/// 字段初始化器
#[derive(Debug)]
pub struct FieldInitializer {
    pub member: IdentSpan,
    pub colon_token: Option<TokenSpan>,
    pub expr: Box<Expression>,
}

/// 标签
#[derive(Debug)]
pub struct Label {
    pub label: IdentSpan,
    pub colon_token: TokenSpan,
}

/// match分支
#[derive(Debug)]
pub struct Arm {
    pub pat: Pattern,
    pub guard: Option<(TokenSpan, Box<Expression>)>,
    pub fat_arrow_token: TokenSpan,
    pub body: Box<Expression>,
}

/// 字段表达式
#[derive(Debug)]
pub struct FieldExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub ident: IdentSpan,
}

/// 元组索引表达式
#[derive(Debug)]
pub struct TupleIndexingExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub index: u32,
    pub index_span: Span,
}

/// 调用表达式
#[derive(Debug)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    pub paren_token: Paren,
    pub args: Punctuated<Expression>,
}

/// 方法调用表达式
#[derive(Debug)]
pub struct MethodCallExpression {
    pub expr: Box<Expression>,
    pub dot_token: TokenSpan,
    pub method: IdentSpan,
    pub paren_token: Paren,
    pub args: Punctuated<Expression>,
}

/// 索引表达式
#[derive(Debug)]
pub struct IndexExpression {
    pub expr: Box<Expression>,
    pub bracket_token: Bracket,
    pub index: Box<Expression>,
}

/// 分组表达式
#[derive(Debug)]
pub struct GroupedExpression {
    pub paren_token: Paren,
    pub expr: Box<Expression>,
}

/// 数组表达式
#[derive(Debug)]
pub struct ArrayExpression {
    pub bracket_token: Bracket,
    pub elems: Punctuated<Expression>,
}

/// 元组表达式
#[derive(Debug)]
pub struct TupleExpression {
    pub paren_token: Paren,
    pub elems: Punctuated<Expression>,
}

/// 范围表达式
#[derive(Debug)]
pub struct RangeExpression {
    pub start: Option<Box<Expression>>,
    pub limits: Spanned<RangeLimits>,
    pub end: Option<Box<Expression>>,
}

/// 闭包表达式
#[derive(Debug)]
pub struct ClosureExpression {
    pub move_token: Option<TokenSpan>,
    pub pipe_token: TokenSpan,
    pub inputs: Punctuated<Pattern>,
    pub output: Option<(TokenSpan, Box<Type>)>,
    pub body: Box<Expression>,
}

/// 块表达式
#[derive(Debug)]
pub struct BlockExpression {
    pub brace_token: Brace,
    pub stmts: Vec<Statement>,
}

/// if表达式
#[derive(Debug)]
pub struct IfExpression {
    pub if_token: TokenSpan,
    pub cond: Box<Expression>,
    pub then_block: BlockExpression,
    pub else_branch: Option<(TokenSpan, Box<Expression>)>,
}

/// match表达式
#[derive(Debug)]
pub struct MatchExpression {
    pub match_token: TokenSpan,
    pub expr: Box<Expression>,
    pub brace_token: Brace,
    pub arms: Punctuated<Arm>,
}

/// 循环表达式
#[derive(Debug)]
pub struct LoopExpression {
    pub loop_token: TokenSpan,
    pub body: BlockExpression,
}

/// break表达式
#[derive(Debug)]
pub struct BreakExpression {
    pub break_token: TokenSpan,
    pub expr: Option<Box<Expression>>,
}

/// continue表达式
#[derive(Debug)]
pub struct ContinueExpression {
    pub continue_token: TokenSpan,
}

/// return表达式
#[derive(Debug)]
pub struct ReturnExpression {
    pub return_token: TokenSpan,
    pub expr: Option<Box<Expression>>,
}

/// 下划线表达式
#[derive(Debug)]
pub struct UnderscoreExpression {
    pub underscore_token: TokenSpan,
}




/// Pattern 是一种用于匹配值结构的语法结构
/// 
/// 在LieScript中，Pattern用于多种上下文：
/// - let语句中的变量绑定
/// - 函数参数
/// - match表达式的分支
/// - if let和while let表达式
/// 
/// 与Rust不同，LieScript中的变量默认为可变(mutable)
///
/// 根据Rust标准文档，Pattern的顶级定义为：
/// Pattern → |? PatternNoTopAlt ( | PatternNoTopAlt )*
/// PatternNoTopAlt → PatternWithoutRange | RangePattern
#[derive(Debug)]
pub enum Pattern {
    /// 字面量模式，如 `42`, `"hello"`, `true`
    /// 
    /// 匹配规则：值必须等于字面量
    /// 
    /// 示例：
    /// ```
    /// match x {
    ///     42 => println!("是42"),
    ///     _ => println!("不是42"),
    /// }
    /// ```
    Literal(LiteralPattern),

    /// 标识符模式，如 `x`, `person`
    /// 
    /// 匹配规则：总是匹配，并将值绑定到标识符
    /// 默认情况下，绑定是可变的
    /// 
    /// 示例：
    /// ```
    /// match value {
    ///     x => println!("绑定到x: {}", x),
    /// }
    /// ```
    Identifier(IdentifierPattern),

    /// 通配符模式，使用下划线 `_`
    /// 
    /// 匹配规则：匹配任何值，但不绑定
    /// 
    /// 示例：
    /// ```
    /// match value {
    ///     _ => println!("匹配任何值"),
    /// }
    /// ```
    Wildcard(WildcardPattern),

    /// 剩余模式，如 `..`
    /// 
    /// 匹配规则：匹配剩余的任何内容
    /// 
    /// 示例：
    /// ```
    /// match tuple {
    ///     (first, ..) => println!("第一个元素: {}", first),
    /// }
    /// ```
    Rest(RestPattern),

    /// 引用模式，如 `&x`
    /// 
    /// 匹配规则：值必须是引用，并且引用的值匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match value {
    ///     &x => println!("引用值: {}", x),
    /// }
    /// ```
    Reference(ReferencePattern),

    /// 结构体模式，如 `Point { x, y }`
    /// 
    /// 匹配规则：值必须是指定类型的结构体，且字段匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match point {
    ///     Point { x, y } => println!("x: {}, y: {}", x, y),
    /// }
    /// ```
    Struct(StructPattern),

    /// 元组结构体模式，如 `Some(x)`
    /// 
    /// 匹配规则：值必须是指定的元组结构体，且元素匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match option {
    ///     Some(value) => println!("有值: {}", value),
    ///     None => println!("无值"),
    /// }
    /// ```
    TupleStruct(TupleStructPattern),

    /// 元组模式，如 `(x, y)`
    /// 
    /// 匹配规则：值必须是元组，且元素匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match pair {
    ///     (x, y) => println!("x: {}, y: {}", x, y),
    /// }
    /// ```
    Tuple(TuplePattern),

    /// 分组模式，如 `(1 | 2)`
    /// 
    /// 匹配规则：值必须匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match x {
    ///     (1 | 2) => println!("是1或2"),
    ///     _ => println!("是其他值"),
    /// }
    /// ```
    Grouped(GroupedPattern),

    /// 切片模式，如 `[head, tail @ ..]`
    /// 
    /// 匹配规则：值必须是切片或数组，且元素匹配内部模式
    /// 
    /// 示例：
    /// ```
    /// match arr {
    ///     [first, second, ..] => println!("至少有两个元素: {}, {}", first, second),
    /// }
    /// ```
    Slice(SlicePattern),

    /// 路径模式，如 `None`, `std::option::Option::None`
    /// 
    /// 匹配规则：值必须等于路径指定的值
    /// 
    /// 示例：
    /// ```
    /// match option {
    ///     None => println!("是None"),
    ///     _ => println!("不是None"),
    /// }
    /// ```
    Path(PathPattern),

    /// 范围模式，如 `1..5`, `'a'..='z'`
    /// 
    /// 匹配规则：值必须在指定范围内
    /// 
    /// 示例：
    /// ```
    /// match x {
    ///     1..5 => println!("1到4之间"),
    ///     'a'..='z' => println!("小写字母"),
    ///     _ => println!("其他值"),
    /// }
    /// ```
    Range(RangePattern),
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Literal(pat) => pat.span(),
            Pattern::Identifier(pat) => pat.span(),
            Pattern::Wildcard(pat) => pat.span(),
            Pattern::Rest(pat) => pat.span(),
            Pattern::Reference(pat) => pat.span(),
            Pattern::Struct(pat) => pat.span(),
            Pattern::TupleStruct(pat) => pat.span(),
            Pattern::Tuple(pat) => pat.span(),
            Pattern::Grouped(pat) => pat.span(),
            Pattern::Slice(pat) => pat.span(),
            Pattern::Path(pat) => pat.span(),
            Pattern::Range(pat) => pat.span(),
        }
    }
}

impl HasSpan for Pattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 字面量模式
#[derive(Debug)]
pub struct LiteralPattern {
    pub lit: LiteralSpan,
}

impl LiteralPattern {
    pub fn span(&self) -> Span {
        self.lit.span()
    }
}

impl HasSpan for LiteralPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 标识符模式
#[derive(Debug)]
pub struct IdentifierPattern {
    pub ident: IdentSpan,
    /// 绑定模式，默认为可变
    pub by_ref: Option<TokenSpan>, // 引用绑定，如 `ref x`
    pub is_mut: Option<TokenSpan>, // 显式指定可变性，如 `mut x`
    pub subpat: Option<(TokenSpan, Box<Pattern>)>, // 子模式，如 `x @ 1..5`
}

impl IdentifierPattern {
    pub fn span(&self) -> Span {
        let start = if let Some(by_ref) = &self.by_ref {
            by_ref.span().start
        } else if let Some(is_mut) = &self.is_mut {
            is_mut.span().start
        } else {
            self.ident.span().start
        };

        let end = if let Some((_, subpat)) = &self.subpat {
            subpat.span().end
        } else {
            self.ident.span().end
        };

        Span::new(start, end)
    }
}

impl HasSpan for IdentifierPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 通配符模式
#[derive(Debug)]
pub struct WildcardPattern {
    pub underscore_token: TokenSpan,
}

impl WildcardPattern {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for WildcardPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 范围模式
#[derive(Debug)]
pub struct RangePattern {
    pub lo: Box<Expression>,
    pub limits: Spanned<RangeLimits>,
    pub hi: Box<Expression>,
}

impl RangePattern {
    pub fn span(&self) -> Span {
        Span::new(self.lo.span().start, self.hi.span().end)
    }
}

impl HasSpan for RangePattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 引用模式
#[derive(Debug)]
pub struct ReferencePattern {
    pub and_token: TokenSpan,
    pub is_mut: Option<TokenSpan>, // 显式指定可变性，如 `&mut x`
    pub pat: Box<Pattern>,
}

impl ReferencePattern {
    pub fn span(&self) -> Span {
        Span::new(self.and_token.span().start, self.pat.span().end)
    }
}

impl HasSpan for ReferencePattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 结构体模式
#[derive(Debug)]
pub struct StructPattern {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldPattern>,
    pub rest: Option<RestPattern>,
}

impl StructPattern {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for StructPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 字段模式
#[derive(Debug)]
pub struct FieldPattern {
    pub member: IdentSpan,
    pub colon_token: Option<TokenSpan>,
    pub pat: Box<Pattern>,
}

impl FieldPattern {
    pub fn span(&self) -> Span {
        let start = self.member.span().start;
        let end = self.pat.span().end;
        Span::new(start, end)
    }
}

impl HasSpan for FieldPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组结构体模式
#[derive(Debug)]
pub struct TupleStructPattern {
    pub path: Path,
    pub paren_token: Paren,
    pub elems: Punctuated<Pattern>,
}

impl TupleStructPattern {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for TupleStructPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组模式
#[derive(Debug)]
pub struct TuplePattern {
    pub paren_token: Paren,
    pub elems: Punctuated<Pattern>,
}

impl TuplePattern {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TuplePattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 切片模式
#[derive(Debug)]
pub struct SlicePattern {
    pub bracket_token: Bracket,
    pub elems: Punctuated<Pattern>,
}

impl SlicePattern {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for SlicePattern {
    fn span(&self) -> Span {
        self.span()
    }
}



/// 分组模式
#[derive(Debug)]
pub struct GroupedPattern {
    pub paren_token: Paren,
    pub pat: Box<Pattern>,
}

impl GroupedPattern {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for GroupedPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 剩余模式
#[derive(Debug)]
pub struct RestPattern {
    pub dot2_token: TokenSpan,
}

impl RestPattern {
    pub fn span(&self) -> Span {
        self.dot2_token.span()
    }
}

impl HasSpan for RestPattern {
    fn span(&self) -> Span {
        self.span()
    }
}



/// 路径模式
#[derive(Debug)]
pub struct PathPattern {
    pub path: Path,
}

impl PathPattern {
    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl HasSpan for PathPattern {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 路径，用于表示命名空间中的项
#[derive(Debug)]
pub struct Path {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<PathSegment>,
}

impl Path {
    pub fn span(&self) -> Span {
        let start = if let Some(leading_colon) = &self.leading_colon {
            leading_colon.span().start
        } else if let Some((first, _)) = self.segments.items.first() {
            first.ident.span().start
        } else {
            return Span::default();
        };

        let end = if let Some((last, _)) = self.segments.items.last() {
            last.ident.span().end
        } else {
            return Span::default();
        };

        Span::new(start, end)
    }
}

impl HasSpan for Path {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 路径段
#[derive(Debug)]
pub struct PathSegment {
    pub ident: IdentSpan,
}

/// 范围限制
#[derive(Debug)]
pub enum RangeLimits {
    /// `..` 半开区间（不包含上界）
    HalfOpen,
    /// `..=` 闭区间（包含上界）
    Closed,
}





/// Type 是一种用于表示值类型的语法结构
/// 
/// 在LieScript中，Type用于多种上下文：
/// - 变量和常量声明
/// - 函数参数和返回值
/// - 结构体和枚举定义
/// - 类型别名
/// 
/// 根据Rust标准文档，Type的顶级定义为：
/// Type → TypeNoBounds | ImplTraitType
/// TypeNoBounds → ParenthesizedType | ImplTraitTypeOneBound | TraitObjectTypeOneBound | TypePath | TupleType | NeverType | RawPointerType | ReferenceType | ArrayType | SliceType | InferredType | QualifiedPathInType | BareFunctionType | MacroInvocation
#[derive(Debug)]
pub enum Type {
    /// 括号类型，如 `(i32)`
    /// 
    /// 示例：
    /// ```
    /// let x: (i32) = 42;
    /// ```
    Parenthesized(ParenthesizedType),

    /// 类型路径，如 `std::string::String`, `i32`
    /// 
    /// 示例：
    /// ```
    /// let s: String = "hello".to_string();
    /// let n: i32 = 42;
    /// ```
    Path(TypePath),

    /// 元组类型，如 `(i32, String)`
    /// 
    /// 示例：
    /// ```
    /// let pair: (i32, String) = (42, "hello".to_string());
    /// ```
    Tuple(TupleType),

    /// Never类型，使用 `!`
    /// 
    /// 示例：
    /// ```
    /// fn never_returns() -> ! {
    ///     panic!("This function never returns");
    /// }
    /// ```
    Never(NeverType),

    /// 引用类型，如 `&i32`, `&mut String`
    /// 
    /// 示例：
    /// ```
    /// let r: &i32 = &42;
    /// let rm: &mut String = &mut "hello".to_string();
    /// ```
    Reference(ReferenceType),

    /// 数组类型，如 `[i32; 5]`
    /// 
    /// 示例：
    /// ```
    /// let arr: [i32; 5] = [1, 2, 3, 4, 5];
    /// ```
    Array(ArrayType),

    /// 切片类型，如 `[i32]`
    /// 
    /// 示例：
    /// ```
    /// let slice: &[i32] = &[1, 2, 3];
    /// ```
    Slice(SliceType),

    /// 推断类型，使用 `_`
    /// 
    /// 示例：
    /// ```
    /// let x: _ = 42;
    /// ```
    Inferred(InferredType),

    /// 函数指针类型，如 `fn(i32) -> i32`
    /// 
    /// 示例：
    /// ```
    /// let f: fn(i32) -> i32 = |x| x + 1;
    /// ```
    BareFn(BareFunctionType),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Parenthesized(ty) => ty.span(),
            Type::Path(ty) => ty.span(),
            Type::Tuple(ty) => ty.span(),
            Type::Never(ty) => ty.span(),
            Type::Reference(ty) => ty.span(),
            Type::Array(ty) => ty.span(),
            Type::Slice(ty) => ty.span(),
            Type::Inferred(ty) => ty.span(),
            Type::BareFn(ty) => ty.span(),
        }
    }
}

impl HasSpan for Type {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 括号类型
#[derive(Debug)]
pub struct ParenthesizedType {
    pub paren_token: Paren,
    pub ty: Box<Type>,
}

impl ParenthesizedType {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for ParenthesizedType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 类型路径
#[derive(Debug)]
pub struct TypePath {
    pub path: Path,
}

impl TypePath {
    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl HasSpan for TypePath {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组类型
#[derive(Debug)]
pub struct TupleType {
    pub paren_token: Paren,
    pub elems: Punctuated<Type>,
}

impl TupleType {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TupleType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// Never类型
#[derive(Debug)]
pub struct NeverType {
    pub bang_token: TokenSpan,
}

impl NeverType {
    pub fn span(&self) -> Span {
        self.bang_token.span()
    }
}

impl HasSpan for NeverType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 引用类型
#[derive(Debug)]
pub struct ReferenceType {
    pub and_token: TokenSpan,
    pub is_mut: Option<TokenSpan>, // 显式指定可变性，如 `&mut i32`
    pub ty: Box<Type>,
}

impl ReferenceType {
    pub fn span(&self) -> Span {
        Span::new(self.and_token.span().start, self.ty.span().end)
    }
}

impl HasSpan for ReferenceType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 数组类型
#[derive(Debug)]
pub struct ArrayType {
    pub bracket_token: Bracket,
    pub elem: Box<Type>,
    pub semi_token: TokenSpan,
    pub len: Box<Expression>,
}

impl ArrayType {
    pub fn span(&self) -> Span {
        Span::new(self.bracket_token.span().start, self.bracket_token.span().end)
    }
}

impl HasSpan for ArrayType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 切片类型
#[derive(Debug)]
pub struct SliceType {
    pub bracket_token: Bracket,
    pub elem: Box<Type>,
}

impl SliceType {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for SliceType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 推断类型
#[derive(Debug)]
pub struct InferredType {
    pub underscore_token: TokenSpan,
}

impl InferredType {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for InferredType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 函数指针类型
#[derive(Debug)]
pub struct BareFunctionType {
    pub fn_token: TokenSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<Type>,
    pub output: Option<(TokenSpan, Box<Type>)>, // (-> token, return type)
}

impl BareFunctionType {
    pub fn span(&self) -> Span {
        let start = self.fn_token.span().start;
        let end = if let Some((_, output)) = &self.output {
            output.span().end
        } else {
            self.paren_token.span().end
        };
        
        Span::new(start, end)
    }
}

impl HasSpan for BareFunctionType {
    fn span(&self) -> Span {
        self.span()
    }
}