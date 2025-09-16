use super::{expressions::Expression, names::Path};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};


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
#[derive(Debug, PartialEq)]
pub enum Pattern {
    /// 字面量模式，如 `42`, `"hello"`, `true`
    ///
    /// 匹配规则：值必须等于字面量
    ///
    /// 示例：
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
    /// ```ignore
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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

/// 范围限制
#[derive(Debug, PartialEq)]
pub enum RangeLimits {
    /// `..` 半开区间（不包含上界）
    HalfOpen,
    /// `..=` 闭区间（包含上界）
    Closed,
}
