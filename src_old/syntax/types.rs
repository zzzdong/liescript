use super::{BinOp, Expression, Pattern, RangeLimits, Statement, UnOp};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};
use crate::syntax::TypePath;

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
#[derive(Debug, PartialEq)]
pub enum Type {
    /// 原生类型 `any`, `bool`, `byte`, `int`, `float`, `char`, `string`
    Primitive(Primitive),

    /// 括号类型，如 `(i32)`
    ///
    /// 示例：
    /// ```ignore
    /// let x: (i32) = 42;
    /// ```
    Parenthesized(ParenthesizedType),

    /// 类型路径，如 `std::string::String`, `i32`
    ///
    /// 示例：
    /// ```ignore
    /// let s: String = "hello".to_string();
    /// let n: i32 = 42;
    /// ```
    Path(TypePath),

    /// 元组类型，如 `(i32, String)`
    ///
    /// 示例：
    /// ```ignore
    /// let pair: (i32, String) = (42, "hello".to_string());
    /// ```
    Tuple(TupleType),

    /// Never类型，使用 `!`
    ///
    /// 示例：
    /// ```ignore
    /// fn never_returns() -> ! {
    ///     panic!("This function never returns");
    /// }
    /// ```
    Never(NeverType),

    /// 引用类型，如 `&i32`, `&mut String`
    ///
    /// 示例：
    /// ```ignore
    /// let r: &i32 = &42;
    /// let rm: &mut String = &mut "hello".to_string();
    /// ```
    Reference(ReferenceType),

    /// 数组类型，如 `[i32; 5]`
    ///
    /// 示例：
    /// ```ignore
    /// let arr: [i32; 5] = [1, 2, 3, 4, 5];
    /// ```
    Array(ArrayType),

    /// 切片类型，如 `[i32]`
    ///
    /// 示例：
    /// ```ignore
    /// let slice: &[i32] = &[1, 2, 3];
    /// ```
    Slice(SliceType),

    /// 推断类型，使用 `_`
    ///
    /// 示例：
    /// ```ignore
    /// let x: _ = 42;
    /// ```
    Inferred(InferredType),

    /// 函数指针类型，如 `fn(i32) -> i32`
    ///
    /// 示例：
    /// ```ignore
    /// let f: fn(i32) -> i32 = |x| x + 1;
    /// ```
    BareFn(BareFunctionType),
}

impl HasSpan for Type {
    fn span(&self) -> Span {
        self.span()
    }
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Primitive(ty) => ty.span(),
            Type::Parenthesized(ty) => ty.span(),
            Type::Path(ty) => ty.span(),
            Type::Reference(ty) => ty.span(),
            Type::Slice(ty) => ty.span(),
            Type::Array(ty) => ty.span(),
            Type::Tuple(ty) => ty.span(),
            Type::Never(ty) => ty.span(),
            Type::Inferred(ty) => ty.span(),
            Type::BareFn(ty) => ty.span(),
        }
    }
}

/// 原生类型
#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    /// Any
    Any(TokenSpan),
    /// Boolean
    Boolean(TokenSpan),
    /// Byte
    Byte(TokenSpan),
    /// Integer
    Integer(TokenSpan),
    /// Float
    Float(TokenSpan),
    /// Char
    Char(TokenSpan),
    /// String
    String(TokenSpan),
}

impl Primitive {
    pub fn span(&self) -> Span {
        match self {
            Primitive::Any(t) => t.span(),
            Primitive::Boolean(t) => t.span(),
            Primitive::Byte(t) => t.span(),
            Primitive::Integer(t) => t.span(),
            Primitive::Float(t) => t.span(),
            Primitive::Char(t) => t.span(),
            Primitive::String(t) => t.span(),
        }
    }
}

impl HasSpan for Primitive {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 括号类型
#[derive(Debug, PartialEq)]
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

/// 元组类型
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub struct ReferenceType {
    pub and_token: TokenSpan,
    pub is_mut: Option<TokenSpan>, // 显式指定可变性，如 `&mut i32`
    pub ty: Box<Type>,
}

impl ReferenceType {
    pub fn span(&self) -> Span {
        self.and_token.span().merge(self.ty.span())
    }
}

impl HasSpan for ReferenceType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 数组类型
#[derive(Debug, PartialEq)]
pub struct ArrayType {
    pub bracket_token: Bracket,
    pub elem: Box<Type>,
    pub semi_token: TokenSpan,
    pub len: Box<Expression>,
}

impl ArrayType {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for ArrayType {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 切片类型
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub struct BareFunctionType {
    pub fn_token: TokenSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<Type>,
    pub output: Option<(TokenSpan, Box<Type>)>, // (-> token, return type)
}

impl BareFunctionType {
    pub fn span(&self) -> Span {
        let start = self.fn_token.span();
        let end = self
            .output
            .as_ref()
            .map(|(_, ty)| ty.span())
            .unwrap_or(self.inputs.span());

        start.merge(end)
    }
}

impl HasSpan for BareFunctionType {
    fn span(&self) -> Span {
        self.span()
    }
}
