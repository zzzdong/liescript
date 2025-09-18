use super::{BinOp, Expression, Pattern, RangeLimits, Type, UnOp};
use crate::diagnostic::{HasSpan, Span, Spanned};
use crate::lexical::{Brace, Bracket, IdentSpan, LiteralSpan, Paren, Punctuated, TokenSpan};
use crate::syntax::{SimplePath, Statement, TypePath, Visibility};

/// 项声明语句，用于声明模块级别的项
///
/// 根据Rust规范文档，项声明包括：
/// - 函数定义
/// - 结构体定义
/// - 枚举定义
/// - 类型别名
/// - 常量定义
/// - 静态变量定义
/// - 模块定义
/// - 导入声明
///
/// 参考：https://doc.rust-lang.org/reference/items.html
#[derive(Debug, PartialEq)]
pub enum Item {
    /// 函数定义
    Function(FunctionItem),

    /// 结构体定义
    Struct(StructItem),

    /// 枚举定义
    Enum(EnumItem),

    /// 类型别名定义
    TypeAlias(TypeAliasItem),

    /// 常量定义
    Const(ConstItem),

    /// 静态变量定义
    Static(StaticItem),

    /// 模块定义
    Module(ModuleItem),

    /// 导入声明
    Use(UseItem),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Function(item) => item.span(),
            Item::Struct(item) => item.span(),
            Item::Enum(item) => item.span(),
            Item::TypeAlias(item) => item.span(),
            Item::Const(item) => item.span(),
            Item::Static(item) => item.span(),
            Item::Module(item) => item.span(),
            Item::Use(item) => item.span(),
        }
    }
}

impl HasSpan for Item {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 函数定义
///
/// 根据Rust规范文档，函数定义的定义为：
/// FunctionItem → fn IDENTIFIER GenericParams? FunctionParameters FunctionReturnType? FunctionBody
///
/// 参考：https://doc.rust-lang.org/reference/items/functions.html
#[derive(Debug, PartialEq)]
pub struct FunctionItem {
    pub fn_token: TokenSpan,
    pub name: IdentSpan,
    pub generics: Option<GenericParams>,
    pub params: FunctionParams,
    pub return_type: Option<(TokenSpan, Box<Type>)>, // (-> token, return type)
    pub body: FunctionBody,
}

impl FunctionItem {
    pub fn span(&self) -> Span {
        Span::new(self.fn_token.span().start, self.body.span().end)
    }
}

impl HasSpan for FunctionItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 函数参数
///
/// 根据Rust规范文档，函数参数的定义为：
/// FunctionParameters → ( FunctionParamList? )
///
/// 参考：https://doc.rust-lang.org/reference/items/functions.html#function-parameters
#[derive(Debug, PartialEq)]
pub struct FunctionParams {
    pub paren_token: Paren,
    pub params: Punctuated<FunctionParam>,
}

impl FunctionParams {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for FunctionParams {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 单个函数参数
///
/// 根据Rust规范文档，函数参数的定义为：
/// FunctionParam → Pattern TypeAnnotation
///
/// 参考：https://doc.rust-lang.org/reference/items/functions.html#function-parameters
#[derive(Debug, PartialEq)]
pub struct FunctionParam {
    pub pattern: Pattern,
    pub type_annotation: (TokenSpan, Box<Type>), // (: token, type)
}

impl FunctionParam {
    pub fn span(&self) -> Span {
        Span::new(self.pattern.span().start, self.type_annotation.1.span().end)
    }
}

impl HasSpan for FunctionParam {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 函数体
///
/// 根据Rust规范文档，函数体的定义为：
/// FunctionBody → BlockExpression
///
/// 参考：https://doc.rust-lang.org/reference/items/functions.html#function-bodies
#[derive(Debug, PartialEq)]
pub struct FunctionBody {
    pub brace_token: Brace,
    pub stmts: Vec<Statement>,
}

impl FunctionBody {
    pub fn span(&self) -> Span {
        self.brace_token.span()
    }
}

impl HasSpan for FunctionBody {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 泛型参数
///
/// 根据Rust规范文档，泛型参数的定义为：
/// GenericParams → < GenericParamList >
///
/// 参考：https://doc.rust-lang.org/reference/items/generics.html
#[derive(Debug, PartialEq)]
pub struct GenericParams {
    pub angle_bracket_token: (TokenSpan, TokenSpan), // (<, >)
    pub params: Punctuated<GenericParam>,
}

impl GenericParams {
    pub fn span(&self) -> Span {
        Span::new(
            self.angle_bracket_token.0.span().start,
            self.angle_bracket_token.1.span().end,
        )
    }
}

impl HasSpan for GenericParams {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 单个泛型参数
///
/// 根据Rust规范文档，泛型参数的定义为：
/// GenericParam → IDENTIFIER ( : TypeParamBounds? )? ( = Type )?
///
/// 参考：https://doc.rust-lang.org/reference/items/generics.html#generic-parameters
#[derive(Debug, PartialEq)]
pub enum GenericParam {
    /// 类型参数
    Type(TypeParam),
}

impl GenericParam {
    pub fn span(&self) -> Span {
        match self {
            GenericParam::Type(param) => param.span(),
        }
    }
}

impl HasSpan for GenericParam {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 类型参数
#[derive(Debug, PartialEq)]
pub struct TypeParam {
    pub name: IdentSpan,
    pub bounds: Option<(TokenSpan, Punctuated<TypeParamBound>)>, // (: token, bounds)
    pub default: Option<(TokenSpan, Box<Type>)>,                 // (= token, default type)
}

impl TypeParam {
    pub fn span(&self) -> Span {
        let start = self.name.span().start;
        let end = if let Some((_, ref default)) = self.default {
            default.span().end
        } else if let Some((_, ref bounds)) = self.bounds {
            // 获取最后一个bound的结束位置
            if let Some(last) = bounds.last.as_ref() {
                last.span().end
            } else if let Some((last, _)) = bounds.items.last() {
                last.span().end
            } else {
                self.name.span().end
            }
        } else {
            self.name.span().end
        };

        Span::new(start, end)
    }
}

impl HasSpan for TypeParam {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 结构体定义
///
/// 根据Rust规范文档，结构体定义的定义为：
/// StructItem → struct IDENTIFIER GenericParams? ( StructFields? ) ;
///
/// 参考：https://doc.rust-lang.org/reference/items/structs.html
#[derive(Debug, PartialEq)]
pub struct StructItem {
    pub struct_token: TokenSpan,
    pub name: IdentSpan,
    pub generics: Option<GenericParams>,
    pub fields: StructFields,
    pub semi_token: Option<TokenSpan>,
}

impl StructItem {
    pub fn span(&self) -> Span {
        let end = if let Some(semi) = &self.semi_token {
            semi.span().end
        } else {
            self.fields.span().end
        };

        Span::new(self.struct_token.span().start, end)
    }
}

impl HasSpan for StructItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 结构体字段
///
/// 根据Rust规范文档，结构体字段的定义为：
/// StructFields → NamedFields | TupleFields
///
/// 参考：https://doc.rust-lang.org/reference/items/structs.html
#[derive(Debug, PartialEq)]
pub enum StructFields {
    /// 命名字段，如 `struct Point { x: i32, y: i32 }`
    Named(NamedFields),

    /// 元组字段，如 `struct Point(i32, i32)`
    Tuple(TupleFields),

    /// 单元结构体，如 `struct Unit`
    Unit,
}

impl StructFields {
    pub fn span(&self) -> Span {
        match self {
            StructFields::Named(fields) => fields.span(),
            StructFields::Tuple(fields) => fields.span(),
            StructFields::Unit => Span::default(), // 单元结构体没有字段，返回默认span
        }
    }
}

impl HasSpan for StructFields {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 命名字段
///
/// 根据Rust规范文档，命名字段的定义为：
/// NamedFields → { NamedFieldList? }
///
/// 参考：https://doc.rust-lang.org/reference/items/structs.html
#[derive(Debug, PartialEq)]
pub struct NamedFields {
    pub brace_token: Brace,
    pub visibility: Option<Visibility>,
    pub fields: Punctuated<NamedField>,
}

impl NamedFields {
    pub fn span(&self) -> Span {
        self.brace_token.span()
    }
}

impl HasSpan for NamedFields {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 单个命名字段
///
/// 根据Rust规范文档，命名字段的定义为：
/// NamedField → IDENTIFIER : Type
///
/// 参考：https://doc.rust-lang.org/reference/items/structs.html
#[derive(Debug, PartialEq)]
pub struct NamedField {
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
    pub ty: Box<Type>,
}

impl NamedField {
    pub fn span(&self) -> Span {
        Span::new(self.name.span().start, self.ty.span().end)
    }
}

impl HasSpan for NamedField {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 元组字段
///
/// 根据Rust规范文档，元组字段的定义为：
/// TupleFields → ( TupleFieldList? )
///
/// 参考：https://doc.rust-lang.org/reference/items/structs.html
#[derive(Debug, PartialEq)]
pub struct TupleFields {
    pub paren_token: Paren,
    pub visibility: Option<Visibility>,
    pub fields: Punctuated<TupleField>,
}

impl TupleFields {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TupleFields {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 单个元组字段
#[derive(Debug, PartialEq)]
pub struct TupleField {
    pub visibility: Option<Visibility>,
    pub ty: Box<Type>,
}

impl TupleField {
    pub fn span(&self) -> Span {
        match &self.visibility {
            Some(visibility) => Span::new(visibility.span().start, self.ty.span().end),
            None => self.ty.span(),
        }
    }
}

impl HasSpan for TupleField {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 枚举定义
///
/// 根据Rust规范文档，枚举定义的定义为：
/// EnumItem → enum IDENTIFIER GenericParams? { EnumVariants? }
///
/// 参考：https://doc.rust-lang.org/reference/items/enumerations.html
#[derive(Debug, PartialEq)]
pub struct EnumItem {
    pub enum_token: TokenSpan,
    pub name: IdentSpan,
    pub generics: Option<GenericParams>,
    pub brace_token: Brace,
    pub variants: Punctuated<EnumVariant>,
}

impl EnumItem {
    pub fn span(&self) -> Span {
        Span::new(self.enum_token.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for EnumItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 枚举变体
///
/// 根据Rust规范文档，枚举变体的定义为：
/// EnumVariant → IDENTIFIER ( EnumVariantFields? )? EnumVariantDiscriminant?
///
/// 参考：https://doc.rust-lang.org/reference/items/enumerations.html
#[derive(Debug, PartialEq)]
pub struct EnumVariant {
    pub name: IdentSpan,
    pub fields: Option<EnumVariantFields>,
    pub discriminant: Option<(TokenSpan, Box<Expression>)>, // (= token, expression)
}

impl EnumVariant {
    pub fn span(&self) -> Span {
        let start = self.name.span().start;
        let end = if let Some((_, ref expr)) = self.discriminant {
            expr.span().end
        } else if let Some(ref fields) = self.fields {
            fields.span().end
        } else {
            self.name.span().end
        };

        Span::new(start, end)
    }
}

impl HasSpan for EnumVariant {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 枚举变体字段
///
/// 根据Rust规范文档，枚举变体字段的定义为：
/// EnumVariantFields → NamedFields | TupleFields
///
/// 参考：https://doc.rust-lang.org/reference/items/enumerations.html
#[derive(Debug, PartialEq)]
pub enum EnumVariantFields {
    /// 命名字段，如 `enum Message { Move { x: i32, y: i32 } }`
    Named(NamedFields),

    /// 元组字段，如 `enum Message { Write(String) }`
    Tuple(TupleFields),
}

impl EnumVariantFields {
    pub fn span(&self) -> Span {
        match self {
            EnumVariantFields::Named(fields) => fields.span(),
            EnumVariantFields::Tuple(fields) => fields.span(),
        }
    }
}

impl HasSpan for EnumVariantFields {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 类型别名定义
///
/// 根据Rust规范文档，类型别名定义的定义为：
/// TypeAliasItem → type IDENTIFIER GenericParams? = Type ;
///
/// 参考：https://doc.rust-lang.org/reference/items/type-aliases.html
#[derive(Debug, PartialEq)]
pub struct TypeAliasItem {
    pub type_token: TokenSpan,
    pub name: IdentSpan,
    pub generics: Option<GenericParams>,
    pub eq_token: TokenSpan,
    pub ty: Box<Type>,
    pub semi_token: TokenSpan,
}

impl TypeAliasItem {
    pub fn span(&self) -> Span {
        Span::new(self.type_token.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for TypeAliasItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 常量定义
///
/// 根据Rust规范文档，常量定义的定义为：
/// ConstItem → const IDENTIFIER : Type = Expression ;
///
/// 参考：https://doc.rust-lang.org/reference/items/constant-items.html
#[derive(Debug, PartialEq)]
pub struct ConstItem {
    pub const_token: TokenSpan,
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
    pub ty: Box<Type>,
    pub eq_token: TokenSpan,
    pub expr: Box<Expression>,
    pub semi_token: TokenSpan,
}

impl ConstItem {
    pub fn span(&self) -> Span {
        Span::new(self.const_token.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for ConstItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 静态变量定义
///
/// 根据Rust规范文档，静态变量定义的定义为：
/// StaticItem → static MUT? IDENTIFIER : Type = Expression ;
///
/// 参考：https://doc.rust-lang.org/reference/items/static-items.html
#[derive(Debug, PartialEq)]
pub struct StaticItem {
    pub static_token: TokenSpan,
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
    pub ty: Box<Type>,
    pub eq_token: TokenSpan,
    pub expr: Box<Expression>,
    pub semi_token: TokenSpan,
}

impl StaticItem {
    pub fn span(&self) -> Span {
        Span::new(self.static_token.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for StaticItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 模块定义
///
/// 根据Rust规范文档，模块定义的定义为：
/// ModuleItem → mod IDENTIFIER ( ; | { InnerItem* } )
///
/// 参考：https://doc.rust-lang.org/reference/items/modules.html
#[derive(Debug, PartialEq)]
pub struct ModuleItem {
    pub mod_token: TokenSpan,
    pub name: IdentSpan,
    pub content: Option<ModuleContent>,
    pub semi_token: Option<TokenSpan>,
}

impl ModuleItem {
    pub fn span(&self) -> Span {
        let end = if let Some(semi) = &self.semi_token {
            semi.span().end
        } else if let Some(content) = &self.content {
            content.span().end
        } else {
            self.name.span().end
        };

        Span::new(self.mod_token.span().start, end)
    }
}

impl HasSpan for ModuleItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 模块内容
#[derive(Debug, PartialEq)]
pub struct ModuleContent {
    pub brace_token: Brace,
    pub items: Vec<Item>,
}

impl ModuleContent {
    pub fn span(&self) -> Span {
        self.brace_token.span()
    }
}

impl HasSpan for ModuleContent {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 导入声明
///
/// 根据Rust规范文档，导入声明的定义为：
/// UseItem → use UseTree ;
///
/// 参考：https://doc.rust-lang.org/reference/items/use-declarations.html
#[derive(Debug, PartialEq)]
pub struct UseItem {
    pub use_token: TokenSpan,
    pub tree: UseTree,
    pub semi_token: TokenSpan,
}

impl UseItem {
    pub fn span(&self) -> Span {
        Span::new(self.use_token.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for UseItem {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 导入树
///
/// 根据Rust规范文档，导入树的定义为：
/// UseTree →
///       ( SimplePath? :: )? *
///     | ( SimplePath? :: )? { ( UseTree ( , UseTree )* ,? )? }
///     | SimplePath ( as ( IDENTIFIER | _ ) )?
///
/// 参考：https://doc.rust-lang.org/reference/items/use-declarations.html
#[derive(Debug, PartialEq)]
pub enum UseTree {
    /// 路径导入，如 `use std::io`
    Path(SimplePath),

    /// 重命名导入，如 `use std::io as IO`
    Rename(UseRenameTree),

    /// 通配符导入，如 `use std::io::*`
    Glob(UseGlobTree),

    /// 分组导入，如 `use std::{io, fs}`
    Group(UseGroupTree),
}

impl UseTree {
    pub fn span(&self) -> Span {
        match self {
            UseTree::Path(tree) => tree.span(),
            UseTree::Group(tree) => tree.span(),
            UseTree::Glob(tree) => tree.span(),
            UseTree::Rename(tree) => tree.span(),
        }
    }
}

impl HasSpan for UseTree {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 路径导入
#[derive(Debug, PartialEq)]
pub struct UsePathTree {
    pub path: SimplePath,
    pub colon_colon: Option<TokenSpan>,
    pub tree: Option<Box<UseTree>>,
}

impl UsePathTree {
    pub fn span(&self) -> Span {
        let start = self.path.span().start;
        let end = if let Some(tree) = &self.tree {
            tree.span().end
        } else {
            self.path.span().end
        };

        Span::new(start, end)
    }
}

impl HasSpan for UsePathTree {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 分组导入
#[derive(Debug, PartialEq)]
pub struct UseGroupTree {
    pub prefix: Option<SimplePath>,
    pub brace_token: Brace,
    pub items: Punctuated<UseTree>,
}

impl UseGroupTree {
    pub fn span(&self) -> Span {
        self.brace_token.span()
    }
}

impl HasSpan for UseGroupTree {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 通配符导入
#[derive(Debug, PartialEq)]
pub struct UseGlobTree {
    pub prefix: Option<SimplePath>,
    pub star_token: TokenSpan,
}

impl UseGlobTree {
    pub fn span(&self) -> Span {
        self.star_token.span()
    }
}

impl HasSpan for UseGlobTree {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 重命名导入
#[derive(Debug, PartialEq)]
pub struct UseRenameTree {
    pub path: SimplePath,
    pub as_token: TokenSpan,
    pub rename: IdentSpan,
}

impl UseRenameTree {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.rename.span().end)
    }
}

impl HasSpan for UseRenameTree {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 类型参数约束
///
/// 根据Rust规范文档，类型参数约束的定义为：
/// TypeParamBound → TraitBound
///
/// 参考：https://doc.rust-lang.org/reference/trait-bounds.html
#[derive(Debug, PartialEq)]
pub enum TypeParamBound {
    /// 特质约束
    Trait(TraitBound),
}

impl TypeParamBound {
    pub fn span(&self) -> Span {
        match self {
            TypeParamBound::Trait(bound) => bound.span(),
        }
    }
}

impl HasSpan for TypeParamBound {
    fn span(&self) -> Span {
        self.span()
    }
}

/// 特质约束
///
/// 根据Rust规范文档，特质约束的定义为：
/// TraitBound → Path
///
/// 参考：https://doc.rust-lang.org/reference/trait-bounds.html
#[derive(Debug, PartialEq)]
pub struct TraitBound {
    pub path: TypePath,
}

impl TraitBound {
    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl HasSpan for TraitBound {
    fn span(&self) -> Span {
        self.span()
    }
}
