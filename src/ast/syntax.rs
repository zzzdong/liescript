use crate::{
    ast::{
        ident::Identifier,
        literal::Literal,
        op::{BinOp, UnOp},
        token::{Brace, Bracket, Paren, Punctuated, TokenSpan},
    },
    diagnostic::Spanned,
};

pub type ExprSpan = Spanned<Expr>;

pub enum Expr {
    Array(ExprArray),
    Assign(ExprAssign),
    Binary(ExprBinary),
    Block(ExprBlock),
    Break(ExprBreak),
    Call(ExprCall),
    Cast(ExprCast),
    Closure(ExprClosure),
    Continue(ExprContinue),
    Field(ExprField),
    ForLoop(ExprForLoop),
    Group(ExprGroup),
    If(ExprIf),
    Index(ExprIndex),
    Infer(ExprInfer),
    Lit(ExprLit),
    Loop(ExprLoop),
    Match(ExprMatch),
    MethodCall(ExprMethodCall),
    Path(ExprPath),
    Range(ExprRange),
    Reference(ExprReference),
    Repeat(ExprRepeat),
    Return(ExprReturn),
    Struct(ExprStruct),
    Try(ExprTry),
    Tuple(ExprTuple),
    Unary(ExprUnary),
    While(ExprWhile),
}

pub struct ExprArray {
    pub bracket_token: Bracket,
    pub elems: Punctuated<ExprSpan>,
}

pub struct ExprAssign {
    pub left: Box<ExprSpan>,
    pub eq_token: TokenSpan,
    pub right: Box<ExprSpan>,
}

pub struct ExprBinary {
    pub op: Spanned<BinOp>,
    pub lhs: Box<ExprSpan>,
    pub rhs: Box<ExprSpan>,
}

pub struct ExprBlock {
    pub label: Option<Label>,
    pub block: Block,
}

pub struct ExprBreak {
    pub break_token: TokenSpan,
    pub label: Option<Label>,
    pub expr: Option<Box<ExprSpan>>,
}

pub struct ExprCall {
    pub paren_token: Paren,
    pub func: Box<ExprSpan>,
    pub args: Punctuated<ExprSpan>,
}

pub struct ExprCast {
    pub expr: Box<ExprSpan>,
    pub as_token: TokenSpan,
    pub ty: Box<TypeSpan>,
}

pub struct ExprClosure {
    pub or1_token: TokenSpan,
    pub inputs: Vec<ExprSpan>,
    pub or2_token: TokenSpan,
    pub output: RetureType,
    pub body: Box<ExprSpan>,
}

pub struct ExprContinue {
    pub continue_token: TokenSpan,
    pub label: Option<Label>,
}

pub struct ExprField {
    pub expr: Box<ExprSpan>,
    pub dot_token: TokenSpan,
    pub member: Ident,
}

pub struct ExprForLoop {
    pub for_token: TokenSpan,
    pub label: Option<Label>,
    pub pat: Box<PatSpan>,
    pub in_token: TokenSpan,
    pub expr: Box<ExprSpan>,
    pub body: Block,
}

pub struct ExprGroup {
    pub paren_token: Paren, // `()`
    pub expr: Box<ExprSpan>,
}

pub struct ExprIf {
    pub if_token: TokenSpan,
    pub cond: Box<ExprSpan>,
    pub then_branch: Block,
    pub else_branch: Option<Box<ExprSpan>>,
}

pub struct ExprIndex {
    pub expr: Box<ExprSpan>,
    pub bracket_token: Bracket,
    pub index: Box<ExprSpan>,
}

pub struct ExprInfer {
    pub underscore_token: TokenSpan,
}

pub struct ExprLit {
    pub lit: Lit,
}

pub struct ExprLoop {
    pub loop_token: TokenSpan,
    pub label: Option<Label>,
    pub body: Block,
}

pub struct ExprMatch {
    pub match_token: TokenSpan,
    pub expr: Box<ExprSpan>,
    pub brace_token: Brace,
    pub arms: Vec<Arm>,
}

pub struct ExprMethodCall {
    pub receiver: Box<ExprSpan>,
    pub method: Ident,
    pub paren_token: Paren,
    pub args: Punctuated<ExprSpan>,
}

pub struct ExprPath {
    pub path: Path,
}

pub struct ExprRange {
    pub start: Option<Box<ExprSpan>>,
    pub end: Option<Box<ExprSpan>>,
    pub limits: Spanned<RangeLimits>,
}

pub struct ExprReference {
    pub and_token: TokenSpan,
    pub expr: Box<ExprSpan>,
}

pub struct ExprRepeat {
    pub bracket_token: Bracket,
    pub expr: Box<ExprSpan>,
    pub semicolon_token: TokenSpan,
    pub len: Box<ExprSpan>,
}

pub struct ExprReturn {
    pub return_token: TokenSpan,
    pub expr: Option<Box<ExprSpan>>,
}

pub struct ExprStruct {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldValue>,
    pub dot2_token: Option<TokenSpan>,
    pub rest: Option<Ident>,
}

pub struct ExprTry {
    pub expr: Box<ExprSpan>,
    pub question_token: TokenSpan,
}

pub struct ExprTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<ExprSpan>,
}

pub struct ExprUnary {
    pub op: Spanned<UnOp>,
    pub expr: Box<ExprSpan>,
}

pub struct ExprWhile {
    pub label: Option<Label>,
    pub while_token: TokenSpan,
    pub cond: Box<ExprSpan>,
    pub body: Block,
}

pub struct Label {
    pub name: Ident,
    pub colon_token: TokenSpan,
}

pub struct Block {
    pub brac_token: Brace,
    pub stmts: Vec<StmtSpan>,
}

pub enum RetureType {
    Default,
    Typed(TokenSpan, Box<TypeSpan>),
}

pub struct Arm {
    pub pat: Box<PatSpan>,
    pub guard: Option<Box<ExprSpan>>,
    pub body: Box<ExprSpan>,
    pub comma: bool,
}

pub enum RangeLimits {
    /// `..`
    HalfOpen,
    /// `..=`
    Closed,
}

pub struct FieldValue {
    pub member: Ident,
    pub expr: Box<ExprSpan>,
}

pub type PatSpan = Spanned<Pat>;

pub enum Pat {
    Lit(Lit),
    Path(Path),
    Rest(PatRest),
    Struct(PatStruct),
    Tuple(PatTuple),
    Type(PatType),
    Wild(PatWild),
}

pub struct PatStruct {
    pub path: Path,
    pub fields: Vec<FieldPat>,
    pub rest: Option<Ident>,
}

pub struct FieldPat {
    pub member: Ident,
    pub pat: Box<PatSpan>,
}

pub struct PatTuple {
    pub elts: Vec<PatSpan>,
}

pub struct PatType {
    pub pat: Box<PatSpan>,
    pub colon_token: TokenSpan,
    pub ty: Box<TypeSpan>,
}

pub struct PatRest {
    pub dot2_token: TokenSpan,
}

pub struct PatWild {
    pub underscore_token: TokenSpan,
}

pub struct Path {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<PathSegment>,
}

pub struct PathSegment {
    pub ident: Ident,
}

pub type Ident = Spanned<Identifier>;

pub type Lit = Spanned<Literal>;

pub type Keyword = Spanned<super::keyword::Keyword>;

pub type Symbol = Spanned<super::symbol::Symbol>;

pub type TypeSpan = Spanned<Type>;

pub enum Type {
    Any(TypeAny),
    Array(TypeArray),
    BareFn(TypeBareFn),
    Infer(TypeInfer),
    Never(TypeNever),
    Path(Path),
    Paren(TypeParen),
    Reference(TypeReference),
    Slice(TypeSlice),
    TraitObject(TypeTraitObject),
    Tuple(TypeTuple),
}

pub struct TypeAny {
    pub any_token: TokenSpan,
}

pub struct TypeArray {
    pub bracket_token: Bracket,
    pub elem: Box<TypeSpan>,
    pub semicolon: TokenSpan,
    pub len: ExprSpan,
}

pub struct TypeBareFn {
    pub fn_token: TokenSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<BareFnArg>,
    pub output: RetureType,
}

pub struct BareFnArg {
    pub name: Option<(Ident, TokenSpan)>,
    pub ty: TypeSpan,
}

pub struct TypeInfer {
    pub underscore_token: TokenSpan,
}

pub struct TypeNever {
    pub never_token: TokenSpan,
}

pub struct TypeParen {
    pub paren_token: Paren,
    pub elem: Box<TypeSpan>,
}

pub struct TypeReference {
    pub and_token: TokenSpan,
    pub elem: Box<TypeSpan>,
}

pub struct TypeSlice {
    pub bracket_token: Bracket,
    pub elem: Box<TypeSpan>,
}

pub struct TypeTraitObject {
    pub dyn_token: TokenSpan,
    pub bounds: Punctuated<TypeParamBound>,
}

pub struct TypeParamBound {
    pub name: Ident,
}

pub struct TypeTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<TypeSpan>,
}

pub type StmtSpan = Spanned<Stmt>;

pub enum Stmt {
    /// 表达式语句：`x + 1;`
    Expr(ExprSpan),
    /// 变量绑定：`let x = 42;` 或 `let mut y = 0;`
    Let(LetStmt),
    /// Item definition.
    Item(Item),
    /// 空语句：`;`
    Empty,
}

pub struct LetStmt {
    pub let_token: TokenSpan,
    pub pat: PatSpan,
    pub colon_token: TokenSpan,
    pub ty: Option<TypeSpan>,
    pub eq_token: TokenSpan,
    pub expr: ExprSpan,
    pub semi_token: TokenSpan,
}

pub type VisibilitySpan = Spanned<Visibility>;

pub enum Visibility {
    Public(TokenSpan),
    Private(TokenSpan),
    Inherited,
}

enum Item {
    Enum(ItemEnum),
    Fn(ItemFn),
    Impl(ItemImpl),
    Struct(ItemStruct),
    Type(ItemType),
    Use(ItemUse),
}

pub struct ItemEnum {
    pub vis: VisibilitySpan,
    pub enum_token: TokenSpan,
    pub name: Ident,
    pub brace_token: Brace,
    pub variants: Punctuated<Variant>,
}

pub struct Variant {
    pub name: Ident,
    pub fields: FieldsSpan,
}

pub struct ItemFn {
    pub vis: VisibilitySpan,
    pub sig: Signature,
    pub block: Block,
}

pub struct Signature {
    pub fn_token: TokenSpan,
    pub name: Ident,
    pub paren_token: Paren,
    pub inputs: Punctuated<FnArgSpan>,
    pub output: Option<TypeSpan>,
}

pub type FnArgSpan = Spanned<FnArg>;

pub enum FnArg {
    Receiver(Receiver),
    PatType(PatType),
}

pub struct Receiver {
    pub reference: Option<TokenSpan>,
    pub self_token: TokenSpan,
}

pub struct ItemImpl {
    pub impl_token: TokenSpan,
    pub trait_: Option<(Path, TokenSpan)>, // `From for`
    pub self_ty: Box<TypeSpan>,
    pub brace_token: Brace,
    pub items: Vec<ImplItemSpan>,
}

pub type ImplItemSpan = Spanned<ImplItem>;

pub enum ImplItem {
    Const(ImplItemConst),
    Fn(ImplItemFn),
}

pub struct ImplItemConst {
    pub const_token: TokenSpan,
    pub name: Ident,
    pub colon_token: TokenSpan,
    pub ty: TypeSpan,
    pub eq_token: TokenSpan,
    pub expr: ExprSpan,
    pub semi_token: TokenSpan,
}

pub struct ImplItemFn {
    pub vis: VisibilitySpan,
    pub sig: Signature,
    pub block: Block,
}

pub struct ItemStruct {
    pub vis: VisibilitySpan,
    pub struct_token: TokenSpan,
    pub name: Ident,
    pub fields: FieldsSpan,
}

pub type FieldsSpan = Spanned<Fields>;

pub enum Fields {
    Named(FieldsNamed),
    Unnamed(FieldsUnnamed),
    Unit,
}

pub struct FieldsNamed {
    pub brace_token: Brace,
    pub named: Punctuated<Field>,
}

pub struct FieldsUnnamed {
    pub paren_token: Paren,
    pub unnamed: Punctuated<Field>,
}

pub struct Field {
    pub vis: VisibilitySpan,
    pub name: Option<Ident>,
    pub colon_token: Option<TokenSpan>,
    pub ty: TypeSpan,
}

pub struct ItemType {
    pub vis: VisibilitySpan,
    pub type_token: TokenSpan,
    pub ident: Ident,
    pub eq_token: TokenSpan,
    pub ty: TypeSpan,
    pub semi_token: TokenSpan,
}

pub struct ItemUse {
    pub use_token: TokenSpan,
    pub leading_colon: Option<TokenSpan>,
    pub tree: UseTreeSpan,
    pub semi_token: TokenSpan,
}

pub type UseTreeSpan = Spanned<UseTree>;

pub enum UseTree {
    Path(UsePath),
    Name(UseName),
    Rename(UseRename),
    Glob(UseGlob),
    Group(UseGroup),
}

pub struct UsePath {
    pub name: Ident,
    pub colon2_token: TokenSpan,
    pub tree: Box<UseTreeSpan>,
}

pub struct UseName {
    pub name: Ident,
}

pub struct UseRename {
    pub name: Ident,
    pub as_token: TokenSpan,
    pub alias: Ident,
}

pub struct UseGlob {
    pub star_token: TokenSpan,
}

pub struct UseGroup {
    pub brace_token: Brace,
    pub items: Punctuated<UseTreeSpan>,
}
