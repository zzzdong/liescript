use crate::{
    ast::{
        ident::Identifier,
        keyword::Keyword,
        literal::Literal,
        op::{BinOp, UnOp},
        symbol::Symbol,
        token::{Brace, Bracket, Paren, Punctuated, TokenSpan},
    },
    diagnostic::{Span, Spanned},
};

pub type ExprSpan = Spanned<Expr>;


impl From<Expr> for ExprSpan {
    fn from(value: Expr) -> Self {
        match value {
            Expr::Array(ExprArray { bracket_token, elems }) => {
                Spanned::new(value, bracket_token.span())
            }
            Expr::Assign(ExprAssign { left, eq_token, right }) => {
                Spanned::new(value, left.span().join(right.span()))
            }
            Expr::Binary(ExprBinary { op, lhs, rhs }) => {
                Spanned::new(value, lhs.span().join(rhs.span()))
            }
            Expr::Block(ExprBlock { block, .. }) => {
                Spanned::new(value, block.brac_token.span())
            }
            Expr::Break(ExprBreak { break_token, expr, .. }) => {
                let span = if let Some(expr) = expr {
                    break_token.span().join(expr.span())
                } else {
                    break_token.span()
                };
                Spanned::new(value, span)
            }
            Expr::Call(ExprCall { paren_token, func, args }) => {
                Spanned::new(value, func.span().join(paren_token.span()))
            }
            Expr::Cast(ExprCast { expr, as_token, ty }) => {
                Spanned::new(value, expr.span().join(ty.span()))
            }
            Expr::Closure(ExprClosure { or1_token, or2_token, body, .. }) => {
                Spanned::new(value, or1_token.span().join(body.span()))
            }
            Expr::Continue(ExprContinue { continue_token, .. }) => {
                Spanned::new(value, continue_token.span())
            }
            Expr::Field(ExprField { expr, dot_token, member }) => {
                Spanned::new(value, expr.span().join(member.span()))
            }
            Expr::ForLoop(ExprForLoop { for_token, expr, body, .. }) => {
                Spanned::new(value, for_token.span().join(body.span()))
            }
            Expr::Group(ExprGroup { paren_token, expr: inner_expr }) => {
                Spanned::new(value, paren_token.span())
            }
            Expr::If(ExprIf { if_token, then_branch, else_branch, .. }) => {
                let span = if let Some(else_branch) = else_branch {
                    if_token.span().join(else_branch.span())
                } else {
                    if_token.span().join(then_branch.span())
                };
                Spanned::new(value, span)
            }
            Expr::Index(ExprIndex { expr, bracket_token, index }) => {
                Spanned::new(value, expr.span().join(index.span()))
            }
            Expr::Infer(ExprInfer { underscore_token }) => {
                Spanned::new(value, underscore_token.span())
            }
            Expr::Lit(ExprLit { lit }) => {
                Spanned::new(value, lit.span())
            }
            Expr::Loop(ExprLoop { loop_token, body, .. }) => {
                Spanned::new(value, loop_token.span().join(body.span()))
            }
            Expr::Match(ExprMatch { match_token, expr, brace_token, .. }) => {
                Spanned::new(value, match_token.span().join(brace_token.span()))
            }
            Expr::MethodCall(ExprMethodCall { receiver, method, paren_token, args }) => {
                Spanned::new(value, receiver.span().join(paren_token.span()))
            }
            Expr::Path(ExprPath { path }) => {
                Spanned::new(value, path.span())
            }
            Expr::Range(ExprRange { start, end, limits }) => {
                let start_span = start.as_ref().map(|s| s.span()).unwrap_or(limits.span());
                let end_span = end.as_ref().map(|e| e.span()).unwrap_or(limits.span());
                Spanned::new(value, start_span.join(end_span))
            }
            Expr::Reference(ExprReference { and_token, expr }) => {
                Spanned::new(value, and_token.span().join(expr.span()))
            }
            Expr::Repeat(ExprRepeat { bracket_token, expr, len, .. }) => {
                Spanned::new(value, bracket_token.span())
            }
            Expr::Return(ExprReturn { return_token, expr }) => {
                let span = if let Some(expr) = expr {
                    return_token.span().join(expr.span())
                } else {
                    return_token.span()
                };
                Spanned::new(value, span)
            }
            Expr::Struct(ExprStruct { brace_token, .. }) => {
                Spanned::new(value, brace_token.span())
            }
            Expr::Try(ExprTry { expr, question_token }) => {
                Spanned::new(value, expr.span().join(question_token.span()))
            }
            Expr::Tuple(ExprTuple { paren_token, .. }) => {
                Spanned::new(value, paren_token.span())
            }
            Expr::Unary(ExprUnary { op, expr }) => {
                Spanned::new(value, op.span().join(expr.span()))
            }
            Expr::While(ExprWhile { while_token, body, .. }) => {
                Spanned::new(value, while_token.span().join(body.span()))
            }
        }
    }
}


#[derive(Debug)]
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

#[derive(Debug)]
pub struct ExprArray {
    pub bracket_token: Bracket,
    pub elems: Punctuated<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprAssign {
    pub left: Box<ExprSpan>,
    pub eq_token: TokenSpan,
    pub right: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprBinary {
    pub op: Spanned<BinOp>,
    pub lhs: Box<ExprSpan>,
    pub rhs: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprBlock {
    pub label: Option<Label>,
    pub block: Block,
}

#[derive(Debug)]
pub struct ExprBreak {
    pub break_token: TokenSpan,
    pub label: Option<Label>,
    pub expr: Option<Box<ExprSpan>>,
}

#[derive(Debug)]
pub struct ExprCall {
    pub paren_token: Paren,
    pub func: Box<ExprSpan>,
    pub args: Punctuated<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprCast {
    pub expr: Box<ExprSpan>,
    pub as_token: TokenSpan,
    pub ty: Box<TypeSpan>,
}

#[derive(Debug)]
pub struct ExprClosure {
    pub or1_token: TokenSpan,
    pub inputs: Vec<ExprSpan>,
    pub or2_token: TokenSpan,
    pub output: RetureType,
    pub body: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprContinue {
    pub continue_token: TokenSpan,
    pub label: Option<Label>,
}

#[derive(Debug)]
pub struct ExprField {
    pub expr: Box<ExprSpan>,
    pub dot_token: TokenSpan,
    pub member: IdentSpan,
}

#[derive(Debug)]
pub struct ExprForLoop {
    pub for_token: TokenSpan,
    pub label: Option<Label>,
    pub pat: Box<PatSpan>,
    pub in_token: TokenSpan,
    pub expr: Box<ExprSpan>,
    pub body: Block,
}

#[derive(Debug)]
pub struct ExprGroup {
    pub paren_token: Paren, // `()`
    pub expr: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprIf {
    pub if_token: TokenSpan,
    pub cond: Box<ExprSpan>,
    pub then_branch: Block,
    pub else_branch: Option<Box<ExprSpan>>,
}

#[derive(Debug)]
pub struct ExprIndex {
    pub expr: Box<ExprSpan>,
    pub bracket_token: Bracket,
    pub index: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprInfer {
    pub underscore_token: TokenSpan,
}

#[derive(Debug)]
pub struct ExprLit {
    pub lit: LiteralSpan,
}

#[derive(Debug)]
pub struct ExprLoop {
    pub loop_token: TokenSpan,
    pub label: Option<Label>,
    pub body: Block,
}

#[derive(Debug)]
pub struct ExprMatch {
    pub match_token: TokenSpan,
    pub expr: Box<ExprSpan>,
    pub brace_token: Brace,
    pub arms: Vec<Arm>,
}

#[derive(Debug)]
pub struct ExprMethodCall {
    pub receiver: Box<ExprSpan>,
    pub method: IdentSpan,
    pub paren_token: Paren,
    pub args: Punctuated<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprPath {
    pub path: Path,
}

#[derive(Debug)]
pub struct ExprRange {
    pub start: Option<Box<ExprSpan>>,
    pub end: Option<Box<ExprSpan>>,
    pub limits: Spanned<RangeLimits>,
}

#[derive(Debug)]
pub struct ExprReference {
    pub and_token: TokenSpan,
    pub expr: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprRepeat {
    pub bracket_token: Bracket,
    pub expr: Box<ExprSpan>,
    pub semicolon_token: TokenSpan,
    pub len: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprReturn {
    pub return_token: TokenSpan,
    pub expr: Option<Box<ExprSpan>>,
}

#[derive(Debug)]
pub struct ExprStruct {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldValue>,
    pub dot2_token: Option<TokenSpan>,
    pub rest: Option<IdentSpan>,
}

#[derive(Debug)]
pub struct ExprTry {
    pub expr: Box<ExprSpan>,
    pub question_token: TokenSpan,
}

#[derive(Debug)]
pub struct ExprTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprUnary {
    pub op: Spanned<UnOp>,
    pub expr: Box<ExprSpan>,
}

#[derive(Debug)]
pub struct ExprWhile {
    pub label: Option<Label>,
    pub while_token: TokenSpan,
    pub cond: Box<ExprSpan>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Label {
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
}

#[derive(Debug)]
pub struct Block {
    pub brac_token: Brace,
    pub stmts: Vec<StmtSpan>,
}

impl Block {
    pub fn is_empty(&self) -> bool {
        self.stmts.is_empty()
    }

    pub fn span(&self) -> Span {
        self.brac_token.span()
    }
}



#[derive(Debug)]
pub enum RetureType {
    Default,
    Typed(TokenSpan, Box<TypeSpan>),
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Box<PatSpan>,
    pub guard: Option<Box<ExprSpan>>,
    pub body: Box<ExprSpan>,
    pub comma: bool,
}

#[derive(Debug)]
pub enum RangeLimits {
    /// `..`
    HalfOpen,
    /// `..=`
    Closed,
}

#[derive(Debug)]
pub struct FieldValue {
    pub member: IdentSpan,
    pub expr: Box<ExprSpan>,
}

pub type PatSpan = Spanned<Pat>;

#[derive(Debug)]
pub enum Pat {
    Lit(LiteralSpan),
    Path(Path),
    Rest(PatRest),
    Struct(PatStruct),
    Tuple(PatTuple),
    Type(PatType),
    Wild(PatWild),
}

#[derive(Debug)]
pub struct PatStruct {
    pub path: Path,
    pub fields: Vec<FieldPat>,
    pub rest: Option<IdentSpan>,
}

#[derive(Debug)]
pub struct FieldPat {
    pub member: IdentSpan,
    pub pat: Box<PatSpan>,
}

#[derive(Debug)]
pub struct PatTuple {
    pub elts: Vec<PatSpan>,
}

#[derive(Debug)]
pub struct PatType {
    pub pat: Box<PatSpan>,
    pub colon_token: TokenSpan,
    pub ty: Box<TypeSpan>,
}

#[derive(Debug)]
pub struct PatRest {
    pub dot2_token: TokenSpan,
}

#[derive(Debug)]
pub struct PatWild {
    pub underscore_token: TokenSpan,
}

#[derive(Debug)]
pub struct Path {
    pub leading_colon: Option<TokenSpan>,
    pub segments: Punctuated<PathSegment>,
}

impl Path {
    pub fn span(&self) -> Span {
       let span = match self.segments.items.first() {
            Some((first, _)) => first.ident.span(),
            None => Span::default(),
       };
       if let Some(leading_colon) = &self.leading_colon {
           Span::new(leading_colon.span().start, span.end)
       } else {
           span
       }
    }
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: IdentSpan,
}

pub type IdentSpan = Spanned<Identifier>;

pub type LiteralSpan = Spanned<Literal>;

pub type KeywordSpan = Spanned<Keyword>;

pub type SymbolSpan = Spanned<Symbol>;

pub type TypeSpan = Spanned<Type>;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct TypeAny {
    pub any_token: TokenSpan,
}

#[derive(Debug)]
pub struct TypeArray {
    pub bracket_token: Bracket,
    pub elem: Box<TypeSpan>,
    pub semicolon: TokenSpan,
    pub len: ExprSpan,
}

#[derive(Debug)]
pub struct TypeBareFn {
    pub fn_token: TokenSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<BareFnArg>,
    pub output: RetureType,
}

#[derive(Debug)]
pub struct BareFnArg {
    pub name: Option<(IdentSpan, TokenSpan)>,
    pub ty: TypeSpan,
}

#[derive(Debug)]
pub struct TypeInfer {
    pub underscore_token: TokenSpan,
}

#[derive(Debug)]
pub struct TypeNever {
    pub never_token: TokenSpan,
}

#[derive(Debug)]
pub struct TypeParen {
    pub paren_token: Paren,
    pub elem: Box<TypeSpan>,
}

#[derive(Debug)]
pub struct TypeReference {
    pub and_token: TokenSpan,
    pub elem: Box<TypeSpan>,
}

#[derive(Debug)]
pub struct TypeSlice {
    pub bracket_token: Bracket,
    pub elem: Box<TypeSpan>,
}

#[derive(Debug)]
pub struct TypeTraitObject {
    pub dyn_token: TokenSpan,
    pub bounds: Punctuated<TypeParamBound>,
}

#[derive(Debug)]
pub struct TypeParamBound {
    pub name: IdentSpan,
}

#[derive(Debug)]
pub struct TypeTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<TypeSpan>,
}

pub type StmtSpan = Spanned<Stmt>;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Visibility {
    Public(TokenSpan),
    Private(TokenSpan),
    Inherited,
}

#[derive(Debug)]
pub enum Item {
    Enum(ItemEnum),
    Fn(ItemFn),
    Impl(ItemImpl),
    Struct(ItemStruct),
    Type(ItemType),
    Use(ItemUse),
}

#[derive(Debug)]
pub struct ItemEnum {
    pub vis: VisibilitySpan,
    pub enum_token: TokenSpan,
    pub name: IdentSpan,
    pub brace_token: Brace,
    pub variants: Punctuated<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name: IdentSpan,
    pub fields: FieldsSpan,
}

#[derive(Debug)]
pub struct ItemFn {
    pub vis: VisibilitySpan,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug)]
pub struct Signature {
    pub fn_token: TokenSpan,
    pub name: IdentSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<FnArgSpan>,
    pub output: Option<TypeSpan>,
}

pub type FnArgSpan = Spanned<FnArg>;

#[derive(Debug)]
pub enum FnArg {
    Receiver(Receiver),
    PatType(PatType),
}

#[derive(Debug)]
pub struct Receiver {
    pub reference: Option<TokenSpan>,
    pub self_token: TokenSpan,
}

#[derive(Debug)]
pub struct ItemImpl {
    pub impl_token: TokenSpan,
    pub trait_: Option<(Path, TokenSpan)>, // `From for`
    pub self_ty: Box<TypeSpan>,
    pub brace_token: Brace,
    pub items: Vec<ImplItemSpan>,
}

pub type ImplItemSpan = Spanned<ImplItem>;

#[derive(Debug)]
pub enum ImplItem {
    Const(ImplItemConst),
    Fn(ImplItemFn),
}

#[derive(Debug)]
pub struct ImplItemConst {
    pub const_token: TokenSpan,
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
    pub ty: TypeSpan,
    pub eq_token: TokenSpan,
    pub expr: ExprSpan,
    pub semi_token: TokenSpan,
}

#[derive(Debug)]
pub struct ImplItemFn {
    pub vis: VisibilitySpan,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug)]
pub struct ItemStruct {
    pub vis: VisibilitySpan,
    pub struct_token: TokenSpan,
    pub name: IdentSpan,
    pub fields: FieldsSpan,
}

pub type FieldsSpan = Spanned<Fields>;

#[derive(Debug)]
pub enum Fields {
    Named(FieldsNamed),
    Unnamed(FieldsUnnamed),
    Unit,
}

#[derive(Debug)]
pub struct FieldsNamed {
    pub brace_token: Brace,
    pub named: Punctuated<Field>,
}

#[derive(Debug)]
pub struct FieldsUnnamed {
    pub paren_token: Paren,
    pub unnamed: Punctuated<Field>,
}

#[derive(Debug)]
pub struct Field {
    pub vis: VisibilitySpan,
    pub name: Option<IdentSpan>,
    pub colon_token: Option<TokenSpan>,
    pub ty: TypeSpan,
}

#[derive(Debug)]
pub struct ItemType {
    pub vis: VisibilitySpan,
    pub type_token: TokenSpan,
    pub ident: IdentSpan,
    pub eq_token: TokenSpan,
    pub ty: TypeSpan,
    pub semi_token: TokenSpan,
}

#[derive(Debug)]
pub struct ItemUse {
    pub use_token: TokenSpan,
    pub leading_colon: Option<TokenSpan>,
    pub tree: UseTreeSpan,
    pub semi_token: TokenSpan,
}

pub type UseTreeSpan = Spanned<UseTree>;

#[derive(Debug)]
pub enum UseTree {
    Path(UsePath),
    Name(UseName),
    Rename(UseRename),
    Glob(UseGlob),
    Group(UseGroup),
}

#[derive(Debug)]
pub struct UsePath {
    pub name: IdentSpan,
    pub colon2_token: TokenSpan,
    pub tree: Box<UseTreeSpan>,
}

#[derive(Debug)]
pub struct UseName {
    pub name: IdentSpan,
}

#[derive(Debug)]
pub struct UseRename {
    pub name: IdentSpan,
    pub as_token: TokenSpan,
    pub alias: IdentSpan,
}

#[derive(Debug)]
pub struct UseGlob {
    pub star_token: TokenSpan,
}

#[derive(Debug)]
pub struct UseGroup {
    pub brace_token: Brace,
    pub items: Punctuated<UseTreeSpan>,
}
