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
    Let(ExprLet), // TODO: let Some(x) = opt.
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

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Array(expr) => expr.span(),
            Expr::Assign(expr) => expr.span(),
            Expr::Binary(expr) => expr.span(),
            Expr::Block(expr) => expr.span(),
            Expr::Break(expr) => expr.span(),
            Expr::Call(expr) => expr.span(),
            Expr::Cast(expr) => expr.span(),
            Expr::Closure(expr) => expr.span(),
            Expr::Continue(expr) => expr.span(),
            Expr::Field(expr) => expr.span(),
            Expr::ForLoop(expr) => expr.span(),
            Expr::Group(expr) => expr.span(),
            Expr::If(expr) => expr.span(),
            Expr::Index(expr) => expr.span(),
            Expr::Infer(expr) => expr.span(),
            Expr::Lit(expr) => expr.span(),
            Expr::Loop(expr) => expr.span(),
            Expr::Match(expr) => expr.span(),
            Expr::MethodCall(expr) => expr.span(),
            Expr::Path(expr) => expr.span(),
            Expr::Range(expr) => expr.span(),
            Expr::Reference(expr) => expr.span(),
            Expr::Repeat(expr) => expr.span(),
            Expr::Return(expr) => expr.span(),
            Expr::Struct(expr) => expr.span(),
            Expr::Try(expr) => expr.span(),
            Expr::Tuple(expr) => expr.span(),
            Expr::Unary(expr) => expr.span(),
            Expr::While(expr) => expr.span(),
        }
    }
}

impl HasSpan for Expr {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprArray {
    pub bracket_token: Bracket,
    pub elems: Punctuated<Expr>,
}

impl ExprArray {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for ExprArray {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprAssign {
    pub left: Box<Expr>,
    pub eq_token: TokenSpan,
    pub right: Box<Expr>,
}

impl ExprAssign {
    pub fn span(&self) -> Span {
        Span::new(self.left.span().start, self.right.span().end)
    }
}

impl HasSpan for ExprAssign {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprBinary {
    pub op: Spanned<BinOp>,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl ExprBinary {
    pub fn span(&self) -> Span {
        Span::new(self.lhs.span().start, self.rhs.span().end)
    }
}

impl HasSpan for ExprBinary {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprBlock {
    pub label: Option<Label>,
    pub block: Block,
}

impl ExprBlock {
    pub fn span(&self) -> Span {
        if let Some(label) = &self.label {
            Span::new(label.span().start, self.block.span().end)
        } else {
            self.block.span()
        }
    }
}

impl HasSpan for ExprBlock {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprBreak {
    pub break_token: TokenSpan,
    pub label: Option<Label>,
    pub expr: Option<Box<Expr>>,
}

impl ExprBreak {
    pub fn span(&self) -> Span {
        let start = self.break_token.span().start;
        if let Some(expr) = &self.expr {
            Span::new(start, expr.span().end)
        } else if let Some(label) = &self.label {
            Span::new(start, label.span().end)
        } else {
            self.break_token.span()
        }
    }
}

impl HasSpan for ExprBreak {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprCall {
    pub paren_token: Paren,
    pub func: Box<Expr>,
    pub args: Punctuated<Expr>,
}

impl ExprCall {
    pub fn span(&self) -> Span {
        Span::new(self.func.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for ExprCall {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprCast {
    pub expr: Box<Expr>,
    pub as_token: TokenSpan,
    pub ty: Box<Type>,
}

impl ExprCast {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.ty.span().end)
    }
}

impl HasSpan for ExprCast {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprClosure {
    pub or1_token: TokenSpan,
    pub inputs: Punctuated<Pat>,
    pub or2_token: TokenSpan,
    pub output: RetureType,
    pub body: Box<Expr>,
}

impl ExprClosure {
    pub fn span(&self) -> Span {
        Span::new(self.or1_token.span().start, self.body.span().end)
    }
}

impl HasSpan for ExprClosure {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprContinue {
    pub continue_token: TokenSpan,
    pub label: Option<Label>,
}

impl ExprContinue {
    pub fn span(&self) -> Span {
        let start = self.continue_token.span().start;
        if let Some(label) = &self.label {
            Span::new(start, label.span().end)
        } else {
            self.continue_token.span()
        }
    }
}

impl HasSpan for ExprContinue {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprField {
    pub expr: Box<Expr>,
    pub dot_token: TokenSpan,
    pub member: IdentSpan,
}

impl ExprField {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.member.span().end)
    }
}

impl HasSpan for ExprField {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprForLoop {
    pub for_token: TokenSpan,
    pub label: Option<Label>,
    pub pat: Box<Pat>,
    pub in_token: TokenSpan,
    pub expr: Box<Expr>,
    pub body: Block,
}

impl ExprForLoop {
    pub fn span(&self) -> Span {
        let start = self.for_token.span().start;
        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for ExprForLoop {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprGroup {
    pub paren_token: Paren, // `()`
    pub expr: Box<Expr>,
}

impl ExprGroup {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for ExprGroup {
    fn span(&self) -> Span {
        self.span()
    }
}
#[derive(Debug)]
pub struct ExprIf {
    pub if_token: TokenSpan,
    pub cond: Box<Expr>,
    pub then_branch: Block,
    pub else_branch: Option<Box<Expr>>,
}

impl ExprIf {
    pub fn span(&self) -> Span {
        let start = self.if_token.span().start;
        if let Some(else_branch) = &self.else_branch {
            Span::new(start, else_branch.span().end)
        } else {
            Span::new(start, self.then_branch.span().end)
        }
    }
}

impl HasSpan for ExprIf {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprIndex {
    pub expr: Box<Expr>,
    pub bracket_token: Bracket,
    pub index: Box<Expr>,
}

impl ExprIndex {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.bracket_token.span().end)
    }
}

impl HasSpan for ExprIndex {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprInfer {
    pub underscore_token: TokenSpan,
}

impl ExprInfer {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for ExprInfer {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprLit {
    pub lit: LiteralSpan,
}

impl ExprLit {
    pub fn span(&self) -> Span {
        self.lit.span()
    }
}

impl HasSpan for ExprLit {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprLoop {
    pub loop_token: TokenSpan,
    pub label: Option<Label>,
    pub body: Block,
}

impl ExprLoop {
    pub fn span(&self) -> Span {
        let start = self.loop_token.span().start;
        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for ExprLoop {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprMatch {
    pub match_token: TokenSpan,
    pub expr: Box<Expr>,
    pub brace_token: Brace,
    pub arms: Vec<Arm>,
}

impl ExprMatch {
    pub fn span(&self) -> Span {
        Span::new(self.match_token.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for ExprMatch {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprMethodCall {
    pub receiver: Box<Expr>,
    pub method: IdentSpan,
    pub paren_token: Paren,
    pub args: Punctuated<Expr>,
}

impl ExprMethodCall {
    pub fn span(&self) -> Span {
        Span::new(self.receiver.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for ExprMethodCall {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprPath {
    pub path: Path,
}

impl ExprPath {
    pub fn span(&self) -> Span {
        self.path.span()
    }
}

impl HasSpan for ExprPath {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprRange {
    pub start: Option<Box<Expr>>,
    pub end: Option<Box<Expr>>,
    pub limits: Spanned<RangeLimits>,
}

impl ExprRange {
    pub fn span(&self) -> Span {
        match (&self.start, &self.end) {
            (Some(start), Some(end)) => Span::new(start.span().start, end.span().end),
            (Some(start), None) => Span::new(start.span().start, self.limits.span().end),
            (None, Some(end)) => Span::new(self.limits.span().start, end.span().end),
            (None, None) => self.limits.span(),
        }
    }
}

impl HasSpan for ExprRange {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprReference {
    pub and_token: TokenSpan,
    pub expr: Box<Expr>,
}

impl ExprReference {
    pub fn span(&self) -> Span {
        Span::new(self.and_token.span().start, self.expr.span().end)
    }
}

impl HasSpan for ExprReference {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprRepeat {
    pub bracket_token: Bracket,
    pub expr: Box<Expr>,
    pub semicolon_token: TokenSpan,
    pub len: Box<Expr>,
}

impl ExprRepeat {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for ExprRepeat {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprReturn {
    pub return_token: TokenSpan,
    pub expr: Option<Box<Expr>>,
}

impl ExprReturn {
    pub fn span(&self) -> Span {
        if let Some(expr) = &self.expr {
            Span::new(self.return_token.span().start, expr.span().end)
        } else {
            self.return_token.span()
        }
    }
}

impl HasSpan for ExprReturn {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprStruct {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldValue>,
    pub dot2_token: Option<TokenSpan>,
    pub rest: Option<IdentSpan>,
}

impl ExprStruct {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for ExprStruct {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprTry {
    pub expr: Box<Expr>,
    pub question_token: TokenSpan,
}

impl ExprTry {
    pub fn span(&self) -> Span {
        Span::new(self.expr.span().start, self.question_token.span().end)
    }
}

impl HasSpan for ExprTry {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<Expr>,
}

impl ExprTuple {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for ExprTuple {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprUnary {
    pub op: Spanned<UnOp>,
    pub expr: Box<Expr>,
}

impl ExprUnary {
    pub fn span(&self) -> Span {
        Span::new(self.op.span().start, self.expr.span().end)
    }
}

impl HasSpan for ExprUnary {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ExprWhile {
    pub label: Option<Label>,
    pub while_token: TokenSpan,
    pub cond: Box<Expr>,
    pub body: Block,
}

impl ExprWhile {
    pub fn span(&self) -> Span {
        let start = self.while_token.span().start;
        Span::new(start, self.body.span().end)
    }
}

impl HasSpan for ExprWhile {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct Label {
    pub name: IdentSpan,
    pub colon_token: TokenSpan,
}

impl Label {
    pub fn span(&self) -> Span {
        Span::new(self.name.span().start, self.colon_token.span().end)
    }
}

impl HasSpan for Label {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct Block {
    pub brac_token: Brace,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn is_empty(&self) -> bool {
        self.stmts.is_empty()
    }

    pub fn span(&self) -> Span {
        self.brac_token.span()
    }
}

impl HasSpan for Block {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum RetureType {
    Default,
    Typed(TokenSpan, Box<Type>),
}

impl RetureType {
    pub fn span(&self) -> Span {
        match self {
            RetureType::Default => Span::default(),
            RetureType::Typed(token, ty) => Span::new(token.span().start, ty.span().end),
        }
    }
}

impl HasSpan for RetureType {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct Arm {
    pub pat: Box<Pat>,
    pub guard: Option<Box<Expr>>,
    pub fat_arrow_token: TokenSpan,
    pub body: Box<Expr>,
    pub comma: bool,
}

impl Arm {
    pub fn span(&self) -> Span {
        let start = self.pat.span().start;
        let end = self.body.span().end;
        Span::new(start, end)
    }
}

impl HasSpan for Arm {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum RangeLimits {
    /// `..`
    HalfOpen,
    /// `..=`
    Closed,
}

impl RangeLimits {
    pub fn span(&self) -> Span {
        // 由于RangeLimits本身没有span信息，我们返回一个默认的span
        Span::default()
    }
}

impl HasSpan for RangeLimits {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct FieldValue {
    pub member: IdentSpan,
    pub expr: Box<Expr>,
}

impl FieldValue {
    pub fn span(&self) -> Span {
        Span::new(self.member.span().start, self.expr.span().end)
    }
}

impl HasSpan for FieldValue {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum Pat {
    // 保持与现有解析器代码兼容的模式类型
    Lit(LiteralSpan),
    Ident(IdentSpan),
    Path(Path),
    Wild(PatWild),
    Rest(PatRest),
    Struct(PatStruct),
    Tuple(PatTuple),
    Type(PatType),
    // 新增的模式类型，根据Rust文档
    Range(PatRange),
    Reference(PatReference),
    TupleStruct(PatTupleStruct),
    Slice(PatSlice),
    Or(PatOr),
    Paren(PatParen),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Lit(lit) => lit.span(),
            Pat::Ident(ident) => ident.span(),
            Pat::Path(path) => path.span(),
            Pat::Wild(pat) => pat.span(),
            Pat::Rest(pat) => pat.span(),
            Pat::Struct(pat) => pat.span(),
            Pat::Tuple(pat) => pat.span(),
            Pat::Type(pat) => pat.span(),
            Pat::Range(pat) => pat.span(),
            Pat::Reference(pat) => pat.span(),
            Pat::TupleStruct(pat) => pat.span(),
            Pat::Slice(pat) => pat.span(),
            Pat::Or(pat) => pat.span(),
            Pat::Paren(pat) => pat.span(),
        }
    }
}

impl HasSpan for Pat {
    fn span(&self) -> Span {
        self.span()
    }
}

// 删除不再需要的PatLit和PatIdent结构体，因为我们直接使用LiteralSpan和IdentSpan

// 通配符模式
#[derive(Debug)]
pub struct PatWild {
    pub underscore_token: TokenSpan,
}

impl PatWild {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for PatWild {
    fn span(&self) -> Span {
        self.span()
    }
}

// 范围模式
#[derive(Debug)]
pub struct PatRange {
    pub lo: Box<Expr>,
    pub limits: Spanned<RangeLimits>,
    pub hi: Box<Expr>,
}

impl PatRange {
    pub fn span(&self) -> Span {
        Span::new(self.lo.span().start, self.hi.span().end)
    }
}

impl HasSpan for PatRange {
    fn span(&self) -> Span {
        self.span()
    }
}

// 引用模式
#[derive(Debug)]
pub struct PatReference {
    pub and_token: TokenSpan,
    pub pat: Box<Pat>,
}

impl PatReference {
    pub fn span(&self) -> Span {
        Span::new(self.and_token.span().start, self.pat.span().end)
    }
}

impl HasSpan for PatReference {
    fn span(&self) -> Span {
        self.span()
    }
}

// 结构体模式
#[derive(Debug)]
pub struct PatStruct {
    pub path: Path,
    pub brace_token: Brace,
    pub fields: Punctuated<FieldPat>,
    pub rest: Option<PatRest>,
}

impl PatStruct {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for PatStruct {
    fn span(&self) -> Span {
        self.span()
    }
}

// 元组结构体模式
#[derive(Debug)]
pub struct PatTupleStruct {
    pub path: Path,
    pub paren_token: Paren,
    pub elems: Punctuated<Pat>,
}

impl PatTupleStruct {
    pub fn span(&self) -> Span {
        Span::new(self.path.span().start, self.paren_token.span().end)
    }
}

impl HasSpan for PatTupleStruct {
    fn span(&self) -> Span {
        self.span()
    }
}

// 字段模式
#[derive(Debug)]
pub struct FieldPat {
    pub member: IdentSpan,
    pub colon_token: Option<TokenSpan>,
    pub pat: Box<Pat>,
}

impl FieldPat {
    pub fn span(&self) -> Span {
        let start = self.member.span().start;
        let end = self.pat.span().end;
        Span::new(start, end)
    }
}

impl HasSpan for FieldPat {
    fn span(&self) -> Span {
        self.span()
    }
}

// 元组模式
#[derive(Debug)]
pub struct PatTuple {
    pub elts: Vec<Pat>,
}

impl PatTuple {
    pub fn span(&self) -> Span {
        // 由于没有paren_token字段，我们需要从elts中推断span
        if let Some(first) = self.elts.first() {
            if let Some(last) = self.elts.last() {
                return Span::new(first.span().start, last.span().end);
            }
        }
        Span::default()
    }
}

impl HasSpan for PatTuple {
    fn span(&self) -> Span {
        self.span()
    }
}

// 切片模式
#[derive(Debug)]
pub struct PatSlice {
    pub bracket_token: Bracket,
    pub elems: Punctuated<Pat>,
}

impl PatSlice {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for PatSlice {
    fn span(&self) -> Span {
        self.span()
    }
}

// 删除不再需要的PatPath结构体，因为我们直接使用Path

// 或模式
#[derive(Debug)]
pub struct PatOr {
    pub cases: Punctuated<Pat>,
}

impl PatOr {
    pub fn span(&self) -> Span {
        if let Some((first, _)) = self.cases.items.first() {
            if let Some((last, _)) = self.cases.items.last() {
                return Span::new(first.span().start, last.span().end);
            }
        }
        Span::default()
    }
}

impl HasSpan for PatOr {
    fn span(&self) -> Span {
        self.span()
    }
}

// 剩余模式
#[derive(Debug)]
pub struct PatRest {
    pub dot2_token: TokenSpan,
}

impl PatRest {
    pub fn span(&self) -> Span {
        self.dot2_token.span()
    }
}

impl HasSpan for PatRest {
    fn span(&self) -> Span {
        self.span()
    }
}

// 类型标注模式
#[derive(Debug)]
pub struct PatType {
    pub pat: Box<Pat>,
    pub colon_token: TokenSpan,
    pub ty: Box<Type>,
}

impl PatType {
    pub fn span(&self) -> Span {
        Span::new(self.pat.span().start, self.ty.span().end)
    }
}

impl HasSpan for PatType {
    fn span(&self) -> Span {
        self.span()
    }
}

// 分组模式
#[derive(Debug)]
pub struct PatParen {
    pub paren_token: Paren,
    pub pat: Box<Pat>,
}

impl PatParen {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for PatParen {
    fn span(&self) -> Span {
        self.span()
    }
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

impl HasSpan for Path {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: IdentSpan,
}



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

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Any(ty) => ty.span(),
            Type::Array(ty) => ty.span(),
            Type::BareFn(ty) => ty.span(),
            Type::Infer(ty) => ty.span(),
            Type::Never(ty) => ty.span(),
            Type::Path(ty) => ty.span(),
            Type::Paren(ty) => ty.span(),
            Type::Reference(ty) => ty.span(),
            Type::Slice(ty) => ty.span(),
            Type::TraitObject(ty) => ty.span(),
            Type::Tuple(ty) => ty.span(),
        }
    }
}

impl HasSpan for Type {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeAny {
    pub any_token: TokenSpan,
}

impl TypeAny {
    pub fn span(&self) -> Span {
        self.any_token.span()
    }
}

impl HasSpan for TypeAny {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeArray {
    pub bracket_token: Bracket,
    pub elem: Box<Type>,
    pub semicolon: TokenSpan,
    pub len: Expr,
}

impl TypeArray {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for TypeArray {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeBareFn {
    pub fn_token: TokenSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<BareFnArg>,
    pub output: RetureType,
}

impl TypeBareFn {
    pub fn span(&self) -> Span {
        self.fn_token.span()
    }
}

impl HasSpan for TypeBareFn {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct BareFnArg {
    pub name: Option<(IdentSpan, TokenSpan)>,
    pub ty: Type,
}

// 无需为BareFnArg实现span方法，因为它不是独立的类型表达式

#[derive(Debug)]
pub struct TypeInfer {
    pub underscore_token: TokenSpan,
}

impl TypeInfer {
    pub fn span(&self) -> Span {
        self.underscore_token.span()
    }
}

impl HasSpan for TypeInfer {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeNever {
    pub never_token: TokenSpan,
}

impl TypeNever {
    pub fn span(&self) -> Span {
        self.never_token.span()
    }
}

impl HasSpan for TypeNever {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeParen {
    pub paren_token: Paren,
    pub elem: Box<Type>,
}

impl TypeParen {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TypeParen {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeReference {
    pub and_token: TokenSpan,
    pub elem: Box<Type>,
}

impl TypeReference {
    pub fn span(&self) -> Span {
        Span::new(self.and_token.span().start, self.elem.span().end)
    }
}

impl HasSpan for TypeReference {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeSlice {
    pub bracket_token: Bracket,
    pub elem: Box<Type>,
}

impl TypeSlice {
    pub fn span(&self) -> Span {
        self.bracket_token.span()
    }
}

impl HasSpan for TypeSlice {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeTraitObject {
    pub dyn_token: TokenSpan,
    pub bounds: Punctuated<TypeParamBound>,
}

impl TypeTraitObject {
    pub fn span(&self) -> Span {
        self.dyn_token.span()
    }
}

impl HasSpan for TypeTraitObject {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct TypeParamBound {
    pub name: IdentSpan,
}



#[derive(Debug)]
pub struct TypeTuple {
    pub paren_token: Paren,
    pub elems: Punctuated<Type>,
}

impl TypeTuple {
    pub fn span(&self) -> Span {
        self.paren_token.span()
    }
}

impl HasSpan for TypeTuple {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum Stmt {
    /// 表达式语句：`x + 1;`
    Expr(Expr),
    /// 变量绑定：`let x = 42;` 或 `let mut y = 0;`
    Let(LetStmt),
    /// Item definition.
    Item(Item),
    /// 空语句：`;`
    Empty,
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expr(expr) => expr.span(),
            Stmt::Let(let_stmt) => {
                Span::new(let_stmt.let_token.span().start, let_stmt.semi_token.span().end)
            }
            Stmt::Item(item) => match item {
                Item::Enum(item) => Span::new(item.vis.span().start, item.brace_token.span().end),
                Item::Fn(item) => Span::new(item.vis.span().start, item.block.span().end),
                Item::Impl(item) => Span::new(item.impl_token.span().start, item.brace_token.span().end),
                Item::Struct(item) => {
                    if let Some(semi) = &item.semi_token {
                        Span::new(item.vis.span().start, semi.span().end)
                    } else {
                        match &item.fields {
                            Fields::Named(fields) => Span::new(item.vis.span().start, fields.brace_token.span().end),
                            Fields::Unnamed(fields) => Span::new(item.vis.span().start, fields.paren_token.span().end),
                            Fields::Unit => item.struct_token.span(),
                        }
                    }
                }
                Item::Type(item) => Span::new(item.vis.span().start, item.semi_token.span().end),
                Item::Use(item) => Span::new(item.vis.span().start, item.semi_token.span().end),
            },
            Stmt::Empty => Span::default(),
        }
    }
}

impl HasSpan for Stmt {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct LetStmt {
    pub let_token: TokenSpan,
    pub pat: Pat,
    pub init: Option<LocalInit>,
    pub semi_token: TokenSpan,
}

impl LetStmt {
    pub fn span(&self) -> Span {
        Span::new(self.let_token.span().start, self.semi_token.span().end)
    }
}

impl HasSpan for LetStmt {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct LocalInit {
    pub eq_token: TokenSpan,
    pub expr: Expr,
}

impl LocalInit {
    pub fn span(&self) -> Span {
        Span::new(self.eq_token.span().start, self.expr.span().end)
    }
}

impl HasSpan for LocalInit {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub enum Visibility {
    Public(TokenSpan),
    Private(TokenSpan),
    Inherited,
}

impl Visibility {
    pub fn span(&self) -> Span {
        match self {
            Visibility::Public(span) => span.span(),
            Visibility::Private(span) => span.span(),
            Visibility::Inherited => Span::default(),
        }
    }
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

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Enum(item) => Span::new(item.vis.span().start, item.brace_token.span().end),
            Item::Fn(item) => Span::new(item.vis.span().start, item.block.span().end),
            Item::Impl(item) => Span::new(item.impl_token.span().start, item.brace_token.span().end),
            Item::Struct(item) => {
                if let Some(semi) = &item.semi_token {
                    Span::new(item.vis.span().start, semi.span().end)
                } else {
                    match &item.fields {
                        Fields::Named(fields) => Span::new(item.vis.span().start, fields.brace_token.span().end),
                        Fields::Unnamed(fields) => Span::new(item.vis.span().start, fields.paren_token.span().end),
                        Fields::Unit => item.struct_token.span(),
                    }
                }
            }
            Item::Type(item) => Span::new(item.vis.span().start, item.semi_token.span().end),
            Item::Use(item) => Span::new(item.vis.span().start, item.semi_token.span().end),
        }
    }
}

impl HasSpan for Item {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct ItemEnum {
    pub vis: Visibility,
    pub enum_token: TokenSpan,
    pub name: IdentSpan,
    pub brace_token: Brace,
    pub variants: Punctuated<Variant>,
}

impl ItemEnum {
    pub fn span(&self) -> Span {
        Span::new(self.vis.span().start, self.brace_token.span().end)
    }
}

impl HasSpan for ItemEnum {
    fn span(&self) -> Span {
        self.span()
    }
}

#[derive(Debug)]
pub struct Variant {
    pub name: IdentSpan,
    pub fields: Fields,
    pub discriminant: Option<(TokenSpan, Expr)>,
}

#[derive(Debug)]
pub struct ItemFn {
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug)]
pub struct Signature {
    pub fn_token: TokenSpan,
    pub name: IdentSpan,
    pub paren_token: Paren,
    pub inputs: Punctuated<FnArg>,
    pub output: RetureType,
}

#[derive(Debug)]
pub enum FnArg {
    Receiver(Receiver),
    Typed(PatType),
}

#[derive(Debug)]
pub struct Receiver {
    pub and_token: Option<TokenSpan>,
    pub self_token: TokenSpan,
}

#[derive(Debug)]
pub struct ItemImpl {
    pub impl_token: TokenSpan,
    pub trait_: Option<(Path, TokenSpan)>, // `From for`
    pub self_ty: Box<Type>,
    pub brace_token: Brace,
    pub items: Vec<ImplItem>,
}

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
    pub ty: Type,
    pub eq_token: TokenSpan,
    pub expr: Expr,
    pub semi_token: TokenSpan,
}

#[derive(Debug)]
pub struct ImplItemFn {
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Block,
}

#[derive(Debug)]
pub struct ItemStruct {
    pub vis: Visibility,
    pub struct_token: TokenSpan,
    pub name: IdentSpan,
    pub fields: Fields,
    pub semi_token: Option<TokenSpan>,
}

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
    pub vis: Visibility,
    pub name: Option<IdentSpan>,
    pub colon_token: Option<TokenSpan>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct ItemType {
    pub vis: Visibility,
    pub type_token: TokenSpan,
    pub ident: IdentSpan,
    pub eq_token: TokenSpan,
    pub ty: Type,
    pub semi_token: TokenSpan,
}

#[derive(Debug)]
pub struct ItemUse {
    pub vis: Visibility,
    pub use_token: TokenSpan,
    pub leading_colon: Option<TokenSpan>, // `::`
    pub tree: UseTree,
    pub semi_token: TokenSpan,
}

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
    pub tree: Box<UseTree>,
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
    pub items: Punctuated<UseTree>,
}
