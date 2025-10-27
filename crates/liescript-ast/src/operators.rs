use std::fmt;

use liescript_lexical::{ident::IdentSpan, keyword::Keyword, literal::{Literal, LiteralSpan}, symbol::Symbol, token::{Brace, Bracket, Paren, Punctuated, Token, TokenSpan}, HasSpan, Span, Spanned};


#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub enum UnOp {
    Neg,    // -
    Not,    // !
    Deref,  // *
    Borrow, // &
}

pub type UnOpSpan = Spanned<UnOp>;

impl UnOp {
    pub const ALL: &'static [UnOp] = &[UnOp::Neg, UnOp::Not, UnOp::Deref];

    pub const STRS: &'static [&'static str] = &["-", "!", "*"];

    pub fn all() -> impl Iterator<Item = UnOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Symbol(s) => match s.as_str() {
                "-" => Some(UnOp::Neg),
                "!" => Some(UnOp::Not),
                "*" => Some(UnOp::Deref),
                "&" => Some(UnOp::Borrow),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
            UnOp::Deref => "*",
            UnOp::Borrow => "&",
        }
    }

    pub fn binding_power(&self) -> u8 {
        use UnOp::*;

        match self {
            Neg | Not | Deref | Borrow => 130,
        }
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, Debug)]
pub enum BinOp {
    Add,             // +
    Sub,             // -
    Mul,             // *
    Div,             // /
    Rem,             // %
    BitAnd,          // &
    BitOr,           // |
    BitXor,          // ^
    BitShl,          // <<
    BitShr,          // >>
    Eq,              // ==
    NotEq,           // !=
    LessThen,        // <
    LessThenOrEq,    // <=
    GreaterThen,     // >
    GreaterThenOrEq, // >=
    LogicAnd,        // &&
    LogicOr,         // ||
    Assign,          // =
    AddAssign,       // +=
    SubAssign,       // -=
    MulAssign,       // *=
    DivAssign,       // /=
    RemAssign,       // %=
    Range,           // ..
    RangeInclusive,  // ..=
    Cast,            // as
}

pub type BinOpSpan = Spanned<BinOp>;

impl BinOp {
    pub const ALL: &'static [BinOp] = &[
        BinOp::Add,
        BinOp::Sub,
        BinOp::Mul,
        BinOp::Div,
        BinOp::Rem,
        BinOp::BitAnd,
        BinOp::BitOr,
        BinOp::BitXor,
        BinOp::BitShl,
        BinOp::BitShr,
        BinOp::Eq,
        BinOp::NotEq,
        BinOp::LessThen,
        BinOp::LessThenOrEq,
        BinOp::GreaterThen,
        BinOp::GreaterThenOrEq,
        BinOp::LogicAnd,
        BinOp::LogicOr,
        BinOp::Assign,
        BinOp::AddAssign,
        BinOp::SubAssign,
        BinOp::MulAssign,
        BinOp::DivAssign,
        BinOp::RemAssign,
        BinOp::Range,
        BinOp::RangeInclusive,
    ];

    pub const STRS: &'static [&'static str] = &[
        "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", "<=", ">", ">=", "&&",
        "||", "=", "+=", "-=", "*=", "/=", "%=", "..", "..=",
    ];

    pub fn all() -> impl Iterator<Item = BinOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Symbol(Symbol::Plus) => Some(BinOp::Add),
            Token::Symbol(Symbol::Minus) => Some(BinOp::Sub),
            Token::Symbol(Symbol::Star) => Some(BinOp::Mul),
            Token::Symbol(Symbol::Slash) => Some(BinOp::Div),
            Token::Symbol(Symbol::Percent) => Some(BinOp::Rem),
            Token::Symbol(Symbol::And) => Some(BinOp::BitAnd),
            Token::Symbol(Symbol::Or) => Some(BinOp::BitOr),
            Token::Symbol(Symbol::Caret) => Some(BinOp::BitXor),
            Token::Symbol(Symbol::Shl) => Some(BinOp::BitShl),
            Token::Symbol(Symbol::Shr) => Some(BinOp::BitShr),
            Token::Symbol(Symbol::EqEq) => Some(BinOp::Eq),
            Token::Symbol(Symbol::Ne) => Some(BinOp::NotEq),
            Token::Symbol(Symbol::Gt) => Some(BinOp::GreaterThen),
            Token::Symbol(Symbol::Lt) => Some(BinOp::LessThen),
            Token::Symbol(Symbol::Ge) => Some(BinOp::GreaterThenOrEq),
            Token::Symbol(Symbol::Le) => Some(BinOp::LessThenOrEq),
            Token::Symbol(Symbol::AndAnd) => Some(BinOp::LogicAnd),
            Token::Symbol(Symbol::OrOr) => Some(BinOp::LogicOr),
            Token::Symbol(Symbol::Eq) => Some(BinOp::Assign),
            Token::Symbol(Symbol::PlusEq) => Some(BinOp::AddAssign),
            Token::Symbol(Symbol::MinusEq) => Some(BinOp::SubAssign),
            Token::Symbol(Symbol::StarEq) => Some(BinOp::MulAssign),
            Token::Symbol(Symbol::SlashEq) => Some(BinOp::DivAssign),
            Token::Symbol(Symbol::PercentEq) => Some(BinOp::RemAssign),
            // Token::Symbol(Symbol::DotDot) => Some(BinOp::Range),
            // Token::Symbol(Symbol::DotDotEq) => Some(BinOp::RangeInclusive),
            Token::Keyword(Keyword::As) => Some(BinOp::Cast),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Rem => "%",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::BitShl => "<<",
            BinOp::BitShr => ">>",
            BinOp::Eq => "==",
            BinOp::NotEq => "!=",
            BinOp::LessThen => "<",
            BinOp::LessThenOrEq => "<=",
            BinOp::GreaterThen => ">",
            BinOp::GreaterThenOrEq => ">=",
            BinOp::LogicAnd => "&&",
            BinOp::LogicOr => "||",
            BinOp::Assign => "=",
            BinOp::AddAssign => "+=",
            BinOp::SubAssign => "-=",
            BinOp::MulAssign => "*=",
            BinOp::DivAssign => "/=",
            BinOp::RemAssign => "%=",
            BinOp::Range => "..",
            BinOp::RangeInclusive => "..=",
            BinOp::Cast => "as",
        }
    }

    pub fn get_binding_power(&self) -> (u8, u8) {
        use BinOp::*;

        match self {
            /* 最弱 10 级：右结合 */
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => (10, 11), // = += -= …  right

            /* 20 级：不结合（需括号）*/
            Range | RangeInclusive => (20, 19), // ..  ..=   non-assoc

            /* 30 级：左结合 */
            LogicOr => (30, 30), // ||

            /* 40 级：左结合 */
            LogicAnd => (40, 40), // &&

            /* 50 级：不结合（比较类）*/
            Eq | NotEq | LessThen | LessThenOrEq | GreaterThen | GreaterThenOrEq => (50, 49), // == != < … non-assoc

            /* 60 级：左结合 */
            BitOr => (60, 60), // |

            /* 70 级：左结合 */
            BitXor => (70, 70), // ^

            /* 80 级：左结合 */
            BitAnd => (80, 80), // &

            /* 90 级：左结合 */
            BitShl | BitShr => (90, 90), // <<  >>

            /* 100 级：左结合 */
            Add | Sub => (100, 100), // +  -

            /* 110 级：左结合 */
            Mul | Div | Rem => (110, 110), // *  /  %

            /* 120 级：左结合 */
            Cast => (120, 120), // as
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum PostfixOp {
    FieldAccess, // field access
    Index,       // array index
    Call,        // call
    Try,         // try
}

impl PostfixOp {
    pub const ALL: &'static [PostfixOp] = &[
        PostfixOp::FieldAccess,
        PostfixOp::Index,
        PostfixOp::Call,
        PostfixOp::Try,
    ];

    pub const STRS: &'static [&'static str] = &[".", "[", "(", "?"];

    pub fn all() -> impl Iterator<Item = PostfixOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Symbol(Symbol::Dot) => Some(PostfixOp::FieldAccess),
            Token::Symbol(Symbol::LBracket) => Some(PostfixOp::Index),
            Token::Symbol(Symbol::LParen) => Some(PostfixOp::Call),
            Token::Symbol(Symbol::Question) => Some(PostfixOp::Try),
            _ => None,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            PostfixOp::FieldAccess => ".",
            PostfixOp::Index => "[",
            PostfixOp::Call => "(",
            PostfixOp::Try => "?",
        }
    }
}

impl fmt::Display for PostfixOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum OpKind {
    Num,
    Bit,
    Comp,
    Log,
    Assign,
    Range,
    Access,
    Cast, // 处理as转换
    Decl,
    Path,
}

impl BinOp {
    pub fn kind(&self) -> OpKind {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => OpKind::Num,
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::BitShl | BinOp::BitShr => {
                OpKind::Bit
            }
            BinOp::Eq
            | BinOp::NotEq
            | BinOp::LessThen
            | BinOp::LessThenOrEq
            | BinOp::GreaterThen
            | BinOp::GreaterThenOrEq => OpKind::Comp,
            BinOp::LogicAnd | BinOp::LogicOr => OpKind::Log,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign
            | BinOp::RemAssign => OpKind::Assign,
            BinOp::Range | BinOp::RangeInclusive => OpKind::Range,
            BinOp::Cast => OpKind::Cast,
        }
    }
}
