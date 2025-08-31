use std::fmt;

use crate::diagnostic::Spanned;
use crate::lexical::{Brace, Bracket, IdentSpan, Keyword, LiteralSpan, Paren, Punctuated, Symbol, Token, TokenSpan};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum UnOp {
    Neg,    // -
    Not,    // !
    Deref,  // *
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BinOp {
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Rem,    // %
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    BitShl, // <<
    BitShr, // >>
    Eq,     // ==
    NotEq,  // !=
    LessThen, // <
    LessThenOrEq, // <=
    GreaterThen, // >
    GreaterThenOrEq, // >=
    LogicAnd, // &&
    LogicOr, // ||
    Assign, // =
    AddAssign, // +=
    SubAssign, // -=
    MulAssign, // *=
    DivAssign, // /=
    RemAssign, // %=
    Range,  // ..
    RangeInclusive, // ..=
    Cast,   // as
}

pub type UnOpSpan = Spanned<UnOp>;
pub type BinOpSpan = Spanned<BinOp>;

impl UnOp {
    pub const ALL: &'static [UnOp] = &[
        UnOp::Neg,
        UnOp::Not,
        UnOp::Deref,
    ];

    pub const STRS: &'static [&'static str] = &[
        "-", "!", "*"
    ];

    pub fn all() -> impl Iterator<Item=UnOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Symbol(s) => match s.as_str() {
                "-" => Some(UnOp::Neg),
                "!" => Some(UnOp::Not),
                "*" => Some(UnOp::Deref),
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
        }
    }
}

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
        "+", "-", "*", "/", "%",
        "&", "|", "^", "<<", ">>",
        "==", "!=", "<", "<=", ">", ">=",
        "&&", "||", "=", "+=", "-=", "*=", "/=", "%=",
        "..", "..="
    ];

    pub fn all() -> impl Iterator<Item=BinOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Symbol(Symbol::Shl) => Some(BinOp::BitShl),
            Token::Symbol(Symbol::Shr) => Some(BinOp::BitShr),
            Token::Symbol(Symbol::EqEq) => Some(BinOp::Eq),
            Token::Symbol(Symbol::Ne) => Some(BinOp::NotEq),
            Token::Symbol(Symbol::Le) => Some(BinOp::LessThenOrEq),
            Token::Symbol(Symbol::Ge) => Some(BinOp::GreaterThenOrEq),
            Token::Symbol(Symbol::AndAnd) => Some(BinOp::LogicAnd),
            Token::Symbol(Symbol::OrOr) => Some(BinOp::LogicOr),
            Token::Symbol(Symbol::PlusEq) => Some(BinOp::AddAssign),
            Token::Symbol(Symbol::MinusEq) => Some(BinOp::SubAssign),
            Token::Symbol(Symbol::StarEq) => Some(BinOp::MulAssign),
            Token::Symbol(Symbol::SlashEq) => Some(BinOp::DivAssign),
            Token::Symbol(Symbol::PercentEq) => Some(BinOp::RemAssign),
            Token::Symbol(Symbol::DotDot) => Some(BinOp::Range),
            Token::Symbol(Symbol::DotDotEq) => Some(BinOp::RangeInclusive),
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
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for BinOp {
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
    Cast,  // 处理as转换
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
