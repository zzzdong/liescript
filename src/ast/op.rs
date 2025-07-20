use std::fmt;

use crate::ast::keyword::Keyword;
use crate::ast::symbol::Symbol;

macro_rules! define_op {
    (
        $def:ident,
        $(
            $str:expr => ($name:ident, $punc:ident),
        )*
    ) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub enum $def {
            $($name,)*
        }

        impl $def {
            pub const ALL: &'static [$def] = &[
                $($def::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=$def> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Self {
                match s {
                    $($str => $def::$name,)*
                    _ => {
                        unreachable!();
                    }
                }
            }

            pub fn from_symbol(p: Symbol) -> Result<Self, &'static str> {
                match p {
                    $(Symbol::$punc => Ok($def::$name),)*
                    _ => Err("unknown op"),
                }
            }

            pub fn as_str(self) -> &'static str {
                match self {
                    $($def::$name => $str,)*
                }
            }
        }

        impl fmt::Display for $def {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.as_str())
            }
        }
    };
}

define_op!(PrefixOp,
    "-" => (Neg, Minus),
    "!" => (Not, Not),
    "&" => (Ref, And),
    "*" => (Deref, Star),
);

define_op!(PostfixOp,
    "?" => (Try, Question),
);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    Equal,
    NotEqual,
    LessThen,
    LessThenOrEqual,
    GreaterThen,
    GreaterThenOrEqual,
    LogicAnd,
    LogicOr,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    Range,
    RangeInclusive,
    MemberAccess,
    Path,
    Cast,
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
        BinOp::Equal,
        BinOp::NotEqual,
        BinOp::LessThen,
        BinOp::LessThenOrEqual,
        BinOp::GreaterThen,
        BinOp::GreaterThenOrEqual,
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
        BinOp::MemberAccess,
        BinOp::Path,
        BinOp::Cast,
    ];

    pub const STRS: &'static [&'static str] = &[
        "+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>", "==", "!=", "<", "<=", ">", ">=", "&&",
        "||", "=", "+=", "-=", "*=", "/=", "%=", "..", "..=", ".", "::", "as",
    ];

    pub fn all() -> impl Iterator<Item = BinOp> {
        Self::ALL.iter().copied()
    }

    pub fn from_str(s: &str) -> Self {
        match s {
            "+" => BinOp::Add,
            "-" => BinOp::Sub,
            "*" => BinOp::Mul,
            "/" => BinOp::Div,
            "%" => BinOp::Rem,
            "&" => BinOp::BitAnd,
            "|" => BinOp::BitOr,
            "^" => BinOp::BitXor,
            "<<" => BinOp::BitShl,
            ">>" => BinOp::BitShr,
            "==" => BinOp::Equal,
            "!=" => BinOp::NotEqual,
            "<" => BinOp::LessThen,
            "<=" => BinOp::LessThenOrEqual,
            ">" => BinOp::GreaterThen,
            ">=" => BinOp::GreaterThenOrEqual,
            "&&" => BinOp::LogicAnd,
            "||" => BinOp::LogicOr,
            "=" => BinOp::Assign,
            "+=" => BinOp::AddAssign,
            "-=" => BinOp::SubAssign,
            "*=" => BinOp::MulAssign,
            "/=" => BinOp::DivAssign,
            "%=" => BinOp::RemAssign,
            ".." => BinOp::Range,
            "..=" => BinOp::RangeInclusive,
            "." => BinOp::MemberAccess,
            "::" => BinOp::Path,
            "as" => BinOp::Cast,
            _ => unreachable!(),
        }
    }

    pub fn from_symbol(p: Symbol) -> Result<Self, &'static str> {
        match p {
            Symbol::Plus => Ok(BinOp::Add),
            Symbol::Minus => Ok(BinOp::Sub),
            Symbol::Star => Ok(BinOp::Mul),
            Symbol::Slash => Ok(BinOp::Div),
            Symbol::Percent => Ok(BinOp::Rem),
            Symbol::And => Ok(BinOp::BitAnd),
            Symbol::Or => Ok(BinOp::BitOr),
            Symbol::Caret => Ok(BinOp::BitXor),
            Symbol::LShift => Ok(BinOp::BitShl),
            Symbol::RShift => Ok(BinOp::BitShr),
            Symbol::EqualEqual => Ok(BinOp::Equal),
            Symbol::NotEqual => Ok(BinOp::NotEqual),
            Symbol::LessThan => Ok(BinOp::LessThen),
            Symbol::LessThenEqual => Ok(BinOp::LessThenOrEqual),
            Symbol::GreatThen => Ok(BinOp::GreaterThen),
            Symbol::GreatThenEqual => Ok(BinOp::GreaterThenOrEqual),
            Symbol::AndAnd => Ok(BinOp::LogicAnd),
            Symbol::OrOr => Ok(BinOp::LogicOr),
            Symbol::Equal => Ok(BinOp::Assign),
            Symbol::PlusEqual => Ok(BinOp::AddAssign),
            Symbol::MinusEqual => Ok(BinOp::SubAssign),
            Symbol::StarEqual => Ok(BinOp::MulAssign),
            Symbol::SlashEqual => Ok(BinOp::DivAssign),
            Symbol::PercentEqual => Ok(BinOp::RemAssign),
            Symbol::DotDot => Ok(BinOp::Range),
            Symbol::DotDotEqual => Ok(BinOp::RangeInclusive),
            Symbol::Dot => Ok(BinOp::MemberAccess),
            Symbol::ColonColon => Ok(BinOp::Path),
            _ => Err("unknown op"),
        }
    }

    pub fn from_keyword(p: Keyword) -> Result<Self, &'static str> {
        match p {
            Keyword::As => Ok(BinOp::Cast),
            _ => Err("unknown op"),
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
            BinOp::Equal => "==",
            BinOp::NotEqual => "!=",
            BinOp::LessThen => "<",
            BinOp::LessThenOrEqual => "<=",
            BinOp::GreaterThen => ">",
            BinOp::GreaterThenOrEqual => ">=",
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
            BinOp::MemberAccess => ".",
            BinOp::Path => "::",
            BinOp::Cast => "as",
        }
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
    Cast,
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
            BinOp::Equal
            | BinOp::NotEqual
            | BinOp::LessThen
            | BinOp::LessThenOrEqual
            | BinOp::GreaterThen
            | BinOp::GreaterThenOrEqual => OpKind::Comp,
            BinOp::LogicAnd | BinOp::LogicOr => OpKind::Log,
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::DivAssign
            | BinOp::RemAssign => OpKind::Assign,
            BinOp::Range | BinOp::RangeInclusive => OpKind::Range,
            BinOp::MemberAccess => OpKind::Access,
            BinOp::Path => OpKind::Path,
            BinOp::Cast => OpKind::Cast,
        }
    }
}
