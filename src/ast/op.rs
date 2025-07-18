use std::fmt;

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
                match self {
                    $($def::$name => write!(f, $str),)*
                }
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

define_op!(NumOp,
    "+" => (Add, Plus),
    "-" => (Sub, Minus),
    "*" => (Mul, Star),
    "/" => (Div, Slash),
    "%" => (Rem, Percent),
);

define_op!(BitOp,
    "&" => (And, And),
    "|" => (Or, Or),
    "^" => (Xor, Caret),
    "<<" => (Shl, LShift),
    ">>" => (Shr, RShift),
);

define_op!(CompOp,
    "==" => (Equal, EqEq),
    "!=" => (NotEqual, NotEq),
    "<" => (LessThan, Lt),
    "<=" => (LessThanOrEqual, LtE),
    ">" => (GreatThan, Gt),
    ">=" => (GreatThanOrEqual, GtE),
);

define_op!(LogOp,
    "&&" => (And, AndAnd),
    "||" => (Or, OrOr),
);

define_op!(AssignOp,
    "=" => (Assign, Eq),
    "+=" => (Add, PlusEq),
    "-=" => (Sub, MinusEq),
    "*=" => (Mul, StarEq),
    "/=" => (Div, SlashEq),
    "%=" => (Mod, PercentEq),
);

define_op!(RangeOp,
    ".." => (Range, DotDot),
    "..=" => (RangeTo, DotDotEq),
);

define_op!(AccessOp,
    "." => (Field, Dot),
    "::=" => (Path, PathSep),
);

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
    Shl,
    Shr,
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
    pub fn from_symbol(p: Symbol) -> Result<Self, &'static str> {
        match p {
            // num op
            Symbol::Plus => Ok(BinOp::Add),
            Symbol::Minus => Ok(BinOp::Sub),
            Symbol::Star => Ok(BinOp::Mul),
            Symbol::Slash => Ok(BinOp::Div),
            Symbol::Percent => Ok(BinOp::Rem),
            // bit op
            Symbol::And => Ok(BinOp::BitAnd),
            Symbol::Or => Ok(BinOp::BitOr),
            Symbol::Caret => Ok(BinOp::BitXor),
            Symbol::LShift => Ok(BinOp::Shl),
            Symbol::RShift => Ok(BinOp::Shr),
            // comp op
            Symbol::EqEq => Ok(BinOp::Equal),
            Symbol::NotEq => Ok(BinOp::NotEqual),
            Symbol::Lt => Ok(BinOp::LessThen),
            Symbol::LtE => Ok(BinOp::LessThenOrEqual),
            Symbol::Gt => Ok(BinOp::GreaterThen),
            Symbol::GtE => Ok(BinOp::GreaterThenOrEqual),
            // logic op
            Symbol::AndAnd => Ok(BinOp::LogicAnd),
            Symbol::OrOr => Ok(BinOp::LogicOr),
            // assign op
            Symbol::Eq => Ok(BinOp::Assign),
            Symbol::PlusEq => Ok(BinOp::AddAssign),
            Symbol::MinusEq => Ok(BinOp::SubAssign),
            Symbol::StarEq => Ok(BinOp::MulAssign),
            Symbol::SlashEq => Ok(BinOp::DivAssign),
            Symbol::PercentEq => Ok(BinOp::RemAssign),
            // range op
            Symbol::DotDot => Ok(BinOp::Range),
            Symbol::DotDotEq => Ok(BinOp::RangeInclusive),
            // field access
            Symbol::Dot => Ok(BinOp::MemberAccess),
            Symbol::PathSep => Ok(BinOp::Path),
            // cast op
            _ => Err("unknown bin op"),
        }
    }

    pub fn kind(&self) -> OpKind {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => OpKind::Num,
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::Shl | BinOp::Shr => OpKind::Bit,
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

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Rem => write!(f, "%"),
            BinOp::BitAnd => write!(f, "&"),
            BinOp::BitOr => write!(f, "|"),
            BinOp::BitXor => write!(f, "^"),
            BinOp::Shl => write!(f, "<<"),
            BinOp::Shr => write!(f, ">>"),
            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::LessThen => write!(f, "<"),
            BinOp::LessThenOrEqual => write!(f, "<="),
            BinOp::GreaterThen => write!(f, ">"),
            BinOp::GreaterThenOrEqual => write!(f, ">="),
            BinOp::LogicAnd => write!(f, "&&"),
            BinOp::LogicOr => write!(f, "||"),
            BinOp::Assign => write!(f, "="),
            BinOp::AddAssign => write!(f, "+="),
            BinOp::SubAssign => write!(f, "-="),
            BinOp::MulAssign => write!(f, "*="),
            BinOp::DivAssign => write!(f, "/="),
            BinOp::RemAssign => write!(f, "%="),
            BinOp::Range => write!(f, ".."),
            BinOp::RangeInclusive => write!(f, "..="),
            BinOp::MemberAccess => write!(f, "."),
            BinOp::Path => write!(f, "::"),
            BinOp::Cast => write!(f, "as"),
        }
    }
}
