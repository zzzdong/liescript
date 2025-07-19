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

define_op!(BinOp,
    "+" => (Add, Plus),
    "-" => (Sub, Minus),
    "*" => (Mul, Star),
    "/" => (Div, Slash),
    "%" => (Rem, Percent),
    "&" => (BitAnd, And),
    "|" => (BitOr, Or),
    "^" => (BitXor, Caret),
    "<<" => (BitShl, LShift),
    ">>" => (BitShr, RShift),
    "==" => (Equal, Equal),
    "!=" => (NotEqual, NotEqual),
    "<" => (LessThen, LessThan),
    "<=" => (LessThenOrEqual, LessThenEqual),
    ">" => (GreaterThen, GreatThen),
    ">=" => (GreaterThenOrEqual, GreatThenEqual),
    "&&" => (LogicAnd, AndAnd),
    "||" => (LogicOr, OrOr),
    "=" => (Assign, Equal),
    "+=" => (AddAssign, PlusEqual),
    "-=" => (SubAssign, MinusEqual),
    "*=" => (MulAssign, StarEqual),
    "/=" => (DivAssign, SlashEqual),
    "%=" => (RemAssign, PercentEqual),
    ".." => (Range, DotDot),
    "..=" => (RangeInclusive, DotDotEq),
    "." => (MemberAccess, Dot),
    "::" => (Path, ColonColon),
    "as" => (Cast, As),
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
