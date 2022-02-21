use std::fmt;

use super::punctuation::Punctuation;

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

            pub fn from_punctuation(p: Punctuation) -> Result<Self, &'static str> {
                match p {
                    $($punc => Ok($def::$name),)*
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
);

define_op!(PostfixOp,
    "?" => (Question, Question),
);

define_op!(NumOp,
    "+" => (Add, Plus),
    "-" => (Sub, Minus),
    "*" => (Mul, Star),
    "/" => (Div, Slash),
    "%" => (Mod, Percent),
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
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BinOp {
    Num(NumOp),
    Bit(BitOp),
    Comp(CompOp),
    Log(LogOp),
    Assign(AssignOp),
    Range(RangeOp),
    Access(AccessOp),
}

impl BinOp {
    pub fn from_punctuation(p: Punctuation) -> Result<Self, &'static str> {
        match p {
            // num op
            Punctuation::Plus => Ok(BinOp::Num(NumOp::Add)),
            Punctuation::Minus => Ok(BinOp::Num(NumOp::Sub)),
            Punctuation::Star => Ok(BinOp::Num(NumOp::Mul)),
            Punctuation::Slash => Ok(BinOp::Num(NumOp::Div)),
            Punctuation::Percent => Ok(BinOp::Num(NumOp::Mod)),
            // bit op
            Punctuation::And => Ok(BinOp::Bit(BitOp::And)),
            Punctuation::Or => Ok(BinOp::Bit(BitOp::Or)),
            Punctuation::Caret => Ok(BinOp::Bit(BitOp::Xor)),
            Punctuation::LShift => Ok(BinOp::Bit(BitOp::Shl)),
            Punctuation::RShift => Ok(BinOp::Bit(BitOp::Shr)),
            // comp op
            Punctuation::EqEq => Ok(BinOp::Comp(CompOp::Equal)),
            Punctuation::NotEq => Ok(BinOp::Comp(CompOp::NotEqual)),
            Punctuation::Lt => Ok(BinOp::Comp(CompOp::LessThan)),
            Punctuation::LtE => Ok(BinOp::Comp(CompOp::LessThanOrEqual)),
            Punctuation::Gt => Ok(BinOp::Comp(CompOp::GreatThan)),
            Punctuation::GtE => Ok(BinOp::Comp(CompOp::GreatThanOrEqual)),
            // logic op
            Punctuation::AndAnd => Ok(BinOp::Log(LogOp::And)),
            Punctuation::OrOr => Ok(BinOp::Log(LogOp::Or)),
            // assign op
            Punctuation::Eq => Ok(BinOp::Assign(AssignOp::Assign)),
            Punctuation::PlusEq => Ok(BinOp::Assign(AssignOp::Add)),
            Punctuation::MinusEq => Ok(BinOp::Assign(AssignOp::Sub)),
            Punctuation::StarEq => Ok(BinOp::Assign(AssignOp::Mul)),
            Punctuation::SlashEq => Ok(BinOp::Assign(AssignOp::Div)),
            Punctuation::PercentEq => Ok(BinOp::Assign(AssignOp::Mod)),
            // range op
            Punctuation::DotDot => Ok(BinOp::Range(RangeOp::Range)),
            Punctuation::DotDotEq => Ok(BinOp::Range(RangeOp::RangeTo)),
            // field access
            Punctuation::Dot => Ok(BinOp::Access(AccessOp::Field)),
            Punctuation::PathSep => Ok(BinOp::Access(AccessOp::Path)),
            _ => Err("unknown bin op"),
        }
    }

    pub fn kind(&self) -> OpKind {
        match self {
            BinOp::Num(_op) => OpKind::Num,
            BinOp::Bit(_op) => OpKind::Bit,
            BinOp::Comp(_op) => OpKind::Comp,
            BinOp::Log(_op) => OpKind::Log,
            BinOp::Assign(_op) => OpKind::Assign,
            BinOp::Range(_op) => OpKind::Range,
            BinOp::Access(_op) => OpKind::Access,
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Num(op) => write!(f, "{}", op.as_str()),
            BinOp::Bit(op) => write!(f, "{}", op.as_str()),
            BinOp::Comp(op) => write!(f, "{}", op.as_str()),
            BinOp::Log(op) => write!(f, "{}", op.as_str()),
            BinOp::Assign(op) => write!(f, "{}", op.as_str()),
            BinOp::Range(op) => write!(f, "{}", op.as_str()),
            BinOp::Access(op) => write!(f, "{}", op.as_str()),
        }
    }
}
