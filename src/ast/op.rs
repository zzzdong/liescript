use std::fmt;

use super::Symbol;

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
    Cast,
    Decl,
    Path,
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
    CastOp,
    DeclOp,
    PathOp,
}

impl BinOp {
    pub fn from_symbol(p: Symbol) -> Result<Self, &'static str> {
        match p {
            // num op
            Symbol::Plus => Ok(BinOp::Num(NumOp::Add)),
            Symbol::Minus => Ok(BinOp::Num(NumOp::Sub)),
            Symbol::Star => Ok(BinOp::Num(NumOp::Mul)),
            Symbol::Slash => Ok(BinOp::Num(NumOp::Div)),
            Symbol::Percent => Ok(BinOp::Num(NumOp::Mod)),
            // bit op
            Symbol::And => Ok(BinOp::Bit(BitOp::And)),
            Symbol::Or => Ok(BinOp::Bit(BitOp::Or)),
            Symbol::Caret => Ok(BinOp::Bit(BitOp::Xor)),
            Symbol::LShift => Ok(BinOp::Bit(BitOp::Shl)),
            Symbol::RShift => Ok(BinOp::Bit(BitOp::Shr)),
            // comp op
            Symbol::EqEq => Ok(BinOp::Comp(CompOp::Equal)),
            Symbol::NotEq => Ok(BinOp::Comp(CompOp::NotEqual)),
            Symbol::Lt => Ok(BinOp::Comp(CompOp::LessThan)),
            Symbol::LtE => Ok(BinOp::Comp(CompOp::LessThanOrEqual)),
            Symbol::Gt => Ok(BinOp::Comp(CompOp::GreatThan)),
            Symbol::GtE => Ok(BinOp::Comp(CompOp::GreatThanOrEqual)),
            // logic op
            Symbol::AndAnd => Ok(BinOp::Log(LogOp::And)),
            Symbol::OrOr => Ok(BinOp::Log(LogOp::Or)),
            // assign op
            Symbol::Eq => Ok(BinOp::Assign(AssignOp::Assign)),
            Symbol::PlusEq => Ok(BinOp::Assign(AssignOp::Add)),
            Symbol::MinusEq => Ok(BinOp::Assign(AssignOp::Sub)),
            Symbol::StarEq => Ok(BinOp::Assign(AssignOp::Mul)),
            Symbol::SlashEq => Ok(BinOp::Assign(AssignOp::Div)),
            Symbol::PercentEq => Ok(BinOp::Assign(AssignOp::Mod)),
            // range op
            Symbol::DotDot => Ok(BinOp::Range(RangeOp::Range)),
            Symbol::DotDotEq => Ok(BinOp::Range(RangeOp::RangeTo)),
            // field access
            Symbol::Dot => Ok(BinOp::Access(AccessOp::Field)),
            Symbol::PathSep => Ok(BinOp::Access(AccessOp::Path)),
            // type decl
            Symbol::Colon => Ok(BinOp::DeclOp),
            // path op
            Symbol::PathSep => Ok(BinOp::PathOp),
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
            BinOp::CastOp => OpKind::Cast,
            BinOp::DeclOp => OpKind::Decl,
            BinOp::PathOp => OpKind::Path,
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
            BinOp::CastOp => write!(f, "as"),
            BinOp::DeclOp => write!(f, ":"),
            BinOp::PathOp => write!(f, "::"),
        }
    }
}
