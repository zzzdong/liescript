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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BinOp {
    Num(NumOp),
    Bit(BitOp),
    Comp(CompOp),
    Log(LogOp),
    Assign(AssignOp),
    Range(RangeOp),
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
            _ => Err("unknown bin op"),
        }
    }
}
