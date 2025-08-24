use std::ops::Sub;

use super::op::BinOp;

/// 运算符优先级表 (参考Rust语言规范)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment = 1,    // = += -= *= /= %= &= |= ^= <<= >>=
    Range = 2,        // .. ..=
    LogicalOr = 3,    // ||
    LogicalAnd = 4,   // &&
    BitwiseOr = 5,    // |
    BitwiseXor = 6,   // ^
    BitwiseAnd = 7,   // &
    Equality = 8,     // == !=
    Comparison = 9,   // < > <= >=
    Shift = 10,       // << >>
    Additive = 11,    // + -
    Multiplicative = 12, // * / %
    Unary = 13,       // ! - * & 
    Call = 14,        // () [] . ??
    Primary = 15,
}

impl Precedence {
    pub fn for_binop(op: &BinOp) -> Self {
        use super::op::BinOp::*;
        match op {
            Add | Sub => Precedence::Additive,
            Mul | Div | Rem => Precedence::Multiplicative,
            LogicAnd => Precedence::LogicalAnd,
            LogicOr => Precedence::LogicalOr,
            BitAnd => Precedence::BitwiseAnd,
            BitOr => Precedence::BitwiseOr,
            BitXor => Precedence::BitwiseXor,
            BitShl | BitShr => Precedence::Shift,
            Equal | NotEqual => Precedence::Equality,
            LessThen | LessThenOrEqual | GreaterThen | GreaterThenOrEqual => Precedence::Comparison,
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => Precedence::Assignment,
            Range | RangeInclusive => Precedence::Range,
        }
    }
}

impl From<u8> for Precedence {
    fn from(value: u8) -> Self {
        match value {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Range,
            3 => Precedence::LogicalOr,
            4 => Precedence::LogicalAnd,
            5 => Precedence::BitwiseOr,
            6 => Precedence::BitwiseXor,
            7 => Precedence::BitwiseAnd,
            8 => Precedence::Equality,
            9 => Precedence::Comparison,
            10 => Precedence::Shift,
            11 => Precedence::Additive,
            12 => Precedence::Multiplicative,
            13 => Precedence::Unary,
            14 => Precedence::Call,
            15 => Precedence::Primary,
            _ => Precedence::None,
        }
    }
}

impl Sub<u8> for Precedence {
    type Output = Self;

    fn sub(self, rhs: u8) -> Self::Output {
        Self::from(self as u8 - rhs)
    }
}
