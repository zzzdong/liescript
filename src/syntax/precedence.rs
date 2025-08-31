use std::ops::Sub;

use crate::{lexical::Token, syntax::UnOp};

use super::operators::BinOp;

/// 运算符优先级表 (参考Rust语言规范)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Precedence {
    None = 0,
    Assignment = 10,      // = += -= *= /= %= &= |= ^= <<= >>=
    Range = 20,           // .. ..=
    LogicalOr = 30,       // ||
    LogicalAnd = 40,      // &&
    BitwiseOr = 50,       // |
    BitwiseXor = 60,      // ^
    BitwiseAnd = 70,      // &
    Eqity = 80,           // == !=
    Comparison = 90,      // < > <= >=
    Shift = 100,          // << >>
    Additive = 110,       // + -
    Multiplicative = 120, // * / %
    Cast = 130,           // as
    Unary = 140,          // ! - * &
    Try = 150,            // ?
    Call = 160,           // Function calls, array indexing
    Field = 170,          // Field expressions
    MethodCall = 180,     // Method calls
    Path = 190,           // Path expressions
    Primary = 200,
}

impl Precedence {
    pub fn for_binop(op: &BinOp) -> Self {
        use super::operators::BinOp::*;
        match op {
            Add | Sub => Precedence::Additive,
            Mul | Div | Rem => Precedence::Multiplicative,
            LogicAnd => Precedence::LogicalAnd,
            LogicOr => Precedence::LogicalOr,
            BitAnd => Precedence::BitwiseAnd,
            BitOr => Precedence::BitwiseOr,
            BitXor => Precedence::BitwiseXor,
            BitShl | BitShr => Precedence::Shift,
            Eq | NotEq => Precedence::Eqity,
            LessThen | LessThenOrEq | GreaterThen | GreaterThenOrEq => Precedence::Comparison,
            Assign | AddAssign | SubAssign | MulAssign | DivAssign | RemAssign => {
                Precedence::Assignment
            }
            Range | RangeInclusive => Precedence::Range,
            Cast => Precedence::Cast,
            _ => Precedence::None,
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
            8 => Precedence::Eqity,
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

impl Precedence {
    /// 获取运算符的绑定力对(左绑定力, 右绑定力)
    pub fn binding_powers(&self) -> (u8, u8) {
        match self {
            // 左结合运算符: (prec, prec + 1)
            Precedence::Additive | Precedence::Multiplicative |
            Precedence::BitwiseAnd | Precedence::BitwiseOr | Precedence::BitwiseXor |
            Precedence::Shift | Precedence::Cast | Precedence::Call | 
            Precedence::Field | Precedence::MethodCall | Precedence::Path => {
                (*self as u8, (*self as u8).saturating_add(1))
            }
            // 右结合运算符: (prec - 1, prec)
            Precedence::Assignment | Precedence::LogicalOr | Precedence::LogicalAnd |
            Precedence::Try => {
                ((*self as u8).saturating_sub(1), *self as u8)
            }
            // 非结合运算符: (prec, prec)
            Precedence::Range | Precedence::Eqity | Precedence::Comparison => {
                (*self as u8, *self as u8)
            }
            // 一元运算符: (prec, prec)
            Precedence::Unary => (*self as u8, *self as u8),
            _ => (0, 0)
        }
    }

    /// 直接从Token获取绑定力对(左绑定力, 右绑定力)
    pub fn from_token(token: &Token) -> Option<(u8, u8)> {
        use crate::lexical::{Keyword, Symbol};
        use crate::syntax::operators::BinOp;

        match token {
            // 二元运算符
            Token::Symbol(sym) => {
                // 一元运算符
                if let Some(op) = UnOp::from_token(token) {
                    return Some(Precedence::Unary.binding_powers());
                }

                if let Some(op) = BinOp::from_token(token) {
                    return Some(Precedence::for_binop(&op).binding_powers());
                }
                
                match sym {
                    Symbol::LParen | Symbol::LBracket | Symbol::Dot => {
                        Some(Precedence::Call.binding_powers())
                    }
                    Symbol::Question => Some(Precedence::Try.binding_powers()),
                    Symbol::DotDot => Some(Precedence::Range.binding_powers()),
                    Symbol::DotDotEq => Some(Precedence::Range.binding_powers()),
                    _ => None,
                }
            }
            // 类型转换运算符
            Token::Keyword(Keyword::As) => Some(Precedence::Cast.binding_powers()),
            _ => None,
        }
    }
}
