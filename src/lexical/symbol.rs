use crate::diagnostic::Spanned;
use std::{fmt, str::FromStr};

pub type SymbolSpan = Spanned<Symbol>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Symbol {
    // 单字符标点
    Eq,         // =
    Lt,         // <
    Gt,         // >
    Not,        // !
    Tilde,      // ~
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    And,        // &
    Or,         // |
    At,         // @
    Dot,        // .
    Comma,      // ,
    Semi,       // ;
    Colon,      // :
    Pound,      // #
    Dollar,     // $
    Question,   // ?
    Underscore, // _
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    LParen,     // (
    RParen,     // )

    // 多字符标点
    Le,         // <=
    EqEq,       // ==
    Ne,         // !=
    Ge,         // >=
    AndAnd,     // &&
    OrOr,       // ||
    Shl,        // <<
    Shr,        // >>
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=
    CaretEq,    // ^=
    AndEq,      // &=
    OrEq,       // |=
    ShlEq,      // <<=
    ShrEq,      // >>=
    DotDot,     // ..
    DotDotDot,  // ...
    DotDotEq,   // ..=
    ColonColon, // ::
    RArrow,     // ->
    LArrow,     // <-
    FatArrow,   // =>
}

impl Symbol {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            // 单字符
            "=" => Some(Symbol::Eq),
            "<" => Some(Symbol::Lt),
            ">" => Some(Symbol::Gt),
            "!" => Some(Symbol::Not),
            "~" => Some(Symbol::Tilde),
            "+" => Some(Symbol::Plus),
            "-" => Some(Symbol::Minus),
            "*" => Some(Symbol::Star),
            "/" => Some(Symbol::Slash),
            "%" => Some(Symbol::Percent),
            "^" => Some(Symbol::Caret),
            "&" => Some(Symbol::And),
            "|" => Some(Symbol::Or),
            "@" => Some(Symbol::At),
            "." => Some(Symbol::Dot),
            "," => Some(Symbol::Comma),
            ";" => Some(Symbol::Semi),
            ":" => Some(Symbol::Colon),
            "#" => Some(Symbol::Pound),
            "$" => Some(Symbol::Dollar),
            "?" => Some(Symbol::Question),
            "_" => Some(Symbol::Underscore),
            "{" => Some(Symbol::LBrace),
            "}" => Some(Symbol::RBrace),
            "[" => Some(Symbol::LBracket),
            "]" => Some(Symbol::RBracket),
            "(" => Some(Symbol::LParen),
            ")" => Some(Symbol::RParen),

            // 多字符 (严格按规范顺序)
            "<=" => Some(Symbol::Le),
            "==" => Some(Symbol::EqEq),
            "!=" => Some(Symbol::Ne),
            ">=" => Some(Symbol::Ge),
            "&&" => Some(Symbol::AndAnd),
            "||" => Some(Symbol::OrOr),
            "<<" => Some(Symbol::Shl),
            ">>" => Some(Symbol::Shr),
            "+=" => Some(Symbol::PlusEq),
            "-=" => Some(Symbol::MinusEq),
            "*=" => Some(Symbol::StarEq),
            "/=" => Some(Symbol::SlashEq),
            "%=" => Some(Symbol::PercentEq),
            "^=" => Some(Symbol::CaretEq),
            "&=" => Some(Symbol::AndEq),
            "|=" => Some(Symbol::OrEq),
            "<<=" => Some(Symbol::ShlEq),
            ">>=" => Some(Symbol::ShrEq),
            ".." => Some(Symbol::DotDot),
            "..." => Some(Symbol::DotDotDot),
            "..=" => Some(Symbol::DotDotEq),
            "::" => Some(Symbol::ColonColon),
            "->" => Some(Symbol::RArrow),
            "<-" => Some(Symbol::LArrow),
            "=>" => Some(Symbol::FatArrow),

            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            // 单字符
            Symbol::Eq => "=",
            Symbol::Lt => "<",
            Symbol::Gt => ">",
            Symbol::Not => "!",
            Symbol::Tilde => "~",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Star => "*",
            Symbol::Slash => "/",
            Symbol::Percent => "%",
            Symbol::Caret => "^",
            Symbol::And => "&",
            Symbol::Or => "|",
            Symbol::At => "@",
            Symbol::Dot => ".",
            Symbol::Comma => ",",
            Symbol::Semi => ";",
            Symbol::Colon => ":",
            Symbol::Pound => "#",
            Symbol::Dollar => "$",
            Symbol::Question => "?",
            Symbol::Underscore => "_",
            Symbol::LBrace => "{",
            Symbol::RBrace => "}",
            Symbol::LBracket => "[",
            Symbol::RBracket => "]",
            Symbol::LParen => "(",
            Symbol::RParen => ")",

            // 多字符
            Symbol::Le => "<=",
            Symbol::EqEq => "==",
            Symbol::Ne => "!=",
            Symbol::Ge => ">=",
            Symbol::AndAnd => "&&",
            Symbol::OrOr => "||",
            Symbol::Shl => "<<",
            Symbol::Shr => ">>",
            Symbol::PlusEq => "+=",
            Symbol::MinusEq => "-=",
            Symbol::StarEq => "*=",
            Symbol::SlashEq => "/=",
            Symbol::PercentEq => "%=",
            Symbol::CaretEq => "^=",
            Symbol::AndEq => "&=",
            Symbol::OrEq => "|=",
            Symbol::ShlEq => "<<=",
            Symbol::ShrEq => ">>=",
            Symbol::DotDot => "..",
            Symbol::DotDotDot => "...",
            Symbol::DotDotEq => "..=",
            Symbol::ColonColon => "::",
            Symbol::RArrow => "->",
            Symbol::LArrow => "<-",
            Symbol::FatArrow => "=>",
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
