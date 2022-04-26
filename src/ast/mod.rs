use std::fmt;

pub mod nodes;
pub mod op;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(ident: impl ToString) -> Self {
        Ident(ident.to_string())
    }

    pub fn value(&self) -> &str {
        &self.0
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

macro_rules! define_keywords {
        (
            $(
                $name:ident => $str:expr,
            )*
        ) => {
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            pub enum Keyword {
                $($name,)*
            }

            impl Keyword {
                pub const ALL: &'static [Keyword] = &[
                    $(Keyword::$name,)*
                ];

                pub const STRS: &'static [&'static str] = &[
                    $($str,)*
                ];

                pub fn all() -> impl Iterator<Item=Keyword> {
                    Self::ALL.iter().copied()
                }

                pub fn from_str(s: &str) -> Self {
                    match s {
                        $($str => Keyword::$name,)*
                        _ => {
                            unreachable!();
                        }

                    }
                }
            }
        };
    }

define_keywords! {
    As => "as",
    Bool => "bool",
    Break => "break",
    Byte => "byte",
    Class => "class",
    Catch => "catch",
    Const => "const",
    Continue => "continue",
    Char => "char",
    Crate => "crate",
    Else => "else",
    Enum => "enum",
    Extern => "extern",
    Extends => "extends",
    False => "false",
    Finally => "finally",
    Float => "float",
    Fn => "fn",
    For => "for",
    If => "if",
    Impl => "impl",
    In => "in",
    Int => "int",
    Let => "let",
    Loop => "loop",
    Match => "match",
    Mod => "mod",
    Move => "move",
    Priv => "priv",
    Pub => "pub",
    Ref => "ref",
    Return => "return",
    SelfValue => "self",
    SelfType => "Self",
    Static => "static",
    Str => "str",
    Struct => "struct",
    Super => "super",
    Then => "then",
    Throw => "throw",
    Trait => "trait",
    Try => "try",
    Type => "type",
    Unsafe => "unsafe",
    Use => "use",
    Where => "where",
    While => "while",
    Await => "await",
    Async => "async",
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Byte(u8),
    Char(char),
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

macro_rules! define_symbols {
        (
            $(
                $name:ident => $str:expr,
            )*
        ) => {
            #[derive(Copy, Clone, Debug,  PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub enum Symbol {
                $(#[doc=$str]$name,)*
            }

            impl Symbol {
                pub const ALL: &'static [Symbol] = &[
                    $(Symbol::$name,)*
                ];

                pub const STRS: &'static [&'static str] = &[
                    $($str,)*
                ];

                pub fn all() -> impl Iterator<Item=Symbol> {
                    Self::ALL.iter().copied()
                }

                pub fn from_str(s: &str) -> Result<Self, &'static str> {
                    match s {
                        $($str => Ok(Symbol::$name),)*
                        _ => {
                            Err("unkown punctuation")
                        }
                    }
                }

                pub fn as_str(self) -> &'static str {
                    match self {
                        $(Symbol::$name => $str,)*
                    }
                }
            }
        };
    }

define_symbols! {
    Plus       => "+",
    Minus      => "-",
    Star       => "*",
    Slash      => "/",
    Percent    => "%",
    Caret      => "^",
    Not        => "!",
    And        => "&",
    Or         => "|",
    AndAnd     => "&&",
    OrOr       => "||",
    LShift     => "<<",
    RShift     => ">>",
    PlusEq     => "+=",
    MinusEq    => "-=",
    StarEq     => "*=",
    SlashEq    => "/=",
    PercentEq  => "%=",
    CaretEq    => "^=",
    AndEq      => "&=",
    OrEq       => "|=",
    ShlEq      => "<<=",
    ShrEq      => ">>=",
    Eq         => "=",
    EqEq       => "==",
    NotEq      => "!=",
    Lt         => "<",
    LtE        => "<=",
    Gt         => ">",
    GtE        => ">=",
    At         => "@",
    Dot        => ".",
    DotDot     => "..",
    DotDotEq   => "..=",
    Comma     => ",",
    Semicolon  => ";",
    Colon      => ":",
    PathSep    => "::",
    RArrow     => "->",
    FatArrow   => "=>",
    Pound      => "#",
    Dollar     => "$",
    Question   => "?",
    LParen     => "(",
    RParen     => ")",
    LBracket    => "[",
    RBracket    => "]",
    LBrace   => "{",
    RBrace   => "}",
    // SQuotes    => "'",
    // DQuotes    => "\"",
}
