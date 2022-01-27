use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    filename: String,
    line: usize,
    column: usize,
}

impl Location {
    pub fn new(filename: impl ToString, line: usize, column: usize) -> Self {
        Location {
            filename: filename.to_string(),
            line,
            column,
        }
    }
}

#[derive(Debug)]
pub struct TokenError {
    location: Location,
    detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(location: Location, detail: D) -> TokenError {
        TokenError {
            location: location,
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

impl PartialEq for TokenError {
    fn eq(&self, other: &Self) -> bool {
        unreachable!()
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Whitespace(String),
    Ident(String),
    BoolLit(bool),
    CharLit(char),
    IntegerLit(i64),
    FloatLit(f64),
    StringLit(String),
    Comment(String),
    Keywrod(Keyword),
    Delimiter(Delimiter),
    Operator(Operator),
    Unknown(char),
    Error(TokenError),
}

impl Token {
    pub(crate) fn ident(ident: impl ToString) -> Token {
        Token::Ident(ident.to_string())
    }
    pub(crate) fn whitespace(ws: impl ToString) -> Token {
        Token::Whitespace(ws.to_string())
    }
    pub(crate) fn int(i: i64) -> Token {
        Token::IntegerLit(i)
    }
    pub(crate) fn float(f: f64) -> Token {
        Token::FloatLit(f)
    }
    pub(crate) fn string(s: impl ToString) -> Token {
        Token::StringLit(s.to_string())
    }
    pub(crate) fn delimiter(s: &str) -> Token {
        Token::Delimiter(Delimiter::from_str(s).unwrap())
    }
}

// #[derive(Debug, PartialEq)]
// pub enum Keyword {
//     As,
//     Break,
//     Const,
//     Continue,
//     Crate,
//     Else,
//     Enum,
//     Extern,
//     False,
//     Fn,
//     For,
//     Impl,
//     In,
//     Let,
//     Loop,
//     Match,
//     Mod,
//     Move,
//     Pub,
//     Ref,
//     Return,
//     SelfValue,
//     SelfType,
//     Static,
//     Struct,
//     Super,
//     Trait,
//     Type,
//     Unsafe,
//     Use,
//     Where,
//     While,
//     Await,
//     Async,
// }

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum Delimiter {
//     LParen,   // (
//     RParen,   // )
//     LSquare,  // [
//     RSquare,  // ]
//     LBracket, // {
//     RBracker, // }
// }

macro_rules! define_delimiters {
    (
        $(
            $name:ident => $str:expr,
        )*
    ) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub enum Delimiter {
            $($name,)*
        }

        impl Delimiter {
            pub const ALL: &'static [Delimiter] = &[
                $(Delimiter::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=Delimiter> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Result<Self, String> {
                match s {
                    $($str => Ok(Delimiter::$name),)*
                    _ => Err(format!("unknown {} for delimiter", s))
                }
            }
        }
    };
}

define_delimiters! {
    PathSep   => "::",
    RArrow    => "->",


    LParen    => "(",
    RParen    => ")",
    LSquare   => "[",
    RSquare   => "]",
    LBracket  => "{",
    RBracker  => "}",
    Comma     => ",",
    Colon     => ":",
    Semicolon => ";",
    Eq        => "=",
    SQuotes   => "'",
    DQuotes   => "\"",
    Sharp     => "#",
}

macro_rules! define_operators {
    (
        $(
            $name:ident => $str:expr,
        )*
    ) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub enum Operator {
            $($name,)*
        }

        impl Operator {
            pub const ALL: &'static [Operator] = &[
                $(Operator::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=Operator> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Result<Self, String> {
                match s {
                    $($str => Ok(Operator::$name),)*
                    _ => Err(format!("unknown {} for operator", s))
                }
            }
        }
    };
}

define_operators! {
    PlusEq    => "+=",
    MinusEq   => "-=",
    MulEq     => "*=",
    DivEq     => "/=",
    ModEq     => "%=",
    PowEq     => "^=",
    LShift    => "<<",
    RShift    => ">>",
    And       => "&&",
    Or        => "||",
    Eq        => "==",
    NotEq     => "!=",
    LtE       => "<=",
    GtE       => ">=",
    DotDot    => "..",

    Not       => "!",
    Question  => "?",
    Plus      => "+",
    Minus     => "-",
    Mul       => "*",
    Div       => "/",
    Mod       => "%",
    Pow       => "^",
    Lt        => "<",
    Gt        => ">",
    Ref       => "&",
    Dot       => ".",

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
    Break => "break",
    Class => "class",
    Catch => "catch",
    Const => "const",
    Continue => "continue",
    Crate => "crate",
    Else => "else",
    Enum => "enum",
    Extern => "extern",
    Extends => "extends",
    False => "false",
    Finally => "finally",
    Fn => "fn",
    For => "for",
    Impl => "impl",
    In => "in",
    Let => "let",
    Loop => "loop",
    Match => "match",
    Mod => "mod",
    Move => "move",
    Pub => "pub",
    Ref => "ref",
    Return => "return",
    SelfValue => "self",
    SelfType => "Self",
    Static => "static",
    Struct => "struct",
    Super => "super",
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

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum Punctuation {
//     Plus,       // +
//     Minus,      // -
//     Star,       // *
//     Slash,      // /
//     Percent,    // %
//     Caret,      // ^
//     Not,        // !
//     And,        // &
//     Or,         // |
//     AndAnd,     // &&
//     OrOr,       // ||
//     LShift,     // <<
//     RShift,     // >>
//     PlusEq,     // +=
//     MinusEq,    // -=
//     StarEq,     // *=
//     SlashEq,    // /=
//     PercentEq,  // %=
//     CaretEq,    // ^=
//     AndEq,      // &=
//     OrEq,       // |=
//     ShlEq,      // <<=
//     ShrEq,      // >>=
//     Eq,         // =
//     EqEq,       // ==
//     NotEq,      // !=
//     Lt,         // <
//     LtE,        // <=
//     Gt,         // >
//     GtE,        // >=
//     At,         // @
//     Underscore, // _
//     Dot,        // .
//     DotDot,     // ..
//     DotDotDot,  // ...
//     DotDotEq,   // ..=
//     Comman,     // ,
//     Semicolon,  // ;
//     Colon,      // :
//     PathSep,    // ::
//     RArrow,     // ->
//     FatArrow,   // =>
//     Pound,      // #
//     Dollar,     // $
//     Question,   // ?
// }
