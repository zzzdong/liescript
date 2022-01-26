#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    Comma, // ,
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
    Unknown(String),
    Error(String),
}

impl Token {
    pub fn ident(ident: impl ToString) -> Token {
        Token::Ident(ident.to_string())
    }
    pub fn whitespace(ws: impl ToString) -> Token {
        Token::Whitespace(ws.to_string())
    }
    pub fn int(i: i64) -> Token {
        Token::IntegerLit(i)
    }
    pub fn float(f: f64) -> Token {
        Token::FloatLit(f)
    }
    pub fn string(s: impl ToString) -> Token {
        Token::StringLit(s.to_string())
    }
    pub fn decimal(c: char) -> Token {
        Token::Delimiter(Delimiter::from_char(c))
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
            $name:ident => $char:expr,
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

            pub const CHARS: &'static [char] = &[
                $($char,)*
            ];

            pub fn all() -> impl Iterator<Item=Delimiter> {
                Self::ALL.iter().copied()
            }

            pub fn from_char(ch: char) -> Self {
                match ch {
                    $($char => Delimiter::$name,)*
                    _ => {
                        unreachable!();
                    }

                }
            }
        }
    };
}

define_delimiters! {
    LParen   => '(',
    RParen   => ')',
    LSquare  => '[',
    RSquare  => ']',
    LBracket => '{',
    RBracker => '}',
    Comma    => ',',
    Colon    => ':',
    Semicolon => ';',
    Eq       => '=',
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

            pub fn from_str(s: &str) -> Self {
                match s {
                    $($str => Operator::$name,)*
                    _ => {
                        unreachable!();
                    }

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    Not,        // !
    And,        // &
    Or,         // |
    AndAnd,     // &&
    OrOr,       // ||
    LShift,     // <<
    RShift,     // >>
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
    Eq,         // =
    EqEq,       // ==
    NotEq,      // !=
    Lt,         // <
    LtE,        // <=
    Gt,         // >
    GtE,        // >=
    At,         // @
    Underscore, // _
    Dot,        // .
    DotDot,     // ..
    DotDotDot,  // ...
    DotDotEq,   // ..=
    Comman,     // ,
    Semicolon,  // ;
    Colon,      // :
    PathSep,    // ::
    RArrow,     // ->
    FatArrow,   // =>
    Pound,      // #
    Dollar,     // $
    Question,   // ?
}
