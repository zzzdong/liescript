use std::borrow::Cow;

pub use self::{identifiers::Ident, keywords::Keyword, literal::Literal, punctuation::Punctuation};

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

impl Clone for TokenError {
    fn clone(&self) -> Self {
        unreachable!()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'i> {
    Eof,
    Whitespace(&'i str),
    Ident(Ident),
    Literal(Literal),
    Comment(&'i str),
    Keyword(Keyword),
    Punctuation(Punctuation),
    TokenTree(TreeType, Vec<Token<'i>>),
    Unknown(char),
    Error(TokenError),
}

impl<'i> Token<'i> {
    pub(crate) fn ident(ident: impl ToString) -> Token<'i> {
        Token::Ident(Ident::new(ident))
    }
    pub(crate) fn whitespace(ws: &'i str) -> Token<'i> {
        Token::Whitespace(ws)
    }
    pub(crate) fn int(i: i64) -> Token<'i> {
        Token::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> Token<'i> {
        Token::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> Token<'i> {
        Token::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn punctuation(s: &str) -> Token {
        Token::Punctuation(Punctuation::from_str(s).unwrap())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TreeType {
    Group,
    Array,
    Block,
}

mod identifiers {
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
}

mod keywords {
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
        If => "if",
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
}

mod literal {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Literal {
        Byte(u8),
        Char(char),
        Bool(bool),
        Integer(i64),
        Float(f64),
        String(String),
    }
}

mod punctuation {
    macro_rules! define_punctuations {
        (
            $(
                $name:ident => $str:expr,
            )*
        ) => {
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            pub enum Punctuation {
                $($name,)*
            }

            impl Punctuation {
                pub const ALL: &'static [Punctuation] = &[
                    $(Punctuation::$name,)*
                ];

                pub const STRS: &'static [&'static str] = &[
                    $($str,)*
                ];

                pub fn all() -> impl Iterator<Item=Punctuation> {
                    Self::ALL.iter().copied()
                }

                pub fn from_str(s: &str) -> Result<Self, &'static str> {
                    match s {
                        $($str => Ok(Punctuation::$name),)*
                        _ => {
                            Err("unkown punctuation")
                        }
                    }
                }

                pub fn as_str(self) -> &'static str {
                    match self {
                        $(Punctuation::$name => $str,)*
                    }
                }
            }
        };
    }

    define_punctuations! {
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
        LSquare    => "[",
        RSquare    => "]",
        LBracket   => "{",
        RBracket   => "}",
        // SQuotes    => "'",
        // DQuotes    => "\"",
    }
}
