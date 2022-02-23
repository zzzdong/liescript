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
