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

            pub fn as_str(&self) -> &'static str {
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
