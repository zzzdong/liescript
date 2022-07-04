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

            pub fn as_str(&self) -> &str {
                match self {
                    $(Keyword::$name => $str,)*
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
