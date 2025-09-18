use crate::diagnostic::Spanned;

pub type KeywordSpan = Spanned<Keyword>;

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

            pub fn from_str(s: &str) -> Option<Self> {
                match s {
                    $($str => Some(Keyword::$name),)*
                    _ => {
                        None
                    }

                }
            }

            pub fn as_str(&self) -> &str {
                match self {
                    $(Keyword::$name => $str,)*
                }
            }
        }

        impl std::fmt::Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(self.as_str())
            }
        }
    };
}

define_keywords! {
    // 严格关键字(Strict keywords)
    As => "as",
    Break => "break",
    Const => "const",
    Continue => "continue",
    Crate => "crate",
    Else => "else",
    Enum => "enum",
    Extern => "extern",
    False => "false",
    Fn => "fn",
    For => "for",
    If => "if",
    Impl => "impl",
    Let => "let",
    Loop => "loop",
    Match => "match",
    Mod => "mod",
    Move => "move",
    Mut => "mut",
    Pub => "pub",
    Ref => "ref",
    Return => "return",
    SelfValue => "self",
    SelfType => "Self",
    Static => "static",
    Struct => "struct",
    Super => "super",
    Trait => "trait",
    True => "true",
    Type => "type",
    Unsafe => "unsafe",
    Use => "use",
    Where => "where",
    While => "while",
    In => "in",

    // 保留关键字(Reserved keywords)
    Abstract => "abstract",
    Become => "become",
    Box => "box",
    Do => "do",
    Final => "final",
    Macro => "macro",
    Override => "override",
    Priv => "priv",
    Try => "try",
    Typeof => "typeof",
    Unsized => "unsized",
    Virtual => "virtual",
    Yield => "yield",

    // 弱关键字(Weak keywords)
    Async => "async",
    Await => "await",
    Dyn => "dyn",

    // 类型关键字(Type keywords)
    Any => "any",
    Bool => "bool",
    Byte => "byte",
    Char => "char",
    String => "string",
    Int => "int",
    Float => "float",
}
