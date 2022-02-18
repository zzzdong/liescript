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
