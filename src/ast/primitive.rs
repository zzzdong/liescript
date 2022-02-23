use std::fmt;


macro_rules! define_primitive {
    (
        $(
            $name:ident => $str:expr,
        )*
    ) => {
        #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        pub enum Primitive {
            $($name,)*
        }

        impl Primitive {
            pub const ALL: &'static [Primitive] = &[
                $(Primitive::$name,)*
            ];

            pub const STRS: &'static [&'static str] = &[
                $($str,)*
            ];

            pub fn all() -> impl Iterator<Item=Primitive> {
                Self::ALL.iter().copied()
            }

            pub fn from_str(s: &str) -> Self {
                match s {
                    $($str => Primitive::$name,)*
                    _ => {
                        unreachable!();
                    }
                }
            }

            pub fn as_str(self) -> &'static str {
                match self {
                    $(Primitive::$name => $str,)*
                }
            }

            pub fn from_ident(ident: &super::Ident) -> Option<Self> {
                match ident.as_str() {
                    $($str => Some(Primitive::$name),)*
                    _ => None,
                }
            }
        }

        impl fmt::Display for Primitive {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Primitive::$name => write!(f, $str),)*
                }
            }
        }
    };
}


define_primitive!{
    Byte => "byte",
    Char => "char",
    Int => "int",
    Float => "float",
    Str => "str",
}