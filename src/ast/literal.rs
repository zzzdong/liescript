use std::fmt;


#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Byte(u8),
    Char(char),
    Integer(i64),
    Float(f64),
    String(String),
    ByteSlice(Vec<u8>),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Byte(b) => write!(f, "{b:#x}"),
            Literal::Char(c) => write!(f, "'{c}'"),
            Literal::Bool(b) => write!(f, "{b}"),
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(i) => write!(f, "{i}"),
            Literal::String(s) => write!(f, "{s}"),
            Literal::ByteSlice(bs) => write!(
                f,
                "\\x{}",
                bs.iter().map(|b| format!("{b:02x}")).collect::<String>()
            ),
        }
    }
}

impl From<u8> for Literal {
    fn from(b: u8) -> Self {
        Literal::Byte(b)
    }
}

impl From<bool> for Literal {
    fn from(b: bool) -> Self {
        Literal::Bool(b)
    }
}

impl From<char> for Literal {
    fn from(c: char) -> Self {
        Literal::Char(c)
    }
}

impl From<i64> for Literal {
    fn from(i: i64) -> Self {
        Literal::Integer(i)
    }
}

impl From<f64> for Literal {
    fn from(f: f64) -> Self {
        Literal::Float(f)
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Literal::String(s)
    }
}

impl From<&str> for Literal {
    fn from(s: &str) -> Self {
        Literal::String(s.to_owned())
    }
}
