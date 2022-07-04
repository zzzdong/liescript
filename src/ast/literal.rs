use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Byte(u8),
    Bool(bool),
    Char(char),
    Integer(i64),
    Float(f64),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Byte(b) => write!(f, "{:#x}", b),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Integer(i) => write!(f, "{}", i),
            Literal::Float(i) => write!(f, "{}", i),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}
