

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Byte(u8),
    Char(char),
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
}