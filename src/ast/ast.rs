use std::{
    fmt, io,
    io::{Error, ErrorKind},
    str::FromStr,
};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(BinOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Literal(Literal),
    Identifier(String),
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogicAnd,
    LogicOr,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    As,
    Range,
    Dot,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::LogicAnd => write!(f, "&&"),
            BinOp::LogicOr => write!(f, "||"),
            BinOp::Less => write!(f, "<"),
            BinOp::LessEqual => write!(f, "<="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEqual => write!(f, ">="),
            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::As => write!(f, "as"),
            BinOp::Range => write!(f, "..="),
            BinOp::Dot => write!(f, "."),
        }
    }
}

impl FromStr for BinOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinOp::Add),
            "-" => Ok(BinOp::Sub),
            "*" => Ok(BinOp::Mul),
            "/" => Ok(BinOp::Div),
            "%" => Ok(BinOp::Mod),
            "&&" => Ok(BinOp::LogicAnd),
            "||" => Ok(BinOp::LogicOr),
            "<" => Ok(BinOp::Less),
            "<=" => Ok(BinOp::LessEqual),
            ">" => Ok(BinOp::Greater),
            ">=" => Ok(BinOp::GreaterEqual),
            "==" => Ok(BinOp::Equal),
            "!=" => Ok(BinOp::NotEqual),
            "as" => Ok(BinOp::As),
            "..=" => Ok(BinOp::Range),
            "." => Ok(BinOp::Dot),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid binanry op: {}", s),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Try,
}

impl FromStr for UnaryOp {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(UnaryOp::Neg),
            "!" => Ok(UnaryOp::Not),
            "?" => Ok(UnaryOp::Try),
            _ => Err(Error::new(
                ErrorKind::InvalidInput,
                format!("Invalid unary op: {}", s),
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
}
