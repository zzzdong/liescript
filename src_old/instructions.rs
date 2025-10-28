use crate::value::Value;

pub enum Instruction {
    LoadConst(Value),
    LookUp(String),
    GetAttr(String),
    Add,
    Sub,
    Mul,
    Div,
    Or,
    And,
    Lsh,
    Rsh,
    Neg,
    Mod,
    Xor,
    Mov,
}
