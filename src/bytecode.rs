
///    +--------+--------+----+----+~~~~+
///    | OPCODE |  FLAG  |RSRC|RTRG|IMM |
///    +--------+--------+---------+~~~~+
///    |  1~255 |  1~255 |4bit|4bit|0~8B|
///    +--------+--------+---------+~~~~+

pub struct Opcode {
    pub op: Op,
    pub mode: Mode,
    pub dst: u8,
    pub src: u8,
    pub imm: u64,
}


pub enum Op {
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


pub enum Mode {
    Register,
    IMM,
}