use std::collections::HashMap;

use crate::instructions::Instruction;
use crate::value::Value;

#[derive(Debug, Default)]
pub struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub fn new(capacity: usize) -> Self {
        Stack {
            values: Vec::with_capacity(capacity),
        }
    }

    pub fn push(&mut self, arg: Value) {
        self.values.push(arg);
    }

    pub fn pop(&mut self) -> Value {
        self.values.pop().expect("stack was empty")
    }

    pub fn try_pop(&mut self) -> Option<Value> {
        self.values.pop()
    }

    pub fn peek(&self) -> &Value {
        self.values.last().expect("stack was empty")
    }
}

pub struct Compiled {
    instructions: Vec<Instruction>,
}

#[derive(Default)]
pub struct Context {
    globals: HashMap<String, Value>,
}

impl Context {
    pub fn new() -> Context {
        Context::default()
    }

    pub fn register(&mut self, name: impl ToString, value: Value) -> Option<Value> {
        self.globals.insert(name.to_string(), value)
    }
}

pub struct Vm {
    pc: usize,
    stack: Stack,
}

impl Vm {
    // fn eval_context(
    //     &mut self,
    //     ctx: Context,
    //     compiled: Compiled,
    // ) -> Result<Option<Value>, Box<dyn std::error::Error + Send + Sync + 'static>> {
    //     let instructions = compiled.instructions;

    //     while let Some(instr) = instructions.get(self.pc) {
    //         match instr {
    //             Instruction::LoadConst(name) => {

    //             }
    //             Instruction::LookUp(name) => {

    //             }
    //         }

    //     }

    //     Ok(self.stack.try_pop())
    // }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_eval_context() {}
}
