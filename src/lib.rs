mod ast;
mod bytecode;
mod error;
mod instructions;
mod parser;
mod tokenizer;
mod value;
mod vm;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
