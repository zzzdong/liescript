mod ast;
mod bytecode;
mod instructions;
mod value;
mod vm;
mod parser;
mod tokenizer;
mod token;


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
