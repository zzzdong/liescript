mod syntax;
mod lexical;
mod bytecode;
mod diagnostic;
mod error;
mod instructions;
mod parser;
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
