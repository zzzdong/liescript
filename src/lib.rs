mod bytecode;
mod diagnostic;
mod error;
mod instructions;
mod lexical;
mod parser;
mod syntax;
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
