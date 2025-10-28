mod bytecode;
mod diagnostic;
mod error;
mod instructions;
mod lexical;
mod parser;
mod source;
mod syntax;
mod value;
mod vm;

// 公开导出主要类型，供外部使用
pub use diagnostic::{Diagnostics, Span, Spanned};
pub use lexical::{Keyword, Literal, Symbol, Token, TokenStream};
pub use parser::{Parse, ParseContext, Parser, ParserConfig};
pub use source::FileId;
pub use syntax::expressions::Expression;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
