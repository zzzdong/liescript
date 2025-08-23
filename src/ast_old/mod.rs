pub mod expression;
pub mod ident;
pub mod keyword;
pub mod literal;
pub mod op;
pub mod statement;
pub mod symbol;
pub mod syntax;
pub mod token;

pub use expression::*;
pub use ident::Identifier;
pub use keyword::Keyword;
pub use literal::Literal;
pub use op::*;
pub use statement::*;
pub use symbol::Symbol;

use crate::diagnostic::Spanned;

#[derive(Debug, Clone)]
pub struct Unit {
    pub items: Vec<TopLevel>,
}

impl Unit {
    pub fn new() -> Self {
        Unit { items: Vec::new() }
    }
}



