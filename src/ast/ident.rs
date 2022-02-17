#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(ident: impl ToString) -> Self {
        Ident(ident.to_string())
    }
}