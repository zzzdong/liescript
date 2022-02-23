#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(ident: impl ToString) -> Self {
        Ident(ident.to_string())
    }

    pub fn value(&self) -> &str {
        &self.0
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

