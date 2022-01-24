use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidSyntax,
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    filename: String,
    line: usize,
    column: usize,
}

impl Location {
    pub fn new(filename: impl ToString, line: usize, column: usize) -> Self {
        Location {
            filename: filename.to_string(),
            line,
            column,
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    location: Location,
    detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl ParserError {
    pub fn new<D: Into<Cow<'static, str>>>(location: Location, detail: D) -> ParserError {
        ParserError {
            location: location,
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}
