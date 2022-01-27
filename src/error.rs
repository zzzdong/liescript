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


