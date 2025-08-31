use crate::diagnostic::Spanned;
use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token, TokenSpan
};
use crate::syntax::{BinOp, RangeLimits, UnOp};
use crate::syntax::names::Path;
use crate::syntax::patterns::Pattern;
use crate::syntax::precedence::Precedence;
use crate::syntax::statements::Statement;
use crate::syntax::types::Type;
use crate::syntax::statements::*;

use super::parse::{Parse, ParseError, ParseStream};


impl Parse for Statement {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
    }
}