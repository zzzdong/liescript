use std::borrow::Cow;
use std::fmt::{self};
use std::slice;
use std::str::Chars;

use crate::ast::{Ident, Keyword, Literal, Symbol};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}~{}:{}",
            self.start.line, self.start.column, self.end.line, self.end.column
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Pos {
    pub file: usize,
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn new() -> Self {
        Pos {
            file: 0,
            offset: 0,
            line: 1,
            column: 1,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Token { span, kind }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Ident(Ident),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    Whitespace(String),
    Comment(String),
    Tree(Vec<Token>),
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Ident(ident) => {
                write!(f, "{}", ident.as_str())
            }
            TokenKind::Literal(lit) => {
                write!(f, "{}", lit)
            }
            TokenKind::Keyword(kw) => {
                write!(f, "{}", kw.as_str())
            }
            TokenKind::Symbol(sym) => {
                write!(f, "{}", sym.as_str())
            }
            TokenKind::Whitespace(ws) => {
                write!(f, "{}", ws.as_str())
            }
            TokenKind::Comment(c) => {
                write!(f, "{}", c.as_str())
            }
            TokenKind::Tree(t) => {
                write!(f, "Tree()")
            }
            TokenKind::Eof => {
                write!(f, "EOF")
            }
        }
    }
}

#[derive(Debug)]
pub struct TokenError {
    pub(crate) detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> TokenError {
        TokenError {
            detail: Some(detail.into()),
            source: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }
}

impl TokenKind {
    pub(crate) fn ident(ident: impl ToString) -> TokenKind {
        TokenKind::Ident(Ident::new(ident))
    }
    pub(crate) fn int(i: i64) -> TokenKind {
        TokenKind::Literal(Literal::Integer(i))
    }
    pub(crate) fn float(f: f64) -> TokenKind {
        TokenKind::Literal(Literal::Float(f))
    }
    pub(crate) fn string(s: impl ToString) -> TokenKind {
        TokenKind::Literal(Literal::String(s.to_string()))
    }
    pub(crate) fn symbol(s: &str) -> TokenKind {
        TokenKind::Symbol(Symbol::from_str(s).unwrap())
    }
    pub(crate) fn whitespace(s: &str) -> TokenKind {
        TokenKind::Whitespace(s.into())
    }
}

#[derive(Clone)]
pub struct Tokenizer<'i> {
    chars: Chars<'i>,
    input: &'i str,
    pos: Pos,
}

impl<'i> Tokenizer<'i> {
    pub fn new(input: &'i str) -> Self {
        let chars = input.chars();

        Tokenizer {
            chars,
            input,
            pos: Pos::new(),
        }
    }

    fn pos(&self) ->  Pos {
        self.pos
    }

    fn new_token(&self, start: Pos, kind: TokenKind) -> Token {
        let span = Span::new(start, self.pos);

        Token::new(span, kind)
    }

    pub fn next_token(&mut self) -> Result<Token, TokenError> {
        let start = self.pos();

        self.next_token_inner()
            .map(|kind| self.new_token(start, kind))
    }

    fn next_token_inner(&mut self) -> Result<TokenKind, TokenError> {
        match self.peek() {
            Some(c) => {
                match c {
                    '\t' | '\n' | '\x0C' | '\r' | ' ' => self.eat_whitespace(),
                    '_' | 'a'..='z' | 'A'..='Z' => self.eat_ident(),
                    '0'..='9' => self.eat_number(),
                    '\'' => self.eat_char(),
                    '"' => self.eat_string(),
                    c => {
                        // comment
                        if self.starts_with("//") {
                            return self.eat_comment();
                        }
                        // symbol
                        self.eat_symbol(c)
                    }
                }
            }
            None => Ok(TokenKind::Eof),
        }
    }

    fn len(&self) -> usize {
        self.chars.clone().count()
    }

    fn has_at_lease(&self, n: usize) -> bool {
        self.chars.clone().nth(n - 1).is_some()
    }

    fn starts_with(&self, pat: &str) -> bool {
        self.chars.clone().as_str().starts_with(pat)
    }

    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|c| {
            self.pos.offset += c.len_utf8();
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
            c
        })
    }

    fn advance(&mut self, n: usize) {
        for i in 0..n {
            if self.next_char().is_none() {
                break;
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn eat_while<P>(&mut self, mut predicate: P) -> &'i str
    where
        P: FnMut(char) -> bool,
    {
        let start = self.chars.as_str();
        let mut len = 0;

        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                return &start[..len];
            }
            len += ch.len_utf8();
            self.advance(1);
        }

        &start[..len]
    }

    pub fn eat_whitespace(&mut self) -> Result<TokenKind, TokenError> {
        let ws = self.eat_while(|c| c.is_whitespace());

        Ok(TokenKind::Whitespace(ws.to_string()))
    }

    pub fn eat_ident(&mut self) -> Result<TokenKind, TokenError> {
        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        let token = match got {
            "true" => TokenKind::Literal(Literal::Bool(true)),
            "false" => TokenKind::Literal(Literal::Bool(false)),
            kw if Keyword::STRS.contains(&kw) => {
                let kw = Keyword::from_str(kw);
                TokenKind::Keyword(kw)
            }
            _ => TokenKind::Ident(Ident::new(got)),
        };

        Ok(token)
    }

    fn eat_number(&mut self) -> Result<TokenKind, TokenError> {
        let mut is_float = false;

        let num = self.eat_while(|c| {
            if !is_float && c == '.' {
                is_float = true;
                true
            } else {
                c.is_ascii_digit()
            }
        });

        if is_float {
            num.parse::<f64>()
                .map(|f| TokenKind::float(f))
                .map_err(|e| TokenError::new("parse float failed").with_source(e))
        } else {
            num.parse::<i64>()
                .map(|i| TokenKind::int(i))
                .map_err(|e| TokenError::new("parse float failed").with_source(e))
        }
    }

    fn eat_string(&mut self) -> Result<TokenKind, TokenError> {
        self.eat_qoutes('"')
            .map(|s| TokenKind::Literal(Literal::String(s)))
    }

    fn eat_char(&mut self) -> Result<TokenKind, TokenError> {
        let s = self.eat_qoutes('\'')?;

        if s.chars().count() == 1 {
            Ok(TokenKind::Literal(Literal::Char(s.chars().next().unwrap())))
        } else {
            Err(TokenError::new("too many char for CharLit"))
        }
    }

    fn eat_comment(&mut self) -> Result<TokenKind, TokenError> {
        self.advance(2);

        let s = self.eat_while(|c| c != '\n');

        self.advance(1); // eat `\n`

        Ok(TokenKind::Comment(s.to_string()))
    }

    fn eat_qoutes(&mut self, qoute: char) -> Result<String, TokenError> {
        let mut ret = String::new();

        self.advance(1); // skip start qoute

        let mut is_backslash_previous = false;

        while let Some(c) = self.next_char() {
            match c {
                '\\' => {
                    if is_backslash_previous {
                        ret.push(c);
                        is_backslash_previous = false;
                    } else {
                        is_backslash_previous = true;
                    }
                }
                _ => {
                    if c == qoute {
                        return Ok(ret);
                    }

                    if is_backslash_previous {
                        let ch = match c {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            _ => {
                                if c == qoute {
                                    c
                                } else {
                                    return Err(TokenError::new("unknown char after escape"));
                                }
                            }
                        };

                        is_backslash_previous = true;
                        ret.push(ch);
                    } else {
                        ret.push(c);
                    }
                }
            }
        }

        Err(TokenError::new("incompleted qouted"))
    }

    fn eat_tree(&mut self) -> Result<Token, TokenError> {
        let mut group = Vec::new();

        let _open = self.next_char().unwrap();

        loop {
            let start = self.pos();

            let token = self.next_token()?;

            match token.kind {
                TokenKind::Eof => {
                    return Err(TokenError::new("unclose group"));
                }
                TokenKind::Symbol(Symbol::RParen)
                | TokenKind::Symbol(Symbol::RBracket)
                | TokenKind::Symbol(Symbol::RBrace) => {
                    return Ok(self.new_token(start, TokenKind::Tree(group)))
                }

                t => {
                    group.push(self.new_token(start, t));
                }
            }
        }
    }

    fn eat_symbol(&mut self, peek: char) -> Result<TokenKind, TokenError> {
        // try 3 byte
        if self.has_at_lease(3) {
            if self.starts_with("..=") {
                self.advance(3);
                return Ok(TokenKind::Symbol(Symbol::DotDotEq));
            }
        }
        // try 2 byte
        if self.has_at_lease(2) {
            let pat = &self.chars.clone().as_str()[..2];
            let token = match pat {
                // logic op
                "&&" | "||" |
                // assign
                "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" |
                // compare op
                "==" | "!=" | ">=" | "<=" |
                // others
                "::" | "->" | ".." => {
                    Symbol::from_str(pat).ok().map(TokenKind::Symbol)
                }
                _ => None,
            };

            if let Some(t) = token {
                self.advance(2);
                return Ok(t);
            }
        }

        let token = match peek {
            // num op
            '+' | '-' | '*' | '/' | '%' | '^' |
            // compare op
            '>' | '<' |
            // paren
            '(' | ')' | '[' | ']' | '{' | '}' |
            // others
            ',' | ':' | ';' | '#' | '!' | '?' | '&' | '=' | '.' => {
                let mut tmp = [0u8; 4];
                Symbol::from_str(peek.encode_utf8(&mut tmp)).ok().map(TokenKind::Symbol)
            }
            _ => None,
        };

        if let Some(t) = token {
            self.advance(1);
            return Ok(t);
        }

        Err(TokenError::new(format!("unknown({peek})")))
    }

    pub fn token_stream(self) -> Result<TokenStream, TokenError> {
        let tokens: Result<Vec<_>, TokenError> = self.into_iter().collect();
        tokens.map(TokenStream::new)
    }
}

impl<'i> fmt::Debug for Tokenizer<'i> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tokenizer")
            .field("input", &self.input)
            .finish()
    }
}

impl<'i> Iterator for Tokenizer<'i> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(t) if t.kind == TokenKind::Eof => None,
            t => Some(t),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TokenStream {
    iter: <Vec<Token> as IntoIterator>::IntoIter,
}

impl<'i> TokenStream {
    pub fn new(iter: Vec<Token>) -> Self {
        TokenStream {
            iter: iter.into_iter(),
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.next()
    }
}

impl<'i> Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|token| match token.kind {
            TokenKind::Comment(_) | TokenKind::Whitespace(_) => self.next(),
            _ => Some(token),
        })
    }
}

/// A token stream iterator.
#[derive(Debug, Clone)]
pub struct TokenStreamIter<'i> {
    iter: slice::Iter<'i, TokenKind>,
}

impl<'i> Iterator for TokenStreamIter<'i> {
    type Item = TokenKind;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().cloned()
    }
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let inputs: Vec<(&str, Vec<TokenKind>)> = vec![
            (
                "a b\tc",
                vec![
                    TokenKind::ident("a"),
                    TokenKind::whitespace(" "),
                    TokenKind::ident("b"),
                    TokenKind::whitespace(" "),
                    TokenKind::ident("c"),
                ],
            ),
            (
                "hello,123,123.4",
                vec![
                    TokenKind::ident("hello"),
                    TokenKind::symbol(","),
                    TokenKind::int(123),
                    TokenKind::symbol(","),
                    TokenKind::float(123.4),
                ],
            ),
            (
                r#"let a=b+c*123;"#,
                vec![
                    TokenKind::Keyword(Keyword::Let),
                    TokenKind::whitespace(" "),
                    TokenKind::ident("a"),
                    TokenKind::symbol("="),
                    TokenKind::ident("b"),
                    TokenKind::symbol("+"),
                    TokenKind::ident("c"),
                    TokenKind::symbol("*"),
                    TokenKind::int(123),
                    TokenKind::symbol(";"),
                ],
            ),
        ];

        for i in &inputs {
            let mut tokenizer = Tokenizer::new(i.0);

            let mut ret = Vec::new();

            loop {
                let t = tokenizer.next_token().unwrap();
                if t.kind == TokenKind::Eof {
                    break;
                }

                ret.push(t.kind);
            }

            assert_eq!(ret, i.1);
        }
    }

    #[test]
    fn test_tokenizer2() {
        use std::io::Read;

        let filepath = "src/vm.rs";

        let mut file = std::fs::File::open(filepath).unwrap();

        let mut input = String::new();
        file.read_to_string(&mut input).unwrap();

        let tokenizer = Tokenizer::new(&input);

        for t in tokenizer {
            println!("{:?}", t);
        }
    }
}
