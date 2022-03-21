use std::fmt::{self};
use std::slice;
use std::str::Chars;

use crate::token::{Ident, Keyword, LineCol, Literal, Span, Symbol, Token};
use crate::token::{TokenError, TokenKind};

#[derive(Clone)]
pub struct Tokenizer<'i> {
    chars: Chars<'i>,
    input: &'i str,
    filename: &'i str,
    offset: usize,
    location: LineCol,
}

impl<'i> Tokenizer<'i> {
    pub fn new(filename: &'i str, input: &'i str) -> Self {
        let chars = input.chars();

        Tokenizer {
            chars,
            filename,
            offset: 0,
            input,
            location: LineCol::new(),
        }
    }

    fn location(&self) -> (usize, LineCol) {
        (self.offset, self.location)
    }

    fn new_token(&self, start: (usize, LineCol), kind: TokenKind) -> Token {
        let span = Span::new(start.1, self.location);

        Token { span, kind }
    }

    pub fn next_token(&mut self) -> Result<Token, TokenError> {
        let start = self.location();

        self.next_token_kind()
            .map(|kind| self.new_token(start, kind))
    }

    fn next_token_kind(&mut self) -> Result<TokenKind, TokenError> {
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
                        // parser tree
                        if matches!(c, '(' | '[' | '{') {
                            return self.eat_tree();
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
            self.offset += c.len_utf8();
            if c == '\n' {
                self.location.line += 1;
                self.location.column = 1;
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
        // let start = self.offset;
        // while let Some(ch) = self.peek() {
        //     if !predicate(ch) {
        //         break;
        //     }

        //     self.advance(1);
        // }

        // &self.input[start..self.offset]

        let start = self.chars.as_str();
        let mut len = 0;

        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                return &start[..len];
            }
            // ret.push(self.next_char().unwrap());
            len += ch.len_utf8();
            self.advance(1);
        }

        // ret
        &start[..len]
    }

    pub fn eat_whitespace(&mut self) -> Result<TokenKind, TokenError> {
        let ws = self.eat_while(|c| c.is_whitespace());

        Ok(TokenKind::Whitespace)
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
        let start = self.offset;
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
        let pos = self.offset;
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

        Ok(TokenKind::Comment)
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

    fn eat_tree(&mut self) -> Result<TokenKind, TokenError> {
        let mut group = Vec::new();

        let _open = self.next_char().unwrap();

        loop {
            let start = self.location();

            match self.next_token_kind()? {
                TokenKind::Eof => {
                    return Err(TokenError::new("unclose group"));
                }
                TokenKind::Symbol(Symbol::RParen) => return Ok(TokenKind::ParenTree(group)),
                TokenKind::Symbol(Symbol::RSquare) => return Ok(TokenKind::SquareTree(group)),
                TokenKind::Symbol(Symbol::RBracket) => return Ok(TokenKind::BracketTree(group)),

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
            // closed tree
            ')' | ']' | '}' |
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
            .field("filename", &self.filename)
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

    pub fn next_token(&mut self) -> Option<TokenKind> {
        self.next().map(|t| t.kind)
    }
}

impl<'i> Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|token| match token.kind {
            TokenKind::Comment | TokenKind::Whitespace => self.next(),
            _ => Some(token),
        })
    }
}

/// A token stream iterator.
#[derive(Debug, Clone)]
pub struct TokenStreamIter<'i> {
    iter: slice::Iter<'i, Token>,
}

impl<'i> Iterator for TokenStreamIter<'i> {
    type Item = Token;

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
                    TokenKind::Whitespace,
                    TokenKind::ident("b"),
                    TokenKind::Whitespace,
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
                    TokenKind::Whitespace,
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
            let mut tokenizer = Tokenizer::new("", i.0);

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

        let tokenizer = Tokenizer::new(filepath, &input);

        for t in tokenizer {
            println!("{:?}", t);
        }
    }

    #[test]
    fn test_tokenizer_tokentree() {
        let inputs: Vec<&str> = vec!["a[b[c]]", "a[b[c[d[e[f]]]]]", "a(b(c(d(e()))))"];

        fn print(token: &Token) {
            match &token.kind {
                TokenKind::ParenTree(group) => {
                    for g in group {
                        print!("->");
                        print(&g)
                    }
                }
                TokenKind::SquareTree(group) => {
                    for g in group {
                        print!("->");
                        print(&g)
                    }
                }
                TokenKind::BracketTree(group) => {
                    for g in group {
                        print!("->");
                        print(&g)
                    }
                }
                _ => {
                    println!("{token:?}");
                }
            }
        }

        for i in inputs {
            let tokenizer = Tokenizer::new("", i);
            for t in tokenizer {
                let t = t.unwrap();
                print(&t);
                println!("{t:?}");
            }
        }
    }
}
