use std::fmt;
use std::{borrow::Cow, str::Chars};

use crate::token::{Ident, Keyword, Literal, Punctuation};
use crate::token::{Location, Token, TokenError, TreeType};

#[derive(Clone)]
pub struct Tokenizer<'i> {
    chars: Chars<'i>,
    input: &'i str,
    filename: String,
    position: usize,
}

impl<'i> Tokenizer<'i> {
    pub fn new(filename: &str, input: &'i str) -> Self {
        let chars = input.chars();

        Tokenizer {
            chars,
            filename: filename.to_string(),
            position: 0,
            input,
        }
    }

    fn location(&self, pos: usize) -> Location {
        let mut line = 1;
        let mut column = 1;

        let mut chars = self.input.chars();

        for i in 0..=pos {
            if let Some('\n') = chars.next() {
                line += 1;
                column = 1;
            }

            column += 1;
        }

        Location::new(&self.filename, line, column)
    }

    pub fn next_token(&mut self) -> Token<'i> {
        match self.peek() {
            Some(c) => {
                match c {
                    '\t' | '\n' | '\x0C' | '\r' | ' ' => self.eat_whitespace(),
                    '_' | 'a'..='z' | 'A'..='Z' => self.eat_ident(),
                    '0'..='9' => self.eat_number(),
                    '\'' => self.eat_char(),
                    '"' => self.eat_string(),
                    _ => {
                        if self.has_at_lease(3) {
                            let pat = &self.chars.clone().as_str()[..3];
                            if pat == "..=" {
                                self.advance(3);
                                return Token::Punctuation(Punctuation::DotDotEq);
                            }
                        }
                        // try 2 byte
                        if self.has_at_lease(2) {
                            let pat = &self.chars.clone().as_str()[..2];
                            let token = match pat {
                                // comment
                                "//" => {
                                    return self.eat_comment();
                                }
                                // logic op
                                "&&" | "||" |
                                // assign
                                "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "&=" | "|=" |
                                // compare op
                                "==" | "!=" | ">=" | "<=" |
                                // others
                                "::" | "->" | ".." => {
                                    Punctuation::from_str(pat).ok().map(Token::Punctuation)
                                }
                                _ => None,
                            };

                            if let Some(t) = token {
                                self.advance(2);
                                return t;
                            }
                        }

                        // parser tree
                        if matches!(c, '(' | '[' | '{') {
                            return self.eat_tree();
                        }

                        let token = match c {
                            // brackets
                            '(' | ')' | '[' | ']' | '{' | '}' |
                            // num op
                            '+' | '-' | '*' | '/' | '%' | '^' |
                            // compare op
                            '>' | '<' |
                            // others
                            ',' | ':' | ';' | '#' | '!' | '?' | '&' | '=' | '.' => {
                                let mut tmp = [0u8; 4];
                                Punctuation::from_str(c.encode_utf8(&mut tmp)).ok().map(Token::Punctuation)
                            }
                            _ => None,
                        };

                        if let Some(t) = token {
                            self.advance(1);
                            return t;
                        }

                        dbg!(c, self.location(self.position));

                        self.advance(1);

                        Token::Unknown(c)
                    }
                }
            }
            None => Token::Eof,
        }
    }

    fn len(&self) -> usize {
        self.chars.clone().count()
    }

    fn has_at_lease(&self, n: usize) -> bool {
        self.chars.clone().nth(n - 1).is_some()
    }

    pub fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next().map(|c| {
            self.position += 1;
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
        // let mut ret = String::new();

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

    pub fn eat_whitespace(&mut self) -> Token<'i> {
        let ws = self.eat_while(|c| c.is_whitespace());

        Token::Whitespace(ws)
    }

    pub fn eat_ident(&mut self) -> Token<'i> {
        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match got {
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(false)),
            kw if Keyword::STRS.contains(&kw) => {
                let kw = Keyword::from_str(kw);
                Token::Keyword(kw)
            }
            _ => Token::Ident(Ident::new(got)),
        }
    }

    fn eat_number(&mut self) -> Token<'i> {
        let start = self.position;
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
            match num.parse::<f64>() {
                Ok(i) => Token::float(i),
                Err(e) => Token::Error(
                    TokenError::new(self.location(start), "parse float failed").with_source(e),
                ),
            }
        } else {
            match num.parse::<i64>() {
                Ok(i) => Token::int(i),
                Err(e) => Token::Error(
                    TokenError::new(self.location(start), "parse int failed").with_source(e),
                ),
            }
        }
    }

    fn eat_string(&mut self) -> Token<'i> {
        match self.eat_qoutes('"') {
            Ok(s) => Token::Literal(Literal::String(s)),
            Err(t) => t,
        }
    }

    fn eat_char(&mut self) -> Token<'i> {
        let pos = self.position;
        match self.eat_qoutes('\'') {
            Ok(s) => {
                if s.chars().count() == 1 {
                    Token::Literal(Literal::Char(s.chars().next().unwrap()))
                } else {
                    let location = self.location(pos);
                    Token::Error(TokenError::new(location, "too many char for CharLit"))
                }
            }
            Err(t) => t,
        }
    }

    fn eat_comment(&mut self) -> Token<'i> {
        self.advance(2);

        let s = self.eat_while(|c| c != '\n');

        self.advance(1);

        Token::Comment(s)
    }

    fn eat_qoutes(&mut self, qoute: char) -> Result<String, Token<'i>> {
        let mut ret = String::new();

        self.advance(1); // skip qoute

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
                                    let location = self.location(self.position);
                                    return Err(Token::Error(TokenError::new(
                                        location,
                                        "unknown char after escape",
                                    )));
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

        Err(Token::Eof)
    }

    fn eat_tree(&mut self) -> Token<'i> {
        let mut group = Vec::new();

        let close = match self.next_char().unwrap() {
            '(' => (TreeType::Group, Token::Punctuation(Punctuation::RParen)),
            '[' => (TreeType::Array, Token::Punctuation(Punctuation::RSquare)),
            '{' => (TreeType::Block, Token::Punctuation(Punctuation::RBracket)),
            _ => unreachable!(),
        };

        loop {
            match self.next_token() {
                Token::Eof => {
                    let location = self.location(self.position);
                    return Token::Error(TokenError::new(location, "unclose group"));
                }
                t if t == close.1 => {
                    return Token::TokenTree(close.0, group);
                }
                t => {
                    group.push(t);
                }
            }
        }
    }

    pub fn stripped(self) -> StrippedTokenizer<'i, Tokenizer<'i>> {
        let a = self.into_iter();
        StrippedTokenizer::new(a)
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
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            t => Some(t),
        }
    }
}

#[derive(Clone)]
pub struct StrippedTokenizer<'i, I: IntoIterator<Item = Token<'i>>> {
    iter: I,
}

impl<'i, I: IntoIterator<Item = Token<'i>>> StrippedTokenizer<'i, I> {
    pub fn with_input(filename: &str, input: &'i str) -> StrippedTokenizer<'i, Tokenizer<'i>> {
        let iter = Tokenizer::new(filename, input);
        StrippedTokenizer::new(iter)
    }

    pub fn new(iter: I) -> Self {
        StrippedTokenizer { iter }
    }

    pub fn from_iter(iter: I) -> Self {
        StrippedTokenizer { iter }
    }
}

impl<'i, I: Iterator<Item = Token<'i>>> Iterator for StrippedTokenizer<'i, I> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(Token::Comment(_)) => self.next(),
            Some(Token::Whitespace(_)) => self.next(),
            Some(t) => Some(t),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ws() {
        let inputs: Vec<(&str, &str)> = vec![
            ("", ""),
            ("\t   ", "\t   "),
            (" \t\n\rabc", " \t\n\r"),
            ("abc", ""),
        ];

        for i in &inputs {
            let mut tokenizer = Tokenizer::new("", i.0);

            let ws = tokenizer.eat_whitespace();

            // println!("pos: >{}<", tokenizer.pos())
            assert_eq!(ws, Token::whitespace(i.1));
        }
    }

    #[test]
    fn test_tokenizer() {
        let inputs: Vec<(&str, Vec<Token>)> = vec![
            (
                "a b\tc",
                vec![
                    Token::ident("a"),
                    Token::whitespace(" "),
                    Token::ident("b"),
                    Token::whitespace("\t"),
                    Token::ident("c"),
                ],
            ),
            (
                "hello,123,123.4",
                vec![
                    Token::ident("hello"),
                    Token::punctuation(","),
                    Token::int(123),
                    Token::punctuation(","),
                    Token::float(123.4),
                ],
            ),
            (
                r#"let a=b+c*123;"#,
                vec![
                    Token::Keyword(Keyword::Let),
                    Token::whitespace(" "),
                    Token::ident("a"),
                    Token::punctuation("="),
                    Token::ident("b"),
                    Token::punctuation("+"),
                    Token::ident("c"),
                    Token::punctuation("*"),
                    Token::int(123),
                    Token::punctuation(";"),
                ],
            ),
        ];

        for i in &inputs {
            let mut tokenizer = Tokenizer::new("", i.0);

            let mut ret = Vec::new();

            loop {
                let t = tokenizer.next_token();
                if t == Token::Eof {
                    break;
                }

                ret.push(t);
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
            if let Token::TokenTree(ty, group) = token {
                for g in group {
                    print!("->");
                    print(g)
                }
            } else {
                println!("{token:?}");
            }
        }

        for i in inputs {
            let tokenizer = Tokenizer::new("", i);
            for t in tokenizer {
                print(&t);
                println!("{t:?}");
            }
        }
    }
}
