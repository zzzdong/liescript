use std::{borrow::Cow, str::Chars};

use crate::token::{Delimiter, Keyword, Location, Operator, Token, TokenError};

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

    pub fn next_token(&mut self) -> Token {
        while let Some(c) = self.peek() {
            match c {
                '\t' | '\n' | '\x0C' | '\r' | ' ' => return self.eat_whitespace(),
                '_' | 'a'..='z' | 'A'..='Z' => return self.eat_ident(),
                '0'..='9' => return self.eat_number(),
                '\'' => return self.eat_char(),
                '"' => return self.eat_string(),
                _ => {
                    // try 2 byte
                    if self.has_at_lease(2) {
                        let pat = &self.chars.clone().as_str()[..2];
                        let token = match pat {
                            "//" => {
                                return self.eat_comment();
                            }
                            "::" | "->" => Some(Token::Delimiter(Delimiter::RArrow)),
                            "+=" | "-=" | "*=" | "/=" | "%=" | "^=" | "<<" | ">>" | "&&" | "||"
                            | "==" | "!=" | "<=" | ">=" | ".." => {
                                Operator::from_str(pat).ok().map(Token::Operator)
                            }
                            _ => None,
                        };

                        if let Some(t) = token {
                            self.advance(2);
                            return t;
                        }
                    }

                    // try 1 byte
                    let pat = &self.chars.clone().as_str()[..1];
                    let token = match pat {
                        "(" | ")" | "[" | "]" | "{" | "}" | "," | ":" | ";" | "=" | "#" => {
                            Delimiter::from_str(pat).ok().map(Token::Delimiter)
                        }
                        "!" | "?" | "+" | "-" | "*" | "/" | "%" | "^" | "<" | ">" | "&" | "." => {
                            Operator::from_str(pat).ok().map(Token::Operator)
                        }
                        _ => None,
                    };

                    if let Some(t) = token {
                        self.advance(1);
                        return t;
                    }

                    dbg!(c, self.location(self.position));
                    // unreachable!()

                    self.advance(1);
                    return Token::Unknown(c);
                }
            }
        }

        Token::Eof
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

    fn eat_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool,
    {
        let mut ret = String::new();

        while let Some(ch) = self.peek() {
            if !predicate(ch) {
                return ret;
            }
            ret.push(self.next_char().unwrap());
        }

        ret
    }

    pub fn eat_whitespace(&mut self) -> Token {
        let ws = self.eat_while(|c| c.is_whitespace());

        Token::Whitespace(ws)
    }

    pub fn eat_ident(&mut self) -> Token {
        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        if got == "true" {
            return Token::BoolLit(false);
        } else if got == "false" {
            return Token::BoolLit(false);
        }

        if Keyword::STRS.contains(&&got.as_str()) {
            let kw = Keyword::from_str(got.as_str());
            return Token::Keywrod(kw);
        }

        Token::Ident(got)
    }

    fn eat_number(&mut self) -> Token {
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

    fn eat_string(&mut self) -> Token {
        match self.eat_qoutes('"') {
            Ok(s) => Token::StringLit(s),
            Err(t) => t,
        }
    }

    fn eat_char(&mut self) -> Token {
        let pos = self.position;
        match self.eat_qoutes('\'') {
            Ok(s) => {
                if s.chars().count() == 1 {
                    Token::CharLit(s.chars().next().unwrap())
                } else {
                    let location = self.location(pos);
                    Token::Error(TokenError::new(location, "too many char for CharLit"))
                }
            }
            Err(t) => t,
        }
    }

    fn eat_comment(&mut self) -> Token {
        self.advance(2);

        let s = self.eat_while(|c| c != '\n');

        self.advance(1);

        Token::Comment(s)
    }

    fn eat_qoutes(&mut self, qoute: char) -> Result<String, Token> {
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
                                    let location = self.location(0);
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
}

impl<'i> Iterator for Tokenizer<'i> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::Eof => None,
            Token::Whitespace(_) => self.next(),
            Token::Comment(_) => self.next(),
            t => Some(t),
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
                    Token::delimiter(","),
                    Token::int(123),
                    Token::delimiter(","),
                    Token::float(123.4),
                ],
            ),
            (
                r#"hello("hello")123"#,
                vec![
                    Token::ident("hello"),
                    Token::delimiter("("),
                    Token::string("hello"),
                    Token::delimiter(")"),
                    Token::int(123),
                ],
            ),
            (
                r#"let a=b+c*123;"#,
                vec![
                    Token::Keywrod(Keyword::Let),
                    Token::whitespace(" "),
                    Token::ident("a"),
                    Token::Delimiter(Delimiter::Eq),
                    Token::ident("b"),
                    Token::Operator(Operator::Plus),
                    Token::ident("c"),
                    Token::Operator(Operator::Mul),
                    Token::int(123),
                    Token::Delimiter(Delimiter::Semicolon),
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
}
