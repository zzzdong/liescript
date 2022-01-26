use std::ops::RangeBounds;
use std::str::CharIndices;
use std::{iter::Peekable, str::Chars};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::value,
    sequence::preceded, IResult,
};

use crate::error::{Location, ParserError};
use crate::token::{Delimiter, Keyword, Operator, Token};

struct Tokenizer<'i> {
    input: Peekable<Chars<'i>>,
    filename: String,
}

impl<'i> Tokenizer<'i> {
    pub fn new(filename: &str, input: &'i str) -> Self {
        let input = input.chars().peekable();

        Tokenizer {
            input,
            filename: filename.to_string(),
        }
    }

    fn location(&self, pos: usize) -> Location {
        let mut line = 1;
        let mut column = 1;

        // for i in 0..=pos {
        //     if self.input[i] == '\n' {
        //         line += 1;
        //         column = 1;
        //     }

        //     column += 1;
        // }

        Location::new(&self.filename, line, column)
    }

    pub fn next_token(&mut self) -> Result<Token, ParserError> {
        while let Some(c) = self.peek() {
            match c {
                '\t' | '\n' | '\x0C' | '\r' | ' ' => return self.eat_whitespace(),
                '_' | 'a'..='z' | 'A'..='Z' => return self.eat_ident(),
                '0'..='9' => return self.eat_number(),
                '"' => return self.eat_string(),
                _ => {
                    if Delimiter::CHARS.contains(&c) {
                        return self.eat_delimiter();
                    }
                    if let Some(op) = self.try_eat_operator() {
                        return Ok(Token::Operator(op));
                    }

                    dbg!(c);
                    unreachable!()
                }
            }
        }

        Ok(Token::Eof)
    }

    fn len(&self) -> usize {
        self.input.clone().count()
    }

    pub fn is_eof(&mut self) -> bool {
        self.input.peek().is_none()
    }

    fn next(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
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
            ret.push(self.input.next().unwrap());
        }

        ret
    }

    pub fn eat_whitespace(&mut self) -> Result<Token, ParserError> {
        let ws = self.eat_while(|c| c.is_whitespace());

        Ok(Token::Whitespace(ws))
    }

    pub fn eat_ident(&mut self) -> Result<Token, ParserError> {
        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        if Keyword::STRS.contains(&&got.as_str()) {
            let kw = Keyword::from_str(got.as_str());
            return Ok(Token::Keywrod(kw));
        }

        // TODO: keyword

        Ok(Token::Ident(got))
    }

    fn eat_number(&mut self) -> Result<Token, ParserError> {
        let start = 0;
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
                Ok(i) => Ok(Token::float(i)),
                Err(e) => {
                    Err(ParserError::new(self.location(start), "parse float failed").with_source(e))
                }
            }
        } else {
            match num.parse::<i64>() {
                Ok(i) => Ok(Token::int(i)),
                Err(e) => {
                    Err(ParserError::new(self.location(start), "parse int failed").with_source(e))
                }
            }
        }
    }

    fn eat_string(&mut self) -> Result<Token, ParserError> {
        let mut ret = String::new();

        self.next().unwrap();

        let mut is_backslash_previous = false;

        while let Some(c) = self.next() {
            match c {
                '"' => {
                    return Ok(Token::StringLit(ret));
                }
                '\\' => {
                    if is_backslash_previous {
                        ret.push(c);
                        is_backslash_previous = false;
                    } else {
                        is_backslash_previous = true;
                    }
                }
                _ => {
                    if is_backslash_previous {
                        let ch = match c {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            '"' => '"',
                            _ => {
                                let location = self.location(0);
                                return Err(ParserError::new(
                                    location,
                                    "unknown char after escape",
                                ));
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

        Ok(Token::Eof)
    }

    fn eat_delimiter(&mut self) -> Result<Token, ParserError> {
        let ch = self.next().unwrap();

        let delimiter = Delimiter::from_char(ch);

        Ok(Token::Delimiter(delimiter))
    }

    fn try_eat_operator(&mut self) -> Option<Operator> {
        let mut input = self.input.clone();

        let first = input.next().unwrap();
        match input.peek() {
            Some(second) => {
                let got: String = [first, *second].iter().collect();
                let got = got.as_str();
                if Operator::STRS.contains(&&got) {
                    self.input.next();
                    self.input.next();
                    return Some(Operator::from_str(got));
                }
            }
            None => {}
        }

        let got: String = [first].iter().collect();
        let got = got.as_str();
        if Operator::STRS.contains(&&got) {
            self.input.next();
            return Some(Operator::from_str(got));
        }

        // assert!(ch == ',');

        None
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

            let ws = tokenizer.eat_whitespace().unwrap();

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
                    Token::decimal(','),
                    Token::int(123),
                    Token::decimal(','),
                    Token::float(123.4),
                ],
            ),
            (
                r#"hello("hello")123"#,
                vec![
                    Token::ident("hello"),
                    Token::decimal('('),
                    Token::string("hello"),
                    Token::decimal(')'),
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

            while let Ok(t) = tokenizer.next_token() {
                if t == Token::Eof {
                    break;
                }

                ret.push(t);
            }

            assert_eq!(ret, i.1);
        }
    }
}
