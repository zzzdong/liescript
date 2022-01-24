use std::{iter::Peekable, str::Chars};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::value,
    sequence::preceded, IResult,
};

use crate::error::{Location, ParserError};
use crate::token::{Delimiter, Punctuation, Token};

struct Tokenizer {
    input: Vec<char>,
    pos: usize,
    filename: String,
}

impl Tokenizer {
    pub fn new(filename: &str, input: &str) -> Self {
        let input = input.chars().collect();

        Tokenizer {
            input,
            pos: 0,
            filename: filename.to_string(),
        }
    }

    fn location(&self, pos: usize) -> Location {
        let mut line = 0;
        let mut column = 0;

        for i in 0..=pos {
            if self.input[i] == '\n' {
                line += 1;
                column = 0;
            }

            column += 1;
        }

        Location::new(&self.filename, line, column)
    }

    pub fn next(&mut self) -> Result<Token, ParserError> {
        while let Some(c) = self.peek() {
            if c.is_ascii_whitespace() {
                return self.eat_whitespace();
            } else if c.is_ascii_alphabetic() || *c == '_' {
                return self.eat_ident();
            } else if c.is_ascii_digit() {
                return self.eat_number();
            }

            match c {
                ' ' | '\t' | '\n' | '\r' => return self.eat_whitespace(),
                '_' | 'a'..='z' | 'A'..='Z' => return self.eat_ident(),
                '0'..='9' => return self.eat_number(),
                '(' | ')' | '[' | ']' | '{' | '}' => {
                    return Ok(Token::Delimiter(self.eat_delimiter(*c)))
                }
                // '"' => return self.eat_qoute(),
                _ => {
                    dbg!(c, self.pos);
                    unreachable!()
                }
            }
        }

        Ok(Token::Eof)
    }

    fn starts_with(&self, p: &str) -> Option<usize> {
        let mut count = 0;

        for (i, c) in p.chars().enumerate() {
            match self.input.get(self.pos + i) {
                Some(ch) => {
                    if *ch == c {
                        count += 1;
                    } else {
                        return None;
                    }
                }
                None => return None,
            }
        }

        Some(count)
    }

    fn has_at_least(&self, n: usize) -> bool {
        self.pos + n < self.input.len()
    }

    pub fn rest(&self) -> String {
        self.input[self.pos..].into_iter().collect()
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    fn at(&self, pos: usize) -> char {
        self.input[pos]
    }

    fn get(&self, range: std::ops::Range<usize>) -> String {
        self.input[range].into_iter().collect()
    }

    fn len(&self) -> usize {
        self.input.len()
    }

    fn advance(&mut self, n: usize) -> usize {
        self.pos += n;
        self.pos
    }

    fn reset(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn is_eof(&mut self) -> bool {
        self.pos == self.input.len()
    }

    fn peek(&self) -> Option<&char> {
        self.input.get(self.pos)
    }

    fn peekn(&self, n: usize) -> Option<String> {
        self.input
            .get(self.pos..self.pos + n)
            .map(|items| items.iter().collect())
    }

    fn eat_while<P>(&mut self, mut predicate: P) -> String
    where
        P: FnMut(char) -> bool,
    {
        let start = self.pos;
        let mut end = self.input.len();

        for i in self.pos..self.input.len() {
            if !predicate(self.at(i)) {
                end = i;
                break;
            }
        }

        self.pos = end;

        self.get(start..end)
    }

    pub fn eat_whitespace(&mut self) -> Result<Token, ParserError> {
        let ws = self.eat_while(|c| c.is_whitespace());

        Ok(Token::Whitespace(ws))
    }

    pub fn eat_ident(&mut self) -> Result<Token, ParserError> {
        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        // TODO: keyword

        Ok(Token::Ident(got))
    }

    fn eat_number(&mut self) -> Result<Token, ParserError> {
        let start = self.pos;
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

    // fn eat_qoute(&mut self) -> Result<Token, ParserError> {
    //     let start = self.pos;
    // }

    fn in_quotes(&mut self, quote: char) -> Result<String, ParserError> {
        let mut ret = String::new();

        let start = self.pos();
        let mut pos = self.pos();

        loop {
            match self.at(pos) {
                '\\' => {
                    if pos + 1 < self.len() {
                        pos += 1;
                        let ch = self.at(pos);
                        let got = match ch {
                            '\\' => '\\',
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            ch => {
                                if quote == ch {
                                    ch
                                } else {
                                    ret.push('\\');
                                    ch
                                }
                            }
                        };
                    } else {
                        let location = self.location(pos);
                        return Err(ParserError::new(location, "incomplete string"))
                    }
                }
                c => {
                    if c == quote {
                        self.reset(pos);
                        return Ok(self.get(start..pos))
                    }
                }
            }
        }
    }

    fn eat_delimiter(&mut self, c: char) -> Delimiter {
        match c {
            '(' => Delimiter::LParen,
            ')' => Delimiter::RParen,
            '[' => Delimiter::LBracket,
            ']' => Delimiter::RBracker,
            '{' => Delimiter::LBracket,
            '}' => Delimiter::RBracker,
            _ => unreachable!(),
        }
    }

    // fn parse_delimiter(&self) -> IResult<&'s str, Delimiter> {
    //     alt((
    //         value(Delimiter::Comman, tag(",")),
    //         value(Delimiter::Semicolon, tag(";")),
    //         value(Delimiter::Dot, tag(".")),
    //         value(Delimiter::Assign, tag("=")),
    //         value(Delimiter::Reture, tag("->")),
    //         value(Delimiter::LParen, tag("(")),
    //         value(Delimiter::RParen, tag(")")),
    //         value(Delimiter::LSquare, tag("[")),
    //         value(Delimiter::RSquare, tag("]")),
    //         value(Delimiter::LBracket, tag("{")),
    //         value(Delimiter::RBracker, tag("}")),
    //     ))(self.pos)
    // }

    // fn parse_op(&self) -> IResult<&'s str, Operator> {
    //     alt((
    //         value(Operator::Not, tag("!")),
    //         value(Operator::Question, tag("?")),
    //         value(Operator::Plus, tag("+")),
    //         value(Operator::Minus, tag("-")),
    //         value(Operator::Mul, tag("*")),
    //         value(Operator::Div, tag("/")),
    //         value(Operator::Mod, tag("%")),
    //         value(Operator::Pow, tag("^")),
    //         value(Operator::LShift, tag("<<")),
    //         value(Operator::RShift, tag(">>")),
    //         value(Operator::BitOr, tag("|")),
    //         value(Operator::BitAnd, tag("&")),
    //         value(Operator::And, tag("&&")),
    //         value(Operator::Or, tag("||")),
    //         value(Operator::Eq, tag("==")),
    //         value(Operator::NotEq, tag("!=")),
    //         value(Operator::Lt, tag("<")),
    //         value(Operator::LtE, tag("<=")),
    //         value(Operator::Gt, tag(">")),
    //         value(Operator::GtE, tag(">=")),
    //     ))(self.pos)
    // }
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
                "hello 123 123.4",
                vec![
                    Token::ident("hello"),
                    Token::whitespace(" "),
                    Token::int(123),
                    Token::whitespace(" "),
                    Token::float(123.4),
                ],
            ),
        ];

        for i in &inputs {
            let mut tokenizer = Tokenizer::new("", i.0);

            let mut ret = Vec::new();

            while let Ok(t) = tokenizer.next() {
                if t == Token::Eof {
                    break;
                }

                ret.push(t);
            }

            // println!("pos: >{}<", tokenizer.pos())
            assert_eq!(ret, i.1);
        }
    }
}
