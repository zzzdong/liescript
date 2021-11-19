use std::{iter::Peekable, str::Chars};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::char, combinator::value, IResult,
};

use crate::token::{Delimiter, Operator, Token};

struct Tokenizer<'s> {
    input: &'s str,
    pos: &'s str,
    cur_line: usize,
    cur_column: usize,
}

impl<'s> Tokenizer<'s> {
    pub fn new(input: &'s str) -> Self {
        Tokenizer {
            input,
            pos: input,
            cur_line: 0,
            cur_column: 0,
        }
    }

    pub fn is_eof(&mut self) -> bool {
        self.pos.chars().next().is_none()
    }

    fn peek(&self) -> Option<char> {
        self.pos.chars().next()
    }

    pub fn next(&mut self) -> Token {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\t' | '\n' | '\r'=> self.eat_whitespace(),
                '_' | 'a'..='z' | 'A'..='Z' => 

            }
        }
    }

    fn eat_whitespace(&mut self) {
        let mut tmp = self.pos.chars();
        tmp.take_while(|c|c.is_ascii_whitespace());

        self.pos = tmp.as_str();
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
