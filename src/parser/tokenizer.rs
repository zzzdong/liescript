use std::borrow::Cow;
use std::fmt;
use std::str::Chars;

use super::token::{Token, TokenStream};
use crate::ast::{Identifier, Keyword, Literal, Symbol};
use crate::diagnostic::{Pos, Span, Spanned};

#[derive(Debug)]
pub struct TokenError {
    pub(crate) detail: Option<Cow<'static, str>>,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
    span: Option<Span>,
}

impl TokenError {
    pub fn new<D: Into<Cow<'static, str>>>(detail: D) -> TokenError {
        TokenError {
            detail: Some(detail.into()),
            source: None,
            span: None,
        }
    }

    pub fn with_source<E: std::error::Error + Send + Sync + 'static>(mut self, source: E) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn with_span(mut self, span: Span) -> TokenError {
        self.span = Some(span);
        self
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

    fn pos(&self) -> Pos {
        self.pos
    }

    fn new_token(&self, token: Token, span: Span) -> Spanned<Token> {
        Spanned::new(token, span)
    }

    pub fn next_token(&mut self) -> Result<Spanned<Token>, TokenError> {
        let start = self.pos();

        self.next_token_inner()
            .map(|token| self.new_token(token, Span::new(start, self.pos())))
            .map_err(|e| e.with_span(Span::new(start, self.pos())))
    }

    fn next_token_inner(&mut self) -> Result<Token, TokenError> {
        match self.peek() {
            Some(c) => {
                match c {
                    c if c.is_ascii_whitespace() => self.eat_whitespace(),
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
            None => Ok(Token::Eof),
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
        self.chars.next().inspect(|&c| {
            self.pos.offset += c.len_utf8();
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += 1;
            }
        })
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
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

    pub fn eat_whitespace(&mut self) -> Result<Token, TokenError> {
        let ws = self.eat_while(|c| c.is_ascii_whitespace());

        Ok(Token::Whitespace(ws.to_string()))
    }

    pub fn eat_ident(&mut self) -> Result<Token, TokenError> {
        if self.starts_with("b\'") {
            return self.eat_byte();
        }
        if self.starts_with("b\"") {
            return self.eat_byte_slice();
        }

        let got = self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');

        let token = match got {
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(false)),
            kw if Keyword::STRS.contains(&kw) => {
                let kw = Keyword::from_str(kw);
                Token::Keyword(kw)
            }
            _ => Token::Ident(Identifier::new(got)),
        };

        Ok(token)
    }

    fn eat_number(&mut self) -> Result<Token, TokenError> {
        // binary number, eg: 0b10101
        if self.starts_with("0b") || self.starts_with("0B") {
            return self.eat_bin_number();
        }
        // octal number, eg: 0o123
        if self.starts_with("0o") || self.starts_with("0O") {
            return self.eat_oct_number();
        }
        // hex number, eg: 0x123
        if self.starts_with("0x") || self.starts_with("0X") {
            return self.eat_hex_number();
        }

        let start = self.pos();

        let mut is_float = false;

        let i = self.eat_while(|c| c.is_ascii_digit() || c == '_');
        if self.peek() == Some('.') {
            self.advance(1);
            is_float = true;
            let f = self.eat_while(|c| c.is_ascii_digit() || c == '_');

            let mut number = i.to_string();
            number.push('.');
            number.push_str(f);

            let number = number.replace("_", "");

            return number.parse::<f64>().map(Token::float).map_err(|e| {
                TokenError::new("parse float failed")
                    .with_source(e)
                    .with_span(Span::new(start, self.pos()))
            });
        }

        let number = i.to_string().replace("_", "");

        number.parse::<i64>().map(Token::int).map_err(|e| {
            TokenError::new("parse float failed")
                .with_source(e)
                .with_span(Span::new(start, self.pos()))
        })
    }

    fn eat_bin_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| matches!(c, '0' | '1'));
        let i = i64::from_str_radix(s, 2)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_oct_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| matches!(c, '0'..='7'));
        let i = i64::from_str_radix(s, 8)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_hex_number(&mut self) -> Result<Token, TokenError> {
        self.advance(2);
        let s = self.eat_while(|c| c.is_ascii_hexdigit());
        let i = i64::from_str_radix(s, 16)
            .map_err(|e| TokenError::new("parse int failed").with_source(e))?;
        Ok(Token::Literal(Literal::Integer(i)))
    }

    fn eat_byte(&mut self) -> Result<Token, TokenError> {
        self.advance(1);
        let token = self.eat_char()?;
        match token {
            Token::Literal(Literal::Char(ch)) if ch as u32 <= 0xff => {
                Ok(Token::Literal(Literal::Byte(ch as u8)))
            }
            _ => Err(TokenError::new("invalid byte char literal")),
        }
    }

    fn eat_byte_slice(&mut self) -> Result<Token, TokenError> {
        self.advance(1);
        let token = self.eat_string()?;
        match token {
            Token::Literal(Literal::String(s)) => {
                Ok(Token::Literal(Literal::ByteSlice(s.into_bytes())))
            }
            _ => Err(TokenError::new("invalid byte slice string literal")),
        }
    }

    fn eat_string(&mut self) -> Result<Token, TokenError> {
        self.eat_qoutes('"')
            .map(|s| Token::Literal(Literal::String(s)))
    }

    fn eat_char(&mut self) -> Result<Token, TokenError> {
        let s = self.eat_qoutes('\'')?;

        if s.chars().count() == 1 {
            Ok(Token::Literal(Literal::Char(s.chars().next().unwrap())))
        } else {
            Err(TokenError::new("too many char for CharLit"))
        }
    }

    fn eat_comment(&mut self) -> Result<Token, TokenError> {
        self.advance(2);

        let s = self.eat_while(|c| c != '\n');

        self.advance(1); // eat `\n`

        Ok(Token::Comment(s.to_string()))
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

    fn eat_tree(&mut self) -> Result<Spanned<Token>, TokenError> {
        let mut group = Vec::new();

        let _open = self.next_char().unwrap();

        let group_start = self.pos();

        loop {
            let start = self.pos();

            let token = self.next_token()?;

            match token.inner {
                Token::Eof => {
                    return Err(TokenError::new("unclose group"));
                }
                Token::Symbol(Symbol::RParen)
                | Token::Symbol(Symbol::RBracket)
                | Token::Symbol(Symbol::RBrace) => {
                    return Ok(
                        self.new_token(Token::Tree(group), Span::new(group_start, self.pos()))
                    );
                }

                t => {
                    group.push(self.new_token(t, Span::new(start, self.pos())));
                }
            }
        }
    }

    fn eat_symbol(&mut self, peek: char) -> Result<Token, TokenError> {
        // try 3 byte
        if self.starts_with("..=") {
            self.advance(3);
            return Ok(Token::Symbol(Symbol::DotDotEq));
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
                // shift
                "<<" | ">>" |
                // others
                "::" | ".." | "->" | "=>" => {
                    Symbol::from_str(pat).ok().map(Token::Symbol)
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
                Symbol::from_str(&peek.to_string()).ok().map(Token::Symbol)
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
    type Item = Result<Spanned<Token>, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(t) if t.inner == Token::Eof => None,
            t => Some(t),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize_identifier() {
        let input = "let x = 5;";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::Let),
                Token::Ident("x".into()),
                Token::Symbol(Symbol::Equal),
                Token::Literal(Literal::Integer(5)),
                Token::Symbol(Symbol::Semicolon)
            ]
        );
    }

    #[test]
    fn test_tokenize_number() {
        let input = "123 45.67";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Integer(123)),
                Token::Literal(Literal::Float(45.67)),
            ]
        );
    }

    #[test]
    fn test_tokenize_special_number_formats() {
        let input = "0b101 0o644 0xFF";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Integer(5)),   // 0b101 = 5
                Token::Literal(Literal::Integer(420)), // 0o644 = 420
                Token::Literal(Literal::Integer(255)), // 0xFF = 255
            ]
        );
    }

    #[test]
    fn test_tokenize_string_char() {
        let input = "\"hello world\" 'a'";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::String("hello world".to_string())),
                Token::Literal(Literal::Char('a')),
            ]
        );
    }

    #[test]
    fn test_tokenize_byte_string_char() {
        let input = r#"b"hello world" b'a'"#;
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::ByteSlice("hello world".as_bytes().to_vec())),
                Token::Literal(Literal::Byte(b'a')),
            ]
        );
    }

    #[test]
    fn test_tokenize_comment() {
        let input = "// This is a comment\nlet y = 10;";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Comment(" This is a comment".to_string()),
                Token::Keyword(Keyword::Let),
                Token::Ident(Identifier::new("y")),
                Token::Symbol(Symbol::Equal),
                Token::Literal(Literal::Integer(10)),
                Token::Symbol(Symbol::Semicolon),
            ]
        );
    }

    #[test]
    fn test_tokenize_operator() {
        let input = "+ - * / % == != > < =>";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::Plus),
                Token::Symbol(Symbol::Minus),
                Token::Symbol(Symbol::Star),
                Token::Symbol(Symbol::Slash),
                Token::Symbol(Symbol::Percent),
                Token::Symbol(Symbol::EqualEqual),
                Token::Symbol(Symbol::NotEqual),
                Token::Symbol(Symbol::GreatThen),
                Token::Symbol(Symbol::LessThan),
                Token::Symbol(Symbol::FatArrow),
            ]
        );
    }

    #[test]
    fn test_tokenize_keyword() {
        let input = "if else for while loop match";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Keyword(Keyword::Else),
                Token::Keyword(Keyword::For),
                Token::Keyword(Keyword::While),
                Token::Keyword(Keyword::Loop),
                Token::Keyword(Keyword::Match),
            ]
        );
    }

    #[test]
    fn test_tokenize_bool() {
        let input = "true false";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Literal(Literal::Bool(true)),
                Token::Literal(Literal::Bool(false)),
            ]
        );
    }

    #[test]
    fn test_tokenize_parentheses() {
        let input = "()[]{}";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::LParen),
                Token::Symbol(Symbol::RParen),
                Token::Symbol(Symbol::LBracket),
                Token::Symbol(Symbol::RBracket),
                Token::Symbol(Symbol::LBrace),
                Token::Symbol(Symbol::RBrace),
            ]
        );
    }

    #[test]
    fn test_tokenize_more_symbols() {
        let input = "!= += -= *= /= %= ^= &= |= << >> -> ::";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Symbol(Symbol::NotEqual),
                Token::Symbol(Symbol::PlusEqual),
                Token::Symbol(Symbol::MinusEqual),
                Token::Symbol(Symbol::StarEqual),
                Token::Symbol(Symbol::SlashEqual),
                Token::Symbol(Symbol::PercentEqual),
                Token::Symbol(Symbol::CaretEqual),
                Token::Symbol(Symbol::AndEqual),
                Token::Symbol(Symbol::OrEqual),
                Token::Symbol(Symbol::LShift),
                Token::Symbol(Symbol::RShift),
                Token::Symbol(Symbol::RArrow),
                Token::Symbol(Symbol::ColonColon),
            ]
        );
    }

    #[test]
    fn test_tokenize_mixed_statement() {
        let input = "if (x > 5) { return x; }";
        let tokenizer = Tokenizer::new(input);

        let tokens = tokenizer
            .into_iter()
            .filter_map(|r| r.ok().map(|span| span.inner))
            .filter(|token| !token.is_whitespace())
            .collect::<Vec<_>>();

        assert_eq!(
            tokens,
            vec![
                Token::Keyword(Keyword::If),
                Token::Symbol(Symbol::LParen),
                Token::Ident(Identifier::new("x")),
                Token::Symbol(Symbol::GreatThen),
                Token::Literal(Literal::Integer(5)),
                Token::Symbol(Symbol::RParen),
                Token::Symbol(Symbol::LBrace),
                Token::Keyword(Keyword::Return),
                Token::Ident(Identifier::new("x")),
                Token::Symbol(Symbol::Semicolon),
                Token::Symbol(Symbol::RBrace),
            ]
        );
    }

    #[test]
    fn test_tokenizer_error() {
        let input = "0xZF";
        let mut tokenizer = Tokenizer::new(input);
        let t = tokenizer.next();
        println!("{:?}", t);
    }
}
