use crate::lexical::{
    Brace, Bracket, Keyword, LiteralSpan, Paren, Punctuated, Symbol, Token, TokenSpan,
};
use crate::parser::parse::{Parse, ParseError, ParseStream};
use crate::syntax::types::*;
use crate::syntax::{Expression, TypePath};

fn parse_type(stream: &mut ParseStream) -> Result<Type, ParseError> {
    let peek = stream.peek().ok_or(ParseError::eof())?;

    match peek.value() {
        Token::Symbol(Symbol::Underscore) => {
            let underscore_token = stream.consume()?;
            Ok(Type::Inferred(InferredType { underscore_token }))
        }
        Token::Symbol(Symbol::Not) => Type::parse_never(stream),
        Token::Symbol(Symbol::LParen) => Type::parse_tuple(stream),
        Token::Symbol(Symbol::LBracket) => match stream.try_parse(Type::parse_array) {
            Some(arr) => Ok(arr),
            None => Type::parse_slice(stream),
        },
        Token::Symbol(Symbol::And) => Type::parse_reference(stream),
        Token::Keyword(Keyword::Fn) => Type::parse_fn(stream),
        Token::Ident(_) => Type::parse_path(stream),
        _ => Type::parse_primitive(stream),
    }
}

impl Parse for Type {
    fn parse(stream: &mut ParseStream) -> Result<Type, ParseError> {
        parse_type(stream)
    }
}

impl Type {
    fn parse_never(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let bang_token = stream.expect(&Token::Symbol(Symbol::Not))?;
        Ok(Type::Never(NeverType { bang_token }))
    }

    fn parse_primitive(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.consume()?;

        match token.value() {
            Token::Keyword(Keyword::Any) => Ok(Type::Primitive(Primitive::Any(token))),
            Token::Keyword(Keyword::Bool) => Ok(Type::Primitive(Primitive::Boolean(token))),
            Token::Keyword(Keyword::Byte) => Ok(Type::Primitive(Primitive::Byte(token))),
            Token::Keyword(Keyword::Int) => Ok(Type::Primitive(Primitive::Integer(token))),
            Token::Keyword(Keyword::Float) => Ok(Type::Primitive(Primitive::Float(token))),
            Token::Keyword(Keyword::Char) => Ok(Type::Primitive(Primitive::Char(token))),
            Token::Keyword(Keyword::String) => Ok(Type::Primitive(Primitive::String(token))),
            _ => Err(ParseError::new("Expected primitive type")
                .with_span(token.span())
                .with_expected("primitive type keyword")),
        }
    }

    fn parse_tuple(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LParen))?;
        // empty tuple
        if stream.next_is(Token::Symbol(Symbol::RParen)) {
            let close = stream.expect(&Token::Symbol(Symbol::RParen))?;
            return Ok(Type::Tuple(TupleType {
                paren_token: Paren::new(open, close),
                elems: Punctuated::new(),
            }));
        }

        // try parse parenthesized type, e.g: `(i32)`
        if let Some(ty) = stream.try_parse(|s| {
            let ty = Type::parse(s)?;
            let close = s.expect(&Token::Symbol(Symbol::RParen))?;
            Ok(Type::Parenthesized(ParenthesizedType {
                paren_token: Paren::new(open.clone(), close),
                ty: Box::new(ty),
            }))
        }) {
            return Ok(ty);
        };

        let mut elems = Punctuated::new();

        while !stream.next_is(&Token::Symbol(Symbol::RParen)) {
            let item = Type::parse(stream)?;
            if stream.next_is(&Token::Symbol(Symbol::Comma)) {
                let comma = stream.consume()?;
                elems.push(item, comma);
            } else {
                elems.last = Some(Box::new(item));
                break;
            }
        }

        let close = stream.expect(&Token::Symbol(Symbol::RParen))?;
        Ok(Type::Tuple(TupleType {
            paren_token: Paren::new(open, close),
            elems,
        }))
    }

    fn parse_slice(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LBracket))?;
        let elem = Type::parse(stream)?;
        let close = stream.expect(&Token::Symbol(Symbol::RBracket))?;
        Ok(Type::Slice(SliceType {
            bracket_token: Bracket::new(open, close),
            elem: Box::new(elem),
        }))
    }

    fn parse_array(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open = stream.expect(&Token::Symbol(Symbol::LBracket))?;
        let elem = Type::parse(stream)?;
        let semi = stream.expect(&Token::Symbol(Symbol::Semi))?;
        let len = Expression::parse(stream)?;
        let close = stream.expect(&Token::Symbol(Symbol::RBracket))?;
        Ok(Type::Array(ArrayType {
            bracket_token: Bracket::new(open, close),
            elem: Box::new(elem),
            semi_token: semi,
            len: Box::new(len),
        }))
    }

    fn parse_reference(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let and_token = stream.expect(&Token::Symbol(Symbol::And))?;
        let is_mut = stream
            .try_parse(|s| s.expect(&Token::Keyword(Keyword::Mut)))
            .is_some();
        let ty = Type::parse(stream)?;
        Ok(Type::Reference(ReferenceType {
            and_token,
            is_mut: if is_mut {
                Some(Token::Keyword(Keyword::Mut).into())
            } else {
                None
            },
            ty: Box::new(ty),
        }))
    }

    fn parse_fn(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
        let open = stream.expect(&Token::Symbol(Symbol::LParen))?;

        let inputs = stream.parse_punctuated::<Type>(&Token::Symbol(Symbol::Comma))?;

        let close = stream.expect(&Token::Symbol(Symbol::RParen))?;

        let output = if stream.next_is(&Token::Symbol(Symbol::RArrow)) {
            let rarrow = stream.consume()?;
            Some((rarrow, Box::new(Type::parse(stream)?)))
        } else {
            None
        };

        Ok(Type::BareFn(BareFunctionType {
            fn_token,
            paren_token: Paren::new(open, close),
            inputs,
            output,
        }))
    }

    fn parse_path(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let path = TypePath::parse(stream)?;
        Ok(Type::Path(path))
    }
}

mod tests {
    use super::*;
    use crate::diagnostic::{Span};
    use crate::lexical::{IdentSpan, Identifier, Keyword, Literal, Symbol, Token, TokenStream};
    use crate::source::FileId;
    use crate::syntax::{PathIdentSegment, TypePathSegment, expressions::*};

    /// 解析类型并忽略Span信息进行比较
    macro_rules! assert_type_eq {
        ($input:literal, $expected:expr) => {{
            let actual = parse_type($input).unwrap();
            assert_eq!(actual, $expected);
        }};
    }

    fn path_type(paths: &[&str]) -> TypePath {
        let mut segments = Punctuated::new();
        if let Some((last, rest)) = paths.split_last() {
            for path in rest {
                segments.push(
                    TypePathSegment {
                        ident: PathIdentSegment::Ident(Identifier::new(path).into()),
                        args: None,
                    },
                    Token::Symbol(Symbol::ColonColon).into(),
                );
            }
            segments.push_last(TypePathSegment {
                ident: PathIdentSegment::Ident(Identifier::new(last).into()),
                args: None,
            });
        }

        TypePath {
            leading_colon: None,
            segments,
        }
    }

    fn parse_type(input: &str) -> Result<Type, ParseError> {
        let tokens = TokenStream::parse(FileId::default(), input).unwrap();
        let mut stream = ParseStream::new(&tokens);
        Type::parse(&mut stream)
    }

    #[test]
    fn test_primitive_types() {
        assert_type_eq!(
            "any",
            Type::Primitive(Primitive::Any(Token::Keyword(Keyword::Any).into()))
        );
        assert_type_eq!(
            "bool",
            Type::Primitive(Primitive::Boolean(Token::Keyword(Keyword::Bool).into()))
        );
        assert_type_eq!(
            "byte",
            Type::Primitive(Primitive::Byte(Token::Keyword(Keyword::Byte).into()))
        );
        assert_type_eq!(
            "int",
            Type::Primitive(Primitive::Integer(Token::Keyword(Keyword::Int).into()))
        );
        assert_type_eq!(
            "float",
            Type::Primitive(Primitive::Float(Token::Keyword(Keyword::Float).into()))
        );
        assert_type_eq!(
            "char",
            Type::Primitive(Primitive::Char(Token::Keyword(Keyword::Char).into()))
        );
        assert_type_eq!(
            "string",
            Type::Primitive(Primitive::String(Token::Keyword(Keyword::String).into()))
        );
    }

    #[test]
    fn test_path_types() {
        assert_type_eq!("Foo", Type::Path(path_type(&["Foo"])));
        assert_type_eq!(
            "std::vec::Vec",
            Type::Path(path_type(&["std", "vec", "Vec"]))
        );

        assert_type_eq!("MyStruct", Type::Path(path_type(&["MyStruct"])));

        // FIXME: 泛型参数
        // assert_type_eq!(
        //     "core::option::Option<int>",
        //     Type::Path(path_type(&["core", "option", "Option<int>"]))
        // );
    }

    #[test]
    fn test_parenthesized_types() {
        assert_type_eq!(
            "(int)",
            Type::Parenthesized(ParenthesizedType {
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                ty: Box::new(Type::Primitive(Primitive::Integer(
                    Token::Keyword(Keyword::Int).into()
                ))),
            })
        );
    }

    #[test]
    fn test_tuple_types() {
        assert_type_eq!(
            "()",
            Type::Tuple(TupleType {
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                elems: Punctuated::new(),
            })
        );

        let mut single_elem = Punctuated::new();
        single_elem.push(
            Type::Primitive(Primitive::Integer(Token::Keyword(Keyword::Int).into())),
            Token::Symbol(Symbol::Comma).into(),
        );

        assert_type_eq!(
            "(int,)",
            Type::Tuple(TupleType {
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                elems: single_elem,
            })
        );

        let mut two_elems = Punctuated::new();
        two_elems.push(
            Type::Primitive(Primitive::Integer(Token::Keyword(Keyword::Int).into())),
            Token::Symbol(Symbol::Comma).into(),
        );
        two_elems.last = Some(Box::new(Type::Primitive(Primitive::String(
            Token::Keyword(Keyword::String).into(),
        ))));

        assert_type_eq!(
            "(int, string)",
            Type::Tuple(TupleType {
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                elems: two_elems,
            })
        );
    }

    #[test]
    fn test_never_type() {
        assert_type_eq!(
            "!",
            Type::Never(NeverType {
                bang_token: Token::Symbol(Symbol::Not).into(),
            })
        );
    }

    #[test]
    fn test_reference_types() {
        assert_type_eq!(
            "&int",
            Type::Reference(ReferenceType {
                and_token: Token::Symbol(Symbol::And).into(),
                is_mut: None,
                ty: Box::new(Type::Primitive(Primitive::Integer(
                    Token::Keyword(Keyword::Int).into()
                ))),
            })
        );

        assert_type_eq!(
            "&mut string",
            Type::Reference(ReferenceType {
                and_token: Token::Symbol(Symbol::And).into(),
                is_mut: Some(Token::Keyword(Keyword::Mut).into()),
                ty: Box::new(Type::Primitive(Primitive::String(
                    Token::Keyword(Keyword::String).into()
                ))),
            })
        );
    }

    #[test]
    fn test_array_types() {
        assert_type_eq!(
            "[int; 5]",
            Type::Array(ArrayType {
                bracket_token: Bracket {
                    open: Token::Symbol(Symbol::LBracket).into(),
                    close: Token::Symbol(Symbol::RBracket).into(),
                },
                elem: Box::new(Type::Primitive(Primitive::Integer(
                    Token::Keyword(Keyword::Int).into()
                ))),
                semi_token: Token::Symbol(Symbol::Semi).into(),
                len: Box::new(Expression::Literal(LiteralExpression {
                    lit: Literal::Integer(5).into(),
                })),
            })
        );
    }

    #[test]
    fn test_slice_types() {
        assert_type_eq!(
            "[int]",
            Type::Slice(SliceType {
                bracket_token: Bracket {
                    open: Token::Symbol(Symbol::LBracket).into(),
                    close: Token::Symbol(Symbol::RBracket).into(),
                },
                elem: Box::new(Type::Primitive(Primitive::Integer(
                    Token::Keyword(Keyword::Int).into()
                ))),
            })
        );
    }

    #[test]
    fn test_inferred_type() {
        assert_type_eq!(
            "_",
            Type::Inferred(InferredType {
                underscore_token: Token::Symbol(Symbol::Underscore).into(),
            })
        );
    }

    #[test]
    fn test_bare_function_types() {
        assert_type_eq!(
            "fn()",
            Type::BareFn(BareFunctionType {
                fn_token: Token::Keyword(Keyword::Fn).into(),
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                inputs: Punctuated::new(),
                output: None,
            })
        );

        let mut single_input = Punctuated::new();
        single_input.last = Some(Box::new(Type::Primitive(Primitive::Integer(
            Token::Keyword(Keyword::Int).into(),
        ))));

        assert_type_eq!(
            "fn(int) -> string",
            Type::BareFn(BareFunctionType {
                fn_token: Token::Keyword(Keyword::Fn).into(),
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                inputs: single_input,
                output: Some((
                    Token::Symbol(Symbol::RArrow).into(),
                    Box::new(Type::Primitive(Primitive::String(
                        Token::Keyword(Keyword::String).into()
                    )))
                )),
            })
        );

        let mut two_inputs = Punctuated::new();
        two_inputs.push(
            Type::Primitive(Primitive::Integer(Token::Keyword(Keyword::Int).into())),
            Token::Symbol(Symbol::Comma).into(),
        );
        two_inputs.last = Some(Box::new(Type::Primitive(Primitive::String(
            Token::Keyword(Keyword::String).into(),
        ))));

        assert_type_eq!(
            "fn(int, string) -> bool",
            Type::BareFn(BareFunctionType {
                fn_token: Token::Keyword(Keyword::Fn).into(),
                paren_token: Paren {
                    open: Token::Symbol(Symbol::LParen).into(),
                    close: Token::Symbol(Symbol::RParen).into(),
                },
                inputs: two_inputs,
                output: Some((
                    Token::Symbol(Symbol::RArrow).into(),
                    Box::new(Type::Primitive(Primitive::Boolean(
                        Token::Keyword(Keyword::Bool).into()
                    )))
                )),
            })
        );
    }
}
