use super::parse::{Parse, ParseError, ParseStream};

use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::{
    Expression, Pattern, SimplePath, Statement, Type, TypePath, Visibility, items::*,
};

// Item 相关的 Parse 实现
impl Parse for Item {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        parse_item(stream)
    }
}

fn parse_item(stream: &mut ParseStream) -> Result<Item, ParseError> {
    let peek = stream.peek().ok_or(ParseError::eof())?;

    match peek.value() {
        Token::Keyword(Keyword::Fn) => parse_function(stream).map(Item::Function),
        Token::Keyword(Keyword::Struct) => parse_struct(stream).map(Item::Struct),
        Token::Keyword(Keyword::Enum) => parse_enum(stream).map(Item::Enum),
        Token::Keyword(Keyword::Type) => parse_type_alias(stream).map(Item::TypeAlias),
        Token::Keyword(Keyword::Const) => parse_const(stream).map(Item::Const),
        Token::Keyword(Keyword::Static) => parse_static(stream).map(Item::Static),
        Token::Keyword(Keyword::Mod) => parse_module(stream).map(Item::Module),
        Token::Keyword(Keyword::Use) => parse_use(stream).map(Item::Use),

        _ => Err(ParseError::new(
            "expected an item declaration (function, struct, enum, type, const, static, mod, or use)",
        )),
    }
}

/// 解析函数定义
fn parse_function(stream: &mut ParseStream) -> Result<FunctionItem, ParseError> {
    let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
    let name = IdentSpan::parse(stream)?;

    // 解析泛型参数
    let generics = if stream.next_is(Symbol::Lt) {
        Some(parse_generic_params(stream)?)
    } else {
        None
    };

    // 解析函数参数
    let params = parse_function_params(stream)?;

    // 解析返回类型
    let return_type = if stream.next_is(Symbol::RArrow) {
        let arrow_token = stream.expect_symbol(Symbol::RArrow)?;
        let return_ty = Box::new(Type::parse(stream)?);
        Some((arrow_token, return_ty))
    } else {
        None
    };

    // 解析函数体
    let body = parse_function_body(stream)?;

    Ok(FunctionItem {
        fn_token,
        name,
        generics,
        params,
        return_type,
        body,
    })
}

/// 解析泛型参数
fn parse_generic_params(stream: &mut ParseStream) -> Result<GenericParams, ParseError> {
    let lt_token = stream.expect_symbol(Symbol::Lt)?;
    let params =
        stream.parse_punctuated_with(|s| parse_generic_param(s), &Token::Symbol(Symbol::Comma))?;
    let gt_token = stream.expect_symbol(Symbol::Gt)?;

    Ok(GenericParams {
        angle_bracket_token: (lt_token, gt_token),
        params,
    })
}

/// 解析单个泛型参数
fn parse_generic_param(stream: &mut ParseStream) -> Result<GenericParam, ParseError> {
    let name = IdentSpan::parse(stream)?;

    let bounds = if stream.next_is(Symbol::Colon) {
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let bounds = stream
            .parse_punctuated_with(|s| parse_type_param_bound(s), &Token::Symbol(Symbol::Plus))?;
        Some((colon_token, bounds))
    } else {
        None
    };

    let default = if stream.next_is(Symbol::Eq) {
        let eq_token = stream.expect_symbol(Symbol::Eq)?;
        let default_ty = Box::new(Type::parse(stream)?);
        Some((eq_token, default_ty))
    } else {
        None
    };

    Ok(GenericParam::Type(TypeParam {
        name,
        bounds,
        default,
    }))
}

/// 解析类型参数约束
fn parse_type_param_bound(stream: &mut ParseStream) -> Result<TypeParamBound, ParseError> {
    let path = TypePath::parse(stream)?;
    Ok(TypeParamBound::Trait(TraitBound { path }))
}

/// 解析函数参数
fn parse_function_params(stream: &mut ParseStream) -> Result<FunctionParams, ParseError> {
    let open = stream.expect_symbol(Symbol::LParen)?;
    let params =
        stream.parse_punctuated_with(|s| parse_function_param(s), &Token::Symbol(Symbol::Comma))?;
    let close = stream.expect_symbol(Symbol::RParen)?;

    Ok(FunctionParams {
        paren_token: Paren::new(open, close),
        params,
    })
}

/// 解析单个函数参数
fn parse_function_param(stream: &mut ParseStream) -> Result<FunctionParam, ParseError> {
    let pattern = Pattern::parse(stream)?;
    let colon_token = stream.expect_symbol(Symbol::Colon)?;
    let param_type = Box::new(Type::parse(stream)?);

    Ok(FunctionParam {
        pattern,
        type_annotation: (colon_token, param_type),
    })
}

/// 解析函数体
fn parse_function_body(stream: &mut ParseStream) -> Result<FunctionBody, ParseError> {
    let open = stream.expect_symbol(Symbol::LBrace)?;
    let mut stmts = Vec::new();

    while !stream.is_empty() && !stream.next_is(Symbol::RBrace) {
        if let Ok(stmt) = Statement::parse(stream) {
            stmts.push(stmt);
        } else {
            break;
        }
    }

    let close = stream.expect_symbol(Symbol::RBrace)?;

    Ok(FunctionBody {
        brace_token: Brace::new(open, close),
        stmts,
    })
}

/// 解析结构体定义
fn parse_struct(stream: &mut ParseStream) -> Result<StructItem, ParseError> {
    let struct_token = stream.expect(&Token::Keyword(Keyword::Struct))?;
    let name = IdentSpan::parse(stream)?;

    // 解析泛型参数
    let generics = if stream.next_is(Symbol::Lt) {
        Some(parse_generic_params(stream)?)
    } else {
        None
    };

    // 解析结构体字段
    let (fields, semi_token) = if stream.next_is(Symbol::LParen) {
        // 元组结构体
        let fields = StructFields::Tuple(parse_tuple_fields(stream)?);
        let semi_token = stream.expect_symbol(Symbol::Semi)?;
        (fields, Some(semi_token))
    } else if stream.next_is(Symbol::LBrace) {
        // 命名字段结构体
        let fields = StructFields::Named(parse_named_fields(stream)?);
        (fields, None)
    } else {
        // 单元结构体
        let semi_token = stream.expect_symbol(Symbol::Semi)?;
        (StructFields::Unit, Some(semi_token))
    };

    Ok(StructItem {
        struct_token,
        name,
        generics,
        fields,
        semi_token,
    })
}

/// 解析命名字段
fn parse_named_fields(stream: &mut ParseStream) -> Result<NamedFields, ParseError> {
    let visibility = stream.try_parse(Visibility::parse);

    let open = stream.expect_symbol(Symbol::LBrace)?;
    let fields =
        stream.parse_punctuated_with(|s| parse_named_field(s), &Token::Symbol(Symbol::Comma))?;
    let close = stream.expect_symbol(Symbol::RBrace)?;

    Ok(NamedFields {
        brace_token: Brace::new(open, close),
        visibility,
        fields,
    })
}

/// 解析单个命名字段
fn parse_named_field(stream: &mut ParseStream) -> Result<NamedField, ParseError> {
    let name = IdentSpan::parse(stream)?;
    let colon_token = stream.expect_symbol(Symbol::Colon)?;
    let ty = Box::new(Type::parse(stream)?);

    Ok(NamedField {
        name,
        colon_token,
        ty,
    })
}

/// 解析元组字段
fn parse_tuple_fields(stream: &mut ParseStream) -> Result<TupleFields, ParseError> {
    let visibility = stream.try_parse(Visibility::parse);

    let open = stream.expect_symbol(Symbol::LParen)?;
    let fields =
        stream.parse_punctuated_with(|s| parse_tuple_field(s), &Token::Symbol(Symbol::Comma))?;
    let close = stream.expect_symbol(Symbol::RParen)?;

    Ok(TupleFields {
        paren_token: Paren::new(open, close),
        visibility,
        fields,
    })
}

/// 解析单个元组字段
fn parse_tuple_field(stream: &mut ParseStream) -> Result<TupleField, ParseError> {
    let visibility = stream.try_parse(Visibility::parse);

    let ty = Box::new(Type::parse(stream)?);

    Ok(TupleField { visibility, ty })
}

/// 解析枚举定义
fn parse_enum(stream: &mut ParseStream) -> Result<EnumItem, ParseError> {
    let enum_token = stream.expect(&Token::Keyword(Keyword::Enum))?;
    let name = IdentSpan::parse(stream)?;

    // 解析泛型参数
    let generics = if stream.next_is(Symbol::Lt) {
        Some(parse_generic_params(stream)?)
    } else {
        None
    };

    // 解析枚举变体
    let open = stream.expect_symbol(Symbol::LBrace)?;
    let variants =
        stream.parse_punctuated_with(|s| parse_enum_variant(s), &Token::Symbol(Symbol::Comma))?;
    let close = stream.expect_symbol(Symbol::RBrace)?;

    Ok(EnumItem {
        enum_token,
        name,
        generics,
        brace_token: Brace::new(open, close),
        variants,
    })
}

/// 解析枚举变体
fn parse_enum_variant(stream: &mut ParseStream) -> Result<EnumVariant, ParseError> {
    let name = IdentSpan::parse(stream)?;

    // 解析变体字段
    let fields = if stream.next_is(Symbol::LParen) {
        Some(EnumVariantFields::Tuple(parse_tuple_fields(stream)?))
    } else if stream.next_is(Symbol::LBrace) {
        Some(EnumVariantFields::Named(parse_named_fields(stream)?))
    } else {
        None
    };

    // 解析判别式
    let discriminant = if stream.next_is(Symbol::Eq) {
        let eq_token = stream.expect_symbol(Symbol::Eq)?;
        let expr = Box::new(Expression::parse(stream)?);
        Some((eq_token, expr))
    } else {
        None
    };

    Ok(EnumVariant {
        name,
        fields,
        discriminant,
    })
}

/// 解析类型别名
fn parse_type_alias(stream: &mut ParseStream) -> Result<TypeAliasItem, ParseError> {
    let type_token = stream.expect(&Token::Keyword(Keyword::Type))?;
    let name = IdentSpan::parse(stream)?;

    // 解析泛型参数
    let generics = if stream.next_is(Symbol::Lt) {
        Some(parse_generic_params(stream)?)
    } else {
        None
    };

    let eq_token = stream.expect_symbol(Symbol::Eq)?;
    let ty = Box::new(Type::parse(stream)?);
    let semi_token = stream.expect_symbol(Symbol::Semi)?;

    Ok(TypeAliasItem {
        type_token,
        name,
        generics,
        eq_token,
        ty,
        semi_token,
    })
}

/// 解析常量定义
fn parse_const(stream: &mut ParseStream) -> Result<ConstItem, ParseError> {
    let const_token = stream.expect(&Token::Keyword(Keyword::Const))?;
    let name = IdentSpan::parse(stream)?;
    let colon_token = stream.expect_symbol(Symbol::Colon)?;
    let ty = Box::new(Type::parse(stream)?);
    let eq_token = stream.expect_symbol(Symbol::Eq)?;
    let expr = Box::new(Expression::parse(stream)?);
    let semi_token = stream.expect_symbol(Symbol::Semi)?;

    Ok(ConstItem {
        const_token,
        name,
        colon_token,
        ty,
        eq_token,
        expr,
        semi_token,
    })
}

/// 解析静态变量定义
fn parse_static(stream: &mut ParseStream) -> Result<StaticItem, ParseError> {
    let static_token = stream.expect(&Token::Keyword(Keyword::Static))?;
    let name = IdentSpan::parse(stream)?;
    let colon_token = stream.expect_symbol(Symbol::Colon)?;
    let ty = Box::new(Type::parse(stream)?);
    let eq_token = stream.expect_symbol(Symbol::Eq)?;
    let expr = Box::new(Expression::parse(stream)?);
    let semi_token = stream.expect_symbol(Symbol::Semi)?;

    Ok(StaticItem {
        static_token,
        name,
        colon_token,
        ty,
        eq_token,
        expr,
        semi_token,
    })
}

/// 解析模块定义
fn parse_module(stream: &mut ParseStream) -> Result<ModuleItem, ParseError> {
    let mod_token = stream.expect(&Token::Keyword(Keyword::Mod))?;
    let name = IdentSpan::parse(stream)?;

    let (content, semi_token) = if stream.next_is(Symbol::LBrace) {
        let open = stream.expect_symbol(Symbol::LBrace)?;

        let mut items = Vec::new();

        while !stream.is_empty() && !stream.next_is(Symbol::RBrace) {
            if let Ok(item) = Item::parse(stream) {
                items.push(item);
            } else {
                break;
            }
        }

        let close = stream.expect_symbol(Symbol::RBrace)?;

        let content = Some(ModuleContent {
            brace_token: Brace::new(open, close),
            items,
        });
        (content, None)
    } else {
        let semi_token = stream.expect_symbol(Symbol::Semi)?;
        (None, Some(semi_token))
    };

    Ok(ModuleItem {
        mod_token,
        name,
        content,
        semi_token,
    })
}

/// 解析导入声明
fn parse_use(stream: &mut ParseStream) -> Result<UseItem, ParseError> {
    let use_token = stream.expect(&Token::Keyword(Keyword::Use))?;
    let tree = parse_use_tree(stream)?;
    let semi_token = stream.expect_symbol(Symbol::Semi)?;

    Ok(UseItem {
        use_token,
        tree,
        semi_token,
    })
}

/// 解析导入树
fn parse_use_tree(stream: &mut ParseStream) -> Result<UseTree, ParseError> {
    if stream.next_is(Symbol::Star) {
        let star_token = stream.expect_symbol(Symbol::Star)?;
        return Ok(UseTree::Glob(UseGlobTree {
            prefix: None,
            star_token,
        }));
    }

    // 解析路径
    let path = SimplePath::parse(stream)?;

    if !path.is_ends_with_colon() {
        if stream.next_is(Keyword::As) {
            let as_token = stream.expect(&Token::Keyword(Keyword::As))?;
            let rename = IdentSpan::parse(stream)?;
            return Ok(UseTree::Rename(UseRenameTree {
                path,
                as_token,
                rename,
            }));
        }

        return Ok(UseTree::Path(path));
    }

    // when path ends with ::, maybe it's a glob import or a group import, e.g. `use std::*`, `use std::{...}`

    if stream.next_is(Symbol::Star) {
        let star_token = stream.expect_symbol(Symbol::Star)?;
        return Ok(UseTree::Glob(UseGlobTree {
            prefix: Some(path),
            star_token,
        }));
    }

    if stream.next_is(Symbol::LBrace) {
        let open = stream.expect_symbol(Symbol::LBrace)?;
        let items = stream.parse_punctuated_with(parse_use_tree, &Token::Symbol(Symbol::Comma))?;
        let close = stream.expect_symbol(Symbol::RBrace)?;
        return Ok(UseTree::Group(UseGroupTree {
            prefix: Some(path),
            brace_token: Brace::new(open, close),
            items,
        }));
    }

    Err(ParseError::new("expected '*' or '{' after path")
        .with_found(format!("{:?}", stream.peek())))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::{Span, Spanned};
    use crate::lexical::{Identifier, Literal, Token, TokenStream};
    use crate::syntax::{
        BinOp, Expression, Pattern, Statement, Type, expressions::*, items::*,
        patterns::IdentifierPattern, statements::ExpressionStatement, types::Primitive,
    };
    use crate::syntax::{PathExprSegment, PathIdentSegment, PathInExpression, SimplePathSegment};

    /// 解析项并忽略Span信息进行比较
    macro_rules! assert_item_eq {
        ($input:literal, $expected:expr) => {{
            let actual = parse_item($input).unwrap();
            assert_eq!(actual, $expected);
        }};
    }

    fn parse_item(input: &str) -> Result<Item, ParseError> {
        let tokens = TokenStream::parse(input).unwrap();
        let mut stream = ParseStream::new(&tokens);
        Item::parse(&mut stream)
    }

    fn ident_pattern(name: &str) -> Pattern {
        Pattern::Identifier(IdentifierPattern {
            ident: Identifier::new(name).into(),
            by_ref: None,
            is_mut: None,
            subpat: None,
        })
    }

    fn literal_expr(value: i64) -> Expression {
        Expression::Literal(LiteralExpression {
            lit: Literal::Integer(value).into(),
        })
    }

    fn path_expr(paths: &[&str]) -> PathExpression {
        PathExpression { path: path(paths) }
    }

    fn path(paths: &[&str]) -> PathInExpression {
        let mut segments = Punctuated::new();
        if let Some((last, rest)) = paths.split_last() {
            for path in rest {
                segments.push(
                    PathExprSegment {
                        ident: PathIdentSegment::Ident(Identifier::new(path).into()),
                        args: None,
                    },
                    Token::Symbol(Symbol::ColonColon).into(),
                );
            }
            segments.push_last(PathExprSegment {
                ident: PathIdentSegment::Ident(Identifier::new(last).into()),
                args: None,
            });
        }

        PathInExpression {
            leading_colon: None,
            segments,
        }
    }

    fn simple_path(paths: &[&str]) -> SimplePath {
        let mut segments = Punctuated::new();
        if let Some((last, rest)) = paths.split_last() {
            for path in rest {
                segments.push(
                    SimplePathSegment::Ident(Identifier::new(path).into()),
                    Token::Symbol(Symbol::ColonColon).into(),
                );
            }
            segments.push_last(SimplePathSegment::Ident(Identifier::new(last).into()));
        }

        SimplePath {
            leading_colon: None,
            segments,
        }
    }

    #[test]
    fn test_function_item() {
        assert_item_eq!(
            "fn foo() { }",
            Item::Function(FunctionItem {
                fn_token: Token::Keyword(Keyword::Fn).into(),
                name: Identifier::new("foo").into(),
                generics: None,
                params: FunctionParams {
                    paren_token: Paren {
                        open: Token::Symbol(Symbol::LParen).into(),
                        close: Token::Symbol(Symbol::RParen).into(),
                    },
                    params: Punctuated::new(),
                },
                return_type: None,
                body: FunctionBody {
                    brace_token: Brace {
                        open: Token::Symbol(Symbol::LBrace).into(),
                        close: Token::Symbol(Symbol::RBrace).into(),
                    },
                    stmts: vec![],
                },
            })
        );
    }

    #[test]
    fn test_function_with_params() {
        assert_item_eq!(
            "fn add(a: int, b: int) -> int { a + b }",
            Item::Function(FunctionItem {
                fn_token: Token::Keyword(Keyword::Fn).into(),
                name: Identifier::new("add").into(),
                generics: None,
                params: FunctionParams {
                    paren_token: Paren {
                        open: Token::Symbol(Symbol::LParen).into(),
                        close: Token::Symbol(Symbol::RParen).into(),
                    },
                    params: {
                        let mut params = Punctuated::new();
                        params.push(
                            FunctionParam {
                                pattern: ident_pattern("a"),
                                type_annotation: (
                                    Token::Symbol(Symbol::Colon).into(),
                                    Box::new(Type::Primitive(Primitive::Integer(
                                        Token::Keyword(Keyword::Int).into(),
                                    ))),
                                ),
                            },
                            Token::Symbol(Symbol::Comma).into(),
                        );
                        params.last = Some(Box::new(FunctionParam {
                            pattern: ident_pattern("b"),
                            type_annotation: (
                                Token::Symbol(Symbol::Colon).into(),
                                Box::new(Type::Primitive(Primitive::Integer(
                                    Token::Keyword(Keyword::Int).into(),
                                ))),
                            ),
                        }));
                        params
                    },
                },
                return_type: Some((
                    Token::Symbol(Symbol::RArrow).into(),
                    Box::new(Type::Primitive(Primitive::Integer(
                        Token::Keyword(Keyword::Int).into()
                    ))),
                )),
                body: FunctionBody {
                    brace_token: Brace {
                        open: Token::Symbol(Symbol::LBrace).into(),
                        close: Token::Symbol(Symbol::RBrace).into(),
                    },
                    stmts: vec![Statement::Expression(ExpressionStatement {
                        expr: Expression::Operator(OperatorExpression::Arithmetic {
                            left: Box::new(Expression::Path(path_expr(&["a"]))),
                            op: Spanned::new(BinOp::Add, Span::default()),
                            right: Box::new(Expression::Path(path_expr(&["b"]))),
                        }),
                        semi_token: None,
                    })],
                },
            })
        );
    }

    #[test]
    fn test_struct_item() {
        assert_item_eq!(
            "struct Point { x: int, y: int }",
            Item::Struct(StructItem {
                struct_token: Token::Keyword(Keyword::Struct).into(),
                name: Identifier::new("Point").into(),
                generics: None,
                fields: StructFields::Named(NamedFields {
                    brace_token: Brace {
                        open: Token::Symbol(Symbol::LBrace).into(),
                        close: Token::Symbol(Symbol::RBrace).into(),
                    },
                    visibility: None,
                    fields: {
                        let mut fields = Punctuated::new();
                        fields.push(
                            NamedField {
                                name: Identifier::new("x").into(),
                                colon_token: Token::Symbol(Symbol::Colon).into(),
                                ty: Box::new(Type::Primitive(Primitive::Integer(
                                    Token::Keyword(Keyword::Int).into(),
                                ))),
                            },
                            Token::Symbol(Symbol::Comma).into(),
                        );
                        fields.last = Some(Box::new(NamedField {
                            name: Identifier::new("y").into(),
                            colon_token: Token::Symbol(Symbol::Colon).into(),
                            ty: Box::new(Type::Primitive(Primitive::Integer(
                                Token::Keyword(Keyword::Int).into(),
                            ))),
                        }));
                        fields
                    },
                }),
                semi_token: None,
            })
        );
    }

    #[test]
    fn test_unit_struct_item() {
        assert_item_eq!(
            "struct Unit;",
            Item::Struct(StructItem {
                struct_token: Token::Keyword(Keyword::Struct).into(),
                name: Identifier::new("Unit").into(),
                generics: None,
                fields: StructFields::Unit,
                semi_token: Some(Token::Symbol(Symbol::Semi).into()),
            })
        );
    }

    #[test]
    fn test_enum_item() {
        assert_item_eq!(
            "enum Option { Some(int), None }",
            Item::Enum(EnumItem {
                enum_token: Token::Keyword(Keyword::Enum).into(),
                name: Identifier::new("Option").into(),
                generics: None,
                brace_token: Brace {
                    open: Token::Symbol(Symbol::LBrace).into(),
                    close: Token::Symbol(Symbol::RBrace).into(),
                },
                variants: {
                    let mut variants = Punctuated::new();
                    variants.push(
                        EnumVariant {
                            name: Identifier::new("Some").into(),
                            fields: Some(EnumVariantFields::Tuple(TupleFields {
                                paren_token: Paren {
                                    open: Token::Symbol(Symbol::LParen).into(),
                                    close: Token::Symbol(Symbol::RParen).into(),
                                },
                                visibility: None,
                                fields: {
                                    let mut fields = Punctuated::new();
                                    fields.last = Some(Box::new(TupleField {
                                        visibility: None,
                                        ty: Box::new(Type::Primitive(Primitive::Integer(
                                            Token::Keyword(Keyword::Int).into(),
                                        ))),
                                    }));
                                    fields
                                },
                            })),
                            discriminant: None,
                        },
                        Token::Symbol(Symbol::Comma).into(),
                    );
                    variants.last = Some(Box::new(EnumVariant {
                        name: Identifier::new("None").into(),
                        fields: None,
                        discriminant: None,
                    }));
                    variants
                },
            })
        );
    }

    #[test]
    fn test_type_alias_item() {
        assert_item_eq!(
            "type MyInt = int;",
            Item::TypeAlias(TypeAliasItem {
                type_token: Token::Keyword(Keyword::Type).into(),
                name: Identifier::new("MyInt").into(),
                generics: None,
                eq_token: Token::Symbol(Symbol::Eq).into(),
                ty: Box::new(Type::Primitive(Primitive::Integer(
                    Token::Keyword(Keyword::Int).into()
                ))),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_const_item() {
        assert_item_eq!(
            "const PI: float = 3.14;",
            Item::Const(ConstItem {
                const_token: Token::Keyword(Keyword::Const).into(),
                name: Identifier::new("PI").into(),
                colon_token: Token::Symbol(Symbol::Colon).into(),
                ty: Box::new(Type::Primitive(Primitive::Float(
                    Token::Keyword(Keyword::Float).into()
                ))),
                eq_token: Token::Symbol(Symbol::Eq).into(),
                expr: Box::new(Literal::Float(3.14).into()),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_use_item() {
        assert_item_eq!(
            "use std::io;",
            Item::Use(UseItem {
                use_token: Token::Keyword(Keyword::Use).into(),
                tree: UseTree::Path(simple_path(&["std", "io"])),
                semi_token: Token::Symbol(Symbol::Semi).into(),
            })
        );
    }

    #[test]
    fn test_module_item() {
        assert_item_eq!(
            "mod foo;",
            Item::Module(ModuleItem {
                mod_token: Token::Keyword(Keyword::Mod).into(),
                name: Identifier::new("foo").into(),
                content: None,
                semi_token: Some(Token::Symbol(Symbol::Semi).into()),
            })
        );
    }
}
