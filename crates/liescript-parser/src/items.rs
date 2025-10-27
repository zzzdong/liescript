use liescript_ast::{
    expressions::Expression,
    items::{
        ConstItem, EnumItem, EnumVariant, EnumVariantFields, FunctionBody, FunctionItem,
        FunctionParam, FunctionParams, Item, ModuleContent, ModuleItem, NamedField, NamedFields,
        StaticItem, StructFields, StructItem, TupleField, TupleFields, TypeAliasItem, UseItem,
        UseTree,
    },
    names::SimplePath,
    operators::{BinOp, UnOp},
    patterns::{Pattern, RangeLimits},
    statements::Statement,
    types::Type,
};
use liescript_lexical::{
    Spanned,
    keyword::Keyword,
    literal::Literal,
    symbol::Symbol,
    token::{Brace, Bracket, Paren, Punctuated, Token},
};

use super::{
    Parse,
    context::ParseContext,
    diagnostic::{ParseError, ParseResult},
};

impl Parse for Item {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_item(cx)
    }
}

impl Parse for FunctionItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_function_item(cx)
    }
}

impl Parse for StructItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_struct_item(cx)
    }
}

impl Parse for EnumItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_enum_item(cx)
    }
}

impl Parse for TypeAliasItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_type_alias_item(cx)
    }
}

impl Parse for ConstItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_const_item(cx)
    }
}

impl Parse for StaticItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_static_item(cx)
    }
}

impl Parse for ModuleItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_module_item(cx)
    }
}

impl Parse for UseItem {
    fn parse(cx: &mut ParseContext) -> ParseResult<Self> {
        parse_use_item(cx)
    }
}

/// 解析项
pub fn parse_item(cx: &mut ParseContext) -> ParseResult<Item> {
    if cx.is_eof() {
        return Err(cx.create_eof_error("item"));
    }

    let token = if let Some(token) = cx.peek() {
        token.clone()
    } else {
        return Err(cx.create_eof_error("item"));
    };

    if let Token::Keyword(keyword) = &token.value {
        match keyword {
            Keyword::Fn => parse_function_item(cx).map(Item::Function),
            Keyword::Struct => parse_struct_item(cx).map(Item::Struct),
            Keyword::Enum => parse_enum_item(cx).map(Item::Enum),
            Keyword::Type => parse_type_alias_item(cx).map(Item::TypeAlias),
            Keyword::Const => parse_const_item(cx).map(Item::Const),
            Keyword::Static => parse_static_item(cx).map(Item::Static),
            Keyword::Mod => parse_module_item(cx).map(Item::Module),
            Keyword::Use => parse_use_item(cx).map(Item::Use),
            _ => Err(cx.create_error(
                "Unexpected keyword for item".to_string(),
                token.span,
                Some("fn, struct, enum, type, const, static, mod, or use".to_string()),
                Some(format!("{:?}", token.value)),
            )),
        }
    } else {
        Err(cx.create_error(
            "Expected keyword for item".to_string(),
            token.span,
            Some("fn, struct, enum, type, const, static, mod, or use".to_string()),
            Some(format!("{:?}", token.value)),
        ))
    }
}

/// 解析函数项
fn parse_function_item(cx: &mut ParseContext) -> ParseResult<FunctionItem> {
    let fn_token = cx.expect_keyword(Keyword::Fn)?;
    let name = cx.expect_identifier()?;

    // TODO: 解析泛型参数
    let generics = None;

    // 解析函数参数
    let lparen = cx.expect_symbol(Symbol::LParen)?;
    let mut params = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen))) && !cx.is_eof() {
        let pattern = Pattern::parse(cx)?;
        let colon_token = cx.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(cx)?;

        params.push((pattern, colon_token, ty));

        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
            cx.consume();
        } else {
            break;
        }
    }

    let rparen = cx.expect_symbol(Symbol::RParen)?;

    // 解析返回类型
    let return_type = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RArrow))) {
        let arrow_token = cx.consume()?;
        let ty = Type::parse(cx)?;
        Some((arrow_token, Box::new(ty)))
    } else {
        None
    };

    // 解析函数体
    let lbrace = cx.expect_symbol(Symbol::LBrace)?;
    let mut stmts = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
        stmts.push(Statement::parse(cx)?);
    }

    let rbrace = cx.expect_symbol(Symbol::RBrace)?;

    Ok(FunctionItem {
        fn_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        generics,
        params: FunctionParams {
            paren_token: Paren::new(lparen, rparen),
            params: {
                let mut punctuated = Punctuated::new();
                for (pat, colon, ty) in params {
                    punctuated.push_last(FunctionParam {
                        pattern: pat,
                        type_annotation: (colon, Box::new(ty)),
                    });
                }
                punctuated
            },
        },
        return_type,
        body: FunctionBody {
            brace_token: Brace::new(lbrace, rbrace),
            stmts,
        },
    })
}

/// 解析结构体项
fn parse_struct_item(cx: &mut ParseContext) -> ParseResult<StructItem> {
    let struct_token = cx.expect_keyword(Keyword::Struct)?;
    let name = cx.expect_identifier()?;

    // TODO: 解析泛型参数
    let generics = None;

    let fields = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LBrace))) {
        // 命名字段结构体
        let lbrace = cx.expect_symbol(Symbol::LBrace)?;
        let mut named_fields = Vec::new();

        while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
            let field_name = cx.expect_identifier()?;
            let colon_token = cx.expect_symbol(Symbol::Colon)?;
            let field_type = Type::parse(cx)?;

            named_fields.push(NamedField {
                name: Spanned::new(field_name.value.into_ident(), field_name.span),
                colon_token,
                ty: Box::new(field_type),
            });

            if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
                cx.consume();
            } else {
                break;
            }
        }

        let rbrace = cx.expect_symbol(Symbol::RBrace)?;
        StructFields::Named(NamedFields {
            brace_token: Brace::new(lbrace, rbrace),
            visibility: None, // TODO: 支持可见性
            fields: {
                let mut punctuated = Punctuated::new();
                for field in named_fields {
                    punctuated.push_last(field);
                }
                punctuated
            },
        })
    } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LParen))) {
        // 元组结构体
        let lparen = cx.expect_symbol(Symbol::LParen)?;
        let mut tuple_fields = Vec::new();

        while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen))) && !cx.is_eof() {
            let field_type = Type::parse(cx)?;
            tuple_fields.push(TupleField {
                visibility: None, // TODO: 支持可见性
                ty: Box::new(field_type),
            });

            if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
                cx.consume();
            } else {
                break;
            }
        }

        let rparen = cx.expect_symbol(Symbol::RParen)?;
        StructFields::Tuple(TupleFields {
            paren_token: Paren::new(lparen, rparen),
            visibility: None, // TODO: 支持可见性
            fields: {
                let mut punctuated = Punctuated::new();
                for field in tuple_fields {
                    punctuated.push_last(field);
                }
                punctuated
            },
        })
    } else {
        // 单元结构体
        StructFields::Unit
    };

    let semi_token = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Semi))) {
        Some(cx.consume()?)
    } else {
        None
    };

    Ok(StructItem {
        struct_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        generics,
        fields,
        semi_token,
    })
}

/// 解析枚举项
fn parse_enum_item(cx: &mut ParseContext) -> ParseResult<EnumItem> {
    let enum_token = cx.expect_keyword(Keyword::Enum)?;
    let name = cx.expect_identifier()?;

    // TODO: 解析泛型参数
    let generics = None;

    let lbrace = cx.expect_symbol(Symbol::LBrace)?;
    let mut variants = Vec::new();

    while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
        let variant_name = cx.expect_identifier()?;

        let variant_fields = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LBrace))) {
            // 命名字段变体
            let lbrace = cx.expect_symbol(Symbol::LBrace)?;
            let mut named_fields = Vec::new();

            while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace)))
                && !cx.is_eof()
            {
                let field_name = cx.expect_identifier()?;
                let colon_token = cx.expect_symbol(Symbol::Colon)?;
                let field_type = Type::parse(cx)?;

                named_fields.push(NamedField {
                    name: Spanned::new(field_name.value.into_ident(), field_name.span),
                    colon_token,
                    ty: Box::new(field_type),
                });

                if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
                    cx.consume();
                } else {
                    break;
                }
            }

            let rbrace = cx.expect_symbol(Symbol::RBrace)?;
            Some(EnumVariantFields::Named(NamedFields {
                brace_token: Brace::new(lbrace, rbrace),
                visibility: None,
                fields: {
                    let mut punctuated = Punctuated::new();
                    for field in named_fields {
                        punctuated.push_last(field);
                    }
                    punctuated
                },
            }))
        } else if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LParen))) {
            // 元组变体
            let lparen = cx.expect_symbol(Symbol::LParen)?;
            let mut tuple_fields = Vec::new();

            while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RParen)))
                && !cx.is_eof()
            {
                let field_type = Type::parse(cx)?;
                tuple_fields.push(TupleField {
                    visibility: None,
                    ty: Box::new(field_type),
                });

                if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
                    cx.consume();
                } else {
                    break;
                }
            }

            let rparen = cx.expect_symbol(Symbol::RParen)?;
            Some(EnumVariantFields::Tuple(TupleFields {
                paren_token: Paren::new(lparen, rparen),
                visibility: None,
                fields: {
                    let mut punctuated = Punctuated::new();
                    for field in tuple_fields {
                        punctuated.push_last(field);
                    }
                    punctuated
                },
            }))
        } else {
            None
        };

        variants.push(EnumVariant {
            name: Spanned::new(variant_name.value.into_ident(), variant_name.span),
            fields: variant_fields,
            discriminant: None, // TODO: 支持判别式
        });

        if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::Comma))) {
            cx.consume();
        } else {
            break;
        }
    }

    let rbrace = cx.expect_symbol(Symbol::RBrace)?;

    Ok(EnumItem {
        enum_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        generics,
        brace_token: Brace::new(lbrace, rbrace),
        variants: {
            let mut punctuated = Punctuated::new();
            for variant in variants {
                punctuated.push_last(variant);
            }
            punctuated
        },
    })
}

/// 解析类型别名项
fn parse_type_alias_item(cx: &mut ParseContext) -> ParseResult<TypeAliasItem> {
    let type_token = cx.expect_keyword(Keyword::Type)?;
    let name = cx.expect_identifier()?;

    // TODO: 解析泛型参数
    let generics = None;

    let eq_token = cx.expect_symbol(Symbol::Eq)?;
    let ty = Type::parse(cx)?;
    let semi_token = cx.expect_symbol(Symbol::Semi)?;

    Ok(TypeAliasItem {
        type_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        generics,
        eq_token,
        ty: Box::new(ty),
        semi_token,
    })
}

/// 解析常量项
fn parse_const_item(cx: &mut ParseContext) -> ParseResult<ConstItem> {
    let const_token = cx.expect_keyword(Keyword::Const)?;
    let name = cx.expect_identifier()?;
    let colon_token = cx.expect_symbol(Symbol::Colon)?;
    let ty = Type::parse(cx)?;
    let eq_token = cx.expect_symbol(Symbol::Eq)?;
    let expr = Expression::parse(cx)?;
    let semi_token = cx.expect_symbol(Symbol::Semi)?;

    Ok(ConstItem {
        const_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        colon_token,
        ty: Box::new(ty),
        eq_token,
        expr: Box::new(expr),
        semi_token,
    })
}

/// 解析静态项
fn parse_static_item(cx: &mut ParseContext) -> ParseResult<StaticItem> {
    let static_token = cx.expect_keyword(Keyword::Static)?;
    let name = cx.expect_identifier()?;
    let colon_token = cx.expect_symbol(Symbol::Colon)?;
    let ty = Type::parse(cx)?;
    let eq_token = cx.expect_symbol(Symbol::Eq)?;
    let expr = Expression::parse(cx)?;
    let semi_token = cx.expect_symbol(Symbol::Semi)?;

    Ok(StaticItem {
        static_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        colon_token,
        ty: Box::new(ty),
        eq_token,
        expr: Box::new(expr),
        semi_token,
    })
}

/// 解析模块项
fn parse_module_item(cx: &mut ParseContext) -> ParseResult<ModuleItem> {
    let mod_token = cx.expect_keyword(Keyword::Mod)?;
    let name = cx.expect_identifier()?;

    let content = if cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::LBrace))) {
        let lbrace = cx.expect_symbol(Symbol::LBrace)?;
        let mut items = Vec::new();

        while !cx.next_is(|t: &Token| matches!(t, Token::Symbol(Symbol::RBrace))) && !cx.is_eof() {
            items.push(parse_item(cx)?);
        }

        let rbrace = cx.expect_symbol(Symbol::RBrace)?;
        Some(ModuleContent {
            brace_token: Brace::new(lbrace, rbrace),
            items,
        })
    } else {
        None
    };

    let semi_token = if content.is_none() {
        Some(cx.expect_symbol(Symbol::Semi)?)
    } else {
        None
    };

    Ok(ModuleItem {
        mod_token,
        name: Spanned::new(name.value.into_ident(), name.span),
        content,
        semi_token,
    })
}

/// 解析use项
fn parse_use_item(cx: &mut ParseContext) -> ParseResult<UseItem> {
    let use_token = cx.expect_keyword(Keyword::Use)?;
    let path = SimplePath::parse(cx)?;
    let semi_token = cx.expect_symbol(Symbol::Semi)?;

    // TODO: 实现完整的use树解析
    Ok(UseItem {
        use_token,
        tree: UseTree::Path(path),
        semi_token,
    })
}
