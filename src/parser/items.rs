use super::parse::{Parse, ParseError, ParseStream};

use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::{Expression, Path, Pattern, Statement, Type, Visibility, items::*};

// Item 相关的 Parse 实现
impl Parse for Item {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
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
    let path = Path::parse(stream)?;
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
    // 解析通配符导入 (::*)
    if stream.next_is(Symbol::Star) {
        let star_token = stream.expect_symbol(Symbol::Star)?;
        return Ok(UseTree::Glob(UseGlobTree {
            prefix: None,
            colon_colon: None,
            star_token,
        }));
    }

    // 解析路径
    let path = Path::parse(stream)?;

    // 解析 ::* 或 ::{...}
    if stream.next_is(Symbol::ColonColon) {
        let colon_colon = stream.expect_symbol(Symbol::ColonColon)?;

        if stream.next_is(Symbol::Star) {
            let star_token = stream.expect_symbol(Symbol::Star)?;
            return Ok(UseTree::Glob(UseGlobTree {
                prefix: Some(path),
                colon_colon: Some(colon_colon),
                star_token,
            }));
        }

        if stream.next_is(Symbol::LBrace) {
            let open = stream.expect_symbol(Symbol::LBrace)?;
            let items = stream
                .parse_punctuated_with(|s| parse_use_tree(s), &Token::Symbol(Symbol::Comma))?;
            let close = stream.expect_symbol(Symbol::RBrace)?;
            return Ok(UseTree::Group(UseGroupTree {
                brace_token: Brace::new(open, close),
                items,
            }));
        }
    }

    // 解析 as 重命名
    if stream.next_is(Keyword::As) {
        let as_token = stream.expect(&Token::Keyword(Keyword::As))?;
        let rename = IdentSpan::parse(stream)?;

        // 从 path 中提取最后一个 ident 作为 name
        let last_segment = path
            .segments
            .last()
            .ok_or_else(|| ParseError::new("expected at least one segment in path"))?
            .clone();

        let name = last_segment.ident.clone();

        return Ok(UseTree::Rename(UseRenameTree {
            name,
            as_token,
            rename,
        }));
    }

    // 普通路径导入
    Ok(UseTree::Path(UsePathTree {
        path,
        colon_colon: None,
        tree: None,
    }))
}
