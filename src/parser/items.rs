use super::parse::{Parse, ParseError, ParseStream};

use crate::lexical::{
    Brace, Bracket, IdentSpan, Keyword, Literal, LiteralSpan, Paren, Punctuated, Symbol, Token,
    TokenSpan,
};
use crate::syntax::items::*;

// Item 相关的 Parse 实现
impl Parse for Item {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Keyword(Keyword::Fn) => Ok(Item::Fn(ItemFn::parse(stream)?)),
            Token::Keyword(Keyword::Struct) => Ok(Item::Struct(ItemStruct::parse(stream)?)),
            Token::Keyword(Keyword::Enum) => Ok(Item::Enum(ItemEnum::parse(stream)?)),
            Token::Keyword(Keyword::Impl) => Ok(Item::Impl(ItemImpl::parse(stream)?)),
            Token::Keyword(Keyword::Type) => Ok(Item::Type(ItemType::parse(stream)?)),
            Token::Keyword(Keyword::Use) => Ok(Item::Use(ItemUse::parse(stream)?)),
            _ => Err(ParseError::new("Expected item")
                .with_span(token.span)
                .with_expected("fn, struct, enum, impl, type, or use")),
        }
    }
}

impl Parse for ItemFn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;
        let sig = Signature::parse(stream)?;
        let block = Block::parse(stream)?;
        Ok(ItemFn { vis, sig, block })
    }
}

impl Parse for Signature {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let fn_token = stream.expect(&Token::Keyword(Keyword::Fn))?;
        let name = IdentSpan::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LParen)?;

        let inputs = if stream.next_is_symbol(Symbol::RParen) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };

        let close_token = stream.expect_symbol(Symbol::RParen)?;

        let output = RetureType::parse(stream)?;

        Ok(Signature {
            fn_token,
            name,
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            inputs,
            output,
        })
    }
}

impl Parse for FnArg {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // Try to parse as receiver first
        if let Ok(receiver) = stream.try_parse(Receiver::parse) {
            return Ok(FnArg::Receiver(receiver));
        }

        // Otherwise parse as typed argument
        let pat = Pat::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;

        Ok(FnArg::Typed(PatType {
            pat: Box::new(pat),
            colon_token,
            ty: Box::new(ty),
        }))
    }
}

impl Parse for Receiver {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        // Check for &self or &mut self
        let and_token = if stream.next_is_symbol(Symbol::And) {
            Some(stream.expect_symbol(Symbol::And)?)
        } else {
            None
        };

        let self_token = stream.expect(&Token::Keyword(Keyword::SelfValue))?;

        Ok(Receiver {
            and_token,
            self_token,
        })
    }
}

impl Parse for ItemStruct {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let struct_token = stream.expect(&Token::Keyword(Keyword::Struct))?;
        let name = IdentSpan::parse(stream)?;

        // Parse fields
        let fields = if stream.next_is_symbol(Symbol::LBrace) {
            Fields::Named(FieldsNamed::parse(stream)?)
        } else if stream.next_is_symbol(Symbol::LParen) {
            Fields::Unnamed(FieldsUnnamed::parse(stream)?)
        } else {
            Fields::Unit
        };

        let semi_token = if matches!(fields, Fields::Unit) || matches!(fields, Fields::Unnamed(_)) {
            Some(stream.expect_symbol(Symbol::Semi)?)
        } else {
            None
        };

        Ok(ItemStruct {
            vis,
            struct_token,
            name,
            fields,
            semi_token,
        })
    }
}

impl Parse for FieldsNamed {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let named = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated_with(Field::parse_named, &Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(FieldsNamed {
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            named,
        })
    }
}

impl Parse for FieldsUnnamed {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LParen)?;
        let unnamed = if stream.next_is_symbol(Symbol::RParen) {
            Punctuated::new()
        } else {
            stream.parse_punctuated_with(Field::parse_unnamed, &Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RParen)?;

        Ok(FieldsUnnamed {
            paren_token: Paren {
                open: open_token,
                close: close_token,
            },
            unnamed,
        })
    }
}

impl Parse for Field {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        if let Ok(field) = stream.try_parse(Field::parse_unnamed) {
            return Ok(field);
        }

        Field::parse_named(stream)
    }
}

impl Field {
    fn parse_named(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;
        let name = IdentSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;

        Ok(Field {
            vis,
            name: Some(name),
            colon_token: Some(colon_token),
            ty,
        })
    }

    fn parse_unnamed(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let ty = Type::parse(stream)?;

        Ok(Field {
            vis: Visibility::Inherited,
            name: None,
            colon_token: None,
            ty,
        })
    }
}

impl Parse for ItemEnum {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let enum_token = stream.expect(&Token::Keyword(Keyword::Enum))?;
        let name = IdentSpan::parse(stream)?;
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let variants = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(ItemEnum {
            vis,
            enum_token,
            name,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            variants,
        })
    }
}

impl Parse for Variant {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;

        // Parse fields if present
        let fields = if stream.next_is_symbol(Symbol::LBrace) {
            Fields::Named(FieldsNamed::parse(stream)?)
        } else if stream.next_is_symbol(Symbol::LParen) {
            Fields::Unnamed(FieldsUnnamed::parse(stream)?)
        } else {
            Fields::Unit
        };

        // Parse discriminant if present
        let discriminant = if stream.next_is_symbol(Symbol::Eq) {
            let eq_token = stream.expect_symbol(Symbol::Eq)?;
            let expr = Expression::parse(stream)?;
            Some((eq_token, expr))
        } else {
            None
        };

        Ok(Variant {
            name,
            fields,
            discriminant,
        })
    }
}

impl Parse for ItemImpl {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let impl_token = stream.expect(&Token::Keyword(Keyword::Impl))?;
        let self_ty = Box::new(Type::parse(stream)?);
        let open_token = stream.expect_symbol(Symbol::LBrace)?;

        let mut items = Vec::new();
        while !stream.next_is_symbol(Symbol::RBrace) {
            items.push(ImplItem::parse(stream)?);
        }

        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(ItemImpl {
            impl_token,
            trait_: None, // For simplicity, not parsing trait impls like `Trait for Type`
            self_ty,
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            items,
        })
    }
}

impl Parse for ImplItem {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let token = stream.lookahead1()?;

        match &token.value {
            Token::Keyword(Keyword::Fn) => Ok(ImplItem::Fn(ImplItemFn::parse(stream)?)),
            Token::Keyword(Keyword::Const) => Ok(ImplItem::Const(ImplItemConst::parse(stream)?)),
            _ => Err(ParseError::new("Expected impl item")
                .with_span(token.span)
                .with_expected("fn or const")),
        }
    }
}

impl Parse for ImplItemFn {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let sig = Signature::parse(stream)?;
        let block = Block::parse(stream)?;
        Ok(ImplItemFn { vis, sig, block })
    }
}

impl Parse for ImplItemConst {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let const_token = stream.expect(&Token::Keyword(Keyword::Const))?;
        let name = IdentSpan::parse(stream)?;
        let colon_token = stream.expect_symbol(Symbol::Colon)?;
        let ty = Type::parse(stream)?;
        let eq_token = stream.expect_symbol(Symbol::Eq)?;
        let expr = Expression::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semi)?;

        Ok(ImplItemConst {
            const_token,
            name,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        })
    }
}

impl Parse for ItemType {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let type_token = stream.expect(&Token::Keyword(Keyword::Type))?;
        let ident = IdentSpan::parse(stream)?;
        let eq_token = stream.expect_symbol(Symbol::Eq)?;
        let ty = Type::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semi)?;

        Ok(ItemType {
            vis,
            type_token,
            ident,
            eq_token,
            ty,
            semi_token,
        })
    }
}

impl Parse for ItemUse {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let vis = Visibility::parse(stream)?;

        let use_token = stream.expect(&Token::Keyword(Keyword::Use))?;

        let leading_colon = if stream.next_is_symbol(Symbol::ColonColon) {
            Some(stream.expect_symbol(Symbol::ColonColon)?)
        } else {
            None
        };

        let tree = UseTree::parse(stream)?;
        let semi_token = stream.expect_symbol(Symbol::Semi)?;

        Ok(ItemUse {
            vis,
            use_token,
            leading_colon,
            tree,
            semi_token,
        })
    }
}

impl Parse for UseTree {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        // Try to parse as path
        if let Ok(path) = stream.try_parse(UsePath::parse) {
            return Ok(UseTree::Path(path));
        }

        // Try to parse as group
        if let Ok(group) = stream.try_parse(UseGroup::parse) {
            return Ok(UseTree::Group(group));
        }

        // Try to parse as name
        if let Ok(name) = stream.try_parse(UseName::parse) {
            return Ok(UseTree::Name(name));
        }

        // Try to parse as rename
        if let Ok(rename) = stream.try_parse(UseRename::parse) {
            return Ok(UseTree::Rename(rename));
        }

        // Try to parse as glob
        if let Ok(glob) = stream.try_parse(UseGlob::parse) {
            return Ok(UseTree::Glob(glob));
        }

        let token = stream.lookahead1()?;
        Err(ParseError::new("Expected use tree")
            .with_span(token.span)
            .with_expected("path, group, name, rename, or glob"))
    }
}

impl Parse for UsePath {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let name = IdentSpan::parse(stream)?;
        let colon2_token = stream.expect_symbol(Symbol::ColonColon)?;
        let tree = UseTree::parse(stream)?;
        Ok(UsePath {
            name,
            colon2_token,
            tree: Box::new(tree),
        })
    }
}

impl Parse for UseGroup {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let open_token = stream.expect_symbol(Symbol::LBrace)?;
        let items = if stream.next_is_symbol(Symbol::RBrace) {
            Punctuated::new()
        } else {
            stream.parse_punctuated(&Token::Symbol(Symbol::Comma))?
        };
        let close_token = stream.expect_symbol(Symbol::RBrace)?;

        Ok(UseGroup {
            brace_token: Brace {
                open: open_token,
                close: close_token,
            },
            items,
        })
    }
}

impl Parse for UseGlob {
    fn parse(stream: &mut ParseStream) -> Result<Self, ParseError> {
        let star_token = stream.expect_symbol(Symbol::Star)?;
        Ok(UseGlob { star_token })
    }
}