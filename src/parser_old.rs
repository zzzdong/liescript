mod pest_parser {
    use pest::{
        iterators::{Pair, Pairs},
        Parser, Span,
    };

    #[derive(pest_derive::Parser)]
    #[grammar = "grammar.pest"]
    struct PestParser;

    impl PestParser {
        pub fn parse_input(
            input: &str,
        ) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
            let mut rules = PestParser::parse(Rule::program, input).unwrap();
            let rules = rules.next().unwrap();
            for rule in rules.into_inner() {
                println!("{:?}", rule);
            }

            Ok(())
        }
    }

    fn span_into_str(span: Span) -> &str {
        span.as_str()
    }

    pub enum ConversionError {
        NoMatch,
        FatalError(Box<dyn std::error::Error + Send + Sync + 'static>),
    }

    // pub(crate) trait FromPest<'p>: Sized {
    //     fn from_pest(p: &mut Pairs<'p, Rule>) -> Result<Self, ConversionError>;
    // }

    // impl<'p> FromPest<'p> for PrefixOp {
    //     fn from_pest(p: &mut Pairs<'p, Rule>) -> Result<Self, ConversionError> {
    //         let mut clone = p.clone();
    //         let pair = clone.next().ok_or(ConversionError::NoMatch)?;

    //         if pair.as_rule() == Rule::prefixOp {
    //             let span = pair.as_span();
    //             let mut inner = pair.into_inner();
    //             let inner = &mut inner;

    //         }

    //         Err(ConversionError::NoMatch)
    //     }
    // }

    #[cfg(test)]
    mod test {
        use std::{io::BufRead, str::Chars};

        use super::*;
        use pest::{
            iterators::{Pair, Pairs},
            Parser,
        };

        #[test]
        fn test() {
            let program = r#"
                        -1;
                        a*-a;
                        a+-a;
                        a+b.c*d/e-f.g;
                        a+b;
                        a+b+c;
                        a+b-c;
                        a+b.c;
                        a+b*c;
                        a+b*c/d-e;
                        let a = 0;
                        let a = 0.0;
                        let a = 100;
                        let a = 'a';
                        let a = "a";
                        let a = "a\"bc";
                        let a = true;
                        struct AAA {}
                        struct AAA {
                            a: int,
                            b: float,
                            c: string,
                        }
                        fn a() {}
                        fn a(a: int, b: float) -> string {}
                        // this is comment
                        "#;

            PestParser::parse_input(program).unwrap();

            // println!("parsed: {:?}", rules);
        }
    }
}

mod nom_parser {

    use nom::{
        branch::alt,
        bytes::complete::{is_not, tag, take_until},
        character::complete::{
            alpha1, alphanumeric1, anychar, char, multispace0, multispace1, one_of, space0, space1,
        },
        combinator::{cut, map_res, opt, recognize, value},
        error::ParseError,
        multi::{many0, many1, separated_list0},
        sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
        IResult,
    };

    use crate::ast::*;

    struct LieParse {}

    pub fn parse_let_stmt(input: &str) -> IResult<&str, AstNode> {
        let (input, _) = tuple((tag("let"), multispace1))(input)?;

        let (input, (ident, expr)) = separated_pair(
            parse_identifier,
            delimited(multispace0, tag("="), multispace0),
            parse_expr,
        )(input)?;

        let let_stmt = LetStmt {
            ident: ident.to_string(),
            expr: expr,
        };

        Ok((input, AstNode::Let(let_stmt)))
    }

    fn parens(i: &str) -> IResult<&str, Expr> {
        delimited(space0, delimited(tag("("), parse_expr, tag(")")), space0)(i)
    }

    fn prefix(i: &str) -> IResult<&str, Expr> {
        let (i, op) = alt((
            value(PrefixOp::Neg, tag("-")),
            value(PrefixOp::Not, tag("!")),
        ))(i)?;

        let (i, num) = parse_expr(i)?;

        let prefix_expr = PrefixOpExpr {
            op,
            rhs: Box::new(num),
        };

        Ok((i, Expr::Prefix(prefix_expr)))
    }

    fn factor(i: &str) -> IResult<&str, Expr> {
        alt((
            delimited(
                space0,
                alt((parse_literal_expr, parse_ident_expr, prefix)),
                space0,
            ),
            parens,
        ))(i)
    }

    fn dot(i: &str) -> IResult<&str, Expr> {
        let (i, init) = factor(i)?;

        let (i, rest) = many0(pair(value(Operator::Dot, char('.')), factor))(i)?;

        let mut lhs = init;

        for (op, rhs) in rest {
            lhs = Expr::BinOp(BinOpExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }

        Ok((i, lhs))
    }

    fn power(i: &str) -> IResult<&str, Expr> {
        let (i, init) = dot(i)?;

        let (i, rest) = many0(pair(value(Operator::Pow, char('^')), dot))(i)?;

        let mut lhs = init;

        for (op, rhs) in rest {
            lhs = Expr::BinOp(BinOpExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }

        Ok((i, lhs))
    }

    fn term(i: &str) -> IResult<&str, Expr> {
        let (i, init) = power(i)?;

        let (i, rest) = many0(pair(
            alt((
                value(Operator::Mul, char('*')),
                value(Operator::Div, char('/')),
                value(Operator::Mod, char('%')),
            )),
            power,
        ))(i)?;

        let mut lhs = init;

        for (op, rhs) in rest {
            lhs = Expr::BinOp(BinOpExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }

        Ok((i, lhs))
    }

    fn parse_op_expr(i: &str) -> IResult<&str, Expr> {
        let (i, init) = term(i)?;

        let (i, rest) = many0(pair(
            alt((
                value(Operator::Plus, char('+')),
                value(Operator::Minus, char('-')),
            )),
            term,
        ))(i)?;

        let mut lhs = init;

        for (op, rhs) in rest {
            lhs = Expr::BinOp(BinOpExpr {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        }

        Ok((i, lhs))
    }

    fn parse_funcall_expr(i: &str) -> IResult<&str, Expr> {
        let (i, (name, params)) = separated_pair(parse_ident_expr, multispace0, parse_params)(i)?;

        let funccall = FuncCallExpr {
            name: Box::new(name),
            params: params,
        };

        Ok((i, Expr::FuncCall(funccall)))
    }

    fn parse_params(i: &str) -> IResult<&str, Vec<Expr>> {
        preceded(
            char('('),
            cut(terminated(
                separated_list0(preceded(multispace0, char(',')), parse_expr),
                preceded(multispace0, char(')')),
            )),
        )(i)
    }

    fn parse_if_expr(i: &str) -> IResult<&str, Expr> {
        let (i, (name, params)) = separated_pair(parse_ident_expr, multispace0, parse_params)(i)?;

        let funccall = FuncCallExpr {
            name: Box::new(name),
            params: params,
        };

        Ok((i, Expr::FuncCall(funccall)))
    }

    // fn parse_block(i: &str) -> IResult<&str, Expr> {
    //     delimited(
    //         tuple((char('('), multispace0)),
    //         second,
    //         tuple((multispace0, char(')'))),
    //     )
    // }

    fn parse_expr(i: &str) -> IResult<&str, Expr> {
        alt((parse_funcall_expr, parse_op_expr))(i)
    }

    fn parse_binop_expr(input: &str) -> IResult<&str, Expr> {
        let (input, lhs) = parse_expr(input)?;
        multispace0(input)?;
        let (input, binop) = parse_binop(input)?;
        multispace0(input)?;
        let (input, rhs) = parse_expr(input)?;

        let binop_expr = BinOpExpr {
            op: binop,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        };

        Ok((input, Expr::BinOp(binop_expr)))
    }

    fn parse_ident_expr(input: &str) -> IResult<&str, Expr> {
        let (input, ident) = parse_identifier(input)?;

        Ok((input, Expr::Ident(IdentExpr::new(ident))))
    }

    fn parse_literal_expr(input: &str) -> IResult<&str, Expr> {
        let (input, lit) = parse_literal(input)?;

        Ok((input, Expr::Literal(lit)))
    }

    fn parse_binop(input: &str) -> IResult<&str, Operator> {
        alt((
            value(Operator::Plus, tag("+")),
            value(Operator::Minus, tag("-")),
            value(Operator::Mul, tag("*")),
            value(Operator::Div, tag("/")),
            value(Operator::Mod, tag("%")),
            value(Operator::And, tag("&&")),
            value(Operator::Or, tag("||")),
            value(Operator::Eq, tag("==")),
            value(Operator::NotEq, tag("!=")),
            value(Operator::Lt, tag("<")),
            value(Operator::LtE, tag("<=")),
            value(Operator::Gt, tag(">")),
            value(Operator::GtE, tag(">=")),
        ))(input)
    }

    pub fn parse_identifier(input: &str) -> IResult<&str, &str> {
        let (input, ident) = recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(input)?;

        Ok((input, ident))
    }

    pub fn parse_literal(input: &str) -> IResult<&str, LiteralExpr> {
        alt((
            true_lit,
            false_lit,
            char_lit,
            float_lit,
            integer_lit,
            string_lit,
        ))(input)
    }

    fn true_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, _) = tag("true")(input)?;
        Ok((input, LiteralExpr::Bool(true)))
    }

    fn false_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, _) = tag("false")(input)?;
        Ok((input, LiteralExpr::Bool(false)))
    }

    fn char_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, ch) = delimited(tag("'"), anychar, tag("'"))(input)?;

        Ok((input, LiteralExpr::Char(ch)))
    }

    fn integer_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, int) = map_res(decimal, |s| {
            let mut s = s.to_string();
            s.retain(|c| c != '_');
            s.parse::<i64>()
        })(input)?;

        Ok((input, LiteralExpr::Integer(int)))
    }

    fn float_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, f) = map_res(float, |s| {
            let mut s = s.to_string();
            s.retain(|c| c != '_');
            s.parse::<f64>()
        })(input)?;

        Ok((input, LiteralExpr::Float(f)))
    }

    fn string_lit(input: &str) -> IResult<&str, LiteralExpr> {
        let (input, s) = double_quoted(input)?;

        Ok((input, LiteralExpr::String(s)))
    }

    fn float(input: &str) -> IResult<&str, &str> {
        alt((
            // Case one: .42
            recognize(tuple((
                char('.'),
                decimal,
                opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
            ))), // Case two: 42e42 and 42.42e42
            recognize(tuple((
                decimal,
                opt(preceded(char('.'), decimal)),
                one_of("eE"),
                opt(one_of("+-")),
                decimal,
            ))), // Case three: 42. and 42.42
            recognize(tuple((decimal, char('.'), opt(decimal)))),
        ))(input)
    }

    fn decimal(input: &str) -> IResult<&str, &str> {
        recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
    }

    pub fn peol_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
        value(
            (), // Output is thrown away.
            pair(char('%'), is_not("\n\r")),
        )(i)
    }

    pub fn pinline_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E> {
        value(
            (), // Output is thrown away.
            tuple((tag("(*"), take_until("*)"), tag("*)"))),
        )(i)
    }

    pub fn double_quoted(input: &str) -> IResult<&str, String> {
        delimited(tag("\""), |i| in_quotes(i, '"'), tag("\""))(input)
    }

    pub fn back_quoted(input: &str) -> IResult<&str, String> {
        delimited(tag("`"), |i| in_quotes(i, '`'), tag("`"))(input)
    }

    fn in_quotes(input: &str, quote: char) -> IResult<&str, String> {
        let mut ret = String::new();
        let mut iter = input.chars().peekable();
        let mut offset = 0;

        loop {
            match iter.next() {
                Some('\\') => {
                    offset += 1;
                    let ch = iter
                        .next()
                        .ok_or(nom::Err::Incomplete(nom::Needed::Unknown))?;

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

                    ret.push(got);
                    offset += got.len_utf8();
                }
                Some(ch) => {
                    if ch == quote {
                        return Ok((&input[offset..], ret));
                    }
                    ret.push(ch);
                    offset += ch.len_utf8();
                }
                None => {
                    return Err(nom::Err::Incomplete(nom::Needed::Unknown));
                }
            }
        }
    }

    #[test]
    fn test_parse_literal() {
        let input = r#"
            'c';
            '汉';
            "a string";
            "汉字字符串'`🙂";
            "string \\ \t \n \r \" with \" quote";
            true;
            false;
            0;
            100;
            100_000_000;
            0.0;
            1.0;
            1_000.000_001;
        "#;

        let inputs: Vec<&str> = input
            .split(';')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<&str>>();

        for i in inputs {
            println!("=> {}", i);
            let (s, ast) = parse_literal(i).unwrap();

            assert_eq!(s, "");

            println!("parse: {:?}", ast);
        }
    }

    #[test]
    fn test_parse_let_stmt() {
        let input = r#"
            let a = a + b * c;
        "#;

        let inputs: Vec<&str> = input
            .split(';')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<&str>>();

        for i in inputs {
            println!("=> {}", i);

            let (s, ast) = parse_let_stmt(i).unwrap();

            assert_eq!(s, "");

            println!("parsed: {:?}", ast);
        }
    }

    #[test]
    fn test_parse_binop_expr() {
        let input = r#"
            1 + 2*3 + 4 - 5 / 6 * 7 % 8;
            a + b * c - (e + f) * g;
            a.b + c * d - e.f * g + h;
            a . b + c * d - e.f * g + h;
            a.b ^ 3 + c * d - e.f ^ 2 * g + h;
            -1 + 2 - -3;
            -a + b - -c;
            !a;
            a + b ^ 4 + -c;
            a+b-c-d-e-f;
            a.b-c.d*e-f*g+h;
        "#;

        let inputs: Vec<&str> = input
            .split(';')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<&str>>();

        for i in inputs {
            println!("=> {}", i);

            let (s, expr) = parse_expr(i).unwrap();

            assert_eq!(s, "");

            println!("parsed:\n {}", expr);
        }
    }

    #[test]
    fn test_parse_funccall_expr() {
        let input = r#"
            -a.b + c;
            hello();
            hello(a);
            hello(a, b, c);
            hello(a, b+c, d-e);
            hello(world("what" + "?"));
        "#;

        let inputs: Vec<&str> = input
            .split(';')
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<&str>>();

        for i in inputs {
            println!("=> {}", i);

            let (s, expr) = parse_expr(i).unwrap();

            assert_eq!(s, "");

            println!("parsed:\n {}", expr);
        }
    }

    #[test]
    fn test_parse_if_expr() {
        let input = r#"
            {
                let c = a;
                a = b;
                b = c;
            }
        "#;

        println!("=> {}", input);

        let (s, expr) = parse_expr(input).unwrap();

        assert_eq!(s, "");

        println!("parsed:\n {}", expr);
    }
}
