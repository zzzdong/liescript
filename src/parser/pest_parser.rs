use pest::iterators::Pair;
use pest::{iterators::Pairs, pratt_parser::Op};
use pest::pratt_parser::{PrattParser, Assoc};
use pest::Parser;
use std::io::{self, BufRead};
use std::sync::OnceLock;

use crate::ast::Expr;
use crate::ast::ast::*;

#[derive(pest_derive::Parser)]
#[grammar = "parser/grammar.pest"]
pub struct LieParser;

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

fn pratt_parser() -> &'static PrattParser<Rule> {
    // reference: https://doc.rust-lang.org/nightly/reference/expressions.html
    PRATT_PARSER.get_or_init(|| {
        PrattParser::new()
            .op(Op::infix(Rule::add_assign_operator, Assoc::Right)
                | Op::infix(Rule::sub_assign_operator, Assoc::Right)
                | Op::infix(Rule::mul_assign_operator, Assoc::Right)
                | Op::infix(Rule::div_assign_operator, Assoc::Right)
                | Op::infix(Rule::mod_assign_operator, Assoc::Right))
            .op(Op::infix(Rule::range_operator, Assoc::Left))
            .op(Op::infix(Rule::or_operator, Assoc::Left))
            .op(Op::infix(Rule::and_operator, Assoc::Left))
            .op(Op::infix(Rule::equal_operator, Assoc::Left)
                | Op::infix(Rule::not_equal_operator, Assoc::Left)
                | Op::infix(Rule::less_operator, Assoc::Left)
                | Op::infix(Rule::less_equal_operator, Assoc::Left)
                | Op::infix(Rule::greater_operator, Assoc::Left)
                | Op::infix(Rule::greater_equal_operator, Assoc::Left))
            .op(Op::infix(Rule::add_operator, Assoc::Left)
                | Op::infix(Rule::sub_operator, Assoc::Left))
            .op(Op::infix(Rule::mul_operator, Assoc::Left)
                | Op::infix(Rule::div_operator, Assoc::Left)
                | Op::infix(Rule::mod_operator, Assoc::Left))
            .op(Op::infix(Rule::pow_operator, Assoc::Right))
            .op(Op::infix(Rule::as_operator, Assoc::Left))
            .op(Op::prefix(Rule::negative_operator) | Op::prefix(Rule::not_operator))
            .op(Op::postfix(Rule::try_operator))
            .op(Op::infix(Rule::as_operator, Assoc::Left))
            .op(Op::infix(Rule::dot_operator, Assoc::Left))
            .op(Op::infix(Rule::path_operator, Assoc::Left))
    })
}

fn parse_atom(pair: Pair<Rule>) -> Expression {
    let atom = pair.into_inner().next().unwrap();

    match atom.as_rule() {
        Rule::identifier => Expression::Identifier(atom.as_str().to_string()),
        Rule::literal => Expression::Literal(parse_literal(atom)),
        _ => unreachable!("{:?}", atom),
    }
}

fn parse_expr(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expression {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::literal => Expression::Literal(parse_literal(primary)),
            Rule::expression => parse_expr(primary.into_inner(), pratt), // from "(" ~ expr ~ ")"
            Rule::atom => parse_atom(primary),
            _ => unreachable!("{:?}", primary),
        })
        .map_prefix(|op, rhs| {
            Expression::Unary(op.as_str().parse::<UnaryOp>().unwrap(), Box::new(rhs))
        })
        .map_postfix(|lhs, op| {
            Expression::Unary(op.as_str().parse::<UnaryOp>().unwrap(), Box::new(lhs))
        })
        .map_infix(|lhs, op, rhs| {
            Expression::Binary(
                op.as_str().parse::<BinOp>().unwrap(),
                Box::new(lhs),
                Box::new(rhs),
            )
        })
        .parse(pairs)
}

fn parse_literal(rule: Pair<Rule>) -> Literal {
    let mut pairs = rule.into_inner();
    let value = pairs.next().unwrap();

    match value.as_rule() {
        Rule::boolean => Literal::Boolean(pairs.next().unwrap().as_str() == "true"),
        Rule::integer => Literal::Integer(value.as_str().parse().unwrap()),
        Rule::float => Literal::Float(value.as_str().parse().unwrap()),
        Rule::string => Literal::String(value.as_str().trim_matches('"').to_string()),
        Rule::character => Literal::Char(value.as_str().chars().next().unwrap()),
        _ => unreachable!(),
    }
}




#[cfg(test)]
mod test {
    use crate::parser;

    use super::*;
    use super::LieParser;

    #[test]
    fn test_parser() {
        // let pairs = LieParser::parse(Rule::program, "-a+b-c*d/e+f*g").unwrap_or_else(|e| panic!("{}", e));


        // parse_expr(pairs, &PRATT_PARSER);

        let program = r#"if a {a;}"#;


        let pairs = LieParser::parse(Rule::program, program).unwrap_or_else(|e| panic!("{}", e));

        println!("=> {:?}", pairs);
    }
}