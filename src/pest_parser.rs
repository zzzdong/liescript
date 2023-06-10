use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;
use pest::Parser;
use std::io::{self, BufRead};

use crate::ast::Expr;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct LieParser;

// lazy_static::lazy_static! {
//     static ref PRATT_PARSER: PrattParser<Rule> = {
//         use pest::pratt_parser::{Assoc::*, Op};
//         use Rule::*;

//         // Precedence is defined lowest to highest
//         PrattParser::new()
//             // Addition and subtract have equal precedence
//             .op(Op::infix(add, Left) | Op::infix(sub, Left))
//             .op(Op::infix(mul, Left) | Op::infix(div, Left) | Op::infix(pow, Left))
//             .op(Op::prefix(neg))
//     };
// }


// fn parse_expr(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Expr {
//     pratt
//         .map_primary(|p| match p.as_rule() {
//             Rule::int  => p.as_str().parse().unwrap(),
//             Rule::expr => parse_expr(p.into_inner(), pratt), // from "(" ~ expr ~ ")"
//             _          => unreachable!(),
//         })
//         .map_prefix(|op, rhs| match op.as_rule() {
//             Rule::neg  => -rhs,
//             _          => unreachable!(),
//         })
//         .map_postfix(|lhs, op| match op.as_rule() {
//             Rule::que  => (1..lhs+1).product(),
//             _          => unreachable!(),
//         })
//         .map_infix(|lhs, op, rhs| match op.as_rule() {
//             Rule::add  => lhs + rhs,
//             Rule::sub  => lhs - rhs,
//             Rule::mul  => lhs * rhs,
//             Rule::div  => lhs / rhs,
//             Rule::pow  => (1..rhs+1).map(|_| lhs).product(),
//             _          => unreachable!(),
//         })
//         .parse(pairs)
// }



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