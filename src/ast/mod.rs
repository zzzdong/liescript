use std::fmt;

pub mod expr;
pub mod ident;
pub mod keyword;
pub mod literal;
pub mod op;
pub mod stmt;
pub mod symbol;

pub use expr::*;
pub use ident::Ident;
pub use keyword::Keyword;
pub use literal::Literal;
pub use op::*;
pub use stmt::*;
pub use symbol::Symbol;

#[derive(Debug, Clone)]
pub struct Ast {
    pub stmts: Vec<Statement>,
}

impl Ast {
    pub fn new() -> Self {
        Ast { stmts: Vec::new() }
    }

    pub fn print(&self) {
        let mut graph = petgraph::Graph::new();

        let root = graph.add_node("ROOT".into());

        for stmt in &self.stmts {
            match stmt {
                Statement::Let(st) => {
                    let node = graph.add_node(format!("LetStmt"));

                    let name = graph.add_node(format!("{}", st.var));

                    graph.add_edge(node, name, "name");

                    if let Some(expr) = &st.expr {
                        let expr = Expr::expr_graph(&expr, &mut graph);
                        graph.add_edge(node, expr, "");
                        graph.add_edge(node, name, "expr");
                    }

                    graph.add_edge(root, node, "");
                }
                Statement::Expr(expr) => {
                    let node = graph.add_node(format!("Expr"));

                    let e = Expr::expr_graph(&expr, &mut graph);
                    graph.add_edge(node, e, "");

                    graph.add_edge(root, node, "");
                }
                Statement::Semi(expr) => {
                    let node = graph.add_node(format!("Expr"));

                    let e = Expr::expr_graph(&expr, &mut graph);

                    graph.add_edge(node, e, "");

                    graph.add_edge(root, node, "");
                }
                Statement::Item(item) => match item {
                    Item::Use(item) => {
                        let node = graph.add_node(format!("ItemUse{:?}", item));

                        graph.add_edge(root, node, "");
                    }
                    Item::Struct(item) => {
                        let node = graph.add_node(format!("ItemStruct{:?}", item));

                        graph.add_edge(root, node, "");
                    }
                    Item::Fn(item) => {
                        let node = graph.add_node(format!("ItemFn{:?}", item.sig));

                        graph.add_edge(root, node, "");
                    }
                },
                Statement::Empty => {
                    let node = graph.add_node(format!("Empty"));

                    graph.add_edge(root, node, "");
                }
            }
        }

        println!("{}", petgraph::dot::Dot::new(&graph));
    }
}
