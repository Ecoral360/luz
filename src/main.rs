use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::ast::parser::parse_stmts;

mod ast;
mod compiler;
mod luz;
mod runner;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
struct LuaParser;

fn run(input: &str) -> Result<(), LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, input);
    match &mut stmts {
        Ok(stmts) => {
            dbg!(&stmts);
            let stmts = parse_stmts(stmts)?;
            dbg!(stmts);
            Ok(())
        }
        Err(err) => {
            dbg!(err);
            todo!();
        }
    }
}

fn main() -> Result<(), LuzError> {
    run(r#"
    y = (12 - 1) == z
    x = -12.12 * y
"#)
}
