use std::fs;

use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::{
    ast::{parser::parse_script, Stat},
    compiler::visitor::Visitor,
    compiler::Compiler,
};

pub mod ast;
pub mod compiler;
pub mod luz;
pub mod parser;
pub mod runner;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct LuaParser;

pub fn run(input: &str) -> Result<(), LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, input);
    match &mut stmts {
        Ok(stmts) => {
            // dbg!(&stmts);
            let stmts = parse_script(stmts)?;
            dbg!(stmts);
            Ok(())
        }
        Err(err) => {
            dbg!(err);
            panic!();
        }
    }
}

pub fn run_file(path: &str) -> Result<(), LuzError> {
    let input = fs::read_to_string(path).map_err(|_| LuzError::LoadFile(path.to_string()))?;
    let start = if input.starts_with("#") {
        input.find("\n").unwrap_or(input.len())
    } else {
        0
    };

    let mut stmts = LuaParser::parse(Rule::Chunk, &input[start..]);

    match &mut stmts {
        Ok(stmts) => {
            // dbg!(&stmts);
            let stmts = parse_script(stmts)?;
            // dbg!(&stmts);
            run_compiler(stmts)
        }
        Err(err) => {
            dbg!(err);
            panic!();
        }
    }
}

fn run_compiler(stmts: Vec<Stat>) -> Result<(), LuzError> {
    let mut compiler = Compiler::default();
    for stmt in stmts {
        compiler.visit_stat(&stmt)?;
    }
    compiler.print_instructions();
    let mut runner = runner::Runner::new(compiler.scope_clone());
    let res = runner.run()?;
    dbg!(res);
    Ok(())
}
