use std::fs;

use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::ast::parser::parse_stmts;

pub mod ast;
pub mod compiler;
pub mod luz;
pub mod runner;
pub mod parser;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct LuaParser;

pub fn run(input: &str) -> Result<(), LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, input);
    match &mut stmts {
        Ok(stmts) => {
            // dbg!(&stmts);
            let stmts = parse_stmts(stmts)?;
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
            let stmts = parse_stmts(stmts)?;
            dbg!(stmts);
            Ok(())
        }
        Err(err) => {
            dbg!(err);
            panic!();
        }
    }
}
