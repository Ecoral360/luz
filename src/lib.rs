use std::{cell::RefCell, fs, rc::Rc};

use log::{debug, info, trace};
use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::{
    ast::{parser::parse_script, StatNode},
    compiler::{ctx::{CompilerCtx, ScopeRef, Upvalue}, visitor::Visitor, Compiler},
    luz::obj::{LuzFunction, LuzObj},
    runner::{err::LuzRuntimeError, Runner},
};

pub mod ast;
pub mod compiler;
pub mod luz;
pub mod runner;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct LuaParser;

pub fn run(input: &str) -> Result<(), LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, &input);

    match &mut stmts {
        Ok(stmts) => {
            let stmts = parse_script(stmts)?;
            trace!("{:#?}", &stmts);
            run_compiler(stmts)
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

    run(&input[start..])
}

fn run_compiler(stmts: Vec<StatNode>) -> Result<(), LuzError> {
    let mut compiler = Compiler {};
    let mut ctx = CompilerCtx::new_main();
    for stmt in stmts {
        compiler.visit_stat(&stmt, &mut ctx)?;
    }
    debug!("{}", ctx.instructions_to_string());
    let mut runner = runner::Runner::new(ctx.scope_clone());
    let res = runner.run()?;
    info!("{:?}", res);
    Ok(())
}

pub fn load(
    input: &str,
    name: String,
    parent_scope: Option<ScopeRef>,
    upvalues: Vec<Upvalue>,
) -> Result<LuzFunction, LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, &input)?;

    let stmts = parse_script(&mut stmts)?;
    trace!("{:#?}", &stmts);

    let mut compiler = Compiler {};
    let mut ctx = CompilerCtx::new_chunk(name, parent_scope, upvalues);
    for stmt in stmts {
        compiler.visit_stat(&stmt, &mut ctx)?;
    }

    debug!("{}", ctx.instructions_to_string());

    let mut runner = runner::Runner::new(ctx.scope_clone());
    // let res = runner.run()?;
    // info!("{:?}", res);
    // Ok(())
    Ok(LuzFunction::new_native(
        0,
        Rc::new(RefCell::new(
            move |_: &mut Runner, _args: Vec<LuzObj>| {
                runner
                    .run()
                    .map_err(|e| LuzRuntimeError::message(e.to_string()))
            },
        )),
    ))
}
