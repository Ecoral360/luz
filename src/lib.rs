use std::{cell::RefCell, fs, rc::Rc};

use log::{debug, info, trace};
use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::{
    ast::{parser::parse_script, Stat},
    compiler::{
        ctx::{CompilerCtx, ScopeRef, Upvalue},
        visitor::Visitor,
        Compiler,
    },
    luz::{
        lib::env::make_env_table,
        obj::{LuzFunction, LuzObj},
    },
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
            run_compiler(String::new(), input, stmts)
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

    let input = &input[start..];

    let mut stmts = LuaParser::parse(Rule::Chunk, &input);
    match &mut stmts {
        Ok(stmts) => {
            let stmts = parse_script(stmts)?;
            trace!("{:#?}", &stmts);
            run_compiler(path.to_string(), input, stmts)
        }
        Err(err) => {
            dbg!(err);
            panic!();
        }
    }
}

fn run_compiler(filename: String, input: &str, stmts: Vec<Stat>) -> Result<(), LuzError> {
    // let mut compiler = Compiler::new(input);
    let ctx = Compiler::compile(filename.clone(), input, stmts)?;
    // let mut ctx = CompilerCtx::new_main();
    // for stmt in stmts {
    //     compiler.visit_stat(&stmt, &mut ctx)?;
    // }
    debug!("{}", ctx.instructions_to_string());
    let mut runner = runner::Runner::new(
        filename,
        input,
        ctx.scope_clone(),
        make_env_table(luz_table!().as_table_or_err()?),
    );
    let res = runner.run()?;
    info!("{:?}", res);
    Ok(())
}

pub fn load(
    filename: Option<String>,
    input: String,
    name: String,
    parent_scope: Option<ScopeRef>,
    upvalues: Vec<Upvalue>,
) -> Result<LuzFunction, LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, &input)?;

    let stmts = parse_script(&mut stmts)?;
    trace!("{:#?}", &stmts);

    let mut compiler = Compiler::new(&input);
    let mut ctx = CompilerCtx::new_chunk(
        filename.as_ref().unwrap_or(&String::new()).clone(),
        name,
        parent_scope,
        upvalues,
    );
    for stmt in stmts {
        compiler.visit_stat(&stmt, &mut ctx)?;
    }

    debug!("{}", ctx.instructions_to_string());

    // let res = runner.run()?;
    // info!("{:?}", res);
    // Ok(())
    let filename = filename.unwrap_or_default();
    Ok(LuzFunction::new_native(
        0,
        Rc::new(RefCell::new(move |run: &mut Runner, _args: Vec<LuzObj>| {
            let mut runner = runner::Runner::new_chunk(
                filename.clone(),
                &input,
                ctx.scope_clone(),
                run.registry(),
            );
            runner
                .run()
                .map_err(|e| LuzRuntimeError::message(e.to_string()))
        })),
    ))
}
