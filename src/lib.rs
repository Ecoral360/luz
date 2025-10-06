use std::{cell::RefCell, fs, rc::Rc};

use log::{debug, info, trace};
use luz::err::LuzError;
use pest::Parser;
use pest_derive::Parser;

use crate::{
    ast::{parser::parse_script, Stat},
    compiler::{
        ctx::{CompilerCtx, Scope, ScopeRef, Upvalue},
        visitor::Visitor,
        Compiler,
    },
    luz::{
        lib::env::make_env_table,
        obj::{LuzFunction, LuzObj, LuzType, TableRef},
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

pub fn run(input: &str) -> Result<Vec<LuzObj>, LuzError> {
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

pub fn run_file(path: &str) -> Result<Vec<LuzObj>, LuzError> {
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

pub fn parse_file(path: &str) -> Result<(), LuzError> {
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
        }
        Err(err) => {
            dbg!(err);
            panic!();
        }
    }
    Ok(())
}
fn run_compiler(filename: String, input: &str, stmts: Vec<Stat>) -> Result<Vec<LuzObj>, LuzError> {
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
    Ok(res)
}

pub fn run_repl() -> Result<(), LuzError> {
    use std::io::{self, Write};

    let mut input = String::new();
    let mut buffer = String::new();
    let mut line_num = 1;

    let mut ctx = CompilerCtx::new_main("stdin".to_string());
    let mut runner = Runner::new(
        String::from("REPL"),
        &buffer,
        ctx.scope_clone(),
        make_env_table(luz_table!().as_table_or_err()?),
    );

    loop {
        print!("{}> ", line_num);
        io::stdout().flush().unwrap();

        input.clear();
        io::stdin().read_line(&mut input).unwrap();

        if input.trim().is_empty() {
            continue;
        }

        buffer.push_str(&input);

        let mut stmts = LuaParser::parse(Rule::Chunk, &buffer);

        match &mut stmts {
            Ok(stmts) => {
                let stmts = parse_script(stmts)?;
                run_compiler("stdin".to_string(), &input, stmts)?;
                line_num += 1;
            }
            Err(err) => {
                if err.to_string().contains("end of file") {
                    line_num += 1;
                    continue;
                } else {
                    println!("Error: {}", err);
                    buffer.clear();
                    line_num = 1;
                }
            }
        }
    }
}

pub fn load(
    filename: Option<String>,
    input: String,
    name: String,
    env: LuzObj,
) -> Result<LuzFunction, LuzError> {
    let mut stmts = LuaParser::parse(Rule::Chunk, &input)?;

    let stmts = parse_script(&mut stmts)?;
    trace!("{:#?}", &stmts);

    let mut compiler = Compiler::new(&input);
    let mut ctx = CompilerCtx::new_chunk(
        filename.as_ref().unwrap_or(&String::new()).clone(),
        name,
        None,
        vec![],
    );

    for stmt in stmts {
        compiler.visit_stat(&stmt, &mut ctx)?;
    }

    compiler.finish(&mut ctx);

    ctx.scope_mut().set_upvalue_value(0, env);

    // debug!("{}", ctx.instructions_to_string());

    Ok(LuzFunction::new_user(
        0,
        ctx.scope_clone(),
        filename.unwrap_or_default(),
    ))

    // let filename = filename.unwrap_or_default();
    // Ok(LuzFunction::new_native(
    //     0,
    //     Some(parent_scope),
    //     Rc::new(RefCell::new(move |run: &mut Runner, _args: Vec<LuzObj>| {
    //         let mut runner = runner::Runner::new_chunk(
    //             filename.clone(),
    //             &input,
    //             ctx.scope_clone(),
    //             run.registry(),
    //         );
    //         runner
    //             .run()
    //             .map_err(|e| LuzRuntimeError::message(e.to_string()))
    //     })),
    // ))
}
