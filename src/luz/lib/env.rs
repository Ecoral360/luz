use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use crate::{
    compiler::ctx::{RegisterBuilder, Scope, Upvalue},
    load,
    luz::{
        lib::{math::math_lib, require::package_lib, string::string_lib, LuzNativeLib},
        obj::{LuzObj, Numeral, Table, TableRef},
    },
    luz_fn,
    runner::err::LuzRuntimeError,
};

pub fn make_env_table(registry: TableRef) -> (LuzObj, TableRef) {
    let mut table = HashMap::new();

    table.insert(LuzObj::str("_VERSION"), LuzObj::str("Lua 5.4"));
    table.insert(LuzObj::str("_LUZ_VERSION"), LuzObj::str("Luz 0.1"));

    table.insert(
        LuzObj::str("print"),
        luz_fn!([0, _runner, args]() {
            println!(
                "{}",
                args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join("\t")
            );
            Ok(vec![])
        }),
    );

    table.insert(
        LuzObj::str("type"),
        luz_fn!([1, _runner, args]() {
            let arg = args.get(0).unwrap_or(&LuzObj::Nil);
            Ok(vec![LuzObj::String(arg.get_type().to_string())])
        }),
    );

    table.insert(
        LuzObj::str("assert"),
        luz_fn!([0, runner, args]() {
            let mut args = args;
            let Some(condition) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'assert' (value expected)",
                ));
            };
            if condition.is_truthy() {
                Ok(args.into())
            } else {
                if args.len() < 2 {
                    runner.dump_trace();
                    Err(LuzRuntimeError::message("assertion failed!"))
                } else {
                    // Here we use swap_remove instead of just 'remove'
                    // because it's O(1) and we don't care about the vararg
                    // vector afterward
                    Err(LuzRuntimeError::ErrorObj(args.pop_front().unwrap()))
                }
            }
        }),
    );

    table.insert(
        LuzObj::str("load"),
        luz_fn!([1, runner, args]() {
            let mut args = VecDeque::from(args);
            let Some(chunk) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'load' (value expected)",
                ));
            };

            let LuzObj::String(input) = chunk else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'load' (value expected)",
                ));
            };

            // let name = args.pop_front();

            let upvalues = vec![Upvalue::new("_ENV".to_owned(), 0, 0, true)];
            let r = load(None, input.clone(), input.clone(), runner.env_scope(), upvalues)
                .map_err(|e| LuzRuntimeError::message(e.to_string()))?;

            Ok(vec![LuzObj::Function(Rc::new(RefCell::new(r)))])
        }),
    );
    table.insert(
        LuzObj::str("select"),
        luz_fn!([1, _runner, args]() {
            let arg = args.pop_front().unwrap();
            let vararg = args;
            match &arg {
                LuzObj::Numeral(Numeral::Int(i)) => {
                    let vararg_iter = vararg.clone().into_iter();
                    let idx = if *i < 0 {
                        vararg.len() - i.abs() as usize
                    } else {
                        *i as usize
                    };
                    Ok(vararg_iter.skip(idx).collect())
                }
                LuzObj::String(s) if s == "#" => {
                    Ok(vec![LuzObj::int(vararg.len() as i64)])
                }
                _ => Err(LuzRuntimeError::message(format!("bad argument #1 to 'select' (integer or '#' expected)")))
            }
        }));

    table.insert(
        LuzObj::str("pcall"),
        luz_fn!([1, runner, args]() {
            let LuzObj::Function(f) = args.pop_front().unwrap() else { unreachable!() };
            let f = f.borrow();

            let results = f.call(runner, vec![], args.into());

            Ok(match results {
                Ok(mut results) => {
                    results.insert(0, LuzObj::Boolean(true));
                    results
                }
                Err(err) => {
                    vec![
                        LuzObj::Boolean(false),
                        match err {
                            LuzRuntimeError::ErrorObj(luz_obj) => luz_obj,
                            LuzRuntimeError::Crashing(c) => LuzObj::String(c)
                        }
                    ]
                }
            })
        }),
    );

    table.insert(
        LuzObj::str("rawget"), 
        luz_fn!([2, _runner, args]() {
            let LuzObj::Table(t) = &args[0] else {
                return Err(LuzRuntimeError::message("bad argument #1 to 'rawget' (a table expected)"))
            };
            let t = t.borrow();
            Ok(vec![t.get(&args[1]).clone()])
        }),
    );

    add_lib(&mut table, string_lib(Rc::clone(&registry)));
    add_lib(&mut table, package_lib(Rc::clone(&registry)));
    add_lib(&mut table, math_lib(Rc::clone(&registry)));

    let global_env = LuzObj::Table(Rc::new(RefCell::new(Table::new(table, None))));
    {
        let LuzObj::Table(ref global_table) = global_env else {
            unreachable!()
        };
        let global_table_ref = Rc::clone(global_table);
        global_table
            .borrow_mut()
            .insert(LuzObj::str("_G"), LuzObj::Table(global_table_ref));
    }
    (global_env, registry)
}

fn add_lib(glob_table: &mut HashMap<LuzObj, LuzObj>, lib: LuzNativeLib) {
    for (name, val) in lib.exports {
        glob_table.insert(LuzObj::str(&name), val);
    }
}

pub fn get_builtin_scope() -> Rc<RefCell<Scope>> {
    let mut env = Scope::new_global();
    // env.get_or_add_const(obj)

    env.push_reg(
        RegisterBuilder::default()
            .name(Some(String::from("_ENV")))
            .val(None)
            .free(false),
    );

    Rc::new(RefCell::new(env))
}
