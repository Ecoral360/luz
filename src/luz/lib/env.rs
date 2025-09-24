use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{
    compiler::ctx::{RegisterBuilder, Scope, Upvalue},
    load,
    luz::{
        lib::{math::math_lib, require::package_lib, string::string_lib, LuzNativeLib},
        obj::{LuzObj, Numeral, TableRef},
    },
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn make_env_table(registry: TableRef) -> (LuzObj, TableRef) {
    let table = luz_table! {
        _VERSION: LuzObj::str("Lua 5.4"),
        _LUZ_VERSION: LuzObj::str("Luz 0.1"),

        print: luz_fn!([0, _runner](*args) {
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

        type: luz_fn!([1, _runner](*args) {
            let arg = args.get(0).unwrap_or(&LuzObj::Nil);
            Ok(vec![LuzObj::String(arg.get_type().to_string())])
        }),

        assert: luz_fn!([0](*args) {
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
                    Err(LuzRuntimeError::message("assertion failed!"))
                } else {
                    // Here we use swap_remove instead of just 'remove'
                    // because it's O(1) and we don't care about the vararg
                    // vector afterward
                    Err(LuzRuntimeError::ErrorObj(args.pop_front().unwrap()))
                }
            }
        }),

        load: luz_fn!([1, runner](*args) {
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

        select: luz_fn!([1, _runner](*args) {
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
        }),

        pcall: luz_fn!([1, runner](*args) {
            let LuzObj::Function(f) = args.pop_front().unwrap() else { unreachable!() };
            let f = f.borrow();

            let nb_fixed = f.nb_fixed_params();

            let mut args_iter = args.into_iter();
            let mut fixed_args = Vec::with_capacity(nb_fixed as usize);
            for _ in 0..nb_fixed {
                fixed_args.push(args_iter.next().unwrap_or(LuzObj::Nil));
            }

            // We collect the rest if there is
            let varargs = args_iter.collect();

            let results = f.call(runner, fixed_args, varargs);

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

        rawget: luz_fn!([2, _runner](*args) {
            let LuzObj::Table(t) = &args[0] else {
                return Err(LuzRuntimeError::message("bad argument #1 to 'rawget' (a table expected)"))
            };
            let t = t.borrow();
            Ok(vec![t.get(&args[1]).clone()])
        }),

        ipairs: luz_fn!([1](table @ LuzObj::Table(..)) {

            let iter_func = luz_fn!([2](LuzObj::Table(state), LuzObj::Numeral(Numeral::Int(ctrl))) {
                let table = state.borrow();
                let obj = table.get(&LuzObj::int(ctrl + 1));

                if obj.is_nil() {
                    Ok(vec![LuzObj::Nil])
                } else {
                    Ok(vec![LuzObj::int(ctrl + 1), obj.clone()])
                }
            });

            Ok(vec![iter_func, table, LuzObj::int(0)])
        }),

        next: luz_fn!([1](LuzObj::Table(table), index) {
            let table = table.borrow();
            if index.is_nil() {
                let next = table.first_idx();
                let value = table.get(&next);
                return Ok(vec![next, value.clone()]);
            }
            let next = table.next_idx(&index);
            if next.is_nil() {
                Ok(vec![next])
            } else {
                let value = table.get(&next);
                Ok(vec![next, value.clone()])
            }
        }),

        pairs: luz_fn!([1, runner](t @ LuzObj::Table(..)) {
            let next = runner.get_val("next").unwrap_or(LuzObj::Nil);
            return Ok(vec![next, t, LuzObj::Nil])
        })
    };

    add_lib(&table, string_lib(Rc::clone(&registry)));
    add_lib(&table, package_lib(Rc::clone(&registry)));
    add_lib(&table, math_lib(Rc::clone(&registry)));

    let global_env = table;
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

fn add_lib(glob_table: &LuzObj, lib: LuzNativeLib) {
    let glob_table = glob_table.as_table_or_err().unwrap();
    for (name, val) in lib.exports {
        glob_table.borrow_mut().insert(LuzObj::str(&name), val);
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
