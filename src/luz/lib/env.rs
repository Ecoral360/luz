use std::{cell::RefCell, collections::VecDeque, rc::Rc, str::FromStr};

use crate::{
    compiler::ctx::{RegisterBuilder, Scope, Upvalue},
    load,
    luz::{
        lib::{
            math::math_lib, require::package_lib, string::string_lib, table::table_lib,
            LuzNativeLib,
        },
        obj::{LuzObj, Numeral, TableRef},
    },
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn make_env_table(registry: TableRef) -> TableRef {
    // adding the string metatable to the registry
    {
        let mut registry = registry.borrow_mut();
        registry.insert(
            LuzObj::str(":hidden.string.metatable:"),
            luz_table! {
                __add: luz_fn!([1]() { Ok(vec![])})
            },
        );
    }

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

        type: luz_fn!([0, _runner](*args) {
            let Some(arg) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'type' (value expected)",
                ));
            };
            Ok(vec![LuzObj::String(arg.get_type().to_string())])
        }),

        error: luz_fn!([1](message) {
            Err(LuzRuntimeError::message(message.to_string()))
        }),


        getmetatable: luz_fn!([1, runner](obj) {
            match obj {
                LuzObj::String(..) => {
                    let registry = runner.registry();
                    let registry = registry.borrow();
                    let meta = registry.rawget(&LuzObj::str(":hidden.string.metatable:"));
                    Ok(vec![meta.clone()])
                }
                _ => Ok(vec![LuzObj::Nil])
            }
        }),

        setmetatable: luz_fn!([2](LuzObj::Table(table), metatable @ (LuzObj::Table(..) | LuzObj::Nil)) {
            let mut table_obj = table.borrow_mut();
            let existing_meta = table_obj.rawget_metatable(&LuzObj::str("__metatable"));
            if !existing_meta.is_nil() {
                return Err(LuzRuntimeError::message(
                    "Cannot set the metatable for this table because the __metatable metafield is set."
                ));
            }
            if metatable.is_nil() {
                table_obj.set_metatable(None);
            } else {
                let metatable = metatable.as_table_or_err()?;
                table_obj.set_metatable(Some(metatable));
            }

            Ok(vec![LuzObj::Table(Rc::clone(&table))])
        }),

        assert: luz_fn!([0](*args) {
            let mut args = args;
            let Some(condition) = args.get(0) else {
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

        load: luz_fn!([1](*args) {
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

            let r = load(None, input.clone(), input.clone(), None, vec![])
                .map_err(|e| LuzRuntimeError::message(e.to_string()))?;

            Ok(vec![LuzObj::Function(Rc::new(RefCell::new(r)))])
        }),

        select: luz_fn!([1](*args) {
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
            let Some((LuzObj::Function(f), prefix_args)) = args.pop_front().unwrap().callable() else { unreachable!() };
            let f = f.borrow();

            let nb_fixed = f.nb_fixed_params() + prefix_args.len() as u32;

            let mut args_iter = prefix_args.into_iter().chain(args.into_iter());
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

        xpcall: luz_fn!([1, runner](f, msgh, *args) {
            let Some((LuzObj::Function(f), prefix_args)) = f.callable() else { unreachable!() };
            let f = f.borrow();

            let nb_fixed = f.nb_fixed_params() + prefix_args.len() as u32;

            let mut args_iter = prefix_args.into_iter().chain(args.into_iter());
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
                    let Some((LuzObj::Function(msgh), mut prefix_args)) = msgh.callable() else { unreachable!() };
                    let msgh = msgh.borrow();
                    let err_obj = match err {
                        LuzRuntimeError::ErrorObj(luz_obj) => luz_obj,
                        LuzRuntimeError::Crashing(c) => LuzObj::String(c)
                    };
                    prefix_args.push(err_obj);
                    let mut results = msgh.call(runner, prefix_args, vec![]);
                    let mut result = None;
                    for _ in 0..100 {
                        match results {
                            Ok(obj) => {
                                result = Some(obj);
                                break;
                            }
                            Err(err) => {
                                let err_obj = match err {
                                    LuzRuntimeError::ErrorObj(luz_obj) => luz_obj,
                                    LuzRuntimeError::Crashing(c) => LuzObj::String(c)
                                };
                                results = msgh.call(runner, vec![err_obj], vec![]);
                            }
                        }
                    }
                    if let Some(result) = result {
                        vec![
                            LuzObj::Boolean(false),
                            result.get(0).unwrap_or(&LuzObj::Nil).clone()
                        ]
                    } else {
                        vec![
                            LuzObj::Boolean(false),
                            LuzObj::str("error in error handling"),
                        ]
                    }
                }
            })
        }),

        rawget: luz_fn!([2, _runner](*args) {
            let LuzObj::Table(t) = &args[0] else {
                return Err(LuzRuntimeError::message("bad argument #1 to 'rawget' (a table expected)"))
            };
            let t = t.borrow();
            Ok(vec![t.rawget(&args[1]).clone()])
        }),

        ipairs: luz_fn!([1](table @ LuzObj::Table(..)) {

            let iter_func = luz_fn!([2](LuzObj::Table(state), LuzObj::Numeral(Numeral::Int(ctrl))) {
                let table = state.borrow();
                let obj = table.rawget(&LuzObj::int(ctrl + 1));

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
                let value = table.rawget(&next);
                return Ok(vec![next, value.clone()]);
            }
            let next = table.next_idx(&index);
            if next.is_nil() {
                Ok(vec![next])
            } else {
                let value = table.rawget(&next);
                Ok(vec![next, value.clone()])
            }
        }),

        pairs: luz_fn!([1, runner](t @ LuzObj::Table(..)) {
            let next = runner.get_val("next").unwrap_or(LuzObj::Nil);
            return Ok(vec![next, t, LuzObj::Nil])
        }),

        tonumber: luz_fn!([1](e, base @ (LuzObj::Numeral(Numeral::Int(..)) | LuzObj::Nil)) {
            match e {
                LuzObj::String(s) => {
                    let num = Numeral::from_str(&s).map(|num| LuzObj::Numeral(num)).unwrap_or(LuzObj::Nil);
                    Ok(vec![num])
                }
                LuzObj::Numeral(..) => Ok(vec![e]),
                _ => Ok(vec![LuzObj::Nil])
            }
        }),
    };

    add_lib(&table, string_lib(Rc::clone(&registry)));
    add_lib(&table, package_lib(Rc::clone(&registry)));
    add_lib(&table, math_lib(Rc::clone(&registry)));
    add_lib(&table, table_lib(Rc::clone(&registry)));

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
    registry.borrow_mut().insert(LuzObj::str("_G"), global_env);
    registry
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
