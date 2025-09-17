use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
    result,
};

use crate::{
    compiler::ctx::{RegisterBuilder, Scope, Upvalue},
    load,
    luz::obj::{LuzFunction, LuzObj, Numeral, Table},
    runner::{err::LuzRuntimeError, Runner},
};

macro_rules! luz_fn {
    ([$nb_fixed:literal, $runner:ident, $args:ident, $vararg:ident]() $body:block ) => {
        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_native(
            $nb_fixed,
            Rc::new(RefCell::new(
                |$runner: &mut Runner, $args: Vec<LuzObj>, $vararg: Vec<LuzObj>| $body,
            )),
        ))))
    };
}

macro_rules! luz_table {
    ($($key:ident : $val:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut table = HashMap::new();
        {
            $(table.insert(LuzObj::str(stringify!($key)), $val);)*
        }

        LuzObj::Table(Rc::new(RefCell::new(Table::new(table, None))))
    }};
}

fn make_env_table() -> LuzObj {
    let mut table = HashMap::new();

    table.insert(
        LuzObj::str("print"),
        luz_fn!([0, _runner, _args, vararg]() {
            println!(
                "{}",
                vararg
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
        luz_fn!([1, _runner, args, _vararg]() {
            let arg = args.get(0).unwrap_or(&LuzObj::Nil);
            Ok(vec![LuzObj::String(arg.get_type().to_string())])
        }),
    );

    table.insert(
        LuzObj::str("assert"),
        luz_fn!([0, _runner, _args, vararg]() {
            let mut vararg = vararg;
            let Some(condition) = vararg.get(0).cloned() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'assert' (value expected)",
                ));
            };
            if condition.is_truthy() {
                Ok(vararg)
            } else {
                if vararg.len() < 2 {
                    Err(LuzRuntimeError::message("assertion failed!"))
                } else {
                    // Here we use swap_remove instead of just 'remove'
                    // because it's O(1) and we don't care about the vararg
                    // vector afterward
                    Err(LuzRuntimeError::ErrorObj(vararg.swap_remove(1)))
                }
            }
        }),
    );

    table.insert(
        LuzObj::str("load"),
        luz_fn!([1, runner, args, _vararg]() {
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
            let r = load(&input, input.clone(), runner.env_scope(), upvalues)
                .map_err(|e| LuzRuntimeError::message(e.to_string()))?;

            Ok(vec![LuzObj::Function(Rc::new(RefCell::new(r)))])
        }),
    );
    table.insert(
        LuzObj::str("select"),
        luz_fn!([1, _runner, args, vararg]() {
            match &args[0] {
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
        luz_fn!([1, runner, args, vararg]() {
            let LuzObj::Function(f) = &args[0] else { unreachable!() };
            let f = f.borrow();

            let results = match *f {
                LuzFunction::User { ref scope, .. } => {
                    let mut fc_scope = scope.borrow().clone();
                    for (i, arg) in args.iter().enumerate() {
                        fc_scope.set_reg_val(i as u8, arg.clone());
                    }
                    let mut fc_runner = Runner::new(Rc::new(RefCell::new(fc_scope)));
                    fc_runner.set_vararg(Some(vararg));

                    fc_runner.run().map_err(|err| LuzRuntimeError::ErrorObj(LuzObj::str(&err.to_string())))
                }
                LuzFunction::Native { ref fn_ptr, .. } => {
                    let mut fn_ptr = fn_ptr.borrow_mut();
                    (fn_ptr)(runner, args.clone(), vararg)
                }
            };
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
        LuzObj::str("string"),
        luz_table! {
            len: luz_fn!([1, _runner, args, _vararg]() {
                let mut args = VecDeque::from(args);
                let Some(LuzObj::String(s)) = args.pop_front() else {
                    return Err(LuzRuntimeError::message(
                        "bad argument #1 to 'string.len' (string value expected)",
                    ));
                };
                Ok(vec![LuzObj::Numeral(Numeral::Int(s.chars().count() as i64))])
            }),
            lower: luz_fn!([1, _runner, args, _vararg]() {
                let mut args = VecDeque::from(args);
                let Some(LuzObj::String(s)) = args.pop_front() else {
                    return Err(LuzRuntimeError::message(
                        "bad argument #1 to 'string.lower' (string value expected)",
                    ));
                };
                Ok(vec![LuzObj::String(s.to_lowercase())])
            }),
            upper: luz_fn!([1, _runner, args, _vararg]() {
                let mut args = VecDeque::from(args);
                let Some(LuzObj::String(s)) = args.pop_front() else {
                    return Err(LuzRuntimeError::message(
                        "bad argument #1 to 'string.upper' (string value expected)",
                    ));
                };
                Ok(vec![LuzObj::String(s.to_uppercase())])
            }),
        },
    );

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
    global_env
}

pub fn get_builtin_scope() -> Rc<RefCell<Scope>> {
    let mut env = Scope::new_global();
    // env.get_or_add_const(obj)

    env.push_reg(
        RegisterBuilder::default()
            .name(Some(String::from("_ENV")))
            .val(Some(make_env_table()))
            .free(false),
    );

    Rc::new(RefCell::new(env))
}
