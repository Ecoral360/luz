use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
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
    ($($key:ident : $val:expr)*) => {{
        #[allow(unused_mut)]
        let mut table = HashMap::new();
        {
            $(table.insert(LuzObj::str(stringify!($key)), $val);)*
        }

        LuzObj::Table(Rc::new(RefCell::new(Table::new(table, None))))
    }};
}

fn make_env_table() -> Table {
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
            })
        },
    );

    Table::new(table, None)
}

pub fn get_builtin_scope() -> Rc<RefCell<Scope>> {
    let mut env = Scope::new(String::from("GLOBAL"), None);

    env.push_reg(
        RegisterBuilder::default()
            .name(Some(String::from("_ENV")))
            .val(Some(LuzObj::Table(Rc::new(RefCell::new(make_env_table())))))
            .free(false),
    );

    Rc::new(RefCell::new(env))
}
