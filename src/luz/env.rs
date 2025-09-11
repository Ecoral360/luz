use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::{RegisterBuilder, Scope},
    luz::{
        err::LuzError,
        obj::{LuzFunction, LuzObj, Table},
    },
    runner::Runner,
};

fn make_env_table() -> Table {
    let mut table = HashMap::new();

    table.insert(
        LuzObj::str("print"),
        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_native(Rc::new(
            RefCell::new(|_: &mut Runner, args: Vec<LuzObj>| {
                println!(
                    "{}",
                    args.iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join("\t")
                );
                Ok(vec![])
            }),
        ))))),
    );

    table.insert(
        LuzObj::str("type"),
        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_native(Rc::new(
            RefCell::new(|_: &mut Runner, args: Vec<LuzObj>| {
                let arg = args.get(0).cloned().unwrap_or(LuzObj::Nil);
                Ok(vec![LuzObj::String(arg.get_type().to_string())])
            }),
        ))))),
    );

    table.insert(
        LuzObj::str("assert"),
        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_native(Rc::new(
            RefCell::new(|_: &mut Runner, args: Vec<LuzObj>| {
                let condition = args.get(0).cloned().unwrap_or(LuzObj::Nil);
                if condition.is_truthy() {
                    Ok(vec![condition])
                } else {
                    let message = args
                        .get(1)
                        .cloned()
                        .unwrap_or(LuzObj::String("assertion failed!".to_string()));
                    Err(LuzError::RuntimeError(message.to_string()))
                }
            }),
        ))))),
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
