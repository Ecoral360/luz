use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    compiler::{ctx::CompilerCtx, RegisterBuilder, Scope},
    luz::obj::{LuzFunction, LuzObj, Table},
};

fn make_env_table() -> Table {
    let mut table = HashMap::new();

    table.insert(
        LuzObj::str("print"),
        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_native(Rc::new(
            RefCell::new(|args: Vec<LuzObj>| {
                println!(
                    "{}",
                    args.iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>()
                        .join("\t")
                );
                vec![]
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
