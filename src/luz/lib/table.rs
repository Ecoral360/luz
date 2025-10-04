use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, Table, TableRef},
    },
    luz_fn, luz_table,
};

pub fn table_lib(_registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        pack: luz_fn!([1](*args) {
            let mut table = Table::new(HashMap::new(), None);
            let len = args.len() as i64;
            for arg in args {
                table.push(arg);
            }
            table.insert(LuzObj::str("n"), LuzObj::int(len));
            Ok(vec![LuzObj::Table(Rc::new(RefCell::new(table)))])
        }),
    };

    LuzNativeLib {
        exports: vec![(String::from("table"), table)],
    }
}
