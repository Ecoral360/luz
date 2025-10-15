use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use std::io::Write;

use crate::{
    borrowed,
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, Numeral, Table, TableRef},
        userdata::Userdata,
    },
    luz_fn, luz_let, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn io_lib(_registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        output: luz_fn!([0]() {
            Ok(vec![
                LuzObj::Userdata(Rc::new(RefCell::new(Userdata::Full(Box::new(std::io::stdout())))))
            ])
        }),

        write: luz_fn!([0](*args) {
            write!(
                std::io::stdout(),
                "{}",
                args.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join("")
            )
            .map_err(|err| LuzRuntimeError::message(format!("error while writing to stdout ({})", err)))?;
            Ok(vec![])
        })
    };

    LuzNativeLib {
        exports: vec![(String::from("io"), table)],
    }
}
