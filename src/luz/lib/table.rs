use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use crate::{
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, Table, TableRef},
    },
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn table_lib(_registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        pack: luz_fn!([1](*args) {
            let mut table = Table::new(HashMap::new(), None);
            let len = args.len() as i64;
            for arg in args {
                table.push(arg);
            }
            table.rawset(LuzObj::str("n"), LuzObj::int(len));
            Ok(vec![LuzObj::Table(Rc::new(RefCell::new(table)))])
        }),

        remove: luz_fn!([1](LuzObj::Table(list), *args) {
            let mut list = list.borrow_mut();
            let pos = args.pop_front().unwrap_or(LuzObj::int(list.len() as i64));

            Ok(vec![list.remove(&pos)])
        }),

        sort: luz_fn!([1, runner](LuzObj::Table(list), *args) {
            let comp = args.pop_front();
            if let Some(comp) = comp {
                let (callable, prefix_args) = comp.callable()
                    .ok_or(LuzRuntimeError::message("bad argument #2 in 'table.sort' (comp must be callable)"))?;
                let mut l = list.borrow_mut();
                let arr = l.arr_mut();
                let mut err = None;
                arr.sort_by(|a, b| {
                    let mut args = prefix_args.clone();
                    args.push(a.clone());
                    args.push(b.clone());
                    let result = callable.call(runner, args, vec![]);
                    if let Err(run_err) = result {
                        err = Some(run_err);
                        return Ordering::Equal;
                    }
                    if result.unwrap().get(0).unwrap_or(&LuzObj::Nil).is_true() {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });

                if let Some(err) = err {
                    Err(err)?
                }
            } else {
                let mut l = list.borrow_mut();
                let arr = l.arr_mut();
                arr.sort_by(|a, b| {
                    if a < b {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                });
            }
            Ok(vec![])
        })
    };

    LuzNativeLib {
        exports: vec![(String::from("table"), table)],
    }
}
