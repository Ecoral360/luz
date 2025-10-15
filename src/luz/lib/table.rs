use std::{cell::RefCell, cmp::Ordering, collections::HashMap, rc::Rc};

use crate::{
    borrowed,
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, Numeral, Table, TableRef},
    },
    luz_fn, luz_let, luz_table,
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

        insert: luz_fn!([2](LuzObj::Table(list), *args) {
            if args.is_empty() {
                return Err(LuzRuntimeError::message("wrong number of arguments to 'insert'"));
            }

            let list_len = list.borrow().len() + 1;
            borrowed!(mut list);

            if args.len() == 1 {
                // this is safe because the len is 1
                let value = args.pop_front().unwrap();
                list.insert(value, list_len)
                    .map_err(|err| LuzRuntimeError::message(format!("bad argument #2 to 'insert' ({})", err)))?;
            } else {
                // this is safe because the len is at least 2
                luz_let!(LuzObj::Numeral(pos) = args.pop_front().unwrap());
                if !pos.is_int_compatible() {
                    Err(LuzRuntimeError::message("bad argument #2 to 'insert' (number has no integer representation)"))?;
                }

                let value = args.pop_front().unwrap();
                list.insert(value, pos.as_int() as usize)
                    .map_err(|err| LuzRuntimeError::message(format!("bad argument #2 to 'insert' ({})", err)))?;
            }

            Ok(vec![])
        }),

        concat: luz_fn!([1, runner](LuzObj::Table(list), sep, i, j) {
            borrowed!(list);

            luz_let!(LuzObj::String(sep) = sep.or(LuzObj::str("")));
            luz_let!(LuzObj::Numeral(Numeral::Int(i)) = i.or(LuzObj::int(1)));
            luz_let!(LuzObj::Numeral(Numeral::Int(j)) = j.or(LuzObj::int(list.len() as i64)));

            let mut final_str = vec![];
            for idx in i..=j {
                let obj = list.get(runner, &LuzObj::int(idx as i64))?.unwrap_or(LuzObj::Nil);
                match obj {
                    LuzObj::String(s) => {
                        final_str.extend(s);
                    }
                    LuzObj::Numeral(n) => {
                        final_str.extend(n.to_string().into_bytes());
                    }
                    _ => return Err(LuzRuntimeError::message(
                        format!("invalid value ({}) at index {} in table for 'concat'", obj.get_type(), idx)
                    ))
                }

                if idx < j {
                    final_str.extend(&sep);
                }

            }

            Ok(vec![LuzObj::String(final_str)])
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
