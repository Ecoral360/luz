use std::collections::VecDeque;

use crate::{
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, Numeral, TableRef},
    },
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn string_lib(_registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        len: luz_fn!([1, _runner, args]() {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.len' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::Numeral(Numeral::Int(s.chars().count() as i64))])
        }),
        lower: luz_fn!([1, _runner, args]() {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.lower' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::String(s.to_lowercase())])
        }),
        upper: luz_fn!([1, _runner, args]() {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.upper' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::String(s.to_uppercase())])
        }),
    };

    LuzNativeLib {
        exports: vec![(String::from("string"), table)],
    }
}
