use crate::{
    luz::obj::{LuzFunction, LuzObj, TableRef},
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn debug_lib(_registry: TableRef) -> LuzObj {
    let table = luz_table! {
        getinfo: luz_fn!([1](f) {

            let result = match f {
                LuzObj::Function(f) => {
                    let f = f.borrow();
                    let Some(scope) = f.scope() else {
                        return Err(LuzRuntimeError::message(
                            "bad argument #1 for 'debug.getinfo' (native function cannot be debugged)."
                        ));
                    };


                    let scope = scope.borrow();
                    luz_table!{
                        source: LuzObj::str(scope.name().cloned().unwrap_or_default()),
                    }
                }
                _ => Err(LuzRuntimeError::message("bad argument #1 for 'debug.getinfo' (function or integer expected)."))?
            };

            Ok(vec![result])
        }),
    };

    table
}
