use crate::{
    borrowed,
    luz::obj::{fail_value, LuzObj, Numeral, TableRef},
    luz_fn, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn debug_lib(_registry: TableRef) -> LuzObj {
    let table = luz_table! {
        getinfo: luz_fn!([1](f) {

            let result = match f {
                LuzObj::Function(f) => {
                    borrowed!(f);
                    let Some(scope) = f.scope() else {
                        return Err(LuzRuntimeError::message(
                            "bad argument #1 for 'debug.getinfo' (native function cannot be debugged)."
                        ));
                    };


                    borrowed!(scope);
                    luz_table!{
                        source: LuzObj::str(scope.name().cloned().unwrap_or_default()),
                    }
                }
                _ => Err(LuzRuntimeError::message("bad argument #1 for 'debug.getinfo' (function or integer expected)."))?
            };

            Ok(vec![result])
        }),

        getupvalue: luz_fn!([2](LuzObj::Function(f), LuzObj::Numeral(Numeral::Int(up))) {
            borrowed!(f);
            let Some(scope) = f.scope() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 for 'debug.getupvalue' (native function cannot be debugged)."
                ));
            };

            borrowed!(scope);

            let upvalue = scope.get_opt_upvalue(up as u8);
            match upvalue {
                Some(upvalue) => {
                    let value = scope.get_upvalue_value(upvalue.addr);
                    Ok(vec![LuzObj::str(&upvalue.name), value.unwrap_or_default()])
                }
                None => Ok(vec![fail_value()]),
            }
        }),

        setupvalue: luz_fn!([3](LuzObj::Function(f), LuzObj::Numeral(Numeral::Int(up)), val) {
            borrowed!(f);
            let Some(scope) = f.scope() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 for 'debug.getupvalue' (native function cannot be debugged)."
                ));
            };

            borrowed!(mut scope);

            let upvalue = scope.get_opt_upvalue(up as u8).cloned();
            match upvalue {
                Some(upvalue) => {
                    scope.set_upvalue_value(upvalue.addr, val);
                    Ok(vec![LuzObj::str(&upvalue.name)])
                }
                None => Ok(vec![fail_value()]),
            }
        }),
    };

    table
}
