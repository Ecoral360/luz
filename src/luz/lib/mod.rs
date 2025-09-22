use derive_new::new;

use crate::luz::obj::LuzObj;

pub mod env;
mod require;
mod string;

#[macro_export]
macro_rules! luz_fn {
    ([$nb_fixed:literal, $runner:ident, $args:ident]($($arg:pat),* $(,)?) $body:block ) => {
        LuzObj::Function(std::rc::Rc::new(std::cell::RefCell::new(
            $crate::luz::obj::LuzFunction::new_native(
                $nb_fixed,
                std::rc::Rc::new(std::cell::RefCell::new(
                    |$runner: &mut $crate::runner::Runner, $args: Vec<$crate::luz::obj::LuzObj>| {
                        let mut _i = 0;
                        $(
                            #[allow(irrefutable_let_patterns)]
                            let $arg = $args.get(_i).unwrap_or(&$crate::luz::obj::LuzObj::Nil)
                            else {
                                return Err($crate::LuzRuntimeError::message(format!("bad argument #{} ({})", _i + 1, stringify!($arg))));
                            };
                            _i+=1;
                        )*
                        $body
                    },
                )),
            ),
        )))
    };
}

#[macro_export]
macro_rules! luz_table {
    ($($key:ident : $val:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut table = std::collections::HashMap::new();
        {
            $(table.insert($crate::luz::obj::LuzObj::str(stringify!($key)), $val);)*
        }

        $crate::luz::obj::LuzObj::Table(std::rc::Rc::new(std::cell::RefCell::new($crate::luz::table::Table::new(table, None))))
    }};
    ($($val:expr),* $(,)?) => {{
        #[allow(unused_mut)]
        let mut table = $crate::luz::table::Table::new(std::collections::HashMap::new(), None);
        {
            $(table.push($val);)*
        }

        $crate::luz::obj::LuzObj::Table(std::rc::Rc::new(std::cell::RefCell::new(table)))
    }};
}

#[macro_export]
macro_rules! luz_let {
    ($var:pat = $obj:expr) => {
        #[allow(irrefutable_let_patterns)]
        let $var = $obj
        else {
            return Err($crate::LuzRuntimeError::message("Invalid cast"));
        };
    };

    ($var:pat =? $obj:expr) => {
        #[allow(irrefutable_let_patterns)]
        let $var = $obj.ok_or_else(|| $crate::LuzRuntimeError::message("Invalid cast"))?
        else {
            return Err($crate::LuzRuntimeError::message("Invalid cast"));
        };
    };

    ($var:pat = $obj:expr; else $err_msg:expr) => {
        #[allow(irrefutable_let_patterns)]
        let $var = $obj
        else {
            return Err($crate::luz::err::LuzRuntimeError::message($err_msg));
        };
    };

    ($var:pat =? $obj:expr; else $err_msg:expr) => {
        #[allow(irrefutable_let_patterns)]
        let $var = $obj.ok_or_else(|| $crate::LuzRuntimeError::message($err_msg))?
        else {
            return Err($crate::LuzRuntimeError::message($err_msg));
        };
    };
}

#[derive(Debug, Clone, new)]
pub struct LuzNativeLib {
    pub exports: Vec<(String, LuzObj)>,
}
