use derive_new::new;

use crate::luz::obj::LuzObj;

pub mod env;
mod math;
mod require;
mod string;
mod table;

#[macro_export]
macro_rules! luz_fn {
    ([$nb_fixed:literal, $runner:ident]($($arg:pat,)* *$args:ident) $body:block ) => {
        LuzObj::Function(std::rc::Rc::new(std::cell::RefCell::new(
            $crate::luz::obj::LuzFunction::new_native(
                $nb_fixed,
                std::rc::Rc::new(std::cell::RefCell::new(
                    |$runner: &mut $crate::runner::Runner, $args: Vec<$crate::luz::obj::LuzObj>| {
                        #[allow(unused_mut)]
                        let mut $args = std::collections::VecDeque::from_iter($args.into_iter());
                        let mut _i = 0;
                        $(
                            let val = $args.pop_front().unwrap_or($crate::luz::obj::LuzObj::Nil);
                            #[allow(irrefutable_let_patterns)]
                            let $arg = val
                            else {
                                return Err($crate::LuzRuntimeError::message(format!("bad argument #{} (expected {}, got {})", _i + 1, stringify!($arg), val)));
                            };
                            _i+=1;
                        )*
                        $body
                    },
                )),
            ),
        )))
    };

    ([$nb_fixed:literal, $runner:ident]($($arg:pat),* $(,)?) $body:block ) => {
        LuzObj::Function(std::rc::Rc::new(std::cell::RefCell::new(
            $crate::luz::obj::LuzFunction::new_native(
                $nb_fixed,
                std::rc::Rc::new(std::cell::RefCell::new(
                    |$runner: &mut $crate::runner::Runner, args: Vec<$crate::luz::obj::LuzObj>| {
                        #[allow(unused_mut)]
                        let mut _args = std::collections::VecDeque::from_iter(args.into_iter());
                        let mut _i = 0;
                        $(
                            let val = _args.pop_front().unwrap_or($crate::luz::obj::LuzObj::Nil);
                            #[allow(irrefutable_let_patterns)]
                            let $arg = val
                            else {
                                return Err($crate::LuzRuntimeError::message(format!("bad argument #{} (expected {}, got {})", _i + 1, stringify!($arg), val)));
                            };
                            _i+=1;
                        )*
                        $body
                    },
                )),
            ),
        )))
    };

    ([$nb_fixed:literal]($($arg:pat,)* *$args:ident) $body:block ) => {
        LuzObj::Function(std::rc::Rc::new(std::cell::RefCell::new(
            $crate::luz::obj::LuzFunction::new_native(
                $nb_fixed,
                std::rc::Rc::new(std::cell::RefCell::new(
                    |_: &mut $crate::runner::Runner, $args: Vec<$crate::luz::obj::LuzObj>| {
                        #[allow(unused_mut)]
                        let mut $args = std::collections::VecDeque::from_iter($args.into_iter());
                        let mut _i = 0;
                        $(
                            let val = $args.pop_front().unwrap_or($crate::luz::obj::LuzObj::Nil);
                            #[allow(irrefutable_let_patterns)]
                            let $arg = val
                            else {
                                return Err($crate::LuzRuntimeError::message(format!("bad argument #{} (expected {}, got {})", _i + 1, stringify!($arg), val)));
                            };
                            _i+=1;
                        )*
                        $body
                    },
                )),
            ),
        )))
    };

    ([$nb_fixed:literal]($($arg:pat),* $(,)?) $body:block ) => {
        LuzObj::Function(std::rc::Rc::new(std::cell::RefCell::new(
            $crate::luz::obj::LuzFunction::new_native(
                $nb_fixed,
                std::rc::Rc::new(std::cell::RefCell::new(
                    |_: &mut $crate::runner::Runner, _args: Vec<$crate::luz::obj::LuzObj>| {
                        #[allow(unused_mut)]
                        let mut _args = std::collections::VecDeque::from_iter(_args.into_iter());
                        let mut _i = 0;
                        $(
                            let val = _args.pop_front().unwrap_or($crate::luz::obj::LuzObj::Nil);
                            #[allow(irrefutable_let_patterns)]
                            let $arg = val
                            else {
                                return Err($crate::LuzRuntimeError::message(format!("bad argument #{} (expected {}, got {})", _i + 1, stringify!($arg), val)));
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

        $crate::luz::obj::LuzObj::Table(std::rc::Rc::new(std::cell::RefCell::new($crate::luz::obj::Table::new(table, None))))
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
            Err($crate::LuzRuntimeError::message("Invalid cast"))?
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
