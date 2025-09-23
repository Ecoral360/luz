use std::collections::VecDeque;

use crate::{
    ast::CmpOp,
    luz::{
        lib::LuzNativeLib,
        obj::{LuzObj, LuzType, Numeral, TableRef},
    },
    luz_fn, luz_let, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn math_lib(_registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        abs: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Int(i.abs()),
                Numeral::Float(f) => Numeral::Float(f.abs()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        acos: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).acos()),
                Numeral::Float(f) => Numeral::Float(f.acos()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        asin: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).asin()),
                Numeral::Float(f) => Numeral::Float(f.asin()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        atan: luz_fn!([1](LuzObj::Numeral(y), x @ (LuzObj::Numeral(..) | LuzObj::Nil)) {
            luz_let!(LuzObj::Numeral(x) = x.clone().or(LuzObj::float(1.0)));

            let result = match y {
                Numeral::Int(i) => Numeral::Float((i as f64).atan2(x.as_float())),
                Numeral::Float(f) => Numeral::Float(f.atan2(x.as_float())),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        ceil: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Int(i),
                Numeral::Float(f) => Numeral::Int(f.ceil() as i64),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        cos: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).cos()),
                Numeral::Float(f) => Numeral::Float(f.cos()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        deg: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).to_degrees()),
                Numeral::Float(f) => Numeral::Float(f.to_degrees()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        exp: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).exp()),
                Numeral::Float(f) => Numeral::Float(f.exp()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        floor: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Int(i),
                Numeral::Float(f) => Numeral::Int(f.floor() as i64),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        fmod: luz_fn!([2](LuzObj::Numeral(x), LuzObj::Numeral(y)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64) % y.as_float()),
                Numeral::Float(f) => Numeral::Float(f % y.as_float()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        huge: LuzObj::float(f64::MAX),
        log: luz_fn!([1](LuzObj::Numeral(x), base @ (LuzObj::Numeral(..) | LuzObj::Nil)) {
            luz_let!(LuzObj::Numeral(base) = base.clone().or(LuzObj::float(std::f64::consts::E)));

            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).log(base.as_float())),
                Numeral::Float(f) => Numeral::Float(f.log(base.as_float())),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        max: luz_fn!([1](x, *args) {
            let mut max = x;
            for arg in args {
                if max.apply_cmp(CmpOp::Lt, &arg)?.is_true() {
                    max = arg;
                }
            }

            Ok(vec![max])
        }),
        maxinteger: LuzObj::int(i64::MAX),
        min: luz_fn!([1](x, *args) {
            let mut min = x;
            for arg in args {
                if min.apply_cmp(CmpOp::Lt, &arg)?.is_false() {
                    min = arg;
                }
            }

            Ok(vec![min])
        }),
        mininteger: LuzObj::int(i64::MIN),
        modf: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => vec![LuzObj::int(i), LuzObj::float(0.0)],
                Numeral::Float(f) => vec![LuzObj::int(f.trunc() as i64), LuzObj::float(f.fract())]
            };

            Ok(result)
        }),
        pi: LuzObj::float(std::f64::consts::PI),
        rad: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).to_radians()),
                Numeral::Float(f) => Numeral::Float(f.to_radians()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),

        sin: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).sin()),
                Numeral::Float(f) => Numeral::Float(f.sin()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        sqrt: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).sqrt()),
                Numeral::Float(f) => Numeral::Float(f.sqrt()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        tan: luz_fn!([1](LuzObj::Numeral(x)) {
            let result = match x {
                Numeral::Int(i) => Numeral::Float((i as f64).tan()),
                Numeral::Float(f) => Numeral::Float(f.tan()),
            };

            Ok(vec![LuzObj::Numeral(result)])
        }),
        tointeger: luz_fn!([1](x) {
            let result = x.coerse(LuzType::Integer).unwrap_or(LuzObj::str("fail"));

            Ok(vec![result])
        }),
        type: luz_fn!([1](x) {
            let result = match x {
                LuzObj::Numeral(Numeral::Int(..)) => "integer",
                LuzObj::Numeral(Numeral::Float(..)) => "float",
                _ => "fail",
            };

            Ok(vec![LuzObj::str(result)])
        }),
        ult: luz_fn!([2](LuzObj::Numeral(m), LuzObj::Numeral(n)) {
            let m = m.as_int() as u64;
            let n = n.as_int() as u64;

            Ok(vec![LuzObj::Boolean(m < n)])
        }),
    };

    LuzNativeLib {
        exports: vec![(String::from("math"), table)],
    }
}
