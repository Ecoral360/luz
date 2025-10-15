use std::collections::VecDeque;

use crate::{
    luz::{
        fn_dump,
        lib::LuzNativeLib,
        obj::{AsUTF8Unchecked, LuzObj, Numeral, TableRef},
    },
    luz_fn, luz_let, luz_table,
    runner::err::LuzRuntimeError,
};

pub fn string_lib(registry: TableRef) -> LuzNativeLib {
    let table = luz_table! {
        len: luz_fn!([1](*args) {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.len' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::Numeral(Numeral::Int(s.len() as i64))])
        }),
        lower: luz_fn!([1](*args) {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.lower' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::str(s.as_utf8_string_unchecked().to_lowercase())])
        }),
        upper: luz_fn!([1](*args) {
            let mut args = VecDeque::from(args);
            let Some(LuzObj::String(s)) = args.pop_front() else {
                return Err(LuzRuntimeError::message(
                    "bad argument #1 to 'string.upper' (string value expected)",
                ));
            };
            Ok(vec![LuzObj::str(s.as_utf8_string_unchecked().to_uppercase())])
        }),
        find: luz_fn!([2](LuzObj::String(s),
                LuzObj::String(pattern),
                init @ (LuzObj::Numeral(..) | LuzObj::Nil),
                plain @ (LuzObj::Boolean(..) | LuzObj::Nil))
        {
            let LuzObj::Numeral(init) = init.or(LuzObj::int(1)) else { unreachable!() };

            let mut init = init.as_int();

            if init == 0 {
                init = 1;
            }
            if init - 1 < 0 {
                init = (s.len() - init as usize) as i64;
            }
            let Some((start, end)) = pattern_match(
                &pattern.as_utf8_string_unchecked(),
                &(&s[init as usize - 1..]).as_utf8_string_unchecked()
            ) else {
                return Ok(vec![LuzObj::Nil]);
            };
            Ok(vec![LuzObj::int(start as i64), LuzObj::int(end as i64)])
        }),
        sub: luz_fn!([3](LuzObj::String(s), LuzObj::Numeral(i), j @ (LuzObj::Numeral(_) | LuzObj::Nil)) {
            luz_let!(LuzObj::Numeral(j) = j.or(LuzObj::int(-1)));

            let j = j.as_int();
            let mut j = if j < 0 { s.len() as i64 - (j + 1) } else { j };
            if j > s.len() as i64 {
                j = s.len() as i64;
            }

            let i = i.as_int();
            let mut i = if i < 0 { s.len() as i64 - i } else { i };
            if i < 1 {
                i = 1;
            }

            if i > j {
                Ok(vec![LuzObj::str(String::new())])
            } else {
                Ok(vec![LuzObj::String(s[(i as usize - 1)..j as usize].to_vec())])
            }
        }),
        format: luz_fn!([1](LuzObj::String(formatstring), *args) {
            Ok(vec![LuzObj::str(format(&formatstring, args))])
        }),
        pack: luz_fn!([1](LuzObj::String(formatstring), *args) {
            Ok(vec![LuzObj::str(format(&formatstring, args))])
        }),
        packsize: luz_fn!([1](LuzObj::String(formatstring)) {
            Ok(vec![LuzObj::int(packsize_string(&formatstring).unwrap() as i64)])
        }),


        dump: luz_fn!([1](LuzObj::Function(f)) {
            let f = f.borrow();
            Ok(vec![LuzObj::String(fn_dump::dump(&f)?)])
        }),
    };

    registry
        .borrow_mut()
        .rawset(LuzObj::str(":hidden.string.metatable:"), table.clone());

    LuzNativeLib {
        exports: vec![(String::from("string"), table)],
    }
}

fn pattern_match(pattern: &str, s: &str) -> Option<(usize, usize)> {
    let start = s.find(pattern);
    start.map(|start| (start, pattern.len()))
}

/// Implements the format function from lua's string library
fn format(formatstring: &[u8], mut args: VecDeque<LuzObj>) -> String {
    let mut final_string = String::new();
    let mut chars = formatstring.iter().map(|c| *c as char).peekable();
    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(&next_c) = chars.peek() {
                match next_c {
                    's' => {
                        chars.next();
                        if let Some(arg) = args.pop_front() {
                            final_string.push_str(&arg.to_string());
                        } else {
                            final_string.push_str("%s");
                        }
                    }
                    'd' => {
                        chars.next();
                        if let Some(arg) = args.pop_front() {
                            match arg {
                                LuzObj::Numeral(Numeral::Int(i)) => {
                                    final_string.push_str(&i.to_string());
                                }
                                LuzObj::Numeral(Numeral::Float(f)) => {
                                    final_string.push_str(&f.to_string());
                                }
                                _ => {
                                    final_string.push_str("%d");
                                }
                            }
                        } else {
                            final_string.push_str("%d");
                        }
                    }
                    '%' => {
                        chars.next();
                        final_string.push('%');
                    }
                    _ => {
                        final_string.push(c);
                    }
                }
            } else {
                final_string.push(c);
            }
        } else {
            final_string.push(c);
        }
    }

    final_string
}

/// Implements the pack function from lua's string library
fn pack_string(fmt: &str, values: VecDeque<LuzObj>) -> Vec<u8> {
    let mut result = vec![];
    let mut chars = fmt.chars().peekable();
    let mut vals = values.into_iter();

    let mut little_endian = true;

    while let Some(c) = chars.next() {
        match c {
            '<' => {
                // Little-endian
                little_endian = true;
                continue;
            }
            '>' => {
                // Big-endian
                little_endian = false;
                continue;
            }
            'b' => {
                // 1-byte signed int
                if let Some(LuzObj::Numeral(Numeral::Int(i))) = vals.next() {
                    if little_endian {
                        for i in i.to_le_bytes().iter() {
                            result.push(*i);
                        }
                    } else {
                        for i in i.to_be_bytes().iter() {
                            result.push(*i);
                        }
                    }
                }
            }
            'B' => {
                // 1-byte unsigned int
                if let Some(LuzObj::Numeral(Numeral::Int(i))) = vals.next() {
                    let i = i as u8;
                    if little_endian {
                        for i in i.to_le_bytes().iter() {
                            result.push(*i);
                        }
                    } else {
                        for i in i.to_be_bytes().iter() {
                            result.push(*i);
                        }
                    }
                }
            }
            'j' => {
                // Lua integer
                if let Some(LuzObj::Numeral(Numeral::Int(i))) = vals.next() {
                    if little_endian {
                        for i in i.to_le_bytes().iter() {
                            result.push(*i);
                        }
                    } else {
                        for i in i.to_be_bytes().iter() {
                            result.push(*i);
                        }
                    }
                }
            }
            'J' => {
                // Lua unsigne integer (8 bytes)
                if let Some(LuzObj::Numeral(Numeral::Int(i))) = vals.next() {
                    let i = i as u64;
                    if little_endian {
                        for i in i.to_le_bytes().iter() {
                            result.push(*i);
                        }
                    } else {
                        for i in i.to_be_bytes().iter() {
                            result.push(*i);
                        }
                    }
                }
            }
            'i' => {
                // 4-byte integer
                if let Some(LuzObj::Numeral(Numeral::Int(i))) = vals.next() {}
            }
            _ => {
                // Ignore unknown format characters
                continue;
            }
        }
    }

    result
}

fn packsize_string(fmt: &[u8]) -> Option<usize> {
    let mut size = 0;
    let mut chars = fmt.iter().map(|c| *c as char).peekable();

    while let Some(c) = chars.next() {
        match c {
            'b' | 'B' => {
                size += 1;
            }
            'j' | 'J' => {
                size += 8;
            }
            _ => {
                // Ignore unknown format characters
                continue;
            }
        }
    }

    Some(size)
}
