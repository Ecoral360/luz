use core::fmt;
use std::cell::RefCell;
use std::char;
use std::collections::VecDeque;
use std::fmt::Display;
use std::hash::Hash;
use std::mem::transmute;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;

use derive_more::derive::From;

pub use super::function::*;
pub use super::numeral::*;
pub use super::table::*;

use super::err::LuzError;
use crate::ast::{Binop, CmpOp, Unop};
use crate::compiler::opcode::TMcode;
use crate::luz::thread::LuzThread;
use crate::luz::userdata::Userdata;
use crate::runner::err::LuzRuntimeError;
use crate::runner::Runner;

pub type TableRef = Rc<RefCell<Table>>;

#[derive(Debug, From)]
pub enum LuzObj {
    Numeral(Numeral),
    Boolean(bool),
    String(Vec<u8>),
    Function(Rc<RefCell<LuzFunction>>),
    Table(TableRef),
    Thread(Arc<Mutex<LuzThread>>),
    Userdata(Arc<Mutex<Userdata>>),
    Nil,
}

pub trait AsUTF8Unchecked {
    fn as_utf8_string_unchecked(&self) -> String;
}

impl AsUTF8Unchecked for Vec<u8> {
    fn as_utf8_string_unchecked(&self) -> String {
        unsafe { String::from_utf8_unchecked(self.to_vec()) }
    }
}

impl AsUTF8Unchecked for &[u8] {
    fn as_utf8_string_unchecked(&self) -> String {
        unsafe { String::from_utf8_unchecked(self.to_vec()) }
    }
}

impl Clone for LuzObj {
    fn clone(&self) -> Self {
        match self {
            Self::Numeral(arg0) => Self::Numeral(arg0.clone()),
            Self::Boolean(arg0) => Self::Boolean(arg0.clone()),
            Self::String(arg0) => Self::String(arg0.clone()),
            Self::Function(arg0) => Self::Function(Rc::clone(arg0)),
            Self::Table(arg0) => Self::Table(Rc::clone(arg0)),
            Self::Thread(arg0) => Self::Thread(Arc::clone(arg0)),
            Self::Userdata(arg0) => Self::Userdata(arg0.clone()),
            Self::Nil => Self::Nil,
        }
    }
}

impl Display for LuzObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LuzObj::Numeral(numeral) => write!(f, "{numeral}"),
            LuzObj::Boolean(b) => write!(f, "{b}"),
            LuzObj::String(s) => {
                write!(f, "{}", unsafe { String::from_utf8_unchecked(s.to_vec()) })
            }
            LuzObj::Function(ref_cell) => write!(f, "{:?}", ref_cell.borrow()),
            LuzObj::Table(ref_cell) => write!(f, "{:#?}", ref_cell.borrow()),
            LuzObj::Thread(mutex) => todo!(),
            LuzObj::Userdata(mutex) => todo!(),
            LuzObj::Nil => write!(f, "nil"),
        }
    }
}

impl LuzObj {
    pub fn call_metamethod(
        runner: &mut Runner,
        tm: TMcode,
        lhs: LuzObj,
        rhs: LuzObj,
    ) -> Result<LuzObj, LuzRuntimeError> {
        let mut meta = lhs.get_metamethod(runner, tm)?;
        if meta.is_none() {
            meta = rhs.get_metamethod(runner, tm)?;
        }

        let Some((LuzObj::Function(f), mut prefix_args)) = meta else {
            return Err(LuzRuntimeError::message(format!(
                "no metamethod '{}' for obj {} and {}",
                tm,
                lhs.repr(),
                rhs.repr()
            )));
        };

        prefix_args.push(lhs.clone());
        prefix_args.push(rhs.clone());
        let result = f.borrow().call(runner, prefix_args, vec![])?;

        Ok(result.get(0).unwrap_or(&LuzObj::Nil).clone())
    }

    pub fn get_metamethod(
        &self,
        runner: &mut Runner,
        tm: TMcode,
    ) -> Result<Option<(LuzObj, Vec<LuzObj>)>, LuzRuntimeError> {
        match self {
            LuzObj::Table(t) => {
                let t = t.borrow();
                let method = t.rawget_metatable(&LuzObj::str(tm));
                if method.is_nil() {
                    Ok(None)
                } else {
                    Ok(method.callable())
                }
            }
            LuzObj::String(..) => {
                let registry = runner.registry();
                let registry = registry.borrow();
                let meta = registry
                    .rawget(&LuzObj::str(":hidden.string.metatable:"))
                    .as_table_or_err()?;
                let meta = meta.borrow();
                let method = meta.get(runner, &LuzObj::str(tm))?;
                Ok(method.map(|met| met.callable()).flatten())
            }
            _ => Err(LuzRuntimeError::message(format!(
                "no metamethod '{}' for obj {}",
                tm, self
            ))),
        }
    }

    pub fn index(
        &self,
        runner: &mut Runner,
        key: &LuzObj,
    ) -> Result<Option<LuzObj>, LuzRuntimeError> {
        match self {
            LuzObj::Table(t) => {
                let t = t.borrow();
                t.get(runner, key)
            }
            LuzObj::String(..) => {
                let registry = runner.registry();
                let registry = registry.borrow();
                let meta = registry
                    .rawget(&LuzObj::str(":hidden.string.metatable:"))
                    .as_table_or_err()?;
                let meta = meta.borrow();
                meta.get(runner, key)
            }
            _ => Err(LuzRuntimeError::message(format!(
                "obj {} is not indexable",
                self
            ))),
        }
    }

    pub fn or(self, default: LuzObj) -> Self {
        if self.is_nil() {
            default
        } else {
            self
        }
    }

    pub fn str<T: ToString>(string: T) -> Self {
        Self::String(string.to_string().into_bytes())
    }

    pub fn int(i: i64) -> Self {
        Self::Numeral(Numeral::Int(i))
    }

    pub fn float(f: f64) -> Self {
        Self::Numeral(Numeral::Float(f))
    }

    pub fn from_literal_str(s: &str) -> Result<Self, LuzError> {
        if s.starts_with("[") {
            let level = s.chars().skip(1).take_while(|c| *c == '=').count() + 2;
            let string = if s.chars().next().unwrap() == '\n' {
                &s[level + 1..s.len() - level]
            } else {
                &s[level..s.len() - level]
            };
            return Ok(LuzObj::str(string));
        }

        let s = &s[1..s.len() - 1];

        let mut string = String::with_capacity(s.len());
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '\\' => {
                    let Some(escape) = chars.next() else {
                        return Err(LuzError::InvalidStringEscape {
                            escape: '$',
                            str: s.to_string(),
                        });
                    };
                    match escape {
                        'a' => string.push('\x07'),
                        'b' => string.push('\x08'),
                        'f' => string.push('\x0c'),
                        'n' => string.push('\x0a'),
                        'r' => string.push('\x0d'),
                        't' => string.push('\x09'),
                        'v' => string.push('\x0b'),
                        '\'' => string.push('\''),
                        '"' => string.push('"'),
                        '\n' => string.push('\n'),
                        '\\' => string.push('\\'),
                        'z' => {
                            while let Some(nc) = chars.peek() {
                                if !nc.is_ascii_whitespace() {
                                    break;
                                }
                                _ = chars.next();
                            }
                        }
                        'x' => {
                            let code_str = format!(
                                "{}{}",
                                chars.next().ok_or(LuzError::InvalidStringEscape {
                                    escape: 'x',
                                    str: s.to_string(),
                                })?,
                                chars.next().ok_or(LuzError::InvalidStringEscape {
                                    escape: 'x',
                                    str: s.to_string(),
                                })?
                            );
                            let code = u8::from_str_radix(&code_str, 16).map_err(|_| {
                                LuzError::InvalidStringEscape {
                                    escape: 'x',
                                    str: s.to_string(),
                                }
                            })?;
                            string.push(code as char);
                        }
                        d if d.is_ascii_digit() => {
                            let mut d_str = format!("{}", d);
                            for _ in 0..2 {
                                if chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                                    d_str.push(chars.next().unwrap());
                                } else {
                                    break;
                                }
                            }

                            let code =
                                d_str
                                    .parse::<u8>()
                                    .map_err(|_| LuzError::InvalidStringEscape {
                                        escape: 'd',
                                        str: s.to_string(),
                                    })?;
                            string.push(code as char);
                        }
                        'u' => {
                            // Opening {
                            let _ = chars.next().ok_or(LuzError::InvalidStringEscape {
                                escape: 'u',
                                str: s.to_string(),
                            })?;
                            let mut code_str = String::new();
                            loop {
                                let digit = chars.next().ok_or(LuzError::InvalidStringEscape {
                                    escape: 'u',
                                    str: s.to_string(),
                                })?;
                                if digit == '}' {
                                    break;
                                }
                                code_str.push(digit);
                            }
                            let code = u32::from_str_radix(&code_str, 16).map_err(|_| {
                                LuzError::InvalidStringEscape {
                                    escape: 'd',
                                    str: s.to_string(),
                                }
                            })?;

                            // FIXME: check if there are any problems doing it this way
                            // I didn't have much of a choice, because rust would only
                            // accept valid UTF8 code points, which is not the case
                            // for lua
                            unsafe {
                                string.push(transmute(code));
                            }
                        }
                        esc => Err(LuzError::InvalidStringEscape {
                            escape: esc,
                            str: s.to_string(),
                        })?,
                    }
                }
                _ => string.push(c),
            }
        }
        string.shrink_to_fit();
        // let s = s.replace("\\\\", to).replace("\\a", "\x07").replace("\\b", "\x08").replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\v", "\x0b").replace("", to);

        Ok(Self::str(string))
    }

    pub fn as_table_or_err(&self) -> Result<Rc<RefCell<Table>>, LuzRuntimeError> {
        match self {
            LuzObj::Table(ref_cell) => Ok(Rc::clone(ref_cell)),
            _ => Err(LuzRuntimeError::message(format!(
                "expected 'table', found {}",
                self.get_type()
            ))),
        }
    }

    /// Returns `true` if the luz obj is [`Nil`].
    ///
    /// [`Nil`]: LuzObj::Nil
    #[must_use]
    #[inline]
    pub const fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    #[must_use]
    #[inline]
    pub fn type_is(&self, luz_type: LuzType) -> bool {
        self.get_type() == luz_type
    }

    #[must_use]
    #[inline]
    pub const fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }

    #[must_use]
    #[inline]
    pub const fn is_true(&self) -> bool {
        matches!(self, Self::Boolean(true))
    }

    #[must_use]
    #[inline]
    pub const fn is_false(&self) -> bool {
        matches!(self, Self::Boolean(false))
    }

    #[inline]
    pub const fn get_type(&self) -> LuzType {
        match self {
            LuzObj::Numeral(_) => LuzType::Number,
            LuzObj::Boolean(_) => LuzType::Boolean,
            LuzObj::String(_) => LuzType::String,
            LuzObj::Table(_) => LuzType::Table,
            LuzObj::Nil => LuzType::Nil,
            LuzObj::Userdata(_) => LuzType::Userdata,
            LuzObj::Thread(_) => LuzType::Thread,
            LuzObj::Function(_) => LuzType::Function,
        }
    }

    pub fn coerse(self, to: LuzType) -> Result<Self, LuzError> {
        Ok(match (self, to) {
            (this, LuzType::Boolean) => {
                (this != LuzObj::Nil && this != LuzObj::Boolean(false)).into()
            }

            (this @ LuzObj::Table(_), LuzType::Table) => this,
            (this @ LuzObj::Table(_), to) => Err(LuzError::InvalidCoersion { obj: this, ty: to })?,

            (LuzObj::Numeral(n), LuzType::Integer) => n.to_lossy_int().into(),
            (LuzObj::Numeral(n), LuzType::Float) => n.to_float().into(),
            (this @ LuzObj::Numeral(_), LuzType::Number) => this,
            (LuzObj::Numeral(n), LuzType::String) => n.to_string().into_bytes().into(),
            (this @ LuzObj::Numeral(_), to) => {
                Err(LuzError::InvalidCoersion { obj: this, ty: to })?
            }

            (LuzObj::Boolean(b), LuzType::Integer) => Numeral::Int(if b { 1 } else { 0 }).into(),
            (LuzObj::Boolean(b), LuzType::Float) => {
                Numeral::Float(if b { 1.0 } else { 0.0 }).into()
            }
            (LuzObj::Boolean(b), LuzType::Number) => Numeral::Int(if b { 1 } else { 0 }).into(),
            (LuzObj::Boolean(b), LuzType::String) => if b { "true" } else { "false" }.into(),
            (this @ LuzObj::Boolean(_), to) => {
                Err(LuzError::InvalidCoersion { obj: this, ty: to })?
            }

            (LuzObj::String(s), LuzType::Integer) => s
                .as_utf8_string_unchecked()
                .parse::<Numeral>()?
                .to_lossy_int()
                .into(),
            (LuzObj::String(s), LuzType::Float) => s
                .as_utf8_string_unchecked()
                .parse::<Numeral>()?
                .to_float()
                .into(),
            (LuzObj::String(s), LuzType::Number) => {
                s.as_utf8_string_unchecked().parse::<Numeral>()?.into()
            }
            (this @ LuzObj::String(_), LuzType::String) => this,
            (this @ LuzObj::String(_), to) => Err(LuzError::InvalidCoersion { obj: this, ty: to })?,

            (LuzObj::Nil, LuzType::Nil) => LuzObj::Nil,
            (this @ LuzObj::Nil, to) => Err(LuzError::InvalidCoersion { obj: this, ty: to })?,

            (this, to) => Err(LuzError::InvalidCoersion { obj: this, ty: to })?,
        })
    }

    pub fn apply_unop(self, unop: Unop) -> Result<Self, LuzError> {
        Ok(match unop {
            Unop::Neg => {
                let LuzObj::Numeral(n) = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'neg': {}", err)))?
                else {
                    unreachable!()
                };
                (-n).into()
            }
            Unop::Not => {
                let LuzObj::Boolean(b) = self
                    .coerse(LuzType::Boolean)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'not': {}", err)))?
                else {
                    unreachable!()
                };
                (!b).into()
            }
            Unop::Len => match self {
                LuzObj::String(s) => Numeral::Int(s.len() as i64).into(),
                LuzObj::Table(t) => Numeral::Int(t.borrow().len() as i64).into(),
                LuzObj::Boolean(_) => Err(LuzError::Type {
                    wrong: LuzType::Boolean,
                    expected: vec![LuzType::String, LuzType::Table],
                })?,
                LuzObj::Numeral(_) => Err(LuzError::Type {
                    wrong: LuzType::Number,
                    expected: vec![LuzType::String, LuzType::Table],
                })?,
                LuzObj::Nil => Err(LuzError::Type {
                    wrong: LuzType::Nil,
                    expected: vec![LuzType::String, LuzType::Table],
                })?,
                _ => Err(LuzError::Type {
                    wrong: self.get_type(),
                    expected: vec![],
                })?,
            },
            Unop::Inv => {
                let self_type = self.get_type();
                let LuzObj::Numeral(Numeral::Int(i)) = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'bnot': {}", err)))?
                else {
                    Err(LuzError::Type {
                        wrong: self_type,
                        expected: vec![],
                    })?
                };

                Numeral::Int(!i).into()
            }
        })
    }

    pub fn apply_binop(self, binop: Binop, rhs: LuzObj) -> Result<Self, LuzError> {
        Ok(match binop {
            Binop::Concat => {
                let lhs = self
                    .coerse(LuzType::String)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'concat': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::String)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'concat': {}", err)))?;

                let (LuzObj::String(s1), LuzObj::String(s2)) = (lhs, rhs) else {
                    unreachable!()
                };

                let mut new_s = s1.to_vec();
                new_s.extend(&s2);

                new_s.into()
            }
            Binop::Add => {
                let lhs = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'add': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'add': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 + n2).into()
            }
            Binop::Sub => {
                let lhs = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'sub': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'sub': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 - n2).into()
            }
            Binop::Mul => {
                let lhs = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'mul': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'mul': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 * n2).into()
            }
            Binop::FloatDiv => {
                let lhs = self
                    .coerse(LuzType::Float)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'div': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Float)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'div': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 / n2).into()
            }
            Binop::FloorDiv => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'idiv': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'idiv': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                n1.floor_div(n2)?.into()
            }
            Binop::Mod => {
                let lhs = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'mod': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'mod': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 % n2)?.into()
            }
            Binop::Exp => {
                let lhs = self
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'exp': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Number)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'exp': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                n1.pow(n2).into()
            }
            Binop::BitAnd => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'band': {}", err)))?;
                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'band': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 & n2).into()
            }
            Binop::BitOr => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'bor': {}", err)))?;
                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'bor': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 | n2).into()
            }
            Binop::BitXor => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'bxor': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'bxor': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 ^ n2).into()
            }
            Binop::ShiftRight => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'shr': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'shr': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 >> n2).into()
            }
            Binop::ShiftLeft => {
                let lhs = self
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'shl': {}", err)))?;

                let rhs = rhs
                    .coerse(LuzType::Integer)
                    .map_err(|err| LuzRuntimeError::message(format!("In 'shl': {}", err)))?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 << n2).into()
            }
        })
    }

    pub fn apply_cmp(&self, cmpop: CmpOp, rhs: &LuzObj) -> Result<LuzObj, LuzError> {
        Ok(match cmpop {
            CmpOp::Eq => self == rhs,
            CmpOp::Neq => self != rhs,
            CmpOp::Lt => self < rhs,
            CmpOp::Gt => self > rhs,
            CmpOp::LtEq => self <= rhs,
            CmpOp::GtEq => self >= rhs,
        }
        .into())
    }

    pub fn repr(&self) -> String {
        match self {
            Self::String(s) => format!("{:?}", s.as_utf8_string_unchecked()),
            _ => self.to_string(),
        }
    }

    pub fn callable(&self) -> Option<(LuzObj, Vec<LuzObj>)> {
        let mut curr = self.clone();
        let mut args = vec![];
        loop {
            curr = match curr {
                LuzObj::Function(..) => break,
                LuzObj::Table(tab) => {
                    let t = tab.borrow();
                    let metavalue = t.rawget_metatable(&LuzObj::str("__call"));
                    args.push(LuzObj::Table(Rc::clone(&tab)));
                    metavalue
                }
                _ => return None,
            }
        }

        args.reverse();
        Some((curr, args))
    }

    pub fn call(
        &self,
        runner: &mut Runner,
        mut args: Vec<LuzObj>,
        vararg: Vec<LuzObj>,
    ) -> Result<Vec<LuzObj>, LuzRuntimeError> {
        let call_args = self.callable();
        match call_args {
            Some((LuzObj::Function(f), mut prefix_args)) => {
                if !prefix_args.is_empty() {
                    prefix_args.append(&mut args);
                    args = prefix_args;
                }
                f.borrow().call(runner, args, vararg)
            }
            _ => Err(LuzRuntimeError::message("not callable")),
        }
    }
}

impl PartialEq for LuzObj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Numeral(l0), Self::Numeral(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => Table::table_eq(Rc::clone(l0), Rc::clone(r0)),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
impl Eq for LuzObj {}

impl PartialOrd for LuzObj {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) => n1.partial_cmp(n2),
            (LuzObj::Numeral(_), _) => None,
            (LuzObj::Boolean(b1), LuzObj::Boolean(b2)) => b1.partial_cmp(b2),
            (LuzObj::Boolean(_), _) => None,
            (LuzObj::String(s1), LuzObj::String(s2)) => s1.partial_cmp(s2),
            (LuzObj::String(_), _) => None,
            (LuzObj::Table(_), LuzObj::Table(_)) => None,
            (LuzObj::Table(_), _) => None,
            (LuzObj::Nil, LuzObj::Nil) => Some(std::cmp::Ordering::Equal),
            (LuzObj::Nil, _) => None,
            (_, _) => None,
        }
    }
}
impl Hash for LuzObj {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LuzType {
    Nil,
    Boolean,
    Integer,
    Float,
    Number,
    String,
    Function,
    Userdata,
    Thread,
    Table,
}

impl Display for LuzType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LuzType::Nil => write!(f, "nil"),
            LuzType::Boolean => write!(f, "boolean"),
            LuzType::Integer => write!(f, "number"),
            LuzType::Float => write!(f, "number"),
            LuzType::Number => write!(f, "number"),
            LuzType::String => write!(f, "string"),
            LuzType::Function => write!(f, "function"),
            LuzType::Userdata => write!(f, "userdata"),
            LuzType::Thread => write!(f, "thread"),
            LuzType::Table => write!(f, "table"),
        }
    }
}
