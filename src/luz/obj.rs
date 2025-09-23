use core::fmt;
use std::cell::RefCell;
use std::char;
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
use crate::luz::numeral;
use crate::luz::thread::LuzThread;
use crate::luz::userdata::Userdata;
use crate::runner::err::LuzRuntimeError;

pub type TableRef = Rc<RefCell<Table>>;

#[derive(Debug, From)]
pub enum LuzObj {
    Numeral(Numeral),
    Boolean(bool),
    String(String),
    Function(Rc<RefCell<LuzFunction>>),
    Table(TableRef),
    Thread(Arc<Mutex<LuzThread>>),
    Userdata(Arc<Mutex<Userdata>>),
    Nil,
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
            LuzObj::String(s) => write!(f, "{s}"),
            LuzObj::Function(ref_cell) => write!(f, "{:?}", ref_cell.borrow()),
            LuzObj::Table(ref_cell) => write!(f, "{:#?}", ref_cell.borrow()),
            LuzObj::Thread(mutex) => todo!(),
            LuzObj::Userdata(mutex) => todo!(),
            LuzObj::Nil => write!(f, "nil"),
        }
    }
}

impl LuzObj {
    pub fn str(string: &str) -> Self {
        Self::String(string.to_owned())
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
            return Ok(LuzObj::String(string.to_string()));
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

        Ok(Self::String(string))
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

    pub fn not(&self) -> Self {
        Self::Boolean(!self.is_truthy())
    }

    pub fn len(&self) -> LuzObj {
        match self {
            LuzObj::Numeral(numeral) => todo!(),
            LuzObj::Boolean(_) => todo!(),
            LuzObj::String(s) => LuzObj::int(s.len() as i64),
            LuzObj::Function(ref_cell) => todo!(),
            LuzObj::Table(ref_cell) => todo!(),
            LuzObj::Thread(mutex) => todo!(),
            LuzObj::Userdata(mutex) => todo!(),
            LuzObj::Nil => todo!(),
        }
    }

    pub fn bnot(&self) -> LuzObj {
        match self {
            LuzObj::Numeral(numeral) => (!*numeral).into(),
            LuzObj::Boolean(_) => todo!(),
            LuzObj::String(s) => LuzObj::int(s.len() as i64),
            LuzObj::Function(ref_cell) => todo!(),
            LuzObj::Table(ref_cell) => todo!(),
            LuzObj::Thread(mutex) => todo!(),
            LuzObj::Userdata(mutex) => todo!(),
            LuzObj::Nil => todo!(),
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
            (LuzObj::Numeral(n), LuzType::String) => n.to_string().into(),
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

            (LuzObj::String(s), LuzType::Integer) => s.parse::<Numeral>()?.to_lossy_int().into(),
            (LuzObj::String(s), LuzType::Float) => s.parse::<Numeral>()?.to_float().into(),
            (LuzObj::String(s), LuzType::Number) => s.parse::<Numeral>()?.into(),
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
                let LuzObj::Numeral(n) = self.coerse(LuzType::Number)? else {
                    unreachable!()
                };
                (-n).into()
            }
            Unop::Not => {
                let LuzObj::Boolean(b) = self.coerse(LuzType::Boolean)? else {
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
                let LuzObj::Numeral(Numeral::Int(i)) = self.coerse(LuzType::Integer)? else {
                    unreachable!()
                };

                Numeral::Int(!i).into()
            }
        })
    }

    pub fn apply_binop(self, binop: Binop, rhs: LuzObj) -> Result<Self, LuzError> {
        Ok(match binop {
            Binop::Concat => {
                let lhs = self.coerse(LuzType::String)?;
                let rhs = rhs.coerse(LuzType::String)?;

                let (LuzObj::String(s1), LuzObj::String(s2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (s1 + &s2).into()
            }
            Binop::Add => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 + n2).into()
            }
            Binop::Sub => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 - n2).into()
            }
            Binop::Mul => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 * n2).into()
            }
            Binop::FloatDiv => {
                let lhs = self.coerse(LuzType::Float)?;
                let rhs = rhs.coerse(LuzType::Float)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 / n2).into()
            }
            Binop::FloorDiv => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                n1.floor_div(n2)?.into()
            }
            Binop::Mod => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 % n2)?.into()
            }
            Binop::Exp => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                n1.pow(n2).into()
            }
            Binop::BitAnd => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 & n2).into()
            }
            Binop::BitOr => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 | n2).into()
            }
            Binop::BitXor => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 ^ n2).into()
            }
            Binop::ShiftRight => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 >> n2).into()
            }
            Binop::ShiftLeft => {
                let lhs = self.coerse(LuzType::Integer)?;
                let rhs = rhs.coerse(LuzType::Integer)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 << n2).into()
            }
        })
    }

    // pub fn apply_logic_cmp(self, logic_cmp_op: LogicCmpOp, rhs: LuzObj) -> Result<Self, LuzError> {
    //     Ok(match logic_cmp_op)
    // }

    pub fn apply_cmp(self, cmpop: CmpOp, rhs: LuzObj) -> Result<Self, LuzError> {
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
            Self::String(s) => format!("{:?}", s),
            _ => self.to_string(),
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
