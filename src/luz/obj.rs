use std::hash::Hash;
use std::ptr;
use std::sync::Arc;
use std::sync::Mutex;

use derive_more::derive::From;

pub use super::function::*;
pub use super::numeral::*;
pub use super::table::*;

use super::err::LuzError;
use crate::ast::{Binop, CmpOp, Unop};
use crate::luz::thread::LuzThread;
use crate::luz::userdata::Userdata;

#[derive(Debug, Clone, From)]
pub enum LuzObj {
    Numeral(Numeral),
    Boolean(bool),
    String(String),
    Function(Arc<Mutex<LuzFunction>>),
    Table(Arc<Mutex<Table>>),
    Thread(Arc<Mutex<LuzThread>>),
    Userdata(Arc<Mutex<Userdata>>),
    Nil,
}

impl LuzObj {
    /// Returns `true` if the luz obj is [`Nil`].
    ///
    /// [`Nil`]: LuzObj::Nil
    #[must_use]
    #[inline]
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
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
                LuzObj::Table(t) => {
                    Numeral::Int(t.lock().expect("Locked table for len").len() as i64).into()
                }
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

                (n1 / n2).into()
            }
            Binop::Mod => {
                let lhs = self.coerse(LuzType::Number)?;
                let rhs = rhs.coerse(LuzType::Number)?;

                let (LuzObj::Numeral(n1), LuzObj::Numeral(n2)) = (lhs, rhs) else {
                    unreachable!()
                };

                (n1 % n2).into()
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
}

impl PartialEq for LuzObj {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Numeral(l0), Self::Numeral(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Table(l0), Self::Table(r0)) => Table::table_eq(Arc::clone(l0), Arc::clone(r0)),
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

#[derive(Debug, Clone, Copy)]
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
