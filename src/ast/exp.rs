use derive_more::derive::From;
use derive_new::new;
use pest::{error::Error as PestError, iterators::Pair};

use crate::{
    luz::{
        err::LuzError,
        obj::{FuncParams, LuzObj},
    },
    Rule,
};

use super::Stat;

#[derive(Debug, Clone, From)]
pub enum Exp {
    Literal(LuzObj),
    Ellipsis,
    Name(String),
    Var(Box<Exp>),
    Unop(Unop, Box<Exp>),
    Binop {
        op: Binop,
        lhs: Box<Exp>,
        rhs: Box<Exp>,
    },
    CmpOp {
        op: CmpOp,
        lhs: Box<Exp>,
        rhs: Box<Exp>,
    },
    LogicCmpOp {
        op: LogicCmpOp,
        lhs: Box<Exp>,
        rhs: Box<Exp>,
    },
    Access(ExpAccess),
    FuncDef(FuncDef),
    FuncCall(FuncCall),
    TableConstructor(ExpTableConstructor),
}

#[derive(Debug, Clone, new)]
pub struct ExpAccess {
    exp: Box<Exp>,
    value: Box<Exp>,
}

#[derive(Debug, Clone, new)]
pub struct FuncDef {
    params: FuncParams,
    body: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub struct FuncCall {
    func: Box<Exp>,
    method_name: Option<String>,
    args: Vec<Exp>,
}

#[derive(Debug, Clone, new)]
pub struct ExpTableConstructor {
    arr_fields: Vec<Exp>,
    obj_fields: Vec<ExpTableConstructorField>,
    last_exp: Option<Box<Exp>>,
}

#[derive(Debug, Clone, new)]
pub struct ExpTableConstructorField {
    key: Box<Exp>,
    val: Box<Exp>,
}

impl Exp {
    pub fn do_unop(self, unop: Unop) -> Result<Self, LuzError> {
        Ok(match self {
            Self::Literal(obj) => obj.apply_unop(unop)?.into(),
            _ => Self::Unop(unop, Box::new(self)),
        })
    }

    pub fn do_binop(self, binop: Binop, rhs: Exp) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            (Self::Literal(obj), Self::Literal(obj2)) => obj.apply_binop(binop, obj2)?.into(),
            (s, rhs) => Self::Binop {
                lhs: Box::new(s),
                op: binop,
                rhs: Box::new(rhs),
            },
        })
    }

    pub fn do_cmp(self, cmpop: CmpOp, rhs: Exp) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            (Self::Literal(obj), Self::Literal(obj2)) => obj.apply_cmp(cmpop, obj2)?.into(),
            (s, rhs) => Self::CmpOp {
                lhs: Box::new(s),
                op: cmpop,
                rhs: Box::new(rhs),
            },
        })
    }

    pub fn do_logic_cmp(self, logic_cmp_op: LogicCmpOp, rhs: Exp) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            (s, rhs) => Self::LogicCmpOp {
                lhs: Box::new(s),
                op: logic_cmp_op,
                rhs: Box::new(rhs),
            },
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Concat,

    Add,
    Sub,
    Mul,
    FloatDiv,
    FloorDiv,
    Mod,
    Exp,

    BitAnd,
    BitOr,
    BitXor,
    ShiftRight,
    ShiftLeft,
}

impl TryFrom<Pair<'_, Rule>> for Binop {
    type Error = PestError<Rule>;

    fn try_from(value: Pair<Rule>) -> Result<Self, Self::Error> {
        Ok(match value.as_rule() {
            Rule::DotDot => Self::Concat,
            Rule::Plus => Self::Add,
            Rule::Minus => Self::Sub,
            Rule::Star => Self::Mul,
            Rule::Slash => Self::FloatDiv,
            Rule::DoubleSlash => Self::FloorDiv,
            Rule::Pourcent => Self::Mod,
            Rule::Caret => Self::Exp,
            Rule::Ampersand => Self::BitAnd,
            Rule::Pipe => Self::BitOr,
            Rule::ShiftRight => Self::ShiftRight,
            Rule::ShiftLeft => Self::ShiftLeft,
            Rule::Tilde => Self::BitXor,

            rule => Err(PestError::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![
                        Rule::DotDot,
                        Rule::Plus,
                        Rule::Minus,
                        Rule::Star,
                        Rule::Slash,
                        Rule::DoubleSlash,
                        Rule::Pourcent,
                        Rule::Caret,
                        Rule::Ampersand,
                        Rule::ShiftRight,
                        Rule::ShiftLeft,
                        Rule::Tilde,
                        Rule::Pipe,
                    ],
                    negatives: vec![rule],
                },
                value.as_span(),
            ))?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LogicCmpOp {
    And,
    Or,
}

impl TryFrom<Pair<'_, Rule>> for LogicCmpOp {
    type Error = PestError<Rule>;

    fn try_from(value: Pair<Rule>) -> Result<Self, Self::Error> {
        Ok(match value.as_rule() {
            Rule::And => Self::And,
            Rule::Or => Self::Or,

            rule => Err(PestError::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![Rule::And, Rule::Or],
                    negatives: vec![rule],
                },
                value.as_span(),
            ))?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

impl TryFrom<Pair<'_, Rule>> for CmpOp {
    type Error = PestError<Rule>;

    fn try_from(value: Pair<Rule>) -> Result<Self, Self::Error> {
        Ok(match value.as_rule() {
            Rule::Eq => Self::Eq,
            Rule::Neq => Self::Neq,
            Rule::Gt => Self::Gt,
            Rule::GtEq => Self::GtEq,
            Rule::Lt => Self::Lt,
            Rule::LtEq => Self::LtEq,

            rule => Err(PestError::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![
                        Rule::Eq,
                        Rule::Neq,
                        Rule::Gt,
                        Rule::GtEq,
                        Rule::Lt,
                        Rule::LtEq,
                    ],
                    negatives: vec![rule],
                },
                value.as_span(),
            ))?,
        })
    }
}
#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Neg,
    Not,
    Len,
    Inv,
}

impl TryFrom<Pair<'_, Rule>> for Unop {
    type Error = PestError<Rule>;

    fn try_from(value: Pair<Rule>) -> Result<Self, Self::Error> {
        Ok(match value.as_rule() {
            Rule::Neg => Self::Neg,
            Rule::Pound => Self::Len,
            Rule::Not => Self::Not,
            Rule::Tilde => Self::Inv,

            rule => Err(PestError::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![Rule::Neg, Rule::Tilde, Rule::Pound, Rule::Not],
                    negatives: vec![rule],
                },
                value.as_span(),
            ))?,
        })
    }
}
