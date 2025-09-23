use derive_more::derive::From;
use derive_new::new;
use pest::{error::Error as PestError, iterators::Pair};

use crate::{
    ast::LineInfo,
    luz::{
        err::LuzError,
        obj::{FuncParams, LuzObj},
    },
    Rule,
};

use super::StatNode;

#[derive(Debug, Clone, new)]
pub struct Exp {
    pub node: ExpNode,
    pub line_info: LineInfo,
}

#[derive(Debug, Clone, From)]
pub enum ExpNode {
    Literal(LuzObj),
    Vararg,
    Name(String),
    Var(Box<ExpNode>),
    Unop(Unop, Box<ExpNode>),
    Binop {
        op: Binop,
        lhs: Box<ExpNode>,
        rhs: Box<ExpNode>,
    },
    CmpOp {
        op: CmpOp,
        lhs: Box<ExpNode>,
        rhs: Box<ExpNode>,
    },
    LogicCmpOp {
        op: LogicCmpOp,
        lhs: Box<ExpNode>,
        rhs: Box<ExpNode>,
    },
    Access(ExpAccess),
    FuncDef(FuncDef),
    FuncCall(FuncCall),
    TableConstructor(ExpTableConstructor),
}

impl ExpNode {
    pub fn normalize(&self) -> &ExpNode {
        match self {
            ExpNode::Unop(unop, exp) if *unop == Unop::Not => exp.normalize(),
            _ => self,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct ExpAccess {
    pub exp: Box<ExpNode>,
    pub prop: Box<ExpNode>,
}

#[derive(Debug, Clone, new)]
pub struct FuncDef {
    pub params: FuncParams,
    pub body: Vec<StatNode>,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub func: Box<ExpNode>,
    pub method_name: Option<String>,
    pub args: Vec<ExpNode>,
    pub variadic: bool,
}

impl FuncCall {
    pub fn new(func: Box<ExpNode>, method_name: Option<String>, args: Vec<ExpNode>) -> Self {
        Self {
            func,
            method_name,
            variadic: ExpNode::is_multires(&args),
            args,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpTableConstructor {
    pub arr_fields: Vec<ExpNode>,
    pub obj_fields: Vec<ExpTableConstructorField>,
    pub last_exp: Option<Box<ExpNode>>,
    pub variadic: bool,
}

impl ExpTableConstructor {
    pub fn new(
        arr_fields: Vec<ExpNode>,
        obj_fields: Vec<ExpTableConstructorField>,
        last_exp: Option<Box<ExpNode>>,
    ) -> Self {
        Self {
            arr_fields,
            obj_fields,
            variadic: last_exp.as_ref().is_some_and(|exp| exp.is_multire()),
            last_exp,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct ExpTableConstructorField {
    pub key: Box<ExpNode>,
    pub val: Box<ExpNode>,
}

impl ExpNode {
    pub fn is_multire(&self) -> bool {
        matches!(self, ExpNode::FuncCall(_) | ExpNode::Vararg)
    }

    pub fn is_multires(explist: &Vec<ExpNode>) -> bool {
        explist.last().is_some_and(|exp| exp.is_multire())
    }

    pub fn do_unop(self, unop: Unop) -> Result<Self, LuzError> {
        Ok(match self {
            Self::Literal(obj) => obj.apply_unop(unop)?.into(),
            _ => Self::Unop(unop, Box::new(self)),
        })
    }

    pub fn do_binop(self, binop: Binop, rhs: ExpNode) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            (ref s @ Self::Literal(ref obj), ref rhs @ Self::Literal(ref obj2)) => {
                let result = obj.clone().apply_binop(binop, obj2.clone());
                match result {
                    Ok(result) => result.into(),
                    Err(_) => Self::Binop {
                        lhs: Box::new(s.clone()),
                        op: binop,
                        rhs: Box::new(rhs.clone()),
                    },
                }
            }
            (s, rhs) => Self::Binop {
                lhs: Box::new(s),
                op: binop,
                rhs: Box::new(rhs),
            },
        })
    }

    pub fn do_cmp(self, cmpop: CmpOp, rhs: ExpNode) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            // (Self::Literal(obj), Self::Literal(obj2)) => obj.apply_cmp(cmpop, obj2)?.into(),
            (s, rhs) => Self::CmpOp {
                lhs: Box::new(s),
                op: cmpop,
                rhs: Box::new(rhs),
            },
        })
    }

    pub fn do_logic_cmp(self, logic_cmp_op: LogicCmpOp, rhs: ExpNode) -> Result<Self, LuzError> {
        Ok(match (self, rhs) {
            (Self::Literal(obj), rhs) if logic_cmp_op == LogicCmpOp::And && obj.is_truthy() => rhs,
            // (Self::Literal(LuzObj::Boolean(b)), rhs) => {
            //     if b {
            //         match logic_cmp_op {
            //             LogicCmpOp::And => rhs,
            //             LogicCmpOp::Or => LuzObj::Boolean(b).into(),
            //         }
            //     } else {
            //         match logic_cmp_op {
            //             LogicCmpOp::And => LuzObj::Boolean(b).into(),
            //             LogicCmpOp::Or => rhs,
            //         }
            //     }
            // }
            //
            // (Self::Literal(LuzObj::Nil), rhs) => match logic_cmp_op {
            //     LogicCmpOp::And => LuzObj::Nil.into(),
            //     LogicCmpOp::Or => rhs,
            // },
            //
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

impl CmpOp {
    pub fn flip_op(self) -> Self {
        match self {
            CmpOp::Eq => CmpOp::Eq,
            CmpOp::Neq => CmpOp::Neq,
            CmpOp::Lt => CmpOp::Gt,
            CmpOp::Gt => CmpOp::Lt,
            CmpOp::LtEq => CmpOp::GtEq,
            CmpOp::GtEq => CmpOp::LtEq,
        }
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            Rule::Bnot => Self::Inv,

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
