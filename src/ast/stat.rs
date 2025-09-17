use std::str::FromStr;

use derive_more::derive::From;
use derive_new::new;

use crate::luz::err::LuzError;

use super::{Exp, FuncCall};

#[derive(Debug, Clone, From)]
pub enum Stat {
    Assign(AssignStat),
    Return(ReturnStat),
    FuncCall(FuncCall),
    // FunctionDef(FunctionDefStat),
    Do(DoStat),
    While(WhileStat),
    Repeat(RepeaStat),
    If(IfStat),
    ForRange(ForRangeStat),
    ForIn(ForInStat),
    Break,
    Goto(GotoStat),
    Label(LabelStat),
}

#[derive(Debug, Clone, new)]
pub struct GotoStat(pub String);

#[derive(Debug, Clone, new)]
pub struct LabelStat(pub String);

#[derive(Debug, Clone)]
pub struct ReturnStat {
    pub explist: Vec<Exp>,
    pub variadic: bool,
}

impl ReturnStat {
    pub fn new(explist: Vec<Exp>) -> Self {
        Self {
            variadic: Exp::is_multires(&explist),
            explist,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct DoStat {
    pub block: Vec<Stat>,
}

// #[derive(Debug, Clone, new)]
// pub enum FunctionDefStat {
//     Normal {
//         name: Vec<String>,
//         method: Option<String>,
//         params: FuncParams,
//         body: Vec<Stat>,
//     },
//     Local {
//         name: String,
//         params: FuncParams,
//         body: Vec<Stat>,
//     },
// }

#[derive(Debug, Clone, new)]
pub struct IfStat {
    pub cond: Box<Exp>,
    pub then_br: Vec<Stat>,
    pub elseif_brs: Vec<(Exp, Vec<Stat>)>,
    pub else_br: Option<Vec<Stat>>,
}

#[derive(Debug, Clone, new)]
pub struct WhileStat {
    pub cond: Box<Exp>,
    pub block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub struct RepeaStat {
    pub block: Vec<Stat>,
    pub cond: Box<Exp>,
}

#[derive(Debug, Clone, new)]
pub struct ForRangeStat {
    pub var: String,
    pub start: Box<Exp>,
    pub limit: Box<Exp>,
    pub step: Option<Box<Exp>>,
    pub block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub struct ForInStat {
    pub vars: Vec<String>,
    pub exps: Vec<Exp>,
    pub block: Vec<Stat>,
}

#[derive(Debug, Clone, Copy)]
pub enum Attrib {
    Const,
    Close,
}

impl FromStr for Attrib {
    type Err = LuzError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s[1..s.len() - 1].trim() {
            "close" => Ok(Self::Close),
            "const" => Ok(Self::Const),
            _ => Err(LuzError::InvalidAttribute(s.to_string())),
        }
    }
}

#[derive(Debug, Clone, new)]
pub enum AssignStat {
    Normal {
        varlist: Vec<Exp>,
        explist: Vec<Exp>,
    },
    Local {
        varlist: Vec<(String, Option<Attrib>)>,
        explist: Vec<Exp>,
        /// If the value assigned is a closure
        closure: bool,
    },
}
