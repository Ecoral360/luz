use std::str::FromStr;

use derive_more::derive::From;
use derive_new::new;

use crate::{ast::LineInfo, luz::err::LuzError};

use super::{ExpNode, FuncCall};

#[derive(Debug, Clone, new)]
pub struct Stat {
    pub node: StatNode,
    pub line_info: LineInfo,
}

#[derive(Debug, Clone, From)]
pub enum StatNode {
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
    pub explist: Vec<ExpNode>,
    pub variadic: bool,
}

impl ReturnStat {
    pub fn new(explist: Vec<ExpNode>) -> Self {
        Self {
            variadic: ExpNode::is_multires(&explist),
            explist,
        }
    }
}

#[derive(Debug, Clone, new)]
pub struct DoStat {
    pub block: Vec<StatNode>,
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
    pub cond: Box<ExpNode>,
    pub then_br: Vec<StatNode>,
    pub elseif_brs: Vec<(ExpNode, Vec<StatNode>)>,
    pub else_br: Option<Vec<StatNode>>,
}

#[derive(Debug, Clone, new)]
pub struct WhileStat {
    pub cond: Box<ExpNode>,
    pub block: Vec<StatNode>,
}

#[derive(Debug, Clone, new)]
pub struct RepeaStat {
    pub block: Vec<StatNode>,
    pub cond: Box<ExpNode>,
}

#[derive(Debug, Clone, new)]
pub struct ForRangeStat {
    pub var: String,
    pub start: Box<ExpNode>,
    pub limit: Box<ExpNode>,
    pub step: Option<Box<ExpNode>>,
    pub block: Vec<StatNode>,
}

#[derive(Debug, Clone, new)]
pub struct ForInStat {
    pub vars: Vec<String>,
    pub exps: Vec<ExpNode>,
    pub block: Vec<StatNode>,
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
        varlist: Vec<ExpNode>,
        explist: Vec<ExpNode>,
    },
    Local {
        varlist: Vec<(String, Option<Attrib>)>,
        explist: Vec<ExpNode>,
        /// If the value assigned is a closure
        closure: bool,
    },
}
