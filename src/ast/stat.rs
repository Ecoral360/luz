use derive_more::derive::From;
use derive_new::new;

use crate::luz::obj::FuncParams;

use super::{Exp, FuncCall};

#[derive(Debug, Clone, From)]
pub enum Stat {
    Assign(AssignStat),
    Return(ReturnStat),
    FuncCall(FuncCall),
    FunctionDef(FunctionDefStat),
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

#[derive(Debug, Clone, new)]
pub struct ReturnStat {
    explist: Vec<Exp>,
}

#[derive(Debug, Clone, new)]
pub struct DoStat {
    block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub enum FunctionDefStat {
    Normal {
        name: Vec<String>,
        method: Option<String>,
        params: FuncParams,
        body: Vec<Stat>,
    },
    Local {
        name: String,
        params: FuncParams,
        body: Vec<Stat>,
    },
}

#[derive(Debug, Clone, new)]
pub struct IfStat {
    cond: Box<Exp>,
    then_br: Vec<Stat>,
    elseif_brs: Vec<(Exp, Vec<Stat>)>,
    else_br: Option<Vec<Stat>>,
}

#[derive(Debug, Clone, new)]
pub struct WhileStat {
    cond: Box<Exp>,
    block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub struct RepeaStat {
    block: Vec<Stat>,
    cond: Box<Exp>,
}

#[derive(Debug, Clone, new)]
pub struct ForRangeStat {
    var: String,
    start: Box<Exp>,
    limit: Box<Exp>,
    step: Option<Box<Exp>>,
    block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub struct ForInStat {
    vars: Vec<String>,
    exps: Vec<Exp>,
    block: Vec<Stat>,
}

#[derive(Debug, Clone, new)]
pub enum AssignStat {
    Normal {
        varlist: Vec<Exp>,
        explist: Vec<Exp>,
    },
    Local {
        varlist: Vec<String>,
        explist: Vec<Exp>,
    },
}
