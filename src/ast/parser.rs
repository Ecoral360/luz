use std::fmt::write;

use pest::iterators::Pair;
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::ast::{
    AssignStat, Exp, ForInStat, ForRangeStat, FunctionDefStat, IfStat, RepeaStat, ReturnStat, Stat,
    WhileStat,
};
use crate::luz::err::LuzError;
use crate::luz::obj::{FuncParams, FuncParamsBuilder, Numeral};
use crate::Rule;

use super::{GotoStat, LabelStat};

lazy_static::lazy_static! {
    static ref PRATT_EXPR_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};

        // Precedence is defined lowest to highest
        PrattParser::new()
                        // Logic op
            .op(Op::infix(Rule::Or, Left))
            .op(Op::infix(Rule::And, Left))

            // Comparaison op
            .op(Op::infix(Rule::Lt, Left) |
                Op::infix(Rule::Gt, Left) |
                Op::infix(Rule::LtEq, Left) |
                Op::infix(Rule::GtEq, Left) |
                Op::infix(Rule::Neq, Left) |
                Op::infix(Rule::Eq, Left)
            )

            // Bitwise op
            .op(Op::infix(Rule::Pipe, Left))
            .op(Op::infix(Rule::Tilde, Left))
            .op(Op::infix(Rule::Ampersand, Left))
            // Bitwise op
            .op(Op::infix(Rule::ShiftLeft, Left) | Op::infix(Rule::ShiftRight, Left))

            // Arithmetic op
            .op(Op::infix(Rule::DotDot, Right))
            .op(Op::infix(Rule::Plus, Left) | Op::infix(Rule::Minus, Left))
            .op(Op::infix(Rule::Star, Left) |
                Op::infix(Rule::Slash, Left) |
                Op::infix(Rule::DoubleSlash, Left) |
                Op::infix(Rule::Pourcent, Left))
            .op(Op::prefix(Rule::Not) | Op::prefix(Rule::Pound) | Op::prefix(Rule::Neg) | Op::prefix(Rule::Tilde))
            .op(Op::infix(Rule::Caret, Right))
            // Ternary

            // Logic op

            // Comparaison op

            // Bitwise op

            // Arithmetic op
    };
}

fn parse_list(pairs: Pairs<Rule>) -> Result<Vec<Exp>, LuzError> {
    pairs
        .map(|pair| parse_top_expr(pair))
        .collect::<Result<_, _>>()
}

fn parse_top_expr(pair: Pair<Rule>) -> Result<Exp, LuzError> {
    Ok(match pair.as_rule() {
        Rule::exp | Rule::PrefixExp => parse_expr(pair.into_inner())?,
        Rule::Var => {
            let var = parse_expr(pair.into_inner())?;
            Exp::Var(Box::new(var))
        }
        Rule::Numeral => {
            if let Ok(int) = pair.as_str().parse::<i64>() {
                Exp::Literal(Numeral::Int(int).into())
            } else if let Ok(float) = pair.as_str().parse::<f64>() {
                Exp::Literal(Numeral::Float(float).into())
            } else {
                todo!()
            }
        }
        Rule::Name => Exp::Name(pair.as_str().to_string()),

        // Rule::FuncDef => {
        // }

        _ => todo!("Rule::{:?}", pair.as_rule()),
    })
}

fn parse_expr(pairs: Pairs<Rule>) -> Result<Exp, LuzError> {
    PRATT_EXPR_PARSER
        .map_primary(|primary| parse_top_expr(primary))
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Neg | Rule::Not | Rule::Tilde => {
                let rhs = rhs?;
                rhs.do_unop(op.try_into()?)
            }
            _ => todo!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            _ => todo!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::Plus
            | Rule::Minus
            | Rule::Star
            | Rule::Slash
            | Rule::DoubleSlash
            | Rule::Pourcent
            | Rule::Caret
            | Rule::Ampersand
            | Rule::ShiftRight
            | Rule::ShiftLeft
            | Rule::Tilde
            | Rule::Pipe => {
                let lhs = lhs?;
                let rhs = rhs?;

                lhs.do_binop(op.try_into()?, rhs)
            }

            Rule::Eq | Rule::Neq | Rule::Gt | Rule::GtEq | Rule::Lt | Rule::LtEq => {
                let lhs = lhs?;
                let rhs = rhs?;

                lhs.do_cmp(op.try_into()?, rhs)
            }

            _ => todo!(),
        })
        .parse(pairs)
}

pub fn parse_namelist(pairs: Pairs<Rule>) -> Vec<String> {
    pairs.map(|e| e.as_str().to_string()).collect::<Vec<_>>()
}

pub fn parse_stmts(pairs: &mut Pairs<Rule>) -> Result<Vec<Stat>, LuzError> {
    pairs.try_fold(vec![], |mut acc, pair| {
        acc.extend(parse_stmt(pair)?);
        Ok(acc)
    })
}

pub fn parse_stmt(pair: Pair<Rule>) -> Result<Vec<Stat>, LuzError> {
    Ok(match pair.as_rule() {
        Rule::Chunk | Rule::block => parse_stmts(&mut pair.into_inner())?,
        Rule::BreakStat => vec![Stat::Break],
        Rule::GotoStat => vec![GotoStat::new(
            pair.into_inner()
                .next()
                .expect("Label to goto")
                .as_str()
                .to_string(),
        )
        .into()],
        Rule::Label => vec![LabelStat::new(
            pair.into_inner()
                .next()
                .expect("Name of label")
                .as_str()
                .to_string(),
        )
        .into()],
        Rule::LocalAssignStat => {
            let mut inner = pair.into_inner();
            let varlist =
                parse_namelist(inner.next().expect("Variables in assignment").into_inner());
            let explist = parse_list(
                inner
                    .next()
                    .expect("Expressions in assignment")
                    .into_inner(),
            )?;

            vec![AssignStat::new_local(varlist, explist).into()]
        }
        Rule::RetStat => {
            let mut inner = pair.into_inner();
            let explist = inner
                .next()
                .map(|e| parse_list(e.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;

            vec![ReturnStat::new(explist).into()]
        }
        Rule::AssignStat => {
            let mut inner = pair.into_inner();
            let varlist = parse_list(inner.next().expect("Variables in assignment").into_inner())?;
            let explist = parse_list(
                inner
                    .next()
                    .expect("Expressions in assignment")
                    .into_inner(),
            )?;

            vec![AssignStat::new_normal(varlist, explist).into()]
        }
        Rule::WhileStat => {
            let mut inner = pair.into_inner();
            let cond = parse_top_expr(inner.next().expect("While cond"))?;
            let body = inner
                .next()
                .map(|s| parse_stmts(&mut s.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;

            vec![WhileStat::new(Box::new(cond), body).into()]
        }
        Rule::RepeatStat => {
            let mut inner = pair.into_inner();
            let body = inner
                .next()
                .map(|s| parse_stmts(&mut s.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;
            let cond = parse_top_expr(inner.next().expect("While cond"))?;

            vec![RepeaStat::new(body, Box::new(cond)).into()]
        }
        Rule::LocalFunctionDefStat => {
            let mut inner = pair.into_inner();
            let name = inner.next().expect("Function name").as_str().to_string();

            let mut signature = inner.next().expect("Function param & body").into_inner();
            let next = signature.peek().expect("Function body has something");
            let mut params = FuncParamsBuilder::default();

            if let Rule::parlist = next.as_rule() {
                let mut parlist = signature.next().unwrap().into_inner();
                let last = parlist.next_back().unwrap();
                let mut fixed = parse_namelist(parlist);
                if last.as_rule() == Rule::Ellipse {
                    params.is_vararg(true);
                } else {
                    fixed.push(last.as_str().to_string());
                }
                params.fixed(fixed);
            }

            let body = parse_stmts(&mut signature.next().expect("Function body").into_inner())?;

            vec![FunctionDefStat::new_local(name, params.build().unwrap(), body).into()]
        }
        Rule::FunctionDefStat => {
            let mut inner = pair.into_inner();
            let mut names = inner.next().expect("Function name").into_inner();
            let last_name = names.next_back().unwrap();
            let mut namelist = parse_namelist(names);

            let mut method = None;
            if matches!(last_name.as_node_tag(), Some("method")) {
                method = Some(last_name.as_str().to_string());
            } else {
                namelist.push(last_name.as_str().to_string());
            }

            let mut signature = inner.next().expect("Function param & body").into_inner();
            let next = signature.peek().expect("Function body has something");
            let mut params = FuncParamsBuilder::default();

            if let Rule::parlist = next.as_rule() {
                let mut parlist = signature.next().unwrap().into_inner();
                let last = parlist.next_back().unwrap();
                let mut fixed = parse_namelist(parlist);
                if last.as_rule() == Rule::Ellipse {
                    params.is_vararg(true);
                } else {
                    fixed.push(last.as_str().to_string());
                }
                params.fixed(fixed);
            }

            let body = parse_stmts(&mut signature.next().expect("Function body").into_inner())?;

            vec![
                FunctionDefStat::new_normal(namelist, method, params.build().unwrap(), body).into(),
            ]
        }
        Rule::ForRangeStat => {
            let mut inner = pair.into_inner();
            let var = inner
                .next()
                .expect("FoRange loop variable")
                .as_str()
                .to_string();
            let start = parse_top_expr(inner.next().expect("FoRange start exp"))?;
            let limit = parse_top_expr(inner.next().expect("FoRange end exp"))?;
            let next = inner.peek().expect("ForRange third element");
            let step = if matches!(next.as_node_tag(), Some("step")) {
                Some(parse_top_expr(inner.next().unwrap())?)
            } else {
                None
            };
            let body = parse_stmts(&mut inner.next().expect("FoRange body").into_inner())?;

            vec![ForRangeStat::new(
                var,
                Box::new(start),
                Box::new(limit),
                step.map(Box::new),
                body,
            )
            .into()]
        }
        Rule::ForInStat => {
            let mut inner = pair.into_inner();
            let namelist = parse_namelist(inner.next().expect("ForIn namelist").into_inner());

            let explist = inner
                .next()
                .expect("ForIn explist")
                .into_inner()
                .map(|e| parse_top_expr(e))
                .collect::<Result<Vec<_>, _>>()?;

            let body = parse_stmts(&mut inner.next().expect("ForIn body").into_inner())?;

            vec![ForInStat::new(namelist, explist, body).into()]
        }
        Rule::IfStat => {
            let mut inner = pair.into_inner();
            let cond = parse_top_expr(inner.next().expect("If cond"))?;
            let mut then_br = vec![];
            let mut elseif_brs = vec![];
            let mut else_br = None;

            for pair in inner {
                match pair.as_rule() {
                    Rule::blockIf => {
                        then_br = parse_stmts(&mut pair.into_inner())?;
                    }
                    Rule::ElseIfStat => {
                        let mut elseif = pair.into_inner();
                        let elseif_cond = parse_top_expr(elseif.next().expect("Elseif cond"))?;
                        let elseif_body = elseif
                            .next()
                            .map(|e| parse_stmts(&mut e.into_inner()))
                            .unwrap_or_else(|| Ok(vec![]))?;
                        elseif_brs.push((elseif_cond, elseif_body));
                    }
                    Rule::ElseStat => {
                        else_br = Some(
                            pair.into_inner()
                                .next()
                                .map(|e| parse_stmts(&mut e.into_inner()))
                                .unwrap_or_else(|| Ok(vec![]))?,
                        );
                    }
                    _ => unreachable!(),
                }
            }

            vec![IfStat::new(Box::new(cond), then_br, elseif_brs, else_br).into()]
        }
        Rule::EOI => {
            vec![]
        }
        _ => todo!("Rule::{:?}", pair.as_rule()),
    })
}
