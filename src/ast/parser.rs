use std::str::FromStr;
use std::vec;

use pest::iterators::Pair;
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::ast::{
    AssignStat, Attrib, DoStat, ExpNode, ExpTableConstructor, ExpTableConstructorField, ForInStat,
    ForRangeStat, FuncCall, FuncDef, IfStat, RepeaStat, ReturnStat, Stat, StatNode, WhileStat,
};
use crate::luz::err::LuzError;
use crate::luz::obj::{FuncParamsBuilder, LuzObj, Numeral};
use crate::Rule;

use super::{ExpAccess, GotoStat, LabelStat};

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
            .op(Op::prefix(Rule::Not) | Op::prefix(Rule::Pound) | Op::prefix(Rule::Neg) | Op::prefix(Rule::Bnot))
            .op(Op::infix(Rule::Caret, Right))

            .op(Op::postfix(Rule::PostfixExp))
    };
}

fn parse_list(pairs: Pairs<Rule>) -> Result<Vec<ExpNode>, LuzError> {
    pairs
        .map(|pair| parse_top_expr(pair))
        .collect::<Result<_, _>>()
}

fn parse_prefix_exp(prefix_exp: ExpNode, pair: Pair<Rule>) -> Result<ExpNode, LuzError> {
    match pair.as_rule() {
        Rule::Access => {
            let mut inner = pair.into_inner();
            let element = inner.next().expect("Accessor");
            if matches!(element.as_rule(), Rule::Name) {
                Ok(ExpNode::Access(ExpAccess::new(
                    Box::new(prefix_exp),
                    Box::new(ExpNode::Literal(LuzObj::str(element.as_str()))),
                )))
            } else {
                Ok(ExpNode::Access(ExpAccess::new(
                    Box::new(prefix_exp),
                    Box::new(parse_top_expr(element)?),
                )))
            }
        }
        Rule::Call => {
            let mut inner = pair.into_inner();

            let mut method_name = None;
            if inner
                .peek()
                .map(|v| v.as_node_tag().is_some_and(|t| t == "method"))
                .unwrap_or_default()
            {
                method_name = Some(
                    inner
                        .next()
                        .expect("Should always work because of previous check")
                        .as_str()
                        .to_string(),
                );
            }
            let args = if inner.len() == 0 {
                vec![]
            } else {
                parse_args(inner.next().expect("args"))?
            };
            Ok(ExpNode::FuncCall(FuncCall::new(
                Box::new(prefix_exp),
                method_name,
                args,
            )))
        }
        _ => todo!("Handle Rule::{:?}", pair.as_rule()),
    }
}

fn parse_args(args: Pair<Rule>) -> Result<Vec<ExpNode>, LuzError> {
    let Some(args) = args.into_inner().next() else {
        return Ok(vec![]);
    };
    if matches!(args.as_rule(), Rule::explist) {
        let args_inner = args.into_inner();
        Ok(args_inner
            .map(|arg| parse_top_expr(arg))
            .collect::<Result<_, _>>()?)
    } else {
        Ok(vec![parse_top_expr(args)?])
    }
}

fn parse_prefix_exps(
    first_expr: Result<ExpNode, LuzError>,
    exp: Pairs<Rule>,
) -> Result<ExpNode, LuzError> {
    let inner = exp;
    inner.fold(first_expr, |expr, el| parse_prefix_exp(expr?, el))
}

fn parse_top_expr(pair: Pair<Rule>) -> Result<ExpNode, LuzError> {
    Ok(match pair.as_rule() {
        Rule::exp => parse_expr(pair.into_inner())?,
        Rule::PrefixExp => {
            // let prefix: Vec<Pair<Rule>> = pair
            //     .into_inner()
            //     .take_while(|pair| pair.as_node_tag().is_none_or(|tag| tag != "postfix"))
            //     .collect();
            // let access_or_calls = pair.into_inner().find_tagged("postfix");

            let inner = pair.into_inner();
            if inner
                .peek()
                .is_some_and(|child| matches!(child.as_node_tag(), Some("in_parent")))
            {
                let exp = parse_expr(inner)?;
                if exp.is_multire() {
                    ExpNode::InParent(Box::new(exp))
                } else {
                    exp
                }
            } else {
                parse_expr(inner)?
            }
        }
        Rule::Nil => ExpNode::Literal(LuzObj::Nil),
        Rule::Boolean => ExpNode::Literal(LuzObj::Boolean(pair.as_str() == "true")),
        Rule::LiteralString => ExpNode::Literal(LuzObj::from_literal_str(pair.as_str())?),
        Rule::Ellipse => ExpNode::Vararg,
        Rule::Var => {
            let mut inner = pair.into_inner();
            let var = parse_top_expr(inner.next().expect("Var"));
            if inner.len() > 0 {
                parse_prefix_exps(var, inner)?
            } else {
                ExpNode::Var(Box::new(var?))
            }
        }
        Rule::Numeral => {
            let string = pair.as_str();
            ExpNode::Literal(LuzObj::Numeral(Numeral::from_str(string)?))
        }
        Rule::Name => ExpNode::Name(pair.as_str().to_string()),

        Rule::Access => {
            todo!()
        }

        Rule::TableConstructor => {
            let fields = pair.into_inner().next();
            if let Some(fields) = fields {
                let mut arr_fields = vec![];
                let mut obj_fields = vec![];
                let mut fields_inner = fields.into_inner();
                let last_field = fields_inner.next_back();

                for field in fields_inner {
                    parse_table_field(&mut arr_fields, &mut obj_fields, field)?;
                }

                match last_field {
                    Some(last_field_val) => {
                        let inner_field = last_field_val.clone().into_inner().next().unwrap();

                        if matches!(inner_field.as_node_tag(), Some("value_field")) {
                            let exp = parse_top_expr(inner_field)?;
                            let last_exp = if exp.is_multire() {
                                Some(Box::new(exp))
                            } else {
                                arr_fields.push(exp);
                                None
                            };
                            ExpNode::TableConstructor(ExpTableConstructor::new(
                                arr_fields, obj_fields, last_exp,
                            ))
                        } else {
                            parse_table_field(&mut arr_fields, &mut obj_fields, last_field_val)?;
                            ExpNode::TableConstructor(ExpTableConstructor::new(
                                arr_fields, obj_fields, None,
                            ))
                        }
                    }
                    None => ExpNode::TableConstructor(ExpTableConstructor::new(
                        arr_fields, obj_fields, None,
                    )),
                }
            } else {
                ExpNode::TableConstructor(ExpTableConstructor::new(vec![], vec![], None))
            }
        }

        Rule::FuncDef => {
            let mut inner = pair.into_inner();

            let mut signature = inner.next().expect("Function param & body").into_inner();
            let next = signature.peek().expect("Function body has something");
            let mut params = FuncParamsBuilder::default();

            let mut fixed = vec![];
            if let Rule::parlist = next.as_rule() {
                let mut parlist = signature.next().unwrap().into_inner();
                let last = parlist.next_back().unwrap();
                if last.as_rule() == Rule::Ellipse {
                    params.is_vararg(true);
                    if let Some(args) = parlist.next() {
                        fixed.append(&mut parse_namelist(args.into_inner()));
                    }
                } else {
                    fixed.append(&mut parse_namelist(last.into_inner()));
                }
            }

            params.fixed(fixed);

            let body = parse_stmts(&mut signature.next().expect("Function body").into_inner())?;

            ExpNode::FuncDef(FuncDef::new(params.build().unwrap(), body))
        }
        _ => todo!("Top Expr Rule::{:?}", pair.as_rule()),
    })
}

fn parse_table_field(
    arr_fields: &mut Vec<ExpNode>,
    obj_fields: &mut Vec<ExpTableConstructorField>,
    field: Pair<Rule>,
) -> Result<(), LuzError> {
    let mut field_inner = field.into_inner();

    let field_inner_first = field_inner.peek().expect("Field key/value");

    let field_inner_next = field_inner.next().expect("Field key/value");
    let exp = if matches!(field_inner_next.as_rule(), Rule::Name) {
        ExpNode::Literal(LuzObj::str(field_inner_next.as_str()))
    } else {
        parse_top_expr(field_inner_next)?
    };

    if matches!(field_inner_first.as_node_tag(), Some("value_field")) {
        arr_fields.push(exp);
    } else {
        let val = parse_top_expr(field_inner.next().expect("Field Value"))?;
        obj_fields.push(ExpTableConstructorField::new(Box::new(exp), Box::new(val)));
    }
    Ok(())
}

fn parse_expr(pairs: Pairs<Rule>) -> Result<ExpNode, LuzError> {
    PRATT_EXPR_PARSER
        .map_primary(|primary| parse_top_expr(primary))
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::Neg | Rule::Not | Rule::Bnot | Rule::Pound => {
                let rhs = rhs?;
                rhs.do_unop(op.try_into()?)
            }
            _ => todo!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::PostfixExp => parse_prefix_exps(lhs, op.into_inner()),
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
            | Rule::DotDot
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

            Rule::And | Rule::Or => {
                let lhs = lhs?;
                let rhs = rhs?;

                lhs.do_logic_cmp(op.try_into()?, rhs)
            }

            op => todo!("Expr {:?}", op),
        })
        .parse(pairs)
}

pub fn parse_namelist(pairs: Pairs<Rule>) -> Vec<String> {
    pairs.map(|e| e.as_str().to_string()).collect::<Vec<_>>()
}

pub fn parse_nameattriblist(pairs: Pairs<Rule>) -> Result<Vec<(String, Option<Attrib>)>, LuzError> {
    let mut names: Vec<(String, Option<Attrib>)> = vec![];
    for pair in pairs {
        if matches!(pair.as_node_tag(), Some("attrib")) {
            names.last_mut().unwrap().1.replace(pair.as_str().parse()?);
        } else {
            names.push((pair.as_str().to_string(), None));
        }
    }
    Ok(names)
}

pub fn parse_script(pairs: &mut Pairs<Rule>) -> Result<Vec<Stat>, LuzError> {
    let mut stats = parse_stmts(pairs)?;
    stats.push(StatNode::Return(ReturnStat::new(vec![])).to_oef_stat());
    Ok(stats)
}

fn parse_stmts(pairs: &mut Pairs<Rule>) -> Result<Vec<Stat>, LuzError> {
    pairs.try_fold(vec![], |mut acc, pair| {
        acc.extend(parse_stmt(pair)?);
        Ok(acc)
    })
}

pub fn parse_func_call(pair: Pair<Rule>) -> Result<FuncCall, LuzError> {
    let mut inner = pair.into_inner();

    let last_call = inner.next_back().expect("Last Call");
    let first_expr = parse_top_expr(inner.next().expect("Caller"));
    let func = parse_prefix_exps(first_expr, inner)?;

    let mut last_call = last_call.into_inner();
    let mut method_name = None;
    if last_call
        .peek()
        .map(|v| v.as_node_tag().is_some_and(|t| t == "method"))
        .unwrap_or_default()
    {
        method_name = Some(
            last_call
                .next()
                .expect("Should always work because of previous check")
                .as_str()
                .to_string(),
        );
    }
    let args = if last_call.len() == 0 {
        vec![]
    } else {
        parse_args(last_call.next().expect("args"))?
    };
    Ok(FuncCall::new(Box::new(func), method_name, args))
}

fn invert<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    x.map_or(Ok(None), |v| v.map(Some))
}

pub fn parse_stmt(pair: Pair<Rule>) -> Result<Vec<Stat>, LuzError> {
    let pair_span = pair.as_span();
    Ok(match pair.as_rule() {
        Rule::Chunk | Rule::block => parse_stmts(&mut pair.into_inner())?,
        Rule::DoStat => vec![StatNode::Do(DoStat {
            block: parse_stmt(pair.into_inner().next().expect("Do body"))?,
        })
        .to_stat(pair_span)],
        Rule::BreakStat => vec![StatNode::Break.to_stat(pair_span)],
        Rule::GotoStat => vec![StatNode::Goto(GotoStat::new(
            pair.into_inner()
                .next()
                .expect("Label to goto")
                .as_str()
                .to_string(),
        ))
        .to_stat(pair_span)],
        Rule::Label => vec![StatNode::Label(LabelStat::new(
            pair.into_inner()
                .next()
                .expect("Name of label")
                .as_str()
                .to_string(),
        ))
        .to_stat(pair_span)],
        Rule::LocalAssignStat => {
            let mut inner = pair.into_inner();
            let varlist =
                parse_nameattriblist(inner.next().expect("Variables in assignment").into_inner())?;
            let explist = invert(inner.next().map(|explist| parse_list(explist.into_inner())))?;

            vec![StatNode::Assign(AssignStat::new_local(
                varlist,
                explist.unwrap_or_default(),
                false,
            ))
            .to_stat(pair_span)]
        }
        Rule::RetStat => {
            let mut inner = pair.into_inner();
            let explist = inner
                .next()
                .map(|e| parse_list(e.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;

            vec![StatNode::Return(ReturnStat::new(explist)).to_stat(pair_span)]
        }
        Rule::FunctionCall => {
            vec![StatNode::FuncCall(parse_func_call(pair)?).to_stat(pair_span)]
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

            vec![StatNode::Assign(AssignStat::new_normal(varlist, explist)).to_stat(pair_span)]
        }
        Rule::WhileStat => {
            let mut inner = pair.into_inner();
            let cond = parse_top_expr(inner.next().expect("While cond"))?;
            let body = inner
                .next()
                .map(|s| parse_stmts(&mut s.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;

            vec![StatNode::While(WhileStat::new(Box::new(cond), body)).to_stat(pair_span)]
        }
        Rule::RepeatStat => {
            let mut inner = pair.into_inner();
            let body = inner
                .next()
                .map(|s| parse_stmts(&mut s.into_inner()))
                .unwrap_or_else(|| Ok(vec![]))?;
            let cond = parse_top_expr(inner.next().expect("While cond"))?;

            vec![StatNode::Repeat(RepeaStat::new(body, Box::new(cond))).to_stat(pair_span)]
        }
        Rule::LocalFunctionDefStat => {
            let mut inner = pair.into_inner();
            let name = inner.next().expect("Function name").as_str().to_string();

            let mut signature = inner.next().expect("Function param & body").into_inner();
            let next = signature.peek().expect("Function body has something");
            let mut params = FuncParamsBuilder::default();

            let mut fixed = vec![];
            if let Rule::parlist = next.as_rule() {
                let mut parlist = signature.next().unwrap().into_inner();
                let last = parlist.next_back().unwrap();
                if last.as_rule() == Rule::Ellipse {
                    params.is_vararg(true);
                    if let Some(args) = parlist.next() {
                        fixed.append(&mut parse_namelist(args.into_inner()));
                    }
                } else {
                    fixed.append(&mut parse_namelist(last.into_inner()));
                }
            }

            params.fixed(fixed);

            let body = parse_script(&mut signature.next().expect("Function body").into_inner())?;

            vec![StatNode::Assign(AssignStat::new_local(
                vec![(name, None)],
                vec![ExpNode::FuncDef(FuncDef {
                    params: params.build().unwrap(),
                    body: body,
                })],
                true,
            ))
            .to_stat(pair_span)]

            // vec![FunctionDefStat::new_local(name, params.build().unwrap(), body).into()]
        }
        Rule::FunctionDefStat => {
            let mut inner = pair.into_inner();
            let mut names = inner.next().expect("Function name").into_inner();
            let last_name = names.next_back().unwrap();
            let mut namelist = parse_namelist(names);

            let mut method = None;
            if matches!(last_name.as_node_tag(), Some("method")) {
                method = Some(last_name.as_str().to_string());
            }
            namelist.push(last_name.as_str().to_string());

            let mut signature = inner.next().expect("Function param & body").into_inner();
            let next = signature.peek().expect("Function body has something");
            let mut params = FuncParamsBuilder::default();
            let mut fixed = method
                .as_ref()
                .map(|_| vec![String::from("self")])
                .unwrap_or(vec![]);

            if let Rule::parlist = next.as_rule() {
                let mut parlist = signature.next().unwrap().into_inner();
                let last = parlist.next_back().unwrap();
                if last.as_rule() == Rule::Ellipse {
                    params.is_vararg(true);
                    if let Some(args) = parlist.next() {
                        fixed.append(&mut parse_namelist(args.into_inner()));
                    }
                } else {
                    fixed.append(&mut parse_namelist(last.into_inner()));
                }
            }

            params.fixed(fixed);

            let body = parse_script(&mut signature.next().expect("Function body").into_inner())?;

            let access = if namelist.len() > 1 {
                namelist[1..]
                    .iter()
                    .fold(ExpNode::Name(namelist[0].clone()), |acc, val| {
                        ExpNode::Access(ExpAccess {
                            exp: Box::new(acc),
                            prop: Box::new(ExpNode::Literal(LuzObj::str(val))),
                        })
                    })
            } else {
                ExpNode::Name(namelist[0].clone())
            };

            vec![StatNode::Assign(AssignStat::new_normal(
                vec![access],
                vec![ExpNode::FuncDef(FuncDef {
                    params: params.build().unwrap(),
                    body: body,
                })],
            ))
            .to_stat(pair_span)]
            // } else {
            //     vec![
            //         FunctionDefStat::new_normal(namelist, method, params.build().unwrap(), body)
            //             .into(),
            //     ]
            // }
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
                parse_top_expr(inner.next().unwrap())?
            } else {
                ExpNode::Literal(LuzObj::int(1))
            };
            let body = parse_stmts(&mut inner.next().expect("FoRange body").into_inner())?;

            vec![StatNode::ForRange(ForRangeStat::new(
                var,
                Box::new(start),
                Box::new(limit),
                Box::new(step),
                body,
            ))
            .to_stat(pair_span)]
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

            vec![StatNode::ForIn(ForInStat::new(namelist, explist, body)).to_stat(pair_span)]
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
                        else_br = Some(parse_stmts(&mut pair.into_inner())?);
                    }
                    _ => unreachable!(),
                }
            }

            vec![
                StatNode::If(IfStat::new(Box::new(cond), then_br, elseif_brs, else_br))
                    .to_stat(pair_span),
            ]
        }
        Rule::EOI => {
            vec![]
        }
        _ => todo!("Statement Rule::{:?} ({:?})", pair.as_rule(), pair.as_str()),
    })
}
