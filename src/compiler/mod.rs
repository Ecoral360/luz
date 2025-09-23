use derive_new::new;

use crate::{
    ast::{
        AssignStat, Binop, CmpOp, DoStat, ExpAccess, ExpNode, ExpTableConstructor,
        ExpTableConstructorField, ForRangeStat, FuncCall, FuncDef, IfStat, LogicCmpOp, RepeaStat,
        ReturnStat, Stat, StatNode, Unop,
    },
    compiler::{
        ctx::{CompilerCtx, CompilerCtxBuilder, RegOrUpvalue, Upvalue},
        instructions::{iABC, iABx, isJ, Instruction, MAX_HALF_sBx, MAX_HALF_sJ},
        opcode::LuaOpCode,
        visitor::Visitor,
    },
    luz::{
        err::LuzError,
        obj::{LuzObj, Numeral},
    },
};

pub mod ctx;
pub mod instructions;
pub mod opcode;
pub mod visitor;

#[derive(Debug, new)]
pub struct Compiler<'a> {
    input: &'a str,
}

#[allow(unused)]
impl<'a> Compiler<'a> {
    fn collect_insts_and_rollback_stat(
        &mut self,
        ctx: &mut CompilerCtx,
        stats: &Vec<Stat>,
    ) -> Result<Vec<Instruction>, LuzError> {
        let nb_inst_before = ctx.scope().instructions().len();
        let to_be_closed = ctx.get_or_push_free_register();

        for stmt in stats {
            self.visit_stat(stmt, ctx);
        }

        let mut scope = ctx.scope_mut();
        let regs = scope.regs()[to_be_closed as usize..]
            .iter()
            .map(|reg| reg.addr)
            .collect::<Vec<u8>>();

        for reg in regs {
            scope.set_end_of_register(reg);
        }

        Ok(scope
            .instructions_mut()
            .drain(nb_inst_before..)
            .collect::<Vec<_>>())
    }

    fn collect_insts_and_rollback_cond(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
    ) -> Result<Vec<Instruction>, LuzError> {
        let nb_inst_before = ctx.scope().instructions().len();
        let to_be_closed = ctx.get_or_push_free_register();

        self.visit_exp(exp, ctx);

        let mut insts = ctx
            .scope_mut()
            .instructions_mut()
            .drain(nb_inst_before..)
            .collect::<Vec<_>>();

        match exp {
            ExpNode::CmpOp { .. } | ExpNode::LogicCmpOp { .. } => {
                if insts
                    .last()
                    .is_some_and(|inst| inst.op() == LuaOpCode::OP_LOADTRUE)
                {
                    insts.pop();
                    insts.pop();
                    insts.pop();
                    let Instruction::iABC(iABC { k, .. }) = insts.last_mut().unwrap() else {
                        unreachable!()
                    };

                    *k = !*k;
                }
            }
            _ => {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_test(reg, false));
            }
        }

        let mut scope = ctx.scope_mut();
        let regs = scope.regs()[to_be_closed as usize..]
            .iter()
            .map(|reg| reg.addr)
            .collect::<Vec<u8>>();

        for reg in regs {
            scope.set_end_of_register(reg);
        }

        Ok(insts)
    }

    fn handle_immidiate(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
        loadi: bool,
    ) -> Result<Option<u8>, LuzError> {
        match exp {
            ExpNode::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i)) => Ok(Some((*i + 128) as u8)),
                LuzObj::Numeral(Numeral::Int(i))
                    if loadi && (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
                {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                    Ok(None)
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn handle_consecutive_exp(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
    ) -> Result<u8, LuzError> {
        let res = self.handle_exp(ctx, exp, false, false, true)?;
        let reg = ctx.get_or_push_free_register();
        match res {
            ExpEval::InImmediate(_) => {}
            ExpEval::InRegister(r) => {
                if reg != r {
                    ctx.push_inst(Instruction::op_move(reg, r))
                }
            }
            ExpEval::InConstant(_) => {}
            ExpEval::InUpvalue { in_table, upvalue } => {
                let ExpNode::Name(name) = exp else {
                    unreachable!()
                };
                if in_table {
                    let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                    ctx.push_inst(Instruction::op_gettabup(reg, upvalue.addr, name_k as u8));
                } else {
                    ctx.push_inst(Instruction::op_getupval(reg, upvalue.addr));
                }
            }
        }
        Ok(reg)
    }

    fn handle_exp(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
        supports_immidiate: bool,
        supports_constants: bool,
        loadi: bool,
    ) -> Result<ExpEval, LuzError> {
        match exp {
            ExpNode::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i))
                    if supports_immidiate && (-128..128).contains(i) =>
                {
                    Ok(ExpEval::InImmediate((*i + 128) as u8))
                }
                LuzObj::Numeral(Numeral::Float(f))
                    if supports_immidiate
                        && f.floor() == *f
                        && (-128..128).contains(&(*f as i32)) =>
                {
                    Ok(ExpEval::InImmediate((*f + 128.0) as u8))
                }
                LuzObj::Numeral(Numeral::Int(i))
                    if loadi && (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
                {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                    Ok(ExpEval::InRegister(reg))
                }
                LuzObj::Numeral(Numeral::Float(f))
                    if loadi
                        && f.floor() == *f
                        && (-(MAX_HALF_sBx as f64)..(MAX_HALF_sBx as f64)).contains(f) =>
                {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadf(reg, *f));
                    Ok(ExpEval::InRegister(reg))
                }
                LuzObj::Nil => {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadnil(reg, 0));
                    Ok(ExpEval::InRegister(reg))
                }
                LuzObj::Boolean(b) if !supports_constants => {
                    let reg = ctx.get_or_push_free_register();
                    if *b {
                        ctx.push_inst(Instruction::op_loadtrue(reg));
                    } else {
                        ctx.push_inst(Instruction::op_loadfalse(reg));
                    }
                    Ok(ExpEval::InRegister(reg))
                }
                // LuzObj::String(name) => {
                //     let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                //     match src {
                //         RegOrUpvalue::Register(register) => Ok(ExpEval::InRegister(register.addr)),
                //         RegOrUpvalue::Upvalue(upvalue) => {
                //             Ok(ExpEval::InUpvalue { in_table, upvalue })
                //         }
                //     }
                // }
                _ => {
                    if supports_constants {
                        Ok(ExpEval::InConstant(ctx.get_or_add_const(lit.clone()) as u8))
                    } else {
                        let constant = ctx.get_or_add_const(lit.clone());
                        let reg = ctx.get_or_push_free_register();
                        ctx.push_inst(Instruction::op_loadk(reg, constant));
                        Ok(ExpEval::InRegister(reg))
                    }
                }
            },
            ExpNode::Name(name) => {
                let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                match src {
                    RegOrUpvalue::Register(register) => Ok(ExpEval::InRegister(register.addr)),
                    RegOrUpvalue::Upvalue(upvalue) => Ok(ExpEval::InUpvalue { in_table, upvalue }),
                }
            }
            _ => {
                self.visit_exp(exp, ctx)?;
                let reg = ctx.get_or_push_free_register();
                Ok(ExpEval::InRegister(reg))
            }
        }
    }

    fn load_exp(&mut self, ctx: &mut CompilerCtx, exp: &ExpNode) -> Result<u8, LuzError> {
        match exp {
            ExpNode::Name(name) => {
                let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                match src {
                    RegOrUpvalue::Register(register) => Ok(register.addr),
                    RegOrUpvalue::Upvalue(upvalue) => {
                        let reg = ctx.get_or_push_free_register();
                        if in_table {
                            let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                            ctx.push_inst(Instruction::op_gettabup(
                                reg,
                                upvalue.addr,
                                name_k as u8,
                            ));
                        } else {
                            ctx.push_inst(Instruction::op_getupval(reg, upvalue.addr));
                        }
                        Ok(reg)
                    }
                }
            }
            _ => {
                self.visit_exp(exp, ctx)?;
                Ok(ctx.get_or_push_free_register())
            }
        }
    }

    fn load_exp_or_const(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
    ) -> Result<(bool, u8), LuzError> {
        match exp {
            ExpNode::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i))
                    if (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
                {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                    Ok((false, reg))
                }
                LuzObj::Nil => {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadnil(reg, 0));
                    Ok((false, reg))
                }
                _ => {
                    let reg = ctx.get_or_push_free_register();
                    let addrk = ctx.get_or_add_const(lit.clone());
                    ctx.push_inst(Instruction::op_loadk(reg, addrk));
                    Ok((false, reg))
                }
            },
            ExpNode::Name(name) => {
                let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                match src {
                    RegOrUpvalue::Register(register) => Ok((false, register.addr)),
                    RegOrUpvalue::Upvalue(upvalue) => {
                        let reg = ctx.get_or_push_free_register();
                        if in_table {
                            let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                            ctx.push_inst(Instruction::op_gettabup(
                                reg,
                                upvalue.addr,
                                name_k as u8,
                            ));
                        } else {
                            ctx.push_inst(Instruction::op_getupval(reg, upvalue.addr));
                        }
                        Ok((false, reg))
                    }
                }
            }
            _ => {
                self.visit_exp(exp, ctx)?;
                let reg = ctx.get_or_push_free_register();
                Ok((false, reg))
            }
        }
    }

    fn assign_to_table(
        &mut self,
        ctx: &mut CompilerCtx,
        tabaddr: u8,
        prop: &ExpNode,
        value: &ExpNode,
    ) -> Result<(), LuzError> {
        let value = self.handle_exp(ctx, value, false, true, false)?;
        let (is_val_const, val_reg) = match value {
            ExpEval::InRegister(reg) => (false, reg),
            ExpEval::InConstant(kreg) => (true, kreg),
            ExpEval::InUpvalue { in_table, upvalue } => todo!(),
            _ => unreachable!("Not supported by op_settabup {:?}", value),
        };

        let prop_imm = self.handle_immidiate(ctx, &prop, true)?;
        if let Some(imm) = prop_imm {
            ctx.push_inst(Instruction::op_seti(tabaddr, imm, val_reg, is_val_const));
        } else {
            match prop {
                ExpNode::Literal(LuzObj::String(ref n)) => {
                    let attr = ctx.get_or_add_const(LuzObj::String(n.clone()));
                    ctx.push_inst(Instruction::op_setfield(
                        tabaddr,
                        attr as u8,
                        val_reg,
                        is_val_const,
                    ));
                }
                _ => {
                    self.visit_exp(&prop, ctx)?;
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_settable(
                        tabaddr,
                        reg,
                        val_reg,
                        is_val_const,
                    ));
                }
            }
        }

        ctx.unclaim_registers(&[tabaddr]);
        Ok(())
    }

    fn concat_branches(
        &self,
        has_cmp: bool,
        is_left_cmp: bool,
        mut left: Vec<Instruction>,
        op: LogicCmpOp,
        right: Vec<Instruction>,
    ) -> Vec<Instruction> {
        let last_jmp = &left.last().and_then(|inst| {
            if inst.op() == LuaOpCode::OP_JMP {
                Some(left.len() - 1)
            } else {
                None
            }
        });
        let offset = if has_cmp {
            if is_left_cmp {
                0
            } else {
                2
            }
        } else {
            if op == LogicCmpOp::Or {
                -1
            } else {
                0
            }
        };
        match last_jmp {
            Some(jmp_idx) => {
                left[*jmp_idx] = Instruction::op_jmp((right.len() as isize + offset) as i32)
            }
            None => {
                if right.len() > 0 {
                    left.push(Instruction::op_jmp((right.len() as isize + offset) as i32));
                }
            }
        }
        let mut final_insts = vec![];
        for inst in left {
            final_insts.push(inst);
        }
        for inst in right {
            final_insts.push(inst);
        }

        final_insts
    }

    fn handle_multires(
        &mut self,
        ctx: &mut CompilerCtx,
        explist: &Vec<ExpNode>,
        nb_expected: u8,
    ) -> Result<Vec<u8>, LuzError> {
        let mut new_ctx = ctx.new_with(CompilerCtxBuilder::default().nb_expected(2));
        let mut claimed = vec![];

        if explist.is_empty() {
            return Ok(claimed);
        }

        for exp in &explist[..explist.len() - 1] {
            self.visit_exp(exp, &mut new_ctx)?;
            claimed.push(ctx.claim_next_free_register());
        }

        let mut last_ctx = if ExpNode::is_multires(explist) {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected(0))
        } else {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected(nb_expected + 2))
        };

        self.visit_exp(&explist[explist.len() - 1], &mut last_ctx)?;
        claimed.push(ctx.claim_next_free_register());

        Ok(claimed)
    }

    pub fn input(&self) -> &'a str {
        self.input
    }
}

#[derive(Debug, Clone)]
enum ExpEval {
    InImmediate(u8),
    InRegister(u8),
    InConstant(u8),
    InUpvalue { in_table: bool, upvalue: Upvalue },
}

#[allow(unused)]
impl<'a> Compiler<'a> {
    fn visit_do_stat(&mut self, ctx: &mut CompilerCtx, do_stat: &DoStat) -> Result<(), LuzError> {
        let DoStat { block } = do_stat;
        let to_be_closed = ctx.get_or_push_free_register();
        for stmt in block {
            self.visit_stat(stmt, ctx);
        }

        ctx.push_inst(Instruction::op_close(to_be_closed));

        let mut scope = ctx.scope_mut();
        let regs = scope.regs()[to_be_closed as usize..]
            .iter()
            .map(|reg| reg.addr)
            .collect::<Vec<u8>>();

        for reg in regs {
            scope.set_end_of_register(reg);
        }

        Ok(())
    }

    fn visit_if(&mut self, ctx: &mut CompilerCtx, if_stat: &IfStat) -> Result<(), LuzError> {
        let IfStat {
            cond,
            then_br,
            elseif_brs,
            else_br,
        } = if_stat;

        // self.visit_exp(cond, ctx);

        // let rhs_len = rhs_instructions.len();
        let mut branches = vec![];

        let cond = self.collect_insts_and_rollback_cond(ctx, cond)?;

        let then_instrs = self.collect_insts_and_rollback_stat(ctx, then_br)?;
        branches.push((cond, then_instrs));

        for (cond, elseif_br) in elseif_brs {
            let cond_br = self.collect_insts_and_rollback_cond(ctx, cond)?;
            let br = self.collect_insts_and_rollback_stat(ctx, elseif_br)?;
            branches.push((cond_br, br));
        }

        let else_instrs = if let Some(else_br) = else_br {
            let else_instrs = self.collect_insts_and_rollback_stat(ctx, else_br)?;
            else_instrs
        } else {
            vec![]
        };

        let to_else_jmp = branches
            .iter()
            .fold(0, |acc, (cond, br)| acc + cond.len() + br.len() + 1);

        let to_end_jmp = to_else_jmp + else_instrs.len();

        let mut insts = branches
            .into_iter()
            .rfold(vec![], |mut insts, (cond, branch)| {
                for inst in cond {
                    insts.push(inst);
                }
                let offset = insts.len();
                insts.push(Instruction::op_jmp((to_else_jmp - offset) as i32));

                for inst in branch {
                    insts.push(inst);
                }
                let offset = insts.len();
                insts.push(Instruction::op_jmp((to_end_jmp - offset) as i32));

                insts
            });

        for inst in else_instrs {
            insts.push(inst);
        }

        for inst in insts {
            ctx.push_inst(inst);
        }

        Ok(())
    }

    fn visit_for_range(
        &mut self,
        ctx: &mut CompilerCtx,
        for_range_stat: &ForRangeStat,
    ) -> Result<(), LuzError> {
        let ForRangeStat {
            var,
            start,
            limit,
            step,
            block,
        } = for_range_stat;

        let to_be_closed = ctx.get_or_push_free_register();

        self.visit_exp(start, ctx);
        let forloop_test = ctx.claim_next_free_register();

        self.visit_exp(limit, ctx);
        let forloop_stop = ctx.claim_next_free_register();

        self.visit_exp(step, ctx);
        let forloop_step = ctx.claim_next_free_register();

        ctx.push_inst(Instruction::op_forprep(forloop_test, 0));

        ctx.unclaim_registers(&[forloop_test, forloop_stop, forloop_step]);

        let nb_inst_before = ctx.instructions_len();
        let forprep_idx = nb_inst_before - 1;

        let forloop_test = ctx
            .rename_or_push_free_register_with_start(String::from("(for state)"), nb_inst_before);
        let forloop_stop = ctx
            .rename_or_push_free_register_with_start(String::from("(for state)"), nb_inst_before);
        let forloop_step = ctx
            .rename_or_push_free_register_with_start(String::from("(for state)"), nb_inst_before);

        ctx.claim_register(forloop_test);
        ctx.claim_register(forloop_stop);
        ctx.claim_register(forloop_step);

        let ctrl = ctx.rename_or_push_free_register_with_start(var.to_string(), nb_inst_before + 1);
        ctx.claim_register(ctrl);

        // Exec the block

        for stmt in block {
            self.visit_stat(stmt, ctx);
        }

        {
            let mut scope = ctx.scope_mut();
            let regs = scope.regs()[to_be_closed as usize..]
                .iter()
                .map(|reg| reg.addr)
                .collect::<Vec<u8>>();

            for reg in regs {
                scope.set_end_of_register(reg);
            }
        }

        let nb_inst_loop = ctx.instructions_len() - nb_inst_before;

        {
            let mut scope = ctx.scope_mut();
            let Instruction::iABx(iABx { b, a, op }) = &mut scope.instructions_mut()[forprep_idx]
            else {
                unreachable!()
            };

            *b = nb_inst_loop as u32;
        }

        // Add the FORLOOP instruction
        ctx.push_inst(Instruction::op_forloop(forloop_test, nb_inst_loop as u32 + 1));

        Ok(())
    }

    fn visit_repeat_until(
        &mut self,
        ctx: &mut CompilerCtx,
        stat: &RepeaStat,
    ) -> Result<(), LuzError> {
        let RepeaStat { block, cond } = stat;

        let nb_inst_before = ctx.scope().instructions().len();

        for stmt in block {
            self.visit_stat(stmt, ctx);
        }

        self.visit_exp(cond, ctx);

        match **cond {
            ExpNode::CmpOp { .. } | ExpNode::LogicCmpOp { .. } => {
                if ctx
                    .scope()
                    .instructions()
                    .last()
                    .is_some_and(|inst| inst.op() == LuaOpCode::OP_LOADTRUE)
                {
                    let mut scope = ctx.scope_mut();
                    let insts = scope.instructions_mut();
                    insts.pop();
                    insts.pop();
                    insts.pop();

                    let Instruction::iABC(iABC { k, .. }) = insts.last_mut().unwrap() else {
                        unreachable!()
                    };

                    *k = !*k;
                }
            }
            _ => {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_test(reg, false));
            }
        }

        let diff = nb_inst_before as i32 - ctx.scope().instructions().len() as i32;

        ctx.push_inst(Instruction::op_jmp(diff));

        Ok(())
    }

    fn visit_normal_assign(
        &mut self,
        ctx: &mut CompilerCtx,
        assign: &AssignStat,
    ) -> Result<(), LuzError> {
        let AssignStat::Normal { varlist, explist } = assign else {
            unreachable!()
        };
        let mut varlist_iter = varlist.iter();

        let mut one_exp_ctx = ctx.new_with(CompilerCtxBuilder::default().nb_expected(2));
        let mut claimed = vec![];

        let mut var_claimed = vec![];
        let mut var_exp_accesses = vec![];

        for var_exp in varlist.iter() {
            let var = if let ExpNode::Var(var) = var_exp {
                var
            } else {
                var_exp
            };
            match var {
                ExpNode::Name(ref name) => {
                    let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                    match src {
                        RegOrUpvalue::Register(src) => {
                            ctx.unclaim_registers(&[src.addr]);
                        }
                        RegOrUpvalue::Upvalue(src) => {
                            // if is_intable {
                            //     let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                            //
                            //     ctx.push_inst(Instruction::op_settabup(
                            //         src.addr,
                            //         name_k as u8,
                            //         dest,
                            //         is_const,
                            //     ));
                            // } else {
                            //     ctx.push_inst(Instruction::op_setupval(src.addr, dest));
                            // }
                        }
                    }
                }
                ExpNode::Access(ref exp_access) => {
                    let ExpAccess { exp: obj, prop } = exp_access;

                    let res = self.handle_exp(&mut one_exp_ctx, &obj, false, true, false)?;
                    var_claimed.push(ctx.claim_next_free_register());
                    var_exp_accesses.push(res);
                }
                _ => {}
            }
        }

        if !explist.is_empty() {
            for exp in &explist[..explist.len() - 1] {
                let dest = self.load_exp_or_const(&mut one_exp_ctx, exp)?;
                if !dest.0 {
                    ctx.claim_register(dest.1);
                }
                claimed.push(dest);
            }

            let nb_last_expected = varlist.len().checked_sub(explist.len() - 1).unwrap_or(0);
            let mut last_exp_ctx =
                ctx.new_with(CompilerCtxBuilder::default().nb_expected(nb_last_expected as u8 + 1));

            let dest = self.load_exp_or_const(&mut last_exp_ctx, explist.last().unwrap())?;
            if !dest.0 {
                ctx.claim_register(dest.1);
            }

            claimed.push(dest);
            // for expected in 0..nb_last_expected {
            //     claimed.push((false, ctx.claim_next_free_register()));
            // }
        }

        ctx.unclaim_registers(&var_claimed);

        ctx.unclaim_registers(
            &claimed
                .iter()
                .filter_map(|(is_const, reg)| if *is_const { None } else { Some(*reg) })
                .collect::<Vec<_>>(),
        );

        let mut var_exp_accesses_iter = var_exp_accesses.iter();

        for (var_exp, (is_const, mut dest)) in varlist.iter().rev().zip(claimed.into_iter().rev()) {
            // let is_const = false;
            // if !is_const {
            //     dest = ctx.claim_next_free_register();
            // }
            let var = if let ExpNode::Var(var) = var_exp {
                var
            } else {
                var_exp
            };

            match var {
                ExpNode::Name(ref name) => {
                    let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                    match src {
                        RegOrUpvalue::Register(src) => {
                            ctx.claim_register(src.addr);
                            if src.addr != dest {
                                ctx.push_inst(Instruction::op_move(src.addr, dest));
                            }
                        }
                        RegOrUpvalue::Upvalue(src) => {
                            if is_intable {
                                let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));

                                ctx.push_inst(Instruction::op_settabup(
                                    src.addr,
                                    name_k as u8,
                                    dest,
                                    is_const,
                                ));
                            } else {
                                ctx.push_inst(Instruction::op_setupval(src.addr, dest));
                            }
                        }
                    }
                }
                ExpNode::Access(ref exp_access) => {
                    let ExpAccess { exp: obj, prop } = exp_access;

                    let tabaddr = ctx.get_or_push_free_register();
                    let res = var_exp_accesses_iter.next().unwrap(); //self.handle_exp(&mut one_exp_ctx, &obj, false, true, false)?;

                    let prop_imm = self.handle_immidiate(&mut one_exp_ctx, &prop, true)?;
                    if let Some(imm) = prop_imm {
                        ctx.push_inst(Instruction::op_seti(tabaddr, imm, dest, is_const));
                    } else {
                        match res {
                            ExpEval::InUpvalue { in_table, upvalue } => {
                                let prop = match **prop {
                                    ExpNode::Literal(LuzObj::String(ref n)) => {
                                        ctx.get_or_add_const(LuzObj::String(n.clone())) as u8
                                    }
                                    _ => {
                                        self.visit_exp(&prop, &mut one_exp_ctx)?;
                                        ctx.get_or_push_free_register()
                                    }
                                };

                                ctx.push_inst(Instruction::op_settabup(
                                    upvalue.addr,
                                    prop,
                                    dest,
                                    is_const,
                                ));
                                return Ok(());
                            }

                            ExpEval::InRegister(r) => {
                                match **prop {
                                    ExpNode::Literal(LuzObj::String(ref n)) => {
                                        let attr = ctx.get_or_add_const(LuzObj::String(n.clone()));
                                        ctx.push_inst(Instruction::op_setfield(
                                            *r, attr as u8, dest, is_const,
                                        ));
                                    }
                                    _ => {
                                        // let reg = ctx.get_or_push_free_register();
                                        let reg = self.load_exp(&mut one_exp_ctx, &prop)?;
                                        ctx.push_inst(Instruction::op_settable(
                                            *r, reg, dest, is_const,
                                        ));
                                    }
                                };
                            }
                            ExpEval::InConstant(_) => todo!(),
                            ExpEval::InImmediate(_) => todo!(),
                        }
                    }
                }
                _ => todo!("Assign normal: {:#?}", var),
            }
        }

        Ok(())
    }

    fn visit_assign(&mut self, ctx: &mut CompilerCtx, assign: &AssignStat) -> Result<(), LuzError> {
        match assign {
            AssignStat::Normal { .. } => self.visit_normal_assign(ctx, assign)?,
            AssignStat::Local {
                varlist,
                explist,
                closure,
            } => {
                let mut var_addrs = vec![];
                for var in varlist {
                    let var_addr = ctx.rename_or_push_free_register(var.0.clone());
                    if *closure {
                        ctx.set_register_start(var_addr, var.0.clone());
                    }
                    var_addrs.push(var_addr);
                }

                let mut one_exp_ctx = ctx.new_with(CompilerCtxBuilder::default().nb_expected(2));
                let mut claimed = vec![];

                if !explist.is_empty() {
                    let nb_last_expected =
                        varlist.len().checked_sub(explist.len() - 1).unwrap_or(0);
                    let mut last_exp_ctx = ctx.new_with(
                        CompilerCtxBuilder::default().nb_expected(nb_last_expected as u8 + 1),
                    );

                    for exp in &explist[..explist.len() - 1] {
                        let reg = self.handle_consecutive_exp(&mut one_exp_ctx, exp)?;
                        ctx.claim_register(reg);
                        claimed.push(reg);
                    }

                    let reg = self
                        .handle_consecutive_exp(&mut last_exp_ctx, &explist[explist.len() - 1])?;
                    ctx.claim_register(reg);
                    claimed.push(reg);
                    if nb_last_expected > 0 {
                        for expected in 0..nb_last_expected - 1 {
                            claimed.push(ctx.claim_next_free_register());
                        }
                    }
                } else {
                    let mut is_compatible = false;
                    {
                        let mut scope = ctx.scope_mut();
                        if let Some(last_inst) = scope.instructions_mut().last_mut() {
                            if last_inst.op() == LuaOpCode::OP_LOADNIL {
                                let Instruction::iABx(iABx { b, a, op }) = last_inst else {
                                    unreachable!()
                                };
                                if *a as u32 + *b + 1 == var_addrs[0] as u32 {
                                    is_compatible = true;
                                    *b += varlist.len() as u32;
                                }
                            }
                        }
                    }
                    if is_compatible {
                        for reg in var_addrs[0..].iter() {
                            ctx.claim_register(*reg);
                        }
                        return Ok(());
                    }
                }

                if varlist.len() > explist.len() && !ExpNode::is_multires(explist) {
                    let reg = var_addrs[0];
                    let diff = varlist.len() - explist.len();
                    ctx.push_inst(Instruction::op_loadnil(reg, diff as u32 - 1));

                    for reg in var_addrs[explist.len()..].iter() {
                        ctx.claim_register(*reg);
                    }
                }
                for (var, addr) in varlist.iter().zip(var_addrs) {
                    ctx.set_register_start(addr, var.0.clone());
                }
            }
        }
        Ok(())
    }

    fn visit_return(&mut self, ctx: &mut CompilerCtx, stat: &ReturnStat) -> Result<(), LuzError> {
        let start = ctx.get_or_push_free_register();
        let size = stat.explist.len();
        let mut new_ctx = if ExpNode::is_multires(&stat.explist) {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected(0))
        } else {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected((size + 1) as u8))
        };

        match stat.explist.len() {
            0 => {
                ctx.push_inst(Instruction::op_return0());
            }
            1 => {
                let reg = self.load_exp(ctx, &stat.explist[0])?;
                ctx.push_inst(Instruction::op_return1(reg));
            }
            // _ if Exp::is_multires(&stat.explist) => {
            //     ctx.push_inst(Instruction::op_return(start, false, size as u8 + 1));
            // }
            _ => {
                for exp in stat.explist.iter() {
                    self.handle_consecutive_exp(ctx, exp)?;
                    ctx.claim_next_free_register();
                }
                ctx.push_inst(Instruction::op_return(start, false, size as u8 + 1));
            }
        }

        Ok(())
    }

    fn visit_binop(&mut self, ctx: &mut CompilerCtx, exp: &ExpNode) -> Result<(), LuzError> {
        let ExpNode::Binop { op, lhs, rhs } = exp else {
            unreachable!()
        };
        if *op == Binop::Concat {
            self.handle_consecutive_exp(ctx, lhs)?;
            let lhs_addr = ctx.claim_next_free_register();

            self.handle_consecutive_exp(ctx, rhs)?;
            let rhs_addr = ctx.get_or_push_free_register();

            ctx.push_inst(Instruction::op_concat(lhs_addr, rhs_addr - lhs_addr + 1));
            ctx.unclaim_registers(&[lhs_addr]);

            return Ok(());
        }

        let result_reg = ctx.claim_next_free_register();

        let lhs_addr = self.handle_exp(
            ctx,
            lhs,
            matches!(op, Binop::Add | Binop::ShiftLeft),
            true,
            !matches!(op, Binop::BitAnd | Binop::BitXor | Binop::BitOr),
        )?;

        ctx.unclaim_registers(&[result_reg]);

        let is_b_const = matches!(lhs_addr, ExpEval::InConstant(_));
        let is_b_imm = matches!(lhs_addr, ExpEval::InImmediate(_));

        let rhs_addr = self.handle_exp(
            ctx,
            rhs,
            matches!(op, Binop::Add | Binop::Sub | Binop::ShiftLeft),
            true,
            !matches!(op, Binop::BitAnd | Binop::BitXor | Binop::BitOr),
        )?;

        let is_c_const = matches!(rhs_addr, ExpEval::InConstant(_));
        let is_c_imm = matches!(rhs_addr, ExpEval::InImmediate(_));

        if is_b_imm && is_c_imm {
            return Err(LuzError::CompileError(format!(
                "Both sides of {:?} cannot be immediate values, should have been optimized away before codegen",
                op
            )));
        }

        let (lhs_dirty, mut lhs_addr) = match lhs_addr {
            ExpEval::InImmediate(i) => (false, i),
            ExpEval::InRegister(r) => (false, r),
            ExpEval::InConstant(c) => (false, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                self.visit_exp(lhs, ctx)?;
                (true, ctx.claim_next_free_register())
            }
        };

        let (rhs_dirty, mut rhs_addr) = match rhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Sub => {
                // 128 - (i - 128) transforms the value from LHS - RHS to LHS + -RHS
                // this allows us to use the ADD_I op code instead of a SUB instruction,
                // which would need us to store the negative number as a constant
                (false, 128 - (i - 128))
            }
            ExpEval::InImmediate(i) => (false, i),
            ExpEval::InRegister(r) => (false, r),
            ExpEval::InConstant(c) => (false, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                self.visit_exp(rhs, ctx)?;
                (true, ctx.claim_next_free_register())
            }
        };

        if lhs_dirty {
            ctx.unclaim_registers(&[lhs_addr]);
        }
        if rhs_dirty {
            ctx.unclaim_registers(&[rhs_addr]);
        }

        if is_b_imm {
            (lhs_addr, rhs_addr) = (rhs_addr, lhs_addr);
        }

        if is_c_imm || is_b_imm {
            match op {
                Binop::ShiftLeft => {
                    if is_c_imm {
                        ctx.push_inst(Instruction::op_shri(
                            result_reg,
                            lhs_addr,
                            false,
                            128 - (rhs_addr - 128),
                        ));
                    } else {
                        ctx.push_inst(Instruction::op_shli(result_reg, lhs_addr, false, rhs_addr));
                    }
                }
                Binop::ShiftRight => {
                    ctx.push_inst(Instruction::op_shri(result_reg, lhs_addr, false, rhs_addr));
                }
                Binop::Add | Binop::Sub => {
                    ctx.push_inst(Instruction::op_addi(result_reg, lhs_addr, false, rhs_addr));
                }
                _ => unreachable!(),
            }
            ctx.push_inst(Instruction::op_mmbini(lhs_addr, rhs_addr, 6, is_b_imm));
        } else {
            ctx.push_inst(Instruction::op_arithmetic(
                *op, result_reg, lhs_addr, is_b_const, rhs_addr, is_c_const,
            ));
            if is_c_const {
                ctx.push_inst(Instruction::op_mmbink(lhs_addr, rhs_addr, 6, false));
            } else if is_b_const {
                ctx.push_inst(Instruction::op_mmbink(rhs_addr, lhs_addr, 6, true));
            } else {
                ctx.push_inst(Instruction::op_mmbin(lhs_addr, rhs_addr, 6));
            }
        }
        Ok(())
    }

    fn visit_access(
        &mut self,
        ctx: &mut CompilerCtx,
        exp_access: &ExpAccess,
    ) -> Result<(), LuzError> {
        let ExpAccess { exp, prop: value } = exp_access;
        let dest = ctx.get_or_push_free_register();

        let tabaddr = self.load_exp(ctx, exp)?;
        ctx.claim_register(tabaddr);

        let prop = self.handle_immidiate(ctx, value, false)?;
        if let Some(imm) = prop {
            ctx.push_inst(Instruction::op_geti(dest, tabaddr, imm));
        } else {
            match **value {
                ExpNode::Literal(LuzObj::String(ref n)) => {
                    let attr = ctx.get_or_add_const(LuzObj::String(n.clone()));
                    ctx.push_inst(Instruction::op_getfield(dest, tabaddr, attr as u8));
                }
                _ => {
                    self.visit_exp(value, ctx)?;
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_gettable(dest, tabaddr, reg));
                }
            }
        }

        ctx.unclaim_registers(&[dest]);
        Ok(())
    }

    fn visit_logicop(&mut self, ctx: &mut CompilerCtx, exp: &ExpNode) -> Result<(), LuzError> {
        let ExpNode::LogicCmpOp { op, lhs, rhs } = exp else {
            unreachable!()
        };
        let is_and = matches!(op, LogicCmpOp::And);

        let jmps = self.visit_inner_logicop(ctx, exp, 0)?;
        Ok(())
    }

    fn visit_inner_logicop(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &ExpNode,
        depth: usize,
    ) -> Result<Vec<(u32, Option<bool>, LogicCmpOp)>, LuzError> {
        let ExpNode::LogicCmpOp { op, lhs, rhs } = exp else {
            unreachable!()
        };

        let is_and = matches!(op, LogicCmpOp::And);

        let nb_inst_before = ctx.scope().instructions().len();

        let mut jumps = vec![];
        let mut post_cmp = vec![];

        match lhs.normalize() {
            ExpNode::LogicCmpOp { op: inner_op, .. } => {
                let lhs_jumps = self.visit_inner_logicop(ctx, lhs, depth + 1)?;
                // We pop the LFALSEKIP and LOADTRUE
                if lhs_jumps.iter().any(|(_, is_cmp, _)| is_cmp.is_some()) {
                    post_cmp = vec![
                        ctx.pop_instruction().unwrap(),
                        ctx.pop_instruction().unwrap(),
                    ];
                    post_cmp.reverse();
                }
                for jmp in lhs_jumps {
                    jumps.push(jmp);
                }
            }
            ExpNode::Name(ref name) => {
                let dest = ctx.get_or_push_free_register();
                let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                let val = match src {
                    RegOrUpvalue::Register(register) => register.addr,
                    RegOrUpvalue::Upvalue(upvalue) => {
                        let reg = ctx.get_or_push_free_register();
                        if in_table {
                            let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                            ctx.push_inst(Instruction::op_gettabup(
                                reg,
                                upvalue.addr,
                                name_k as u8,
                            ));
                        } else {
                            ctx.push_inst(Instruction::op_getupval(reg, upvalue.addr));
                        }
                        reg
                    }
                };
                if dest != val {
                    ctx.push_inst(Instruction::op_testset(dest, val, !is_and));
                }
                jumps.push((ctx.instructions_len() as u32, None, *op));
                ctx.push_inst(Instruction::op_jmp(1));
            }
            ExpNode::CmpOp { .. } => {
                self.visit_exp(lhs, ctx)?;
                post_cmp = vec![
                    ctx.pop_instruction().unwrap(),
                    ctx.pop_instruction().unwrap(),
                ];
                post_cmp.reverse();

                let len = ctx.instructions_len();
                let mut scope_mut = ctx.scope_mut();
                let Instruction::iABC(iABC { k, .. }) =
                    scope_mut.instructions_mut().get_mut(len - 2).unwrap()
                else {
                    unreachable!();
                };

                if is_and {
                    *k = !*k;
                }
                jumps.push((len as u32 - 1, Some(*k), *op));
            }
            _ => {
                self.visit_exp(lhs, ctx)?;
                let lhs_test_addr = ctx.get_or_push_free_register();
                let inv_test = if matches!(
                    **lhs,
                    ExpNode::Literal(LuzObj::Boolean(false) | LuzObj::Nil)
                ) {
                    is_and
                } else {
                    !is_and
                };
                ctx.push_inst(Instruction::op_test(lhs_test_addr, inv_test));
                jumps.push((ctx.instructions_len() as u32, None, *op));
                ctx.push_inst(Instruction::op_jmp(1));
            }
        }

        match rhs.normalize() {
            ExpNode::CmpOp { .. } => {
                self.visit_exp(rhs, ctx)?;

                post_cmp = vec![
                    ctx.pop_instruction().unwrap(),
                    ctx.pop_instruction().unwrap(),
                ];
                post_cmp.reverse();

                let len = ctx.instructions_len();
                let mut scope_mut = ctx.scope_mut();
                let Instruction::iABC(iABC { k, .. }) =
                    scope_mut.instructions_mut().get_mut(len - 2).unwrap()
                else {
                    unreachable!();
                };

                if depth != 0 {
                    if is_and {
                        *k = !*k;
                    }
                    jumps.push((len as u32 - 1, Some(*k), *op));
                }
            }
            ExpNode::Name(ref name) => {
                let dest = ctx.get_or_push_free_register();
                let (in_table, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                let val = match src {
                    RegOrUpvalue::Register(register) => register.addr,
                    RegOrUpvalue::Upvalue(upvalue) => {
                        let reg = ctx.get_or_push_free_register();
                        if in_table {
                            let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                            ctx.push_inst(Instruction::op_gettabup(
                                reg,
                                upvalue.addr,
                                name_k as u8,
                            ));
                        } else {
                            ctx.push_inst(Instruction::op_getupval(reg, upvalue.addr));
                        }
                        reg
                    }
                };
                if dest != val {
                    if depth != 0 {
                        ctx.push_inst(Instruction::op_testset(dest, val, is_and));
                        jumps.push((ctx.instructions_len() as u32, None, *op));
                        ctx.push_inst(Instruction::op_jmp(1));
                    } else {
                        ctx.push_inst(Instruction::op_move(dest, val));
                    }
                }
            }
            _ => {
                self.visit_exp(rhs, ctx)?;

                if depth != 0 {
                    let rhs_last_inst = {
                        if ctx
                            .instructions()
                            .last()
                            .is_some_and(|inst| inst.op() == LuaOpCode::OP_MOVE)
                        {
                            ctx.pop_instruction()
                        } else {
                            None
                        }
                    };
                    let inv_test = if matches!(
                        **lhs,
                        ExpNode::Literal(LuzObj::Boolean(false) | LuzObj::Nil)
                    ) {
                        !is_and
                    } else {
                        is_and
                    };
                    if let Some(last_move) = rhs_last_inst {
                        let Instruction::iABC(iABC { c, b, k, a, op }) = last_move else {
                            unreachable!()
                        };
                        let val = b;
                        let dest = a;
                        ctx.push_inst(Instruction::op_testset(dest, val, inv_test));
                    } else {
                        let reg = ctx.get_or_push_free_register();
                        ctx.push_inst(Instruction::op_test(reg, inv_test));
                    }
                    jumps.push((ctx.instructions_len() as u32, None, *op));
                    ctx.push_inst(Instruction::op_jmp(1));
                }
            }
        }

        let has_cmp = !post_cmp.is_empty();
        if depth == 0
            && has_cmp
            && ctx
                .instructions()
                .last()
                .is_some_and(|inst| inst.op() != LuaOpCode::OP_JMP)
        {
            ctx.push_inst(Instruction::op_jmp(2));
        }

        let len = ctx.instructions_len() - 1;
        for (idx, (jmp_addr, is_cmp, jmp_op)) in jumps.iter().enumerate() {
            let mut scope_mut = ctx.scope_mut();
            let instr_mut = scope_mut.instructions_mut();
            let Instruction::isJ(isJ { b, .. }) = &mut instr_mut[*jmp_addr as usize] else {
                unreachable!()
            };
            *b = if *jmp_op == LogicCmpOp::And || idx == jumps.len() - 1 {
                let next = jumps[idx + 1..]
                    .iter()
                    .find_map(|(idx, _, op)| {
                        if *op == LogicCmpOp::Or {
                            Some(*idx as usize)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(len);
                next as u32 - *jmp_addr + MAX_HALF_sJ
            } else {
                jumps[idx + 1].0 - *jmp_addr + MAX_HALF_sJ
            };
            if let Some(cmp) = is_cmp {
                if *cmp {
                    *b += 1;
                }
            } else if has_cmp {
                *b += 2;
            }
        }

        for inst in post_cmp {
            ctx.push_inst(inst);
        }
        // if let [Instruction::iABC(iABC { k, .. }), Instruction::isJ(isJ { b, .. })] =
        //     &mut ctx.instructions()[len..]
        // {
        //     if *b == MAX_HALF_sJ {
        //         *b += 1;
        //         if is_and {
        //             *k = !*k;
        //         }
        //     }
        // }
        Ok(jumps)
    }

    fn visit_cmpop(&mut self, ctx: &mut CompilerCtx, exp: &ExpNode) -> Result<(), LuzError> {
        let ExpNode::CmpOp { mut op, lhs, rhs } = exp else {
            unreachable!()
        };

        let lhs_result = ctx.get_or_push_free_register();
        let lhs_addr = self.handle_exp(ctx, lhs, true, false, false)?;

        let is_lhs_immidiate = matches!(lhs_addr, ExpEval::InImmediate(_));
        let is_lhs_constant = matches!(lhs_addr, ExpEval::InConstant(_));

        let (lhs_dirty, mut lhs_addr) = match lhs_addr {
            ExpEval::InImmediate(i) => (None, i),
            ExpEval::InRegister(i) => {
                ctx.claim_register(lhs_result);
                (Some(lhs_result), i)
            }
            // ExpEval::InConstant(c) => (None, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                self.visit_exp(lhs, ctx)?;
                let reg = ctx.claim_next_free_register();
                (Some(reg), reg)
            }
            _ => {
                return Err(LuzError::CompileError(format!(
                    "This value not supported in comparison: {:?}",
                    lhs_addr
                )))
            }
        };

        let rhs_addr = self.handle_exp(
            ctx,
            rhs,
            !is_lhs_immidiate,
            matches!(op, CmpOp::Eq | CmpOp::Neq),
            true,
        )?;

        let is_rhs_immidiate = matches!(rhs_addr, ExpEval::InImmediate(_));
        let is_rhs_constant = matches!(rhs_addr, ExpEval::InConstant(_));

        let (rhs_dirty, mut rhs_addr) = match rhs_addr {
            ExpEval::InRegister(r) => (None, r),
            ExpEval::InImmediate(i) => (None, i),
            ExpEval::InConstant(c) => (None, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                let reg = ctx.claim_next_free_register();
                self.visit_exp(rhs, ctx)?;
                (Some(reg), reg)
            }
            _ => {
                return Err(LuzError::CompileError(format!(
                    "This value not supported in comparison: {:?}",
                    rhs_addr
                )))
            }
        };

        if let Some(addr_to_free) = lhs_dirty {
            ctx.unclaim_registers(&[addr_to_free]);
        }
        if let Some(addr_to_free) = rhs_dirty {
            ctx.unclaim_registers(&[addr_to_free]);
        }

        let is_immidiate = is_lhs_immidiate || is_rhs_immidiate;
        if is_lhs_immidiate || is_lhs_constant {
            op = op.flip_op();
            (lhs_addr, rhs_addr) = (rhs_addr, lhs_addr);
        }

        match op {
            CmpOp::Eq | CmpOp::Neq => {
                if is_immidiate {
                    ctx.push_inst(Instruction::op_eqi(
                        lhs_addr,
                        rhs_addr,
                        op != CmpOp::Neq && !ctx.in_not(),
                    ));
                } else {
                    ctx.push_inst(Instruction::op_eq(
                        lhs_addr,
                        rhs_addr,
                        is_rhs_constant,
                        op != CmpOp::Neq && !ctx.in_not(),
                    ));
                }
            }
            CmpOp::Lt => {
                ctx.push_inst(Instruction::op_lt(
                    lhs_addr,
                    rhs_addr,
                    is_immidiate,
                    ctx.in_not(),
                ));
            }
            CmpOp::Gt => {
                ctx.push_inst(Instruction::op_gt(
                    lhs_addr,
                    rhs_addr,
                    is_immidiate,
                    ctx.in_not(),
                ));
            }
            CmpOp::LtEq => {
                ctx.push_inst(Instruction::op_le(
                    lhs_addr,
                    rhs_addr,
                    is_immidiate,
                    ctx.in_not(),
                ));
            }
            CmpOp::GtEq => {
                ctx.push_inst(Instruction::op_ge(
                    rhs_addr,
                    lhs_addr,
                    is_immidiate,
                    ctx.in_not(),
                ));
            }
        }

        ctx.push_inst(Instruction::op_jmp(1));
        let next_addr = ctx.get_or_push_free_register();
        ctx.push_inst(Instruction::op_lfalseskip(next_addr));
        ctx.push_inst(Instruction::op_loadtrue(next_addr));
        Ok(())
    }

    fn visit_function_def_exp(
        &mut self,
        ctx: &mut CompilerCtx,
        func_def: &FuncDef,
    ) -> Result<(), LuzError> {
        let FuncDef { params, body } = func_def;

        let reg = ctx.get_or_push_free_register();
        let idx = ctx.push_scope(None);
        ctx.scope_mut().set_nb_params(params.fixed.len() as u32);
        for param in &params.fixed {
            ctx.push_claimed_register_with_start(Some(param.clone()), 1);
        }
        if params.is_vararg {
            let vararg_reg = ctx.push_claimed_register(None);
            ctx.push_inst(Instruction::op_varargprep(vararg_reg));
        }
        for stat in body {
            self.visit_stat(stat, ctx)?;
        }
        ctx.pop_scope()?;
        ctx.push_inst(Instruction::op_closure(reg, idx as u32));
        Ok(())
    }

    fn visit_function_call(
        &mut self,
        ctx: &mut CompilerCtx,
        f_call: &FuncCall,
    ) -> Result<(), LuzError> {
        let FuncCall {
            func,
            method_name,
            args,
            variadic,
        } = f_call;
        let mut f_addr = self.load_exp(ctx, func)?;
        let is_f_addr_next_free = f_addr == ctx.get_or_push_free_register();

        ctx.claim_register(f_addr);

        let mut claimed = vec![];

        let nb_expected = ctx.nb_expected();

        if let Some(m) = method_name {
            let reg = if is_f_addr_next_free {
                claimed.push(f_addr);
                f_addr
            } else {
                let r = ctx.claim_next_free_register();
                claimed.push(r);
                r
            };
            let method_name_const = ctx.get_or_add_const(LuzObj::String(m.clone()));
            ctx.push_inst(Instruction::op_self(
                reg,
                f_addr,
                method_name_const as u8,
                true,
            ));
            f_addr = reg;

            // 'self' arg
            claimed.push(ctx.claim_next_free_register());
        } else {
            let reg = if is_f_addr_next_free {
                claimed.push(f_addr);
                f_addr
            } else {
                let r = ctx.claim_next_free_register();
                ctx.push_inst(Instruction::op_move(r, f_addr));
                claimed.push(r);
                r
            };
            f_addr = reg;
            // if no 'self', claim the function
            // claimed.push(f_addr);
        }

        if !args.is_empty() {
            for arg in &args[..args.len() - 1] {
                let mut one_exp_ctx = ctx.new_with(CompilerCtxBuilder::default().nb_expected(2));
                let reg = self.handle_consecutive_exp(&mut one_exp_ctx, arg)?;
                ctx.claim_register(reg);
                claimed.push(reg);
            }
            let mut all_out_ctx = ctx.new_with(
                CompilerCtxBuilder::default().nb_expected(if ExpNode::is_multires(args) {
                    0
                } else {
                    2
                }),
            );
            let reg = self.handle_consecutive_exp(&mut all_out_ctx, &args[args.len() - 1])?;
            ctx.claim_register(reg);
            claimed.push(reg);
        }

        ctx.unclaim_registers(&claimed);

        ctx.push_inst(Instruction::op_call(
            f_addr,
            if *variadic {
                0
            } else {
                (args.len() + 1 + method_name.as_ref().map_or(0, |_| 1)) as u8
            },
            nb_expected,
        ));
        Ok(())
    }

    fn visit_table_constructor(
        &mut self,
        ctx: &mut CompilerCtx,
        exp_table_constructor: &ExpTableConstructor,
    ) -> Result<(), LuzError> {
        let ExpTableConstructor {
            arr_fields,
            obj_fields,
            last_exp,
            variadic,
        } = exp_table_constructor;

        let dest = ctx.claim_next_free_register();

        ctx.push_inst(Instruction::op_newtable(
            dest,
            obj_fields.len() as u8,
            arr_fields.len() as u8,
        ));
        ctx.push_inst(Instruction::op_extraarg(0));

        let mut claimed = vec![];
        for arr_field in arr_fields {
            let reg = self.handle_consecutive_exp(ctx, arr_field)?;
            ctx.claim_register(reg);
            claimed.push(reg);
        }

        // Unclaim dest to allow field to be set to it
        for obj_field in obj_fields {
            let ExpTableConstructorField { key, val } = obj_field;
            claimed.push(ctx.claim_next_free_register());
            self.assign_to_table(ctx, dest, key, val)?;
        }

        if !arr_fields.is_empty() {
            ctx.push_inst(Instruction::op_setlist(dest, arr_fields.len() as u8, 0));
        }

        ctx.unclaim_registers(&[dest]);
        ctx.unclaim_registers(&claimed);
        Ok(())
    }

    fn visit_literal(&mut self, ctx: &mut CompilerCtx, lit: &LuzObj) -> Result<(), LuzError> {
        match lit {
            LuzObj::Numeral(Numeral::Int(i))
                if (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
            {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                // ctx.claim_next_free_register();
                Ok(())
            }
            LuzObj::Nil => {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_loadnil(reg, 0));
                Ok(())
            }
            LuzObj::Boolean(b) => {
                let reg = ctx.get_or_push_free_register();
                if *b {
                    ctx.push_inst(Instruction::op_loadtrue(reg));
                } else {
                    ctx.push_inst(Instruction::op_loadfalse(reg));
                }
                Ok(())
            }
            _ => {
                let addr = ctx.get_or_add_const(lit.clone());
                let reg = ctx.get_or_push_free_register();

                ctx.push_inst(Instruction::op_loadk(reg, addr));
                Ok(())
            }
        }
    }

    fn visit_vararg(&mut self, ctx: &mut CompilerCtx) -> Result<(), LuzError> {
        let reg = ctx.get_or_push_free_register();
        let nb = ctx.nb_expected();
        ctx.push_inst(Instruction::op_vararg(reg, nb));
        Ok(())
    }

    fn visit_name(&mut self, ctx: &mut CompilerCtx, name: &str) -> Result<(), LuzError> {
        let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
        let reg = ctx.get_or_push_free_register();
        match src {
            RegOrUpvalue::Register(src) => {
                if reg != src.addr {
                    // ctx.push_inst(Instruction::op_move(reg, src.addr));
                    // ctx.unclaim_registers(&[src.addr]);
                }
            }
            RegOrUpvalue::Upvalue(src) => {
                if is_intable {
                    let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                    ctx.push_inst(Instruction::op_gettabup(reg, src.addr, name_k as u8));
                } else {
                    ctx.push_inst(Instruction::op_getupval(reg, src.addr));
                }
            }
        }

        Ok(())
    }

    fn visit_unop(&mut self, ctx: &mut CompilerCtx, exp: &ExpNode) -> Result<(), LuzError> {
        let ExpNode::Unop(unop, exp) = exp else {
            unreachable!()
        };

        if *unop == Unop::Not
            && matches!(
                exp.normalize(),
                ExpNode::CmpOp { .. } | ExpNode::LogicCmpOp { .. }
            )
        {
            self.visit_exp(
                exp,
                &mut ctx.new_with(CompilerCtxBuilder::default().in_not(!ctx.in_not())),
            );
        } else {
            let dest = ctx.get_or_push_free_register();
            let val_addr = self.load_exp(ctx, exp)?;
            ctx.push_inst(Instruction::op_unop(*unop, dest, val_addr));
        }

        Ok(())
    }
}

#[allow(unused)]
impl<'a> Visitor for Compiler<'a> {
    type Return = Result<(), LuzError>;
    type Ctx = CompilerCtx;

    #[must_use]
    fn visit_exp(&mut self, exp: &ExpNode, ctx: &mut Self::Ctx) -> Self::Return {
        match exp {
            ExpNode::Literal(luz_obj) => self.visit_literal(ctx, luz_obj),
            ExpNode::Binop { .. } => self.visit_binop(ctx, exp),
            ExpNode::Vararg => self.visit_vararg(ctx),
            ExpNode::Name(name) => self.visit_name(ctx, name),
            ExpNode::Var(exp) => todo!(),
            ExpNode::Unop(..) => self.visit_unop(ctx, exp),
            ExpNode::CmpOp { op, lhs, rhs } => self.visit_cmpop(ctx, exp),
            ExpNode::LogicCmpOp { op, lhs, rhs } => self.visit_logicop(ctx, exp),
            ExpNode::Access(exp_access) => self.visit_access(ctx, exp_access),
            ExpNode::FuncDef(func_def) => self.visit_function_def_exp(ctx, func_def),
            ExpNode::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            ExpNode::TableConstructor(exp_table_constructor) => {
                self.visit_table_constructor(ctx, exp_table_constructor)
            }
        }
    }

    #[must_use]
    fn visit_stat(&mut self, stat: &Stat, ctx: &mut Self::Ctx) -> Self::Return {
        let ctx = &mut ctx.new_with(CompilerCtxBuilder::default().nb_expected(1));
        ctx.scope_mut().push_line_infos(stat.line_info);
        match &stat.node {
            StatNode::Assign(assign_stat) => self.visit_assign(ctx, assign_stat),
            StatNode::Return(return_stat) => self.visit_return(ctx, return_stat),
            StatNode::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            // Stat::FunctionDef(function_def_stat) => {
            //     self.visit_function_def_stat(ctx, function_def_stat)
            // }
            StatNode::Do(do_stat) => self.visit_do_stat(ctx, do_stat),
            StatNode::While(while_stat) => todo!(),
            StatNode::Repeat(repeat_stat) => self.visit_repeat_until(ctx, repeat_stat),
            StatNode::If(if_stat) => self.visit_if(ctx, if_stat),
            StatNode::ForRange(for_range_stat) => self.visit_for_range(ctx, for_range_stat),
            StatNode::ForIn(for_in_stat) => todo!(),
            StatNode::Break => todo!(),
            StatNode::Goto(goto_stat) => todo!(),
            StatNode::Label(label_stat) => todo!(),
        }
    }
}
