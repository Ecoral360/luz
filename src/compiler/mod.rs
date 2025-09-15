use crate::{
    ast::{
        AssignStat, Binop, CmpOp, DoStat, Exp, ExpAccess, ExpTableConstructor,
        ExpTableConstructorField, FuncCall, FuncDef, ReturnStat, Stat,
    },
    compiler::{
        ctx::{CompilerCtx, CompilerCtxBuilder, RegOrUpvalue, Upvalue},
        instructions::{iABC, Instruction, MAX_HALF_sBx},
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

#[derive(Debug)]
pub struct Compiler {}

#[allow(unused)]
impl Compiler {
    fn handle_immidiate(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &Exp,
        loadi: bool,
    ) -> Result<Option<u8>, LuzError> {
        match exp {
            Exp::Literal(lit) => match lit {
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

    fn handle_consecutive_exp(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<u8, LuzError> {
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
                if in_table {
                    let name_k = ctx.get_or_add_const(LuzObj::String(upvalue.name.to_owned()));
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
        exp: &Exp,
        supports_immidiate: bool,
        supports_constants: bool,
        loadi: bool,
    ) -> Result<ExpEval, LuzError> {
        match exp {
            Exp::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i))
                    if supports_immidiate && (-127..129).contains(i) =>
                {
                    Ok(ExpEval::InImmediate((*i + 128) as u8))
                }
                LuzObj::Numeral(Numeral::Int(i))
                    if loadi && (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
                {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                    Ok(ExpEval::InRegister(reg))
                }
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
            Exp::Name(name) => {
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

    fn assign_to_table(
        &mut self,
        ctx: &mut CompilerCtx,
        tabaddr: u8,
        prop: &Exp,
        value: &Exp,
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
                Exp::Name(ref n) => {
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
}

#[derive(Debug, Clone)]
enum ExpEval {
    InImmediate(u8),
    InRegister(u8),
    InConstant(u8),
    InUpvalue { in_table: bool, upvalue: Upvalue },
}

#[allow(unused)]
impl Compiler {
    fn visit_do_stat(&mut self, ctx: &mut CompilerCtx, do_stat: &DoStat) -> Result<(), LuzError> {
        let DoStat { block } = do_stat;
        let vars_before = ctx.scope().regs().len();
        for stmt in block {
            self.visit_stat(stmt, ctx);
        }

        let mut scope = ctx.scope_mut();
        let regs = scope.regs()[vars_before..]
            .iter()
            .filter_map(|reg| reg.name.clone())
            .collect::<Vec<_>>();

        for reg in regs {
            scope.set_end_of_register(&reg);
        }

        Ok(())
    }

    fn visit_assign(&mut self, ctx: &mut CompilerCtx, assign: &AssignStat) -> Result<(), LuzError> {
        match assign {
            AssignStat::Normal { varlist, explist } => {
                let mut explist_iter = explist.iter();
                for var_exp in varlist {
                    let var = if let Exp::Var(var) = var_exp {
                        var
                    } else {
                        var_exp
                    };
                    match var {
                        Exp::Name(ref name) => {
                            let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
                            match src {
                                RegOrUpvalue::Register(src) => {
                                    self.handle_consecutive_exp(
                                        ctx,
                                        explist_iter.next().expect("value"),
                                    )?;
                                }
                                RegOrUpvalue::Upvalue(src) => {
                                    if is_intable {
                                        let name_k =
                                            ctx.get_or_add_const(LuzObj::String(name.to_owned()));

                                        let res = self.handle_exp(
                                            ctx,
                                            explist_iter.next().expect("value"),
                                            false,
                                            true,
                                            true,
                                        )?;

                                        let (is_val_const, reg) = match res {
                                            ExpEval::InRegister(reg) => (false, reg),
                                            ExpEval::InConstant(kreg) => (true, kreg),
                                            ExpEval::InUpvalue { in_table, upvalue } => todo!(),
                                            _ => unreachable!(
                                                "Not supported by op_settabup {:?}",
                                                res
                                            ),
                                        };
                                        ctx.push_inst(Instruction::op_settabup(
                                            src.addr,
                                            name_k as u8,
                                            reg,
                                            is_val_const,
                                        ));
                                    } else {
                                        self.handle_consecutive_exp(
                                            ctx,
                                            explist_iter.next().expect("value"),
                                        )?;
                                        let reg = ctx.get_or_push_free_register();
                                        ctx.push_inst(Instruction::op_setupval(src.addr, reg));
                                    }
                                }
                            }
                        }
                        Exp::Access(ref exp_access) => {
                            let ExpAccess { exp, value: prop } = exp_access;

                            self.visit_exp(&exp, ctx)?;
                            let tabaddr = ctx.claim_next_free_register();

                            let value = self.handle_exp(
                                ctx,
                                explist_iter.next().expect("value"),
                                false,
                                true,
                                false,
                            )?;
                            let (is_val_const, val_reg) = match value {
                                ExpEval::InRegister(reg) => (false, reg),
                                ExpEval::InConstant(kreg) => (true, kreg),
                                ExpEval::InUpvalue { in_table, upvalue } => todo!(),
                                _ => unreachable!("Not supported by op_settabup {:?}", value),
                            };

                            let prop_imm = self.handle_immidiate(ctx, &prop, true)?;
                            if let Some(imm) = prop_imm {
                                ctx.push_inst(Instruction::op_seti(
                                    tabaddr,
                                    imm,
                                    val_reg,
                                    is_val_const,
                                ));
                            } else {
                                match **prop {
                                    Exp::Name(ref n) => {
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
                        }
                        _ => todo!("Assign normal: {:#?}", var),
                    }
                }
            }
            AssignStat::Local { varlist, explist } => {
                let mut new_ctx = ctx
                    .new_with(CompilerCtxBuilder::default().nb_expected((varlist.len() + 1) as u8));

                for var in varlist {
                    ctx.rename_or_push_free_register(var.0.clone());
                }
                for exp in explist {
                    self.handle_consecutive_exp(&mut new_ctx, exp)?;
                    ctx.claim_next_free_register();
                }

                if varlist.len() > explist.len() && !Exp::is_multires(explist) {
                    for var in varlist[explist.len()..].iter() {
                        let reg = ctx.claim_next_free_register();
                        ctx.push_inst(Instruction::op_loadnil(reg, 0));
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_return(&mut self, ctx: &mut CompilerCtx, stat: &ReturnStat) -> Result<(), LuzError> {
        let start = ctx.get_or_push_free_register();
        let size = stat.explist.len();
        let mut new_ctx = if Exp::is_multires(&stat.explist) {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected(0))
        } else {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected((size + 1) as u8))
        };
        for exp in stat.explist.iter() {
            self.handle_consecutive_exp(ctx, exp)?;
            ctx.claim_next_free_register();
        }
        ctx.push_inst(Instruction::op_return(start, false, size as u8 + 1));
        Ok(())
    }

    fn visit_binop(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<(), LuzError> {
        let Exp::Binop { op, lhs, rhs } = exp else {
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

        let result_reg = ctx.get_or_push_free_register();

        let lhs_addr = self.handle_exp(ctx, lhs, matches!(op, Binop::Add), true, true)?;
        let rhs_addr =
            self.handle_exp(ctx, rhs, matches!(op, Binop::Add | Binop::Sub), true, true)?;

        let is_b_const = matches!(lhs_addr, ExpEval::InConstant(_));
        let is_b_imm = matches!(lhs_addr, ExpEval::InImmediate(_));

        let is_c_const = matches!(rhs_addr, ExpEval::InConstant(_));
        let is_c_imm = matches!(rhs_addr, ExpEval::InImmediate(_));

        if is_b_imm && is_c_imm {
            return Err(LuzError::CompileError(format!(
                "Both sides of {:?} cannot be immediate values, should have been optimized away before codegen",
                op
            )));
        }

        let (lhs_dirty, mut lhs_addr) = match lhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Sub => (false, ((i as i32) - 256) as u8),
            ExpEval::InImmediate(i) => (false, i),
            ExpEval::InRegister(r) => (false, r),
            ExpEval::InConstant(c) => (false, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                self.visit_exp(lhs, ctx)?;
                (true, ctx.claim_next_free_register())
            }
        };
        let (rhs_dirty, mut rhs_addr) = match rhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Sub => (false, ((i as i32) - 256) as u8),
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
            ctx.push_inst(Instruction::op_addi(result_reg, lhs_addr, false, rhs_addr));

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

    fn visit_logicop(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<(), LuzError> {
        let Exp::LogicCmpOp { op, lhs, rhs } = exp else {
            unreachable!()
        };

        let post_instructions = self.visit_inner_logicop(ctx, exp, 0, 0)?;
        for inst in post_instructions {
            ctx.push_inst(inst);
        }
        Ok(())
    }

    fn visit_access(
        &mut self,
        ctx: &mut CompilerCtx,
        exp_access: &ExpAccess,
    ) -> Result<(), LuzError> {
        let ExpAccess { exp, value } = exp_access;
        let dest = ctx.get_or_push_free_register();

        self.visit_exp(exp, ctx)?;
        let tabaddr = ctx.claim_next_free_register();

        let prop = self.handle_immidiate(ctx, value, false)?;
        if let Some(imm) = prop {
            ctx.push_inst(Instruction::op_geti(dest, tabaddr, imm));
        } else {
            match **value {
                Exp::Name(ref n) => {
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

        // ctx.unclaim_registers(&[tabaddr]);
        Ok(())
    }

    fn visit_inner_logicop(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &Exp,
        jmp_size: usize,
        depth: usize,
    ) -> Result<Vec<Instruction>, LuzError> {
        let Exp::LogicCmpOp { op, lhs, rhs } = exp else {
            unreachable!()
        };
        let nb_inst_before = ctx.scope().instructions().len();

        self.visit_exp(rhs, ctx)?;

        let mut rhs_instructions = {
            let mut scope = ctx.scope_mut();
            scope
                .instructions_mut()
                .drain(nb_inst_before..)
                .collect::<Vec<_>>()
        };

        if depth != 0 && !matches!(**rhs, Exp::CmpOp { .. }) {
            let rhs_last_inst = {
                if rhs_instructions
                    .last()
                    .expect("There must be an instruction here")
                    .op()
                    == LuaOpCode::OP_MOVE
                {
                    rhs_instructions.pop()
                } else {
                    None
                }
            };
            if let Some(last_move) = rhs_last_inst {
                let Instruction::iABC(iABC { c, b, k, a, op }) = last_move else {
                    unreachable!()
                };
                let val = b;
                let dest = a;
                rhs_instructions.push(Instruction::op_testset(dest, val, true));
            } else {
                let reg = ctx.get_or_push_free_register();
                rhs_instructions.push(Instruction::op_test(reg, true));
            }
            rhs_instructions.push(Instruction::op_jmp(jmp_size as u32));
        }

        let rhs_len = rhs_instructions.len();
        let mut post_instructions = if matches!(**lhs, Exp::LogicCmpOp { .. }) {
            self.visit_inner_logicop(ctx, lhs, jmp_size + rhs_len, depth + 1)?
        } else {
            self.visit_exp(lhs, ctx)?;
            vec![]
        };

        match **lhs {
            Exp::CmpOp { .. } => {
                {
                    let nb_drop = ctx.scope().instructions().len() - 3;
                    let mut scope = ctx.scope_mut();
                    scope.instructions_mut().truncate(nb_drop);
                    let Instruction::iABC(iABC { k, .. }) =
                        scope.instructions_mut().last_mut().unwrap()
                    else {
                        unreachable!();
                    };
                    *k = !*k;
                }
                if matches!(**rhs, Exp::CmpOp { .. }) {
                    ctx.push_inst(Instruction::op_jmp((jmp_size + rhs_len - 2) as u32));
                    if depth != 0 {
                        rhs_instructions.truncate(rhs_len - 3);
                    }
                    for inst in rhs_instructions {
                        ctx.push_inst(inst);
                    }
                    if depth != 0 {
                        ctx.push_inst(Instruction::op_jmp(jmp_size as u32));
                    }
                } else {
                    ctx.push_inst(Instruction::op_jmp((jmp_size + rhs_len) as u32));
                    let next_addr = ctx.get_or_push_free_register();
                    for inst in rhs_instructions {
                        ctx.push_inst(inst);
                    }
                    ctx.push_inst(Instruction::op_jmp(jmp_size as u32));

                    post_instructions.push(Instruction::op_lfalseskip(next_addr));
                    post_instructions.push(Instruction::op_loadtrue(next_addr));
                }
            }
            Exp::LogicCmpOp { .. } => {
                for inst in rhs_instructions {
                    ctx.push_inst(inst);
                }
            }
            _ => {
                let last_inst = {
                    let mut scope = ctx.scope_mut();
                    if scope
                        .instructions()
                        .last()
                        .expect("There must be an instruction here")
                        .op()
                        == LuaOpCode::OP_MOVE
                    {
                        scope.instructions_mut().pop()
                    } else {
                        None
                    }
                };
                if let Some(last_move) = last_inst {
                    let Instruction::iABC(iABC { c, b, k, a, op }) = last_move else {
                        unreachable!()
                    };
                    let val = b;
                    let dest = a;
                    ctx.push_inst(Instruction::op_testset(dest, val, true));
                } else {
                    let reg = ctx.get_or_push_free_register();
                    ctx.push_inst(Instruction::op_test(reg, true));
                }
                ctx.push_inst(Instruction::op_jmp((jmp_size + rhs_len) as u32));
                // we are at the top level of this "and"
                for inst in rhs_instructions {
                    ctx.push_inst(inst);
                }
            }
        }

        Ok(post_instructions)
    }

    fn visit_cmpop(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<(), LuzError> {
        let Exp::CmpOp { mut op, lhs, rhs } = exp else {
            unreachable!()
        };

        let lhs_addr =
            self.handle_exp(ctx, lhs, true, matches!(op, CmpOp::Eq | CmpOp::Neq), true)?;
        let rhs_addr =
            self.handle_exp(ctx, rhs, true, matches!(op, CmpOp::Eq | CmpOp::Neq), true)?;

        let is_lhs_immidiate = matches!(lhs_addr, ExpEval::InImmediate(_));
        let is_lhs_constant = matches!(lhs_addr, ExpEval::InConstant(_));

        let is_rhs_immidiate = matches!(rhs_addr, ExpEval::InImmediate(_));
        let is_rhs_constant = matches!(rhs_addr, ExpEval::InConstant(_));

        let (lhs_dirty, mut lhs_addr) = match lhs_addr {
            ExpEval::InRegister(r) => (None, r),
            ExpEval::InImmediate(i) => (Some(ctx.claim_next_free_register()), i),
            ExpEval::InConstant(c) => (None, c),
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
        let (rhs_dirty, mut rhs_addr) = match rhs_addr {
            ExpEval::InRegister(r) => (None, r),
            ExpEval::InImmediate(i) => (Some(ctx.claim_next_free_register()), i),
            ExpEval::InConstant(c) => (None, c),
            ExpEval::InUpvalue { in_table, upvalue } => {
                self.visit_exp(rhs, ctx)?;
                let reg = ctx.claim_next_free_register();
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
        }

        match op {
            CmpOp::Eq | CmpOp::Neq => {
                if is_rhs_immidiate {
                    ctx.push_inst(Instruction::op_eqi(lhs_addr, rhs_addr, op == CmpOp::Neq));
                } else {
                    ctx.push_inst(Instruction::op_eq(
                        lhs_addr,
                        rhs_addr,
                        is_rhs_constant,
                        op == CmpOp::Neq,
                    ));
                }
            }
            CmpOp::Lt => {
                ctx.push_inst(Instruction::op_lt(lhs_addr, rhs_addr, is_immidiate, false));
            }
            CmpOp::Gt => {
                ctx.push_inst(Instruction::op_gt(rhs_addr, lhs_addr, is_immidiate, false));
            }
            CmpOp::LtEq => {
                ctx.push_inst(Instruction::op_le(lhs_addr, rhs_addr, is_immidiate, false));
            }
            CmpOp::GtEq => {
                ctx.push_inst(Instruction::op_ge(rhs_addr, lhs_addr, is_immidiate, false));
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
            ctx.push_claimed_register(Some(param.clone()));
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
        self.visit_exp(func, ctx)?;
        let mut f_addr = ctx.claim_next_free_register();

        let mut claimed = vec![];

        if let Some(m) = method_name {
            let reg = ctx.get_or_push_free_register();
            let method_name_const = ctx.get_or_add_const(LuzObj::String(m.clone()));
            ctx.push_inst(Instruction::op_self(
                reg,
                f_addr,
                method_name_const as u8,
                true,
            ));
            f_addr = reg;
            // the function
            claimed.push(ctx.claim_next_free_register());
            // 'self' arg
            claimed.push(ctx.claim_next_free_register());
        } else {
            // if no 'self', claim the function
            claimed.push(f_addr);
        }

        if !args.is_empty() {
            for arg in &args[..args.len() - 1] {
                let mut one_exp_ctx = ctx.new_with(CompilerCtxBuilder::default().nb_expected(2));
                self.handle_consecutive_exp(&mut one_exp_ctx, arg)?;
                claimed.push(ctx.claim_next_free_register());
            }
            let mut all_out_ctx = ctx.new_with(
                CompilerCtxBuilder::default().nb_expected(if Exp::is_multires(args) {
                    0
                } else {
                    2
                }),
            );
            self.handle_consecutive_exp(&mut all_out_ctx, &args[args.len() - 1])?;
            claimed.push(ctx.claim_next_free_register());
        }

        let nb_expected = ctx.nb_expected();
        let nb_to_unclaim = ctx.scope().regs().len();
        ctx.unclaim_registers(&claimed);

        // ctx.unclaim_register_range(f_addr, claimed);

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

        let dest = ctx.get_or_push_free_register();

        ctx.push_inst(Instruction::op_newtable(
            dest,
            obj_fields.len() as u8,
            arr_fields.len() as u8,
        ));

        let mut claimed = vec![];
        for arr_field in arr_fields {
            self.visit_exp(arr_field, ctx)?;
            claimed.push(ctx.claim_next_free_register());
        }

        // Unclaim dest to allow field to be set to it
        for obj_field in obj_fields {
            let ExpTableConstructorField { key, val } = obj_field;
            self.assign_to_table(ctx, dest, key, val)?;
            claimed.push(ctx.claim_next_free_register());
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
            LuzObj::Numeral(Numeral::Int(i)) if *i >= 0 && *i <= 255 => {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                // ctx.claim_next_free_register();
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

    fn visit_name(&mut self, ctx: &mut CompilerCtx, name: &str) -> Result<(), LuzError> {
        let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
        let reg = ctx.get_or_push_free_register();
        match src {
            RegOrUpvalue::Register(src) => {
                if reg != src.addr {
                    ctx.unclaim_registers(&[src.addr]);
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

    fn visit_unop(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<(), LuzError> {
        let Exp::Unop(unop, exp) = exp else {
            unreachable!()
        };

        let dest = ctx.get_or_push_free_register();

        self.visit_exp(exp, ctx);
        let val_addr = ctx.get_or_push_free_register();

        ctx.push_inst(Instruction::op_unop(*unop, dest, val_addr));

        Ok(())
    }
}

#[allow(unused)]
impl Visitor for Compiler {
    type Return = Result<(), LuzError>;
    type Ctx = CompilerCtx;

    #[must_use]
    fn visit_exp(&mut self, exp: &Exp, ctx: &mut Self::Ctx) -> Self::Return {
        match exp {
            Exp::Literal(luz_obj) => self.visit_literal(ctx, luz_obj),
            Exp::Binop { .. } => self.visit_binop(ctx, exp),
            Exp::Vararg => todo!(),
            Exp::Name(name) => self.visit_name(ctx, name),
            Exp::Var(exp) => todo!(),
            Exp::Unop(..) => self.visit_unop(ctx, exp),
            Exp::CmpOp { op, lhs, rhs } => self.visit_cmpop(ctx, exp),
            Exp::LogicCmpOp { op, lhs, rhs } => self.visit_logicop(ctx, exp),
            Exp::Access(exp_access) => self.visit_access(ctx, exp_access),
            Exp::FuncDef(func_def) => self.visit_function_def_exp(ctx, func_def),
            Exp::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            Exp::TableConstructor(exp_table_constructor) => {
                self.visit_table_constructor(ctx, exp_table_constructor)
            }
        }
    }

    #[must_use]
    fn visit_stat(&mut self, stat: &Stat, ctx: &mut Self::Ctx) -> Self::Return {
        match stat {
            Stat::Assign(assign_stat) => self.visit_assign(ctx, assign_stat),
            Stat::Return(return_stat) => self.visit_return(ctx, return_stat),
            Stat::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            // Stat::FunctionDef(function_def_stat) => {
            //     self.visit_function_def_stat(ctx, function_def_stat)
            // }
            Stat::Do(do_stat) => self.visit_do_stat(ctx, do_stat),
            Stat::While(while_stat) => todo!(),
            Stat::Repeat(repea_stat) => todo!(),
            Stat::If(if_stat) => todo!(),
            Stat::ForRange(for_range_stat) => todo!(),
            Stat::ForIn(for_in_stat) => todo!(),
            Stat::Break => todo!(),
            Stat::Goto(goto_stat) => todo!(),
            Stat::Label(label_stat) => todo!(),
        }
    }
}
