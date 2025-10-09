use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    path::Prefix,
    rc::Rc,
};

use num_enum::TryFromPrimitive;

use crate::{
    ast::{Binop, CmpOp, LineInfo, Unop},
    compiler::{
        ctx::{Scope, ScopeRef},
        instructions::{self, iABC, iABx, iAsBx, isJ, Instruction, MAX_HALF_sBx, MAX_HALF_sJ},
        opcode::{LuaOpCode, TMcode},
    },
    luz::{
        err::LuzError,
        obj::{LuzFunction, LuzObj, LuzType, Numeral, Table, TableRef},
    },
    luz_let,
    runner::err::LuzRuntimeError,
};

pub mod err;

pub enum InstructionResult {
    Continue,
    Jmp(i32),
    Return(Vec<LuzObj>),
}

#[allow(unused)]
pub struct Runner<'a> {
    filename: String,
    input: &'a str,
    scope: Rc<RefCell<Scope>>,
    vararg: Option<Vec<LuzObj>>,
    curr_instruction: Option<(usize, Instruction)>,
    registry: Rc<RefCell<Table>>,
    pc: usize,
    starting_line_info: Option<LineInfo>,
    depth: usize,
}

#[allow(unused)]
impl<'a> Runner<'a> {
    pub fn dump_trace(&self) {
        if let Some((idx, inst)) = &self.curr_instruction {
            println!("[{}] {}", idx, inst);
        }
    }

    pub fn new_chunk(
        filename: String,
        input: &'a str,
        scope: Rc<RefCell<Scope>>,
        registry: TableRef,
    ) -> Self {
        let env = Scope::get_global(Rc::clone(&scope)).unwrap();
        let global = registry.borrow().rawget(&LuzObj::str("_G")).clone();
        let mut env = env.borrow_mut();
        let env_value = env.get_reg_with_name("_ENV");
        // Only set _ENV if it has no value yet
        if env_value.is_some_and(|v| v.val.is_none()) {
            env.set_reg_with_name("_ENV", global);
        }

        Self {
            filename,
            input,
            scope,
            vararg: Some(vec![]),
            curr_instruction: None,
            registry,
            pc: 1,
            starting_line_info: None,
            depth: 0,
        }
    }

    pub fn new(
        filename: String,
        input: &'a str,
        scope: Rc<RefCell<Scope>>,
        registry: TableRef,
    ) -> Self {
        let env = Scope::get_global(Rc::clone(&scope)).unwrap();
        let global = registry.borrow().rawget(&LuzObj::str("_G")).clone();
        let mut env = env.borrow_mut();
        let env_value = env.get_reg_with_name("_ENV");
        // Only set _ENV if it has no value yet
        if env_value.is_some_and(|v| v.val.is_none()) {
            env.set_reg_with_name("_ENV", global);
        }

        Self {
            filename,
            input,
            scope,
            vararg: Some(vec![]),
            curr_instruction: None,
            registry,
            pc: 1,
            starting_line_info: None,
            depth: 0,
        }
    }

    pub fn clone_scope(&self) -> Rc<RefCell<Scope>> {
        Rc::clone(&self.scope)
    }

    pub fn env_scope(&self) -> Option<ScopeRef> {
        Scope::get_global(Rc::clone(&self.scope))
    }

    pub fn get_val(&mut self, name: &str) -> Option<LuzObj> {
        self.scope_mut().get_reg_or_upvalue_value(name)
    }

    pub fn scope(&self) -> Ref<Scope> {
        self.scope.borrow()
    }

    pub fn reset(&mut self, new_scope: Rc<RefCell<Scope>>) {
        self.scope = new_scope;
        // 0 because it will be increased in the next iteration
        self.pc = 0;
        self.curr_instruction = None;
    }

    fn scope_mut(&mut self) -> RefMut<Scope> {
        self.scope.borrow_mut()
    }

    fn get_line(&self, line_info: &LineInfo) -> &'a str {
        &self.input[line_info.start_pos..line_info.end_pos]
    }

    fn format_err(&self, pc: usize, err: LuzError) -> LuzError {
        let scope = self.scope();
        let Some((_, line_info)) = scope.get_line_info(pc) else {
            return LuzError::LuzRuntimeError(LuzRuntimeError::message(format!(
                "luz: {}: {}",
                self.filename, err
            )));
        };
        LuzError::LuzRuntimeError(LuzRuntimeError::message(format!(
            "{}\n{}:{}[pc={}] {}",
            err,
            self.filename,
            line_info.start_line_col.0,
            self.pc,
            self.get_line(&line_info),
        )))
    }

    pub fn run(&mut self) -> Result<Vec<LuzObj>, LuzError> {
        if self.depth > 200 {
            Err(LuzRuntimeError::message("stack overflow"))?;
        }
        let mut rets = vec![];
        loop {
            let Some(instruction) = ({
                let scope = self.scope();
                scope.instructions().get(self.pc - 1).cloned()
            }) else {
                break;
            };

            self.curr_instruction = Some((self.pc - 1, instruction.clone()));
            rets = match self
                .run_instruction(&instruction)
                .map_err(|err| self.format_err(self.pc, err))?
            {
                InstructionResult::Continue => {
                    self.pc += 1;
                    continue;
                }
                InstructionResult::Jmp(n) => {
                    if n == 0 {
                        Err(self.format_err(
                            self.pc,
                            LuzRuntimeError::message("infinite loop").into(),
                        ))?;
                    }
                    if n < 0 {
                        self.pc -= n.abs() as usize;
                    } else {
                        self.pc += n as usize + 1;
                    }
                    continue;
                }
                InstructionResult::Return(rets) => rets,
            };
            break;
        }
        Ok(rets)
    }

    fn get_reg_val_or_const(&self, val: u8, is_const: bool) -> Result<LuzObj, LuzError> {
        if is_const {
            self.get_const_val(val)
        } else {
            self.get_reg_val(val)
        }
    }

    fn get_reg_val(&self, reg: u8) -> Result<LuzObj, LuzError> {
        let val = self
            .scope
            .borrow_mut()
            .regs()
            .get(reg as usize)
            .ok_or(LuzError::RuntimeError(format!("Register not found {reg}")))?
            .val
            .as_ref()
            .ok_or(LuzError::RuntimeError(format!(
                "Value not found for register {reg}"
            )))?
            .clone();
        Ok(val)
    }

    fn get_reg_val_or_nil(&self, reg: u8) -> LuzObj {
        let val = self
            .scope
            .borrow_mut()
            .regs()
            .get(reg as usize)
            .map(|reg| reg.val.clone().unwrap_or(LuzObj::Nil))
            .unwrap_or(LuzObj::Nil);
        val
    }

    fn take_reg_val(&mut self, reg: u8) -> Result<Option<LuzObj>, LuzError> {
        Ok(self.scope.borrow_mut().take_reg_val(reg))
    }

    fn get_const_val(&self, idx: u8) -> Result<LuzObj, LuzError> {
        let val = self.scope.borrow_mut().constants()[idx as usize].clone();
        Ok(val)
    }

    fn run_instruction(
        &mut self,
        instruction: &Instruction,
    ) -> Result<InstructionResult, LuzError> {
        match instruction {
            Instruction::iABC(i_abc @ iABC { c, b, k, a, op }) => match op {
                LuaOpCode::OP_LT | LuaOpCode::OP_LE => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_reg_val(*b)?;

                    let res = lhs.apply_cmp(
                        if *op == LuaOpCode::OP_LT {
                            CmpOp::Lt
                        } else {
                            CmpOp::LtEq
                        },
                        &rhs,
                    )?;

                    if res.is_truthy() != *k {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }
                LuaOpCode::OP_LTI | LuaOpCode::OP_LEI | LuaOpCode::OP_GEI | LuaOpCode::OP_GTI => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = (*b as i64) - 128;

                    let cmp_op = match op {
                        LuaOpCode::OP_LTI => CmpOp::Lt,
                        LuaOpCode::OP_LEI => CmpOp::LtEq,
                        LuaOpCode::OP_GEI => CmpOp::GtEq,
                        LuaOpCode::OP_GTI => CmpOp::Gt,
                        _ => unreachable!(),
                    };
                    let res = lhs.apply_cmp(cmp_op, &LuzObj::Numeral(Numeral::Int(rhs)))?;

                    if res.is_truthy() != *k {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }
                LuaOpCode::OP_EQK => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_const_val(*b)?;

                    let LuzObj::Boolean(b) = lhs.apply_cmp(CmpOp::Eq, &rhs)? else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }

                LuaOpCode::OP_EQ => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_reg_val(*b)?;

                    let LuzObj::Boolean(b) = lhs.apply_cmp(CmpOp::Eq, &rhs)? else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }

                LuaOpCode::OP_EQI => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = (*b as i64) - 128;

                    let LuzObj::Boolean(b) =
                        lhs.apply_cmp(CmpOp::Eq, &LuzObj::Numeral(Numeral::Int(rhs)))?
                    else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }

                LuaOpCode::OP_TEST => {
                    let val = self.get_reg_val(*a)?;

                    if !(val.is_truthy() == *k) {
                        return Ok(InstructionResult::Jmp(1));
                    }
                }
                LuaOpCode::OP_TESTSET => {
                    let val = self.get_reg_val(*b)?;

                    if !(val.is_truthy() == *k) {
                        return Ok(InstructionResult::Jmp(1));
                    } else {
                        self.scope_mut().set_reg_val(*a, val);
                    }
                }
                LuaOpCode::OP_CLOSE => {
                    let reg_len = self.scope().regs().len();
                    for addr in *a..reg_len as u8 {
                        self.scope_mut().set_reg_val(addr, LuzObj::Nil);
                    }
                }
                LuaOpCode::OP_LOADTRUE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(true));
                }
                LuaOpCode::OP_LOADFALSE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(false));
                }
                LuaOpCode::OP_LFALSESKIP => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(false));
                    return Ok(InstructionResult::Jmp(1));
                }
                LuaOpCode::OP_NOT => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.apply_unop(Unop::Not)?);
                }
                LuaOpCode::OP_LEN => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.apply_unop(Unop::Len)?);
                }
                LuaOpCode::OP_BNOT => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.apply_unop(Unop::Inv)?);
                }
                LuaOpCode::OP_UNM => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.apply_unop(Unop::Neg)?);
                }
                LuaOpCode::OP_ADD
                | LuaOpCode::OP_ADDK
                | LuaOpCode::OP_SUB
                | LuaOpCode::OP_SUBK
                | LuaOpCode::OP_MUL
                | LuaOpCode::OP_MULK
                | LuaOpCode::OP_DIV
                | LuaOpCode::OP_DIVK
                | LuaOpCode::OP_IDIV
                | LuaOpCode::OP_IDIVK
                | LuaOpCode::OP_MOD
                | LuaOpCode::OP_MODK
                | LuaOpCode::OP_POW
                | LuaOpCode::OP_POWK
                | LuaOpCode::OP_BXOR
                | LuaOpCode::OP_BXORK
                | LuaOpCode::OP_BAND
                | LuaOpCode::OP_BANDK
                | LuaOpCode::OP_BOR
                | LuaOpCode::OP_BORK
                | LuaOpCode::OP_SHL
                | LuaOpCode::OP_SHR => self.run_arithmetic(i_abc)?,

                LuaOpCode::OP_ADDI | LuaOpCode::OP_SHLI | LuaOpCode::OP_SHRI => {
                    let iABC { c, b, k, a, .. } = *i_abc;

                    let mut lhs = self.get_reg_val_or_const(b, k)?;

                    let c = (c as i64) - 128; // from excess-128

                    let mut rhs = LuzObj::Numeral(Numeral::Int(c));

                    let binop = match op {
                        LuaOpCode::OP_ADDI => Binop::Add,
                        LuaOpCode::OP_SHLI => {
                            (lhs, rhs) = (rhs, lhs);
                            Binop::ShiftLeft
                        }
                        LuaOpCode::OP_SHRI => Binop::ShiftRight,
                        _ => unreachable!(),
                    };

                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, lhs.apply_binop(binop, rhs)?);
                }

                LuaOpCode::OP_MMBINK | LuaOpCode::OP_MMBINI | LuaOpCode::OP_MMBIN => {
                    let result_reg = {
                        let scope = self.scope();
                        let Instruction::iABC(iABC { a, .. }) = &scope.instructions()[self.pc - 2]
                        else {
                            unreachable!(
                                "Should not have anything but an operation before the MMBIN op"
                            )
                        };
                        *a
                    };

                    let mut lhs = self.get_reg_val(*a)?;
                    let mut rhs = match op {
                        LuaOpCode::OP_MMBINK => self.get_const_val(*b)?,
                        LuaOpCode::OP_MMBINI => LuzObj::int((*b as i64) - 128),
                        LuaOpCode::OP_MMBIN => self.get_reg_val(*b)?,
                        _ => unreachable!(),
                    };

                    if *k {
                        (lhs, rhs) = (rhs, lhs)
                    }

                    if lhs.get_type() == LuzType::Number && rhs.get_type() == LuzType::Number {
                        return Ok(InstructionResult::Continue);
                    }

                    let tm = TMcode::try_from_primitive(*c).expect("Valid metamethod");

                    let result = LuzObj::call_metamethod(self, tm, lhs, rhs)?;
                    self.scope_mut().set_reg_val(result_reg, result);
                }

                LuaOpCode::OP_CONCAT => {
                    let start = *a;
                    let end = start + *b;

                    let dest = *a;

                    let mut str = String::new();
                    for addr in start..end {
                        let obj = self.get_reg_val(addr)?;
                        str += &obj.to_string();
                    }

                    self.scope_mut().set_reg_val(start, LuzObj::str(str));
                }

                LuaOpCode::OP_MOVE => {
                    let iABC { a, b, .. } = *i_abc;
                    let val = self.get_reg_val(b)?;
                    self.scope.borrow_mut().set_reg_val(a, val);
                }

                LuaOpCode::OP_RETURN => {
                    let mut rets = vec![];
                    if *b == 0 {
                        let mut addr = 0;
                        while let Ok(Some(val)) = self.take_reg_val(*a + addr) {
                            rets.push(val);
                            addr += 1;
                        }
                    } else {
                        for i in 0..*b - 1 {
                            rets.push(self.get_reg_val(*a + i)?);
                        }
                    }
                    return Ok(InstructionResult::Return(rets));
                }
                LuaOpCode::OP_RETURN1 => {
                    return Ok(InstructionResult::Return(vec![self.get_reg_val(*a)?]));
                }
                LuaOpCode::OP_RETURN0 => {
                    return Ok(InstructionResult::Return(vec![]));
                }

                LuaOpCode::OP_VARARG => {
                    if *c == 0 {
                        let mut scope = self.scope_mut();

                        let vararg = scope.vararg().to_vec();
                        let mut vararg_iter = vararg.iter().enumerate();
                        for (addr, next_val) in vararg_iter {
                            scope.set_or_push_reg_val(*a + addr as u8, next_val.clone());
                        }
                    } else {
                        for addr in 0..=*c - 2 {
                            let mut scope = self.scope_mut();

                            let next_val = scope
                                .vararg()
                                .get(addr as usize)
                                .cloned()
                                .unwrap_or(LuzObj::Nil);

                            scope.set_reg_val(*a + addr, next_val);
                        }
                    }
                }

                LuaOpCode::OP_VARARGPREP => {
                    let vararg = self.vararg.take().expect("Var arg");
                    self.scope_mut().set_vararg(vararg);
                }
                LuaOpCode::OP_SELF => {
                    let self_table = self.get_reg_val(*b)?;

                    self.scope_mut().set_reg_val(*a + 1, self_table.clone());

                    let LuzObj::Table(self_table) = self_table else {
                        return Err(LuzError::Type {
                            wrong: self_table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let table = self_table.borrow();
                    let val = table.rawget(&key);
                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
                }
                LuaOpCode::OP_CALL | LuaOpCode::OP_TAILCALL => {
                    let iABC {
                        c: nb_expected_results,
                        b: nb_args,
                        a: func_addr,
                        ..
                    } = *i_abc;

                    let is_tail_call = *op == LuaOpCode::OP_TAILCALL;

                    let func = self.get_reg_val(func_addr)?;
                    let Some((LuzObj::Function(f), prefix_args)) = func.callable() else {
                        return Err(LuzError::Type {
                            wrong: func.get_type(),
                            expected: vec![LuzType::Function],
                        });
                    };

                    let f = f.borrow();

                    let nb_params = f.nb_fixed_params();

                    let mut args = prefix_args;
                    let mut vararg = vec![];
                    let mut arg_addr = 0;
                    while let Ok(Some(arg_val)) = self.take_reg_val(func_addr + 1 + arg_addr as u8)
                    {
                        if nb_args != 0 && arg_addr as u8 == nb_args - 1 {
                            break;
                        }
                        let args_len = args.len() as u32;
                        if args_len >= nb_params {
                            vararg.push(arg_val);
                        } else {
                            args.push(arg_val);
                        }
                        arg_addr += 1;
                    }
                    let args_len = args.len() as u32;
                    if args_len < nb_params {
                        for _ in args_len..nb_params {
                            args.push(LuzObj::Nil);
                        }
                    }

                    let results = if is_tail_call {
                        let Some(results) = f.tailcall(self, args, vararg)? else {
                            return Ok(InstructionResult::Continue);
                        };
                        results
                    } else {
                        f.call(self, args, vararg)?
                    };

                    let nb_results = results.len() as u8;
                    if nb_expected_results == 0 {
                        // TODO: check we have enough registers
                        for (i, result) in results.into_iter().enumerate() {
                            self.scope_mut()
                                .set_or_push_reg_val(func_addr + i as u8, result);
                        }
                        let mut extra = 0;
                        // The rest of the stack is set to nil
                        while let Some(val) = self
                            .scope_mut()
                            .take_reg_val(func_addr + nb_results + extra)
                        {
                            extra += 1;
                        }
                    } else {
                        for (i, result) in results.into_iter().enumerate() {
                            if i == nb_expected_results as usize {
                                break;
                            };
                            self.scope_mut().set_reg_val(func_addr + i as u8, result);
                        }
                        if (nb_expected_results - 1) > nb_results {
                            for addr in nb_results..nb_expected_results {
                                self.scope_mut().set_reg_val(func_addr + addr, LuzObj::Nil);
                            }
                        }
                    }
                }
                LuaOpCode::OP_GETUPVAL => {
                    let iABC { a, b, c, .. } = *i_abc;
                    let val = self
                        .scope()
                        .get_upvalue_value(b)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    self.scope.borrow_mut().set_reg_val(a, val);
                }
                LuaOpCode::OP_SETUPVAL => {
                    let val = self.get_reg_val(*a)?;
                    self.scope_mut().set_upvalue_value(*b, val);
                }
                LuaOpCode::OP_GETFIELD => {
                    let table = self.get_reg_val(*b)?;

                    let key = self.get_const_val(*c)?;
                    let Some(val) = table.index(self, &key)? else {
                        self.scope.borrow_mut().set_reg_val(*a, LuzObj::Nil);
                        return Ok(InstructionResult::Continue);
                    };

                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
                }
                LuaOpCode::OP_GETTABUP => {
                    let iABC { a, b, c, .. } = *i_abc;

                    let table = self
                        .scope()
                        .get_upvalue_value(b)
                        .ok_or(LuzError::CompileError("Upvalue table missing".to_owned()))?;

                    let key = self.get_const_val(c)?;
                    let Some(val) = table.index(self, &key)? else {
                        self.scope.borrow_mut().set_reg_val(a, LuzObj::Nil);
                        return Ok(InstructionResult::Continue);
                    };

                    self.scope.borrow_mut().set_reg_val(a, val.clone());
                }
                LuaOpCode::OP_GETTABLE => {
                    let table = self.get_reg_val(*b)?;

                    let key = self.get_reg_val(*c)?;
                    let Some(val) = table.index(self, &key)? else {
                        self.scope.borrow_mut().set_reg_val(*a, LuzObj::Nil);
                        return Ok(InstructionResult::Continue);
                    };

                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
                }
                LuaOpCode::OP_GETI => {
                    let table = self.get_reg_val(*b)?;

                    let key = LuzObj::Numeral(Numeral::Int((*c as i64) - 128));
                    let Some(val) = table.index(self, &key)? else {
                        self.scope.borrow_mut().set_reg_val(*a, LuzObj::Nil);
                        return Ok(InstructionResult::Continue);
                    };

                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
                }
                LuaOpCode::OP_SETTABUP => {
                    let table = self
                        .scope()
                        .get_upvalue_value(*a)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_const_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.rawset(key, val);
                }
                LuaOpCode::OP_SETTABLE => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_reg_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.rawset(key, val);
                }
                LuaOpCode::OP_SETI => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = (*b as i64) - 128;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.rawset(LuzObj::Numeral(Numeral::Int(key)), val);
                }
                LuaOpCode::OP_SETFIELD => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };

                    let key = self.get_const_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.rawset(key, val);
                }
                LuaOpCode::OP_NEWTABLE => {
                    let mut table = HashMap::new();

                    let table_obj = Table::new(table, None);

                    self.scope_mut()
                        .set_reg_val(*a, LuzObj::Table(Rc::new(RefCell::new(table_obj))));
                }
                LuaOpCode::OP_SETLIST => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };

                    if *b == 0 {
                        let mut addr = 1;
                        while let Ok(Some(val)) = self.take_reg_val(*a + addr) {
                            table
                                .borrow_mut()
                                .rawset(LuzObj::Numeral(Numeral::Int((*c + addr) as i64)), val);
                            addr += 1;
                        }
                    } else {
                        for i in 1..=*b {
                            let val = self.get_reg_val_or_nil(*a + i);
                            table
                                .borrow_mut()
                                .rawset(LuzObj::Numeral(Numeral::Int((*c + i) as i64)), val);
                        }
                    }
                }

                LuaOpCode::OP_TFORCALL => {
                    luz_let!(
                        Some((LuzObj::Function(iter), mut prefix_args)) =
                            self.get_reg_val(*a)?.callable()
                    );
                    let state = self.get_reg_val(*a + 1)?;
                    let ctrl = self.get_reg_val(*a + 2)?;

                    prefix_args.push(state);
                    prefix_args.push(ctrl);
                    let result = iter.borrow().call(self, prefix_args, vec![])?;

                    let mut result_iter = result.into_iter();

                    for addr in *a + 4..=*a + 3 + *c {
                        let val = result_iter.next().unwrap_or(LuzObj::Nil);
                        self.scope_mut().set_reg_val(addr, val);
                    }

                    let ctrl = self.get_reg_val(*a + 4)?;
                    self.scope_mut().set_reg_val(*a + 2, ctrl);
                }

                op => todo!("iABC {:?}", op),
            },
            Instruction::iABx(i_abx) => match i_abx.op {
                LuaOpCode::OP_LOADK => self.run_loadk(i_abx)?,
                LuaOpCode::OP_LOADNIL => {
                    let iABx { b, a, .. } = *i_abx;
                    for i in 0..=b {
                        self.scope
                            .borrow_mut()
                            .set_reg_val(a + i as u8, LuzObj::Nil);
                    }
                }
                LuaOpCode::OP_CLOSURE => {
                    let iABx { b, a, .. } = *i_abx;
                    // 1. get les variables locales qui ont une fin et qui sont encore vivantes
                    // 2. close ces variables
                    let sub_scope = Rc::clone(&self.scope().sub_scopes()[b as usize]);
                    let nb_params = sub_scope.borrow().nb_params();
                    self.scope.borrow_mut().set_reg_val(
                        a,
                        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_user(
                            nb_params,
                            sub_scope,
                            self.filename.clone(),
                        )))),
                    );
                }
                LuaOpCode::OP_FORPREP => {
                    let iABx { a, b, .. } = *i_abx;
                    let LuzObj::Numeral(init) = self.get_reg_val(a)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };
                    let LuzObj::Numeral(limit) =
                        self.get_reg_val(a + 1)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };
                    let LuzObj::Numeral(step) = self.get_reg_val(a + 2)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };

                    // set the control variable to init
                    self.scope_mut()
                        .set_reg_val(a + 3, LuzObj::Numeral(init.clone()));

                    if step == Numeral::Int(0) {
                        Err(LuzRuntimeError::message("step cannot be 0."))?
                    }

                    if step > Numeral::Int(0) {
                        // we don't do the loop
                        if init > limit {
                            return Ok(InstructionResult::Jmp(b as i32 + 1));
                        }
                    } else {
                        // we don't do the loop
                        if init < limit {
                            return Ok(InstructionResult::Jmp(b as i32 + 1));
                        }
                    }
                }

                LuaOpCode::OP_FORLOOP => {
                    let iABx { a, b, .. } = *i_abx;
                    let LuzObj::Numeral(test) = self.get_reg_val(a)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };
                    let LuzObj::Numeral(limit) =
                        self.get_reg_val(a + 1)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };
                    let LuzObj::Numeral(step) = self.get_reg_val(a + 2)?.coerse(LuzType::Number)?
                    else {
                        unreachable!()
                    };

                    // augment the test
                    // TODO: prevent wrap around
                    let test = test + step;

                    self.scope_mut().set_reg_val(a, LuzObj::Numeral(test));
                    self.scope_mut().set_reg_val(a + 3, LuzObj::Numeral(test));

                    if step > Numeral::Int(0) {
                        // we continue the loop
                        if test <= limit {
                            return Ok(InstructionResult::Jmp(-(b as i32)));
                        }
                    } else {
                        // we continue the loop
                        if test >= limit {
                            return Ok(InstructionResult::Jmp(-(b as i32)));
                        }
                    }
                }

                LuaOpCode::OP_TFORPREP => {
                    let iABx { a, b, .. } = *i_abx;

                    let iter = self.get_reg_val(a)?;
                    let state = self.get_reg_val(a + 1)?;
                    let init = self.get_reg_val(a + 2)?;
                    let close = self.get_reg_val(a + 3)?;

                    return Ok(InstructionResult::Jmp(b as i32));
                }

                LuaOpCode::OP_TFORLOOP => {
                    let iABx { a, b, .. } = *i_abx;
                    let val = self.get_reg_val(a + 2)?;

                    if !val.is_nil() {
                        // self.scope_mut().set_reg_val(a, val);
                        return Ok(InstructionResult::Jmp(-(b as i32) + 1));
                    }
                }

                op => todo!("iABx {:?}", op),
            },
            Instruction::iAsBx(i_asbx) => match i_asbx.op {
                LuaOpCode::OP_LOADI => {
                    let iAsBx { b, a, .. } = *i_asbx;
                    let imm = (b as i64) - MAX_HALF_sBx as i64;
                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, LuzObj::Numeral(Numeral::Int(imm)));
                }
                LuaOpCode::OP_LOADF => {
                    let iAsBx { b, a, .. } = *i_asbx;
                    let imm = (b as f64) - MAX_HALF_sBx as f64;
                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, LuzObj::Numeral(Numeral::Float(imm)));
                }
                op => todo!("iAsBx {:?}", op),
            },
            Instruction::isJ(is_j) => match is_j.op {
                LuaOpCode::OP_JMP => {
                    let isJ { j: b, .. } = *is_j;
                    let offset = b - MAX_HALF_sJ;
                    return Ok(InstructionResult::Jmp(offset as i32));
                }
                op => todo!("isJ {:?}", op),
            },
            Instruction::iAx(i_ax) => match i_ax.op {
                LuaOpCode::OP_EXTRAARG => {}
                op => todo!("iAx {:?}", op),
            },
            Instruction::NOP => unimplemented!(),
            // IGNORE those
            Instruction::LOG(_) => {}
        }
        Ok(InstructionResult::Continue)
    }

    fn run_arithmetic(&mut self, code: &iABC) -> Result<(), LuzError> {
        let iABC { c, b, k, a, op } = *code;

        let lhs = self.get_reg_val_or_const(b, k)?;
        let rhs = self.get_reg_val_or_const(
            c,
            matches!(
                op,
                LuaOpCode::OP_ADDK
                    | LuaOpCode::OP_SUBK
                    | LuaOpCode::OP_MULK
                    | LuaOpCode::OP_DIVK
                    | LuaOpCode::OP_IDIVK
                    | LuaOpCode::OP_BANDK
                    | LuaOpCode::OP_BORK
                    | LuaOpCode::OP_BXORK
                    | LuaOpCode::OP_MODK
                    | LuaOpCode::OP_POWK
            ),
        )?;

        let result = match op {
            LuaOpCode::OP_ADD | LuaOpCode::OP_ADDK => lhs.apply_binop(Binop::Add, rhs)?,
            LuaOpCode::OP_SUB | LuaOpCode::OP_SUBK => lhs.apply_binop(Binop::Sub, rhs)?,
            LuaOpCode::OP_MUL | LuaOpCode::OP_MULK => lhs.apply_binop(Binop::Mul, rhs)?,
            LuaOpCode::OP_DIV | LuaOpCode::OP_DIVK => lhs.apply_binop(Binop::FloatDiv, rhs)?,
            LuaOpCode::OP_IDIV | LuaOpCode::OP_IDIVK => lhs.apply_binop(Binop::FloorDiv, rhs)?,
            LuaOpCode::OP_MOD | LuaOpCode::OP_MODK => lhs.apply_binop(Binop::Mod, rhs)?,
            LuaOpCode::OP_POW | LuaOpCode::OP_POWK => lhs.apply_binop(Binop::Exp, rhs)?,
            LuaOpCode::OP_SHL => lhs.apply_binop(Binop::ShiftLeft, rhs)?,
            LuaOpCode::OP_SHR => lhs.apply_binop(Binop::ShiftRight, rhs)?,
            LuaOpCode::OP_BAND | LuaOpCode::OP_BANDK => lhs.apply_binop(Binop::BitAnd, rhs)?,
            LuaOpCode::OP_BOR | LuaOpCode::OP_BORK => lhs.apply_binop(Binop::BitOr, rhs)?,
            LuaOpCode::OP_BXOR | LuaOpCode::OP_BXORK => lhs.apply_binop(Binop::BitXor, rhs)?,
            _ => unreachable!(),
        };

        self.scope.borrow_mut().set_reg_val(a, result);

        Ok(())
    }

    fn run_loadk(&mut self, code: &iABx) -> Result<(), LuzError> {
        let iABx { b, a, op } = *code;

        let constant = self.scope.borrow().constants()[b as usize].clone();
        self.scope.borrow_mut().set_reg_val(a, constant);

        Ok(())
    }

    fn run_add(&mut self, code: &iABC) -> Result<(), LuzError> {
        let iABC { c, b, k, a, op } = *code;

        let lhs = if k {
            self.scope.borrow_mut().constants()[b as usize].clone()
        } else {
            self.scope
                .borrow_mut()
                .get_reg(b)
                .val
                .as_ref()
                .expect("A value")
                .clone()
        };

        let rhs = self.scope.borrow_mut().regs()[c as usize]
            .val
            .as_ref()
            .expect("A value")
            .clone();

        self.scope
            .borrow_mut()
            .set_reg_val(a, lhs.apply_binop(Binop::Add, rhs)?);

        Ok(())
    }

    pub fn vararg(&self) -> Option<&Vec<LuzObj>> {
        self.vararg.as_ref()
    }
    pub fn set_vararg(&mut self, vararg: Option<Vec<LuzObj>>) {
        self.vararg = vararg;
    }

    pub fn registry(&self) -> Rc<RefCell<Table>> {
        Rc::clone(&self.registry)
    }

    pub fn input(&self) -> &'a str {
        self.input
    }

    pub fn depth(&self) -> usize {
        self.depth
    }

    pub fn set_depth(&mut self, depth: usize) {
        self.depth = depth;
    }
}
