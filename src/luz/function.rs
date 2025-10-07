use core::fmt;
use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;
use log::{debug, trace};

use crate::{
    compiler::{
        ctx::{RegisterBuilder, Scope, ScopeRef, Upvalue},
        instructions::{self, Instruction},
    },
    luz::{
        function,
        lib::env::get_builtin_scope,
        obj::{AsUTF8Unchecked, LuzObj, Numeral},
    },
    runner::{err::LuzRuntimeError, Runner},
};

#[derive(Debug, Clone, Builder, new)]
pub struct FuncParams {
    #[builder(default = vec![])]
    pub fixed: Vec<String>,
    #[builder(default = false)]
    pub is_vararg: bool,
}

#[derive(Clone, new)]
pub enum LuzFunction {
    User {
        nb_fixed_params: u32,
        scope: ScopeRef,
        filename: String,
    },
    Native {
        nb_fixed_params: u32,
        scope: Option<ScopeRef>,
        fn_ptr:
            Rc<RefCell<dyn Fn(&mut Runner, Vec<LuzObj>) -> Result<Vec<LuzObj>, LuzRuntimeError>>>,
    },
}

impl LuzFunction {
    pub fn scope(&self) -> Option<ScopeRef> {
        let scope = match self {
            LuzFunction::User { scope, .. } => Some(scope),
            LuzFunction::Native { scope, .. } => scope.as_ref(),
        };
        scope.map(|scope| Rc::clone(scope))
    }

    pub fn nb_fixed_params(&self) -> u32 {
        match self {
            LuzFunction::User {
                nb_fixed_params, ..
            } => *nb_fixed_params,
            LuzFunction::Native {
                nb_fixed_params, ..
            } => *nb_fixed_params,
        }
    }

    pub fn tailcall(
        &self,
        runner: &mut Runner,
        mut args: Vec<LuzObj>,
        mut vararg: Vec<LuzObj>,
    ) -> Result<Option<Vec<LuzObj>>, LuzRuntimeError> {
        match self {
            LuzFunction::User { ref scope, .. } => {
                let fc_scope = scope.borrow().make_closure();
                for (i, arg) in args.into_iter().enumerate() {
                    fc_scope.borrow_mut().set_or_push_reg_val(i as u8, arg);
                }
                runner.reset(fc_scope);
                // let mut fc_runner =
                //     Runner::new(String::new(), runner.input(), fc_scope, runner.registry());
                runner.set_vararg(Some(vararg));

                Ok(None)
                // runner
                //     .run()
                //     .map_err(|err| LuzRuntimeError::ErrorObj(LuzObj::str(&err.to_string())))
            }
            LuzFunction::Native { ref fn_ptr, .. } => {
                let fn_ptr = fn_ptr.borrow();
                args.append(&mut vararg);
                Ok(Some((fn_ptr)(runner, args)?))
            }
        }
    }

    pub fn call(
        &self,
        runner: &mut Runner,
        mut args: Vec<LuzObj>,
        mut vararg: Vec<LuzObj>,
    ) -> Result<Vec<LuzObj>, LuzRuntimeError> {
        match self {
            LuzFunction::User {
                ref scope,
                filename,
                ..
            } => {
                let fc_scope = scope.borrow().make_closure();
                for (i, arg) in args.into_iter().enumerate() {
                    fc_scope.borrow_mut().set_reg_val(i as u8, arg);
                }
                let mut fc_runner = Runner::new(
                    filename.clone(),
                    runner.input(),
                    fc_scope,
                    runner.registry(),
                );
                fc_runner.set_depth(runner.depth() + 1);
                fc_runner.set_vararg(Some(vararg));

                fc_runner
                    .run()
                    .map_err(|err| LuzRuntimeError::ErrorObj(LuzObj::str(&err.to_string())))
            }
            LuzFunction::Native { ref fn_ptr, .. } => {
                let fn_ptr = fn_ptr.borrow();
                args.append(&mut vararg);
                (fn_ptr)(runner, args)
            }
        }
    }

    pub fn dump(&self) -> Result<Vec<u8>, LuzRuntimeError> {
        let Self::User {
            // nb_fixed_params,
            scope,
            ..
        } = self
        else {
            return Err(LuzRuntimeError::message(
                "error: cannot do a dump of a native function",
            ));
        };
        // luz magic number : code(141) LUZ code(141)
        let mut dump = vec![141, 76, 85, 90, 141];

        LuzFunction::dump_scope(&mut dump, &scope.borrow(), 0)?;

        Ok(dump)
    }

    fn dump_scope(dump: &mut Vec<u8>, scope: &Scope, depth: usize) -> Result<(), LuzRuntimeError> {
        // number of parameters
        dump.extend(scope.nb_params().to_be_bytes());

        // number of registers
        dump.extend((scope.regs().len() as u32).to_be_bytes());

        // number of upvalues
        dump.extend((scope.upvalues().len() as u32).to_be_bytes());

        // number of subscopes
        dump.extend((scope.sub_scopes().len() as u32).to_be_bytes());

        // number of instructions
        dump.extend((scope.instructions().len() as u32).to_be_bytes());

        // number of constants
        dump.extend((scope.constants().len() as u32).to_be_bytes());
        for constant in scope.constants() {
            match constant {
                LuzObj::String(s) => {
                    // 0 means its a string
                    dump.push(0);
                    // then we push the length of the string
                    dump.extend((s.len() as u32).to_be_bytes());
                    // then we push the bytes of the string
                    dump.extend(s);
                }
                LuzObj::Numeral(Numeral::Int(i)) => {
                    // 1 means its an int
                    dump.push(1);
                    // then we push the bytes of the int
                    dump.extend(i.to_be_bytes());
                }
                LuzObj::Numeral(Numeral::Float(f)) => {
                    // 2 means its a float
                    dump.push(2);
                    // then we push the bytes of the int
                    dump.extend(f.to_be_bytes());
                }
                LuzObj::Boolean(true) => {
                    // 3 means true
                    dump.push(3);
                }
                LuzObj::Boolean(false) => {
                    // 4 means false
                    dump.push(4);
                }
                LuzObj::Nil => {
                    // 5 means nil
                    dump.push(5);
                }
                _ => return Err(LuzRuntimeError::message(
                    "error: cannot do a dump of a non literal constant (how did you do that ???)",
                )),
            }
        }

        let has_env = scope.upvalues().iter().any(|up| up.name == "_ENV");
        for (i, upvalue) in scope.upvalues().iter().enumerate() {
            let is_env = upvalue.name == "_ENV";
            if depth == 0 {
                if is_env {
                    dump.push(0);
                } else if has_env {
                    dump.push(i as u8 + 1);
                } else {
                    dump.push(i as u8);
                }
            } else {
                dump.push(upvalue.parent_addr);
            }
            // flag for in_stack
            if is_env && depth == 0 {
                dump.push(2); // 2 means is_env
            } else {
                dump.push(upvalue.in_stack as u8);
            }

            // we push the len of the name of the upvalue
            dump.extend((upvalue.name.len() as u32).to_be_bytes());
            // we push the name of the upvalue
            dump.extend(upvalue.name.clone().into_bytes());
        }

        for instruction in scope.instructions() {
            let inst_code: u32 = instruction.clone().try_into()?;
            // push the instruction
            dump.extend(inst_code.to_be_bytes());
        }

        for sub_scope in scope.sub_scopes() {
            LuzFunction::dump_scope(dump, &sub_scope.borrow(), depth + 1)?;
        }

        Ok(())
    }

    /// Returns if a bin vector is a luz precompiled dump
    pub fn is_valid_bin(bin: &[u8]) -> bool {
        return !bin.is_empty() && bin[0..5] == [141, 76, 85, 90, 141];
    }

    pub fn load_bin(
        bin: &[u8],
        env: LuzObj,
        name: Option<String>,
    ) -> Result<LuzFunction, LuzRuntimeError> {
        if !LuzFunction::is_valid_bin(&bin) {
            return Err(LuzRuntimeError::message("Invaid luz bin format."));
        }
        let mut ptr = 5;

        let scope = get_builtin_scope();
        scope.borrow_mut().set_reg_val(0, env);
        let scope = LuzFunction::load_scope_bin(&bin, name, &mut ptr, scope)?;
        let num_params = scope.borrow().nb_params();

        trace!("{:#?}", LuzFunctionDump::from(&scope));

        debug!("\n{}", scope.borrow().instructions_to_string());

        let fun = LuzFunction::User {
            nb_fixed_params: num_params,
            scope: scope,
            filename: String::new(),
        };

        Ok(fun)
    }

    fn load_scope_bin(
        bin: &[u8],
        name: Option<String>,
        ptr: &mut usize,
        parent_scope: ScopeRef,
    ) -> Result<ScopeRef, LuzRuntimeError> {
        let num_params = get_next_u32(&bin, ptr);
        let num_regs = get_next_u32(&bin, ptr);
        let num_upvals = get_next_u32(&bin, ptr);
        let num_sub_scopes = get_next_u32(&bin, ptr);
        let num_instructions = get_next_u32(&bin, ptr);
        let num_constants = get_next_u32(&bin, ptr);
        let mut constants = vec![];

        for _ in 0..num_constants {
            let code = get_next_u8(&bin, ptr);
            match code {
                0 => {
                    let str_len = get_next_u32(&bin, ptr);
                    let string = bin[*ptr..*ptr + str_len as usize].to_vec();
                    *ptr += str_len as usize;
                    constants.push(LuzObj::String(string));
                }
                1 => {
                    let i = i64::from_be_bytes(get_next_8bytes(&bin, ptr));
                    constants.push(LuzObj::int(i));
                }
                2 => {
                    let f = f64::from_be_bytes(get_next_8bytes(&bin, ptr));
                    constants.push(LuzObj::float(f));
                }
                3 => {
                    constants.push(LuzObj::Boolean(true));
                }
                4 => {
                    constants.push(LuzObj::Boolean(false));
                }
                5 => {
                    constants.push(LuzObj::Nil);
                }
                _ => return Err(LuzRuntimeError::message("error: invalid constant code")),
            }
        }

        let mut scope = Scope::new(name, Some(parent_scope));
        for _ in 0..num_regs {
            scope.push_reg(&mut RegisterBuilder::default());
        }
        scope.set_constants(constants);

        scope.set_nb_params(num_params);

        for i in 0..num_upvals {
            let parent_addr = get_next_u8(&bin, ptr);
            let flags = get_next_u8(&bin, ptr);
            let in_stack = flags != 0;
            let is_env = flags == 2;

            let upvalue_name_len = get_next_u32(&bin, ptr);
            let upvalue_name = bin[*ptr..*ptr + upvalue_name_len as usize].to_vec();
            *ptr += upvalue_name_len as usize;

            scope.push_upval(Upvalue::new(
                upvalue_name.as_utf8_string_unchecked(),
                i as u8,
                parent_addr,
                in_stack,
            ));

            if !is_env && in_stack {
                {
                    let mut parent = scope.parent().unwrap().borrow_mut();
                    parent.set_or_push_reg_val(parent_addr, LuzObj::Nil);
                }
            }
        }

        let mut instructions = vec![];
        for _ in 0..num_instructions {
            let next_inst = get_next_u32(&bin, ptr);
            instructions.push(Instruction::try_from(next_inst)?);
        }

        scope.set_instructions(instructions);

        let scope = Rc::new(RefCell::new(scope));
        for _ in 0..num_sub_scopes {
            let sub_scope = LuzFunction::load_scope_bin(&bin, None, ptr, Rc::clone(&scope))?;
            scope.borrow_mut().push_sub_scope(sub_scope);
        }

        Ok(scope)
    }
}

fn get_next_u8(vec: &[u8], start: &mut usize) -> u8 {
    *start += 1;
    vec[*start - 1]
}

fn get_next_u32(vec: &[u8], start: &mut usize) -> u32 {
    *start += 4;
    u32::from_be_bytes([
        vec[*start - 4],
        vec[*start - 3],
        vec[*start - 2],
        vec[*start - 1],
    ])
}
fn get_next_8bytes(vec: &[u8], start: &mut usize) -> [u8; 8] {
    *start += 8;
    [
        vec[*start - 8],
        vec[*start - 7],
        vec[*start - 6],
        vec[*start - 5],
        vec[*start - 4],
        vec[*start - 3],
        vec[*start - 2],
        vec[*start - 1],
    ]
}

impl fmt::Debug for LuzFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User { .. } => write!(f, "user function()"),
            Self::Native { .. } => write!(f, "native function()"),
        }
    }
}

#[allow(unused)]
#[derive(Debug)]
pub struct LuzFunctionDump {
    num_params: u32,
    num_regs: u32,
    num_upvalues: u32,
    num_sub_scopes: u32,
    num_instructions: u32,
    num_constants: u32,
    sub_scopes: Vec<LuzFunctionDump>,
}

impl From<&ScopeRef> for LuzFunctionDump {
    fn from(value: &ScopeRef) -> Self {
        let scope = value.borrow();
        Self {
            num_params: scope.nb_params(),
            num_regs: scope.regs().len() as u32,
            num_upvalues: scope.upvalues().len() as u32,
            num_sub_scopes: scope.sub_scopes().len() as u32,
            num_instructions: scope.instructions().len() as u32,
            num_constants: scope.constants().len() as u32,
            sub_scopes: scope
                .sub_scopes()
                .iter()
                .map(|ss| LuzFunctionDump::from(ss))
                .collect(),
        }
    }
}
