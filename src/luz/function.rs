use core::fmt;
use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    compiler::{
        ctx::{RegisterBuilder, Scope, ScopeRef, Upvalue},
        instructions::{self, Instruction},
    },
    luz::obj::{LuzObj, Numeral},
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
        fn_ptr:
            Rc<RefCell<dyn Fn(&mut Runner, Vec<LuzObj>) -> Result<Vec<LuzObj>, LuzRuntimeError>>>,
    },
}

impl LuzFunction {
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

    pub fn dump(&self) -> Result<String, LuzRuntimeError> {
        let Self::User {
            nb_fixed_params,
            scope,
            ..
        } = self
        else {
            return Err(LuzRuntimeError::message(
                "error: cannot do a dump of a native function",
            ));
        };
        let mut dump = vec![];
        // number of parameters
        dump.extend(nb_fixed_params.to_be_bytes());

        let scope = scope.borrow();

        // number of registers
        dump.extend((scope.regs().len() as u32).to_be_bytes());

        // number of upvalues
        dump.extend((scope.upvalues().len() as u32).to_be_bytes());

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
                    dump.extend(s.clone().into_bytes());
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

        for instruction in scope.instructions() {
            let inst_code: u32 = instruction.clone().try_into()?;
            // push the instruction
            dump.extend(inst_code.to_be_bytes());
        }

        Ok(unsafe { String::from_utf8_unchecked(dump) })
    }

    pub fn load_bin(&self, bin: Vec<u8>, env: LuzObj) -> Result<LuzFunction, LuzRuntimeError> {
        let mut ptr = 0;
        let num_params = get_next_u32(&bin, &mut ptr);
        let num_regs = get_next_u32(&bin, &mut ptr);
        let num_upvals = get_next_u32(&bin, &mut ptr);
        let num_instructions = get_next_u32(&bin, &mut ptr);
        let num_contants = get_next_u32(&bin, &mut ptr);
        let mut constants = vec![];

        for _ in 0..num_contants {
            let code = get_next_u8(&bin, &mut ptr);
            match code {
                0 => {
                    let str_len = get_next_u32(&bin, &mut ptr);
                    let string =
                        unsafe { String::from_utf8_unchecked(bin[ptr..str_len as usize].to_vec()) };
                    ptr += str_len as usize;
                    constants.push(LuzObj::String(string));
                }
                1 => {
                    let i = i64::from_be_bytes(get_next_8bytes(&bin, &mut ptr));
                    constants.push(LuzObj::int(i));
                }
                2 => {
                    let f = f64::from_be_bytes(get_next_8bytes(&bin, &mut ptr));
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

        let mut scope = Scope::new(None, None);
        for _ in 0..num_regs {
            scope.push_reg(&mut RegisterBuilder::default());
        }
        scope.set_constants(constants);

        for i in 0..num_upvals {
            scope.push_upval(Upvalue::new(String::new(), i as u8, 0, true));
        }

        let mut instructions = vec![];
        for _ in 0..num_instructions {
            let next_inst = get_next_u32(&bin, &mut ptr);
            instructions.push(Instruction::try_from(next_inst)?);
        }

        scope.set_instructions(instructions);

        let fun = LuzFunction::User {
            nb_fixed_params: num_params,
            scope: Rc::new(RefCell::new(scope)),
            filename: String::new(),
        };

        Ok(fun)
    }
}

fn get_next_u8(vec: &Vec<u8>, start: &mut usize) -> u8 {
    *start += 1;
    vec[*start - 1]
}

fn get_next_u32(vec: &Vec<u8>, start: &mut usize) -> u32 {
    *start += 4;
    u32::from_be_bytes([
        vec[*start - 4],
        vec[*start - 3],
        vec[*start - 2],
        vec[*start - 1],
    ])
}
fn get_next_8bytes(vec: &Vec<u8>, start: &mut usize) -> [u8; 8] {
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
