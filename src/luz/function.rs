use core::fmt;
use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    ast::LineInfo,
    compiler::ctx::ScopeRef,
    luz::obj::LuzObj,
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
        fn_ptr: Rc<
            RefCell<dyn Fn(&mut Runner, Vec<LuzObj>) -> Result<Vec<LuzObj>, LuzRuntimeError>>,
        >,
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
}

impl fmt::Debug for LuzFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User { .. } => write!(f, "user function()"),
            Self::Native { .. } => write!(f, "native function()"),
        }
    }
}
