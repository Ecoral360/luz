use core::fmt;
use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    compiler::ctx::ScopeRef,
    luz::{err::LuzError, obj::LuzObj},
    runner::Runner,
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
    },
    Native {
        nb_fixed_params: u32,
        fn_ptr: Rc<RefCell<dyn FnMut(&mut Runner, Vec<LuzObj>, Vec<LuzObj>) -> Result<Vec<LuzObj>, LuzError>>>,
    },
}

impl LuzFunction {
    pub fn nb_fixed_params(&self) -> u32 {
        match self {
            LuzFunction::User { nb_fixed_params, .. } => *nb_fixed_params,
            LuzFunction::Native { nb_fixed_params, .. } => *nb_fixed_params,
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
