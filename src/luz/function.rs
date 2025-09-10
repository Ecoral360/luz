use core::fmt;
use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    compiler::Scope,
    luz::obj::LuzObj,
};

#[derive(Debug, Clone, Builder)]
pub struct FuncParams {
    #[builder(default = vec![])]
    pub fixed: Vec<String>,
    #[builder(default = false)]
    pub is_vararg: bool,
}

#[derive(Clone, new)]
pub enum LuzFunction {
    User {
        scope: Rc<RefCell<Scope>>,
    },
    Native {
        fn_ptr: Rc<RefCell<dyn FnMut(Vec<LuzObj>) -> Vec<LuzObj>>>,
    },
}

impl fmt::Debug for LuzFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::User { .. } => write!(f, "user function()"),
            Self::Native { .. } => write!(f, "native function()"),
        }
    }
}
