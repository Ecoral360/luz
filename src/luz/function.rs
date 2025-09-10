use std::{cell::RefCell, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::compiler::Scope;

#[derive(Debug, Clone, Builder)]
pub struct FuncParams {
    #[builder(default = vec![])]
    pub fixed: Vec<String>,
    #[builder(default = false)]
    pub is_vararg: bool,
}



#[derive(Debug, Clone, new)]
pub struct LuzFunction {
    pub scope: Rc<RefCell<Scope>>,
}
