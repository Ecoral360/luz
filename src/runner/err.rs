use thiserror::Error;

use crate::luz::obj::LuzObj;

#[derive(Debug, Error)]
pub enum LuzRuntimeError {
    #[error("{0}")]
    ErrorObj(LuzObj),

    #[error("{0}")]
    Crashing(String),
}

impl LuzRuntimeError {
    pub fn message<S: ToString>(s: S) -> Self {
        Self::ErrorObj(LuzObj::String(s.to_string()))
    }
}
