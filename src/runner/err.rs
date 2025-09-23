use thiserror::Error;

use crate::luz::{err::LuzError, obj::LuzObj};

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

impl From<LuzError> for LuzRuntimeError {
    fn from(value: LuzError) -> Self {
        LuzRuntimeError::message(value.to_string())
    }
}
