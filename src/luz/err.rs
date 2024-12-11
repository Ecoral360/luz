use pest::error::Error as PestError;
use thiserror::Error;

use crate::Rule;

use super::obj::{LuzObj, LuzType};

#[derive(Debug, Error)]
pub enum LuzError {
    #[error("Invalid type {wrong:?}. Expected {expected:?}")]
    Type {
        wrong: LuzType,
        expected: Vec<LuzType>,
    },

    #[error("Cannot coerse {obj:?} into {ty:?}")]
    InvalidCoersion { obj: LuzObj, ty: LuzType },

    #[error("{0:?} is not a valid number")]
    NumberParsing(String),

    #[error("Parsing error: {0:?}")]
    Syntax(Box<PestError<Rule>>),
}

impl From<PestError<Rule>> for LuzError {
    fn from(value: PestError<Rule>) -> Self {
        LuzError::Syntax(Box::new(value))
    }
}
