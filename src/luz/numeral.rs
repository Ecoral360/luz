use std::{
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub},
    str::FromStr,
};

use super::{
    err::LuzError,
    obj::{LuzObj, LuzType},
};

#[derive(Debug, Clone, Copy)]
pub enum Numeral {
    Int(i64),
    Float(f64),
}

impl Numeral {
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(_))
    }
    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float(_))
    }
}

impl PartialOrd for Numeral {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.cmp(r0),
            (Self::Float(l0), Self::Float(r0)) => l0.partial_cmp(r0)?,
            (Self::Int(r0), Self::Float(l0)) | (Self::Float(l0), Self::Int(r0)) => {
                if (l0.floor() == *l0) && (l0.floor() as i64 == *r0) {
                    std::cmp::Ordering::Equal
                } else {
                    l0.partial_cmp(&(*r0 as f64))?
                }
            }
        })
    }
}

impl PartialEq for Numeral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Int(r0), Self::Float(l0)) | (Self::Float(l0), Self::Int(r0)) => {
                (l0.floor() == *l0) && (l0.floor() as i64 == *r0)
            }
        }
    }
}

macro_rules! impl_op {
    ($([$op_name:ident, $func:ident, $op:tt])*) => {$(
        impl $op_name for Numeral {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Numeral::Int(num1), Numeral::Int(num2)) => Numeral::Int(num1 $op num2),
                    (Numeral::Float(num2), Numeral::Int(num1))
                    | (Numeral::Int(num1), Numeral::Float(num2)) => Numeral::Float(num1 as f64 $op num2),
                    (Numeral::Float(num1), Numeral::Float(num2)) => Numeral::Float(num1 $op num2),
                }
            }
        }
    )*};
}

macro_rules! impl_op_int {
    ($([$op_name:ident, $func:ident, $op:tt])*) => {$(
        impl $op_name for Numeral {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Numeral::Int(num1), Numeral::Int(num2)) => Numeral::Int(num1 $op num2),
                    (Numeral::Float(num2), Numeral::Int(num1))
                    | (Numeral::Int(num1), Numeral::Float(num2)) => Numeral::Int(num1 $op num2 as i64),
                    (Numeral::Float(num1), Numeral::Float(num2)) => Numeral::Int((num1 as i64) $op (num2 as i64)),
                }
            }
        }
    )*};
}

impl_op! {
    [Add, add, +]
    [Mul, mul, *]
    [Sub, sub, -]
    [Div, div, /]
    [Rem, rem, %]
}

impl_op_int! {
    [BitOr, bitor, |]
    [BitXor, bitxor, ^]
    [BitAnd, bitand, &]
    [Shl, shl, <<]
    [Shr, shr, >>]
}

impl Numeral {
    #[inline]
    pub fn to_lossy_int(self) -> Self {
        match self {
            Numeral::Int(_) => self,
            Numeral::Float(f) => Self::Int(f as i64),
        }
    }

    /// Only coverts a float into an int if the fractional part of the float
    /// is `0`, else returns an [LuzError::InvalidCoersion]
    #[inline]
    pub fn try_to_int(self) -> Result<Self, LuzError> {
        match self {
            Numeral::Int(_) => Ok(self),
            Numeral::Float(f) if f.fract() == 0.0 => Ok(Self::Int(f as i64)),
            this @ Numeral::Float(_) => Err(LuzError::InvalidCoersion {
                obj: this.into(),
                ty: LuzType::Integer,
            }),
        }
    }

    #[inline]
    pub fn to_float(self) -> Self {
        match self {
            Numeral::Int(i) => Self::Float(i as f64),
            Numeral::Float(_) => self,
        }
    }

    pub fn pow(self, rhs: Self) -> Self {
        match (self.to_float(), rhs.to_float()) {
            (Numeral::Float(f1), Numeral::Float(f2)) => Self::Float(f1.powf(f2)),
            _ => unreachable!("Called to_float beforehand"),
        }
    }
}

impl Display for Numeral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let to_string = match self {
            Numeral::Int(i) => format!("{i}"),
            Numeral::Float(f) => format!("{f}"),
        };
        write!(f, "{}", to_string)
    }
}

impl FromStr for Numeral {
    type Err = LuzError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut string = s.trim();
        let sign = if string.starts_with('-') {
            string = &string[1..];
            -1
        } else {
            1
        };

        if let Ok(int) = string.parse::<i64>() {
            Ok(Numeral::Int(sign * int).into())
        } else if let Ok(float) = string.parse::<f64>() {
            Ok(Numeral::Float(sign as f64 * float).into())
        } else if string.to_lowercase().starts_with("0x") {
            if let Some(dot_pos) = string.find(&['.', ',']) {
                // Decimal hex number
                let int_part = &string[2..dot_pos];
                let frac_part = &string[dot_pos + 1..];
                let int = if int_part.is_empty() {
                    0
                } else {
                    i64::from_str_radix(int_part, 16)
                        .map_err(|_| LuzError::NumberParsing(string.to_string()))?
                };
                let frac = if frac_part.is_empty() {
                    0.0
                } else {
                    let denom = 16f64.powi(frac_part.len() as i32);
                    let numer = i64::from_str_radix(frac_part, 16)
                        .map_err(|_| LuzError::NumberParsing(string.to_string()))?;
                    (numer as f64) / denom
                };
                Ok(Numeral::Float(sign as f64 * (int as f64 + frac)).into())
            } else {
                if let Ok(hex) = i64::from_str_radix(&string[2..], 16) {
                    Ok(Numeral::Int(sign * hex).into())
                } else {
                    Err(LuzError::NumberParsing(string.to_string()))?
                }
            }
        } else {
            todo!("Number conversion of {:?}", s)
        }
    }
}

impl Neg for Numeral {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Numeral::Int(i) => Numeral::Int(-i),
            Numeral::Float(f) => Numeral::Float(-f),
        }
    }
}

impl Not for Numeral {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Numeral::Int(i) => Numeral::Int(!i),
            Numeral::Float(f) => Numeral::Int(!(f as i64)),
        }
    }
}

impl From<&str> for LuzObj {
    fn from(value: &str) -> Self {
        Self::String(String::from(value))
    }
}
