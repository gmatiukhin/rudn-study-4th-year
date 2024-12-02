use core::panic;
use std::{
    fmt::{Debug, Display},
    num::NonZero,
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign},
};

use crate::util::*;

#[derive(Debug, Clone, Copy)]
pub struct Fraction {
    numerator: i64,
    denominator: NonZero<u32>,
}

impl Fraction {
    pub fn new(numerator: i64, denominator: u32) -> Option<Self> {
        Some(
            Self {
                numerator,
                denominator: NonZero::new(denominator)?,
            }
            .normalized(),
        )
    }

    pub fn normalized(self) -> Self {
        let gcd = gcd(self.numerator.abs(), self.denominator.get() as i64);
        Self {
            numerator: self.numerator / gcd,
            denominator: NonZero::new(self.denominator.get() / gcd as u32).unwrap(),
        }
    }

    pub fn normalize(&mut self) {
        let gcd = gcd(self.numerator.abs(), self.denominator.get() as i64);

        self.numerator /= gcd;
        self.denominator = NonZero::new(self.denominator.get() / gcd as u32).unwrap();
    }

    pub fn reciprocal(self) -> Self {
        if self.numerator == 0 {
            panic!("Division by zero!");
        }

        Self {
            numerator: self.denominator.get() as i64,
            denominator: NonZero::new(self.numerator as u32).unwrap(),
        }
    }
}

impl Display for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl Add for Fraction {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut ret = Self {
            numerator: self.numerator,
            denominator: self.denominator,
        };
        ret += rhs;
        ret.normalized()
    }
}

impl AddAssign for Fraction {
    fn add_assign(&mut self, rhs: Self) {
        self.numerator += rhs.numerator;
        if self.denominator != rhs.denominator {
            self.denominator = lcm_non_zero(self.denominator, rhs.denominator);
        }
        self.normalize();
    }
}

impl Sub for Fraction {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut ret = Self {
            numerator: self.numerator,
            denominator: self.denominator,
        };
        ret -= rhs;
        ret.normalized()
    }
}

impl SubAssign for Fraction {
    fn sub_assign(&mut self, rhs: Self) {
        self.numerator -= rhs.numerator;
        if self.denominator != rhs.denominator {
            self.denominator = lcm_non_zero(self.denominator, rhs.denominator);
        }
        self.normalize();
    }
}

impl Mul for Fraction {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut ret = Self {
            numerator: self.numerator,
            denominator: self.denominator,
        };
        ret *= rhs;
        ret.normalized()
    }
}

impl MulAssign for Fraction {
    fn mul_assign(&mut self, rhs: Self) {
        self.numerator *= rhs.numerator;
        self.denominator =
            NonZero::new(u32::from(self.denominator) * u32::from(rhs.denominator)).unwrap();
        self.normalize();
    }
}

impl Div for Fraction {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let mut ret = Self {
            numerator: self.numerator,
            denominator: self.denominator,
        };
        ret /= rhs;
        ret.normalized()
    }
}

impl DivAssign for Fraction {
    #[allow(clippy::suspicious_op_assign_impl)]
    fn div_assign(&mut self, rhs: Self) {
        let rec = rhs.reciprocal();
        *self *= rec;
    }
}
