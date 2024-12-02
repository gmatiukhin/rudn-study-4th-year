use num_traits::PrimInt;
use std::num::{NonZero, ZeroablePrimitive};

pub fn gcd<T>(a: T, b: T) -> T
where
    T: PrimInt,
{
    if a == T::zero() {
        return b;
    }
    gcd(b % a, a)
}

pub fn gcm_non_zero<T>(a: NonZero<T>, b: NonZero<T>) -> NonZero<T>
where
    T: PrimInt + ZeroablePrimitive,
{
    NonZero::new(gcd::<T>(a.get(), b.get())).unwrap()
}

pub fn lcm<T>(a: T, b: T) -> T
where
    T: PrimInt,
{
    (a / gcd(a, b)) * b
}

pub fn lcm_non_zero<T>(a: NonZero<T>, b: NonZero<T>) -> NonZero<T>
where
    T: PrimInt + ZeroablePrimitive,
{
    NonZero::new((a.get() / gcm_non_zero(a, b).get()) * b.get()).unwrap()
}
