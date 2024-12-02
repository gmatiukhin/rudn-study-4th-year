// Класс Дробное число со знаком (Fractions). Число должно быть представлено двумя полями:
// целая часть - длинное целое со знаком,
// дробная часть - беззнаковое короткое целое.
// Реализовать арифметические операции сложения, вычитания, умножения и операции сравнения. В функции main проверить эти методы.

#![feature(nonzero_internals)]
#![feature(stmt_expr_attributes)]
mod fraction;
mod util;

use std::panic;

use fraction::Fraction;

fn main() {
    let one = Fraction::new(1, 1).unwrap();
    println!("1 as a fraction: {}.\n", one);

    let f = Fraction::new(10, 2).unwrap();
    println!("10/2 as a fraction: {}. Wow! It's normalized!", f);
    println!("Reciprocal of the same fraction: {}.\n", f.reciprocal());

    let zero = Fraction::new(0, 1).unwrap();
    println!("0 as a fraction: {}.", zero);
    println!("Trying to get reciprocal: ");
    let res = panic::catch_unwind(|| {
        println!("{}", zero.reciprocal());
    });
    if res.is_err() {
        println!("Whoops! There was a panic.\n");
    }

    let f2 = Fraction::new(3, 8).unwrap();
    println!("Addition:");
    println!("\t{} + {} = {}.", f, f2, f + f2);
    println!("\t{} + {} = {}.", f, f2.reciprocal(), f + f2.reciprocal());
    println!("\t{} + {} = {}.\n", f.reciprocal(), f2, f.reciprocal() + f2);
    println!("Substraction:");
    println!("\t{} - {} = {}.", f, f2, f - f2);
    println!("\t{} - {} = {}.", f, f2.reciprocal(), f - f2.reciprocal());
    println!("\t{} - {} = {}.\n", f.reciprocal(), f2, f.reciprocal() - f2);
    println!("Multiplication:");
    println!("\t{} * {} = {}.", f, f2, f * f2);
    println!("\t{} * {} = {}.", f, f2.reciprocal(), f * f2.reciprocal());
    println!("\t{} * {} = {}.\n", f.reciprocal(), f2, f.reciprocal() * f2);
    println!("Division:");
    println!("\t{} / {} = {}.", f, f2, f / f2);
    println!("\t{} / {} = {}.", f, f2.reciprocal(), f / f2.reciprocal());
    println!("\t{} / {} = {}.\n", f.reciprocal(), f2, f.reciprocal() / f2);
}
