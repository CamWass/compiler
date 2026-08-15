use ast::Expr;
use atoms::JsWord;
use common::SyntaxContext;
use num_bigint::BigInt;

use crate::node_util::{
    expr_may_have_side_effects, get_boolean_value, get_number_value, getBigIntValue, getStringValue,
};

pub mod fold_constants;
pub mod fuse_stmts;
pub mod remove_dead_code;

/**
 * Returns the number value of the node if it has one and it cannot have side effects.
 *
 * <p>Returns {@code null} otherwise.
 */
pub fn getSideEffectFreeNumberValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<f64> {
    let value = get_number_value(expr, unresolved_ctxt);
    // Calculating the number value, if any, is likely to be faster than calculating side effects,
    // and there are only a very few cases where we can compute a number value, but there could
    // also be side effects. e.g. `void doSomething()` has value NaN, regardless of the behavior
    // of `doSomething()`
    if value.is_some() && expr_may_have_side_effects(expr, unresolved_ctxt) {
        None
    } else {
        value
    }
}

/**
 * Returns the bigint value of the node if it has one and it cannot have side effects.
 *
 * <p>Returns {@code null} otherwise.
 */
pub fn getSideEffectFreeBigIntValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<BigInt> {
    let value = getBigIntValue(expr, unresolved_ctxt);
    // Calculating the bigint value, if any, is likely to be faster than calculating side effects,
    // and there are only a very few cases where we can compute a bigint value, but there could
    // also be side effects. e.g. `void doSomething()` has value NaN, regardless of the behavior
    // of `doSomething()`
    if value.is_some() && expr_may_have_side_effects(expr, unresolved_ctxt) {
        None
    } else {
        value
    }
}

/**
 * Gets the value of a node as a String, or {@code null} if it cannot be converted.
 *
 * <p>This method effectively emulates the <code>String()</code> JavaScript cast function when
 * possible and the node has no side effects. Otherwise, it returns {@code null}.
 */
pub fn getSideEffectFreeStringValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<JsWord> {
    let value = getStringValue(expr, unresolved_ctxt);
    // Calculating the string value, if any, is likely to be faster than calculating side effects,
    // and there are only a very few cases where we can compute a string value, but there could
    // also be side effects. e.g. `void doSomething()` has value 'undefined', regardless of the
    // behavior of `doSomething()`
    if value.is_some() && expr_may_have_side_effects(expr, unresolved_ctxt) {
        None
    } else {
        value
    }
}

/**
 * Calculate the known boolean value for a node if possible and if it has no side effects.
 *
 * <p>Returns {@link Tri#UNKNOWN} if the node has side effects or its value cannot be statically
 * determined.
 */
pub fn getSideEffectFreeBooleanValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<bool> {
    let value = get_boolean_value(expr, unresolved_ctxt);
    // Calculating the boolean value, if any, is likely to be faster than calculating side effects,
    // and there are only a very few cases where we can compute a boolean value, but there could
    // also be side effects. e.g. `void doSomething()` has value `false`, regardless of the
    // behavior of `doSomething()`
    if value.is_some() && expr_may_have_side_effects(expr, unresolved_ctxt) {
        None
    } else {
        value
    }
}
