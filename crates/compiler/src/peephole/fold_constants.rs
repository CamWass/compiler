use std::{cmp::Ordering, ops::BitXor};

use ast::*;
use num_traits::FromPrimitive;

use crate::{
    node_util::{TypeFlags, expr_may_have_side_effects, getKnownValueType},
    peephole::{
        getSideEffectFreeBigIntValue, getSideEffectFreeBooleanValue, getSideEffectFreeNumberValue,
        getSideEffectFreeStringValue,
    },
};

pub fn evaluateComparison(op: BinaryOp, left: &Expr, right: &Expr) -> Option<bool> {
    // Don't try to minimize side-effects here.
    if expr_may_have_side_effects(left) || expr_may_have_side_effects(right) {
        return None;
    }

    match op {
        BinaryOp::EqEq => {
            return tryAbstractEqualityComparison(left, right);
        }
        BinaryOp::NotEq => {
            return tryAbstractEqualityComparison(left, right).map(|v| !v);
        }
        BinaryOp::EqEqEq => {
            return tryStrictEqualityComparison(left, right);
        }
        BinaryOp::NotEqEq => {
            return tryStrictEqualityComparison(left, right).map(|v| !v);
        }
        BinaryOp::Lt => {
            return tryAbstractRelationalComparison(left, right, false);
        }
        BinaryOp::Gt => {
            return tryAbstractRelationalComparison(right, left, false);
        }
        BinaryOp::LtEq => {
            return tryAbstractRelationalComparison(right, left, true).map(|v| !v);
        }
        BinaryOp::GtEq => {
            return tryAbstractRelationalComparison(left, right, true).map(|v| !v);
        }
        _ => todo!(),
    }
}

/** https://tc39.es/ecma262/#sec-abstract-relational-comparison */
fn tryAbstractRelationalComparison(left: &Expr, right: &Expr, willNegate: bool) -> Option<bool> {
    let leftValueType = getKnownValueType(left);
    let rightValueType = getKnownValueType(right);
    // First, check for a string comparison.
    if leftValueType == TypeFlags::STRING && rightValueType == TypeFlags::STRING {
        let lvStr = getSideEffectFreeStringValue(left);
        let rvStr = getSideEffectFreeStringValue(right);
        if let Some(lvStr) = lvStr
            && let Some(rvStr) = rvStr
        {
            return Some(lvStr < rvStr);
        }

        // TODO: how necessary is this special case?
        if is_equivalent_typeof_ops(left, right) {
            // Special case: `typeof a < typeof a` is always false.
            return Some(false);
        }
    }

    // Next, try to evaluate based on the value of the node. Try comparing as BigInts first.
    let lvBig = getSideEffectFreeBigIntValue(left);
    let rvBig = getSideEffectFreeBigIntValue(right);
    if let Some(lvBig) = &lvBig
        && let Some(rvBig) = &rvBig
    {
        return Some(lvBig < rvBig);
    }

    // Then, try comparing as Numbers.
    let lvNum = getSideEffectFreeNumberValue(left);
    let rvNum = getSideEffectFreeNumberValue(right);
    if let Some(lvNum) = lvNum
        && let Some(rvNum) = rvNum
    {
        if lvNum.is_nan() || rvNum.is_nan() {
            return Some(willNegate);
        } else {
            return Some(lvNum < rvNum);
        }
    }

    // Finally, try comparisons between BigInt and Number.
    if let Some(lvBig) = &lvBig
        && let Some(rvNum) = rvNum
    {
        return bigintLessThanDouble(lvBig, rvNum, false, willNegate);
    }
    if let Some(lvNum) = lvNum
        && let Some(rvBig) = &rvBig
    {
        return bigintLessThanDouble(rvBig, lvNum, true, willNegate);
    }

    // Special case: `x < x` is always false.
    // TODO: If we knew the named value wouldn't be NaN, it would be nice to handle
    // LE and GE. We should use type information if available here.
    if !willNegate
        && let Expr::Ident(left) = left
        && let Expr::Ident(right) = right
    {
        if left.name == right.name {
            return Some(false);
        }
    }

    return None;
}

// TODO: the bitxors arne't that readable.
fn bigintLessThanDouble(
    bigint: &num_bigint::BigInt,
    number: f64,
    invert: bool,
    willNegate: bool,
) -> Option<bool> {
    // if invert is false, then the number is on the right in tryAbstractRelationalComparison
    // if it's true, then the number is on the left
    if number.is_nan() {
        return Some(willNegate);
    } else if number == f64::INFINITY {
        return Some(true.bitxor(invert));
    } else if number == f64::NEG_INFINITY {
        return Some(false.bitxor(invert));
    }

    // long can hold all values within [-2^53, 2^53]
    let numberAsBigInt = num_bigint::BigInt::from_f64(number)?;
    let negativeMeansBigintSmaller = bigint.cmp(&numberAsBigInt);
    if negativeMeansBigintSmaller == Ordering::Less {
        return Some(true.bitxor(invert));
    } else if negativeMeansBigintSmaller == Ordering::Greater {
        return Some(false.bitxor(invert));
    } else if number.fract() == 0.0 && number.abs() <= u64::MAX as f64 {
        return Some(false); // This is the == case, don't invert.
    } else {
        return Some((number.signum() == 1.0).bitxor(invert));
    }
}

/** http://www.ecma-international.org/ecma-262/6.0/#sec-abstract-equality-comparison */
fn tryAbstractEqualityComparison(left: &Expr, right: &Expr) -> Option<bool> {
    // Evaluate based on the general type.
    let leftValueType = getKnownValueType(left);
    let rightValueType = getKnownValueType(right);
    if leftValueType != TypeFlags::UNKNOWN && rightValueType != TypeFlags::UNKNOWN {
        // Delegate to strict equality comparison for values of the same type.
        if leftValueType == rightValueType {
            return tryStrictEqualityComparison(left, right);
        }

        if leftValueType.is_nullish() && rightValueType.is_nullish() {
            return Some(true);
        }

        // TODO: possibly other cases we can add here, since our
        // getKnownValueType can return unions, not just singular types.

        if leftValueType.contains(TypeFlags::FUNCTION)
            || rightValueType.contains(TypeFlags::FUNCTION)
        {
            todo!();
        }

        if leftValueType.bits().count_ones() > 1 || rightValueType.bits().count_ones() > 1 {
            todo!();
        }

        // TODO: this is horrible...
        fn numberNode(value: f64) -> Expr {
            if value.is_nan() {
                Expr::Ident(Ident {
                    node_id: NodeId::DUMMY,
                    name: id_for_built_in!("NaN"),
                })
            } else {
                let number = if value.is_infinite() {
                    Expr::Ident(Ident {
                        node_id: NodeId::DUMMY,
                        name: id_for_built_in!("Infinity"),
                    })
                } else {
                    Expr::Lit(Lit::Num(Number {
                        node_id: NodeId::DUMMY,
                        value,
                    }))
                };
                if value.signum() == -1.0 {
                    Expr::Unary(UnaryExpr {
                        node_id: NodeId::DUMMY,
                        op: UnaryOp::Minus,
                        arg: Box::new(number),
                    })
                } else {
                    number
                }
            }
        }

        if (leftValueType == TypeFlags::NUMBER && rightValueType == TypeFlags::STRING)
            || rightValueType == TypeFlags::BOOLEAN
        {
            let rv = getSideEffectFreeNumberValue(right);
            return if let Some(rv) = rv {
                tryAbstractEqualityComparison(left, &numberNode(rv))
            } else {
                None
            };
        }
        if (leftValueType == TypeFlags::STRING && rightValueType == TypeFlags::NUMBER)
            || leftValueType == TypeFlags::BOOLEAN
        {
            let lv = getSideEffectFreeNumberValue(left);
            return if let Some(lv) = lv {
                tryAbstractEqualityComparison(&numberNode(lv), right)
            } else {
                None
            };
        }

        if leftValueType == TypeFlags::BIG_INT || rightValueType == TypeFlags::BIG_INT {
            let lv = getSideEffectFreeBigIntValue(left);
            let rv = getSideEffectFreeBigIntValue(right);
            if let Some(lv) = lv
                && let Some(rv) = rv
            {
                return Some(lv == rv);
            }
        }

        if (leftValueType == TypeFlags::STRING || leftValueType == TypeFlags::NUMBER)
            && rightValueType == TypeFlags::OBJECT
        {
            return None;
        }
        if leftValueType == TypeFlags::OBJECT
            && (rightValueType == TypeFlags::STRING || rightValueType == TypeFlags::NUMBER)
        {
            return None;
        }

        return Some(false);
    }

    // In general, the rest of the cases cannot be folded.
    None
}

/** http://www.ecma-international.org/ecma-262/6.0/#sec-strict-equality-comparison */
fn tryStrictEqualityComparison(left: &Expr, right: &Expr) -> Option<bool> {
    // First, try to evaluate based on the general type.
    let leftValueType = getKnownValueType(left);
    let rightValueType = getKnownValueType(right);

    if leftValueType != TypeFlags::UNKNOWN && rightValueType != TypeFlags::UNKNOWN {
        // Strict equality can only be true for values of the same type.
        if leftValueType != rightValueType {
            return Some(false);
        }

        match leftValueType {
            TypeFlags::NULL | TypeFlags::UNDEFINED => {
                return Some(true);
            }
            TypeFlags::NUMBER => {
                if isNaN(left) {
                    return Some(false);
                }
                if isNaN(right) {
                    return Some(false);
                }
                let lv = getSideEffectFreeNumberValue(left);
                let rv = getSideEffectFreeNumberValue(right);
                if let Some(lv) = lv
                    && let Some(rv) = rv
                {
                    return Some(lv == rv);
                }
            }
            TypeFlags::STRING => {
                let lv = getSideEffectFreeStringValue(left);
                let rv = getSideEffectFreeStringValue(right);
                if let Some(lv) = lv
                    && let Some(rv) = rv
                {
                    return Some(lv == rv);
                }

                // TODO: how necessary is this special case?
                if is_equivalent_typeof_ops(left, right) {
                    // Special case, typeof a == typeof a is always true.
                    return Some(true);
                }
            }
            TypeFlags::BOOLEAN => {
                let lv = getSideEffectFreeBooleanValue(left);
                let rv = getSideEffectFreeBooleanValue(right);
                if let Some(lv) = lv
                    && let Some(rv) = rv
                {
                    return Some(lv == rv);
                }
            }
            TypeFlags::BIG_INT => {
                let lv = getSideEffectFreeBigIntValue(left);
                let rv = getSideEffectFreeBigIntValue(right);
                if let Some(lv) = lv
                    && let Some(rv) = rv
                {
                    return Some(lv == rv);
                }
            }
            _ => {
                // Symbol, Object, and Function cannot be folded in the general case.
                return None;
            }
        }
    }

    // Then, try to evaluate based on the value of the node. There's only one special case:
    // Any strict equality comparison against NaN returns false.
    if isNaN(left) || isNaN(right) {
        return Some(false);
    }

    None
}

fn isNaN(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(ident) => ident.name == id_for_built_in!("NaN"),
        Expr::Bin(bin) => {
            bin.op == BinaryOp::Div
                && matches!(
                    bin.left.as_ref(),
                    Expr::Lit(Lit::Num(Number { value: 0.0, .. }))
                )
                && matches!(
                    bin.right.as_ref(),
                    Expr::Lit(Lit::Num(Number { value: 0.0, .. }))
                )
        }
        Expr::Member(member) => {
            if !member.computed {
                if let ExprOrSuper::Expr(obj) = &member.obj {
                    if let Expr::Ident(obj) = obj.as_ref() {
                        if let Expr::Ident(prop) = member.prop.as_ref() {
                            return obj.name == id_for_built_in!("Number")
                                && prop.name == id_for_built_in!("NaN");
                        }
                    }
                }
            }

            false
        }
        _ => false,
    }
}

/// Returns true if left and right are both `typeof a` for some identifier `a`.
fn is_equivalent_typeof_ops(left: &Expr, right: &Expr) -> bool {
    if let Expr::Unary(UnaryExpr {
        op: UnaryOp::TypeOf,
        arg: left_arg,
        ..
    }) = left
        && let Expr::Unary(UnaryExpr {
            op: UnaryOp::TypeOf,
            arg: right_arg,
            ..
        }) = right
    {
        if let Expr::Ident(left_ident) = left_arg.as_ref()
            && let Expr::Ident(right_ident) = right_arg.as_ref()
        {
            if left_ident.name == right_ident.name {
                return true;
            }
        }
    }

    false
}
