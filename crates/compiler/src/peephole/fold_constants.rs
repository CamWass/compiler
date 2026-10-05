use std::{cmp::Ordering, ops::BitXor};

use ast::*;
use big_int::BigIntValue;
use common::{DUMMY_SP, util::take::Take};
use visit::{VisitMut, VisitMutWith};

use crate::{
    node_util::{
        TypeFlags, expr_may_have_side_effects, getKnownValueType,
        isObjectDefinePropertiesDefinition, numberNode,
    },
    peephole::{
        getSideEffectFreeBigIntValue, getSideEffectFreeBooleanValue, getSideEffectFreeNumberValue,
        getSideEffectFreeStringValue,
    },
    utils::unwrap_as,
};

/// When `late` is false, this mean we are currently running before most of the
/// other optimisations. In this case we would avoid optimisations that would
/// make the code harder to analyse. When `late` is true, we would do anything
/// to minimise for size.
pub fn process(ast: &mut Program, program_data: &mut TransformerProgramData, late: bool) {
    let mut visitor = Visitor {
        program_data,
        _late: late,
    };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut TransformerProgramData,
    _late: bool,
}

impl Visitor<'_> {
    /// Remove useless `Object.defineProperties` calls e.g
    /// `Object.defineProperties(o, {})` -> `o`.
    fn tryFoldUselessObjectDotDefinePropertiesCall(&mut self, n: &mut Expr) {
        let call = unwrap_as!(n, Expr::Call(c), c);

        if isObjectDefinePropertiesDefinition(call) {
            let srcObj = &call.args[1];

            fn is_empty_obj(node: &ExprOrSpread) -> bool {
                if let ExprOrSpread::Expr(expr) = node {
                    if let Expr::Object(obj) = expr.as_ref() {
                        return obj.props.is_empty();
                    }
                }

                false
            }

            if is_empty_obj(srcObj) {
                let destObj = unwrap_as!(&mut call.args[0], ExprOrSpread::Expr(e), e);
                *n = destObj.as_mut().take();
            }
        }
    }

    fn tryConvertToNumber(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Lit(Lit::Num(_)) => {
                // Nothing to do.
                return;
            }
            Expr::Bin(BinExpr {
                op: BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::NullishCoalescing,
                right,
                ..
            }) => {
                self.tryConvertToNumber(right);
                return;
            }
            Expr::Seq(seq) => {
                assert!(!seq.exprs.is_empty());
                self.tryConvertToNumber(seq.exprs.last_mut().unwrap());
            }

            Expr::Cond(cond) => {
                self.tryConvertToNumber(&mut cond.cons);
                self.tryConvertToNumber(&mut cond.alt);
                return;
            }
            Expr::Ident(ident) => {
                if ident.name != id_for_built_in!("undefined") {
                    return;
                }
            }
            _ => {}
        }

        let Some(value) = getSideEffectFreeNumberValue(expr) else {
            return;
        };

        let replacement = numberNode(value, Some(expr.node_id()), self.program_data);
        if replacement.eq_ignoring_node_id(expr) {
            return;
        }

        *expr = replacement;
    }
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        node.visit_mut_children_with(self);

        match node {
            Expr::Call(_) => self.tryFoldUselessObjectDotDefinePropertiesCall(node),
            // TODO:
            // case NEW -> tryFoldCtorCall(subtree);
            // TODO:
            // case TYPEOF -> tryFoldTypeof(subtree);
            // TODO:
            // case ITER_SPREAD -> tryFoldSpread(subtree);
            // TODO:
            // case ARRAYLIT, OBJECTLIT -> tryFlattenArrayOrObjectLit(subtree);
            // TODO:
            // case NOT, POS, NEG, BITNOT -> {
            //     tryReduceOperandsForOp(subtree);
            //     yield tryFoldUnaryOperator(subtree);
            // }
            Expr::Unary(unary) => match unary.op {
                UnaryOp::Minus => {
                    self.tryConvertToNumber(&mut unary.arg);

                    if let Expr::Ident(Ident {
                        name: id_for_built_in!("NaN"),
                        ..
                    }) = unary.arg.as_ref()
                    {
                        // "-NaN" is "NaN".
                        *node = unary.arg.as_mut().take();
                    } else if let Expr::Unary(UnaryExpr {
                        op: UnaryOp::Minus,
                        arg: inner_arg,
                        ..
                    }) = unary.arg.as_mut()
                    {
                        if matches!(inner_arg.as_ref(), Expr::Lit(Lit::Num(_) | Lit::BigInt(_))) {
                            // `-(-4)` is `4`.
                            *node = inner_arg.as_mut().take();
                        }
                    }
                }
                UnaryOp::Plus => todo!(),
                UnaryOp::Bang => todo!(),
                UnaryOp::Tilde => todo!(),
                UnaryOp::TypeOf => todo!(),
                UnaryOp::Void => {
                    let is_void_zero = matches!(
                        unary.arg.as_ref(),
                        Expr::Lit(Lit::Num(Number { value: 0.0, .. }))
                    );
                    if !is_void_zero && !expr_may_have_side_effects(&unary.arg) {
                        *unary.arg.as_mut() = Expr::Lit(Lit::Num(Number {
                            node_id: self.program_data.new_id(DUMMY_SP),
                            value: 0.0,
                        }))
                    }
                }
                UnaryOp::Delete => todo!(),
            },
            // TODO:
            // case OPTCHAIN_GETPROP, GETPROP -> tryFoldGetProp(subtree);
            // TODO:
            // case TEMPLATELIT -> tryFoldTemplateLiteralSubstitutions(subtree);
            // TODO:
            // default -> {
            //     tryReduceOperandsForOp(subtree);
            //     yield tryFoldBinaryOperator(subtree);
            // }
            _ => {}
        }
    }
}

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

// TODO: the bitxors aren't that readable.
fn bigintLessThanDouble(
    bigint: &BigIntValue,
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
    let numberAsBigInt = BigIntValue::from_f64(number)?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolve;

    //     #[test]
    //     fn testUndefinedComparison1() {
    //         test_transform("undefined == undefined", "true");
    //         test_transform("undefined == null", "true");
    //         test_transform("undefined == void 0", "true");

    //         test_transform("undefined == 0", "false");
    //         test_transform("undefined == 1", "false");
    //         test_transform("undefined == 'hi'", "false");
    //         test_transform("undefined == true", "false");
    //         test_transform("undefined == false", "false");

    //         test_transform("undefined === undefined", "true");
    //         test_transform("undefined === null", "false");
    //         test_transform("undefined === void 0", "true");

    //         test_same("undefined == this");
    //         test_same("undefined == x");

    //         test_transform("undefined != undefined", "false");
    //         test_transform("undefined != null", "false");
    //         test_transform("undefined != void 0", "false");

    //         test_transform("undefined != 0", "true");
    //         test_transform("undefined != 1", "true");
    //         test_transform("undefined != 'hi'", "true");
    //         test_transform("undefined != true", "true");
    //         test_transform("undefined != false", "true");

    //         test_transform("undefined !== undefined", "false");
    //         test_transform("undefined !== void 0", "false");
    //         test_transform("undefined !== null", "true");

    //         test_same("undefined != this");
    //         test_same("undefined != x");

    //         test_transform("undefined < undefined", "false");
    //         test_transform("undefined > undefined", "false");
    //         test_transform("undefined >= undefined", "false");
    //         test_transform("undefined <= undefined", "false");

    //         test_transform("0 < undefined", "false");
    //         test_transform("true > undefined", "false");
    //         test_transform("'hi' >= undefined", "false");
    //         test_transform("null <= undefined", "false");

    //         test_transform("undefined < 0", "false");
    //         test_transform("undefined > true", "false");
    //         test_transform("undefined >= 'hi'", "false");
    //         test_transform("undefined <= null", "false");

    //         test_transform("null == undefined", "true");
    //         test_transform("0 == undefined", "false");
    //         test_transform("1 == undefined", "false");
    //         test_transform("'hi' == undefined", "false");
    //         test_transform("true == undefined", "false");
    //         test_transform("false == undefined", "false");
    //         test_transform("null === undefined", "false");
    //         test_transform("void 0 === undefined", "true");

    //         test_transform("undefined == NaN", "false");
    //         test_transform("NaN == undefined", "false");
    //         test_transform("undefined == Infinity", "false");
    //         test_transform("Infinity == undefined", "false");
    //         test_transform("undefined == -Infinity", "false");
    //         test_transform("-Infinity == undefined", "false");
    //         test_transform("({}) == undefined", "false");
    //         test_transform("undefined == ({})", "false");
    //         test_transform("([]) == undefined", "false");
    //         test_transform("undefined == ([])", "false");
    //         test_transform("(/a/g) == undefined", "false");
    //         test_transform("undefined == (/a/g)", "false");
    //         test_transform("(function(){}) == undefined", "false");
    //         test_transform("undefined == (function(){})", "false");

    //         test_transform("undefined != NaN", "true");
    //         test_transform("NaN != undefined", "true");
    //         test_transform("undefined != Infinity", "true");
    //         test_transform("Infinity != undefined", "true");
    //         test_transform("undefined != -Infinity", "true");
    //         test_transform("-Infinity != undefined", "true");
    //         test_transform("({}) != undefined", "true");
    //         test_transform("undefined != ({})", "true");
    //         test_transform("([]) != undefined", "true");
    //         test_transform("undefined != ([])", "true");
    //         test_transform("(/a/g) != undefined", "true");
    //         test_transform("undefined != (/a/g)", "true");
    //         test_transform("(function(){}) != undefined", "true");
    //         test_transform("undefined != (function(){})", "true");

    //         test_same("this == undefined");
    //         test_same("x == undefined");
    //     }

    //     #[test]
    //     fn testUndefinedComparison2() {
    //         test_transform("\"123\" !== void 0", "true");
    //         test_transform("\"123\" === void 0", "false");

    //         test_transform("void 0 !== \"123\"", "true");
    //         test_transform("void 0 === \"123\"", "false");
    //     }

    //     #[test]
    //     fn testUndefinedComparison3() {
    //         test_transform("\"123\" !== undefined", "true");
    //         test_transform("\"123\" === undefined", "false");

    //         test_transform("undefined !== \"123\"", "true");
    //         test_transform("undefined === \"123\"", "false");
    //     }

    //     #[test]
    //     fn testUndefinedComparison4() {
    //         test_transform("1 !== void 0", "true");
    //         test_transform("1 === void 0", "false");

    //         test_transform("null !== void 0", "true");
    //         test_transform("null === void 0", "false");

    //         test_transform("undefined !== void 0", "false");
    //         test_transform("undefined === void 0", "true");
    //     }

    //     #[test]
    //     fn testNullComparison1() {
    //         test_transform("null == undefined", "true");
    //         test_transform("null == null", "true");
    //         test_transform("null == void 0", "true");

    //         test_transform("null == 0", "false");
    //         test_transform("null == 1", "false");
    //         test_transform("null == 0n", "false");
    //         test_transform("null == 1n", "false");
    //         test_transform("null == 'hi'", "false");
    //         test_transform("null == true", "false");
    //         test_transform("null == false", "false");

    //         test_transform("null === undefined", "false");
    //         test_transform("null === null", "true");
    //         test_transform("null === void 0", "false");
    //         test_same("null === x");

    //         test_same("null == this");
    //         test_same("null == x");

    //         test_transform("null != undefined", "false");
    //         test_transform("null != null", "false");
    //         test_transform("null != void 0", "false");

    //         test_transform("null != 0", "true");
    //         test_transform("null != 1", "true");
    //         test_transform("null != 0n", "true");
    //         test_transform("null != 1n", "true");
    //         test_transform("null != 'hi'", "true");
    //         test_transform("null != true", "true");
    //         test_transform("null != false", "true");

    //         test_transform("null !== undefined", "true");
    //         test_transform("null !== void 0", "true");
    //         test_transform("null !== null", "false");

    //         test_same("null != this");
    //         test_same("null != x");

    //         test_transform("null < null", "false");
    //         test_transform("null > null", "false");
    //         test_transform("null >= null", "true");
    //         test_transform("null <= null", "true");

    //         test_transform("0 < null", "false");
    //         test_transform("0 > null", "false");
    //         test_transform("0 >= null", "true");
    //         test_transform("0n < null", "false");
    //         test_transform("0n > null", "false");
    //         test_transform("0n >= null", "true");
    //         test_transform("true > null", "true");
    //         test_transform("'hi' < null", "false");
    //         test_transform("'hi' >= null", "false");
    //         test_transform("null <= null", "true");

    //         test_transform("null < 0", "false");
    //         test_transform("null < 0n", "false");
    //         test_transform("null > true", "false");
    //         test_transform("null < 'hi'", "false");
    //         test_transform("null >= 'hi'", "false");
    //         test_transform("null <= null", "true");

    //         test_transform("null == null", "true");
    //         test_transform("0 == null", "false");
    //         test_transform("1 == null", "false");
    //         test_transform("'hi' == null", "false");
    //         test_transform("true == null", "false");
    //         test_transform("false == null", "false");
    //         test_transform("null === null", "true");
    //         test_transform("void 0 === null", "false");

    //         test_transform("null == NaN", "false");
    //         test_transform("NaN == null", "false");
    //         test_transform("null == Infinity", "false");
    //         test_transform("Infinity == null", "false");
    //         test_transform("null == -Infinity", "false");
    //         test_transform("-Infinity == null", "false");
    //         test_transform("({}) == null", "false");
    //         test_transform("null == ({})", "false");
    //         test_transform("([]) == null", "false");
    //         test_transform("null == ([])", "false");
    //         test_transform("(/a/g) == null", "false");
    //         test_transform("null == (/a/g)", "false");
    //         test_transform("(function(){}) == null", "false");
    //         test_transform("null == (function(){})", "false");

    //         test_transform("null != NaN", "true");
    //         test_transform("NaN != null", "true");
    //         test_transform("null != Infinity", "true");
    //         test_transform("Infinity != null", "true");
    //         test_transform("null != -Infinity", "true");
    //         test_transform("-Infinity != null", "true");
    //         test_transform("({}) != null", "true");
    //         test_transform("null != ({})", "true");
    //         test_transform("([]) != null", "true");
    //         test_transform("null != ([])", "true");
    //         test_transform("(/a/g) != null", "true");
    //         test_transform("null != (/a/g)", "true");
    //         test_transform("(function(){}) != null", "true");
    //         test_transform("null != (function(){})", "true");

    //         test_same("({a:f()}) == null");
    //         test_same("null == ({a:f()})");
    //         test_same("([f()]) == null");
    //         test_same("null == ([f()])");

    //         test_same("this == null");
    //         test_same("x == null");
    //     }

    //     #[test]
    //     fn testBooleanBooleanComparison() {
    //         test_same("!x == !y");
    //         test_same("!x < !y");
    //         test_same("!x !== !y");

    //         test_same("!x == !x"); // foldable
    //         test_same("!x < !x"); // foldable
    //         test_same("!x !== !x"); // foldable
    //     }

    //     #[test]
    //     fn testBooleanNumberComparison() {
    //         test_same("!x == +y");
    //         test_same("!x <= +y");
    //         test_transform("!x !== +y", "true");
    //     }

    //     #[test]
    //     fn testNumberBooleanComparison() {
    //         test_same("+x == !y");
    //         test_same("+x <= !y");
    //         test_transform("+x === !y", "false");
    //     }

    //     #[test]
    //     fn testBooleanStringComparison() {
    //         test_same("!x == '' + y");
    //         test_same("!x <= '' + y");
    //         test_transform("!x !== '' + y", "true");
    //     }

    //     #[test]
    //     fn testStringBooleanComparison() {
    //         test_same("'' + x == !y");
    //         test_same("'' + x <= !y");
    //         test_transform("'' + x === !y", "false");
    //     }

    //     #[test]
    //     fn testNumberNumberComparison() {
    //         test_transform("1 > 1", "false");
    //         test_transform("2 == 3", "false");
    //         test_transform("3.6 === 3.6", "true");
    //         test_same("+x > +y");
    //         test_same("+x == +y");
    //         test_same("+x === +y");
    //         test_same("+x == +x");
    //         test_same("+x === +x");

    //         test_same("+x > +x"); // foldable
    //     }

    //     #[test]
    //     fn testStringStringComparison() {
    //         test_transform("'a' < 'b'", "true");
    //         test_transform("'a' <= 'b'", "true");
    //         test_transform("'a' > 'b'", "false");
    //         test_transform("'a' >= 'b'", "false");
    //         test_transform("+'a' < +'b'", "false");
    //         test_same("typeof a < 'a'");
    //         test_same("'a' >= typeof a");
    //         test_transform("typeof a < typeof a", "false");
    //         test_transform("typeof a >= typeof a", "true");
    //         test_transform("typeof 3 > typeof 4", "false");
    //         test_transform("typeof function() {} < typeof function() {}", "false");
    //         test_transform("'a' == 'a'", "true");
    //         test_transform("'b' != 'a'", "true");
    //         test_same("'undefined' == typeof a");
    //         test_same("typeof a != 'number'");
    //         test_same("'undefined' == typeof a");
    //         test_same("'undefined' == typeof a");
    //         test_transform("typeof a == typeof a", "true");
    //         test_transform("'a' === 'a'", "true");
    //         test_transform("'b' !== 'a'", "true");
    //         test_transform("typeof a === typeof a", "true");
    //         test_transform("typeof a !== typeof a", "false");
    //         test_same("'' + x <= '' + y");
    //         test_same("'' + x != '' + y");
    //         test_same("'' + x === '' + y");

    //         test_same("'' + x <= '' + x"); // potentially foldable
    //         test_same("'' + x != '' + x"); // potentially foldable
    //         test_same("'' + x === '' + x"); // potentially foldable
    //     }

    //     #[test]
    //     fn testNumberStringComparison() {
    //         test_transform("1 < '2'", "true");
    //         test_transform("2 > '1'", "true");
    //         test_transform("123 > '34'", "true");
    //         test_transform("NaN >= 'NaN'", "false");
    //         test_transform("1 == '2'", "false");
    //         test_transform("1 != '1'", "false");
    //         test_transform("NaN == 'NaN'", "false");
    //         test_transform("1 === '1'", "false");
    //         test_transform("1 !== '1'", "true");
    //         test_same("+x > '' + y");
    //         test_same("+x == '' + y");
    //         test_transform("+x !== '' + y", "true");
    //     }

    //     #[test]
    //     fn testStringNumberComparison() {
    //         test_transform("'1' < 2", "true");
    //         test_transform("'2' > 1", "true");
    //         test_transform("'123' > 34", "true");
    //         test_transform("'NaN' < NaN", "false");
    //         test_transform("'1' == 2", "false");
    //         test_transform("'1' != 1", "false");
    //         test_transform("'NaN' == NaN", "false");
    //         test_transform("'1' === 1", "false");
    //         test_transform("'1' !== 1", "true");
    //         test_same("'' + x < +y");
    //         test_same("'' + x == +y");
    //         test_transform("'' + x === +y", "false");
    //     }

    //     #[test]
    //     fn testBigIntNumberComparison() {
    //         test_transform("1n < 2", "true");
    //         test_transform("1n > 2", "false");
    //         test_transform("1n == 1", "true");
    //         test_transform("1n == 2", "false");

    //         // comparing with decimals is allowed
    //         test_transform("1n < 1.1", "true");
    //         test_transform("1n < 1.9", "true");
    //         test_transform("1n < 0.9", "false");
    //         test_transform("-1n < -1.1", "false");
    //         test_transform("-1n < -1.9", "false");
    //         test_transform("-1n < -0.9", "true");
    //         test_transform("1n > 1.1", "false");
    //         test_transform("1n > 0.9", "true");
    //         test_transform("-1n > -1.1", "true");
    //         test_transform("-1n > -0.9", "false");

    //         // Don't fold unsafely large numbers because there might be floating-point error
    //         let maxSafeInt = 9007199254740991L;
    //         test_transform("0n > " + maxSafeInt, "false");
    //         test_transform("0n < " + maxSafeInt, "true");
    //         test_transform("0n > " + -maxSafeInt, "true");
    //         test_transform("0n < " + -maxSafeInt, "false");
    //         test_same("0n > " + (maxSafeInt + 1L));
    //         test_same("0n < " + (maxSafeInt + 1L));
    //         test_same("0n > " + -(maxSafeInt + 1L));
    //         test_same("0n < " + -(maxSafeInt + 1L));

    //         // comparing with Infinity is allowed
    //         test_transform("1n < Infinity", "true");
    //         test_transform("1n > Infinity", "false");
    //         test_transform("1n < -Infinity", "false");
    //         test_transform("1n > -Infinity", "true");

    //         // null is interpreted as 0 when comparing with bigint
    //         test_transform("1n < null", "false");
    //         test_transform("1n > null", "true");
    //     }

    //     #[test]
    //     fn testBigIntStringComparison() {
    //         test_transform("1n < '2'", "true");
    //         test_transform("1n <= '2'", "true");
    //         test_transform("1n > '2'", "false");
    //         test_transform("1n >= '2'", "false");
    //         test_transform("1n < '1'", "false");
    //         test_transform("1n <= '1'", "true");
    //         test_transform("1n > '1'", "false");
    //         test_transform("1n >= '1'", "true");
    //         test_transform("1n == '1'", "true");
    //         test_transform("1n != '1'", "false");
    //         test_transform("2n > '1'", "true");
    //         test_transform("123n > '34'", "true");
    //         test_transform("1n == '2'", "false");
    //         test_transform("1n === '1'", "false");
    //         test_transform("1n !== '1'", "true");

    //         test_transform("10n == '  10  '", "true");
    //         test_transform("10n < '  20  '", "true");
    //         test_transform("16n == '0x10'", "true");
    //         test_transform("2n == '0b10'", "true");
    //         test_transform("8n == '0o10'", "true");
    //         test_transform("-1n == '-1'", "true");
    //         test_transform("1n == '+1'", "true");

    //         // Invalid BigInt strings (StringToBigInt returns undefined) evaluate to false for all
    //         // relational comparisons
    //         test_transform("1n < 'foo'", "false");
    //         test_transform("1n > 'foo'", "false");
    //         test_transform("1n <= 'foo'", "false");
    //         test_transform("1n >= 'foo'", "false");
    //         test_transform("1n == 'foo'", "false");
    //         test_transform("1n != 'foo'", "true");

    //         test_transform("1n < '1e2'", "false");
    //         test_transform("1n > '1e2'", "false");
    //         test_transform("1n <= '1e2'", "false");
    //         test_transform("1n >= '1e2'", "false");
    //         test_transform("1n == '1e2'", "false");
    //         test_transform("1n != '1e2'", "true");

    //         test_transform("1n < '1.5'", "false");
    //         test_transform("1n > '1.5'", "false");
    //         test_transform("1n <= '1.5'", "false");
    //         test_transform("1n >= '1.5'", "false");
    //         test_transform("1n == '1.5'", "false");
    //         test_transform("1n != '1.5'", "true");

    //         test_transform("1n < 'Infinity'", "false");
    //         test_transform("1n > 'Infinity'", "false");
    //         test_transform("1n <= 'Infinity'", "false");
    //         test_transform("1n >= 'Infinity'", "false");

    //         test_transform("1n < '-Infinity'", "false");
    //         test_transform("1n > '-Infinity'", "false");
    //         test_transform("1n <= '-Infinity'", "false");
    //         test_transform("1n >= '-Infinity'", "false");
    //     }

    //     #[test]
    //     fn testStringBigIntComparison() {
    //         test_transform("'2' < 1n", "false");
    //         test_transform("'2' <= 1n", "false");
    //         test_transform("'2' > 1n", "true");
    //         test_transform("'2' >= 1n", "true");
    //         test_transform("'1' < 1n", "false");
    //         test_transform("'1' <= 1n", "true");
    //         test_transform("'1' > 1n", "false");
    //         test_transform("'1' >= 1n", "true");
    //         test_transform("'1' == 1n", "true");
    //         test_transform("'1' != 1n", "false");
    //         test_transform("'1' < 2n", "true");
    //         test_transform("'123' > 34n", "true");
    //         test_transform("'1' == 2n", "false");
    //         test_transform("'1' === 1n", "false");
    //         test_transform("'1' !== 1n", "true");

    //         test_transform("'  10  ' == 10n", "true");
    //         test_transform("'  20  ' > 10n", "true");
    //         test_transform("'0x10' == 16n", "true");
    //         test_transform("'0b10' == 2n", "true");
    //         test_transform("'0o10' == 8n", "true");
    //         test_transform("'-1' == -1n", "true");
    //         test_transform("'+1' == 1n", "true");

    //         test_transform("'foo' < 1n", "false");
    //         test_transform("'foo' > 1n", "false");
    //         test_transform("'foo' <= 1n", "false");
    //         test_transform("'foo' >= 1n", "false");
    //         test_transform("'foo' == 1n", "false");
    //         test_transform("'foo' != 1n", "true");

    //         test_transform("'1e2' < 1n", "false");
    //         test_transform("'1e2' > 1n", "false");
    //         test_transform("'1e2' <= 1n", "false");
    //         test_transform("'1e2' >= 1n", "false");

    //         test_transform("'1.5' < 1n", "false");
    //         test_transform("'1.5' > 1n", "false");
    //         test_transform("'1.5' <= 1n", "false");
    //         test_transform("'1.5' >= 1n", "false");
    //     }

    //     #[test]
    //     fn testBigIntEqualityWithNullOrUndefined() {
    //         test_transform("null == 1n", "false");
    //         test_transform("undefined == 1n", "false");
    //         test_transform("1n == null", "false");
    //         test_transform("1n == undefined", "false");
    //         test_transform("null != 1n", "true");
    //         test_transform("undefined != 1n", "true");
    //         test_transform("1n != null", "true");
    //         test_transform("1n != undefined", "true");
    //     }

    //     #[test]
    //     fn testBigIntEqualityWithVariables() {
    //         test_same("x == 1n");
    //         test_same("1n == y");
    //         test_same("x != 1n");
    //         test_same("1n != y");
    //     }

    //     #[test]
    //     fn testNaNComparison() {
    //         test_transform("NaN < 1", "false");
    //         test_transform("NaN <= 1", "false");
    //         test_transform("NaN > 1", "false");
    //         test_transform("NaN >= 1", "false");
    //         test_transform("NaN < 1n", "false");
    //         test_transform("NaN <= 1n", "false");
    //         test_transform("NaN > 1n", "false");
    //         test_transform("NaN >= 1n", "false");

    //         test_transform("NaN < NaN", "false");
    //         test_transform("NaN >= NaN", "false");
    //         test_transform("NaN == NaN", "false");
    //         test_transform("NaN === NaN", "false");

    //         test_transform("NaN < null", "false");
    //         test_transform("null >= NaN", "false");
    //         test_transform("NaN == null", "false");
    //         test_transform("null != NaN", "true");
    //         test_transform("null === NaN", "false");

    //         test_transform("NaN < undefined", "false");
    //         test_transform("undefined >= NaN", "false");
    //         test_transform("NaN == undefined", "false");
    //         test_transform("undefined != NaN", "true");
    //         test_transform("undefined === NaN", "false");

    //         test_same("NaN < x");
    //         test_same("x >= NaN");
    //         test_same("NaN == x");
    //         test_same("x != NaN");
    //         test_transform("NaN === x", "false");
    //         test_transform("x !== NaN", "true");
    //         test_same("NaN == foo()");
    //     }

    //     #[test]
    //     fn testObjectComparison1() {
    //         test_transform("!new Date()", "false");
    //         test_transform("!!new Date()", "true");

    //         test_transform("new Date() == null", "false");
    //         test_transform("new Date() == undefined", "false");
    //         test_transform("new Date() != null", "true");
    //         test_transform("new Date() != undefined", "true");
    //         test_transform("null == new Date()", "false");
    //         test_transform("undefined == new Date()", "false");
    //         test_transform("null != new Date()", "true");
    //         test_transform("undefined != new Date()", "true");
    //     }

    // #[test]
    // fn testUnaryOps() {
    //     // Running on just changed code results in an exception on only the first invocation. Don't
    //     // repeat because it confuses the exception verification.
    //     numRepetitions = 1;

    //     // These cases are handled by PeepholeRemoveDeadCode.
    //     test_same("!foo()");
    //     test_same("~foo()");
    //     test_same("-foo()");

    //     // These cases are handled here.
    //     test_transform("a=!true", "a=false");
    //     test_transform("a=!10", "a=false");
    //     test_transform("a=!false", "a=true");
    //     test_same("a=!foo()");
    //     test_transform("a=-0", "a=-0.0");
    //     test_transform("a=-(0)", "a=-0.0");
    //     test_same("a=-Infinity");
    //     test_transform("a=-NaN", "a=NaN");
    //     test_same("a=-foo()");
    //     test_transform("a=~~0", "a=0");
    //     test_transform("a=~~10", "a=10");
    //     test_transform("a=~-7", "a=6");

    //     test_transform("a=+true", "a=1");
    //     test_transform("a=+10", "a=10");
    //     test_transform("a=+false", "a=0");
    //     test_same("a=+foo()");
    //     test_same("a=+f");
    //     test_transform("a=+(f?true:false)", "a=+(f?1:0)"); // TODO(johnlenz): foldable
    //     test_transform("a=+0", "a=0");
    //     test_transform("a=+Infinity", "a=Infinity");
    //     test_transform("a=+NaN", "a=NaN");
    //     test_transform("a=+-7", "a=-7");
    //     test_transform("a=+.5", "a=.5");

    //     test_transform("a=~0xffffffff", "a=0");
    //     test_transform("a=~~0xffffffff", "a=-1");
    //     test_same("a=~.5", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    // }

    #[test]
    fn testUnaryOpsWithBigInt() {
        test_transform("-(1n)", "-1n");
        test_transform("- -1n", "1n");
        // test_transform("!1n", "false");
        // test_transform("~0n", "-1n");
    }

    //     #[test]
    //     fn testUnaryOpsStringCompare() {
    //         test_same("a = -1");
    //         test_transform("a = ~0", "a = -1");
    //         test_transform("a = ~1", "a = -2");
    //         test_transform("a = ~101", "a = -102");
    //     }

    //     #[test]
    //     fn testFoldLogicalOp() {
    //         test_transform("x = true && x", "x = x");
    //         test_transform("x = [foo()] && x", "x = ([foo()],x)");

    //         test_transform("x = false && x", "x = false");
    //         test_transform("x = true || x", "x = true");
    //         test_transform("x = false || x", "x = x");
    //         test_transform("x = 0 && x", "x = 0");
    //         test_transform("x = 3 || x", "x = 3");
    //         test_transform("x = 0n && x", "x = 0n");
    //         test_transform("x = 3n || x", "x = 3n");
    //         test_transform("x = false || 0", "x = 0");

    //         // unfoldable, because the right-side may be the result
    //         test_transform("a = x && true", "a=x && true");
    //         test_transform("a = x && false", "a=x && false");
    //         test_transform("a = x || 3", "a=x || 3");
    //         test_transform("a = x || false", "a=x || false");
    //         test_transform("a = b ? c : x || false", "a=b ? c:x || false");
    //         test_transform("a = b ? x || false : c", "a=b ? x || false:c");
    //         test_transform("a = b ? c : x && true", "a=b ? c:x && true");
    //         test_transform("a = b ? x && true : c", "a=b ? x && true:c");

    //         // folded, but not here.
    //         test_same("a = x || false ? b : c");
    //         test_same("a = x && true ? b : c");

    //         test_transform("x = foo() || true || bar()", "x = foo() || true");
    //         test_transform("x = foo() || true && bar()", "x = foo() || bar()");
    //         test_transform("x = foo() || false && bar()", "x = foo() || false");
    //         test_transform("x = foo() && false && bar()", "x = foo() && false");
    //         test_transform("x = foo() && false || bar()", "x = (foo() && false,bar())");
    //         test_transform("x = foo() || false || bar()", "x = foo() || bar()");
    //         test_transform("x = foo() && true && bar()", "x = foo() && bar()");
    //         test_transform("x = foo() || true || bar()", "x = foo() || true");
    //         test_transform("x = foo() && false && bar()", "x = foo() && false");
    //         test_transform("x = foo() && 0 && bar()", "x = foo() && 0");
    //         test_transform("x = foo() && 1 && bar()", "x = foo() && bar()");
    //         test_transform("x = foo() || 0 || bar()", "x = foo() || bar()");
    //         test_transform("x = foo() || 1 || bar()", "x = foo() || 1");
    //         test_transform("x = foo() && 0n && bar()", "x = foo() && 0n");
    //         test_transform("x = foo() && 1n && bar()", "x = foo() && bar()");
    //         test_transform("x = foo() || 0n || bar()", "x = foo() || bar()");
    //         test_transform("x = foo() || 1n || bar()", "x = foo() || 1n");
    //         test_same("x = foo() || bar() || baz()");
    //         test_same("x = foo() && bar() && baz()");

    //         test_transform("0 || b()", "b()");
    //         test_transform("1 && b()", "b()");
    //         test_transform("a() && (1 && b())", "a() && b()");
    //         test_transform("(a() && 1) && b()", "a() && b()");

    //         test_transform("(x || '') || y;", "x || y");
    //         test_transform("false || (x || '');", "x || ''");
    //         test_transform("(x && 1) && y;", "x && y");
    //         test_transform("true && (x && 1);", "x && 1");

    //         // Really not foldable, because it would change the type of the
    //         // expression if foo() returns something truthy but not true.
    //         // Cf. FoldConstants.tryFoldAndOr().
    //         // An example would be if foo() is 1 (truthy) and bar() is 0 (falsey):
    //         // (1 && true) || 0 == true
    //         // 1 || 0 == 1, but true =/= 1
    //         test_same("x = foo() && true || bar()");
    //         test_same("foo() && true || bar()");
    //     }

    //     #[test]
    //     fn testFoldLogicalOp2() {
    //         test_transform("x = function(){} && x", "x = x");
    //         test_transform("x = true && function(){}", "x = function(){}");
    //         test_transform(
    //             "x = [(function(){alert(x)})()] && x",
    //             "x = ([(function(){alert(x)})()],x)",
    //         );
    //     }

    //     #[test]
    //     fn testFoldNullishCoalesce() {
    //         // fold if left is null/undefined
    //         test_transform("null ?? 1", "1");
    //         test_transform("undefined ?? false", "false");
    //         test_transform("(a(), null) ?? 1", "(a(), null, 1)");

    //         test_transform("x = [foo()] ?? x", "x = [foo()]");

    //         // short circuit on all non nullish LHS
    //         test_transform("x = false ?? x", "x = false");
    //         test_transform("x = true ?? x", "x = true");
    //         test_transform("x = 0 ?? x", "x = 0");
    //         test_transform("x = 3 ?? x", "x = 3");

    //         // unfoldable, because the right-side may be the result
    //         test_same("a = x ?? true");
    //         test_same("a = x ?? false");
    //         test_same("a = x ?? 3");
    //         test_same("a = b ? c : x ?? false");
    //         test_same("a = b ? x ?? false : c");

    //         // folded, but not here.
    //         test_same("a = x ?? false ? b : c");
    //         test_same("a = x ?? true ? b : c");

    //         test_same("x = foo() ?? true ?? bar()");
    //         test_transform("x = foo() ?? (true && bar())", "x = foo() ?? bar()");
    //         test_same("x = (foo() || false) ?? bar()");

    //         test_transform("a() ?? (1 ?? b())", "a() ?? 1");
    //         test_transform("(a() ?? 1) ?? b()", "a() ?? 1 ?? b()");
    //     }

    //     #[test]
    //     fn testFoldOptChain() {
    //         // can't fold when optional part may execute
    //         test_same("a = x?.y");
    //         test_same("a = x?.()");

    //         // fold args of optional call
    //         test_transform("x = foo() ?. (true && bar())", "x = foo() ?.(bar())");
    //         test_transform("a() ?. (1 ?? b())", "a() ?. (1)");

    //         test_transform("({a})?.a.b.c.d()?.x.y.z", "a.b.c.d()?.x.y.z");

    //         // potential optimization
    //         test_same("x = undefined?.y"); // `x = void 0;`
    //     }

    //     #[test]
    //     fn testFoldOptChain_nonNullReceiver() {
    //         // Constant folding on literals with optional chaining
    //         test_transform("x = 'hello'?.length", "x = 5");
    //         test_transform("x = ''?.length", "x = 0");
    //         test_transform("x = [1, 2, 3]?.length", "x = 3");
    //         test_transform("x = []?.length", "x = 0");
    //         test_transform("x = ([10, 20])?.[0]", "x = 10");
    //         test_transform("x = ([10, 20])?.[1]", "x = 20");
    //         test_transform("x = 'abcdef'?.[1]", "x = 'b'");
    //         test_transform("x = ({a: 1})?.a", "x = 1");
    //         test_transform("x = ({a: 1})?.['a']", "x = 1");

    //         // Guard cases / unknown receivers must remain untouched
    //         test_same("x = str?.length");
    //         test_same("x = arr?.length");
    //         test_same("x = obj?.a");
    //         test_same("x = obj?.[key]");
    //         test_same("x = fn?.(arg)");
    //         test_same("x = [foo()]?.length"); // side-effecting array
    //     }

    //     #[test]
    //     fn testFoldOptChain_nullishReceiver() {
    //         // Current compiler behavior in PeepholeFoldConstants (unfolded / preserved)
    //         test_same("x = null?.y");
    //         test_same("x = undefined?.y");
    //         test_same("x = (void 0)?.y");
    //         test_same("x = null?.[0]");
    //         test_same("x = undefined?.[foo()]");
    //         test_same("x = null?.(foo())");
    //         test_same("x = (foo(), null)?.y");
    //     }

    //     #[test]
    //     fn testFoldNullishCoalesce_advanced() {
    //         // Basic nullish LHS folding
    //         test_transform("null ?? 1", "1");
    //         test_transform("undefined ?? false", "false");
    //         test_transform("(void 0) ?? 'default'", "'default'");
    //         test_transform("(a(), null) ?? 1", "(a(), null, 1)");

    //         // Non-nullish LHS folding
    //         test_transform("x = false ?? x", "x = false");
    //         test_transform("x = true ?? x", "x = true");
    //         test_transform("x = 0 ?? x", "x = 0");
    //         test_transform("x = '' ?? x", "x = ''");
    //         test_transform("x = 'hello' ?? x", "x = 'hello'");
    //         test_transform("x = 3 ?? x", "x = 3");
    //         test_transform("x = [1, 2] ?? x", "x = [1, 2]");
    //         test_transform("x = ({a: 1}) ?? x", "x = ({a: 1})");
    //         test_transform("x = [foo()] ?? x", "x = [foo()]");

    //         // Guard cases: RHS cannot be folded if LHS is unknown
    //         test_same("a = x ?? true");
    //         test_same("a = x ?? false");
    //         test_same("a = x ?? 3");
    //         test_same("a = x ?? null");
    //         test_same("a = (foo() ? null : 1) ?? 2");
    //     }

    //     #[test]
    //     fn testFoldLogicalAssignments() {
    //         // Under normalization, logical assignments normalize to short-circuiting conditional assigns
    //         test_same("x || (x = y)");
    //         test_same("x && (x = y)");
    //         test_same("x ?? (x = y)");
    //         test_same("x || (x = false)");
    //         test_same("x && (x = true)");
    //         test_same("x ?? (x = null)");
    //         test_same("obj[foo()] || (obj[foo()] = y)");
    //     }

    //     #[test]
    //     fn testBatchC_templateLiteralsAndSpread() {
    //         // OPP-012: Template literal substitutions and guards
    //         test_transform("`a${'b'}c`", "`abc`");
    //         test_transform("`a${123}c`", "`a123c`");
    //         test_transform("`a${true}c`", "`atruec`");
    //         test_transform("`a${false}c`", "`afalsec`");
    //         test_transform("`${''}a${''}`", "`a`");
    //         test_same("tag`a${'b'}c`");
    //         test_same("`a${foo()}c`");
    //         test_same("`a${x}c`");
    //         test_same("`$${''}{`");

    //         // OPP-013: Object spread constant flattening and guards
    //         test_transform("x = {...{}}", "x = {}");
    //         test_transform("x = {a, ...{}, b}", "x = {a, b}");
    //         test_transform("x = {...{a, b}, c, ...{d, e}}", "x = {a, b, c, d, e}");
    //         test_transform("x = {...{...{a}, b}, c}", "x = {a, b, c}");
    //         test_same("x = {...obj}");
    //         test_same("x = {...foo()}");
    //         test_same("x = {...{get a() { return 1; }}}");
    //         test_same("x = {...{set a(v) { }}}");

    //         // OPP-014: Array spread constant flattening and guards
    //         test_transform("x = [...[]]", "x = []");
    //         test_transform("x = [0, ...[], 1]", "x = [0, 1]");
    //         test_transform("x = [...[0, 1], 2, ...[3, 4]]", "x = [0, 1, 2, 3, 4]");
    //         test_transform("x = [...[...[0], 1], 2]", "x = [0, 1, 2]");
    //         test_transform("foo(...[0], 1)", "foo(0, 1)");
    //         test_transform("foo([...[...[0], 1], 2])", "foo([0, 1, 2])");
    //         test_same("x = [...iter]");
    //         test_same("foo(...iter)");
    //         test_same("x = [...{}]");
    //         test_same("x = {...[]}");
    //     }

    //     #[test]
    //     fn testBatchE_arithmeticNeutralElements() {
    //         // OPP-020: Arithmetic Neutral Elements & Strength Reductions
    //         // Literal double negation
    //         test_transform("x = -(-4)", "x = 4");
    //         test_transform("x = -(-4n)", "x = 4n");

    //         // Guard cases: expressions that must not fold or are preserved
    //         test_same("x = foo() ** 0"); // side-effectful base
    //         test_same("x = bigIntVal ** 1"); // cannot mix BigInt with number 1
    //         test_same("x = (a + b) ** 1"); // non-constant exponentiation
    //         test_same("x = (a + b) ** 0");
    //         test_same("x = -(-(a + b))");
    //         test_same("x = a % 1");
    //     }

    //     #[test]
    //     fn testBatchE_bitwiseNeutralAndAbsorptionIdentities() {
    //         // OPP-021: Bitwise Neutral, Idempotent & Absorption Identities
    //         // Fold constant bitwise operations
    //         test_transform("x = 1.5 | 0", "x = 1");
    //         test_transform("x = 4294967295 | 0", "x = -1");
    //         test_transform("x = -1 & 0", "x = 0");
    //         test_transform("x = 0 & -1", "x = 0");
    //         test_transform("x = ~0", "x = -1");
    //         test_transform("x = ~-7", "x = 6");

    //         // Guard cases: non-constant / side-effectful expressions
    //         test_same("x = foo() ^ foo()"); // side effects cannot be dropped
    //         test_same("x = foo() & 0"); // side effects cannot be dropped
    //         test_same("x = (a | b) | 0");
    //         test_same("x = (a & b) & -1");
    //         test_same("x = (a ^ b) ^ 0");
    //         test_same("x = (a >> b) >> 0");
    //         test_same("x = (a >>> b) >>> 0");
    //         test_same("x = ~~ (a | b)");
    //         test_same("x = (a + b) ^ (a + b)");
    //         test_same("x = (a + b) & 0");
    //         test_same("x = y | 0");
    //     }

    //     #[test]
    //     fn testFoldBitwiseOp() {
    //         test_transform("x = 1 & 1", "x = 1");
    //         test_transform("x = 1 & 2", "x = 0");
    //         test_transform("x = 3 & 1", "x = 1");
    //         test_transform("x = 3 & 3", "x = 3");

    //         test_transform("x = 1 | 1", "x = 1");
    //         test_transform("x = 1 | 2", "x = 3");
    //         test_transform("x = 3 | 1", "x = 3");
    //         test_transform("x = 3 | 3", "x = 3");

    //         test_transform("x = 1 ^ 1", "x = 0");
    //         test_transform("x = 1 ^ 2", "x = 3");
    //         test_transform("x = 3 ^ 1", "x = 2");
    //         test_transform("x = 3 ^ 3", "x = 0");

    //         test_transform("x = -1 & 0", "x = 0");
    //         test_transform("x = 0 & -1", "x = 0");
    //         test_transform("x = 1 & 4", "x = 0");
    //         test_transform("x = 2 & 3", "x = 2");

    //         // make sure we fold only when we are supposed to -- not when doing so would
    //         // lose information or when it is performed on nonsensical arguments.
    //         test_transform("x = 1 & 1.1", "x = 1");
    //         test_transform("x = 1.1 & 1", "x = 1");
    //         test_transform("x = 1 & 3000000000", "x = 0");
    //         test_transform("x = 3000000000 & 1", "x = 0");

    //         // Try some cases with | as well
    //         test_transform("x = 1 | 4", "x = 5");
    //         test_transform("x = 1 | 3", "x = 3");
    //         test_transform("x = 1 | 1.1", "x = 1");
    //         test_same("x = 1 | 3E9");

    //         // these cases look strange because bitwise OR converts unsigned numbers to be signed
    //         test_transform("x = 1 | 3000000001", "x = -1294967295");
    //         test_transform("x = 4294967295 | 0", "x = -1");
    //     }

    //     #[test]
    //     fn testFoldBitwiseOp2() {
    //         test_transform("x = y & 1 & 1", "x = y & 1");
    //         test_transform("x = y & 1 & 2", "x = y & 0");
    //         test_transform("x = y & 3 & 1", "x = y & 1");
    //         test_transform("x = 3 & y & 1", "x = y & 1");
    //         test_transform("x = y & 3 & 3", "x = y & 3");
    //         test_transform("x = 3 & y & 3", "x = y & 3");

    //         test_transform("x = y | 1 | 1", "x = y | 1");
    //         test_transform("x = y | 1 | 2", "x = y | 3");
    //         test_transform("x = y | 3 | 1", "x = y | 3");
    //         test_transform("x = 3 | y | 1", "x = y | 3");
    //         test_transform("x = y | 3 | 3", "x = y | 3");
    //         test_transform("x = 3 | y | 3", "x = y | 3");

    //         test_transform("x = y ^ 1 ^ 1", "x = y ^ 0");
    //         test_transform("x = y ^ 1 ^ 2", "x = y ^ 3");
    //         test_transform("x = y ^ 3 ^ 1", "x = y ^ 2");
    //         test_transform("x = 3 ^ y ^ 1", "x = y ^ 2");
    //         test_transform("x = y ^ 3 ^ 3", "x = y ^ 0");
    //         test_transform("x = 3 ^ y ^ 3", "x = y ^ 0");

    //         test_transform("x = Infinity | NaN", "x=0");
    //         test_transform("x = 12 | NaN", "x=12");
    //     }

    //     #[test]
    //     fn testFoldBitwiseOpWithBigInt() {
    //         test_transform("x = 1n & 1n", "x = 1n");
    //         test_transform("x = 1n & 2n", "x = 0n");
    //         test_transform("x = 3n & 1n", "x = 1n");
    //         test_transform("x = 3n & 3n", "x = 3n");

    //         test_transform("x = 1n | 1n", "x = 1n");
    //         test_transform("x = 1n | 2n", "x = 3n");
    //         test_transform("x = 1n | 3n", "x = 3n");
    //         test_transform("x = 3n | 1n", "x = 3n");
    //         test_transform("x = 3n | 3n", "x = 3n");
    //         test_transform("x = 1n | 4n", "x = 5n");

    //         test_transform("x = 1n ^ 1n", "x = 0n");
    //         test_transform("x = 1n ^ 2n", "x = 3n");
    //         test_transform("x = 3n ^ 1n", "x = 2n");
    //         test_transform("x = 3n ^ 3n", "x = 0n");

    //         test_transform("x = -1n & 0n", "x = 0n");
    //         test_transform("x = 0n & -1n", "x = 0n");
    //         test_transform("x = 1n & 4n", "x = 0n");
    //         test_transform("x = 2n & 3n", "x = 2n");

    //         test_transform("x = 1n & 3000000000n", "x = 0n");
    //         test_transform("x = 3000000000n & 1n", "x = 0n");

    //         // bitwise OR does not affect the sign of a bigint
    //         test_transform("x = 1n | 3000000001n", "x = 3000000001n");
    //         test_transform("x = 4294967295n | 0n", "x = 4294967295n");

    //         test_transform("x = y & 1n & 1n", "x = y & 1n");
    //         test_transform("x = y & 1n & 2n", "x = y & 0n");
    //         test_transform("x = y & 3n & 1n", "x = y & 1n");
    //         test_transform("x = 3n & y & 1n", "x = y & 1n");
    //         test_transform("x = y & 3n & 3n", "x = y & 3n");
    //         test_transform("x = 3n & y & 3n", "x = y & 3n");

    //         test_transform("x = y | 1n | 1n", "x = y | 1n");
    //         test_transform("x = y | 1n | 2n", "x = y | 3n");
    //         test_transform("x = y | 3n | 1n", "x = y | 3n");
    //         test_transform("x = 3n | y | 1n", "x = y | 3n");
    //         test_transform("x = y | 3n | 3n", "x = y | 3n");
    //         test_transform("x = 3n | y | 3n", "x = y | 3n");

    //         test_transform("x = y ^ 1n ^ 1n", "x = y ^ 0n");
    //         test_transform("x = y ^ 1n ^ 2n", "x = y ^ 3n");
    //         test_transform("x = y ^ 3n ^ 1n", "x = y ^ 2n");
    //         test_transform("x = 3n ^ y ^ 1n", "x = y ^ 2n");
    //         test_transform("x = y ^ 3n ^ 3n", "x = y ^ 0n");
    //         test_transform("x = 3n ^ y ^ 3n", "x = y ^ 0n");
    //     }

    //     #[test]
    //     fn testFoldingMixTypesLate() {
    //         late = true;
    //         disableNormalize();
    //         test_transform("x = x + '2'", "x+='2'");
    //         test_transform("x = +x + +'2'", "x = +x + 2");
    //         test_transform("x = x - '2'", "x-=2");
    //         test_transform("x = x ^ '2'", "x^=2");
    //         test_transform("x = '2' ^ x", "x^=2");
    //         test_transform("x = '2' & x", "x&=2");
    //         test_transform("x = '2' | x", "x|=2");

    //         test_transform("x = '2' | y", "x=2|y");
    //         test_transform("x = y | '2'", "x=y|2");
    //         test_transform("x = y | (a && '2')", "x=y|(a&&2)");
    //         test_transform("x = y | (a,'2')", "x=y|(a,2)");
    //         test_transform("x = y | (a?'1':'2')", "x=y|(a?1:2)");
    //         test_transform("x = y | ('x'?'1':'2')", "x=y|('x'?1:2)");
    //     }

    //     #[test]
    //     fn testFoldingMixTypesEarly() {
    //         late = false;
    //         test_same("x = x + '2'");
    //         test_transform("x = +x + +'2'", "x = +x + 2");
    //         test_transform("x = x - '2'", "x = x - 2");
    //         test_transform("x = x ^ '2'", "x = x ^ 2");
    //         test_transform("x = '2' ^ x", "x = 2 ^ x");
    //         test_transform("x = '2' & x", "x = 2 & x");
    //         test_transform("x = '2' | x", "x = 2 | x");

    //         test_transform("x = '2' | y", "x=2|y");
    //         test_transform("x = y | '2'", "x=y|2");
    //         test_transform("x = y | (a && '2')", "x=y|(a&&2)");
    //         test_transform("x = y | (a,'2')", "x=y|(a,2)");
    //         test_transform("x = y | (a?'1':'2')", "x=y|(a?1:2)");
    //         test_transform("x = y | ('x'?'1':'2')", "x=y|('x'?1:2)");
    //     }

    //     #[test]
    //     fn testFoldingAdd1() {
    //         test_transform("x = null + true", "x=1");
    //         test_same("x = a + true");
    //         test_transform("x = '' + {}", "x = '[object Object]'");
    //         test_transform("x = [] + {}", "x = '[object Object]'");
    //         test_transform("x = {} + []", "x = '[object Object]'");
    //         test_transform("x = {} + ''", "x = '[object Object]'");
    //     }

    //     #[test]
    //     fn testFoldingAdd2() {
    //         test_transform("x = false + []", "x='false'");
    //         test_transform("x = [] + true", "x='true'");
    //         test_transform("NaN + []", "'NaN'");
    //     }

    //     #[test]
    //     fn testFoldBitwiseOpStringCompare() {
    //         test_transform("x = -1 | 0", "x = -1");
    //     }

    //     #[test]
    //     fn testFoldBitShifts() {
    //         // Running on just changed code results in an exception on only the first invocation. Don't
    //         // repeat because it confuses the exception verification.
    //         numRepetitions = 1;

    //         test_transform("x = 1 << 0", "x = 1");
    //         test_transform("x = -1 << 0", "x = -1");
    //         test_transform("x = 1 << 1", "x = 2");
    //         test_transform("x = 3 << 1", "x = 6");
    //         test_transform("x = 1 << 8", "x = 256");

    //         test_transform("x = 1 >> 0", "x = 1");
    //         test_transform("x = -1 >> 0", "x = -1");
    //         test_transform("x = 1 >> 1", "x = 0");
    //         test_transform("x = 2 >> 1", "x = 1");
    //         test_transform("x = 5 >> 1", "x = 2");
    //         test_transform("x = 127 >> 3", "x = 15");
    //         test_transform("x = 3 >> 1", "x = 1");
    //         test_transform("x = 3 >> 2", "x = 0");
    //         test_transform("x = 10 >> 1", "x = 5");
    //         test_transform("x = 10 >> 2", "x = 2");
    //         test_transform("x = 10 >> 5", "x = 0");

    //         test_transform("x = 10 >>> 1", "x = 5");
    //         test_transform("x = 10 >>> 2", "x = 2");
    //         test_transform("x = 10 >>> 5", "x = 0");
    //         test_transform("x = -1 >>> 1", "x = 2147483647"); // 0x7fffffff
    //         test_transform("x = -1 >>> 0", "x = 4294967295"); // 0xffffffff
    //         test_transform("x = -2 >>> 0", "x = 4294967294"); // 0xfffffffe
    //         test_transform("x = 0x90000000 >>> 28", "x = 9");

    //         test_transform("x = 0xffffffff << 0", "x = -1");
    //         test_transform("x = 0xffffffff << 4", "x = -16");
    //         test_same("1 << 32");
    //         test_same("1 << -1");
    //         test_same("1 >> 32");
    //         test_same("1.5 << 0", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    //         test_same("1 << .5", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    //         test_same(
    //             "1.5 >>> 0",
    //             PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND,
    //         );
    //         test_same("1 >>> .5", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    //         test_same("1.5 >> 0", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    //         test_same("1 >> .5", PeepholeFoldConstants.FRACTIONAL_BITWISE_OPERAND);
    //     }

    //     #[test]
    //     fn testFoldBitShiftsStringCompare() {
    //         test_transform("x = -1 << 1", "x = -2");
    //         test_transform("x = -1 << 8", "x = -256");
    //         test_transform("x = -1 >> 1", "x = -1");
    //         test_transform("x = -2 >> 1", "x = -1");
    //         test_transform("x = -1 >> 0", "x = -1");
    //     }

    //     #[test]
    //     fn testStringAdd() {
    //         test_transform("x = 'a' + 'bc'", "x = 'abc'");
    //         test_transform("x = 'a' + 5", "x = 'a5'");
    //         test_transform("x = 5 + 'a'", "x = '5a'");
    //         test_transform("x = 'a' + 5n", "x = 'a5'");
    //         test_transform("x = 5n + 'a'", "x = '5a'");
    //         test_transform("x = 'a' + ''", "x = 'a'");
    //         test_transform("x = 'a' + foo()", "x = 'a'+foo()");
    //         test_transform("x = foo() + 'a' + 'b'", "x = foo()+'ab'");
    //         test_transform("x = (foo() + 'a') + 'b'", "x = foo()+'ab'"); // believe it!
    //         test_transform(
    //             "x = foo() + 'a' + 'b' + 'cd' + bar()",
    //             "x = foo()+'abcd'+bar()",
    //         );
    //         test_transform("x = foo() + 2 + 'b'", "x = foo()+2+\"b\""); // don't fold!
    //         test_transform("x = foo() + 'a' + 2", "x = foo()+\"a2\"");
    //         test_transform("x = '' + null", "x = 'null'");
    //         test_transform("x = true + '' + false", "x = 'truefalse'");
    //         test_transform("x = '' + []", "x = ''");
    //         test_transform("x = foo() + 'a' + 1 + 1", "x = foo() + 'a11'");
    //         test_transform("x = 1 + 1 + 'a'", "x = '2a'");
    //         test_transform("x = 1 + 1 + 'a'", "x = '2a'");
    //         test_transform("x = 'a' + (1 + 1)", "x = 'a2'");
    //         test_transform("x = '_' + p1 + '_' + ('' + p2)", "x = '_' + p1 + '_' + p2");
    //         test_transform("x = 'a' + ('_' + 1 + 1)", "x = 'a_11'");
    //         test_transform("x = 'a' + ('_' + 1) + 1", "x = 'a_11'");
    //         test_transform("x = 1 + (p1 + '_') + ('' + p2)", "x = 1 + (p1 + '_') + p2");
    //         test_transform("x = 1 + p1 + '_' + ('' + p2)", "x = 1 + p1 + '_' + p2");
    //         test_transform("x = 1 + 'a' + p1", "x = '1a' + p1");
    //         test_transform("x = (p1 + (p2 + 'a')) + 'b'", "x = (p1 + (p2 + 'ab'))");
    //         test_transform("'a' + ('b' + p1) + 1", "'ab' + p1 + 1");
    //         test_transform("x = 'a' + ('b' + p1 + 'c')", "x = 'ab' + (p1 + 'c')");
    //         test_same("x = 'a' + (4 + p1 + 'a')");
    //         test_same("x = p1 / 3 + 4");
    //         test_same("foo() + 3 + 'a' + foo()");
    //         test_same("x = 'a' + ('b' + p1 + p2)");
    //         test_same("x = 1 + ('a' + p1)");
    //         test_same("x = p1 + '' + p2");
    //         test_same("x = 'a' + (1 + p1)");
    //         test_same("x = (p2 + 'a') + (1 + p1)");
    //         test_same("x = (p2 + 'a') + (1 + p1 + p2)");
    //         test_same("x = (p2 + 'a') + (1 + (p1 + p2))");
    //     }

    //     #[test]
    //     fn testStringAdd_identity() {
    //         enableTypeCheck();
    //         replaceTypesWithColors();
    //         disableCompareJsDoc();
    //         foldStringTypes("x + ''", "x");
    //         foldStringTypes("'' + x", "x");
    //     }

    //     #[test]
    //     fn testIssue821() {
    //         test_same("var a =(Math.random()>0.5? '1' : 2 ) + 3 + 4;");
    //         test_same("var a = ((Math.random() ? 0 : 1) || (Math.random()>0.5? '1' : 2 )) + 3 + 4;");
    //     }

    //     #[test]
    //     fn testFoldConstructor() {
    //         test_transform("x = this[new String('a')]", "x = this['a']");
    //         test_transform("x = ob[new String(12)]", "x = ob['12']");
    //         test_transform("x = ob[new String(false)]", "x = ob['false']");
    //         test_transform("x = ob[new String(null)]", "x = ob['null']");
    //         test_transform("x = 'a' + new String('b')", "x = 'ab'");
    //         test_transform("x = 'a' + new String(23)", "x = 'a23'");
    //         test_transform("x = 2 + new String(1)", "x = '21'");
    //         test_same("x = ob[new String(a)]");
    //         test_same("x = new String('a')");
    //         test_same("x = (new String('a'))[3]");
    //     }

    //     #[test]
    //     fn testFoldArithmetic() {
    //         test_transform("x = 10 + 20", "x = 30");
    //         test_transform("x = 2 / 4", "x = 0.5");
    //         test_transform("x = 2.25 & 3", "x = 2");
    //         test_same("z = x & y");
    //         test_same("x = y & 5");
    //         test_same("x = 1 / 0");
    //         test_transform("x = 3 % 2", "x = 1");
    //         test_transform("x = 3 % -2", "x = 1");
    //         test_transform("x = -1 % 3", "x = -1");
    //         test_same("x = 1 % 0");
    //         test_transform("x = 2 ** 3", "x = 8");
    //         test_transform("x = 2 ** -3", "x = 0.125");
    //         test_same("x = 2 ** 55"); // backs off folding because 2 ** 55 is too large
    //         test_same("x = 3 ** -1"); // backs off because 3**-1 is shorter than 0.3333333333333333
    //     }

    //     #[test]
    //     fn testFoldArithmetic2() {
    //         test_same("x = y + 10 + 20");
    //         test_same("x = y / 2 / 4");
    //         test_transform("x = y & 2.25 & 3", "x = y & 2");
    //         test_same("x = y & 2.25 & z & 3");
    //         test_same("z = x &y");
    //         test_same("x = y & 5");
    //         test_transform("x = y + (z & 24 & 60 & 60 & 1000)", "x = y + (z & 8)");
    //     }

    //     #[test]
    //     fn testFoldArithmetic3() {
    //         test_transform("x = null | undefined", "x = 0");
    //         test_transform("x = null | 1", "x = 1");
    //         test_transform("x = (null - 1)| 2", "x = -1");
    //         test_transform("x = (null + 1) | 2", "x = 3");
    //         test_transform("x = null ** 0", "x = 1");
    //         test_transform("x = (-0) ** 3", "x = -0");
    //     }

    //     #[test]
    //     fn testFoldArithmeticInfinity() {
    //         test_transform("x=-Infinity-2", "x=-Infinity");
    //         test_transform("x=Infinity-2", "x=Infinity");
    //         test_transform("x=Infinity*5", "x=Infinity");
    //         test_transform("x = Infinity ** 2", "x = Infinity");
    //         test_transform("x = Infinity ** -2", "x = 0");
    //     }

    //     #[test]
    //     fn testFoldArithmeticStringComp() {
    //         test_transform("x = 10 - 20", "x = -10");
    //     }

    //     #[test]
    //     fn testNoFoldArithmeticWithSideEffects() {
    //         // can't fold this to "x = y & 6.75" because you can't remove the "sideEffects()" call
    //         test_same("x = y & 2.25 & (sideEffects(), 3)");
    //     }

    //     #[test]
    //     fn testFoldBigIntArithmetic() {
    //         test_transform("x = 1n + 2n", "x = 3n");
    //         test_transform("x = 1n - 2n", "x = -1n");
    //         test_transform("x = 2n * 3n", "x = 6n");
    //         test_transform("x = 6n / 2n", "x = 3n");
    //         test_transform("x = 3n % 2n", "x = 1n");
    //         test_transform("x = 2n ** 3n", "x = 8n");

    //         // The compiler is not designed to fold expressions with an exponent > 2147483647
    //         test_transform("x = 1n ** 2147483647n", "x = 1n");
    //         test_same("x = 1n ** 2147483648n");

    //         test_transform("x = y & 2n & 3n", "x = y & 2n");

    //         // TODO b/361826515: Optimize associative bigint operations
    //         test_same("x = y * 2n * z * 3n");
    //         test_same("x = y + 2n + z + 3n");
    //     }

    //     #[test]
    //     fn testNoFoldBigIntArithmeticWithSideEffects() {
    //         // can't fold this to "x = y * 6.75" because you can't remove the "sideEffects()" call
    //         test_same("x = y * 2n * (sideEffects(), 3n)");
    //     }

    //     #[test]
    //     fn testFoldComparison() {
    //         test_transform("x = 0 == 0", "x = true");
    //         test_transform("x = 1 == 2", "x = false");
    //         test_transform("x = 0n == 0n", "x = true");
    //         test_transform("x = 1n == 2n", "x = false");
    //         test_transform("x = 'abc' == 'def'", "x = false");
    //         test_transform("x = 'abc' == 'abc'", "x = true");
    //         test_transform("x = \"\" == ''", "x = true");
    //         test_transform("x = foo() == bar()", "x = foo()==bar()");

    //         test_transform("x = 1 != 0", "x = true");
    //         test_transform("x = 1 != 1", "x = false");
    //         test_transform("x = 1n != 0n", "x = true");
    //         test_transform("x = 1n != 1n", "x = false");
    //         test_transform("x = 'abc' != 'def'", "x = true");
    //         test_transform("x = 'a' != 'a'", "x = false");

    //         test_transform("x = 1 < 20", "x = true");
    //         test_transform("x = 3 < 3", "x = false");
    //         test_transform("x = 10 > 1.0", "x = true");
    //         test_transform("x = 10 > 10.25", "x = false");
    //         test_transform("x = 1 <= 1", "x = true");
    //         test_transform("x = 1 <= 0", "x = false");
    //         test_transform("x = 0 >= 0", "x = true");
    //         test_transform("x = -1 >= 9", "x = false");

    //         test_transform("x = 1n < 20n", "x = true");
    //         test_transform("x = 3n < 3n", "x = false");
    //         test_transform("x = 10n > 1n", "x = true");
    //         test_transform("x = 10n > 10n", "x = false");
    //         test_transform("x = 1n <= 1n", "x = true");
    //         test_transform("x = 1n <= 0n", "x = false");
    //         test_transform("x = 0n >= 0n", "x = true");
    //         test_transform("x = -1n >= 9n", "x = false");

    //         test_same("x = y == y");
    //         test_transform("x = y < y", "x = false");
    //         test_transform("x = y > y", "x = false");

    //         test_transform("x = true == true", "x = true");
    //         test_transform("x = false == false", "x = true");
    //         test_transform("x = false == null", "x = false");
    //         test_transform("x = false == true", "x = false");
    //         test_transform("x = true == null", "x = false");

    //         test_transform("0 == 0", "true");
    //         test_transform("1 == 2", "false");
    //         test_transform("0n == 0n", "true");
    //         test_transform("1n == 2n", "false");
    //         test_transform("'abc' == 'def'", "false");
    //         test_transform("'abc' == 'abc'", "true");
    //         test_transform("\"\" == ''", "true");
    //         test_same("foo() == bar()");

    //         test_transform("1 != 0", "true");
    //         test_transform("1 != 1", "false");
    //         test_transform("1n != 0n", "true");
    //         test_transform("1n != 1n", "false");
    //         test_transform("'abc' != 'def'", "true");
    //         test_transform("'a' != 'a'", "false");

    //         test_transform("1 < 20", "true");
    //         test_transform("3 < 3", "false");
    //         test_transform("10 > 1.0", "true");
    //         test_transform("10 > 10.25", "false");
    //         test_same("x == x");
    //         test_transform("x < x", "false");
    //         test_transform("x > x", "false");
    //         test_transform("1 <= 1", "true");
    //         test_transform("1 <= 0", "false");
    //         test_transform("0 >= 0", "true");
    //         test_transform("-1 >= 9", "false");

    //         test_transform("1n < 20n", "true");
    //         test_transform("3n < 3n", "false");
    //         test_transform("10n > 1n", "true");
    //         test_transform("10n > 10n", "false");
    //         test_transform("1n <= 1n", "true");
    //         test_transform("1n <= 0n", "false");
    //         test_transform("0n >= 0n", "true");
    //         test_transform("-1n >= 9n", "false");

    //         test_transform("true == true", "true");
    //         test_transform("false == null", "false");
    //         test_transform("false == true", "false");
    //         test_transform("true == null", "false");
    //     }

    //     // ===, !== comparison tests
    //     #[test]
    //     fn testFoldComparison2() {
    //         test_transform("x = 0 === 0", "x = true");
    //         test_transform("x = 1 === 2", "x = false");
    //         test_transform("x = 0n === 0n", "x = true");
    //         test_transform("x = 1n === 2n", "x = false");
    //         test_transform("x = 'abc' === 'def'", "x = false");
    //         test_transform("x = 'abc' === 'abc'", "x = true");
    //         test_transform("x = \"\" === ''", "x = true");
    //         test_transform("x = foo() === bar()", "x = foo()===bar()");

    //         test_transform("x = 1 !== 0", "x = true");
    //         test_transform("x = 1 !== 1", "x = false");
    //         test_transform("x = 1n !== 0n", "x = true");
    //         test_transform("x = 1n !== 1n", "x = false");
    //         test_transform("x = 'abc' !== 'def'", "x = true");
    //         test_transform("x = 'a' !== 'a'", "x = false");

    //         test_transform("x = y === y", "x = y===y");

    //         test_transform("x = true === true", "x = true");
    //         test_transform("x = false === false", "x = true");
    //         test_transform("x = false === null", "x = false");
    //         test_transform("x = false === true", "x = false");
    //         test_transform("x = true === null", "x = false");

    //         test_transform("0 === 0", "true");
    //         test_transform("1 === 2", "false");
    //         test_transform("0n === 0n", "true");
    //         test_transform("1n === 2n", "false");
    //         test_transform("'abc' === 'def'", "false");
    //         test_transform("'abc' === 'abc'", "true");
    //         test_transform("\"\" === ''", "true");
    //         test_same("foo() === bar()");

    //         test_transform("1 === '1'", "false");
    //         test_transform("1 === true", "false");
    //         test_transform("1 !== '1'", "true");
    //         test_transform("1 !== true", "true");

    //         test_transform("1 !== 0", "true");
    //         test_transform("'abc' !== 'def'", "true");
    //         test_transform("'a' !== 'a'", "false");

    //         test_same("x === x");

    //         test_transform("true === true", "true");
    //         test_transform("false === null", "false");
    //         test_transform("false === true", "false");
    //         test_transform("true === null", "false");
    //     }

    //     #[test]
    //     fn testFoldComparison3() {
    //         test_transform("x = !1 == !0", "x = false");

    //         test_transform("x = !0 == !0", "x = true");
    //         test_transform("x = !1 == !1", "x = true");
    //         test_transform("x = !1 == null", "x = false");
    //         test_transform("x = !1 == !0", "x = false");
    //         test_transform("x = !0 == null", "x = false");

    //         test_transform("!0 == !0", "true");
    //         test_transform("!1 == null", "false");
    //         test_transform("!1 == !0", "false");
    //         test_transform("!0 == null", "false");

    //         test_transform("x = !0 === !0", "x = true");
    //         test_transform("x = !1 === !1", "x = true");
    //         test_transform("x = !1 === null", "x = false");
    //         test_transform("x = !1 === !0", "x = false");
    //         test_transform("x = !0 === null", "x = false");

    //         test_transform("!0 === !0", "true");
    //         test_transform("!1 === null", "false");
    //         test_transform("!1 === !0", "false");
    //         test_transform("!0 === null", "false");
    //     }

    //     #[test]
    //     fn testFoldComparison4() {
    //         test_same("[] == false"); // true
    //         test_same("[] == true"); // false
    //         test_same("[0] == false"); // true
    //         test_same("[0] == true"); // false
    //         test_same("[1] == false"); // false
    //         test_same("[1] == true"); // true
    //         test_same("({}) == false"); // false
    //         test_same("({}) == true"); // true
    //     }

    //     #[test]
    //     fn testFoldGetElem1() {
    //         // Running on just changed code results in an exception on only the first invocation. Don't
    //         // repeat because it confuses the exception verification.
    //         numRepetitions = 1;

    //         test_transform("x = [,10][0]", "x = void 0");
    //         test_transform("x = [10, 20][0]", "x = 10");
    //         test_transform("x = [10, 20][1]", "x = 20");

    //         test_same(
    //             "x = [10, 20][0.5]",
    //             PeepholeFoldConstants.INVALID_GETELEM_INDEX_ERROR,
    //         );
    //         test_transform("x = [10, 20][-1]", "x = void 0;");
    //         test_transform("x = [10, 20][2]", "x = void 0;");

    //         // TODO(b/538156673): Fix PeepholeFoldConstants to not fold out-of-bounds GETELEM on arrays with
    //         // side-effect elements
    //         test_same("x = [foo(), 0][1]");
    //         test_transform("x = [0, foo()][1]", "x = foo()");
    //         test_same("x = [0, foo()][0]");
    //         test_same("x = [foo(), 0][-1]");
    //         test_same("x = [0, foo()][-1]");
    //         test_same("x = [foo(), 0][2]");
    //         test_same("x = [0, foo()][2]");
    //         test_same("for([1][0] in {});");
    //     }

    //     /** Optional versions of the above `testFoldGetElem1` tests */
    //     // TODO(b/538156673): Fix PeepholeFoldConstants to not fold out-of-bounds GETELEM on arrays with
    //     // side-effect elements
    //     #[test]
    //     fn testFoldOptChainGetElem1() {
    //         numRepetitions = 1;
    //         test_transform("x = [,10]?.[0]", "x = void 0");
    //         test_transform("x = [10, 20]?.[0]", "x = 10");
    //         test_transform("x = [10, 20]?.[1]", "x = 20");

    //         test_same(
    //             "x = [10, 20]?.[0.5]",
    //             PeepholeFoldConstants.INVALID_GETELEM_INDEX_ERROR,
    //         );
    //         test_transform("x = [10, 20]?.[-1]", "x = void 0;");
    //         test_transform("x = [10, 20]?.[2]", "x = void 0;");

    //         test_same("x = [foo(), 0]?.[1]");
    //         test_transform("x = [0, foo()]?.[1]", "x = foo()");
    //         test_same("x = [0, foo()]?.[0]");
    //         test_same("x = [foo(), 0]?.[-1]");
    //         test_same("x = [0, foo()]?.[-1]");
    //         test_same("x = [foo(), 0]?.[2]");
    //         test_same("x = [0, foo()]?.[2]");
    //     }

    //     #[test]
    //     fn testFoldGetElem2() {
    //         // Running on just changed code results in an exception on only the first invocation. Don't
    //         // repeat because it confuses the exception verification.
    //         numRepetitions = 1;

    //         test_transform("x = 'string'[5]", "x = 'g'");
    //         test_transform("x = 'string'[0]", "x = 's'");
    //         test_transform("x = 's'[0]", "x = 's'");
    //         // TODO:
    //         // test_same("x = '\uD83D\uDCA9'[0]");

    //         test_same(
    //             "x = 'string'[0.5]",
    //             PeepholeFoldConstants.INVALID_GETELEM_INDEX_ERROR,
    //         );
    //         test_transform("x = 'string'[-1]", "x = void 0;");
    //         test_transform("x = 'string'[6]", "x = void 0;");
    //     }

    //     /** Optional versions of the above `testFoldGetElem2` tests */
    //     #[test]
    //     fn testFoldOptChainGetElem2() {
    //         // Running on just changed code results in an exception on only the first invocation. Don't
    //         // repeat because it confuses the exception verification.
    //         numRepetitions = 1;
    //         test_transform("x = 'string'?.[5]", "x = 'g'");
    //         test_transform("x = 'string'?.[0]", "x = 's'");
    //         test_transform("x = 's'?.[0]", "x = 's'");
    //         // TODO:
    //         // test_same("x = '\uD83D\uDCA9'?.[0]");

    //         test_same(
    //             "x = 'string'?.[0.5]",
    //             PeepholeFoldConstants.INVALID_GETELEM_INDEX_ERROR,
    //         );
    //         test_transform("x = 'string'?.[-1]", "x = void 0;");
    //         test_transform("x = 'string'?.[6]", "x = void 0;");
    //     }

    //     #[test]
    //     fn testFoldArrayLitSpreadGetElem() {
    //         numRepetitions = 1;
    //         test_transform("x = [...[0]][0]", "x = 0;");
    //         test_transform("x = [0, 1, ...[2, 3, 4]][3]", "x = 3;");
    //         test_transform("x = [...[0, 1], 2, ...[3, 4]][3]", "x = 3;");
    //         test_transform("x = [...[...[0, 1], 2, 3], 4][0]", "x = 0");
    //         test_transform("x = [...[...[0, 1], 2, 3], 4][3]", "x = 3");
    //         test_transform(srcs("x = [...[]][100]"), expected("x = void 0;"));
    //         test_transform(srcs("x = [...[0]][100]"), expected("x = void 0;"));
    //     }

    //     /** Optional versions of the above `testFoldArrayLitSpreadGetElem` tests */
    //     #[test]
    //     fn testFoldArrayLitSpreadOptChainGetElem() {
    //         numRepetitions = 1;
    //         test_transform("x = [...[0]]?.[0]", "x = 0;");
    //         test_transform("x = [0, 1, ...[2, 3, 4]]?.[3]", "x = 3;");
    //         test_transform("x = [...[0, 1], 2, ...[3, 4]]?.[3]", "x = 3;");
    //         test_transform("x = [...[...[0, 1], 2, 3], 4]?.[0]", "x = 0");
    //         test_transform("x = [...[...[0, 1], 2, 3], 4]?.[3]", "x = 3");
    //         test_transform(srcs("x = [...[]]?.[100]"), expected("x = void 0;"));
    //         test_transform(srcs("x = [...[0]]?.[100]"), expected("x = void 0;"));
    //     }

    //     #[test]
    //     fn testDontFoldNonLiteralSpreadGetElem() {
    //         test_same("x = [...iter][0];");
    //         test_same("x = [0, 1, ...iter][2];");
    //         //  `...iter` could have side effects, so don't replace `x` with `0`
    //         test_same("x = [0, 1, ...iter][0];");
    //     }

    //     #[test]
    //     fn testFoldArraySpread() {
    //         numRepetitions = 1;
    //         test_transform("x = [...[]]", "x = []");
    //         test_transform("x = [0, ...[], 1]", "x = [0, 1]");
    //         test_transform("x = [...[0, 1], 2, ...[3, 4]]", "x = [0, 1, 2, 3, 4]");
    //         test_transform("x = [...[...[0], 1], 2]", "x = [0, 1, 2]");
    //         test_same("[...[x]] = arr");
    //         test_transform("foo([...[...[0], 1], 2])", "foo([0, 1, 2])");
    //         test_same("x = [1, ...[,], 2];");
    //         test_same("x = [0, ...[,,], 3];");
    //         test_same("x = [...[,]];");
    //     }

    //     #[test]
    //     fn testFoldArrayLitSpreadInArg() {
    //         test_transform("foo(...[0], 1)", "foo(0, 1)");
    //         test_same("foo(...[,]);");
    //         test_same("foo(...[,,,,\"foo\"], 1)");
    //         test_same("foo(...(false ? [0] : [1]))"); // other opts need to fold the ternery first
    //     }

    //     #[test]
    //     fn testFoldObjectLitSpreadGetProp() {
    //         numRepetitions = 1;
    //         test_transform("x = {...{a}}.a", "x = a;");
    //         test_transform("x = {a, b, ...{c, d, e}}.d", "x = d;");
    //         test_transform("x = {...{a, b}, c, ...{d, e}}.d", "x = d;");
    //         test_transform("x = {...{...{a, b}, c, d}, e}.a", "x = a");
    //         test_transform("x = {...{...{a, b}, c, d}, e}.d", "x = d");
    //     }

    //     #[test]
    //     fn testDontFoldNonLiteralObjectSpreadGetProp_gettersImpure() {
    //         this.assumeGettersPure = false;

    //         test_same("x = {...obj}.a;");
    //         test_same("x = {a, ...obj, c}.a;");
    //         test_same("x = {a, ...obj, c}.c;");
    //     }

    //     #[test]
    //     fn testDontFoldNonLiteralObjectSpreadGetProp_assumeGettersPure() {
    //         this.assumeGettersPure = true;

    //         test_same("x = {...obj}.a;");
    //         test_same("x = {a, ...obj, c}.a;");
    //         test_transform("x = {a, ...obj, c}.c;", "x = c;"); // We assume object spread has no side-effects.
    //     }

    //     #[test]
    //     fn testFoldObjectSpread() {
    //         numRepetitions = 1;
    //         test_transform("x = {...{}}", "x = {}");
    //         test_transform("x = {a, ...{}, b}", "x = {a, b}");
    //         test_transform("x = {...{a, b}, c, ...{d, e}}", "x = {a, b, c, d, e}");
    //         test_transform("x = {...{...{a}, b}, c}", "x = {a, b, c}");
    //         test_same("({...{x}} = obj)");
    //     }

    //     #[test]
    //     fn testDontFoldObjectSpread_withGetterSetter() {
    //         numRepetitions = 1;

    //         // At runtime, object spread converts getters to regular data properties, and omits setters.
    //         // We back off on folding spread with getters & setters.
    //         // In theory, we could make the compiler smart enough to turn
    //         //   `{...{get a() { return 1; }}}` into `{a: 1}`, or `{...{set a(v) { }}}` into `{}`, but
    //         // it doesn't seem worth the complexity right now.
    //         test_same("x = {...{get a() { return 1; }}}");
    //         test_same("x = {...{set a(v) { }}}");

    //         // Test case with side-effects. If in the future we implement spread folding for getters/setters
    //         // we would need to preserve evaluation of the console.log.
    //         test_same("x = {...{get a() { console.log('hi'); return 1; }}}");
    //     }

    //     #[test]
    //     fn testDontFoldObjectSpread_withComputedGetterSetter() {
    //         numRepetitions = 1;

    //         test_same("x = {...{get [a]() { return 1; }}}");
    //         test_same("x = {...{set [a](v) { }}}");
    //     }

    //     #[test]
    //     fn testDontFoldMixedObjectAndArraySpread() {
    //         numRepetitions = 1;
    //         test_same("x = [...{}]");
    //         test_same("x = {...[]}");
    //         test_transform("x = [a, ...[...{}]]", "x = [a, ...{}]");
    //         test_transform("x = {a, ...{...[]}}", "x = {a, ...[]}");
    //     }

    //     #[test]
    //     fn testFoldComplex() {
    //         test_transform("x = (3 / 1.0) + (1 * 2)", "x = 5");
    //         test_transform("x = (1 == 1.0) && foo() && true", "x = foo()&&true");
    //         test_transform("x = 'abc' + 5 + 10", "x = \"abc510\"");
    //     }

    //     #[test]
    //     fn testFoldLeft() {
    //         test_same("(+x - 1) + 2"); // not yet
    //         test_transform("(+x & 1) & 2", "+x & 0");
    //     }

    //     #[test]
    //     fn testFoldArrayLength() {
    //         // Can fold
    //         test_transform("x = [].length", "x = 0");
    //         test_transform("x = [1,2,3].length", "x = 3");
    //         test_transform("x = [a,b].length", "x = 2");

    //         // Not handled yet
    //         test_transform("x = [,,1].length", "x = 3");

    //         // Cannot fold
    //         test_transform("x = [foo(), 0].length", "x = [foo(),0].length");
    //         test_same("x = y.length");
    //         test_same("[1, 2].length = 0;");
    //         test_transform("[1, 2].length >>= 1;", "[1, 2].length = 1;");
    //         test_same("[1, 2].length++;");
    //         test_same("++[1, 2].length;");
    //         test_same("[1, 2].length--;");
    //         test_same("--[1, 2].length;");
    //         test_same("x = [...'abc'].length");
    //         test_same("x = [1, ...'ab', 3].length");
    //         test_same("x = [...`abc`].length");
    //     }

    //     #[test]
    //     fn testFoldStringLength() {
    //         // Can fold basic strings.
    //         test_transform("x = ''.length", "x = 0");
    //         test_transform("x = '123'.length", "x = 3");

    //         // TODO:
    //         // Test Unicode escapes are accounted for.
    //         // test_transform("x = '123\u01dc'.length", "x = 4");

    //         // Cannot fold when length is an lvalue
    //         test_same("\"a\".length = 1;");
    //         test_transform("\"a\".length >>= 1;", "\"a\".length = 0;");
    //         test_same("\"a\".length++;");
    //         test_same("++\"a\".length;");
    //         test_same("\"a\".length--;");
    //         test_same("--\"a\".length;");
    //     }

    //     #[test]
    //     fn testFoldTypeof() {
    //         test_transform("x = typeof 1", "x = \"number\"");
    //         test_transform("x = typeof 'foo'", "x = \"string\"");
    //         test_transform("x = typeof true", "x = \"boolean\"");
    //         test_transform("x = typeof false", "x = \"boolean\"");
    //         test_transform("x = typeof null", "x = \"object\"");
    //         test_transform("x = typeof undefined", "x = \"undefined\"");
    //         test_transform("x = typeof void 0", "x = \"undefined\"");
    //         test_transform("x = typeof []", "x = \"object\"");
    //         test_transform("x = typeof [1]", "x = \"object\"");
    //         test_transform("x = typeof [1,[]]", "x = \"object\"");
    //         test_transform("x = typeof {}", "x = \"object\"");
    //         test_transform("x = typeof function() {}", "x = 'function'");

    //         test_same("x = typeof[1,[foo()]]");
    //         test_same("x = typeof{bathwater:baby()}");
    //     }

    //     #[test]
    //     fn testFoldInstanceOf() {
    //         // Non object types are never instances of anything.
    //         test_transform("64 instanceof Object", "false");
    //         test_transform("64 instanceof Number", "false");
    //         test_transform("'' instanceof Object", "false");
    //         test_transform("'' instanceof String", "false");
    //         test_transform("true instanceof Object", "false");
    //         test_transform("true instanceof Boolean", "false");
    //         test_transform("!0 instanceof Object", "false");
    //         test_transform("!0 instanceof Boolean", "false");
    //         test_transform("false instanceof Object", "false");
    //         test_transform("null instanceof Object", "false");
    //         test_transform("undefined instanceof Object", "false");
    //         test_transform("NaN instanceof Object", "false");
    //         test_transform("Infinity instanceof Object", "false");

    //         // Untagged template literals always evaluate to strings.
    //         test_transform("`` instanceof Object", "false");
    //         test_transform("`${[]}` instanceof Object", "false");
    //         test_transform("`${function(){}}` instanceof Object", "false");
    //         // Back off when substitutions have side-effects.
    //         test_same("`${(console.log(0), [])}` instanceof Object");

    //         // Tagged template literals may evaluate to a non-string, though. Would require type information
    //         // to fold.
    //         test_same("tag`${function(){}}` instanceof Object");

    //         // Array and object literals are known to be objects.
    //         test_transform("[] instanceof Object", "true");
    //         test_transform("({}) instanceof Object", "true");

    //         // These cases is foldable, but no handled currently.
    //         test_same("new Foo() instanceof Object");
    //         // These would require type information to fold.
    //         test_same("[] instanceof Foo");
    //         test_same("({}) instanceof Foo");

    //         test_transform("(function() {}) instanceof Object", "true");

    //         // An unknown value should never be folded.
    //         test_same("x instanceof Foo");
    //     }

    //     #[test]
    //     fn testDivision() {
    //         // Make sure the 1/3 does not expand to 0.333333
    //         test_same("print(1/3)");

    //         // Decimal form is preferable to fraction form when strings are the
    //         // same length.
    //         test_transform("print(1/2)", "print(0.5)");
    //     }

    //     #[test]
    //     fn testAssignOpsLate() {
    //         late = true;
    //         disableNormalize();
    //         test_transform("x=x+y", "x+=y");
    //         test_same("x=y+x");
    //         test_transform("x=x*y", "x*=y");
    //         test_transform("x=y*x", "x*=y");
    //         test_transform("x.y=x.y+z", "x.y+=z");
    //         test_same("next().x = next().x + 1");

    //         test_transform("x=x-y", "x-=y");
    //         test_same("x=y-x");
    //         test_transform("x=x|y", "x|=y");
    //         test_transform("x=y|x", "x|=y");
    //         test_transform("x=x|y|z", "x|=y|z");
    //         test_same("x=x&&y&&z");
    //         test_transform("x=x*y", "x*=y");
    //         test_transform("x=y*x", "x*=y");
    //         test_same("x=foo()*x");
    //         test_same("x=(x=5)*x");
    //         test_same("x=(y++)*x");
    //         test_same("x=foo()|x");
    //         test_same("x=foo()&x");
    //         test_same("x=foo()^x");
    //         test_transform("x=x*foo()", "x*=foo()");
    //         test_transform("x=x|foo()", "x|=foo()");
    //         test_transform("x=x&foo()", "x&=foo()");
    //         test_transform("x=x^foo()", "x^=foo()");
    //         test_transform("x=x+foo()", "x+=foo()");
    //         test_transform("x=x**y", "x**=y");
    //         test_same("x=y**x");
    //         test_transform("x.y=x.y+z", "x.y+=z");
    //         test_same("next().x = next().x + 1");
    //         // This is OK, really.
    //         test_transform("({a:1}).a = ({a:1}).a + 1", "({a:1}).a = 2");
    //     }

    //     #[test]
    //     fn testAssignOpsEarly() {
    //         late = false;
    //         test_same("x=x+y");
    //         test_same("x=y+x");
    //         test_same("x=x*y");
    //         test_same("x=y*x");
    //         test_same("x.y=x.y+z");
    //         test_same("next().x = next().x + 1");

    //         test_same("x=x-y");
    //         test_same("x=y-x");
    //         test_same("x=x|y");
    //         test_same("x=y|x");
    //         test_same("x=x*y");
    //         test_same("x=y*x");
    //         test_same("x=x**y");
    //         test_same("x=y**2");
    //         test_same("x.y=x.y+z");
    //         test_same("next().x = next().x + 1");
    //         // This is OK, really.
    //         test_transform("({a:1}).a = ({a:1}).a + 1", "({a:1}).a = 2");
    //     }

    //     #[test]
    //     fn testUnfoldAssignOpsLate() {
    //         late = true;
    //         disableNormalize();
    //         test_same("x+=y");
    //         test_same("x*=y");
    //         test_same("x.y+=z");
    //         test_same("x-=y");
    //         test_same("x|=y");
    //         test_same("x*=y");
    //         test_same("x**=y");
    //         test_same("x.y+=z");
    //         test_same("\"a\".length >>= 1;");
    //         test_same("[1, 2].length >>= 1;");
    //     }

    //     #[test]
    //     fn testUnfoldAssignOpsEarly() {
    //         late = false;
    //         test_transform("x+=y", "x=x+y");
    //         test_transform("x*=y", "x=x*y");
    //         test_transform("x.y+=z", "x.y=x.y+z");
    //         test_transform("x-=y", "x=x-y");
    //         test_transform("x|=y", "x=x|y");
    //         test_transform("x*=y", "x=x*y");
    //         test_transform("x**=y", "x=x**y");
    //         test_transform("x.y+=z", "x.y=x.y+z");
    //     }

    //     #[test]
    //     fn testFoldAdd1() {
    //         test_transform("x=false+1", "x=1");
    //         test_transform("x=true+1", "x=2");
    //         test_transform("x=1+false", "x=1");
    //         test_transform("x=1+true", "x=2");
    //     }

    //     #[test]
    //     fn testFoldLiteralNames() {
    //         test_transform("NaN == NaN", "false");
    //         test_transform("Infinity == Infinity", "true");
    //         test_transform("Infinity == NaN", "false");
    //         test_transform("undefined == NaN", "false");
    //         test_transform("undefined == Infinity", "false");

    //         test_transform("Infinity >= Infinity", "true");
    //         test_transform("NaN >= NaN", "false");
    //     }

    //     #[test]
    //     fn testFoldLiteralsTypeMismatches() {
    //         test_transform("true == true", "true");
    //         test_transform("true == false", "false");
    //         test_transform("true == null", "false");
    //         test_transform("false == null", "false");

    //         // relational operators convert its operands
    //         test_transform("null <= null", "true"); // 0 = 0
    //         test_transform("null >= null", "true");
    //         test_transform("null > null", "false");
    //         test_transform("null < null", "false");

    //         test_transform("false >= null", "true"); // 0 = 0
    //         test_transform("false <= null", "true");
    //         test_transform("false > null", "false");
    //         test_transform("false < null", "false");

    //         test_transform("true >= null", "true"); // 1 > 0
    //         test_transform("true <= null", "false");
    //         test_transform("true > null", "true");
    //         test_transform("true < null", "false");

    //         test_transform("true >= false", "true"); // 1 > 0
    //         test_transform("true <= false", "false");
    //         test_transform("true > false", "true");
    //         test_transform("true < false", "false");
    //     }

    //     #[test]
    //     fn testFoldLeftChildConcat() {
    //         test_same("x +5 + \"1\"");
    //         test_transform("x+\"5\" + \"1\"", "x + \"51\"");
    //         // test_transform("\"a\"+(c+\"b\")", "\"a\"+c+\"b\"");
    //         test_transform("\"a\"+(\"b\"+c)", "\"ab\"+c");
    //     }

    //     #[test]
    //     fn testFoldLeftChildOp() {
    //         test_transform("x & Infinity & 2", "x & 0");
    //         test_same("x - Infinity - 2"); // want "x-Infinity"
    //         test_same("x - 1 + Infinity");
    //         test_same("x - 2 + 1");
    //         test_same("x - 2 + 3");
    //         test_same("1 + x - 2 + 1");
    //         test_same("1 + x - 2 + 3");
    //         test_same("1 + x - 2 + 3 - 1");
    //         test_same("f(x)-0");
    //         test_transform("x-0-0", "x-0");
    //         test_same("x+2-2+2");
    //         test_same("x+2-2+2-2");
    //         test_same("x-2+2");
    //         test_same("x-2+2-2");
    //         test_same("x-2+2-2+2");

    //         test_same("1+x-0-NaN");
    //         test_same("1+f(x)-0-NaN");
    //         test_same("1+x-0+NaN");
    //         test_same("1+f(x)-0+NaN");

    //         test_same("1+x+NaN"); // unfoldable
    //         test_same("x+2-2"); // unfoldable
    //         test_same("x+2"); // nothing to do
    //         test_same("x-2"); // nothing to do
    //     }

    //     #[test]
    //     fn testFoldSimpleArithmeticOp() {
    //         test_same("x|NaN");
    //         test_same("NaN/y");
    //         test_same("f(x)-0");
    //         test_same("f(x)|1");
    //         test_same("1|f(x)");
    //         test_same("0+a+b");
    //         test_same("0-a-b");
    //         test_same("a+b-0");
    //         test_same("(1+x)|NaN");

    //         test_same("(1+f(x))|NaN"); // don't fold side-effects
    //     }

    //     #[test]
    //     fn testFoldLiteralsAsNumbers() {
    //         test_transform("x/'12'", "x/12");
    //         test_transform("x/('12'+'6')", "x/126");
    //         test_transform("true*x", "1*x");
    //         test_transform("x/false", "x/0"); // should we add an error check? :)
    //     }

    //     #[test]
    //     fn testNotFoldBackToTrueFalse() {
    //         late = false;
    //         test_transform("!0", "true");
    //         test_transform("!1", "false");
    //         test_transform("!3", "false");

    //         late = true;
    //         disableNormalize();
    //         test_same("!0");
    //         test_same("!1");
    //         test_transform("!3", "false");
    //         test_same("false");
    //         test_same("true");
    //     }

    //     #[test]
    //     fn testFoldBangConstants() {
    //         test_transform("1 + !0", "2");
    //         test_transform("1 + !1", "1");
    //         test_transform("'a ' + !1", "'a false'");
    //         test_transform("'a ' + !0", "'a true'");
    //     }

    //     #[test]
    //     fn testFoldMixed() {
    //         test_transform("''+[1]", "'1'");
    //         test_transform("false+[]", "\"false\"");
    //     }

    //     #[test]
    //     fn testFoldVoid() {
    //         test_same("void 0");
    //         test_transform("void 1", "void 0");
    //         test_transform("void x", "void 0");
    //         test_same("void x()");
    //     }

    //     #[test]
    //     fn testObjectLiteral() {
    //         test_transform("(!{})", "false");
    //         test_transform("(!{a:1})", "false");
    //         test_same("(!{a:foo()})");
    //         test_same("(!{'a':foo()})");
    //     }

    //     #[test]
    //     fn testArrayLiteral() {
    //         test_transform("(![])", "false");
    //         test_transform("(![1])", "false");
    //         test_transform("(![a])", "false");
    //         test_same("(![foo()])");
    //     }

    //     #[test]
    //     fn testIssue601() {
    //         test_same("'\\v' == 'v'");
    //         test_same("'v' == '\\v'");
    //         test_same("'\\u000B' == '\\v'");
    //     }

    //     #[test]
    //     fn testFoldObjectLiteralRef1() {
    //         // Leave extra side-effects in place
    //         test_same("var x = ({a:foo(),b:bar()}).a");
    //         test_same("var x = ({a:1,b:bar()}).a");
    //         test_same("function f() { return {b:foo(), a:2}.a; }");

    //         // on the LHS the object act as a temporary leave it in place.
    //         test_same("({a:x}).a = 1");
    //         test_transform("({a:x}).a += 1", "({a:x}).a = x + 1");
    //         test_same("({a:x}).a ++");
    //         test_same("({a:x}).a --");

    //         // Getters should not be inlined.
    //         test_same("({get a() {return this}}).a");
    //         test_same("({get a() {return this}})?.a");

    //         // Except, if we can see that the getter function never references 'this'.
    //         test_transform("({get a() {return 0}}).a", "(function() {return 0})()");
    //         test_transform("({get a() {return 0}})?.a", "(function() {return 0})()");
    //         test_transform("({get a() {return 0}})?.a.b", "(function() {return 0})().b");

    //         // It's okay to inline functions, as long as they're not immediately called.
    //         // (For tests where they are immediately called, see testFoldObjectLiteral_X)
    //         test_transform(
    //             "({a:function(){return this}}).a",
    //             "(function(){return this})",
    //         );
    //         test_transform(
    //             "({a:function(){return this}})?.a",
    //             "(function(){return this})",
    //         );

    //         // It's also okay to inline functions that are immediately called, so long as we know for
    //         // sure the function doesn't reference 'this'.
    //         test_transform("({a:function(){return 0}}).a()", "(function(){return 0})()");
    //         test_transform(
    //             "({a:function(){return 0}})?.a()",
    //             "(function(){return 0})()",
    //         );
    //         test_transform(
    //             "({a:function(){return 0}})?.a().b",
    //             "(function(){return 0})().b",
    //         );

    //         // Don't inline setters.
    //         test_same("({set a(b) {return this}}).a");
    //         test_same("({set a(b) {this._a = b}}).a");

    //         // Don't inline if there are side-effects.
    //         test_same("({[foo()]: 1,   a: 0}).a");
    //         test_same("({['x']: foo(), a: 0}).a");
    //         test_same("({x: foo(),     a: 0}).a");

    //         // Leave unknown props alone, the might be on the prototype
    //         test_same("({}).a");

    //         // setters by themselves don't provide a definition
    //         test_same("({}).a");
    //         test_same("({set a(b) {}}).a");
    //         // sets don't hide other definitions.
    //         test_transform("({a:1,set a(b) {}}).a", "1");

    //         // get is transformed to a call (gets don't have self referential names)
    //         test_transform("({get a() {}}).a", "(function (){})()");
    //         // sets don't hide other definitions.
    //         test_transform("({get a() {},set a(b) {}}).a", "(function (){})()");

    //         // a function remains a function not a call.
    //         test_transform(
    //             "var x = ({a:function(){return 1}}).a",
    //             "var x = function(){return 1}",
    //         );

    //         test_transform("var x = ({a:1}).a", "var x = 1");
    //         test_transform("var x = ({a:1, a:2}).a", "var x = 2");
    //         test_transform("var x = ({a:1, a:foo()}).a", "var x = foo()");
    //         test_transform("var x = ({a:foo()}).a", "var x = foo()");

    //         test_transform(
    //             "function f() { return {a:1, b:2}.a; }",
    //             "function f() { return 1; }",
    //         );

    //         // GETELEM is handled the same way.
    //         test_transform("var x = ({'a':1})['a']", "var x = 1");

    //         // try folding string computed properties
    //         test_transform("var a = {['a']:x}['a']", "var a = x");
    //         test_transform("var a = {['a']:x}?.['a']", "var a = x");
    //         test_transform("var a = {['a']:x}?.['a'].b", "var a = x.b");
    //         test_transform("var a = {a: {b: 1}}?.a?.b", "var a = 1");
    //         test_transform("var a = {a: null}?.a?.b", "var a = null?.b");
    //         test_transform("var a = {['a']: {b: 1}}?.['a']?.b", "var a = 1");
    //         test_transform("var a = {['a']: null}?.['a']?.b", "var a = null?.b");

    //         test_transform(
    //             "var a = { get ['a']() { return 1; }}['a']",
    //             "var a = function() { return 1; }();",
    //         );
    //         test_transform("var a = {'a': x, ['a']: y}['a']", "var a = y;");
    //         test_same("var a = {['foo']: x}.a;");
    //         // Note: it may be useful to fold symbols in the future.
    //         test_same("var y = Symbol(); var a = {[y]: 3}[y];");

    //         /*
    //          * We can fold member functions sometimes.
    //          *
    //          * <p>Even though they're different from fn expressions and arrow fns, extracting them only
    //          * causes programs that would have thrown errors to change behaviour.
    //          */
    //         test_transform("var x = {a() { 1; }}.a;", "var x = function() { 1; };");
    //         // Notice `a` isn't invoked, so beahviour didn't change.
    //         test_transform(
    //             "var x = {a() { return this; }}.a;",
    //             "var x = function() { return this; };",
    //         );
    //         // `super` is invisibly captures the object that declared the method so we can't fold.
    //         test_same("var x = {a() { return super.a; }}.a;");
    //         test_transform(
    //             "var x = {a: 1, a() { 2; }}.a;",
    //             "var x = function() { 2; };",
    //         );
    //         test_transform("var x = {a() {}, a: 1}.a;", "var x = 1;");
    //         test_same("var x = {a() {}}.b");
    //         // Don't fold non-computed setters.
    //         test_same("var x = ({ set a(v) { sideEffect() } }).a;");
    //         // Don't fold computed setters.
    //         test_same("var x = ({ set ['a'](v) { sideEffect() } }).a;");
    //         // Fold computed getters.
    //         test_transform(
    //             "var x = ({ get ['a']() { return 1; } }).a;",
    //             "var x = function() { return 1; }();",
    //         );
    //         test_transform(
    //             "var x = ({ get ['a']() { return 1; }, set ['a'](v) {} }).a;",
    //             "var x = function() { return 1; }();",
    //         );
    //         test_transform(
    //             "var x = ({ set ['a'](v) {}, get ['a']() { return 1; } }).a;",
    //             "var x = function() { return 1; }();",
    //         );
    //     }

    //     #[test]
    //     fn testFoldObjectLiteralRef2() {
    //         late = false;
    //         test_transform("({a:x}).a += 1", "({a:x}).a = x + 1");
    //         late = true;
    //         disableNormalize();
    //         test_same("({a:x}).a += 1");
    //     }

    //     // Regression test for https://github.com/google/closure-compiler/issues/2873
    //     // It would be incorrect to fold this to "x();" because the 'this' value inside the function
    //     // will be the global object, instead of the object {a:x} as it should be.
    //     #[test]
    //     fn testFoldObjectLiteral_methodCall_nonLiteralFn() {
    //         test_same("({a:x}).a()");
    //         test_same("({a:x})?.a()");
    //         test_same("({a:x})?.a()?.b");
    //     }

    //     #[test]
    //     fn testFoldObjectLiteral_freeMethodCall() {
    //         test_transform("({a() { return 1; }}).a()", "(function() { return 1; })()");
    //         test_transform("({a() { return 1; }})?.a()", "(function() { return 1; })()");

    //         // grandparent of optional chaining AST continues the chain
    //         test_transform(
    //             "({a() { return 1; }})?.a().b",
    //             "(function() { return 1; })().b",
    //         );
    //         test_transform(
    //             "({a() { return 1; }})?.a().b.c?.d",
    //             "(function() { return 1; })().b.c?.d",
    //         );
    //     }

    //     #[test]
    //     fn testFoldObjectLiteral_freeArrowCall_usingEnclosingThis_late() {
    //         late = true;
    //         disableNormalize();
    //         test_transform("({a: () => this }).a()", "(() => this)()");
    //         test_transform("({a: () => this })?.a()", "(() => this)()");
    //     }

    //     #[test]
    //     fn testFoldObjectLiteral_unfreeMethodCall_dueToThis() {
    //         test_same("({a() { return this; }}).a()");
    //         test_same("({a() { return this; }})?.a()");
    //     }

    //     #[test]
    //     fn testFoldObjectLiteral_unfreeMethodCall_dueToSuper() {
    //         test_same("({a() { return super.toString(); }}).a()");
    //         test_same("({a() { return super.toString(); }})?.a()");
    //     }

    //     #[test]
    //     fn testFoldObjectLiteral_paramToInvocation() {
    //         test_transform("console.log({a: 1}.a)", "console.log(1)");
    //         test_transform("console.log({a: 1}?.a)", "console.log(1)");
    //     }

    //     #[test]
    //     fn testIEString() {
    //         test_same("!+'\\v1'");
    //     }

    //     #[test]
    //     fn testIssue522() {
    //         test_same("[][1] = 1;");
    //     }

    //     #[test]
    //     fn testTypeBasedFoldConstant() {
    //         enableTypeCheck();
    //         test_same("function f(/** number */ x) { x + 1 + 1 + x; }");

    //         test_same("function f(/** boolean */ x) { x + 1 + 1 + x; }");

    //         test_same("function f(/** null */ x) { var y = true > x; }");

    //         test_same("function f(/** null */ x) { var y = null > x; }");

    //         test_same("function f(/** string */ x) { x + 1 + 1 + x; }");

    //         useTypes = false;
    //         test_same("function f(/** number */ x) { x + 1 + 1 + x; }");
    //     }

    //     #[test]
    //     fn testColorBasedFoldConstant() {
    //         enableTypeCheck();
    //         replaceTypesWithColors();
    //         disableCompareJsDoc();
    //         test_same("function f(/** number */ x) { x + 1 + 1 + x; }");

    //         test_same("function f(/** boolean */ x) { x + 1 + 1 + x; }");

    //         test_same("function f(/** null */ x) { var y = true > x; }");

    //         test_same("function f(/** null */ x) { var y = null > x; }");

    //         test_same("function f(/** string */ x) { x + 1 + 1 + x; }");

    //         useTypes = false;
    //         test_same("function f(/** number */ x) { x + 1 + 1 + x; }");
    //     }

    //     #[test]
    //     fn foldDefineProperties() {
    //         test_transform("Object.defineProperties({}, {})", "({})");
    //         test_transform("Object.defineProperties(a, {})", "a");
    //         test_same("Object.defineProperties(a, {anything:1})");
    //     }

    //     #[test]
    //     fn testES6Features() {
    //         test_transform(
    //             "var x = {[undefined != true] : 1};",
    //             "var x = {[true] : 1};",
    //         );
    //         test_transform("let x = false && y;", "let x = false;");
    //         test_transform("const x = null == undefined", "const x = true");
    //         test_transform(
    //             "var [a, , b] = [false+1, true+1, ![]]",
    //             "var a; var b; [a, , b] = [1, 2, false]",
    //         );
    //         test_transform(
    //             "var x = () =>  true || x;",
    //             "var x = () => { return true; }",
    //         );
    //         test_transform(
    //             "function foo(x = (1 !== void 0), y) {return x+y;}",
    //             "function foo(x = true, y) {return x+y;}",
    //         );
    //         test_transform(
    //             "
    // class Foo {
    //     constructor() {this.x = null <= null;}
    // }",
    //             "
    // class Foo {
    //     constructor() {this.x = true;}
    // }",
    //         );
    //         test_transform(
    //             "function foo() {return `${false && y}`}",
    //             "function foo() {return `false`}",
    //         );
    //     }

    //     #[test]
    //     fn testES6Features_late() {
    //         late = true;
    //         disableNormalize();

    //         test_transform(
    //             "var [a, , b] = [false+1, true+1, ![]]",
    //             "var [a, , b] = [1, 2, false]",
    //         );
    //         test_transform("var x = () =>  true || x;", "var x = () => true;");
    //     }

    //     #[test]
    //     fn testClassField() {
    //         test_transform(
    //             "
    // class Foo {
    //     x = null <= null;
    // }",
    //             "
    // class Foo {
    //     x = true;
    // }",
    //         );
    //     }

    //     // TODO:
    //     //   private static final ImmutableList<String> LITERAL_OPERANDS =
    //     //       ImmutableList.of(
    //     //           "null",
    //     //           "undefined",
    //     //           "void 0",
    //     //           "true",
    //     //           "false",
    //     //           "!0",
    //     //           "!1",
    //     //           "0",
    //     //           "1",
    //     //           "''",
    //     //           "'123'",
    //     //           "'abc'",
    //     //           "'def'",
    //     //           "NaN",
    //     //           "Infinity",
    //     //           // TODO(nicksantos): Add more literals
    //     //           "-Infinity"
    //     //           // "({})",
    //     //           // "[]"
    //     //           // "[0]",
    //     //           // "Object",
    //     //           // "(function() {})"
    //     //           );

    //     // TODO:
    //     //   #[test]
    //     //   fn testInvertibleOperators() {
    //     //     ImmutableMap<String, String> inverses =
    //     //         ImmutableMap.<String, String>builder()
    //     //             .put("==", "!=")
    //     //             .put("===", "!==")
    //     //             .put("<=", ">")
    //     //             .put("<", ">=")
    //     //             .put(">=", "<")
    //     //             .put(">", "<=")
    //     //             .put("!=", "==")
    //     //             .put("!==", "===")
    //     //             .buildOrThrow();
    //     //     ImmutableSet<String> comparators = ImmutableSet.of("<=", "<", ">=", ">");
    //     //     ImmutableSet<String> equalitors = ImmutableSet.of("==", "===");
    //     //     ImmutableSet<String> uncomparables = ImmutableSet.of("undefined", "void 0");
    //     //     ImmutableList<String> operators = ImmutableList.copyOf(inverses.values());
    //     //     for (int iOperandA = 0; iOperandA < LITERAL_OPERANDS.size(); iOperandA++) {
    //     //       for (int iOperandB = 0; iOperandB < LITERAL_OPERANDS.size(); iOperandB++) {
    //     //         for (int iOp = 0; iOp < operators.size(); iOp++) {
    //     //           String a = LITERAL_OPERANDS.get(iOperandA);
    //     //           String b = LITERAL_OPERANDS.get(iOperandB);
    //     //           String op = operators.get(iOp);
    //     //           String inverse = inverses.get(op);

    //     //           // Test invertability.
    //     //           if (comparators.contains(op)) {
    //     //             if (uncomparables.contains(a)
    //     //                 || uncomparables.contains(b)
    //     //                 || (a.equals("null") && NodeUtil.getStringNumberValue(b) == null)) {
    //     //               assertSameResults(join(a, op, b), "false");
    //     //               assertSameResults(join(a, inverse, b), "false");
    //     //             }
    //     //           } else if (a.equals(b) && equalitors.contains(op)) {
    //     //             if (a.equals("NaN") || a.equals("Infinity") || a.equals("-Infinity")) {
    //     //               test_transform(join(a, op, b), a.equals("NaN") ? "false" : "true");
    //     //             } else {
    //     //               assertSameResults(join(a, op, b), "true");
    //     //               assertSameResults(join(a, inverse, b), "false");
    //     //             }
    //     //           } else {
    //     //             assertNotSameResults(join(a, op, b), join(a, inverse, b));
    //     //           }
    //     //         }
    //     //       }
    //     //     }
    //     //   }

    //     // TODO:
    //     //   #[test]
    //     //   fn testCommutativeOperators() {
    //     //     late = true;
    //     //     disableNormalize();
    //     //     ImmutableList<String> operators =
    //     //         ImmutableList.of("==", "!=", "===", "!==", "*", "|", "&", "^");
    //     //     for (String a : LITERAL_OPERANDS) {
    //     //       for (String b : LITERAL_OPERANDS) {
    //     //         for (String op : operators) {
    //     //           // Test commutativity.
    //     //           assertSameResults(join(a, op, b), join(b, op, a));
    //     //         }
    //     //       }
    //     //     }
    //     //   }

    //     #[test]
    //     fn testConvertToNumberNegativeInf() {
    //         test_same("var x = 3 & (r ? Infinity : -Infinity);");
    //     }

    //     #[test]
    //     fn testAlgebraicIdentities() {
    //         enableTypeCheck();
    //         replaceTypesWithColors();
    //         disableCompareJsDoc();

    //         foldNumericTypes("x+0", "x");
    //         foldNumericTypes("0+x", "x");
    //         foldNumericTypes("x+0+0+x+x+0", "x+x+x");

    //         foldNumericTypes("x-0", "x");
    //         foldNumericTypes("x-0-0-0", "x");
    //         // 'x-0' is numeric even if x isn't
    //         test_transform("var x='a'; x-0-0", "var x='a';x-0");
    //         foldNumericTypes("0-x", "-x");
    //         test_transform(
    //             "for (var i = 0; i < 5; i++) var x = 0 + i * 1",
    //             "var i = 0; for(; i < 5; i++) var x=i",
    //         );

    //         foldNumericTypes("x*1", "x");
    //         foldNumericTypes("1*x", "x");
    //         // can't optimize these without a non-NaN prover
    //         test_same("x*0");
    //         test_same("0*x");
    //         test_same("0/x");

    //         foldNumericTypes("x/1", "x");
    //     }

    //     #[test]
    //     fn testBigIntAlgebraicIdentities() {
    //         enableTypeCheck();
    //         replaceTypesWithColors();
    //         disableCompareJsDoc();

    //         foldBigIntTypes("x+0n", "x");
    //         foldBigIntTypes("0n+x", "x");
    //         foldBigIntTypes("x+0n+0n+x+x+0n", "x+x+x");

    //         foldBigIntTypes("x-0n", "x");
    //         foldBigIntTypes("0n-x", "-x");
    //         foldBigIntTypes("x-0n-0n-0n", "x");

    //         foldBigIntTypes("x*1n", "x");
    //         foldBigIntTypes("1n*x", "x");
    //         foldBigIntTypes("x*1n*1n*x*x*1n", "x*x*x");

    //         foldBigIntTypes("x/1n", "x");
    //         foldBigIntTypes("x/0n", "x/0n");

    //         test_transform(
    //             "for (var i = 0n; i < 5n; i++) var x = 0n + i * 1n",
    //             "var i = 0n; for(; i < 5n; i++) var x=i",
    //         );
    //         test_transform(
    //             "for (var i = 0n; i % 2n === 0n; i++) var x = i % 2n",
    //             "var i = 0n; for (; i % 2n === 0n; i++) var x = i % 2n",
    //         );

    //         test_transform("(doSomething(),0n)*1n", "(doSomething(),0n)");
    //         test_transform("1n*(doSomething(),0n)", "(doSomething(),0n)");
    //         ignoreWarnings(DiagnosticGroups.CHECK_TYPES);
    //         test_same("(0n,doSomething())*1n");
    //         test_same("1n*(0n,doSomething())");
    //     }

    //     #[test]
    //     fn testAssociativeFoldConstantsWithVariables() {
    //         // MUL and ADD should not fold
    //         test_same("alert(x * 12 * 20);");
    //         test_same("alert(12 * x * 20);");
    //         test_same("alert(x + 12 + 20);");
    //         test_same("alert(12 + x +  20);");

    //         test_transform("alert(x & 12 & 20);", "alert(x & 4);");
    //         test_transform("alert(12 & x & 20);", "alert(x & 4);");
    //     }

    //     #[test]
    //     fn testTemplateLiteralConcat() {
    //         // join at variables
    //         test_transform(
    //             "
    //         const a = `${x}` + `${x}`;
    //         ",
    //             "
    //         const a = `${x}${x}`;
    //         ",
    //         );
    //         test_transform(
    //             "
    //         const a = `${x}${y}` + `${x}${y}`;
    //         ",
    //             "
    //         const a = `${x}${y}${x}${y}`;
    //         ",
    //         );

    //         // join at variable and string
    //         test_transform(
    //             "
    //         const a = `${x}` + `a${x}`;
    //         ",
    //             "
    //         const a =`${x}a${x}`;
    //         ",
    //         );
    //         test_transform(
    //             "
    //         const a = `${x}` + ` ${x}`;
    //         ",
    //             "
    //         const a =`${x} ${x}`;
    //         ",
    //         );

    //         // join at string and variable
    //         test_transform(
    //             "
    //         const a = `a${x}b` + `${x}c`;
    //         ",
    //             "
    //         const a = `a${x}b${x}c`;
    //         ",
    //         );
    //         test_transform(
    //             "
    //         const a = `a${x} ` + `${x}b`;
    //         ",
    //             "
    //         const a = `a${x} ${x}b`;
    //         ",
    //         );

    //         // join at strings
    //         test_transform(
    //             "
    //         const x = 'X';
    //         console.log(`a${x}b` + `c${x}d`);
    //         ",
    //             "
    //         const x = 'X';
    //         console.log(`a${x}bc${x}d`);
    //         ",
    //         );

    //         // complex joins
    //         test_transform(
    //             "
    //         const a = `${foo() + bar()}` + `${baz()}`;
    //         ",
    //             "
    //         const a =`${foo() + bar()}${baz()}`;
    //         ",
    //         );
    //         test_transform(
    //             "
    //         console.log(`<h1>${url}</h1>` + `<p>The URL is ${url}.</p>`);
    //         ",
    //             "
    //         console.log(`<h1>${url}</h1><p>The URL is ${url}.</p>`);
    //         ",
    //         );
    //         test_transform(
    //             "
    //         console.log(`${(() => {return url;})()}</h1>` + `<p>The URL is ${url}.</p>`);
    //         ",
    //             "
    //         console.log(`${(() => {return url;})()}</h1><p>The URL is ${url}.</p>`);
    //         ",
    //         );

    //         // don't fold tagged template literals
    //         test_same(
    //             "
    //         const a = foo`${b}` + `${b}`;
    //         ",
    //         );
    //         test_same(
    //             "
    //         const a = foo`${b}` + bar`${b}`;
    //         ",
    //         );
    //         // don't fold when raw strings meet at unescaped $ and {
    //         test_same("`${x}$` + `{y}${z}`");
    //         test_same("`${x}$` + `{y}`");
    //         test_transform("`${x}\\$` + `{y}`", "`${x}\\${y}`");
    //         test_same("`${x}\\\\$` + `{y}`");
    //     }

    //     #[test]
    //     fn testFoldTemplateLiteralSubstitutions() {
    //         // Basic substitution
    //         test_transform("`a${'b'}c`", "`abc`");
    //         // Number substitution
    //         test_transform("`a${123}c`", "`a123c`");
    //         // Boolean substitution
    //         test_transform("`a${true}c`", "`atruec`");
    //         test_transform("`a${false}c`", "`afalsec`");

    //         // Multiple substitutions
    //         test_transform("`a${'b'}c${'d'}e`", "`abcde`");
    //         // Adjacent substitutions
    //         test_transform("`${'a'}${'b'}`", "`ab`");
    //         // Empty strings
    //         test_transform("`${''}a${''}`", "`a`");

    //         // Corner cases where it should NOT fold
    //         test_same("`a${'`'}c`");
    //         test_same("`a${'$'}c`");
    //         test_same("`a${'\\\\'}c`");
    //         test_same("`a${'\\\\n'}c`");
    //         test_same("`a${'\\\\r'}c`");
    //         test_same("`${x} ${'$'}${'{'}`"); // specifically tests that $ and { are not folded together
    //         test_transform("`$${''}{${''}`", "`$${''}{`");
    //         test_same("`$${''}{foo}`");
    //         test_transform("`\\$${''}{foo}`", "`\\${foo}`");
    //         // These should not fold because \0 followed by a digit is a syntax error in untagged templates.
    //         test_same("`\\0${'77'}`");
    //         test_same("`\\0${77}`");
    //         test_transform("`\\0${'a'}`", "`\\0a` ");
    //         test_transform("`\\\\0${'7'}`", "`\\\\07` ");
    //         test_transform("`\\0${''}${'7'}`", "`\\0${'7'}`");

    //         // Tagged template literals should NOT fold
    //         test_same("tag`a${'b'}c`");

    //         // Non-constant substitutions should NOT fold
    //         test_same("`a${x}c`");
    //         test_same("`a${foo()}c`");
    //     }

    //     #[test]
    //     fn testFoldAddTemplateLiteralAndConstant() {
    //         // String merge
    //         test_transform("`a${x}` + 'b'", "`a${x}b` ");
    //         test_transform("'a' + `b${x}`", "`ab${x}` ");

    //         // Number merge
    //         test_transform("`a${x}` + 1", "`a${x}1` ");
    //         test_transform("1 + `a${x}`", "`1a${x}` ");

    //         // Boolean merge
    //         test_transform("`a${x}` + true", "`a${x}true` ");
    //         test_transform("true + `a${x}`", "`truea${x}` ");

    //         // BigInt merge
    //         test_transform("`a${x}` + 10n", "`a${x}10` ");

    //         // Null/Undefined merge
    //         test_transform("`a${x}` + null", "`a${x}null` ");
    //         test_transform("`a${x}` + undefined", "`a${x}undefined` ");

    //         // Nested templates
    //         test_transform("`a${x}` + `b` ", "`a${x}b` ");
    //         test_transform("`a` + `b${x}` ", "`ab${x}` ");

    //         // Restricted octal escape
    //         test_same("`\\0` + 7");
    //         test_same("`\\0` + '7'");
    //         test_transform("`\\0a` + '7'", "`\\0a7` ");

    //         // Safety - special chars should NOT fold
    //         test_same("`a${x}` + '$'");
    //         test_same("`a${x}` + '{'");
    //         test_same("`a${x}` + '`'");
    //         test_same("`a${x}` + '\\\\'");
    //         test_same("`a${x}` + '\\\\n'");

    //         // Compound addition
    //         test_transform("`a` + `b${x}` + `c` + 'd'", "`ab${x}cd` ");
    //     }

    // TODO:
    //   #[test]
    //   fn
    //       testFoldAddTemplateLiterals_validateRawAndCookedStringForSpecialChars_andValidateChildren() {
    //     // This is a bit tricky because there are two layers of escaping.
    //     // Java sees "\\n" as "(\\)n" so it becomes "\n" for the javascript compiler.
    //     // The compiler then interprets this as the special character "\n"
    //     test_transform("var x = `${a}\\n` + `\\t${b}`", "var x = `${a}\\n\\t${b}`");

    //     // getJsRoot() returns the ROOT node for inputs only.
    //     // The first child is the SCRIPT node containing the input code.
    //     Node script = getLastCompiler().getJsRoot().getFirstChild();
    //     Node var = script.getFirstChild();
    //     Node name = var.getFirstChild();
    //     Node templateLit = name.getFirstChild();
    //     assertThat(templateLit.isTemplateLit()).isTrue();

    //     // TEMPLATELIT should have 5 total children
    //     assertThat(templateLit.getChildCount()).isEqualTo(5);

    //     // Index 0: TEMPLATELIT_STRING ""
    //     @SuppressWarnings({"RhinoNodeGetFirstChild", "Simplification"})
    //     Node child0 = templateLit.getChildAtIndex(0);
    //     assertThat(child0.isTemplateLitString()).isTrue();
    //     assertThat(child0.getCookedString()).isEmpty();
    //     assertThat(child0.getRawString()).isEmpty();

    //     // Index 1: SUB a
    //     @SuppressWarnings({"RhinoNodeGetSecondChild", "Simplification"})
    //     Node child1 = templateLit.getChildAtIndex(1);
    //     assertThat(child1.isTemplateLitSub()).isTrue();
    //     assertThat(child1.getOnlyChild().isName()).isTrue();
    //     assertThat(child1.getOnlyChild().getString()).isEqualTo("a");

    //     // Index 2: TEMPLATELIT_STRING "\n\t"
    //     Node child2 = templateLit.getChildAtIndex(2);
    //     assertThat(child2.isTemplateLitString()).isTrue();
    //     assertThat(child2.getCookedString()).isEqualTo("\n\t");
    //     assertThat(child2.getRawString()).isEqualTo("\\n\\t");

    //     // Index 3: SUB b
    //     Node child3 = templateLit.getChildAtIndex(3);
    //     assertThat(child3.isTemplateLitSub()).isTrue();
    //     assertThat(child3.getOnlyChild().isName()).isTrue();
    //     assertThat(child3.getOnlyChild().getString()).isEqualTo("b");

    //     // Index 4: TEMPLATELIT_STRING ""
    //     Node child4 = templateLit.getChildAtIndex(4);
    //     assertThat(child4.isTemplateLitString()).isTrue();
    //     assertThat(child4.getCookedString()).isEmpty();
    //     assertThat(child4.getRawString()).isEmpty();
    //   }

    // TODO:
    //   #[test]
    //   fn
    //       testFoldAddTemplateLiterals_validateRawAndCookedStringForLiteralBackslash_andValidateChildren() {
    //     // This is a bit tricky because there are two layers of escaping.
    //     // Java sees "\\\\n" as "(\\)(\\)n" so it becomes "\\n" for the javascript compiler.
    //     // The compiler then interprets this as "(\\)n" so it becomes "\" followed by
    //     // "n" NOT the special character "\n".
    //     test_transform(
    //         "var x = `${foo()}\\\\` + `n\\t${()=> {return b;}}`",
    //         "var x = `${foo()}\\\\n\\t${()=> {return b;}}`");

    //     // getJsRoot() returns the ROOT node for inputs only.
    //     // The first child is the SCRIPT node containing the input code.
    //     Node script = getLastCompiler().getJsRoot().getFirstChild();
    //     Node var = script.getFirstChild();
    //     Node name = var.getFirstChild();
    //     Node templateLit = name.getFirstChild();
    //     assertThat(templateLit.isTemplateLit()).isTrue();

    //     // TEMPLATELIT should have 5 children:
    //     assertThat(templateLit.getChildCount()).isEqualTo(5);

    //     // Index 0: TEMPLATELIT_STRING ""
    //     @SuppressWarnings({"RhinoNodeGetFirstChild", "Simplification"})
    //     Node child0 = templateLit.getChildAtIndex(0);
    //     assertThat(child0.isTemplateLitString()).isTrue();
    //     assertThat(child0.getCookedString()).isEmpty();
    //     assertThat(child0.getRawString()).isEmpty();

    //     // Index 1: SUB foo()
    //     @SuppressWarnings({"RhinoNodeGetSecondChild", "Simplification"})
    //     Node child1 = templateLit.getChildAtIndex(1);
    //     assertThat(child1.isTemplateLitSub()).isTrue();
    //     assertThat(child1.getOnlyChild().isCall()).isTrue();
    //     assertThat(child1.getOnlyChild().getOnlyChild().getString()).isEqualTo("foo");

    //     // Index 2: TEMPLATELIT_STRING "\n\t"
    //     Node child2 = templateLit.getChildAtIndex(2);
    //     assertThat(child2.isTemplateLitString()).isTrue();
    //     assertThat(child2.getCookedString()).isEqualTo("\\n\t");
    //     assertThat(child2.getRawString()).isEqualTo("\\\\n\\t");

    //     // Index 3: SUB ()=> {return b;}
    //     Node child3 = templateLit.getChildAtIndex(3);
    //     assertThat(child3.isTemplateLitSub()).isTrue();
    //     assertThat(child3.getOnlyChild().isFunction()).isTrue();
    //     assertThat(child3.getOnlyChild().getFirstChild().getString()).isEmpty();

    //     // Index 4: TEMPLATELIT_STRING ""
    //     Node child4 = templateLit.getChildAtIndex(4);
    //     assertThat(child4.isTemplateLitString()).isTrue();
    //     assertThat(child4.getCookedString()).isEmpty();
    //     assertThat(child4.getRawString()).isEmpty();
    //   }

    //   fn foldBigIntTypes(js:&str, expected:&str) {
    //     test_transform(
    //         "function f(/** @type {bigint} */ x) { " + js + " }",
    //         "function f(/** @type {bigint} */ x) { " + expected + " }");
    //   }

    //   fn foldNumericTypes(js:&str, expected:&str) {
    //     test_transform(
    //         "function f(/** @type {number} */ x) { " + js + " }",
    //         "function f(/** @type {number} */ x) { " + expected + " }");
    //   }

    //   fn foldStringTypes(js:&str, expected:&str) {
    //     test_transform(
    //         "function f(/** @type {string} */ x) { " + js + " }",
    //         "function f(/** @type {string} */ x) { " + expected + " }");
    //   }

    //   fn join(String operandA, String op, String operandB) {
    //     return operandA + " " + op + " " + operandB;
    //   }

    //   fn assertSameResults(String exprA, String exprB) {
    //     assertWithMessage("Expressions did not fold the same\nexprA: %s\nexprB: %s", exprA, exprB)
    //         .that(process(exprB))
    //         .isEqualTo(process(exprA));
    //   }

    //   fn assertNotSameResults(String exprA, String exprB) {
    //     assertWithMessage("Expressions folded the same\nexprA: %s\nexprB: %s", exprA, exprB)
    //         .that(process(exprA).equals(process(exprB)))
    //         .isFalse();
    //   }

    //   private String process(String js) {
    //     return printHelper(js, true);
    //   }

    //   private String printHelper(String js, boolean runProcessor) {
    //     Compiler compiler = createCompiler();
    //     CompilerOptions options = getOptions();
    //     compiler.init(
    //         ImmutableList.<SourceFile>of(),
    //         ImmutableList.of(SourceFile.fromCode("testcode", js)),
    //         options);
    //     Node root = compiler.parseInputs();
    //     compiler.setAccessorSummary(AccessorSummary.create(ImmutableMap.of()));
    //     assertWithMessage(
    //             "Unexpected parse error(s): %s\nEXPR: %s",
    //             Joiner.on("\n").join(compiler.getErrors()), js)
    //         .that(root)
    //         .isNotNull();
    //     Node externsRoot = root.getFirstChild();
    //     Node mainRoot = externsRoot.getNext();
    //     if (runProcessor) {
    //       getProcessor(compiler).process(externsRoot, mainRoot);
    //     }
    //     return compiler.toSource(mainRoot);
    //   }

    // fn test_transform_late(input: &str, expected: &str) {
    //     test_transform_inner(input, expected, true);
    // }
    // fn test_same_late(input: &str) {
    //     test_same_inner(input, true);
    // }

    fn test_transform(input: &str, expected: &str) {
        test_transform_inner(input, expected, false);
    }
    // fn test_same(input: &str) {
    //     test_same_inner(input, false);
    // }

    fn test_transform_inner(input: &str, expected: &str, late: bool) {
        crate::testing::test_transform(
            |program, program_data| {
                resolve(program, program_data);

                process(program, program_data, late)
            },
            input,
            expected,
        );
    }
    // fn test_same_inner(input: &str, late: bool) {
    //     test_transform_inner(input, input, late);
    // }
}
