use ast::*;
use atoms::{JsWord, js_word};
use common::SyntaxContext;
use num_traits::{FromPrimitive, identities::Zero};

use crate::convert::{
    ecma_number_to_int_32, ecma_number_to_string, ecma_string_to_big_int, ecma_string_to_number,
};

// TODO: clean up, tests

/// Returns the boolean value of an expression, or None if the value can't be
/// determined by static analysis.
pub fn get_boolean_value(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<bool> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(string) => Some(!string.is_empty()),
            Lit::Bool(bool) => Some(bool.value),
            Lit::Null(_) => Some(false),
            Lit::Num(number) => Some(number.value != 0.0 && !number.value.is_nan()),
            Lit::BigInt(big_int) => Some(big_int.value.is_zero()),
            Lit::Regex(_) => Some(true),
        },
        Expr::Array(_) | Expr::Object(_) | Expr::Class(_) | Expr::Fn(_) => Some(true),
        Expr::Ident(ident) => {
            if ident.ctxt == unresolved_ctxt {
                match ident.sym {
                    js_word!("undefined") | js_word!("NaN") => Some(false),
                    js_word!("Infinity") => Some(true),
                    _ => None,
                }
            } else {
                None
            }
        }
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Minus | UnaryOp::Plus | UnaryOp::Tilde => {
                if let Some(numeric_value) = get_number_value(expr, unresolved_ctxt) {
                    let isFalsey = numeric_value == 0.0 || numeric_value.is_nan();
                    Some(!isFalsey)
                } else if let Some(bigintVal) = getBigIntValue(expr, unresolved_ctxt) {
                    let isFalsey = bigintVal.is_zero();
                    Some(!isFalsey)
                } else {
                    None
                }
            }
            UnaryOp::Bang => get_boolean_value(&unary.arg, unresolved_ctxt).map(|v| !v),
            UnaryOp::TypeOf => Some(true),
            UnaryOp::Void => Some(false),
            UnaryOp::Delete => None,
        },
        Expr::Seq(seq) => seq
            .exprs
            .last()
            .and_then(|e| get_boolean_value(e, unresolved_ctxt)),
        Expr::Assign(assign) => match assign.op {
            AssignOp::Assign => get_boolean_value(&assign.right, unresolved_ctxt),
            // TODO: &&=, ||=, and ??=
            _ => None,
        },
        Expr::Cond(cond) => {
            let consValue = get_boolean_value(&cond.cons, unresolved_ctxt);
            let altValue = get_boolean_value(&cond.alt, unresolved_ctxt);
            if consValue == altValue {
                consValue
            } else {
                None
            }
        }
        Expr::Tpl(tpl) => {
            if tpl.exprs.is_empty() && tpl.quasis.iter().all(|q| q.raw.is_empty()) {
                Some(false)
            } else if tpl.quasis.iter().any(|q| !q.raw.is_empty()) {
                Some(true)
            } else {
                None
            }
        }
        Expr::Bin(bin) => match bin.op {
            BinaryOp::LogicalOr => {
                let left = get_boolean_value(&bin.left, unresolved_ctxt);
                let right = get_boolean_value(&bin.right, unresolved_ctxt);
                if let Some(left) = left
                    && let Some(right) = right
                {
                    Some(left || right)
                } else {
                    None
                }
            }
            BinaryOp::LogicalAnd => {
                let left = get_boolean_value(&bin.left, unresolved_ctxt);
                let right = get_boolean_value(&bin.right, unresolved_ctxt);
                if let Some(left) = left
                    && let Some(right) = right
                {
                    Some(left && right)
                } else {
                    None
                }
            }
            BinaryOp::NullishCoalescing => {
                let left = get_boolean_value(&bin.left, unresolved_ctxt);
                let right = get_boolean_value(&bin.right, unresolved_ctxt);
                if let Some(true) = left {
                    left
                } else if left == right {
                    left
                } else {
                    None
                }
            }
            _ => None,
        },
        // Constructors can't return anything falsy... except for `document.all`
        // for *delightful javascript reasons*. We assume that no developer will
        // explicitly override their constructor's return value to return
        // `document.all`.
        Expr::New(_) => Some(true),
        _ => None,
    }
}

/// Returns the value of an expression as a String, or None if it cannot be
/// converted. When a String is returned, this function effectively emulates the
/// `String()` JavaScript cast function.
fn getStringValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<JsWord> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(string) => Some(string.value.clone()),
            Lit::Bool(bool) => {
                if bool.value {
                    Some(js_word!("true"))
                } else {
                    Some(js_word!("false"))
                }
            }
            Lit::Null(_) => Some(js_word!("null")),
            Lit::Num(number) => Some(JsWord::from(ecma_number_to_string(number.value))),
            Lit::BigInt(big_int) => Some(JsWord::from(big_int.value.to_string())),
            // TODO: String(regex).
            Lit::Regex(_) => None,
        },
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Void => Some(js_word!("undefined")),
            UnaryOp::Bang => {
                let arg_value = get_boolean_value(&unary.arg, unresolved_ctxt);
                arg_value.map(|bool| {
                    if bool {
                        js_word!("false")
                    } else {
                        js_word!("true")
                    }
                })
            }
            UnaryOp::Minus => {
                let arg_value = get_number_value(&unary.arg, unresolved_ctxt);
                arg_value.map(|v| JsWord::from(ecma_number_to_string(-v)))
            }
            // TODO: why can we analyze Minus but not Plus or Tilde?
            UnaryOp::Plus | UnaryOp::Tilde | UnaryOp::TypeOf | UnaryOp::Delete => None,
        },
        Expr::Ident(ident) => {
            if ident.ctxt == unresolved_ctxt {
                match ident.sym {
                    js_word!("undefined") | js_word!("NaN") | js_word!("Infinity") => {
                        Some(ident.sym.clone())
                    }
                    _ => None,
                }
            } else {
                None
            }
        }
        Expr::Array(array) => arrayToString(array, unresolved_ctxt),
        Expr::Object(_) => Some(JsWord::from("[object Object]")),
        // TODO: template literals, but we need the cooked values for the
        // quasis.
        _ => None,
    }
}

/// When converting arrays to string using `Array.prototype.toString` or
/// `Array.prototype.join`, the rules for conversion to String are different
/// than converting each element individually. Specifically, `null` and
/// `undefined` are converted to an empty string.
fn getArrayElementStringValue(
    element: &Option<ExprOrSpread>,
    unresolved_ctxt: SyntaxContext,
) -> Option<JsWord> {
    let Some(element) = element else {
        return Some(js_word!(""));
    };

    let ExprOrSpread::Expr(element) = element else {
        return None;
    };

    if let Expr::Ident(Ident {
        sym: js_word!("undefined"),
        ctxt,
        ..
    }) = element.as_ref()
    {
        if *ctxt == unresolved_ctxt {
            return Some(js_word!(""));
        }
    }

    if matches!(element.as_ref(), Expr::Lit(Lit::Null(_))) {
        return Some(js_word!(""));
    }

    getStringValue(element, unresolved_ctxt)
}

fn arrayToString(array: &ArrayLit, unresolved_ctxt: SyntaxContext) -> Option<JsWord> {
    let mut result = String::new();

    for (i, el) in array.elems.iter().enumerate() {
        let value = getArrayElementStringValue(el, unresolved_ctxt);

        if let Some(value) = value {
            if i != 0 {
                result.push(',');
            }
            result.push_str(&value);
        } else {
            return None;
        }
    }

    Some(JsWord::from(result))
}

/// Returns the value of am expression as a Number, or None if it cannot be
/// converted. When it returns a number, this function effectively emulates the
/// `Number()` JavaScript cast function.
fn get_number_value(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<f64> {
    get_number_value_inner(expr, unresolved_ctxt, true)
}

fn get_number_value_inner(
    expr: &Expr,
    unresolved_ctxt: SyntaxContext,
    number_conversions: bool,
) -> Option<f64> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(string) => {
                if number_conversions {
                    Some(ecma_string_to_number(&string.value))
                } else {
                    None
                }
            }
            Lit::Bool(bool) => {
                if number_conversions {
                    if bool.value { Some(1.0) } else { Some(0.0) }
                } else {
                    None
                }
            }
            Lit::Null(_) => {
                if number_conversions {
                    Some(0.0)
                } else {
                    None
                }
            }
            Lit::Num(number) => Some(number.value),
            // BigInt can't be converted to number.
            Lit::BigInt(_) => None,
            Lit::Regex(_) => None,
        },
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Minus => get_number_value_inner(&unary.arg, unresolved_ctxt, true).map(|v| -v),
            UnaryOp::Plus => get_number_value_inner(&unary.arg, unresolved_ctxt, true),
            UnaryOp::Tilde => get_number_value_inner(&unary.arg, unresolved_ctxt, true)
                .map(|n| !ecma_number_to_int_32(n) as f64),

            UnaryOp::Void => {
                if number_conversions {
                    Some(f64::NAN)
                } else {
                    None
                }
            }
            UnaryOp::Bang => {
                if number_conversions {
                    let arg_value = get_boolean_value(&unary.arg, unresolved_ctxt);
                    match arg_value {
                        Some(true) => Some(0.0),
                        Some(false) => Some(1.0),
                        None => None,
                    }
                } else {
                    None
                }
            }
            UnaryOp::Delete | UnaryOp::TypeOf => None,
        },
        Expr::Ident(ident) => {
            if ident.ctxt == unresolved_ctxt {
                match ident.sym {
                    js_word!("NaN") => Some(f64::NAN),
                    js_word!("undefined") => {
                        if number_conversions {
                            Some(f64::NAN)
                        } else {
                            None
                        }
                    }
                    js_word!("Infinity") => Some(f64::INFINITY),
                    _ => None,
                }
            } else {
                None
            }
        }
        Expr::Object(_) | Expr::Array(_) | Expr::Tpl(_) => {
            if number_conversions {
                getStringValue(expr, unresolved_ctxt).map(|s| ecma_string_to_number(&s))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Returns the value of an expression as a BigInt, or None if it cannot be
/// converted. When it returns a BigInt, this function effectively emulates the
/// `BigInt()` JavaScript cast function.
fn getBigIntValue(expr: &Expr, unresolved_ctxt: SyntaxContext) -> Option<num_bigint::BigInt> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::Str(string) => ecma_string_to_big_int(&string.value),
            Lit::Bool(bool) => {
                if bool.value {
                    Some(num_bigint::BigInt::ONE)
                } else {
                    Some(num_bigint::BigInt::ZERO)
                }
            }
            Lit::Null(_) => None,
            Lit::Num(number) => num_bigint::BigInt::from_f64(number.value),
            // TODO: Remove clone
            Lit::BigInt(big_int) => Some(num_bigint::BigInt::from(big_int.value.clone())),
            Lit::Regex(_) => None,
        },
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Minus => getBigIntValue(&unary.arg, unresolved_ctxt).map(|v| -v),
            UnaryOp::Bang => match get_boolean_value(expr, unresolved_ctxt) {
                Some(true) => Some(num_bigint::BigInt::ONE),
                Some(false) => Some(num_bigint::BigInt::ZERO),
                None => None,
            },
            UnaryOp::Tilde => getBigIntValue(&unary.arg, unresolved_ctxt).map(|v| !v),
            UnaryOp::TypeOf | UnaryOp::Plus | UnaryOp::Void | UnaryOp::Delete => None,
        },
        Expr::Tpl(_) => {
            getStringValue(expr, unresolved_ctxt).and_then(|s| ecma_string_to_big_int(&s))
        }
        _ => None,
    }
}

/// Returns true if calling this callee may have side-effects.
pub fn function_call_may_have_side_effects(callee: &Expr, unresolved_ctxt: SyntaxContext) -> bool {
    if let Expr::Ident(callee) = callee {
        if callee.ctxt == unresolved_ctxt
            && matches!(
                callee.sym,
                js_word!("Object")
                    | js_word!("Array")
                    | js_word!("String")
                    | js_word!("Number")
                    | js_word!("BigInt")
                    | js_word!("Boolean")
                    | js_word!("RegExp")
                    | js_word!("Error")
            )
        {
            return false;
        }
    }

    let member = match callee {
        Expr::Member(m) => Some(m),
        Expr::OptChain(opt) => match opt.expr.as_ref() {
            Expr::Member(m) => Some(m),
            _ => None,
        },
        _ => None,
    };

    if let Some(member) = member {
        if !member.computed
            && let Expr::Ident(prop) = member.prop.as_ref()
        {
            if let ExprOrSuper::Expr(obj) = &member.obj {
                if let Expr::Ident(obj) = obj.as_ref() {
                    // TODO: lots of other built-in props we can optimise e.g.
                    // String.raw.
                    if obj.ctxt == unresolved_ctxt && obj.sym == js_word!("Math") {
                        if matches!(
                            prop.sym.as_ref(),
                            "abs"
                                | "acos"
                                | "acosh"
                                | "asin"
                                | "asinh"
                                | "atan"
                                | "atanh"
                                | "atan2"
                                | "cbrt"
                                | "ceil"
                                | "cos"
                                | "cosh"
                                | "exp"
                                | "expm1"
                                | "floor"
                                | "hypot"
                                | "log"
                                | "log10"
                                | "log1p"
                                | "log2"
                                | "max"
                                | "min"
                                | "pow"
                                | "round"
                                | "sign"
                                | "sin"
                                | "sinh"
                                | "sqrt"
                                | "tan"
                                | "tanh"
                                | "trunc"
                                | "random"
                        ) {
                            return false;
                        }
                    }
                }
            }
        }
    }

    true
}

/// Returns true if the expression may have side effects when executed.
pub fn expr_may_have_side_effects(expr: &Expr, unresolved_ctxt: SyntaxContext) -> bool {
    match expr {
        // Context switches can conceal side-effects.
        Expr::Yield(_) | Expr::Await(_) => true,
        Expr::This(_) => false,
        Expr::Array(array) => array.elems.iter().any(|el| match el {
            Some(el) => expr_or_spread_may_have_side_effects(el, unresolved_ctxt),
            None => false,
        }),
        Expr::Object(object) => object.props.iter().any(|prop| match prop {
            Prop::KeyValue(prop) => {
                prop_name_may_have_side_effects(&prop.key, unresolved_ctxt)
                    || expr_may_have_side_effects(&prop.value, unresolved_ctxt)
            }
            Prop::Assign(_) => unreachable!(),
            Prop::Getter(GetterProp { key, .. })
            | Prop::Setter(SetterProp { key, .. })
            | Prop::Method(MethodProp { key, .. }) => {
                prop_name_may_have_side_effects(key, unresolved_ctxt)
            }
            Prop::Spread(spread) => expr_may_have_side_effects(&spread.expr, unresolved_ctxt),
        }),
        Expr::Fn(_) => false,
        Expr::Unary(unary) => match unary.op {
            UnaryOp::Minus
            | UnaryOp::Plus
            | UnaryOp::Bang
            | UnaryOp::Tilde
            | UnaryOp::TypeOf
            | UnaryOp::Void => expr_may_have_side_effects(&unary.arg, unresolved_ctxt),
            UnaryOp::Delete => true,
        },
        Expr::Update(_) => true,
        Expr::Bin(bin) => {
            expr_may_have_side_effects(&bin.left, unresolved_ctxt)
                || expr_may_have_side_effects(&bin.right, unresolved_ctxt)
        }
        Expr::Assign(assign) => assign_expr_may_have_side_effects(assign, unresolved_ctxt),
        Expr::Member(member) => {
            expr_may_have_side_effects(&member.prop, unresolved_ctxt)
                || expr_or_super_may_have_side_effects(&member.obj, unresolved_ctxt)
        }
        Expr::Cond(cond) => {
            expr_may_have_side_effects(&cond.test, unresolved_ctxt)
                || expr_may_have_side_effects(&cond.cons, unresolved_ctxt)
                || expr_may_have_side_effects(&cond.alt, unresolved_ctxt)
        }
        // calls to functions that have no side effects have the no
        // side effect property set.
        Expr::Call(call) => {
            (match &call.callee {
                ExprOrSuper::Super(_) => false,
                ExprOrSuper::Expr(callee) => {
                    function_call_may_have_side_effects(callee, unresolved_ctxt)
                }
            }) || {
                call.args
                    .iter()
                    .any(|a| expr_or_spread_may_have_side_effects(a, unresolved_ctxt))
            }
        }
        Expr::New(new) => {
            constructorCallHasSideEffects(new, unresolved_ctxt)
                || new.args.as_ref().is_some_and(|args| {
                    args.iter()
                        .any(|a| expr_or_spread_may_have_side_effects(a, unresolved_ctxt))
                })
        }
        Expr::Seq(seq) => seq
            .exprs
            .iter()
            .any(|e| expr_may_have_side_effects(e, unresolved_ctxt)),
        Expr::Ident(_) => false,
        Expr::Lit(_) => false,
        Expr::Tpl(tpl) => tpl
            .exprs
            .iter()
            .any(|e| expr_may_have_side_effects(e, unresolved_ctxt)),
        Expr::TaggedTpl(tpl) => {
            function_call_may_have_side_effects(&tpl.tag, unresolved_ctxt)
                || expr_may_have_side_effects(&tpl.tag, unresolved_ctxt)
                || tpl
                    .tpl
                    .exprs
                    .iter()
                    .any(|e| expr_may_have_side_effects(e, unresolved_ctxt))
        }
        Expr::Arrow(_) => false,
        Expr::Class(class) => {
            class
                .class
                .extends
                .as_ref()
                .is_some_and(|e| expr_may_have_side_effects(&e.super_class, unresolved_ctxt))
                || class.class.body.iter().any(|m| match m {
                    ClassMember::Constructor(_) => false,
                    ClassMember::Method(method) => {
                        prop_name_may_have_side_effects(&method.key, unresolved_ctxt)
                    }
                    ClassMember::PrivateMethod(_) => false,
                    ClassMember::ClassProp(prop) => {
                        prop_name_may_have_side_effects(&prop.key, unresolved_ctxt)
                            || (prop.is_static
                                && prop.value.as_ref().is_some_and(|v| {
                                    expr_may_have_side_effects(&v, unresolved_ctxt)
                                }))
                    }
                    ClassMember::PrivateProp(prop) => {
                        prop.is_static
                            && prop
                                .value
                                .as_ref()
                                .is_some_and(|v| expr_may_have_side_effects(&v, unresolved_ctxt))
                    }
                })
        }
        Expr::MetaProp(_) => false,
        Expr::PrivateName(_) => false,
        Expr::OptChain(opt) => expr_may_have_side_effects(&opt.expr, unresolved_ctxt),
        Expr::Invalid(_) => unreachable!(),
    }
}

fn assign_expr_may_have_side_effects(assign: &AssignExpr, unresolved_ctxt: SyntaxContext) -> bool {
    // Assignments will have side effects if:
    // a) The RHS has side effects; or
    // b) The LHS has side effects; or
    // c) A name on the LHS will exist beyond this expression;

    // We assume that assigning to a plain identifier has side-effects.
    match &assign.left {
        PatOrExpr::Expr(lhs) => match lhs.as_ref() {
            Expr::Ident(_) => return true,
            _ => {}
        },
        PatOrExpr::Pat(lhs) => match lhs.as_ref() {
            Pat::Ident(_) => return true,
            Pat::Expr(lhs) => match lhs.as_ref() {
                Expr::Ident(_) => return true,
                _ => {}
            },
            _ => {}
        },
    }

    if expr_may_have_side_effects(&assign.right, unresolved_ctxt) {
        return true;
    }

    let lhs_may_have_side_effects = match &assign.left {
        PatOrExpr::Expr(lhs) => expr_may_have_side_effects(lhs, unresolved_ctxt),
        PatOrExpr::Pat(lhs) => pat_may_have_side_effects(lhs, unresolved_ctxt),
    };

    if lhs_may_have_side_effects {
        return true;
    }

    let member = match &assign.left {
        PatOrExpr::Expr(lhs) => match lhs.as_ref() {
            Expr::Member(m) => Some(m),
            _ => None,
        },
        PatOrExpr::Pat(lhs) => match lhs.as_ref() {
            Pat::Expr(lhs) => match lhs.as_ref() {
                Expr::Member(m) => Some(m),
                _ => None,
            },
            _ => None,
        },
    };

    if let Some(_) = member {
        // TODO: port closure's logic.
        true
    } else {
        true
    }
}

fn pat_may_have_side_effects(pat: &Pat, unresolved_ctxt: SyntaxContext) -> bool {
    match pat {
        Pat::Ident(_) => false,
        // Array destructuring iterates the RHS, which can have side-effects.
        Pat::Array(_) => true,
        Pat::Rest(rest) => pat_may_have_side_effects(&rest.arg, unresolved_ctxt),
        Pat::Object(obj) => obj.props.iter().any(|p| match p {
            ObjectPatProp::KeyValue(key_value_pat_prop) => {
                prop_name_may_have_side_effects(&key_value_pat_prop.key, unresolved_ctxt)
                    || pat_may_have_side_effects(&key_value_pat_prop.value, unresolved_ctxt)
            }
            ObjectPatProp::Rest(rest) => pat_may_have_side_effects(&rest.arg, unresolved_ctxt),
        }),
        Pat::Assign(assign) => {
            pat_may_have_side_effects(&assign.left, unresolved_ctxt)
                || expr_may_have_side_effects(&assign.right, unresolved_ctxt)
        }
        Pat::Expr(expr) => expr_may_have_side_effects(expr, unresolved_ctxt),

        Pat::Invalid(_) => unreachable!(),
    }
}

fn expr_or_super_may_have_side_effects(
    expr_or_super: &ExprOrSuper,
    unresolved_ctxt: SyntaxContext,
) -> bool {
    match expr_or_super {
        ExprOrSuper::Super(_) => false,
        ExprOrSuper::Expr(expr) => expr_may_have_side_effects(expr, unresolved_ctxt),
    }
}

fn expr_or_spread_may_have_side_effects(
    expr_or_spread: &ExprOrSpread,
    unresolved_ctxt: SyntaxContext,
) -> bool {
    match expr_or_spread {
        ExprOrSpread::Spread(spread) => {
            !isPureIterable(&spread.expr)
                || expr_may_have_side_effects(&spread.expr, unresolved_ctxt)
        }
        ExprOrSpread::Expr(expr) => expr_may_have_side_effects(expr, unresolved_ctxt),
    }
}

fn prop_name_may_have_side_effects(name: &PropName, unresolved_ctxt: SyntaxContext) -> bool {
    match name {
        PropName::Computed(name) => expr_may_have_side_effects(&name.expr, unresolved_ctxt),
        PropName::Ident(_) | PropName::Str(_) | PropName::Num(_) | PropName::BigInt(_) => false,
    }
}

/// Do calls to this constructor have side effects?
pub fn constructorCallHasSideEffects(new: &NewExpr, unresolved_ctxt: SyntaxContext) -> bool {
    match new.callee.as_ref() {
        Expr::Ident(callee) => {
            let is_pure_built_in = callee.ctxt == unresolved_ctxt
                && matches!(
                    callee.sym,
                    js_word!("Array")
                        | js_word!("Date")
                        | js_word!("Error")
                        | js_word!("Object")
                        | js_word!("RegExp")
                        | js_word!("XMLHttpRequest")
                );

            if is_pure_built_in { false } else { true }
        }
        _ => true,
    }
}

/// Returns true if `expr` is guaranteed to be an `Iterable` that causes no
/// side-effects during iteration, false otherwise.
pub fn isPureIterable(expr: &Expr) -> bool {
    match expr {
        // These iterables are known to be pure.
        Expr::Array(_) | Expr::Tpl(_) | Expr::Lit(Lit::Str(_)) => true,
        // Anything else, including a non-iterable (e.g. `null`), would be
        // impure.
        _ => false,
    }
}
