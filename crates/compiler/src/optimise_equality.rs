use ast::*;
use atoms::{JsWord, js_word};
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::SyntaxContext;

use crate::utils::unwrap_as;

pub fn process(ast: &mut Program, unresolved_ctxt: SyntaxContext) {
    let mut visitor = Visitor { unresolved_ctxt };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor {
    unresolved_ctxt: SyntaxContext,
}

impl VisitMut<'_> for Visitor {
    // TODO: Just visit BinExpr.
    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        expr.visit_mut_children_with(self);

        let Expr::Bin(bin_expr) = expr else {
            return;
        };

        // TODO: extract to function.
        if bin_expr.op == BinaryOp::EqEqEq || bin_expr.op == BinaryOp::NotEqEq {
            let new_loose_op = match bin_expr.op {
                BinaryOp::EqEqEq => BinaryOp::EqEq,
                BinaryOp::NotEqEq => BinaryOp::NotEq,
                _ => unreachable!(),
            };

            if can_change_strict_to_loose(&bin_expr.left, &bin_expr.right, self.unresolved_ctxt) {
                bin_expr.op = new_loose_op;
            }
        }

        let is_lhs_typeof = matches!(
            bin_expr.left.as_ref(),
            Expr::Unary(UnaryExpr {
                op: UnaryOp::TypeOf,
                ..
            })
        );

        let is_rhs_typeof = matches!(
            bin_expr.right.as_ref(),
            Expr::Unary(UnaryExpr {
                op: UnaryOp::TypeOf,
                ..
            })
        );

        let is_lhs_undefined_string_lit = matches!(
            bin_expr.left.as_ref(),
            Expr::Lit(Lit::Str(Str {
                value: js_word!("undefined"),
                ..
            }))
        );
        let is_rhs_undefined_string_lit = matches!(
            bin_expr.right.as_ref(),
            Expr::Lit(Lit::Str(Str {
                value: js_word!("undefined"),
                ..
            }))
        );

        // TODO: extract to function.

        // This optimisation isn't safe on Internet Explorer, which returned
        // "unknown" for the type of some objects.
        if (is_lhs_typeof && is_rhs_undefined_string_lit)
            || (is_lhs_undefined_string_lit && is_rhs_typeof)
        {
            // `typeof x != "undefined"` => `typeof x < "u"`
            if is_lhs_typeof && bin_expr.op == BinaryOp::NotEq {
                bin_expr.op = BinaryOp::Lt;
            }

            // `typeof x == "undefined"` => `typeof x > "u"`
            if is_lhs_typeof && bin_expr.op == BinaryOp::EqEq {
                bin_expr.op = BinaryOp::Gt;
            }

            // `"undefined" != typeof x` => `"u" > typeof x`
            if is_rhs_typeof && bin_expr.op == BinaryOp::NotEq {
                bin_expr.op = BinaryOp::Gt;
            }

            // `"undefined" == typeof x` => `"u" < typeof x`
            if is_rhs_typeof && bin_expr.op == BinaryOp::EqEq {
                bin_expr.op = BinaryOp::Lt;
            }

            // TODO: get mut ref from match above rather than matches!+unwrap_as!.
            let string_lit = if is_lhs_undefined_string_lit {
                unwrap_as!(bin_expr.left.as_mut(), Expr::Lit(Lit::Str(s)), s)
            } else {
                unwrap_as!(bin_expr.right.as_mut(), Expr::Lit(Lit::Str(s)), s)
            };

            string_lit.value = JsWord::from("u");
        }
    }
}

fn can_change_strict_to_loose(a: &Expr, b: &Expr, unresolved_ctxt: SyntaxContext) -> bool {
    let a_type = get_primitive_type_of_expr(a, unresolved_ctxt);
    let b_type = get_primitive_type_of_expr(b, unresolved_ctxt);

    // We can change strict to loose equality iff we can statically determine
    // the types of the lhs and rhs, and those types are the same.
    a_type == b_type && a_type.is_some()
}

#[derive(PartialEq)]
enum Primitive {
    String,
    Number,
    Undefined,
    Null,
    Boolean,
    BigInt,
}

/// Returns None if the type is not primitive or not statically determinable.
fn get_primitive_type_of_expr(
    mut expr: &Expr,
    unresolved_ctxt: SyntaxContext,
) -> Option<Primitive> {
    loop {
        match expr {
            Expr::Array(_)
            | Expr::Object(_)
            | Expr::Fn(_)
            | Expr::Member(_)
            | Expr::Call(_)
            | Expr::New(_)
            | Expr::Arrow(_)
            | Expr::Class(_)
            | Expr::Yield(_)
            | Expr::PrivateName(_)
            | Expr::OptChain(_)
            | Expr::Invalid(_)
            | Expr::This(_)
            | Expr::TaggedTpl(_)
            | Expr::MetaProp(_) => return None,

            // This is only true for primitive arguments to await.
            Expr::Await(await_expr) => expr = &await_expr.arg,

            Expr::Ident(ident) => {
                if ident.ctxt == unresolved_ctxt {
                    return match ident.sym {
                        js_word!("undefined") => Some(Primitive::Undefined),
                        js_word!("NaN") | js_word!("Infinity") => Some(Primitive::Number),
                        _ => None,
                    };
                } else {
                    return None;
                }
            }

            Expr::Unary(unary_expr) => {
                return match unary_expr.op {
                    // Cannot be BigInt as +BigInt throws a type error.
                    UnaryOp::Plus => Some(Primitive::Number),
                    UnaryOp::Bang => Some(Primitive::Boolean),
                    UnaryOp::Tilde | UnaryOp::Minus => {
                        let arg = get_primitive_type_of_expr(&unary_expr.arg, unresolved_ctxt);
                        if arg == Some(Primitive::BigInt) {
                            return Some(Primitive::BigInt);
                        }
                        if arg.is_some() {
                            return Some(Primitive::Number);
                        }
                        // BigInt if arg is BigInt, otherwise Number.
                        return None;
                    }
                    UnaryOp::TypeOf => Some(Primitive::String),
                    UnaryOp::Void => Some(Primitive::Undefined),
                    UnaryOp::Delete => Some(Primitive::Boolean),
                };
            }
            // BigInt if arg is BigInt, otherwise Number.
            Expr::Update(_) => return None,
            Expr::Bin(bin_expr) => {
                return match bin_expr.op {
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::EqEqEq
                    | BinaryOp::NotEqEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq
                    | BinaryOp::In
                    | BinaryOp::InstanceOf => Some(Primitive::Boolean),

                    BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                        merge_primitive_types(&bin_expr.left, &bin_expr.right, unresolved_ctxt)
                    }
                    BinaryOp::Add => {
                        let left = get_primitive_type_of_expr(&bin_expr.left, unresolved_ctxt);
                        let right = get_primitive_type_of_expr(&bin_expr.right, unresolved_ctxt);

                        if left == Some(Primitive::String) || right == Some(Primitive::String) {
                            return Some(Primitive::String);
                        }

                        if left == Some(Primitive::BigInt) && right == Some(Primitive::BigInt) {
                            return Some(Primitive::BigInt);
                        }

                        if left.is_some()
                            && left != Some(Primitive::BigInt)
                            && right.is_some()
                            && right != Some(Primitive::BigInt)
                        {
                            return Some(Primitive::Number);
                        }

                        // Number, BigInt, or String.
                        return None;
                    }

                    // BigInt or Number.
                    BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod
                    | BinaryOp::Exp
                    | BinaryOp::BitOr
                    | BinaryOp::BitXor
                    | BinaryOp::BitAnd
                    | BinaryOp::LShift
                    | BinaryOp::RShift
                    | BinaryOp::ZeroFillRShift => None,

                    BinaryOp::NullishCoalescing => {
                        let left = get_primitive_type_of_expr(&bin_expr.left, unresolved_ctxt);
                        let right = get_primitive_type_of_expr(&bin_expr.right, unresolved_ctxt);

                        if left == Some(Primitive::Null) || left == Some(Primitive::Undefined) {
                            return right;
                        }

                        if left.is_some() {
                            // Definitely not Null or Undefined.
                            return left;
                        }

                        None
                    }
                };
            }
            Expr::Assign(assign_expr) => match assign_expr.op {
                AssignOp::Assign => expr = &assign_expr.right,
                AssignOp::AddAssign => {
                    let right = get_primitive_type_of_expr(&assign_expr.right, unresolved_ctxt);

                    if right == Some(Primitive::String) {
                        return Some(Primitive::String);
                    }

                    // Number, BigInt, or String.
                    return None;
                }

                // BigInt or Number.
                AssignOp::SubAssign
                | AssignOp::MulAssign
                | AssignOp::DivAssign
                | AssignOp::ModAssign
                | AssignOp::ExpAssign
                | AssignOp::BitOrAssign
                | AssignOp::BitXorAssign
                | AssignOp::BitAndAssign
                | AssignOp::LShiftAssign
                | AssignOp::RShiftAssign
                | AssignOp::ZeroFillRShiftAssign => return None,

                // The type of these expressions is either the type of the lhs
                // or rhs, but the only valid lhs for these assign ops are
                // identifiers/member expressions, neither of which we can
                // determine the type of, so the type of the whole assign is
                // effectively unknown.
                AssignOp::AndAssign | AssignOp::OrAssign | AssignOp::NullishAssign => return None,
            },
            Expr::Cond(cond_expr) => {
                return merge_primitive_types(&cond_expr.cons, &cond_expr.alt, unresolved_ctxt);
            }
            Expr::Seq(seq_expr) => expr = seq_expr.exprs.last().unwrap(),
            Expr::Lit(lit) => match lit {
                Lit::Str(_) => return Some(Primitive::String),
                Lit::Bool(_) => return Some(Primitive::Boolean),
                Lit::Null(_) => return Some(Primitive::Null),
                Lit::Num(_) => return Some(Primitive::Number),
                Lit::BigInt(_) => return Some(Primitive::BigInt),
                Lit::Regex(_) => return None,
            },
            Expr::Tpl(_) => return Some(Primitive::String),
        }
    }
}

fn merge_primitive_types(a: &Expr, b: &Expr, unresolved_ctxt: SyntaxContext) -> Option<Primitive> {
    let a_type = get_primitive_type_of_expr(a, unresolved_ctxt);

    if a_type.is_none() {
        return None;
    }

    let b_type = get_primitive_type_of_expr(b, unresolved_ctxt);

    if b_type.is_none() {
        return None;
    }

    if a_type == b_type {
        return a_type;
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{GLOBALS, Globals, Mark};

    #[test]
    fn test_combined_optimisation() {
        test_transform("typeof x !== 'undefined'", "typeof x < 'u'");
        test_transform("typeof x === 'undefined'", "typeof x > 'u'");
        test_transform("'undefined' !== typeof x", "'u' > typeof x");
        test_transform("'undefined' === typeof x", "'u' < typeof x");
    }

    #[test]
    fn test_loose_equality_to_relational_op() {
        test_transform("typeof x != 'undefined'", "typeof x < 'u'");
        test_transform("typeof x == 'undefined'", "typeof x > 'u'");
        test_transform("'undefined' != typeof x", "'u' > typeof x");
        test_transform("'undefined' == typeof x", "'u' < typeof x");

        // This optimisation should only apply to 'undefined'.
        test_same("typeof x != 'string'");
        test_same("typeof x == 'string'");
        test_same("'string' != typeof x");
        test_same("'string' == typeof x");

        test_same("typeof x != 'bigint'");
        test_same("typeof x == 'bigint'");
        test_same("'bigint' != typeof x");
        test_same("'bigint' == typeof x");
    }

    #[test]
    fn test_strict_to_loose_equality_cases() {
        const STRING: &str = "'string'";
        const NUMBER: &str = "1";
        const BOOLEAN: &str = "true";
        const BIG_INT: &str = "1n";
        const UNDEFINED: &str = "undefined";
        const NULL: &str = "null";

        // Expr::Ident - `undefined`.
        test_strict_to_loose_equality_case("undefined", UNDEFINED);
        // Expr::Ident - `NaN` / `Infinity`.
        test_strict_to_loose_equality_case("NaN", NUMBER);
        test_strict_to_loose_equality_case("Infinity", NUMBER);

        // Expr::Unary - `+x`.
        test_strict_to_loose_equality_case("+x", NUMBER);
        // Expr::Unary - `!x`.
        test_strict_to_loose_equality_case("!x", BOOLEAN);
        // Expr::Unary - `-x` where the arg is a Number.
        test_strict_to_loose_equality_case("-1", NUMBER);
        // Expr::Unary - `~x` where the arg is a Number.
        test_strict_to_loose_equality_case("~1", NUMBER);
        // Expr::Unary - `-x` where the arg is a BigInt.
        test_strict_to_loose_equality_case("-1n", BIG_INT);
        // Expr::Unary - `~x` where the arg is a BigInt.
        test_strict_to_loose_equality_case("~1n", BIG_INT);
        // Expr::Unary - `typeof x`.
        test_strict_to_loose_equality_case("typeof x", STRING);
        // Expr::Unary - `void x`.
        test_strict_to_loose_equality_case("void x", UNDEFINED);
        // Expr::Unary - `delete x.y`.
        test_strict_to_loose_equality_case("delete x.y", BOOLEAN);

        // Expr::Bin - comparison/relational ops.
        test_strict_to_loose_equality_case("(1 == null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 != null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 === null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 !== null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 < null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 <= null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 > null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 >= null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 in null)", BOOLEAN);
        test_strict_to_loose_equality_case("(1 instanceof null)", BOOLEAN);

        // Expr::Bin - `||` / `&&` (merged type of both operands).
        test_strict_to_loose_equality_case("(1 || 2)", NUMBER);
        test_strict_to_loose_equality_case("(1 && 2)", NUMBER);

        // Expr::Bin - `+` where either operand is a String.
        test_strict_to_loose_equality_case("(1 + 'a')", STRING);
        // Expr::Bin - `+` where both operands are BigInt.
        test_strict_to_loose_equality_case("(1n + 2n)", BIG_INT);
        // Expr::Bin - `+` where both operands have known type that's not
        // BigInt.
        test_strict_to_loose_equality_case("(1 + 2)", NUMBER);
        test_strict_to_loose_equality_case("(1 + null)", NUMBER);
        test_strict_to_loose_equality_case("(null + undefined)", NUMBER);

        // Expr::Bin - `??` where the lhs is Null/Undefined (result is rhs's
        // type).
        test_strict_to_loose_equality_case("(null ?? 1)", NUMBER);
        test_strict_to_loose_equality_case("(undefined ?? 1)", NUMBER);
        // Expr::Bin - `??` where the lhs has a determinable, non-nullish type
        // (result is lhs's type),
        test_strict_to_loose_equality_case("(1 ?? 's')", NUMBER);
        test_strict_to_loose_equality_case("(1 ?? unknown)", NUMBER);

        // Expr::Assign - `x = y` (result is rhs's type),
        test_strict_to_loose_equality_case("(x = 1)", NUMBER);
        // Expr::Assign - `x += y` where the rhs is a String,
        test_strict_to_loose_equality_case("(x += 'a')", STRING);

        // Expr::Cond - `x ? a : b` (merged type of cons/alt),
        test_strict_to_loose_equality_case("(x ? 1 : 2)", NUMBER);

        // Expr::Seq - `(a, b)` (type of the last expr),
        test_strict_to_loose_equality_case("('s', 2)", NUMBER);

        // Expr::Lit - String / Boolean / Null / Number / BigInt,
        test_strict_to_loose_equality_case("'a'", STRING);
        test_strict_to_loose_equality_case("true", BOOLEAN);
        test_strict_to_loose_equality_case("null", NULL);
        test_strict_to_loose_equality_case("1", NUMBER);
        test_strict_to_loose_equality_case("1n", BIG_INT);

        // Expr::Tpl - template literal,
        test_strict_to_loose_equality_case("`template`", STRING);

        // Expr::Await.
        test_transform(
            "async function f() { (await 1) === 1; }",
            "async function f() { (await 1) == 1; }",
        );
    }

    #[test]
    fn test_strict_to_loose_equality_non_cases() {
        const NUMBER: &str = "1";

        // Expr::Ident - not `undefined` / `NaN` / `Infinity`.
        test_strict_to_loose_equality_non_case("x", NUMBER);

        // Expr::Unary - `-x` / `~x` where the arg's type is not determinable.
        test_strict_to_loose_equality_non_case("-x", NUMBER);
        test_strict_to_loose_equality_non_case("~x", NUMBER);

        // Expr::Update - `x++` / `++x` (always indeterminate).
        test_strict_to_loose_equality_non_case("x++", NUMBER);

        // Expr::Bin - `||` where the lhs type is not determinable.
        test_strict_to_loose_equality_non_case("(x || 1)", NUMBER);
        // Expr::Bin - `||` where the lhs is determinable but the rhs is not.
        // In this example the type of `1 || x` is obviously "number" since 1 is
        // truthy, but our analysis only considers types, not values.
        test_strict_to_loose_equality_non_case("(1 || x)", NUMBER);
        // Expr::Bin - `||` where both sides are determinable but differ.
        // Same as above - we can't tell that `1 || 'string'` is always 1 by
        // looking only at types.
        test_strict_to_loose_equality_non_case("(1 || 'string')", NUMBER);

        // Expr::Bin - `&&` where the lhs type is not determinable.
        test_strict_to_loose_equality_non_case("(x && 1)", NUMBER);
        // Expr::Bin - `&&` where the lhs is determinable but the rhs is not.
        test_strict_to_loose_equality_non_case("(1 && x)", NUMBER);
        // Expr::Bin - `&&` where both sides are determinable but differ.
        // Same as above - we can't tell that `1 && 'string'` is always 'string'
        // by looking only at types.
        test_strict_to_loose_equality_non_case("(1 && 'string')", NUMBER);

        // Expr::Bin - `+` where the lhs type is not determinable.
        test_strict_to_loose_equality_non_case("(x + 1)", NUMBER);
        // Expr::Bin - `+` where the rhs type is not determinable.
        test_strict_to_loose_equality_non_case("(1 + x)", NUMBER);
        // Expr::Bin - `+` where neither side's type is determinable.
        test_strict_to_loose_equality_non_case("(x + x)", NUMBER);
        // Expr::Bin - `+` where one side is BigInt and the other is Number.
        test_strict_to_loose_equality_non_case("(1n + 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 + 1n)", NUMBER);

        // Expr::Bin - arithmetic/bitwise ops (always indeterminate, regardless
        // of operands).
        test_strict_to_loose_equality_non_case("(1 - 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 * 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 / 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 % 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 ** 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 | 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 ^ 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 & 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 << 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 >> 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(1 >>> 1)", NUMBER);

        // Expr::Bin - `??` where the lhs type is not determinable.
        test_strict_to_loose_equality_non_case("(x ?? 1)", NUMBER);
        // Expr::Bin - `??` where the lhs is Null/Undefined and the rhs type is
        // not determinable.
        test_strict_to_loose_equality_non_case("(null ?? x)", NUMBER);
        test_strict_to_loose_equality_non_case("(undefined ?? x)", NUMBER);

        // Expr::Assign - `x = y` where the rhs type is not determinable.
        test_strict_to_loose_equality_non_case("(x = x)", NUMBER);
        // Expr::Assign - `x += y` where the rhs is not a String.
        test_strict_to_loose_equality_non_case("(x += 1)", NUMBER);

        // Expr::Assign - compound assignment ops other than `+=` (always
        // indeterminate).
        test_strict_to_loose_equality_non_case("(x -= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x *= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x /= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x %= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x **= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x |= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x ^= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x &= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x <<= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x >>= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x >>>= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x &&= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x ||= 1)", NUMBER);
        test_strict_to_loose_equality_non_case("(x ??= 1)", NUMBER);

        // Expr::Cond - consequent type is not determinable.
        test_strict_to_loose_equality_non_case("(x ? y : 1)", NUMBER);
        // Expr::Cond - alternate type is not determinable.
        test_strict_to_loose_equality_non_case("(x ? 1 : y)", NUMBER);
        // Expr::Cond - both branches determinable but differ.
        test_strict_to_loose_equality_non_case("(x ? 1 : 'string')", NUMBER);

        // Expr::Seq - type of the last expr is not determinable.
        test_strict_to_loose_equality_non_case("(1, x)", NUMBER);

        // Expr::Lit - regex literal.
        test_strict_to_loose_equality_non_case("/re/", NUMBER);

        // Expr::Array / Object / Fn / Member / Call / New / Arrow / Class /
        // OptChain / This / TaggedTpl - always indeterminate, regardless of
        // contents.
        test_strict_to_loose_equality_non_case("[]", NUMBER);
        test_strict_to_loose_equality_non_case("({})", NUMBER);
        test_strict_to_loose_equality_non_case("(function() {})", NUMBER);
        test_strict_to_loose_equality_non_case("x.y", NUMBER);
        test_strict_to_loose_equality_non_case("f()", NUMBER);
        test_strict_to_loose_equality_non_case("new Foo()", NUMBER);
        test_strict_to_loose_equality_non_case("(() => {})", NUMBER);
        test_strict_to_loose_equality_non_case("(class {})", NUMBER);
        test_strict_to_loose_equality_non_case("x?.y", NUMBER);
        test_strict_to_loose_equality_non_case("this", NUMBER);
        test_strict_to_loose_equality_non_case("tag`template`", NUMBER);

        // Expr::Yield.
        test_same("function* f() { (yield 1) === 1; }");
    }

    fn test_strict_to_loose_equality_case(a: &str, b: &str) {
        test_transform(&format!("{a} === {b}"), &format!("{a} == {b}"));
        test_transform(&format!("{a} !== {b}"), &format!("{a} != {b}"));

        // Ensure the optimisation is symmetric.
        test_transform(&format!("{b} === {a}"), &format!("{b} == {a}"));
        test_transform(&format!("{b} !== {a}"), &format!("{b} != {a}"));
    }

    fn test_strict_to_loose_equality_non_case(a: &str, b: &str) {
        test_transform(&format!("{a} === {b}"), &format!("{a} === {b}"));
        test_transform(&format!("{a} !== {b}"), &format!("{a} !== {b}"));

        // Ensure the optimisation is symmetric.
        test_transform(&format!("{b} === {a}"), &format!("{b} === {a}"));
        test_transform(&format!("{b} !== {a}"), &format!("{b} !== {a}"));
    }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, _program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                    process(&mut program, unresolved_ctxt);

                    program
                })
            },
            input,
            expected,
        );
    }

    fn test_same(input: &str) {
        test_transform(input, input);
    }
}
