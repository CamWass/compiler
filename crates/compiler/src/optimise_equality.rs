use ast::*;
use visit::{VisitMut, VisitMutWith};

use crate::node_util::getKnownValueType;

pub fn process(ast: &mut Program) {
    let mut visitor = Visitor;
    ast.visit_mut_with(&mut visitor);
}

struct Visitor;

impl VisitMut<'_> for Visitor {
    fn visit_mut_bin_expr(&mut self, bin_expr: &mut BinExpr) {
        bin_expr.visit_mut_children_with(self);

        optimise_strict_equality(bin_expr);

        // Note: this comes last so we can benefit from the strict->loose
        // optimisation above.
        optimise_loose_equality(bin_expr);
    }
}

fn optimise_strict_equality(bin_expr: &mut BinExpr) {
    let new_loose_op = match bin_expr.op {
        BinaryOp::EqEqEq => BinaryOp::EqEq,
        BinaryOp::NotEqEq => BinaryOp::NotEq,
        _ => return,
    };

    if can_change_strict_to_loose(&bin_expr.left, &bin_expr.right) {
        bin_expr.op = new_loose_op;
    }
}

fn optimise_loose_equality(bin_expr: &mut BinExpr) {
    let is_lhs_typeof = is_typeof(&bin_expr.left);
    let is_rhs_typeof = is_typeof(&bin_expr.right);

    // This optimisation isn't safe on Internet Explorer, which returned
    // "unknown" for the type of some objects.

    if is_lhs_typeof && let Some(undefined_string) = get_undefined_string(&mut bin_expr.right) {
        // `typeof x != "undefined"` => `typeof x < "u"`
        if bin_expr.op == BinaryOp::NotEq {
            bin_expr.op = BinaryOp::Lt;
        }

        // `typeof x == "undefined"` => `typeof x > "u"`
        if bin_expr.op == BinaryOp::EqEq {
            bin_expr.op = BinaryOp::Gt;
        }

        undefined_string.value = Box::new(String::from("u"));
    }

    if is_rhs_typeof && let Some(undefined_string) = get_undefined_string(&mut bin_expr.left) {
        // `"undefined" != typeof x` => `"u" > typeof x`
        if bin_expr.op == BinaryOp::NotEq {
            bin_expr.op = BinaryOp::Gt;
        }

        // `"undefined" == typeof x` => `"u" < typeof x`
        if bin_expr.op == BinaryOp::EqEq {
            bin_expr.op = BinaryOp::Lt;
        }

        undefined_string.value = Box::new(String::from("u"));
    }
}

fn is_typeof(e: &Expr) -> bool {
    match e {
        Expr::Unary(e) => e.op == UnaryOp::TypeOf,
        _ => false,
    }
}

fn get_undefined_string(e: &mut Expr) -> Option<&mut Str> {
    match e {
        Expr::Lit(Lit::Str(s)) if s.value.as_ref() == "undefined" => Some(s),
        _ => None,
    }
}

fn can_change_strict_to_loose(a: &Expr, b: &Expr) -> bool {
    let a_type = getKnownValueType(a).get_typeof_result();
    let b_type = getKnownValueType(b).get_typeof_result();

    // We can change strict to loose equality iff we can statically determine
    // the types of the lhs and rhs, and those types are the same.
    a_type == b_type && a_type.is_some()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolve;

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

        // Expr::Bin - arithmetic/bitwise ops.
        // Number LHS and RHS results in number.
        test_strict_to_loose_equality_case("(1 - 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 * 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 / 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 % 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 ** 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 | 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 ^ 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 & 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 << 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 >> 1)", NUMBER);
        test_strict_to_loose_equality_case("(1 >>> 1)", NUMBER);
        // If one operand is BigInt, the result is always BigInt.
        test_strict_to_loose_equality_case("(1n - x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n * x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n / x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n % x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n ** x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n | x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n ^ x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n & x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n << x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n >> x)", BIG_INT);
        test_strict_to_loose_equality_case("(1n >>> x)", BIG_INT);

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

        // Non-logical compound assignment ops other than `+=` are Number when
        // RHS is Number...
        test_strict_to_loose_equality_case("(x -= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x *= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x /= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x %= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x **= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x |= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x ^= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x &= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x <<= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x >>= 1)", NUMBER);
        test_strict_to_loose_equality_case("(x >>>= 1)", NUMBER);
        // ...and BigInt when RHS is BigInt.
        test_strict_to_loose_equality_case("(x -= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x *= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x /= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x %= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x **= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x |= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x ^= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x &= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x <<= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x >>= 1n)", BIG_INT);
        test_strict_to_loose_equality_case("(x >>>= 1n)", BIG_INT);

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

        // Logical compound assignments are always indeterminate.
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
            |mut program, program_data| {
                resolve(&mut program, program_data);

                process(&mut program);

                program
            },
            input,
            expected,
        );
    }

    fn test_same(input: &str) {
        test_transform(input, input);
    }
}
