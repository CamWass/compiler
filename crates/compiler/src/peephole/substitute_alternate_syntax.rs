use ast::*;
use common::{DUMMY_SP, util::take::Take};
use visit::{VisitMut, VisitMutWith};

use crate::{
    node_util::{
        TypeFlags, expr_may_have_side_effects, getKnownValueType, isImmutableValue, new_void_zero,
    },
    peephole::getSideEffectFreeStringValue,
    utils::unwrap_as,
};

pub fn process(ast: &mut Program, program_data: &mut TransformerProgramData, late: bool) {
    let mut visitor = Visitor { program_data, late };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut TransformerProgramData,
    late: bool,
}

impl Visitor<'_> {
    fn try_replace_bool(&mut self, expr: &mut Expr, coerced_to_bool: bool) -> bool {
        if !self.late {
            return false;
        }

        if let Expr::Lit(Lit::Bool(bool)) = expr {
            if coerced_to_bool {
                *expr = Expr::Lit(Lit::Num(Number {
                    node_id: self.program_data.new_id_from(bool.node_id),
                    value: if bool.value { 1.0 } else { 0.0 },
                }));
            } else {
                *expr = Expr::Unary(UnaryExpr {
                    node_id: self.program_data.new_id_from(bool.node_id),
                    op: UnaryOp::Bang,
                    arg: Box::new(Expr::Lit(Lit::Num(Number {
                        node_id: self.program_data.new_id_from(bool.node_id),
                        value: if bool.value { 0.0 } else { 1.0 },
                    }))),
                });
            }

            return true;
        }

        false
    }

    fn tryMinimizeArrayLiteral(&mut self, node: &mut Expr) {
        if !self.late {
            return;
        }

        let array = unwrap_as!(node, Expr::Array(a), a);

        let all_strings = array.elems.iter().all(|e| {
            if let Some(ExprOrSpread::Expr(e)) = e {
                matches!(e.as_ref(), Expr::Lit(Lit::Str(_)))
            } else {
                false
            }
        });

        if !all_strings {
            return;
        }

        let numElements = array.elems.len() as isize;
        // We save two bytes per element.
        let saving = numElements * 2 - STRING_SPLIT_OVERHEAD;
        if saving <= 0 {
            return;
        }

        let strings: Vec<_> = array
            .elems
            .iter()
            .map(|e| {
                if let Some(ExprOrSpread::Expr(e)) = e {
                    if let Expr::Lit(Lit::Str(s)) = e.as_ref() {
                        return s.value.as_str();
                    }
                }

                unreachable!();
            })
            .collect();

        // These delimiters are chars that appears a lot in the program therefore
        // probably have a small Huffman encoding.
        if let Some(delimiter) = pick_delimiter(&strings) {
            let template = strings.join(delimiter);
            *node = Expr::Call(CallExpr {
                node_id: self.program_data.new_id_from(array.node_id),
                callee: ExprOrSuper::Expr(Box::new(Expr::Member(MemberExpr {
                    node_id: self.program_data.new_id_from(array.node_id),
                    obj: ExprOrSuper::Expr(Box::new(Expr::Lit(Lit::Str(Str {
                        node_id: self.program_data.new_id_from(array.node_id),
                        value: Box::new(template),
                    })))),
                    prop: Box::new(Expr::Ident(Ident {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        name: id_for_built_in!("split"),
                    })),
                    computed: false,
                }))),
                args: vec![ExprOrSpread::Expr(Box::new(Expr::Lit(Lit::Str(
                    ast::Str {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        value: Box::new(delimiter.to_string()),
                    },
                ))))],
            });
        }
    }

    fn reduce_assign(&mut self, node: &mut Expr) {
        let assign = unwrap_as!(node, Expr::Assign(a), a);

        if assign.op != AssignOp::SubAssign {
            return;
        }

        let AssignTarget::Simple(lhs) = assign.left.as_mut() else {
            return;
        };

        let mut rhs = assign.right.as_ref();
        let mut is_negative = false;

        if let Expr::Unary(unary) = rhs
            && unary.op == UnaryOp::Minus
        {
            is_negative = true;
            rhs = unary.arg.as_ref();
        }

        if let Expr::Lit(Lit::Num(Number { value: 1.0, .. })) = rhs {
            *node = Expr::Update(UpdateExpr {
                node_id: self.program_data.new_id_from(assign.node_id),
                op: if is_negative {
                    UpdateOp::PlusPlus
                } else {
                    UpdateOp::MinusMinus
                },
                prefix: true,
                arg: Box::new(lhs.take()),
            });
        }
    }

    fn tryRotateCommutativeOperator(&mut self, expr: &mut BinExpr) {
        debug_assert!(expr.op.is_commutative());

        if !self.late {
            return;
        }

        let Expr::Bin(right) = expr.right.as_ref() else {
            return;
        };

        let rhs_precedence = right.op.precedence();

        // Transform a * (b / c) to b / c * a
        let rhs = expr.right.as_mut();
        let mut lhs = expr.left.as_mut();
        while matches!(lhs, Expr::Bin(left) if left.op == expr.op && expr.op.is_associative()) {
            if let Expr::Bin(left) = lhs {
                lhs = &mut left.left;
            }
        }
        let precedence = expr.op.precedence();
        let lhs_is_same_precedence = if let Expr::Bin(left) = lhs {
            left.op.precedence() == precedence
        } else {
            false
        };
        if rhs_precedence == precedence && !lhs_is_same_precedence {
            std::mem::swap(rhs, lhs);
        }
    }

    fn tryRotateAssociativeOperator(&mut self, expr: &mut Expr) {
        let bin_expr = unwrap_as!(expr, Expr::Bin(b), b);

        debug_assert!(bin_expr.op.is_associative());

        if !self.late {
            return;
        }

        let Expr::Bin(right) = bin_expr.right.as_mut() else {
            return;
        };

        if bin_expr.op == right.op {
            // Transform a * (b * c) to a * b * c
            let left = bin_expr.left.take();

            *bin_expr.left.as_mut() = Expr::Bin(BinExpr {
                node_id: self.program_data.new_id(DUMMY_SP),
                op: bin_expr.op,
                left: left,
                right: right.left.take(),
            });
            bin_expr.right = right.right.take();
        } else if bin_expr.op.is_commutative() && !expr_may_have_side_effects(expr) {
            let bin_expr = unwrap_as!(expr, Expr::Bin(b), b);
            // Transform a * (b / c) to b / c * a
            self.tryRotateCommutativeOperator(bin_expr)
        }
    }

    fn tryFoldStandardConstructors(&mut self, expr: &mut Expr) {
        let new_expr = unwrap_as!(expr, Expr::New(n), n);

        if canFoldStandardConstructors(new_expr) {
            *expr = Expr::Call(CallExpr {
                node_id: self.program_data.new_id_from(new_expr.node_id),
                callee: ExprOrSuper::Expr(new_expr.callee.take()),
                args: new_expr.args.take().unwrap_or_default(),
            });
        }
    }

    fn tryReduceCall(&mut self, node: &mut Expr) {
        self.tryFoldLiteralConstructor(node);
        if matches!(node, Expr::Call(_)) {
            self.tryFoldSimpleFunctionCall(node);
        }
    }

    /**
     * Replaces a new Array, Object, or RegExp node with a literal, unless the
     * call is to a local constructor function with the same name.
     */
    fn tryFoldLiteralConstructor(&mut self, expr: &mut Expr) {
        let call_expr = unwrap_as!(expr, Expr::Call(c), c);

        let ExprOrSuper::Expr(callee) = &call_expr.callee else {
            return;
        };

        let Expr::Ident(callee) = callee.as_ref() else {
            return;
        };

        let constructorHasArgs = !call_expr.args.is_empty();

        if callee.name == id_for_built_in!("Object") && !constructorHasArgs {
            // "Object()" --> "{}"
            *expr = Expr::Object(ObjectLit {
                node_id: self.program_data.new_id_from(call_expr.node_id),
                props: Vec::new(),
            });
        } else if callee.name == id_for_built_in!("Array") {
            // "Array(arg0, arg1, ...)" --> "[arg0, arg1, ...]"
            let action = isSafeToFoldArrayConstructor(&call_expr.args);

            if action == FoldArrayAction::SafeToFoldWithArgs
                || action == FoldArrayAction::SafeToFoldWithoutArgs
            {
                let elements = if action == FoldArrayAction::SafeToFoldWithArgs {
                    call_expr.args.take().into_iter().map(Some).collect()
                } else {
                    Vec::new()
                };

                *expr = Expr::Array(ArrayLit {
                    node_id: self.program_data.new_id_from(call_expr.node_id),
                    elems: elements,
                });
            }
        }
    }

    fn tryFoldSimpleFunctionCall(&mut self, expr: &mut Expr) {
        let call_expr = unwrap_as!(expr, Expr::Call(c), c);

        let ExprOrSuper::Expr(callee) = &call_expr.callee else {
            return;
        };

        let Expr::Ident(callee) = callee.as_ref() else {
            return;
        };

        if callee.name == id_for_built_in!("Boolean") {
            // Fold Boolean(a) to !!a
            // http://www.ecma-international.org/ecma-262/6.0/index.html#sec-boolean-constructor-boolean-value
            // and
            // http://www.ecma-international.org/ecma-262/6.0/index.html#sec-logical-not-operator-runtime-semantics-evaluation

            // only handle the single known parameter case
            if call_expr.args.len() == 1
                && let Some(ExprOrSpread::Expr(arg)) = call_expr.args.first_mut()
            {
                if getKnownValueType(arg) == TypeFlags::BOOLEAN {
                    // If it is already a boolean do nothing.
                    *expr = arg.as_mut().take();
                } else {
                    // Replace it with a "!!value"
                    *expr = Expr::Unary(UnaryExpr {
                        node_id: self.program_data.new_id_from(call_expr.node_id),
                        op: UnaryOp::Bang,
                        arg: Box::new(Expr::Unary(UnaryExpr {
                            node_id: self.program_data.new_id_from(call_expr.node_id),
                            op: UnaryOp::Bang,
                            arg: arg.take(),
                        })),
                    });
                }
            }
        } else if callee.name == id_for_built_in!("String") {
            // Fold String(a) to '' + (a) on immutable literals,
            // which allows further optimizations
            //
            // We can't do this in the general case, because String(a) has
            // slightly different semantics than '' + (a). See
            // https://blickly.github.io/closure-compiler-issues/#759
            if call_expr.args.len() == 1
                && let Some(ExprOrSpread::Expr(arg)) = call_expr.args.first_mut()
                && isImmutableValue(arg)
            {
                *expr = Expr::Bin(BinExpr {
                    node_id: self.program_data.new_id_from(call_expr.node_id),
                    op: BinaryOp::Add,
                    left: Box::new(Expr::Lit(Lit::Str(Str {
                        node_id: self.program_data.new_id(DUMMY_SP),
                        value: Box::new(String::from("")),
                    }))),
                    right: arg.take(),
                });
            }
        }
    }
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_expr(&mut self, node: &mut Expr) {
        match node {
            Expr::Assign(_) => {
                node.visit_mut_children_with(self);
                self.reduce_assign(node);
            }
            Expr::Lit(Lit::Bool(_)) => {
                self.try_replace_bool(node, false);
            }
            Expr::Bin(bin_expr) => {
                let op = bin_expr.op;
                if matches!(
                    op,
                    BinaryOp::EqEq
                        | BinaryOp::Gt
                        | BinaryOp::GtEq
                        | BinaryOp::LtEq
                        | BinaryOp::Lt
                        | BinaryOp::NotEq
                ) {
                    if !self.try_replace_bool(&mut bin_expr.left, true) {
                        bin_expr.left.visit_mut_with(self);
                    }
                    if !self.try_replace_bool(&mut bin_expr.right, true) {
                        bin_expr.right.visit_mut_with(self);
                    }
                } else {
                    bin_expr.visit_mut_children_with(self);
                }

                if op == BinaryOp::Mul && !expr_may_have_side_effects(node) {
                    let bin_expr = unwrap_as!(node, Expr::Bin(b), b);
                    self.tryRotateCommutativeOperator(bin_expr);
                } else if matches!(
                    op,
                    BinaryOp::LogicalAnd
                        | BinaryOp::LogicalOr
                        | BinaryOp::BitOr
                        | BinaryOp::BitXor
                        | BinaryOp::BitAnd
                        | BinaryOp::NullishCoalescing
                ) {
                    self.tryRotateAssociativeOperator(node);
                }
            }
            Expr::New(_) => {
                node.visit_mut_children_with(self);

                self.tryFoldStandardConstructors(node);
                // tryFoldStandardConstructors() may convert a new expr into a call expr.
                if matches!(node, Expr::Call(_)) {
                    self.tryReduceCall(node)
                }
            }
            Expr::Call(_) => {
                node.visit_mut_children_with(self);

                self.tryReduceCall(node);
            }
            Expr::Ident(ident) => {
                if ident.name == id_for_built_in!("undefined") {
                    *node = new_void_zero(self.program_data, Some(ident.node_id));
                }
            }
            Expr::Array(_) => {
                node.visit_mut_children_with(self);
                self.tryMinimizeArrayLiteral(node);
            }
            Expr::Tpl(_) => {
                node.visit_mut_children_with(self);

                if let Some(string) = getSideEffectFreeStringValue(node) {
                    *node = Expr::Lit(Lit::Str(Str {
                        node_id: self.program_data.new_id_from(node.node_id()),
                        value: Box::new(string.into_owned()),
                    }));
                }
            }
            _ => node.visit_mut_children_with(self),
        }
    }

    fn visit_mut_return_stmt(&mut self, node: &mut ReturnStmt) {
        let Some(arg) = &node.arg else {
            return;
        };

        if let Expr::Unary(unary) = arg.as_ref()
            && unary.op == UnaryOp::Void
            && !expr_may_have_side_effects(&unary.arg)
        {
            node.arg = None;
            return;
        }

        if let Expr::Ident(ident) = arg.as_ref()
            && ident.name == id_for_built_in!("undefined")
        {
            node.arg = None;
            return;
        }

        node.arg.visit_mut_with(self);
    }

    fn visit_mut_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut i = 0;
        // TODO: it might be faster to iterate back-to-front here, or
        // simpler/faster to allocate a new vec and copy element over, since
        // we're removing and inserting elements.
        while i < stmts.len() {
            stmts[i].visit_mut_with(self);

            if !self.late
                && let Stmt::Expr(e) = &stmts[i]
            {
                if let Expr::Seq(_) = e.expr.as_ref() {
                    let mut e = unwrap_as!(stmts[i].take(), Stmt::Expr(e), e);
                    let seq = unwrap_as!(e.expr.as_mut().take(), Expr::Seq(s), s);

                    let num_stmts = seq.exprs.len();

                    let mut new_stmts = Vec::with_capacity(seq.exprs.len());

                    flatten_seq_expr_to_stmt_list(seq, &mut new_stmts, self.program_data);

                    // Insert the new statements at the index of the old one to preserve ordering.
                    stmts.splice(i..=i, new_stmts);

                    if num_stmts > 0 {
                        // Skip over the new stmts.
                        i += num_stmts - 1;
                        continue;
                    } else {
                        i += 1;
                        continue;
                    }
                }
            }

            i += 1;
        }
    }
}

fn flatten_seq_expr_to_stmt_list(
    mut expr: SeqExpr,
    stmts: &mut Vec<Stmt>,
    program_data: &mut TransformerProgramData,
) {
    let exprs = expr.exprs.take();

    for expr in exprs {
        if let Expr::Seq(seq) = expr {
            flatten_seq_expr_to_stmt_list(seq, stmts, program_data);
        } else {
            stmts.push(Stmt::Expr(ExprStmt {
                node_id: program_data.new_id_from(expr.node_id()),
                expr: Box::new(expr),
            }));
        }
    }
}

/// Find a delimiter that does not occur in the given strings.
fn pick_delimiter(strings: &[&str]) -> Option<&'static str> {
    let all_length_1 = strings.iter().all(|s| s.len() == 1);

    if all_length_1 {
        return Some("");
    }

    const DELIMITER_BYTES: [u8; 5] = [b' ', b';', b',', b'{', b'}'];
    const DELIMITER_STRINGS: [&'static str; 5] = [" ", ";", ",", "{", "}"];

    for (delimiter, delimiter_str) in DELIMITER_BYTES.iter().zip(DELIMITER_STRINGS) {
        if strings.iter().any(|s| s.as_bytes().contains(delimiter)) {
            continue;
        }

        return Some(delimiter_str);
    }

    None
}

const STRING_SPLIT_OVERHEAD: isize = ".split('.')".len() as isize;

/**
 * @return Whether "new Object()" can be folded to "Object()" on {@code n}.
 */
fn canFoldStandardConstructors(new_expr: &NewExpr) -> bool {
    if let Expr::Ident(callee) = new_expr.callee.as_ref() {
        // String, Number, and Boolean functions return non-object types, whereas
        // new String, new Number, and new Boolean return object types, so don't
        // include them here.
        if matches!(
            callee.name,
            id_for_built_in!("Object") | id_for_built_in!("Array") | id_for_built_in!("Error")
        ) {
            return true;
        }

        if callee.name == id_for_built_in!("RegExp") {
            // Fold "new RegExp()" to "RegExp()" if there are no args, or if the
            // first argument is a string. Otherwise the first argument could be
            // a regex, in which case "RegExp(regex)" returns the argument as-is
            // and is not equivalent to "new RegExp(regex)".
            // See https://262.ecma-international.org/#sec-regexp-pattern-flags
            match &new_expr.args {
                Some(args) => {
                    if let Some(first_arg) = args.first() {
                        if let ExprOrSpread::Expr(first_arg) = first_arg {
                            if matches!(first_arg.as_ref(), Expr::Lit(Lit::Str(_))) {
                                // First arg is string.
                                return true;
                            }
                        }
                    } else {
                        // Parentheses but no args.
                        return true;
                    }
                }
                None => {
                    // No args or even parentheses.
                    return true;
                }
            }
        }
    }

    false
}

#[derive(PartialEq)]
enum FoldArrayAction {
    NotSafeToFold,
    SafeToFoldWithArgs,
    SafeToFoldWithoutArgs,
}

/**
 * Checks if it is safe to fold Array() constructor into []. It can be
 * obviously done, if the initial constructor has either no arguments or
 * at least two. The remaining case may be unsafe since Array(number)
 * actually reserves memory for an empty array which contains number elements.
 */
fn isSafeToFoldArrayConstructor(args: &[ExprOrSpread]) -> FoldArrayAction {
    let Some(first_arg) = args.first() else {
        return FoldArrayAction::SafeToFoldWithoutArgs;
    };

    if args.len() >= 2 {
        return FoldArrayAction::SafeToFoldWithArgs;
    }

    let ExprOrSpread::Expr(first_arg) = first_arg else {
        return FoldArrayAction::NotSafeToFold;
    };

    match first_arg.as_ref() {
        Expr::Lit(Lit::Str(_)) => {
            // "Array('a')" --> "['a']"
            FoldArrayAction::SafeToFoldWithArgs
        }
        Expr::Lit(Lit::Num(first_arg)) => {
            // "Array(0)" --> "[]"
            if first_arg.value == 0.0 {
                FoldArrayAction::SafeToFoldWithoutArgs
            } else {
                FoldArrayAction::NotSafeToFold
            }
        }
        Expr::Array(_) => {
            // "Array([args])" --> "[[args]]"
            FoldArrayAction::SafeToFoldWithArgs
        }
        _ => FoldArrayAction::NotSafeToFold,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolve;

    #[test]
    fn testFoldRegExpConstructor() {
        // Cannot fold all the way to a literal because there are too few arguments.
        test_transform_late("x = new RegExp", "x = RegExp()");
        // Could fold all the way to /foobar/, but the complexity of that optimization wasn't worth the
        // code size improvements, mainly because there are a lot special cases to check.
        test_transform_late("x = new RegExp(\"foobar\")", "x = RegExp(\"foobar\")");
    }

    #[test]
    fn testFoldLiteralObjectConstructors() {
        // Can fold when normalized
        test_transform_late("x = new Object", "x = ({})");
        test_transform_late("x = new Object()", "x = ({})");
        test_transform_late("x = Object()", "x = ({})");

        // Cannot fold, the constructor being used is actually a local function
        test_same_late("x = (function f(){function Object(){this.x=4};return new Object();})();");
    }

    #[test]
    fn testFoldLiteralArrayConstructors() {
        // No arguments - can fold when normalized
        test_transform_late("x = new Array", "x = []");
        test_transform_late("x = new Array()", "x = []");
        test_transform_late("x = Array()", "x = []");
        test_same_late("x = Array?.()"); // Mustn't fold optional chains

        // One argument - can be fold when normalized
        test_transform_late("x = new Array(0)", "x = []");
        test_transform_late("x = Array(0)", "x = []");
        test_transform_late("x = new Array(\"a\")", "x = [\"a\"]");
        test_transform_late("x = Array(\"a\")", "x = [\"a\"]");

        // One argument - cannot be fold when normalized
        test_transform_late("x = new Array(7)", "x = Array(7)");
        test_same_late("x = Array(7)");
        test_transform_late("x = new Array(y)", "x = Array(y)");
        test_same_late("x = Array(y)");
        test_transform_late("x = new Array(foo())", "x = Array(foo())");
        test_same_late("x = Array(foo())");

        // More than one argument - can be fold when normalized
        test_transform_late("x = new Array(1, 2, 3, 4)", "x = [1, 2, 3, 4]");
        test_transform_late("x = Array(1, 2, 3, 4)", "x = [1, 2, 3, 4]");
        test_transform_late(
            "x = new Array('a', 1, 2, 'bc', 3, {}, 'abc')",
            "x = ['a', 1, 2, 'bc', 3, {}, 'abc']",
        );
        test_transform_late(
            "x = Array('a', 1, 2, 'bc', 3, {}, 'abc')",
            "x = ['a', 1, 2, 'bc', 3, {}, 'abc']",
        );
        test_transform_late(
            "x = new Array(Array(1, '2', 3, '4'))",
            "x = [[1, '2', 3, '4']]",
        );
        test_transform_late("x = Array(Array(1, '2', 3, '4'))", "x = [[1, '2', 3, '4']]");
        test_transform_late(
            "x = new Array(Object(), Array(\"abc\", Object(), Array(Array())))",
            "x = [{}, [\"abc\", {}, [[]]]]",
        );
        test_transform_late(
            "x = new Array(Object(), Array(\"abc\", Object(), Array(Array())))",
            "x = [{}, [\"abc\", {}, [[]]]]",
        );
    }

    #[test]
    fn testFoldStandardConstructors() {
        test_same_late("new Foo('a')");
        test_same_late("var x = new goog.Foo(1)");
        test_same_late("var x = new String(1)");
        test_same_late("var x = new Number(1)");
        test_same_late("var x = new Boolean(1)");

        test_transform_late("var x = new Object('a')", "var x = Object('a')");
        test_transform_late("var x = new RegExp('')", "var x = RegExp('')");
        test_transform_late("var x = new Error('20')", "var x = Error(\"20\")");
        test_transform_late("var x = new Array(20)", "var x = Array(20)");
    }

    #[test]
    fn testFoldTrueFalse() {
        test_transform_late("x = true", "x = !0");
        test_transform_late("x = false", "x = !1");
    }

    #[test]
    fn testFoldTrueFalseComparison() {
        test_transform_late("x == true", "x == 1");
        test_transform_late("x == false", "x == 0");
        test_transform_late("x != true", "x != 1");
        test_transform_late("x < true", "x < 1");
        test_transform_late("x <= true", "x <= 1");
        test_transform_late("x > true", "x > 1");
        test_transform_late("x >= true", "x >= 1");
    }

    #[test]
    fn testFoldSubtractionAssignment() {
        test_transform_late("x -= 1", "--x");
        test_transform_late("x -= -1", "++x");
    }

    #[test]
    fn testFoldReturnResult() {
        test_same_late("function f(){return !1;}");
        test_same_late("function f(){return null;}");
        test_transform_late("function f(){return void 0;}", "function f(){return}");
        test_same_late("function f(){return void foo();}");
        test_transform_late("function f(){return undefined;}", "function f(){return}");
        test_transform_late(
            "function f(){if(a()){return undefined;}}",
            "function f(){if(a()){return}}",
        );
    }

    #[test]
    fn testUndefined() {
        test_transform_late("var x = undefined", "var x=void 0");
        test_same_late(
            "
var undefined = 1;
function f() {var undefined=2;var x = undefined;}",
        );
        test_same_late("function f(undefined) {}");
        test_same_late("try {} catch(undefined) {}");
        test_same_late("for (undefined in {}) {}");
        test_same_late("undefined++;");
        test_transform_late(
            "undefined = undefined + undefined;",
            "undefined = void 0 + void 0;",
        );
    }

    #[test]
    fn testSplitCommaExpressions() {
        // Don't try to split in expressions.
        test_same("while (foo(), !0) boo()");
        test_same("var a = (foo(), !0);");
        test_same("a = (foo(), !0);");

        // Don't try to split COMMA under LABELs.
        test_transform("a:{ a(),b() }", "a:{ a(); b(); }");
        test_transform("1, 2, 3, 4", "1; 2; 3; 4");
        test_transform("x = 1, 2, 3", "x = 1; 2; 3");
        test_same("x = (1, 2, 3)");
        test_transform("1, (2, 3), 4", "1; 2; 3; 4");
        test_transform("(x=2), foo()", "x=2; foo()");
        test_transform("foo(), boo();", "foo(); boo()");
        test_transform("(a(), b()), (c(), d());", "a(); b(); c(); d()");
        test_transform("a(); b(); (c(), d());", "a(); b(); c(); d();");
        test_transform("foo(), true", "foo();true");
        test_same("foo();true");
        test_transform("function x(){foo(), !0}", "function x(){foo(); !0}");
        test_same("function x(){foo(); !0}");
    }

    #[test]
    fn testComma1() {
        test_transform("1, 2", "1; 2");

        test_same_late("1, 2");
    }

    #[test]
    fn testComma2() {
        test_transform("1, a()", "1; a()");
        test_transform("1, a?.()", "1; a?.()");

        test_same_late("1, a()");
        test_same_late("1, a?.()");
    }

    #[test]
    fn testComma3() {
        test_transform("1, a(), b()", "1; a(); b()");
        test_transform("1, a?.(), b?.()", "1; a?.(); b?.()");

        test_same_late("1, a(), b()");
        test_same_late("1, a?.(), b?.()");
    }

    #[test]
    fn testComma4() {
        test_transform("a(), b()", "a();b()");
        test_transform("a?.(), b?.()", "a?.();b?.()");

        test_same_late("a(), b()");
        test_same_late("a?.(), b?.()");
    }

    #[test]
    fn testComma5() {
        test_transform("a(), b(), 1", "a(); b(); 1");
        test_transform("a?.(), b?.(), 1", "a?.(); b?.(); 1");

        test_same_late("a(), b(), 1");
        test_same_late("a?.(), b?.(), 1");
    }

    #[test]
    fn testStringArraySplitting() {
        test_same_late("var x=['1','2','3','4']");
        test_same_late("var x=['1','2','3','4','5']");
        test_transform_late(
            "var x=['1','2','3','4','5','6']",
            "var x='123456'.split('')",
        );
        test_transform_late(
            "var x=['1','2','3','4','5','00']",
            "var x='1 2 3 4 5 00'.split(' ')",
        );
        test_transform_late(
            "var x=['1','2','3','4','5','6','7']",
            "var x='1234567'.split('')",
        );
        test_transform_late(
            "var x=['1','2','3','4','5','6','00']",
            "var x='1 2 3 4 5 6 00'.split(' ')",
        );
        test_transform_late(
            "var x=[' ,',',',',',',',',',',']",
            "var x=' ,;,;,;,;,;,'.split(';')",
        );
        test_transform_late(
            "var x=[',,',' ',',',',',',',',']",
            "var x=',,; ;,;,;,;,'.split(';')",
        );
        test_transform_late(
            "var x=['a,',' ',',',',',',',',']",
            "var x='a,; ;,;,;,;,'.split(';')",
        );

        // all possible delimiters used, leave it alone
        test_same_late("var x=[',', ' ', ';', '{', '}']");
    }

    #[test]
    fn testTemplateStringToString() {
        test_transform_late("`abcde`", "\"abcde\"");
        test_transform_late("`ab cd ef`", "\"ab cd ef\"");
        test_same_late("`hello ${name}`");
        test_same_late("tag `hello ${name}`");
        test_same_late("tag `hello`");
        test_transform_late("`hello ${'foo'}`", "\"hello foo\"");
        test_transform_late("`${2} bananas`", "\"2 bananas\"");
        test_transform_late("`This is ${true}`", "\"This is true\"");
    }

    #[test]
    fn testSimpleFunctionCall1() {
        test_transform_late("var a = String(23)", "var a = '' + 23");
        // Don't fold the existence check to preserve behaviour
        test_same_late("var a = String?.(23)");

        test_transform_late("var a = String('hello')", "var a = '' + 'hello'");
        // Don't fold the existence check to preserve behaviour
        test_same_late("var a = String?.('hello')");

        test_same_late("var a = String('hello', bar());");
        test_same_late("var a = String({valueOf: function() { return 1; }});");
    }

    #[test]
    fn testSimpleFunctionCall2() {
        test_transform_late("var a = Boolean(true)", "var a = !0");
        // Don't fold the existence check to preserve behaviour
        test_transform_late("var a = Boolean?.(true)", "var a = Boolean?.(!0)");

        test_transform_late("var a = Boolean(false)", "var a = !1");
        // Don't fold the existence check to preserve behaviour
        test_transform_late("var a = Boolean?.(false)", "var a = Boolean?.(!1)");

        test_transform_late("var a = Boolean(1)", "var a = !!1");
        // Don't fold the existence check to preserve behaviour
        test_same_late("var a = Boolean?.(1)");

        test_transform_late("var a = Boolean(x)", "var a = !!x");
        // Don't fold the existence check to preserve behaviour
        test_same_late("var a = Boolean?.(x)");

        test_transform_late("var a = Boolean({})", "var a = !!{}");
        // Don't fold the existence check to preserve behaviour
        test_same_late("var a = Boolean?.({})");

        test_same_late("var a = Boolean()");
        test_same_late("var a = Boolean(!0, !1);");
    }

    #[test]
    fn testRotateAssociativeOperators() {
        // Multiplication is not associative because it can include floating point numbers e.g.
        // 1e-300 * 1e300 * 1e9 does not equal 1e-300 * (1e300 * 1e9).
        test_transform_late(
            "a || (b || c); a * (b * c); a | (b | c)",
            "a || b || c; b * c * a; a | b | c",
        );
        test_same_late("a % (b % c); a / (b / c); a - (b - c);");
        test_same_late("(a / b) & (c % d)");
        test_same_late("(c = 5) & (c % d)");
        test_same_late("(a + b) * c * (d % e)");
        test_transform_late("(a + b) * (c % d)", "c % d * (a + b)");
    }

    #[test]
    fn testRotateCommutativeOperators() {
        test_transform_late("a * (b % c);", "b % c * a");
        test_same_late("a * b * (c / d)");
        test_same_late("!a * c * (d % e)");
    }

    #[test]
    fn nullishCoalesce() {
        test_transform_late("a ?? (b ?? c);", "(a ?? b) ?? c");
    }

    #[test]
    fn testNoRotateInfiniteLoop() {
        test_transform_late("1/x || (y/1 ||(1/z))", "1/x || (y/1) || (1/z)");
        test_same_late("1/x || (y/1) || (1/z)");
    }

    fn test_transform_late(input: &str, expected: &str) {
        test_transform_inner(input, expected, true);
    }
    fn test_same_late(input: &str) {
        test_same_inner(input, true);
    }

    fn test_transform(input: &str, expected: &str) {
        test_transform_inner(input, expected, false);
    }
    fn test_same(input: &str) {
        test_same_inner(input, false);
    }

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
    fn test_same_inner(input: &str, late: bool) {
        test_transform_inner(input, input, late);
    }
}
