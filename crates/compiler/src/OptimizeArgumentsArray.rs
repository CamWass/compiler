use ast::{BlockStmt, NameId, NodeId, id_for_built_in};
use common::DUMMY_SP;
use rustc_hash::FxHashMap;
use visit::{Visit, VisitMut, VisitMutWith, VisitWith};

/// Optimizes accesses to the `arguments` array by replacing them with
/// references to parameters, synthesising missing parameters if possible.
///
/// Example:
/// ```js
/// function() { alert(arguments[0] + arguments[1]) }
/// ```
/// to:
/// ```js
/// function(a, b) { alert(a, b) }
/// ```
pub fn process(ast: &mut ast::Program, program_data: &mut ast::TransformerProgramData) {
    let mut analyser = Analyser {
        cur_fn: None,
        highest_indices: FxHashMap::default(),
    };

    ast.visit_with(&mut analyser);

    if !analyser.highest_indices.is_empty() {
        let mut rewriter = ReWriter {
            program_data,
            highest_indices: analyser.highest_indices,
            current_rewrite_map: None,
        };

        ast.visit_mut_with(&mut rewriter);
    }
}

/// Creates a new param from the provided `NameId`.
fn from_id(id: NameId, program_data: &mut ast::TransformerProgramData) -> ast::Param {
    ast::Param {
        node_id: program_data.new_id(DUMMY_SP),
        pat: ast::Pat::Ident(ast::BindingIdent {
            id: ast::Ident {
                node_id: program_data.new_id(DUMMY_SP),
                name: id,
            },
        }),
    }
}

/// Generates a map from argument indices to parameter names.
///
/// The map is sparse in the case that there is an anonymous param, such as a
/// destructuring param. There may also be fewer returned names than required by
/// `highest_observed_index` if there is a rest param, since no additional
/// params may be synthesized.
///
/// `highest_observed_index` - The highest `arguments` index used in the
/// function.
fn assemble_param_names(
    params: &mut Vec<ast::Param>,
    highest_observed_index: u32,
    program_data: &mut ast::TransformerProgramData,
) -> Vec<Option<NameId>> {
    let mut map = Vec::new();
    let mut index: u32 = 0;

    // Collect all existing param names first...
    for param in params.iter() {
        match &param.pat {
            ast::Pat::Ident(n) => {
                map.push(Some(n.id.name));
            }
            // Array and object patterns have no names to substitute into the
            // body.
            ast::Pat::Array(_) | ast::Pat::Object(_) => {
                map.push(None);
            }
            // `arguments` doesn't consider default values. It holds exactly the
            // provided args.
            ast::Pat::Assign(_) => {
                map.push(None);
            }
            // Can't add params after a rest param.
            ast::Pat::Rest(_) => return map,
            ast::Pat::Invalid(_) | ast::Pat::Expr(_) => unreachable!(),
        }

        index += 1;
    }
    // ... then synthesize any additional param names.
    while index < highest_observed_index + 1 {
        let new_name = program_data.new_resolved_name(format!("p{index}").into());
        map.push(Some(new_name));
        params.push(from_id(new_name, program_data));
        index += 1;
    }

    map
}

#[derive(Default, Clone, Copy)]
struct FnInfo {
    invalidated: bool,
    highest_index: Option<u32>,
}

struct Analyser {
    cur_fn: Option<FnInfo>,
    highest_indices: FxHashMap<NodeId, u32>,
}

impl Analyser {
    fn invalidate(&mut self, _reason: &'static str) {
        if let Some(cur_fn) = &mut self.cur_fn {
            cur_fn.invalidated = true;
        }

        const PRINT: bool = false && cfg!(debug_assertions);

        if PRINT {
            println!("Invalidating because: {_reason}");
        }
    }

    /// Returns true if the member expr was a valid arguments access.
    fn handle_member_expr(&mut self, node: &ast::MemberExpr) -> bool {
        // Bail on anything but argument[c] access where c is a constant.

        if let Some(cur_fn) = &mut self.cur_fn {
            if let ast::ExprOrSuper::Expr(obj) = &node.obj {
                if let ast::Expr::Ident(obj) = obj.as_ref() {
                    if obj.name == id_for_built_in!("arguments") {
                        if node.computed {
                            if let ast::Expr::Lit(ast::Lit::Num(n)) = node.prop.as_ref() {
                                // Note: The index will always be positive because
                                // negative indices are represented as a unary op.

                                if n.value.fract() != 0.0 {
                                    // We want to bail out if someone tries to
                                    // access arguments[0.5] for example
                                    self.invalidate("Non integer key");
                                    return false;
                                }

                                let idx = n.value.round() as i64 as u32;

                                // Replace the highest index if we see an access
                                // that has a higher index than all the one we saw
                                // before.
                                if let Some(highest_index) = cur_fn.highest_index {
                                    if idx > highest_index {
                                        cur_fn.highest_index = Some(idx);
                                    }
                                } else {
                                    cur_fn.highest_index = Some(idx);
                                }

                                // Valid; The above conditions verify that the node
                                // is composed of only leaf nodes, so no need to
                                // visit children.
                                return true;
                            } else {
                                // We have something like arguments[x] where x is
                                // not a constant. That means at least one of the
                                // access is not known.
                                self.invalidate("Non numeric literal key");
                            }
                        } else {
                            self.invalidate("Non-computed access to 'arguments' object");
                            return false;
                        }
                    }
                }
            }
        }

        node.visit_children_with(self);
        false
    }
}

impl Visit<'_> for Analyser {
    fn visit_function(&mut self, node: &ast::Function) {
        node.params.visit_with(self);
        let prev = self.cur_fn;
        self.cur_fn = Some(FnInfo::default());
        node.body.visit_children_with(self);
        let cur_fn = self.cur_fn.unwrap();
        if !cur_fn.invalidated
            && let Some(highest_index) = cur_fn.highest_index
        {
            self.highest_indices.insert(node.node_id, highest_index);
        }
        self.cur_fn = prev;
    }
    fn visit_getter_prop(&mut self, node: &ast::GetterProp) {
        node.key.visit_with(self);
        let prev = self.cur_fn;
        self.cur_fn = Some(FnInfo::default());
        node.body.visit_children_with(self);
        self.cur_fn = prev;
    }
    fn visit_setter_prop(&mut self, node: &ast::SetterProp) {
        node.param.visit_with(self);
        node.key.visit_with(self);
        let prev = self.cur_fn;
        self.cur_fn = Some(FnInfo::default());
        node.body.visit_children_with(self);
        let cur_fn = self.cur_fn.unwrap();
        if !cur_fn.invalidated
            && let Some(highest_index) = cur_fn.highest_index
        {
            self.highest_indices.insert(node.node_id, highest_index);
        }
        self.cur_fn = prev;
    }
    fn visit_constructor(&mut self, node: &ast::Constructor) {
        node.params.visit_with(self);
        let prev = self.cur_fn;
        self.cur_fn = Some(FnInfo::default());
        node.body.visit_children_with(self);
        let cur_fn = self.cur_fn.unwrap();
        if !cur_fn.invalidated
            && let Some(highest_index) = cur_fn.highest_index
        {
            self.highest_indices.insert(node.node_id, highest_index);
        }
        self.cur_fn = prev;
    }

    fn visit_member_expr(&mut self, node: &ast::MemberExpr) {
        self.handle_member_expr(node);
    }

    fn visit_call_expr(&mut self, node: &ast::CallExpr) {
        if let ast::ExprOrSuper::Expr(callee) = &node.callee {
            if let ast::Expr::Member(callee) = callee.as_ref() {
                let valid_access = self.handle_member_expr(callee);
                // An otherwise valid access is invalid if used as the callee of
                // a call expr. When we have argument[0](), replacing it with
                // a() is semantically different if argument[0] is a function
                // call that refers to 'this'
                if valid_access {
                    self.invalidate("Valid access, used as callee in call expr, is invalid");
                }
            }
        }

        node.visit_children_with(self);
    }

    fn visit_ident(&mut self, node: &ast::Ident) {
        if node.name == id_for_built_in!("arguments") {
            self.invalidate("Usage of 'arguments' outside of valid member expr is invalid");
        }
    }
}

struct ReWriter<'a> {
    program_data: &'a mut ast::TransformerProgramData,
    highest_indices: FxHashMap<NodeId, u32>,
    current_rewrite_map: Option<Vec<Option<NameId>>>,
}

impl ReWriter<'_> {
    fn handle_class_method(&mut self, func: &mut ast::Function, kind: ast::MethodKind) {
        match kind {
            ast::MethodKind::Method => {
                self.handle_function(func.node_id, &mut func.params, &mut func.body);
            }
            ast::MethodKind::Getter => {
                // It's never valid to add arguments to a getter, so we skip it
                // and only process nested functions.
                let old = self.current_rewrite_map.take();
                self.current_rewrite_map = None;
                func.body.visit_mut_with(self);
                self.current_rewrite_map = old;
            }
            ast::MethodKind::Setter => {
                assert!(func.params.len() == 1);
                let param = func.params.first().unwrap();
                self.handle_setter(func.node_id, &param.pat, &mut func.body);
            }
        }
    }

    fn handle_setter(&mut self, fn_node_id: NodeId, param_pat: &ast::Pat, body: &mut BlockStmt) {
        let old = self.current_rewrite_map.take();
        self.current_rewrite_map = if let ast::Pat::Ident(id) = param_pat {
            if self.highest_indices.get(&fn_node_id).is_none() {
                // Some 'arguments' accesses were invalidating; abort.
                None
            } else {
                Some(vec![Some(id.id.name)])
            }
        } else {
            // Non-ident params don't introduce names for us to bind arguments
            // accesses to, so we skip the setter and only process nested
            // functions.
            None
        };
        body.visit_mut_with(self);
        self.current_rewrite_map = old;
    }

    fn handle_function(
        &mut self,
        fn_node_id: NodeId,
        params: &mut Vec<ast::Param>,
        body: &mut BlockStmt,
    ) {
        params.visit_mut_with(self);
        let old = self.current_rewrite_map.take();
        self.current_rewrite_map =
            self.highest_indices
                .get(&fn_node_id)
                .map(|highest_observed_index| {
                    assemble_param_names(params, *highest_observed_index, self.program_data)
                });
        body.visit_mut_with(self);
        self.current_rewrite_map = old;
    }
}

impl VisitMut<'_> for ReWriter<'_> {
    fn visit_mut_function(&mut self, node: &mut ast::Function) {
        self.handle_function(node.node_id, &mut node.params, &mut node.body);
    }

    fn visit_mut_class_method(&mut self, node: &mut ast::ClassMethod) {
        node.key.visit_mut_with(self);
        self.handle_class_method(&mut node.function, node.kind);
    }

    fn visit_mut_private_method(&mut self, node: &mut ast::PrivateMethod) {
        self.handle_class_method(&mut node.function, node.kind);
    }

    fn visit_mut_setter_prop(&mut self, node: &mut ast::SetterProp) {
        node.key.visit_mut_with(self);
        node.param.visit_mut_with(self);
        self.handle_setter(node.node_id, &node.param.pat, &mut node.body);
    }

    fn visit_mut_getter_prop(&mut self, node: &mut ast::GetterProp) {
        node.key.visit_mut_with(self);
        let old = self.current_rewrite_map.take();
        self.current_rewrite_map = None;
        node.body.visit_mut_with(self);
        self.current_rewrite_map = old;
    }

    fn visit_mut_constructor(&mut self, node: &mut ast::Constructor) {
        self.handle_function(node.node_id, &mut node.params, &mut node.body);
    }

    fn visit_mut_expr(&mut self, node: &mut ast::Expr) {
        if let ast::Expr::Member(expr) = node {
            if let ast::ExprOrSuper::Expr(obj) = &expr.obj {
                if let ast::Expr::Ident(obj) = obj.as_ref() {
                    if obj.name == id_for_built_in!("arguments") {
                        debug_assert!(expr.computed);
                        // TODO: numeric string literal keys e.g. arguments["1"]
                        if let ast::Expr::Lit(ast::Lit::Num(n)) = expr.prop.as_ref() {
                            debug_assert!(n.value.fract() == 0.0);

                            let idx = n.value.round() as i64 as usize;

                            if let Some(rewrite_map) = &self.current_rewrite_map {
                                if let Some(Some(name)) = rewrite_map.get(idx) {
                                    let id = ast::Ident {
                                        node_id: self.program_data.new_id_from(expr.node_id),
                                        name: *name,
                                    };
                                    *node = ast::Expr::Ident(id);
                                    return;
                                }
                            }
                        } else {
                            unreachable!("checked by FnBodyVisitor");
                        }
                    }
                }
            }
        }
        node.visit_mut_children_with(self);
    }
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]
    use crate::resolver::resolve;

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |program, program_data| {
                resolve(program, program_data);

                super::process(program, program_data);
            },
            input,
            expected,
        );
    }
    fn test_same(input: &str) {
        test_transform(input, input);
    }

    #[test]
    fn testNegativeArgumentIndexInvalidatesFunction() {
        test_same(
            "
function f() {
    arguments[0];
    arguments[-1];
    arguments[1];
}",
        );
    }

    #[test]
    fn testVisitChildrenOfCallExpr() {
        // Had a bug where the children of a call expr weren't visited.
        test_transform(
            "
function f() {
    console.log(arguments[0]);
}",
            "
function f(p0) {
    console.log(p0);
}",
        );
    }

    // =================================================================
    // Object literals
    // =================================================================

    #[test]
    fn testObjectMethod() {
        test_transform(
            "
const a = {
    m() {
        arguments[0];
    }
}",
            "
const a = {
    m(p0) {
        p0;
    }
}",
        );
    }

    #[test]
    fn testObjectGetter() {
        // Getters cannot have any params; synthesising them would be an error.
        test_same(
            "
const a ={
    get p () {
        arguments[0];
        arguments[1];
    }
}",
        );
    }

    #[test]
    fn testFunctionNestedInObjectGetter() {
        test_transform(
            "
const a = {
    get p() {
        arguments[0];
        arguments[1];
        function a() {
            arguments[0];
            arguments[1];  
        }
    }
}",
            "
const a = {
    get p() {
        arguments[0];
        arguments[1];
        function a(p0, p1) {
            p0;
            p1;  
        }
    }
}",
        );
    }

    #[test]
    fn testObjectSetter() {
        // Setters can only have one param; synthesising any more would be an
        // error.
        test_transform(
            "
const a = {
    set p(a) {
        arguments[0];
        arguments[1];
    }
}",
            "
const a = {
    set p(a) {
        a;
        arguments[1];
    }
}",
        );
    }

    #[test]
    fn testFunctionNestedInObjectSetter() {
        test_transform(
            "
const a = {
    set p(k) {
        arguments[0];
        arguments[1];
        function a() {
            arguments[0];
            arguments[1];  
        }
    }
}",
            "
const a = {
    set p(k) {
        k;
        arguments[1];
        function a(p0, p1) {
            p0;
            p1;  
        }
    }
}",
        );
    }

    #[test]
    fn testNestedObjectGettersAndSetters() {
        test_transform(
            "
function f() {
    arguments[0];
    const a = {
        get p() {
            arguments[0];
            arguments[1];
        },
        set p(q) {
            arguments[0];
            arguments[1];
        }
    };
}",
            "
function f(p0) {
    p0;
    const a = {
        get p() {
            arguments[0];
            arguments[1];
        },
        set p(q) {
            q;
            arguments[1];
        }
    };
}",
        );
    }

    // =================================================================
    // Classes
    // =================================================================

    #[test]
    fn testClassMethod() {
        test_transform(
            "
class Foo {
    m() {
        arguments[0];
    }
}",
            "
class Foo {
    m(p0) {
        p0;
    }
}",
        );
        test_transform(
            "
class Foo {
#m() {
    arguments[0];
}
}",
            "
class Foo {
#m(p0) {
    p0;
}
}",
        );
    }

    #[test]
    fn testClassGetter() {
        // Getters cannot have any params; synthesising them would be an error.
        test_same(
            "
class Foo {
    get p() {
        arguments[0];
        arguments[1];
    }
}",
        );

        test_same(
            "
class Foo {
    get #p() {
        arguments[0];
        arguments[1];
    }
}",
        );
    }

    #[test]
    fn testFunctionNestedInClassGetter() {
        test_transform(
            "
class Foo {
    get p() {
        arguments[0];
        arguments[1];
        function a() {
            arguments[0];
            arguments[1];  
        }
    }
}",
            "
class Foo {
    get p() {
        arguments[0];
        arguments[1];
        function a(p0, p1) {
            p0;
            p1;  
        }
    }
}",
        );
    }

    #[test]
    fn testClassSetter() {
        // Setters can only have one param; synthesising any more would be an
        // error.
        test_transform(
            "
class Foo {
    set p(a) {
        arguments[0];
        arguments[1];
    }
}",
            "
class Foo {
    set p(a) {
        a;
        arguments[1];
    }
}",
        );

        test_transform(
            "
class Foo {
    set #p(a) {
        arguments[0];
        arguments[1];
    }
}",
            "
class Foo {
    set #p(a) {
        a;
        arguments[1];
    }
}",
        );
    }

    #[test]
    fn testFunctionNestedInClassSetter() {
        test_transform(
            "
class Foo {
    set p(k) {
        arguments[0];
        arguments[1];
        function a() {
            arguments[0];
            arguments[1];  
        }
    }
}",
            "
class Foo {
    set p(k) {
        k;
        arguments[1];
        function a(p0, p1) {
            p0;
            p1;  
        }
    }
}",
        );
    }

    #[test]
    fn testClassConstructor() {
        test_transform(
            "
class Foo {
    constructor() {
        arguments[0];
    }
}",
            "
class Foo {
    constructor(p0) {
        p0;
    }
}",
        );
    }

    #[test]
    fn testFunctionNestedInClassConstructor() {
        test_transform(
            "
class Foo {
    constructor(a) {
        arguments[0];
        function f() {
            arguments[0];
        }
    }
}",
            "
class Foo {
    constructor(a) {
        a;
        function f(p0) {
            p0;
        }
    }
}",
        );
    }

    #[test]
    fn testParameterProperties() {
        test_transform(
            "
class Foo {
    constructor(private a: string, private b?: string, private c = 'c') {
        arguments[0];
        arguments[1];
        arguments[2];
        arguments[3];
    }
}",
            "
class Foo {
    constructor(private a: string, private b?: string, private c = 'c', p3) {
        a;
        b;
        arguments[2];
        p3;
    }
}",
        );
    }

    #[test]
    fn testClassNestedInFunction() {
        test_transform(
            "
function f(a) {
    arguments[0];
    arguments[1];
    class Foo {
        constructor(b) {
            arguments[0];
            arguments[1];
        }
        m(c) {
            arguments[0];
            arguments[1];    
        }
        get p() {
            arguments[0];
            arguments[1];    
        }
        set p(d) {
            arguments[0];
            arguments[1];    
        }
    }
}",
            "
function f(a, p1) {
    a;
    p1;
    class Foo {
        constructor(b, p1) {
            b;
            p1;
        }
        m(c, p1) {
            c;
            p1;    
        }
        get p() {
            arguments[0];
            arguments[1];    
        }
        set p(d) {
            d;
            arguments[1];    
        }
    }
}",
        );
    }

    // =================================================================
    // Tests from closure
    // =================================================================

    #[test]
    fn testSimple() {
        test_transform(
            "
            function foo()   { alert(arguments[0]); }",
            "
            function foo(p0) { alert(          p0); }",
        );
    }

    #[test]
    fn testNoVarArgs() {
        test_same("function f(a,b,c) { alert(a + b + c) }");

        test_transform(
            "
            function f(a,b,c) { alert(arguments[0]) }",
            "
            function f(a,b,c) { alert(           a) }",
        );
    }

    #[test]
    fn testMissingVarArgs() {
        test_same("function f() { alert(arguments[x]) }");
    }

    #[test]
    fn testArgumentRefOnNamedParameter() {
        test_transform(
            "
            function f(a,b) { alert(arguments[0]) }",
            "
            function f(a,b) { alert(a) }",
        );
    }

    #[test]
    fn testTwoVarArgs() {
        test_transform(
            "
            function foo(a)         { alert(arguments[1] + arguments[2]); }",
            "
            function foo(a, p1, p2) { alert(          p1 +           p2); }",
        );
    }

    #[test]
    fn testTwoFourArgsTwoUsed() {
        test_transform(
            "
            function foo() { alert(arguments[0] + arguments[3]); }",
            "
            function foo(p0, p1, p2, p3) { alert(p0 + p3); }",
        );
    }

    #[test]
    fn testOneRequired() {
        test_transform(
            "function foo(req0, var_args) { alert(req0 + arguments[1]); }",
            "function foo(req0, var_args) { alert(req0 + var_args); }",
        );
    }

    #[test]
    fn testTwoRequiredSixthVarArgReferenced() {
        test_transform(
            "function foo(r0, r1, var_args) {alert(r0 + r1 + arguments[5]);}",
            "function foo(r0, r1, var_args, p3, p4, p5) { alert(r0 + r1 + p5); }",
        );
    }

    #[test]
    fn testTwoRequiredOneOptionalFifthVarArgReferenced() {
        test_transform(
            "function foo(r0, r1, opt_1) {alert(r0 + r1 + opt_1 + arguments[4]);}",
            "function foo(r0, r1, opt_1, p3, p4) {alert(r0 + r1 + opt_1 + p4); }",
        );
    }

    #[test]
    fn testTwoRequiredTwoOptionalSixthVarArgReferenced() {
        test_transform(
            "function foo(r0, r1, opt_1, opt_2) {alert(r0 + r1 + opt_1 + opt_2 + arguments[5]);}",
            "function foo(r0, r1, opt_1, opt_2, p4, p5) {alert(r0 + r1 + opt_1 + opt_2 + p5); }",
        );
    }

    #[test]
    fn testInnerFunctions() {
        test_transform(
            "
            function f() { function b(  ) { arguments[0]  }}",
            "
            function f() { function b(p0) {            p0 }}",
        );

        test_transform(
            "
            function f(  ) { function b() { }  arguments[0] }",
            "
            function f(p0) { function b() { }            p0 }",
        );

        // TODO: assert the left two 'p0's are distinct from the right two.
        test_transform(
            "
            function f( )  { arguments[0]; function b(  ) { arguments[0] }}",
            "
            function f(p0) {           p0; function b(p0) {           p0 }}",
        );
    }

    #[test]
    fn testInnerFunctionsWithNamedArgumentInInnerFunction() {
        test_transform(
            "
            function f() { function b(x   ) { arguments[1] }}",
            "
            function f() { function b(x,p1) {           p1 }}",
        );

        test_transform(
            "
            function f(  ) { function b(x) { }  arguments[0] }",
            "
            function f(p0) { function b(x) { }            p0 }",
        );

        test_transform(
            "
            function f( )  { arguments[0]; function b(x   ) { arguments[1] }}",
            "
            function f(p0) {           p0; function b(x,p1) {           p1 }}",
        );
    }

    #[test]
    fn testInnerFunctionsWithNamedArgumentInOutterFunction() {
        test_transform(
            "
            function f(x) { function b(  ) { arguments[0] }}",
            "
            function f(x) { function b(p0) {           p0 }}",
        );

        test_transform(
            "
            function f(x   ) { function b() { }  arguments[1] }",
            "
            function f(x,p1) { function b() { }            p1 }",
        );

        test_transform(
            "
            function f(x   ) { arguments[1]; function b(  ) { arguments[0] }}",
            "
            function f(x,p1) {           p1; function b(p0) {           p0 }}",
        );
    }

    #[test]
    fn testInnerFunctionsWithNamedArgumentInInnerAndOutterFunction() {
        test_transform(
            "
            function f(x) { function b(x   ) { arguments[1] }}",
            "
            function f(x) { function b(x,p1) {           p1 }}",
        );

        test_transform(
            "
            function f(x   ) { function b(x) { }  arguments[1] }",
            "
            function f(x,p1) { function b(x) { }            p1 }",
        );

        test_transform(
            "
            function f(x   ) { arguments[1]; function b(x   ) { arguments[1] }}",
            // TODO: assert the left two 'p1's are distinct from the right two.
            "
            function f(x,p1) {           p1; function b(x,p1) {           p1 }}",
        );
    }

    #[test]
    fn testInnerFunctionsAfterArguments() {
        test_transform(
            "
            function f(  ) { arguments[0]; function b() { function c() { }} }",
            "
            function f(p0) {           p0; function b() { function c() { }} }",
        );
    }

    #[test]
    fn testNoOptimizationWhenGetProp() {
        test_same("function f() { arguments[0]; arguments.size }");
    }

    #[test]
    fn testNoOptimizationWhenIndexIsNotNumberConstant() {
        test_same("function f() { arguments[0]; arguments['callee'].length}");
        test_same("function f() { arguments[0]; arguments.callee.length}");
        test_same("function f() { arguments[0]; var x = 'callee'; arguments[x].length}");
    }

    #[test]
    fn testDecimalArgumentIndex() {
        test_same("function f() { arguments[0.5]; }");
    }

    #[test]
    fn testNegativeArgumentIndex() {
        test_same("function badFunction() { arguments[-1]; }");
    }

    #[test]
    fn testArrowFunctions() {
        // simple
        test_transform(
            "
            function f()   { ( ) => { alert(arguments[0]); } }",
            "
            function f(p0) { ( ) => { alert(          p0); } }",
        );

        // no var args
        test_same("function f() { (a,b,c) => alert(a + b + c); }");

        test_transform(
            "
            function f()   { (a,b,c) => alert(arguments[0]); }",
            "
            function f(p0) { (a,b,c) => alert(          p0); }",
        );

        // two var args
        test_transform(
            "
            function f()         { (a) => alert(arguments[1] + arguments[2]); }",
            "
            function f(p0,p1,p2) { (a) => alert(          p1 +           p2); }",
        );

        // test with required params
        test_transform(
            "
            function f()       { (req0, var_args) => alert(req0 + arguments[1]); }",
            "
            function f(p0, p1) { (req0, var_args) => alert(req0 +           p1); }",
        );
    }

    #[test]
    fn testArrowFunctionIsInnerFunction() {
        test_transform(
            "
            function f()   { ( ) => { arguments[0] } }",
            "
            function f(p0) { ( ) => {           p0 } }",
        );

        // Arrow function after argument
        test_transform(
            "
            function f( )  { arguments[0]; ( ) => { arguments[0] } }",
            "
            function f(p0) {           p0; ( ) => {           p0 } }",
        );
    }

    #[test]
    fn testArrowFunctionInInnerFunctionUsingArguments() {
        // See https://github.com/google/closure-compiler/issues/3195
        test_transform(
            "
function f() {
    function g() {
    arguments[0].map((v) => v.error);
    };
}",
            "
function f() {
    function g(p0) {
    p0.map((v) => v.error);
    };
}",
        );
    }

    #[test]
    fn testArgumentsReferenceInFunctionAndArrow() {
        test_transform(
            "
function f() {
    arguments[0];
    return () => arguments[0];
}",
            "
function f(p0) {
    p0;
    return () => p0;
}",
        );
    }

    #[test]
    fn testArrowFunctionDeclaration() {
        test_transform(
            "
            function f()   { var f = ( ) => { alert(arguments[0]); } }",
            "
            function f(p0) { var f = ( ) => { alert(          p0); } }",
        );
    }

    #[test]
    fn testNestedFunctions() {
        //Arrow inside arrow inside vanilla function

        test_transform(
            "
            function f()   { () => { () => { arguments[0]; } } }",
            "
            function f(p0) { () => { () => {           p0; } } }",
        );

        test_transform(
            "
            function f()   { () => { alert(arguments[0]); () => { arguments[0]; } } }",
            "
            function f(p0) { () => { alert(          p0); () => {           p0; } } }",
        );

        test_transform(
            "
            function f()       { () => { alert(arguments[0]); () => { arguments[1]; } } }",
            "
            function f(p0, p1) { () => { alert(          p0); () => {           p1; } } }",
        );
    }

    #[test]
    fn testNoOptimizationWhenArgumentIsUsedAsFunctionCall() {
        // Replacing the call would change `this`.
        test_same("function f() {arguments[0]()}");
    }

    #[test]
    fn testNoOptimizationWhenArgumentsReassigned() {
        // TODO: can we replace the accesses before the re-assignment?
        // Replacing the post-assignment `arguments[0]` with a named parameter
        // would be incorrect.
        test_same("function f() { arguments[0]; arguments = [3, 4, 5]; arguments[0]; }");
    }

    #[test]
    fn testUnusualArgumentsUsage() {
        test_same("function f(x) { x[arguments]; }");
    }

    #[test]
    fn testUseArguments_withDefaultValue() {
        // `arguments` doesn't consider default values. It holds exactly the
        // provided args.
        test_same("function f(x = 0) { arguments[0]; }");

        test_transform(
            "function f(x = 0) { arguments[1]; }",
            "function f(x = 0, p1) { p1; }",
        );
    }

    #[test]
    fn testUseArguments_withRestParam() {
        test_transform(
            "
            function f(x, ...rest) { arguments[0]; }",
            "
            function f(x, ...rest) { x; }",
        );

        // We could possibly do better here by referencing through `rest`
        // instead, but the additional complexity of tracking and validating the
        // rest parameter isn't worth it.
        test_same("function f(x, ...rest) { arguments[1]; }");
        // Don't synthesize params after a rest.
        test_same("function f(x, ...rest) { arguments[2]; }");
    }

    #[test]
    fn testUseArguments_withArrayDestructuringParam() {
        test_same("function f([x, y]) { arguments[0]; }");

        test_transform(
            "function f([x, y]) { arguments[1]; }",
            "function f([x, y], p1) { p1; }",
        );
    }

    #[test]
    fn testUseArguments_withObjectDestructuringParam() {
        test_transform(
            "function f({x: y}) { arguments[1]; }",
            "function f({x: y}, p1) { p1; }",
        );

        test_same("function f({x: y}) { arguments[0]; }");
    }

    #[test]
    fn testGlobalArgumentsReferences() {
        test_same("arguments;");
        test_same(
            "
if (typeof arguments != 'undefined') {
    console.log(arguments);
}",
        );
    }
}
