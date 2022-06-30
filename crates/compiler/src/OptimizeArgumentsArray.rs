use crate::Id;
use ast;
use ecma_visit::{noop_visit_type, Visit, VisitMut, VisitMutWith, VisitWith};
use global_common::{SyntaxContext, DUMMY_SP};
use std::collections::BTreeMap;
use std::iter::FromIterator;
use swc_atoms::{js_word, JsWord};

/// Returns a globally unique [SyntaxContext].
macro_rules! private_ctxt {
    () => {{
        ::global_common::SyntaxContext::empty().apply_mark(::global_common::Mark::new())
    }};
}

/// Creates a new [Ident][ast::Ident] from the provided [sym][swc_atoms::JsWord], [syntax_ctxt][global_common::SyntaxContext], and [NodeId][ast::NodeId].
macro_rules! ident {
    ($sym:expr, $syntax_ctxt:expr,  $id:expr) => {
        ast::Ident {
            node_id: $id,
            span: ::global_common::DUMMY_SP.with_ctxt($syntax_ctxt),
            sym: $sym,
            optional: false,
        }
    };
}

/// Optimizes accesses to the `arguments` array by replacing them with references to parameters,
/// synthesising missing parameters if possible.
///
/// Example:
/// ```js
/// function() { alert(arguments[0] + arguments[1]) }
/// ```
/// to:
/// ```js
/// function(a, b) { alert(a, b) }
/// ```
pub struct OptimizeArgumentsArray<'a> {
    node_id_gen: &'a mut ast::NodeIdGen,
    unresolved_ctxt: SyntaxContext,
}

impl<'a> OptimizeArgumentsArray<'a> {
    pub fn process(
        ast: &mut ast::Program,
        node_id_gen: &'a mut ast::NodeIdGen,
        unresolved_ctxt: SyntaxContext,
    ) {
        let mut visitor = Self {
            node_id_gen,
            unresolved_ctxt,
        };

        ast.visit_mut_with(&mut visitor);
    }
}

impl VisitMut for OptimizeArgumentsArray<'_> {
    fn visit_mut_function(&mut self, node: &mut ast::Function) {
        node.body.visit_mut_with(self);
        self.try_replace_arguments(node);
    }

    fn visit_mut_class_method(&mut self, node: &mut ast::ClassMethod) {
        self.handle_class_method(&mut node.function, node.kind);
    }

    fn visit_mut_private_method(&mut self, node: &mut ast::PrivateMethod) {
        self.handle_class_method(&mut node.function, node.kind);
    }

    fn visit_mut_setter_prop(&mut self, node: &mut ast::SetterProp) {
        self.handle_setter(&node.param.pat, &mut node.body);
        node.body.visit_mut_with(self);
    }

    fn visit_mut_constructor(&mut self, node: &mut ast::Constructor) {
        node.body.visit_mut_with(self);
        self.try_replace_arguments(node);
    }
}

impl OptimizeArgumentsArray<'_> {
    fn handle_class_method(&mut self, func: &mut ast::Function, kind: ast::MethodKind) {
        match kind {
            ast::MethodKind::Method => {
                self.try_replace_arguments(func);
            }
            ast::MethodKind::Getter => {
                // It's never valid to add arguments to a getter, so we skip it and
                // only process nested functions.
            }
            ast::MethodKind::Setter => {
                assert!(func.params.len() == 1);
                let param = func.params.first().unwrap();
                self.handle_setter(&param.pat, &mut func.body);
            }
        }
        func.body.visit_mut_with(self);
    }

    fn handle_setter(&mut self, param_pat: &ast::Pat, body: &mut Option<ast::BlockStmt>) {
        if let ast::Pat::Ident(id) = param_pat {
            let id = (id.id.sym.clone(), id.id.span.ctxt);
            self.try_replace_setter_argument(id, body);
        } else {
            // Non-ident params don't introduce names for us to bind arguments
            // accesses to, so we skip the setter and only process nested functions.
        }
    }

    /// Tries to optimize all of the `arguments` accesses in this function. Does not look at nested functions.
    fn try_replace_arguments<T>(&mut self, func: &mut T)
    where
        T: AsFn,
        <T as AsFn>::Param: FromId,
    {
        // The number of parameters that can be accessed without using `arguments`.
        let highest_index = if func.get_params().len() == 0 {
            None
        } else {
            Some(func.get_params().len() - 1)
        };

        // Determine the highest index that is used to make an access on `arguments`. By default, assume
        // that the value is the number of parameters to the function.
        let highest_index =
            FnBodyVisitor::get_highest_index(func.get_body(), highest_index, self.unresolved_ctxt);
        let highest_index = match highest_index {
            Some(highest_index) => highest_index,
            None => return,
        };

        let arg_names = func.assemble_param_names(highest_index + 1);
        self.append_new_params(&arg_names, func.get_params());
        FnBodyReWriter::change_body(
            func.get_body(),
            &arg_names,
            self.node_id_gen,
            self.unresolved_ctxt,
        );
    }

    /// Tries to optimize all of the `arguments` accesses in this setter. Does not look at nested functions.
    fn try_replace_setter_argument(&mut self, param: Id, setter_body: &mut Option<ast::BlockStmt>) {
        if FnBodyVisitor::get_highest_index(setter_body, Some(0), self.unresolved_ctxt).is_none() {
            // Some 'arguments' accesses were invalidating; abort.
            return;
        }

        let arg_names = BTreeMap::from_iter([(0, param)]);
        FnBodyReWriter::change_body(
            setter_body,
            &arg_names,
            self.node_id_gen,
            self.unresolved_ctxt,
        );
    }

    /// Appends new formal parameters to the provided list based on the given set of names.
    ///
    /// Example: function() -> function(r0, r1, r2)
    ///
    /// `arg_names` - maps param index to param name, if the param with that index has a name.
    /// `param_list` - the list of params to modify.
    fn append_new_params<P>(&mut self, arg_names: &BTreeMap<usize, Id>, param_list: &mut Vec<P>)
    where
        P: FromId,
    {
        let new_params = arg_names
            .range(param_list.len()..)
            .map(|(_, id)| P::from_id(id, self.node_id_gen));
        param_list.extend(new_params);
    }
}

trait FromId {
    /// Creates a new param from the provided `Id`.
    fn from_id(id: &Id, node_id_gen: &mut ast::NodeIdGen) -> Self;
}

impl FromId for ast::Param {
    fn from_id((sym, ctxt): &Id, node_id_gen: &mut ast::NodeIdGen) -> Self {
        ast::Param {
            node_id: node_id_gen.next(),
            span: DUMMY_SP,
            decorators: Default::default(),
            pat: ast::Pat::Ident(ast::BindingIdent {
                node_id: node_id_gen.next(),
                id: ident!(sym.clone(), *ctxt, node_id_gen.next()),
                type_ann: None,
            }),
        }
    }
}

impl FromId for ast::ParamOrTsParamProp {
    fn from_id(id: &Id, node_id_gen: &mut ast::NodeIdGen) -> Self {
        ast::ParamOrTsParamProp::Param(ast::Param::from_id(id, node_id_gen))
    }
}

trait AsFn {
    type Param;

    /// Generates a map from argument indices to parameter names.
    ///
    /// A map is used because the sequence may be sparse in the case that there is an
    /// anonymous param, such as a destructuring param. There may also be fewer returned names than
    /// `max_count` if there is a rest param, since no additional params may be synthesized.
    ///
    /// `max_count` - The maximum number of argument names in the returned map.
    fn assemble_param_names(&self, max_count: usize) -> BTreeMap<usize, Id>;

    fn get_params(&mut self) -> &mut Vec<Self::Param>;
    fn get_body(&mut self) -> &mut Option<ast::BlockStmt>;
}

impl AsFn for ast::Function {
    type Param = ast::Param;

    fn assemble_param_names(&self, max_count: usize) -> BTreeMap<usize, Id> {
        let mut map = BTreeMap::new();
        let mut index = 0;

        // Collect all existing param names first...
        for param in &self.params {
            match &param.pat {
                ast::Pat::Ident(n) => {
                    map.insert(index, (n.id.sym.clone(), n.id.span.ctxt));
                }
                // Array and object patterns have no names to substitute into the body.
                ast::Pat::Array(_) | ast::Pat::Object(_) => {}
                // `arguments` doesn't consider default values. It holds exactly the provided args.
                ast::Pat::Assign(_) => {}
                // Can't add params after a rest param.
                ast::Pat::Rest(_) => return map,
                ast::Pat::Invalid(_) | ast::Pat::Expr(_) => unreachable!(),
            }

            index += 1;
        }
        // ... then synthesize any additional param names.
        while index < max_count {
            map.insert(
                index,
                (JsWord::from(format!("p{}", index)), private_ctxt!()),
            );
            index += 1;
        }

        map
    }

    fn get_params(&mut self) -> &mut Vec<Self::Param> {
        &mut self.params
    }
    fn get_body(&mut self) -> &mut Option<ast::BlockStmt> {
        &mut self.body
    }
}

impl AsFn for ast::Constructor {
    type Param = ast::ParamOrTsParamProp;

    fn assemble_param_names(&self, max_count: usize) -> BTreeMap<usize, Id> {
        let mut map = BTreeMap::new();
        let mut index = 0;

        // Collect all existing param names first...
        for param in &self.params {
            match param {
                ast::ParamOrTsParamProp::TsParamProp(n) => match &n.param {
                    ast::TsParamPropParam::Ident(n) => {
                        map.insert(index, (n.id.sym.clone(), n.id.span.ctxt));
                    }
                    // `arguments` doesn't consider default values. It holds exactly the provided args.
                    ast::TsParamPropParam::Assign(_) => {}
                },
                ast::ParamOrTsParamProp::Param(n) => match &n.pat {
                    ast::Pat::Ident(n) => {
                        map.insert(index, (n.id.sym.clone(), n.id.span.ctxt));
                    }
                    // Array and object patterns have no names to substitute into the body.
                    ast::Pat::Array(_) | ast::Pat::Object(_) => {}
                    // `arguments` doesn't consider default values. It holds exactly the provided args.
                    ast::Pat::Assign(_) => {}
                    // Can't add params after a rest param.
                    ast::Pat::Rest(_) => return map,
                    ast::Pat::Invalid(_) | ast::Pat::Expr(_) => unreachable!(),
                },
            }

            index += 1;
        }
        // ... then synthesize any additional param names.
        while index < max_count {
            map.insert(
                index,
                (JsWord::from(format!("p{}", index)), private_ctxt!()),
            );
            index += 1;
        }

        map
    }

    fn get_params(&mut self) -> &mut Vec<Self::Param> {
        &mut self.params
    }
    fn get_body(&mut self) -> &mut Option<ast::BlockStmt> {
        &mut self.body
    }
}

pub struct FnBodyVisitor {
    unresolved_ctxt: SyntaxContext,
    invalidated: bool,
    highest_index: Option<usize>,
}

impl FnBodyVisitor {
    /// Iterate through all the references to `arguments` array in the function to determine the real
    /// highest_index. Returns `None` when we should not be replacing any arguments for this scope.
    ///
    /// `highest_index` - highest index that has been accessed from the `arguments` array.
    fn get_highest_index(
        func_body: &Option<ast::BlockStmt>,
        highest_index: Option<usize>,
        unresolved_ctxt: SyntaxContext,
    ) -> Option<usize> {
        let mut visitor = Self {
            unresolved_ctxt,
            invalidated: false,
            highest_index,
        };

        func_body.visit_with(&mut visitor);

        if visitor.invalidated {
            None
        } else {
            visitor.highest_index
        }
    }

    fn invalidate(&mut self, _reason: &'static str) {
        self.invalidated = true;

        const PRINT: bool = true && cfg!(debug_assertions);

        if PRINT {
            println!("FnBodyVisitor: invalidating because: {}", _reason);
        }
    }

    /// Returns true if the member expr was a valid arguments access.
    fn handle_member_expr(&mut self, node: &ast::MemberExpr) -> bool {
        // Bail on anything but argument[c] access where c is a constant.
        // TODO(closure): We might not need to bail out all the time, there might
        // be more cases that we can cover.

        if let ast::ExprOrSuper::Expr(obj) = &node.obj {
            if let ast::Expr::Ident(obj) = obj.as_ref() {
                if obj.sym == js_word!("arguments") && obj.span.ctxt == self.unresolved_ctxt {
                    if node.computed {
                        // TODO: numeric string literal keys e.g. arguments["1"]
                        if let ast::Expr::Lit(ast::Lit::Num(n)) = node.prop.as_ref() {
                            // Note: The index will always be positive because negative indices are
                            // represented as a unary op.

                            if n.value.fract() != 0.0 {
                                // We want to bail out if someone tries to access arguments[0.5] for example
                                self.invalidate("Non integer key");
                                return false;
                            }

                            let idx = n.value.round() as i64 as usize;

                            // Replace the highest index if we see an access that has a higher index
                            // than all the one we saw before.
                            if let Some(highest_index) = self.highest_index {
                                if idx > highest_index {
                                    self.highest_index = Some(idx);
                                }
                            } else {
                                self.highest_index = Some(idx);
                            }

                            // Valid; The above conditions varify that the node is composed of only leaf
                            // nodes, so no need to visit children.
                            return true;
                        } else {
                            // We have something like arguments[x] where x is not a constant. That
                            // means at least one of the access is not known.
                            self.invalidate("Non numeric literal key");
                            return false;
                        }
                    } else {
                        self.invalidate("Non-computed access to 'arguments' object");
                        return false;
                    }
                } else {
                    node.visit_children_with(self);
                    return false;
                }
            } else {
                node.visit_children_with(self);
                return false;
            }
        } else {
            node.visit_children_with(self);
            return false;
        }
    }
}

impl Visit for FnBodyVisitor {
    noop_visit_type!();

    // Don't visit nested functions.
    fn visit_function(&mut self, _: &ast::Function) {}
    fn visit_getter_prop(&mut self, _: &ast::GetterProp) {}
    fn visit_setter_prop(&mut self, _: &ast::SetterProp) {}
    fn visit_constructor(&mut self, _: &ast::Constructor) {}

    fn visit_member_expr(&mut self, node: &ast::MemberExpr) {
        if self.invalidated {
            return;
        }

        self.handle_member_expr(node);
    }

    fn visit_call_expr(&mut self, node: &ast::CallExpr) {
        if self.invalidated {
            return;
        }

        if let ast::ExprOrSuper::Expr(callee) = &node.callee {
            if let ast::Expr::Member(callee) = callee.as_ref() {
                let valid_access = self.handle_member_expr(callee);
                // An otherwise valid access is invalid if used as the callee of a call expr.
                // When we have argument[0](), replacing it with a() is semantically
                // different if argument[0] is a function call that refers to 'this'
                if valid_access {
                    // TODO(closure): We can consider using .call() if aliasing that
                    // argument allows shorter alias for other arguments.
                    self.invalidate("Valid access, used as callee in call expr, is invalid");
                } else {
                    node.visit_children_with(self);
                }
            } else {
                node.visit_children_with(self);
            }
        } else {
            node.visit_children_with(self);
        }
    }

    fn visit_ident(&mut self, node: &ast::Ident) {
        if self.invalidated {
            return;
        }
        if node.sym == js_word!("arguments") && node.span.ctxt == self.unresolved_ctxt {
            self.invalidate("Usage of 'arguments' outside of valid member expr is invalid");
        }
    }

    fn visit_stmt(&mut self, node: &ast::Stmt) {
        if !self.invalidated {
            node.visit_children_with(self);
        }
    }
}

struct FnBodyReWriter<'a> {
    unresolved_ctxt: SyntaxContext,
    arg_names: &'a BTreeMap<usize, Id>,
    node_id_gen: &'a mut ast::NodeIdGen,
}

impl<'a> FnBodyReWriter<'a> {
    /// Performs the replacement of `arguments[x]` -> `a` if `x` is known.
    ///
    /// `arg_names` - maps param index to param name, if the param with that index has a name.
    fn change_body(
        func_body: &mut Option<ast::BlockStmt>,
        arg_names: &'a BTreeMap<usize, Id>,
        node_id_gen: &'a mut ast::NodeIdGen,
        unresolved_ctxt: SyntaxContext,
    ) {
        let mut visitor = Self {
            unresolved_ctxt,
            arg_names,
            node_id_gen,
        };

        func_body.visit_mut_with(&mut visitor);
    }
}

impl VisitMut for FnBodyReWriter<'_> {
    // Don't visit nested functions.
    fn visit_mut_function(&mut self, _: &mut ast::Function) {}
    fn visit_mut_getter_prop(&mut self, _: &mut ast::GetterProp) {}
    fn visit_mut_setter_prop(&mut self, _: &mut ast::SetterProp) {}
    fn visit_mut_constructor(&mut self, _: &mut ast::Constructor) {}

    fn visit_mut_expr(&mut self, node: &mut ast::Expr) {
        if let ast::Expr::Member(expr) = node {
            if let ast::ExprOrSuper::Expr(obj) = &expr.obj {
                if let ast::Expr::Ident(obj) = obj.as_ref() {
                    if obj.sym == js_word!("arguments") && obj.span.ctxt == self.unresolved_ctxt {
                        debug_assert!(expr.computed);
                        // TODO: numeric string literal keys e.g. arguments["1"]
                        if let ast::Expr::Lit(ast::Lit::Num(n)) = expr.prop.as_ref() {
                            debug_assert!(n.value.fract() == 0.0);

                            let idx = n.value.round() as i64 as usize;

                            if let Some((sym, ctxt)) = self.arg_names.get(&idx) {
                                let id = ident!(sym.clone(), *ctxt, self.node_id_gen.next());
                                *node = ast::Expr::Ident(id);
                                return;
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
    use crate::resolver::resolver;
    use ast::*;
    use codegen::{text_writer::JsWriter, Emitter};
    use ecma_visit::{VisitMut, VisitMutWith};
    use global_common::{
        errors::Handler, sync::Lrc, FileName, Mark, SourceMap, Span, SyntaxContext, DUMMY_SP,
    };
    use parser::{Parser, Syntax};

    struct Tester<'a> {
        cm: Lrc<SourceMap>,
        handler: &'a Handler,
    }

    impl<'a> Tester<'a> {
        fn run<F>(op: F)
        where
            F: FnOnce(&mut Tester<'_>) -> Result<(), ()>,
        {
            let out = ::testing::run_test(false, |cm, handler| op(&mut Tester { cm, handler }));

            match out {
                Ok(()) => {}
                Err(stderr) => panic!("Stderr:\n{}", stderr),
            }
        }

        fn apply_transform<T>(&mut self, tr: T, name: &str, src: &str) -> Result<Program, ()>
        where
            T: FnOnce(Program) -> Program,
        {
            let fm = self
                .cm
                .new_source_file(FileName::Real(name.into()), src.into());

            let program = {
                let mut p = Parser::new(
                    Syntax::Typescript(Default::default()),
                    &fm,
                    Default::default(),
                );
                let res = p
                    .parse_program()
                    .map_err(|e| e.into_diagnostic(self.handler).emit());

                for e in p.take_errors() {
                    e.into_diagnostic(self.handler).emit()
                }

                res?
            };

            let mut program = tr(program);

            program.visit_mut_with(&mut DropSpan {
                preserve_ctxt: true,
            });
            program.visit_mut_with(&mut DropNodeId);

            Ok(program)
        }

        fn print(&mut self, program: &Program) -> String {
            let mut buf = vec![];
            {
                let mut emitter = Emitter {
                    cfg: Default::default(),
                    cm: self.cm.clone(),
                    wr: Box::new(JsWriter::new(self.cm.clone(), "\n", &mut buf, None)),
                    comments: None,
                };

                // println!("Emitting: {:?}", module);
                emitter.emit_program(program).unwrap();
            }

            let s = String::from_utf8_lossy(&buf);
            s.to_string()
        }
    }

    fn test_transform(input: &str, expected: &str) {
        Tester::run(|tester| {
            let expected = tester.apply_transform(|m| m, "output.js", expected)?;

            println!("----- Actual -----");

            let mut actual = tester.apply_transform(
                |mut program| {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark, true));

                    let unresolved_ctxt = SyntaxContext::empty().apply_mark(unresolved_mark);

                    super::OptimizeArgumentsArray::process(
                        &mut program,
                        &mut Default::default(),
                        unresolved_ctxt,
                    );
                    program
                },
                "input.js",
                input,
            )?;

            actual.visit_mut_with(&mut DropSpan {
                preserve_ctxt: false,
            });
            actual.visit_mut_with(&mut DropNodeId);

            if actual == expected {
                return Ok(());
            }

            let (actual_src, expected_src) = (tester.print(&actual), tester.print(&expected));

            if actual_src == expected_src {
                return Ok(());
            }

            println!(">>>>> Orig <<<<<\n{}", input);
            println!(">>>>> Code <<<<<\n{}", actual_src);
            if actual_src != expected_src {
                panic!(
                    r#"assertion failed: `(left == right)`
                {}"#,
                    ::testing::diff(&actual_src, &expected_src),
                );
            }

            Err(())
        });
    }

    fn test_same(input: &str) {
        test_transform(input, input);
    }

    struct DropSpan {
        preserve_ctxt: bool,
    }
    impl VisitMut for DropSpan {
        fn visit_mut_span(&mut self, span: &mut Span) {
            *span = if self.preserve_ctxt {
                DUMMY_SP.with_ctxt(span.ctxt())
            } else {
                DUMMY_SP
            };
        }
    }
    struct DropNodeId;
    impl VisitMut for DropNodeId {
        fn visit_mut_node_id(&mut self, span: &mut NodeId) {
            *span = NodeId::from_u32(0);
        }
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
        )
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
        // Setters can only have one param; synthesising any more would be an error.
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
        // Setters can only have one param; synthesising any more would be an error.
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
        test_same("function f() {arguments[0]()}"); // replacing the call would change `this`
    }

    #[test]
    fn testNoOptimizationWhenArgumentsReassigned() {
        // TODO: can we replace the accesses before the re-assignment?
        // replacing the post-assignment `arguments[0]` with a named parameter would be incorrect
        test_same("function f() { arguments[0]; arguments = [3, 4, 5]; arguments[0]; }");
    }

    #[test]
    fn testUnusualArgumentsUsage() {
        test_same("function f(x) { x[arguments]; }");
    }

    #[test]
    fn testUseArguments_withDefaultValue() {
        // `arguments` doesn't consider default values. It holds exaclty the provided args.
        test_same("function f(x = 0) { arguments[0]; }");

        test_transform(
            "function f(x = 0) { arguments[1]; }", //
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

        // We could possibly do better here by referencing through `rest` instead, but the additional
        // complexity of tracking and validating the rest parameter isn't worth it.
        test_same("function f(x, ...rest) { arguments[1]; }");
        test_same("function f(x, ...rest) { arguments[2]; }"); // Don't synthesize params after a rest.
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
