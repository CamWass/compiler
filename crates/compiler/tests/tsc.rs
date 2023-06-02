#![feature(test)]

extern crate test;

use ahash::{AHashMap, AHashSet};
use compiler::{
    ast as local_ast,
    node::*,
    utils::*,
    visit::{Visit, VisitWith},
    Checker, CompilerOptions, SourceFile, TypeCheckerHost,
};
use global_common::{sync::Lrc, BytePos, FileName, FilePathMapping, SourceMap, Span, Spanned};
use parser::{lexer::Lexer, PResult, Parser, Syntax, TsConfig};
use std::{
    env,
    fmt::Write,
    fs::{self, read_dir},
    io::{self},
    path::Path,
    rc::Rc,
};
use test::{
    test_main, DynTestFn, Options, ShouldPanic::No, TestDesc, TestDescAndFn, TestName, TestType,
};
use testing::{NormalizedOutput, StdErr};
// #[testing::fixture("tests/tsc/**/*.ts")]
// fn spec(file: PathBuf) {
//     let file_name = file
//         .display()
//         .to_string()
//         .replace("\\\\", "/")
//         .replace("\\", "/");

//     println!("{}", file_name);

//     panic!();
// }

fn add_test<F: FnOnce() -> Result<(), String> + Send + 'static>(
    tests: &mut Vec<TestDescAndFn>,
    name: String,
    ignore: bool,
    f: F,
) {
    tests.push(TestDescAndFn {
        desc: TestDesc {
            test_type: TestType::UnitTest,
            name: TestName::DynTestName(name),
            ignore,
            should_panic: No,
            compile_fail: false,
            no_run: false,
            ignore_message: Default::default(),
        },
        testfn: DynTestFn(Box::new(f)),
    });
}

fn error_tests(tests: &mut Vec<TestDescAndFn>) -> Result<(), io::Error> {
    let test_dir = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("tests");
        root.push("TypeScript");
        root.push("tests");
        root.push("cases");
        root.push("compiler");
        root
    };
    let ref_dir = {
        let mut root = Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf();
        root.push("tests");
        root.push("TypeScript");
        root.push("tests");
        root.push("baselines");
        root.push("reference");
        root
    };

    eprintln!("Loading tests from {}", test_dir.display());

    let entries = read_dir(&test_dir)?
        .map(|res| res.map(|e| e.path()))
        .collect::<Result<Vec<_>, io::Error>>()?;

    // TODO: this is a temp, implicit, whitelist system. It should be removed once all tests pass.
    const NUM_TESTS: usize = 5;

    assert!(NUM_TESTS <= entries.len());

    let skip = ["abstractPropertyBasics.ts"];

    let mut i = 0;
    let mut count = 0;

    while i < entries.len() && count < NUM_TESTS {
        i += 1;
        let entry = &entries[i];
        if entry.extension().unwrap() != "ts" {
            continue;
        }

        let file_name = entry
            .strip_prefix(&test_dir)
            .expect("failed to strip prefix")
            .to_str()
            .unwrap()
            .to_string();

        if skip.contains(&&*file_name) {
            eprintln!("Skipped test `{}`", &file_name);
            continue;
        }

        count += 1;

        let input = fs::read_to_string(entry)?;

        let mut reference_path = ref_dir.join(&file_name);
        reference_path.set_extension("symbols");

        let reference = fs::read_to_string(reference_path)?;

        let entry = entry.clone();

        let name = format!("tsc::{}", file_name);
        add_test(tests, name, false, move || {
            eprintln!(
                "\n\n========== Running error reporting test {}\nSource:\n{}\n",
                file_name, input
            );

            let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));

            // Parse source
            let program_ast = parse_program(cm.clone(), &entry).unwrap();
            let program = SourceFile::new(file_name.clone(), program_ast.clone());

            let lib_name = "lib.es5.d.ts";
            // let lib_name = "temp.d.ts";
            let lib_ast = parse_program(cm.clone(), &Path::new("../../").join(lib_name)).unwrap();
            let lib = SourceFile::new(lib_name.into(), lib_ast.clone());

            let host = TypeCheckerHost {
                files: vec![lib, program],
                compiler_options: CompilerOptions::default(),
            };

            let program_file_names = new_ahash_map![
                (lib_ast.clone(), lib_name.to_string()),
                (program_ast.clone(), file_name.clone())
            ];
            let mut lib_file_names = AHashSet::with_capacity(1);
            lib_file_names.insert(lib_name.to_string());

            let symbols = SymbolWriter::get_symbols(
                Checker::new(host, true),
                program_file_names,
                lib_file_names,
                cm.lookup_char_pos(program_ast.span().lo()).file,
                cm,
                &file_name,
                program_ast,
            );

            fs::write("output.txt", &symbols);
            fs::write("reference.txt", &reference);

            assert_eq!(symbols, reference);
        });
    }

    Ok(())
}

fn parse_program<'a>(
    cm: Lrc<SourceMap>,
    file_name: &Path,
) -> Result<local_ast::Program, NormalizedOutput> {
    with_parser(cm, file_name, |p| {
        p.parse_program().map(local_ast::convert::convert_program)
    })
}

fn with_parser<F, Ret>(cm: Lrc<SourceMap>, file_name: &Path, f: F) -> Result<Ret, StdErr>
where
    F: FnOnce(&mut Parser<Lexer>) -> PResult<Ret>,
{
    ::testing::run_test_with_source_map(cm.clone(), false, |handler| {
        let src = fs::read_to_string(file_name)
            .expect("faild to read file")
            .trim_start()
            .to_string();
        let fm = cm.new_source_file(FileName::Real(file_name.into()), src);

        let mut p = Parser::new(
            Syntax::Typescript(TsConfig::default()),
            &fm,
            Default::default(),
        );

        let res = f(&mut p).map_err(|e| e.into_diagnostic(handler).emit());

        for e in p.take_errors() {
            e.into_diagnostic(handler).emit();
        }

        if handler.has_errors() {
            return Err(());
        }

        res
    })
}

#[test]
fn error() {
    let args: Vec<_> = env::args().collect();
    let mut tests = Vec::new();
    error_tests(&mut tests).unwrap();
    test_main(&args, tests, Some(Options::new()));
}

macro_rules! generate_visitors {
    (@types, [$([$name:ident, $N:ident]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &Rc<local_ast::$N>, parent: Option<BoundNode>) {
                let node = n.bind_to_opt_parent(parent.clone());
                self.result.push_str(self.checker.getTypeAtLocation(node));
                n.visit_children_with(self, parent);
            }
        )*

    };
    (@symbols, [$([$name:ident, $N:ident]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &Rc<local_ast::$N>, parent: Option<BoundNode>) {
                let node = n.bind_to_opt_parent(parent.clone());
                if isExpressionNode(&node) || matches!(node, BoundNode::Ident(_)) || isDeclarationName(node.clone()) {
                    self.write_symbol(node, parent.clone());
                }
                n.visit_children_with(self, parent);
            }
        )*

    };
}

struct SymbolWriter {
    checker: Checker,
    result: String,
    program_file_names: AHashMap<local_ast::Program, String>,
    lib_file_names: AHashSet<String>,
    line_map: Vec<usize>,
    token_spans: Vec<Span>,
    last_index_written: Option<usize>,
    cm: Lrc<SourceMap>,
}

struct SymbolWriterResult {
    line: usize,
    source_text: String,
    symbol: String,
}

macro_rules! regex {
    ($re:literal $(,)?) => {{
        static RE: once_cell::sync::OnceCell<regex::Regex> = once_cell::sync::OnceCell::new();
        RE.get_or_init(|| regex::Regex::new($re).unwrap())
    }};
}

impl SymbolWriter {
    fn get_symbols(
        checker: Checker,
        program_file_names: AHashMap<local_ast::Program, String>,
        lib_file_names: AHashSet<String>,
        file: Lrc<global_common::SourceFile>,
        cm: Lrc<SourceMap>,
        file_name: &str,
        program_ast: local_ast::Program,
    ) -> String {
        let lexer = Lexer::new(
            Syntax::Typescript(TsConfig::default()),
            Default::default(),
            &file,
        );
        let mut writer = SymbolWriter {
            checker,
            result: String::new(),
            program_file_names,
            lib_file_names,
            line_map: compute_line_starts(file.src.as_ref()),
            token_spans: lexer.map(|t| t.span).collect::<Vec<_>>(),
            last_index_written: None,
            cm,
        };

        write!(
            writer.result,
            "=== tests/cases/compiler/{} ===\r\n",
            file_name
        )
        .expect("failed to write to string");

        program_ast.visit_with(&mut writer, None);

        if let Some(last_index_written) = writer.last_index_written {
            if last_index_written + 1 < writer.line_map.len() {
                if !((last_index_written + 1 < writer.line_map.len()) && {
                    writer.get_code_line(last_index_written + 1, |_, line| {
                        regex!(r"^\s*[{|}]\s*$").is_match(line) || line.trim() == ""
                    })
                }) {
                    writer.result.push_str("\r\n");
                }
                for line_index in last_index_written + 1..writer.line_map.len() {
                    writer.get_code_line(line_index, |writer, line| {
                        write!(writer.result, "{}\r\n", line).expect("failed to write to string");
                    });
                }
            } else {
                writer.result.push_str("\r\n");
            }
        } else {
            // for (const codeLine of codeLines) {
            //     typeLines += codeLine + "\r\nNo type information for this code.";
            // }
            todo!();
        }
        writer.result
    }

    // e.g. `Symbol(Cell, Decl(2dArrays.ts, 0, 0))`
    fn get_symbol_string(
        &mut self,
        node: BoundNode,
        parent: Option<BoundNode>,
    ) -> Result<Option<SymbolWriterResult>, std::fmt::Error> {
        if let Some(result) = self.checker.pubGetSymbolAtLocation_todo(node.clone()) {
            let mut symbol_string = String::new();
            write!(
                symbol_string,
                "Symbol({}",
                self.checker
                    .pubSymbolToString_todo(result, parent, None, None)
            )?;
            for (i, decl) in self.checker.symbols[result]
                .declarations()
                .iter()
                .enumerate()
            {
                if i >= 5 {
                    write!(
                        symbol_string,
                        " ... and {} more",
                        self.checker.symbols[result].declarations().len() - i
                    )?;
                    break;
                }
                let file_name = self.get_file_name_for_node(decl);
                if self.is_lib_file_name(file_name) {
                    write!(symbol_string, ", Decl({}, --, --)", file_name)?;
                } else {
                    // NOTE: This pos, line, and col might not be what you expect. See the comment on
                    // Self::get_pos_of_node for more details.
                    let pos = self.get_pos_of_node(decl);
                    let (line, col) = self.get_line_and_character_of_position(pos);
                    write!(symbol_string, ", Decl({}, {}, {})", file_name, line, col)?;
                }
            }
            symbol_string.push(')');

            // The span of a BindingIdent also includes its type annotation,
            // which we dont want to print.
            let span = if let BoundNode::BindingIdent(i) = &node {
                if let Some(ty) = &i.type_ann {
                    i.id.span.with_hi(ty.span.lo)
                } else {
                    i.id.span
                }
            } else {
                node.span()
            };

            Ok(Some(SymbolWriterResult {
                line: self.get_line_and_character_of_position(span.lo).0,
                source_text: self.cm.span_to_snippet(span).expect("failed to get span"),
                symbol: symbol_string,
            }))
        } else {
            Ok(None)
        }
    }

    /// Gets source of a line, where `index` is an index into `self.line_map`.
    fn get_code_line<T>(
        &mut self,
        index: usize,
        f: impl FnOnce(&mut SymbolWriter, &str) -> T,
    ) -> T {
        let lo = self
            .cm
            .lookup_byte_offset(BytePos(self.line_map[index] as u32));
        let file = lo.sf;
        let lo = lo.pos.0 as usize;
        let line = match file.src[lo..].find(is_line_break) {
            Some(hi) => &file.src[lo..hi + lo],
            // If there are no more line breaks, we must be on the last line.
            None => &file.src[lo..],
        };
        f(self, line)
    }

    fn write_symbol(&mut self, node: BoundNode, parent: Option<BoundNode>) {
        let result = self
            .get_symbol_string(node, parent)
            .expect("failed print symbol");
        if let Some(result) = result {
            if let Some(last_index_written) = self.last_index_written {
                if result.line != last_index_written {
                    if !((last_index_written + 1 < self.line_map.len()) && {
                        self.get_code_line(last_index_written + 1, |_, line| {
                            regex!(r"^\s*[{|}]\s*$").is_match(line) || line.trim() == ""
                        })
                    }) {
                        self.result.push_str("\r\n");
                    }
                    for line_index in last_index_written + 1..result.line + 1 {
                        self.get_code_line(line_index, |writer, line| {
                            write!(writer.result, "{}\r\n", line)
                                .expect("failed to write to string");
                        });
                    }
                }
            } else {
                for line_index in 0..result.line + 1 {
                    self.get_code_line(line_index, |writer, line| {
                        write!(writer.result, "{}\r\n", line).expect("failed to write to string");
                    });
                }
            }
            self.last_index_written = Some(result.line);

            self.result.push('>');
            for line in result.source_text.lines() {
                self.result.push_str(line);
            }
            write!(self.result, " : {}\r\n", result.symbol).expect("failed to write to string");
        }
    }

    /// Gets the name of the source file that contains `node`.
    fn get_file_name_for_node(&self, node: &BoundNode) -> &String {
        let source_file = match getSourceFileOfNode(node.clone()) {
            BoundNode::Script(n) => local_ast::Program::Script(n.node.clone()),
            BoundNode::Module(n) => local_ast::Program::Module(n.node.clone()),
            _ => unreachable!(),
        };

        self.program_file_names.get(&source_file).unwrap()
    }

    fn is_lib_file_name(&self, file_name: &String) -> bool {
        self.lib_file_names.contains(file_name)
    }

    /// Returns (line, col), where line is an index into `self.line_map`, and col is the
    /// BytePos's psotion within the line.
    fn get_line_and_character_of_position(&self, pos: BytePos) -> (usize, usize) {
        let position = pos.0 as usize;
        let line_starts = &self.line_map;
        let line_number = compute_line_of_position(line_starts, position);
        (line_number, position - line_starts[line_number])
    }

    /// # Caution
    /// This returns positions that are unintuative, but match TSC's version of a node start.
    /// In this version, a node starts immediently after the preceding one, ignoring all whitespace/comments
    /// (trivia) in between the content of the nodes.
    /// In our version (span.lo), a node starts where it begins in the input, even if there is trivia between
    /// it and its predesesor.
    ///
    /// For example, in the following code:
    /// ```js
    /// class A {
    /// }
    ///
    /// class B {
    /// }
    /// ```
    /// #### Class A:
    /// |          | start   | end      |
    /// |----------|---------|--------- |
    /// | **TSC**  | 0 (pos) | 11 (end) |
    /// | **Ours** | 0 (lo)  | 12 (hi)  |
    /// #### Class B:
    /// |          | start    | end      |
    /// |----------|--------- |--------- |
    /// | **TSC**  | 11 (pos) | 24 (end) |
    /// | **Ours** | 16 (lo)  | 28 (hi)  |
    ///
    /// Note how in TSC, class B begins immediatly after class A, where as in our version, the 4 whitespace characters are acounted for.
    /// Because of this, **the line numbers TSC generates for our reference tests are non-sensical**. For example, acording to TSC, the symbol
    /// 'B' is declared on line 1, which does not align with the visual representation. In order for our test outputs to match the references,
    /// we have to do some janky stuff:
    /// 1. First, when [SymbolWriter] is created, we re-lex the input to obtain the spans of the tokens.
    /// 2. Then when a node's position is requested in [get_pos_of_node], we find the index of the node's first token in the token stream.
    /// 3. Finally, we get the span of the previous token in the stream. This span's hi value is where the target node would begin if there were no
    /// trivia between it and the previous node. In other words, this hi value is TSC's pos for the target node.
    ///
    /// For example, if we requested the position of class B in the example above, we would:
    /// 1. Find its first token the the token stream (the 'class' keyword).
    /// 2. Get the hi of the previous token's span (11). The previous token is a the right curly brace: '}'.
    ///
    /// This works because our lexer doesn't produce tokens for trivia, so the previous token will always be a part of a node, and any trivia will
    /// be between the target's lo and the previous token's hi.
    fn get_pos_of_node(&self, node: &BoundNode) -> BytePos {
        let current = self
            .token_spans
            .binary_search_by_key(&node.span().lo(), |s| s.lo())
            .expect("span should be found");
        if current == 0 {
            node.span().lo()
        } else {
            self.token_spans[current - 1].hi()
        }
    }
}

mod char_literals {
    pub const LINE_FEED: char = '\u{000a}';
    pub const LINE_SEPARATOR: char = '\u{2028}';
    pub const CARRIAGE_RETURN: char = '\u{000d}';
    pub const PARAGRAPH_SEPARATOR: char = '\u{2029}';
}

/// See https://tc39.github.io/ecma262/#sec-line-terminators
fn is_line_break(ch: char) -> bool {
    matches!(
        ch,
        char_literals::LINE_FEED
            | char_literals::CARRIAGE_RETURN
            | char_literals::LINE_SEPARATOR
            | char_literals::PARAGRAPH_SEPARATOR
    )
}

// We can't use `SourceFile.lines` because the SourceFile/SourceMap uses different line breaks.
fn compute_line_starts(text: &str) -> Vec<usize> {
    let mut result = Vec::new();
    let mut pos = 0;
    let mut line_start = 0;
    while pos < text.len() {
        let string = unsafe { std::str::from_utf8_unchecked(text.as_bytes().get_unchecked(pos..)) };
        let mut chars = string.chars();
        let ch = chars.next().unwrap();
        pos += ch.len_utf8();
        match ch {
            char_literals::CARRIAGE_RETURN => {
                if chars.next() == Some(char_literals::LINE_FEED) {
                    pos += 1;
                }
                result.push(line_start);
                line_start = pos;
            }
            char_literals::LINE_FEED => {
                result.push(line_start);
                line_start = pos;
            }
            _ => {
                if is_line_break(ch) {
                    result.push(line_start);
                    line_start = pos;
                }
            }
        }
    }
    result.push(line_start);
    result
}

fn compute_line_of_position(line_starts: &[usize], position: usize) -> usize {
    match line_starts.binary_search(&position) {
        Ok(i) => i,
        // If the actual position was not found, binary_search returns Err with the index
        // where the value should be inserted to maintain order. We want the index of the
        // previous line start, so we subtract 1.
        Err(i) => i - 1,
    }
}

impl Visit for SymbolWriter {
    generate_visitors!(@symbols,[
        [visit_class, Class],
        [visit_extends_clause, ExtendsClause],
        [visit_class_prop, ClassProp],
        [visit_private_prop, PrivateProp],
        [visit_class_method, ClassMethod],
        [visit_private_method, PrivateMethod],
        [visit_constructor, Constructor],
        [visit_decorator, Decorator],
        [visit_fn_decl, FnDecl],
        [visit_class_decl, ClassDecl],
        [visit_var_decl, VarDecl],
        [visit_var_declarator, VarDeclarator],
        [visit_this_expr, ThisExpr],
        [visit_array_lit, ArrayLit],
        [visit_object_lit, ObjectLit],
        [visit_spread_element, SpreadElement],
        [visit_unary_expr, UnaryExpr],
        [visit_update_expr, UpdateExpr],
        [visit_bin_expr, BinExpr],
        [visit_fn_expr, FnExpr],
        [visit_class_expr, ClassExpr],
        [visit_assign_expr, AssignExpr],
        [visit_member_expr, MemberExpr],
        [visit_cond_expr, CondExpr],
        [visit_call_expr, CallExpr],
        [visit_new_expr, NewExpr],
        [visit_seq_expr, SeqExpr],
        [visit_arrow_expr, ArrowExpr],
        [visit_yield_expr, YieldExpr],
        [visit_meta_prop_expr, MetaPropExpr],
        [visit_await_expr, AwaitExpr],
        [visit_tpl, Tpl],
        [visit_tagged_tpl, TaggedTpl],
        [visit_tpl_element, TplElement],
        [visit_paren_expr, ParenExpr],
        [visit_super, Super],
        [visit_opt_chain_expr, OptChainExpr],
        [visit_function, Function],
        [visit_param, Param],
        [visit_param_without_decorators, ParamWithoutDecorators],
        [visit_ts_ambient_param, TsAmbientParam],
        [visit_binding_ident, BindingIdent],
        [visit_ident, Ident],
        [visit_private_name, PrivateName],
        [visit_jsx_member_expr, JSXMemberExpr],
        [visit_jsx_namespaced_name, JSXNamespacedName],
        [visit_jsx_empty_expr, JSXEmptyExpr],
        [visit_jsx_expr_container, JSXExprContainer],
        [visit_jsx_spread_child, JSXSpreadChild],
        [visit_jsx_opening_element, JSXOpeningElement],
        [visit_jsx_closing_element, JSXClosingElement],
        [visit_jsx_attr, JSXAttr],
        [visit_jsx_text, JSXText],
        [visit_jsx_element, JSXElement],
        [visit_jsx_fragment, JSXFragment],
        [visit_jsx_opening_fragment, JSXOpeningFragment],
        [visit_jsx_closing_fragment, JSXClosingFragment],
        [visit_invalid, Invalid],
        [visit_big_int, BigInt],
        [visit_str, Str],
        [visit_bool, Bool],
        [visit_null, Null],
        [visit_regex, Regex],
        [visit_number, Number],
        [visit_module, Module],
        [visit_script, Script],
        [visit_export_default_expr, ExportDefaultExpr],
        [visit_export_decl, ExportDecl],
        [visit_import_decl, ImportDecl],
        [visit_export_all, ExportAll],
        [visit_named_export, NamedExport],
        [visit_export_default_decl, ExportDefaultDecl],
        [visit_import_default_specifier, ImportDefaultSpecifier],
        [visit_import_star_as_specifier, ImportStarAsSpecifier],
        [visit_import_named_specifier, ImportNamedSpecifier],
        [visit_export_namespace_specifier, ExportNamespaceSpecifier],
        [visit_export_default_specifier, ExportDefaultSpecifier],
        [visit_export_named_specifier, ExportNamedSpecifier],
        [visit_array_pat, ArrayPat],
        [visit_object_pat, ObjectPat],
        [visit_assign_pat, AssignPat],
        [visit_rest_pat, RestPat],
        [visit_key_value_pat_prop, KeyValuePatProp],
        [visit_assign_pat_prop, AssignPatProp],
        [visit_key_value_prop, KeyValueProp],
        [visit_assign_prop, AssignProp],
        [visit_getter_prop, GetterProp],
        [visit_setter_prop, SetterProp],
        [visit_method_prop, MethodProp],
        [visit_computed_prop_name, ComputedPropName],
        [visit_block_stmt, BlockStmt],
        [visit_expr_stmt, ExprStmt],
        [visit_empty_stmt, EmptyStmt],
        [visit_debugger_stmt, DebuggerStmt],
        [visit_with_stmt, WithStmt],
        [visit_return_stmt, ReturnStmt],
        [visit_labeled_stmt, LabeledStmt],
        [visit_break_stmt, BreakStmt],
        [visit_continue_stmt, ContinueStmt],
        [visit_if_stmt, IfStmt],
        [visit_switch_stmt, SwitchStmt],
        [visit_throw_stmt, ThrowStmt],
        [visit_try_stmt, TryStmt],
        [visit_while_stmt, WhileStmt],
        [visit_do_while_stmt, DoWhileStmt],
        [visit_for_stmt, ForStmt],
        [visit_for_in_stmt, ForInStmt],
        [visit_for_of_stmt, ForOfStmt],
        [visit_switch_case, SwitchCase],
        [visit_catch_clause, CatchClause],
        [visit_ts_type_ann, TsTypeAnn],
        [visit_ts_type_param_decl, TsTypeParamDecl],
        [visit_ts_type_param_instantiation, TsTypeParamInstantiation],
        [visit_ts_param_prop, TsParamProp],
        [visit_ts_qualified_name, TsQualifiedName],
        [visit_ts_call_signature_decl, TsCallSignatureDecl],
        [visit_ts_construct_signature_decl, TsConstructSignatureDecl],
        [visit_ts_property_signature, TsPropertySignature],
        [visit_ts_getter_signature, TsGetterSignature],
        [visit_ts_setter_signature, TsSetterSignature],
        [visit_ts_method_signature, TsMethodSignature],
        [visit_ts_index_signature, TsIndexSignature],
        [visit_ts_keyword_type, TsKeywordType],
        [visit_ts_this_type, TsThisType],
        [visit_ts_fn_type, TsFnType],
        [visit_ts_constructor_type, TsConstructorType],
        [visit_ts_type_ref, TsTypeRef],
        [visit_ts_type_predicate, TsTypePredicate],
        [visit_ts_type_query, TsTypeQuery],
        [visit_ts_import_type, TsImportType],
        [visit_ts_type_lit, TsTypeLit],
        [visit_ts_array_type, TsArrayType],
        [visit_ts_tuple_type, TsTupleType],
        [visit_ts_tuple_element, TsTupleElement],
        [visit_ts_optional_type, TsOptionalType],
        [visit_ts_rest_type, TsRestType],
        [visit_ts_union_type, TsUnionType],
        [visit_ts_intersection_type, TsIntersectionType],
        [visit_ts_conditional_type, TsConditionalType],
        [visit_ts_infer_type, TsInferType],
        [visit_ts_parenthesized_type, TsParenthesizedType],
        [visit_ts_type_operator, TsTypeOperator],
        [visit_ts_indexed_access_type, TsIndexedAccessType],
        [visit_ts_mapped_type, TsMappedType],
        [visit_ts_lit_type, TsLitType],
        [visit_ts_tpl_lit_type, TsTplLitType],
        [visit_ts_interface_decl, TsInterfaceDecl],
        [visit_ts_interface_body, TsInterfaceBody],
        [visit_ts_expr_with_type_args, TsExprWithTypeArgs],
        [visit_ts_type_alias_decl, TsTypeAliasDecl],
        [visit_ts_enum_decl, TsEnumDecl],
        [visit_ts_enum_member, TsEnumMember],
        [visit_ts_module_decl, TsModuleDecl],
        [visit_ts_module_block, TsModuleBlock],
        [visit_ts_namespace_decl, TsNamespaceDecl],
        [visit_ts_import_equals_decl, TsImportEqualsDecl],
        [visit_ts_external_module_ref, TsExternalModuleRef],
        [visit_ts_export_assignment, TsExportAssignment],
        [visit_ts_namespace_export_decl, TsNamespaceExportDecl],
        [visit_ts_as_expr, TsAsExpr],
        [visit_ts_type_assertion, TsTypeAssertion],
        [visit_ts_non_null_expr, TsNonNullExpr],
        [visit_ts_const_assertion, TsConstAssertion],
    ]);
}
