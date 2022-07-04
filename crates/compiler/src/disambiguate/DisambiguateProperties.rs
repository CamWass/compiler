use super::ClusterPropagator::ClusterPropagator;
use super::ColorFindPropertyReferences::ColorFindPropertyReferences;
use super::ColorGraphBuilder::ColorGraphBuilder;
use super::ColorGraphNodeFactory::ColorGraphNodeFactory;
use super::Invalidation::Invalidation;
use super::PropertyClustering::*;
use super::UseSiteRenamer::rename_use_sites;
use crate::colors::color_registry::ColorRegistry;
use crate::graph::FixedPointGraphTraversal::*;
use index::vec::IndexVec;

/** Assembles the various parts of the diambiguator to execute them as a compiler pass. */
pub struct DisambiguateProperties {}

impl DisambiguateProperties {
    pub fn process(ast: &mut ::ast::Program, colours: &mut ColorRegistry) {
        let mut flattener = ColorGraphNodeFactory::new(colours);
        let (propIndex, mut propertyClusterings) =
            ColorFindPropertyReferences::find_prop_references(&mut flattener, ast);

        // TODO:
        // invalidateWellKnownProperties(propIndex);

        let allKnownTypes = flattener.getAllKnownTypes();
        let mut graphBuilder = ColorGraphBuilder::new(&mut flattener);
        graphBuilder.addAll(&allKnownTypes);
        let graph = graphBuilder.build();

        // dbg!(&flattener.nodes);

        // for (i, node) in flattener.nodes.iter().enumerate() {
        //     println!("Color graph node {}:", i);
        //     println!("Associated with prop: {}", node.associatedProps.len() > 0);
        //     let color_id = node.color;
        //     println!("ColorId: {:?}", color_id);
        //     println!("Color: {:#?}", &flattener.colours.colors[color_id]);
        //     let mut nodes = Vec::new();
        //     for (node, &color) in flattener.colours.node_to_color_map.iter() {
        //         if color == color_id {
        //             nodes.push(node.clone());
        //         }
        //     }
        //     println!("Nodes with color: {:#?}", nodes);
        //     println!("======================");
        // }

        // let dot = format!(
        //     "{:?}",
        //     Dot::with_config(&graph, &[/*Config::NodeIndexLabel*/])
        // );

        // std::fs::write("colors.dot", dot).expect("Failed to output color graph");

        // TOOD:
        // Model legacy behavior from the old (pre-January 2021) disambiguator.
        // TODO(b/177695515): delete this section.
        // for (ColorGraphNode colorGraphNode : flattener.getAllKnownTypes()) {
        //     if (graph.getOutEdges(colorGraphNode).isEmpty()) {
        //         // Skipping leaf types improves code size, especially as "namespace" types are all leaf
        //         // types and will often have their declared properties collapsed into variables.
        //         continue;
        //     }
        //     if (colorGraphNode.getColor().isUnion()) {
        //         // Only need this step for SINGLE ColorGraphNodes because
        //         // and we will add properties of each alternate elsewhere in this loop.
        //         continue;
        //     }
        //     registerOwnDeclaredProperties(colorGraphNode, propIndex);
        // }

        invalidateBasedOnType(&mut flattener, &mut propertyClusterings);

        let propagator = ClusterPropagator::new(&mut flattener, &mut propertyClusterings);
        FixedPointGraphTraversal::newTraversal(propagator).computeFixedPoint(&graph);

        // let mut renamer = UseSiteRenamer::default();

        // // TODO: is propIndex.values() the same as propertyClusterings? If so, iterating on propertyClusterings might be better.
        // for &prop in propIndex.values() {
        //     renamer.renameUses(&mut propertyClusterings[prop]);
        //     // TODO: error
        //     // if propertyClusterings[prop].isInvalidated() && self.propertiesThatMustDisambiguate.contains(prop.getName())
        //     // {
        //     //     self.compiler.report(this.createInvalidationError(prop));
        //     // }
        // }

        // dbg!(
        //     &propIndex,
        //     &propertyClusterings,
        //     &colours.disambiguationSupertypeGraph
        // );

        // TODO: temp; just for dev sanity
        debug_assert_eq!(propIndex.len(), propertyClusterings.len());

        rename_use_sites(ast, propertyClusterings);
        // TODO: now that we've modified the ast, the node hashes will have changed. This means hashmaps ect will break,
        // e.g. colour regisrty's node_to_color_map. This could be solved by e.g. manually patching the hashmaps,
        // or hashing node IDs instead of nodes.
    }
}

fn invalidateBasedOnType(
    flattener: &mut ColorGraphNodeFactory,
    propertyClusterings: &mut IndexVec<PropertyClusteringId, PropertyClustering>,
) {
    for ty in flattener.getAllKnownTypes() {
        let node = &flattener.nodes[ty];
        let color = &flattener.colours.colors[node.color];
        if color.isInvalidating {
            for &prop in node.associatedProps.keys() {
                propertyClusterings[prop]
                    .invalidate(Invalidation::INVALIDATING_TYPE { reciever_type: ty });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::colors::color_registry::ColorRegistry;
    use crate::{Checker, CompProgram, CompilerOptions, SourceFile, TypeCheckerHost};
    use ast::*;
    use codegen::{text_writer::JsWriter, Emitter};
    use ecma_visit::{VisitMut, VisitMutWith};
    use global_common::{errors::Handler, sync::Lrc, FileName, SourceMap, Span, DUMMY_SP};
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

    fn init(program: ast::Program, file_name: &str) -> ColorRegistry {
        let program_ast = program;

        let program_source_file = SourceFile {
            file_name: file_name.to_string(),
            program: crate::ast::convert::convert_program(program_ast.clone()),
            jsGlobalAugmentations: Default::default(),
        };
        let source_files = vec![program_source_file.clone()];

        let host = TypeCheckerHost {
            files: source_files,
            compiler_options: CompilerOptions::default(),
        };

        let mut checker = Checker::new(host, false);

        let p = CompProgram {
            libs: Vec::new(),
            source: program_source_file.clone(),
        };

        crate::colors::color_collector::collect(&mut checker, &p)
    }

    fn test_transform(input: &str, expected: &str) {
        Tester::run(|tester| {
            let expected = tester.apply_transform(|m| m, "output.js", expected)?;

            println!("----- Actual -----");

            let mut actual = tester.apply_transform(
                |mut ast_program| {
                    let mut colours = init(ast_program.clone(), "input.js");
                    super::DisambiguateProperties::process(&mut ast_program, &mut colours);
                    ast_program
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

    // TODO: test for
    // accessing props (memebr expr)
    // generics
    // class fields
    // object lits
    // interfaces
    // functions?
    // Optional chaining
    // index signatures
    // Missing props
    // getter/setters
    // super

    // Type param with constraint
    #[test]
    fn todo_rename1() {
        test_transform(
            "
class Foo {
    parent: Foo;
    pos: number;
}
function use<T extends Foo>(node: T, parent: Foo) {
    node.parent;
    node.pos;
    parent.parent;
    parent.pos;
}
class Other {
    parent: any;
    pos: any;
}",
            "
class Foo {
    JSC$1_parent: Foo;
    JSC$1_pos: number;
}
function use<T extends Foo>(node: T, parent: Foo) {
    node.JSC$1_parent;
    node.JSC$1_pos;
    parent.JSC$1_parent;
    parent.JSC$1_pos;
}
class Other {
    JSC$2_parent: any;
    JSC$2_pos: any;
}",
        );
    }

    // type alias
    #[test]
    fn todo_rename2() {
        test_transform(
            "
class Foo {
    g() {}
}

function use1(v: Foo) {
    v.g();
}
type Foo1 = Foo;
function use2(v: Foo1) {
    v.g();
}

class Other {
    g() {}
}",
            "
class Foo {
    JSC$1_g() {}
}

function use1(v: Foo) {
    v.JSC$1_g();
}
type Foo1 = Foo;
function use2(v: Foo1) {
    v.JSC$1_g();
}

class Other {
    JSC$2_g() {}
}",
        );
    }

    // common interface ancestor
    #[test]
    fn todo_rename3() {
        test_transform(
            "
interface Common {
    a: string;
}

class Foo implements Common {
    a = '';
}
class Bar implements Common {
    a = '';
}

function f1(v: Foo) {
    v.a;
}
function f2(v: Bar) {
    v.a;
}

class Other {
    a: string;
}",
            "
interface Common {
    JSC$1_a: string;
}

class Foo implements Common {
    JSC$1_a = '';
}
class Bar implements Common {
    JSC$1_a = '';
}

function f1(v: Foo) {
    v.JSC$1_a;
}
function f2(v: Bar) {
    v.JSC$1_a;
}

class Other {
    JSC$4_a: string;
}",
        );
    }

    // =================================================================
    // Tests from closure
    // =================================================================

    #[test]
    fn propertiesAreConflated_byExtends_fromInterface() {
        test_transform(
            "
interface IFoo {
    x();
}

interface IFoo2 extends IFoo {
    x();
}

class Other {
    x() {}
}",
            "
interface IFoo {
    JSC$1_x();
}

interface IFoo2 extends IFoo {
    JSC$1_x();
}

class Other {
    JSC$3_x() {}
}",
        );
    }

    #[test]
    fn propertiesAreConflated_byExtends_fromClass() {
        test_transform(
            "
class Foo {
    y() {}
}
class Foo2 extends Foo {
    y() {}
}
class Other {
    y() {}
}",
            "
class Foo {
    JSC$1_y() {}
}
class Foo2 extends Foo {
    JSC$1_y() {}
}
class Other {
    JSC$3_y() {}
}",
        );
    }

    // TODO: propertiesAreConflated_byExtends_viaPrototype

    #[test]
    fn propertiesAreConflated_byImplements_fromInterface() {
        test_transform(
            "
interface IFoo {
    a();
}
class Foo implements IFoo {
    a() {}
}
class Other {
    a() {}
}",
            "
interface IFoo {
    JSC$1_a();
}
class Foo implements IFoo {
    JSC$1_a() {}
}
class Other {
    JSC$3_a() {}
}",
        );
    }

    #[test]
    fn propertiesAreConflated_byCommonDescendent() {
        test_transform(
            "
interface IFoo0 {
    b();
    c?();
}
interface IFoo1 {
    d?();
    b();
}
class Foo implements IFoo0, IFoo1 {
    b() {}
}
class Other {
    b() {}
    c() {}
    d() {}
}",
            "
interface IFoo0 {
    JSC$1_b();
    JSC$1_c?();
}
interface IFoo1 {
    JSC$2_d?();
    JSC$1_b();
}
class Foo implements IFoo0, IFoo1 {
    JSC$1_b() {}
}
class Other {
    JSC$4_b() {}
    JSC$4_c() {}
    JSC$4_d() {}
}",
        )
    }

    #[test]
    fn propertiesAreConflated_betweenDistantAncestors() {
        test_transform(
            "
class Foo0 {
    f() {}
}

class Foo1 extends Foo0 {}
class Foo2 extends Foo1 {}
class Foo3 extends Foo2 {}
class Foo4 extends Foo3 {}

class Foo5 extends Foo4 {
    f() {}
}

class Other {
    f() {}
}",
            "
class Foo0 {
    JSC$1_f() {}
}

class Foo1 extends Foo0 {}
class Foo2 extends Foo1 {}
class Foo3 extends Foo2 {}
class Foo4 extends Foo3 {}

class Foo5 extends Foo4 {
    JSC$1_f() {}
}

class Other {
    JSC$7_f() {}
}",
        );
    }

    #[test]
    fn propertiesAreConflated_byUnions_iffUnionUsesProperty() {
        test_transform(
            "
class Foo0 {
    a() {}
    b() {}
}

class Foo1 {
    a() {}
    b() {}
}

function mix(x: Foo0|Foo1) {
    x.a();
}

class Other {
    a() {}
    b() {}
}",
            "
class Foo0 {
    JSC$3_a() {}
    JSC$1_b() {}
}

class Foo1 {
    JSC$3_a() {}
    JSC$2_b() {}
}

function mix(x: Foo0|Foo1) {
    x.JSC$3_a();
}

class Other {
    JSC$4_a() {}
    JSC$4_b() {}
}",
        )
    }

    // TODO: (might not be relevant)
    // propertiesAreConflated_betweenUnionAncestors_andTypesMismatchedWithTheUnion

    // TODO: rename this test to mention generics instead of closure templates.
    #[test]
    fn propertiesAreConflated_acrossTemplateSpecializations() {
        test_transform(
            "
class Foo<T = any> {
    h(){}
}

const a: Foo<string> = new Foo();
a.h();
const b: Foo<unknown> = new Foo();
b.h();
const c: Foo<any> = new Foo();
c.h();
const d: Foo = new Foo();
d.h();

class Other {
    h() {}
}",
            "
class Foo<T = any> {
    JSC$1_h(){}
}

const a: Foo<string> = new Foo();
a.JSC$1_h();
const b: Foo<unknown> = new Foo();
b.JSC$1_h();
const c: Foo<any> = new Foo();
c.JSC$1_h();
const d: Foo = new Foo();
d.JSC$1_h();

class Other {
    JSC$2_h() {}
}",
        );
    }

    // TODO: propertiesAreConflated_betweenEnumsAndTheirValues

    #[test]
    fn propertiesAreConflated_acrossNullableAndVoidableReferences() {
        test_transform(
            "
class Foo {
    g() {}
}

function use1(x: null|undefined|Foo) {
    x.g()
}

function use2(x?: Foo) {
    x.g()
}

class Other {
    g() {}
}",
            "
class Foo {
    JSC$1_g() {}
}

function use1(x: null|undefined|Foo) {
    x.JSC$1_g()
}

function use2(x?: Foo) {
    x.JSC$1_g()
}

class Other {
    JSC$2_g() {}
}",
        );
    }

    #[test]
    fn propertiesAreDisambiguated_betweenContructorAndInstanceTypes() {
        test_transform(
            "
class Foo {
    static g() {}
    g() {}
}",
            "
class Foo {
    static JSC$1_g() {}
    JSC$2_g() {}
}",
        );
    }

    // TODO: (why) is this desirable?
    #[test]
    fn propertiesAreDisambiguated_betweenAncestorTypes_ifCommonChildHasNoPropUse() {
        test_transform(
            "
interface IFoo {
    t();
}
class Foo {
    t() {}
}
class SubFoo extends Foo implements IFoo {}

class Other {
    t() {}
}",
            "
interface IFoo {
    JSC$2_t();
}
class Foo {
    JSC$2_t() {}
}
class SubFoo extends Foo implements IFoo {}

class Other {
    JSC$4_t() {}
}",
        );
    }

    #[test]
    fn propertiesAreInvalidated_byToplikeAndBottomlikeTypes() {
        macro_rules! f {
            ($s1:literal + $a:ident + $s2:literal) => {{
                let mut s = String::new();
                s.push_str($s1);
                s.push_str($a);
                s.push_str($s2);
                s
            }};
        }

        let annotations = [
            "unknown",
            "any", // "null",
                  // "undefined",
                  // "void",
                  // "never",
                  // TODO: these unions might have been meaningful in closure, but seem a bit random for us.
                  // "unknown|undefined",
                  // "null|undefined",
        ];

        for annotation in annotations {
            #[rustfmt::skip]
            test_transform(
                &f!("
class Foo0 {
    k() {}
    t() {}
}
class Foo1 {
    k() {}
    t() {}
}

function mix(x: " + annotation + ") {
    x.k;
}"),
                &f!("
class Foo0 {
    k() {}
    JSC$1_t() {}
}
class Foo1 {
    k() {}
    JSC$2_t() {}
}

function mix(x: " + annotation + ") {
    x.k;
}"),
            );
        }
    }

    // TODO: maybe revert this to closure's version: `const z = {ab: 0};`
    // #[test]
    //     fn propertiesAreInvalidated_onObjectLiteralTypes() {
    //         test_same(
    //             "
    // const z = {ab: true};

    // class Other {
    //     ab() { }
    // }",
    //         );
    //     }

    // @Test
    // public void propertiesAreDisambiguated_acrossStructuralTypeMatches_iffMatchUsed() {
    //   testSame(
    //       srcs(
    //           lines(
    //               "/** @record */",
    //               "class Foo0 {",
    //               "  a() { }",
    //               "}",
    //               "/** @record */",
    //               "class Foo1 extends Foo0 {",
    //               "  a() { }",
    //               "  b() { }",
    //               "}",
    //               "class Foo2 {",
    //               "  a() { }",
    //               "  b() { }",
    //               "}",
    //               "",
    //               "const /** !Foo0 */ x = new Foo2();",
    //               "",
    //               "class Other {",
    //               " a() { }",
    //               "}")));
    // }

    // @Test
    // public void propertiesAreInvalidated_byAnonymousFunctionTypes() {
    //   test(
    //       srcs(
    //           lines(
    //               "class Foo {",
    //               "  static g() { }",
    //               "  static j() { }",
    //               "}",
    //               "",
    //               "function use(/** function() */ x) {",
    //               "  x.g;",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  j() { }",
    //               "}")),
    //       expected(
    //           lines(
    //               "class Foo {",
    //               "  static g() { }",
    //               "  static JSC$1_j() { }",
    //               "}",
    //               "",
    //               "function use(/** function() */ x) {",
    //               "  x.g;",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  JSC$3_j() { }",
    //               "}")));
    // }

    // @Test
    // public void propertiesAreDisambiguated_acrossTypeMismatches() {
    //   testSame(
    //       srcs(
    //           lines(
    //               "class Foo0 {",
    //               "  a() { }",
    //               "}",
    //               "class Foo1 extends Foo0 {",
    //               "  a() { }",
    //               "  b() { }",
    //               "}",
    //               "class Foo2 {",
    //               "  a() { }",
    //               "  b() { }",
    //               "}",
    //               "",
    //               "const /** !Foo0 */ x = new Foo2();",
    //               "",
    //               "class Other {",
    //               " a() { }",
    //               "}")));
    // }

    // @Test
    // public void propertiesAreNotRenamed_fromExternsClusters() {
    //   test(
    //       externs(
    //           lines(
    //               "class ExternFoo {", //
    //               "  v() { }",
    //               "}")),
    //       srcs(
    //           lines(
    //               "class Foo extends ExternFoo {",
    //               "  v() { }",
    //               "  p() { }",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  v() { }",
    //               "  p() { }",
    //               "}")),
    //       expected(
    //           lines(
    //               "class Foo extends ExternFoo {",
    //               "  v() { }",
    //               "  JSC$3_p() { }",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  JSC$5_v() { }",
    //               "  JSC$5_p() { }",
    //               "}")));
    // }

    #[test]
    fn propertiesAreNotRenamed_ifTheyHaveASingleCluster() {
        test_transform(
            "
class Foo0 {
    w() {}
    x() {}
}
class Foo1 extends Foo0 {
    w() {}
    y() {}
}
class Foo2 {
    w() {}
    z() {}
}

function mix(x: Foo1|Foo2) {
    x.w();
}

class Other {
    x() {}
    y() {}
    z() {}
}",
            "
class Foo0 {
    w() {}
    JSC$1_x() {}
}
class Foo1 extends Foo0 {
    w() {}
    JSC$2_y() {}
}
class Foo2 {
    w() {}
    JSC$3_z() {}
}

function mix(x: Foo1|Foo2) {
    x.w();
}

class Other {
    JSC$5_x() {}
    JSC$5_y() {}
    JSC$5_z() {}
}",
        );
    }

    // @Test
    // public void propertiesAreInvalidated_givenMissingPropertyError() {
    //   test(
    //       srcs(
    //           lines(
    //               "class Foo {",
    //               "  x() { }",
    //               "  y() { }",
    //               "  z() { }",
    //               "}",
    //               "class Bar { }",
    //               "",
    //               "function mix(/** (!Foo|!Bar) */ fooBar, /** !Bar */ bar) {",
    //               // Invalidate bar.y() but not fooBar.x(). This mirrors the Closure typechecker,
    //               // which thinks (!Foo|!Bar) may have an 'x' property and so allows the access.
    //               "  fooBar.w();",
    //               "  fooBar.x();",
    //               "  bar.y();",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  w() { }",
    //               "  x() { }",
    //               "  y() { }",
    //               "  z() { }",
    //               "}")),
    //       expected(
    //           lines(
    //               "class Foo {",
    //               "  JSC$4_x() { }",
    //               "  y() { }",
    //               "  JSC$1_z() { }",
    //               "}",
    //               "class Bar { }",
    //               "",
    //               "function mix(/** (!Foo|!Bar) */ fooBar, /** !Bar */ bar) {",
    //               // x is disambiguated while w and y are invalidated.
    //               "  fooBar.w();",
    //               "  fooBar.JSC$4_x();",
    //               "  bar.y();",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  w() { }",
    //               "  JSC$6_x() { }",
    //               "  y() { }",
    //               "  JSC$6_z() { }",
    //               "}")));
    // }

    // TODO:
    //     #[test]
    //     fn propertiesAreInvalidated_ifUsedOnType_butNotDeclaredOnAncestor() {
    //         test_transform(
    //             "
    // interface IFoo {
    //     a();
    // }

    // class Foo implements IFoo {
    //     b() {}
    // }

    // new Foo().a;
    // new Foo().b;
    // new Foo().c;

    // class Other {
    //     a() {}
    //     b() {}
    //     c() {}
    // }",
    //             "
    // interface IFoo {
    //     JSC$1_a();
    // }

    // class Foo implements IFoo {
    //     JSC$2_b() {}
    // }

    // new Foo().JSC$1_a;
    // new Foo().JSC$2_b;
    // new Foo().c;

    // class Other {
    //     JSC$3_a() {}
    //     JSC$3_b() {}
    //     c() {}
    // }",
    //         );
    //     }

    // @Test
    // public void propertiesReferenced_throughReflectorFunctions_areRenamed() {
    //   test(
    //       externs(PROP_DEFINER_DEFINITION),
    //       srcs(
    //           lines(
    //               "class Foo {",
    //               "  m() { }",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  m() { }",
    //               "}",
    //               "",
    //               "goog.reflect.objectProperty('m', Foo.prototype);")),
    //       expected(
    //           lines(
    //               "class Foo {",
    //               "  JSC$3_m() { }",
    //               "}",
    //               "",
    //               "class Other {",
    //               "  JSC$5_m() { }",
    //               "}",
    //               "",
    //               "goog.reflect.objectProperty('JSC$3_m', Foo.prototype);")));
    // }

    // @Test
    // public void errorReported_forInvalidation_ofSpecifiedPropNames_oncePerName() {
    //   this.allowSourcelessWarnings();
    //   this.propertiesThatMustDisambiguate =
    //       ImmutableSet.of(
    //           "invalid0", //
    //           "invalid1");

    //   test(
    //       srcs(
    //           lines(
    //               "class Foo {",
    //               "  invalid0() { }",
    //               "  invalid1() { }",
    //               "}",
    //               "",
    //               "function use(/** ? */ x) {",
    //               "  x.invalid0",
    //               "  x.invalid1",
    //               "  x.invalid0",
    //               "  x.invalid1",
    //               "}")),
    //       error(DisambiguateProperties.PROPERTY_INVALIDATION),
    //       error(DisambiguateProperties.PROPERTY_INVALIDATION));
    // }

    // @Test
    // public void invalidatingSubtype_doesNotInvalidatePropertyOnlyReferencedOnSupertype() {
    //   // TODO(b/135045845): track mismatches through subtypes/supertypes
    //   test(
    //       srcs(
    //           lines(
    //               "class FooParent {",
    //               "  parent() {}",
    //               "}",
    //               "",
    //               "class FooChild extends FooParent {",
    //               "  child() {}",
    //               "}",
    //               "",
    //               "class BarParent {",
    //               "  parent() {}",
    //               "}",
    //               "",
    //               "class BarChild extends BarParent {",
    //               "  child() {}",
    //               "}",
    //               "",
    //               "/** ",
    //               " * @suppress {checkTypes} intentional type error",
    //               " * @type {!FooChild}",
    //               " */",
    //               "const fooChild = '';")),
    //       expected(
    //           lines(
    //               "class FooParent {",
    //               "  JSC$1_parent() {}",
    //               "}",
    //               "",
    //               "class FooChild extends FooParent {",
    //               "  child() {}",
    //               "}",
    //               "",
    //               "class BarParent {",
    //               "  JSC$5_parent() {}",
    //               "}",
    //               "",
    //               "class BarChild extends BarParent {",
    //               "  child() {}",
    //               "}",
    //               "",
    //               "/** ",
    //               " * @suppress {checkTypes} intentional type error",
    //               " * @type {!FooChild}",
    //               " */",
    //               "const fooChild = '';")));
    // }

    #[test]
    fn classField() {
        test_transform(
            "
class Foo {
    a;
    static a = true;
}

class Bar {
    a; 
    static a = false;
}",
            "
class Foo {
    JSC$1_a;
    static JSC$2_a = true;
}

class Bar {
    JSC$3_a; 
    static JSC$4_a = false;
}",
        );
    }

    #[test]
    fn classComputedField() {
        test_same(
            "
class Foo {
    ['a'];
    'b' = 2;
    1 = 'hello';
    static ['a'] = 2;
    static 'b' = 2;
    static 1 = 'hello';
}

class Bar {
    ['a'];
    'b' = 2;
    1 = 'hello';
    static ['a'] = 2;
    static 'b' = 2;
    static 1 = 'hello';
}",
        );
    }

    #[test]
    fn classMixedFields() {
        test_transform(
            "
class Foo {
    a;
    static a = 2;
    ['a'];
    static ['a'] = 2;
}

class Bar {
    a;
    static a = 2;
    ['a'];
    static ['a'] = 2;
}",
            "
class Foo {
    JSC$1_a;
    static JSC$2_a = 2;
    ['a'];
    static ['a'] = 2;
}

class Bar {
    JSC$3_a;
    static JSC$4_a = 2;
    ['a'];
    static ['a'] = 2;
}",
        );
    }
}
