use self::parser::Parser;
use super::*;
use crate::config::Config;
use global_common::{FileName, SourceMap};
use parser::{self, EsConfig, Syntax};
use std::{
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
    rc::Rc,
};

struct Builder {
    cfg: Config,
    cm: Lrc<SourceMap>,
    program_data: ProgramData,
}

impl Builder {
    pub fn with<F, Ret>(self, s: &mut Vec<u8>, op: F) -> Ret
    where
        F: FnOnce(&mut Emitter<'_>) -> Ret,
    {
        let writer = text_writer::JsWriter::new("\n", s, None);

        let mut e = Emitter::new(self.cfg, self.cm.clone(), writer, &self.program_data);

        op(&mut e)
    }

    pub fn text<F>(self, op: F) -> String
    where
        F: FnOnce(&mut Emitter<'_>),
    {
        let mut buf = vec![];

        self.with(&mut buf, op);

        String::from_utf8(buf).unwrap()
    }
}

fn parse_then_emit(from: &str, cfg: Config, syntax: Syntax) -> String {
    ::testing::run_test(false, |cm, handler| {
        let src = cm.new_source_file(FileName::Real("custom.js".into()), from.to_string());
        println!(
            "--------------------\nSource: \n{}\nPos: {:?} ~ {:?}\n",
            from, src.start_pos, src.end_pos
        );

        let program_data = Rc::new(RefCell::new(ast::ProgramData::default()));
        let res = {
            let mut parser = Parser::new(syntax, &src, program_data.clone());
            let res = parser
                .parse_module()
                .map_err(|e| e.into_diagnostic(handler).emit());

            for err in parser.take_errors() {
                err.into_diagnostic(handler).emit()
            }

            res?
        };

        let out = Builder {
            cfg,
            cm,
            program_data: Rc::try_unwrap(program_data).unwrap().into_inner(),
        }
        .text(|e| e.emit_module(&res).unwrap());
        Ok(out)
    })
    .unwrap()
}

pub(crate) fn assert_min(from: &str, to: &str) {
    let out = parse_then_emit(
        from,
        Config {
            minify: true,
            target: EsVersion::latest(),
        },
        Syntax::Es(Default::default()),
    );

    assert_eq!(DebugUsingDisplay(out.trim()), DebugUsingDisplay(to),);
}

pub(crate) fn assert_min_target(from: &str, to: &str, target: EsVersion) {
    let out = parse_then_emit(
        from,
        Config {
            minify: true,
            target,
        },
        Syntax::default(),
    );

    assert_eq!(DebugUsingDisplay(out.trim()), DebugUsingDisplay(to),);
}

pub(crate) fn assert_pretty(from: &str, to: &str) {
    let out = parse_then_emit(
        from,
        Config {
            minify: false,
            target: EsVersion::latest(),
        },
        Syntax::default(),
    );

    println!("Expected: {:?}", to);
    println!("Actual:   {:?}", out);
    assert_eq!(DebugUsingDisplay(out.trim()), DebugUsingDisplay(to),);
}

#[track_caller]
fn test_from_to(from: &str, expected: &str) {
    let out = parse_then_emit(from, Default::default(), Syntax::default());

    assert_eq!(
        DebugUsingDisplay(out.trim()),
        DebugUsingDisplay(expected.trim()),
    );
}

fn test_identical(from: &str) {
    test_from_to(from, from)
}

fn test_from_to_custom_config(from: &str, to: &str, cfg: Config, syntax: Syntax) {
    let out = parse_then_emit(from, cfg, syntax);

    assert_eq!(DebugUsingDisplay(out.trim()), DebugUsingDisplay(to.trim()),);
}

#[test]
fn no_octal_escape() {
    test_from_to(
        r#""\x00a";
"\x000";
"\x001";
"\x009""#,
        r#""\0a";
"\x000";
"\x001";
"\x009";"#,
    );
}

#[test]
fn empty_named_export() {
    test_from_to("export { }", "export { };");
}

#[test]
fn empty_named_export_min() {
    test_from_to_custom_config(
        "export { }",
        "export{};",
        Config {
            minify: true,
            ..Default::default()
        },
        Default::default(),
    );
}

#[test]
fn empty_named_export_from() {
    test_from_to("export { } from 'foo';", r#"export { } from "foo";"#);
}

#[test]
fn empty_named_export_from_min() {
    test_from_to_custom_config(
        "export { } from 'foo';",
        r#"export{}from"foo";"#,
        Config {
            minify: true,
            ..Default::default()
        },
        Default::default(),
    );
}

#[test]
fn named_export_from() {
    test_from_to(
        "export { bar } from 'foo';",
        r#"export { bar } from "foo";"#,
    );
}

#[test]
fn named_export_from_min() {
    test_from_to_custom_config(
        "export { bar } from 'foo';",
        r#"export{bar}from"foo";"#,
        Config {
            minify: true,
            ..Default::default()
        },
        Default::default(),
    );
}

#[test]
fn export_namespace_from() {
    test_from_to_custom_config(
        "export * as Foo from 'foo';",
        r#"export * as Foo from "foo";"#,
        Default::default(),
        Syntax::Es(EsConfig::default()),
    );
}

#[test]
fn export_namespace_from_min() {
    test_from_to_custom_config(
        "export * as Foo from 'foo';",
        r#"export*as Foo from"foo";"#,
        Config {
            minify: true,
            ..Default::default()
        },
        Syntax::Es(EsConfig::default()),
    );
}

#[test]
fn named_and_namespace_export_from() {
    test_from_to_custom_config(
        "export * as Foo, { bar } from 'foo';",
        r#"export * as Foo, { bar } from "foo";"#,
        Default::default(),
        Syntax::Es(EsConfig {
            export_default_from: true,
            ..EsConfig::default()
        }),
    );
}

#[test]
fn named_and_namespace_export_from_min() {
    test_from_to_custom_config(
        "export * as Foo, { bar } from 'foo';",
        r#"export*as Foo,{bar}from"foo";"#,
        Config {
            minify: true,
            ..Default::default()
        },
        Syntax::Es(EsConfig {
            export_default_from: true,
            ..EsConfig::default()
        }),
    );
}

#[test]
fn issue_450() {
    test_from_to(
        r#"console.log(`
\`\`\`html
<h1>It works!</h1>
\`\`\`
`);"#,
        r#"console.log(`\n\`\`\`html\n<h1>It works!</h1>\n\`\`\`\n`);"#,
    );
}

#[test]
fn issue_546() {
    test_from_to(
        "import availabilities, * as availabilityFunctions from 'reducers/availabilities';",
        r#"import availabilities, * as availabilityFunctions from "reducers/availabilities";"#,
    );
}

#[test]
fn issue_637() {
    test_from_to(
        r"`\
`;", r"`\
`;",
    );
}

#[test]
fn issue_639() {
    test_from_to(r"`\x1b[33m Yellow \x1b[0m`;", r"`\x1b[33m Yellow \x1b[0m`;");
}

#[test]
fn issue_910() {
    test_from_to(
        "console.log(\"Hello World\");",
        "console.log(\"Hello World\");",
    );

    test_from_to(
        "console.log('Hello World');",
        "console.log(\"Hello World\");",
    );

    test_from_to(
        "console.log(\"Hello\\\" World\");",
        "console.log('Hello\" World');",
    );

    test_from_to(
        "console.log('Hello\\' World');",
        r#"console.log("Hello' World");"#,
    );
}

#[test]
fn tpl_1() {
    test_from_to(
        "`id '${id}' must be a non-empty string`;",
        "`id '${id}' must be a non-empty string`;",
    )
}

#[test]
fn tpl_2() {
    test_from_to(
        "`${Module.wrapper[0]}${script}${Module.wrapper[1]}`",
        "`${Module.wrapper[0]}${script}${Module.wrapper[1]}`;",
    );
}

#[test]
fn tpl_escape_1() {
    test_from_to(
        "`${parent.path}\x00${request}`",
        "`${parent.path}\x00${request}`;",
    )
}

#[test]
fn tpl_escape_2() {
    test_from_to("`${arg}\0`", "`${arg}\0`;");
}

#[test]
fn tpl_escape_3() {
    test_from_to(
        r#"`${resolvedDevice.toLowerCase()}\\`"#,
        r#"`${resolvedDevice.toLowerCase()}\\`;"#,
    );
}

#[test]
fn tpl_escape_4() {
    test_from_to(
        r#"`\\\\${firstPart}\\${path.slice(last)}`"#,
        r#"`\\\\${firstPart}\\${path.slice(last)}`;"#,
    );
}

#[test]
fn tpl_escape_5() {
    test_from_to(
        r#"const data = text.encode(`${arg}\0`);"#,
        r#"const data = text.encode(`${arg}\0`);"#,
    );
}

#[test]
fn tpl_escape_6() {
    let from = r#"export class MultipartReader {
    newLine = encoder.encode("\r\n");
    newLineDashBoundary = encoder.encode(`\r\n--${this.boundary}`);
    dashBoundaryDash = encoder.encode(`--${this.boundary}--`);
}"#;
    let to = r#"export class MultipartReader {
    newLine = encoder.encode("\r\n");
    newLineDashBoundary = encoder.encode(`\r\n--${this.boundary}`);
    dashBoundaryDash = encoder.encode(`--${this.boundary}--`);
}"#;

    let out = parse_then_emit(from, Default::default(), Syntax::Es(EsConfig::default()));
    assert_eq!(DebugUsingDisplay(out.trim()), DebugUsingDisplay(to.trim()),);
}

#[test]
fn issue_915_1() {
    test_identical(r#"relResolveCacheIdentifier = `${parent.path}\x00${request}`;"#);
}

#[test]
fn issue_915_2() {
    test_identical(r#"relResolveCacheIdentifier = `${parent.path}\x00${request}`;"#);
}

#[test]
fn issue_915_3() {
    test_identical(r#"encoder.encode("\\r\\n");"#);
}

#[test]
fn issue_915_4() {
    test_identical(r#"`\\r\\n--${this.boundary}`;"#);
}

#[test]
fn deno_8162() {
    test_from_to(
        r#""\x00\r\n\x85\u2028\u2029";"#,
        "\"\\0\\r\\n\u{0085}\\u2028\\u2029\";",
    );
}

#[test]
fn integration_01() {
    test_from_to(
        r#"
    `Unexpected ${unexpectedKeys.length > 1 ? 'keys' : 'key'} ` +
    `"${unexpectedKeys.join('", "')}" found in ${argumentName}. ` +
    `Expected to find one of the known reducer keys instead: ` +
    `"${reducerKeys.join('", "')}". Unexpected keys will be ignored.`
    "#,
        "
    `Unexpected ${unexpectedKeys.length > 1 ? \"keys\" : \"key\"} ` + `\"${unexpectedKeys.join('\", \
         \"')}\" found in ${argumentName}. ` + `Expected to find one of the known reducer keys \
         instead: ` + `\"${reducerKeys.join('\", \"')}\". Unexpected keys will be ignored.`;
        ",
    );
}

#[test]
fn integration_01_reduced_01() {
    test_from_to(
        r#"
    `Unexpected ${unexpectedKeys.length > 1 ? 'keys' : 'key'} ` +
    `"${unexpectedKeys.join('", "')}" found in ${argumentName}. `
    "#,
        "
    `Unexpected ${unexpectedKeys.length > 1 ? \"keys\" : \"key\"} ` + `\"${unexpectedKeys.join('\", \
         \"')}\" found in ${argumentName}. `;",
    );
}

#[test]
fn deno_8541_1() {
    test_from_to(
        "React.createElement('span', null, '\\u{b7}');",
        "React.createElement(\"span\", null, \"\u{b7}\");",
    );
}

#[test]
fn deno_8925() {
    assert_pretty("const 𝒫 = 2;", "const 𝒫 = 2;");
}

#[test]
#[ignore = "Tested by a bundler test"]
fn deno_9620() {
    assert_pretty(
        "const content = `--------------------------366796e1c748a2fb\r
Content-Disposition: form-data; name=\"payload\"\r
Content-Type: text/plain\r
\r
CONTENT\r
--------------------------366796e1c748a2fb--`",
        "`const content = `--------------------------366796e1c748a2fb\\r\\nContent-Disposition: \
         form-data; name=\"payload\"\\r\\nContent-Type: \
         text/plain\\r\\n\\r\\nCONTENT\\r\\n--------------------------366796e1c748a2fb--`;",
    );
}

#[test]
fn deno_8541_2() {
    test_from_to(
        "React.createElement('span', null, '\\u00b7');",
        "React.createElement(\"span\", null, \"\u{00b7}\");",
    );
}

#[test]
fn issue_1452_1() {
    assert_min("async foo => 0", "async foo=>0");
}

#[test]
fn issue_1619_1() {
    assert_min_target(
        "\"\\x00\" + \"\\x31\"",
        "\"\\0\"+\"1\"",
        EsVersion::latest(),
    );
}

#[test]
fn issue_1619_2() {
    assert_min_target(
        "\"\\x00\" + \"\\x31\"",
        "\"\\0\"+\"1\"",
        EsVersion::latest(),
    );
}

#[test]
fn issue_1619_3() {
    assert_eq!(get_quoted_utf16("\x00\x31", EsVersion::Es3), "\"\\x001\"");
}

// fn check_latest(src: &str, expected: &str) {
//     let actual = parse_then_emit(
//         src,
//         Config {
//             minify: false,
//             target: EsVersion::latest(),
//             ..Default::default()
//         },
//         Default::default(),
//     );
//     assert_eq!(expected, actual.trim());
// }

// #[test]
// fn invalid_unicode_in_ident() {
//     check_latest("\\ud83d;", "\\ud83d;");
// }

// #[test]
// fn test_escape_with_source_str() {
//     check_latest("'\\ud83d'", "'\\ud83d';");
//     check_latest(
//         "'\\ud83d\\ud83d\\ud83d\\ud83d\\ud83d'",
//         "'\\ud83d\\ud83d\\ud83d\\ud83d\\ud83d';",
//     );
// }

#[derive(PartialEq, Eq)]
struct DebugUsingDisplay<'a>(&'a str);

impl Debug for DebugUsingDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.0, f)
    }
}

#[cfg(test)]
mod parens {
    fn run_test(from: &str, to: &str) {
        super::assert_min(from, to);
    }

    macro_rules! test_fixer {
        ($name:ident, $from:literal, $to:literal) => {
            #[test]
            fn $name() {
                run_test($from, $to);
            }
        };
    }

    macro_rules! identical {
        ($name:ident, $src:literal) => {
            test_fixer!($name, $src, $src);
        };
    }

    identical!(fn_expr_position, r#"foo(function(){}())"#);

    identical!(fn_decl, r#"function foo(){}"#);

    identical!(iife, r#"(function(){})()"#);

    identical!(paren_seq_arg, "foo((_temp=_this=init(),_temp))");

    test_fixer!(
        regression_01,
        "_set(_get_prototype_of(Obj.prototype), _ref = proper.prop, (_superRef = \
         +_get(_get_prototype_of(Obj.prototype), _ref, this)) + 1, this, true), _superRef;",
        "_set(_get_prototype_of(Obj.prototype),_ref=proper.prop,(_superRef=+_get(_get_prototype_of(Obj.prototype),_ref,this))+1,this,true),_superRef"
    );

    test_fixer!(
        regression_02,
        "var obj = (_obj = {}, _define_property(_obj, 'first', 'first'), _define_property(_obj, \
         'second', 'second'), _obj);",
        r#"var obj=(_obj={},_define_property(_obj,"first","first"),_define_property(_obj,"second","second"),_obj)"#
    );

    identical!(
        regression_03,
        "_iteratorNormalCompletion=(_step=_iterator.next()).done"
    );

    test_fixer!(
        regression_04,
        "var _tmp;
const _ref = {}, { c =( _tmp = {}, d = _extends({}, _tmp), _tmp)  } = _ref;",
        "var _tmp;const _ref={},{c=(_tmp={},d=_extends({},_tmp),_tmp)}=_ref"
    );

    test_fixer!(
        regression_05,
        "for (var _iterator = arr[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step \
         = _iterator.next()).done); _iteratorNormalCompletion = true) {
    i = _step.value;
}",
        "for(var _iterator=arr[Symbol.iterator](),_step;!(_iteratorNormalCompletion=(_step=_iterator.next()).done);_iteratorNormalCompletion=true)i=_step.value"
    );

    test_fixer!(
        regression_06,
        "
        var _tmp;
        const { [( _tmp = {}, d = _extends({}, _tmp), _tmp)]: c  } = _ref;
        ",
        "var _tmp;const{[(_tmp={},d=_extends({},_tmp),_tmp)]:c}=_ref"
    );

    identical!(
        regression_07,
        "(_temp=super(),_initialize(this),_temp).method()"
    );

    identical!(regression_08, "exports.bar=exports.default=void 0");

    identical!(regression_09, "({x}={x:1})");

    identical!(regression_10, "({x}={x:1}),exports.x=x");

    identical!(regression_11, "(void 0).foo()");

    identical!(regression_12, "(function(){})()");

    identical!(regression_13, "a||(a=1)");

    identical!(issue_192, "a===true&&(a=true)");

    identical!(issue_199, "(i-1).toString()");

    test_fixer!(
        issue_201_01,
        "outer = {
    inner: (_obj = {}, _define_property(_obj, ns.EXPORT1, true), _define_property(_obj, \
         ns.EXPORT2, true), _obj)
};",
        "outer={inner:(_obj={},_define_property(_obj,ns.EXPORT1,true),_define_property(_obj,ns.EXPORT2,true),_obj)}"
    );

    identical!(issue_207, "a=>({x:\"xxx\",y:{a}})");

    test_fixer!(
        fixer_01,
        "var a, b, c, d, e, f;
((a, b), (c())) + ((d, e), (f()));
",
        "var a,b,c,d,e,f;(a,b,c())+(d,e,f())"
    );

    test_fixer!(fixer_02, "(b, c), d;", "b,c,d");

    test_fixer!(fixer_03, "((a, b), (c && d)) && e;", "(a,b,c&&d)&&e");

    test_fixer!(fixer_04, "for ((a, b), c;;) ;", "for(a,b,c;;);");

    test_fixer!(
        fixer_05,
        "var a, b, c = (1), d, e, f = (2);
((a, b), c) + ((d, e), f);",
        "var a,b,c=1,d,e,f=2;(a,b,c)+(d,e,f)"
    );

    test_fixer!(
        fixer_06,
        "var a, b, c, d;
a = ((b, c), d);",
        "var a,b,c,d;a=(b,c,d)"
    );

    test_fixer!(
        fixer_07,
        "a => ((b, c) => ((a, b), c));",
        "a=>(b,c)=>(a,b,c)"
    );

    test_fixer!(fixer_08, "typeof (((1), a), (2));", "typeof(1,a,2)");

    test_fixer!(fixer_09, "(((a, b), c), d) ? e : f;", "(a,b,c,d)?e:f");

    test_fixer!(
        fixer_10,
        "
function a() {
  return (((void (1)), (void (2))), a), (void (3));
}
",
        "function a(){return void 1,void 2,a,void 3}"
    );

    test_fixer!(fixer_11, "c &&((((2), (3)), d), b);", "c&&(2,3,d,b)");

    test_fixer!(fixer_12, "(((a, b), c), d) + e;", "(a,b,c,d)+e");

    test_fixer!(fixer_13, "delete (((1), a), (2));", "delete(1,a,2)");

    test_fixer!(fixer_14, "(1, 2, a)", "1,2,a");

    identical!(issue_231, r#"""+(truthy&&"?")+truthy"#);

    identical!(issue_252, "!!(a&&b)");

    test_fixer!(
        issue_255,
        "b < 0 ? (t = b, b = 1) : (t = -b, b = 0);",
        "b<0?(t=b,b=1):(t=-b,b=0)"
    );

    test_fixer!(
        issue_266_1,
        "'Q' + +x1 + ',' + +y1 + ',' + (this._x1 = +x) + ',' + (this._y1 = +y);",
        r#""Q"+ +x1+","+ +y1+","+(this._x1=+x)+","+(this._y1=+y)"#
    );

    test_fixer!(
        issue_266_2,
        "'Q' + (+x1) + ',' + (+y1) + ',' + (this._x1 = +x) + ',' + (this._y1 = +y);",
        r#""Q"+ +x1+","+ +y1+","+(this._x1=+x)+","+(this._y1=+y)"#
    );

    test_fixer!(
        issue_280,
        "e.hasOwnProperty(a) && (t = e[a] ? this[a] = t(n) : 'target' === a ? this.target = r : \
         this[a] = n[a]);",
        r#"e.hasOwnProperty(a)&&(t=e[a]?this[a]=t(n):"target"===a?this.target=r:this[a]=n[a])"#
    );

    test_fixer!(
        issue_282,
        "!(A = [], B = (function () { return classNames; }).apply(exports, A), B !== undefined && \
         (module.exports = B));",
        "!(A=[],B=(function(){return classNames}).apply(exports,A),B!==undefined&&(module.exports=B))"
    );

    test_fixer!(
        issue_293_1,
        "for (var e in a) a.hasOwnProperty(e) && ((b = a[e]) ? this[e] = b(c) : 'target' === e ? \
         this.target = d : this[e] = c[e]);",
        r#"for(var e in a)a.hasOwnProperty(e)&&((b=a[e])?this[e]=b(c):"target"===e?this.target=d:this[e]=c[e])"#
    );

    test_fixer!(
        issue_293_2,
        "(a = rb ? zb(a, c) : Ab(a, c)) ? (b = nb.getPooled(ub.beforeInput, b, c, d), b.data = a, \
         Ra(b)) : b = null;",
        "(a=rb?zb(a,c):Ab(a,c))?(b=nb.getPooled(ub.beforeInput,b,c,d),b.data=a,Ra(b)):b=null"
    );

    identical!(member_object_lit, "({}).foo");

    identical!(member_cond_expr, "(foo?1:2).foo");

    identical!(member_new_exp_1, "(new Foo).foo");

    identical!(member_new_exp_2, "new ctor().property");

    identical!(member_tagged_tpl, "tag``.foo");

    identical!(member_arrow_expr_1, "(a=>a).foo");

    identical!(member_class, "(class Foo{}).foo");

    identical!(member_yield, "function*foo(){(yield bar).baz}");

    identical!(member_await, "async function foo(){(await bar).baz}");

    identical!(bin_yield_expr_1, "function*foo(){(yield foo)&&bar}");

    identical!(bin_yield_expr_2, "function*foo(){bar&&(yield foo)}");

    identical!(bin_seq_expr_1, "(foo(),op)||(seq(),foo)");

    identical!(bin_seq_expr_2, "(foo,op)||(seq,foo)");

    identical!(cond_object_1, "let foo={}?1:2");

    identical!(cond_object_2, "({})?1:2");

    identical!(cond_in_cond, "(foo?1:2)?3:4");

    identical!(arrow_in_cond, "(()=>{})?3:4");

    identical!(unary_cond_arg, "void(foo?1:2)");

    identical!(unary_arrow_arg, "void(foo=>foo)");

    identical!(unary_yield_arg, "(function* foo(){void(yield foo)})()");

    test_fixer!(
        issue_365,
        "const foo = (() => {
  return 1
})();",
        "const foo=(()=>1)()"
    );

    test_fixer!(
        issue_382_1,
        "const myFilter=(arr,filter)=>arr.filter(((x)=>x)||filter)",
        "const myFilter=(arr,filter)=>arr.filter((x=>x)||filter)"
    );

    test_fixer!(
        issue_382_2,
        "const myFilter=(arr,filter)=>arr.filter(filter||((x)=>x))",
        "const myFilter=(arr,filter)=>arr.filter(filter||(x=>x))"
    );

    identical!(issue_418, "const a=1-(1-1)");

    test_fixer!(
        issue_439,
        "() => {
  return (
    Promise.resolve('foo')
      // Interfering comment
      .then(() => {})
  );
};",
        r#"()=>Promise.resolve("foo").then(()=>{})"#
    );

    test_fixer!(void_and_bin, "(void 0) * 2", "(void 0)*2");

    test_fixer!(new_cond, "new (a ? B : C)()", "new(a?B:C)");

    test_fixer!(issue_931, "new (eval(\"Date\"))()", "new(eval(\"Date\"))");

    test_fixer!(issue_1002, "new (P||(P=Promise))", "new(P||(P=Promise))");

    test_fixer!(
        issue_1050,
        "(a) => (set) => (elemE(a, set) ? removeE : insertE)(a)(set)",
        "a=>set=>(elemE(a,set)?removeE:insertE)(a)(set)"
    );

    test_fixer!(
        deno_001,
        "
    var Status;
    (function init(Status1) {
    })(Status || (Status = {
    }));
",
        "var Status;(function init(Status1){})(Status||(Status={}))"
    );

    identical!(issue_1093, "const x=(fnA||fnB)()");

    test_fixer!(
        issue_1133,
        "async function foo() {
            const item = await (data === null || data === void 0 ? void 0 : data.foo());
        }",
        "async function foo(){const item=await(data===null||data===void 0?void 0:data.foo())}"
    );

    identical!(deno_8722, "console.log((true||false)??true)");

    test_fixer!(
        deno_8597,
        "biasInitializer =new(_a=class CustomInit extends Initializer{})()",
        "biasInitializer=new(_a=class CustomInit extends Initializer{})"
    );

    test_fixer!(
        minifier_001,
        "var bitsLength = 3, bitsOffset = 3, what = (len = 0)",
        "var bitsLength=3,bitsOffset=3,what=len=0"
    );

    test_fixer!(minifier_002, "!(function(){})()", "!function(){}()");

    identical!(issue_1397, "const main=async()=>await(await server)()");

    identical!(deno_9810, "await(bar=Promise.resolve(2))");

    identical!(issue_1493, r#"("a"??"b")||"""#);
    identical!(call_seq, "let x=({},()=>2)()");

    test_fixer!(
        call_seq_with_padding,
        "let x=({},(1, 2), () => 2)();",
        "let x=({},1,2,()=>2)()"
    );

    test_fixer!(
        param_seq,
        "function t(x = ({}, 2)) {
            return x;
        }",
        "function t(x=({},2)){return x}"
    );

    test_fixer!(
        yield_expr_cond,
        "function *test1(foo) {
            return (yield foo) ? 'bar' : 'baz';
        }",
        "function*test1(foo){return(yield foo)?\"bar\":\"baz\"}"
    );

    identical!(
        deno_10487_1,
        "var generator=class MultiVector extends(options.baseType||Float32Array){}"
    );

    identical!(
        deno_10487_2,
        "class MultiVector extends(options.baseType||Float32Array){}"
    );

    identical!(
        extends_nullish_coalescing,
        "class Foo extends(Bar??class{}){}"
    );

    identical!(extends_assign, "class Foo extends(Bar=class{}){}");

    identical!(
        extends_logical_or_assin,
        "class Foo extends(Bar||=class{}){}"
    );

    identical!(
        extends_logical_and_assin,
        "class Foo extends(Bar&&=class{}){}"
    );

    identical!(
        extends_logical_nullish_assin,
        "class Foo extends(Bar??=class{}){}"
    );

    identical!(extends_cond, "class Foo extends(true?Bar:Baz){}");

    test_fixer!(
        extends_await_yield,
        "
        async function* func() {
            class A extends (await p) {}
            class B extends (yield p) {}
        }
        ",
        "async function*func(){class A extends(await p){}class B extends(yield p){}}"
    );

    identical!(deno_10668_1, "console.log(null??(undefined&&true))");

    identical!(deno_10668_2, "console.log(null&&(undefined??true))");

    identical!(minifier_003, "(four**one)**two");

    identical!(minifier_004, "(void 0)(0)");

    identical!(issue_1781, "const n=~~(Math.PI*10)");

    identical!(issue_1789, "+(+1/4)");

    test_fixer!(
        new_member_call_1,
        "new (getObj()).ctor()",
        "new(getObj()).ctor"
    );
    test_fixer!(
        new_member_call_2,
        "new (getObj().ctor)()",
        "new(getObj()).ctor"
    );
    test_fixer!(
        new_member_call_3,
        "new (x.getObj().ctor)()",
        "new(x.getObj()).ctor"
    );
    identical!(new_call, "new(getCtor())");
    test_fixer!(new_member_1, "new obj.ctor()", "new obj.ctor");
    test_fixer!(new_member_2, "new (obj.ctor)", "new obj.ctor");

    identical!(
        new_await_1,
        "async function foo(){new(await getServerImpl())(options)}"
    );
    test_fixer!(minifier_005, "-(1/0)", "-1/0");

    test_fixer!(minifier_006, "-('s'/'b')", r#"-("s"/"b")"#);

    test_fixer!(minifier_007, "(void 0) === value", "void 0===value");
    test_fixer!(minifier_008, "(size--) && (b = (c))", "size--&&(b=c)");

    test_fixer!(
        minifier_009,
        "(--remaining) || deferred.resolveWith()",
        "--remaining||deferred.resolveWith()"
    );

    test_fixer!(minifier_010, "(--remaining) + ''", "--remaining+\"\"");

    test_fixer!(
        issue_2155,
        "
        async function main() {
            let promise;
            await (promise || (promise = Promise.resolve('this is a string')));
        }
        ",
        r#"async function main(){let promise;await(promise||(promise=Promise.resolve("this is a string")))}"#
    );

    identical!(issue_2163_1, "()=>({foo}=bar())");

    identical!(issue_2163_2, "()=>([foo]=bar())");

    identical!(issue_2191, "(-1)**h");

    test_fixer!(issue_2550_1, "(1 && { a: 1 })", "1&&{a:1}");

    identical!(issue_2550_2, "({isNewPrefsActive})&&{a:1}");

    test_fixer!(paren_of_bin_left_1, "({} && 1)", "({})&&1");
    identical!(paren_of_bin_left_2, "({})&&1");
    test_fixer!(
        paren_of_bin_left_3,
        "(function () {} || 2)",
        "(function(){})||2"
    );
    identical!(paren_of_bin_left_4, "(function(){})||2");

    test_fixer!(paren_of_bin_left_5, "(class{} ?? 3)", "(class{})??3");
    identical!(paren_of_bin_left_6, "(class{})??3");

    identical!(issue_4761, "x={...(0,foo)}");

    identical!(issue_4914, "(a??b)?.()");

    identical!(issue_5109_1, "(0,b)?.()");
    identical!(issue_5109_2, "1+(0,b)?.()");
    identical!(issue_5109_3, "(0,a)()?undefined:(0,b)?.()");

    test_fixer!(
        issue_5313,
        "
        async function* foo() {
            (await a)();
            (yield b)();
        }
        ",
        "async function*foo(){(await a)();(yield b)()}"
    );

    identical!(issue_5417, "console.log(a??b??c)");

    identical!(bin_and_unary, "console.log(a++&&b--)");

    test_fixer!(
        new_fn_call,
        "const instance = new (
            function() {}()
        )();",
        "const instance=new(function(){}())"
    );
}
