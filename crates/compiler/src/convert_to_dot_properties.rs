use ast::*;
use atoms::js_word;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::SyntaxContext;

pub fn process(ast: &mut Program, program_data: &mut ProgramData) {
    let mut visitor = Visitor { program_data };
    ast.visit_mut_with(&mut visitor);
}

struct Visitor<'a> {
    program_data: &'a mut ProgramData,
}

impl Visitor<'_> {
    fn handle_prop_name(&mut self, prop: &mut PropName, is_class_prop: bool) {
        prop.visit_mut_children_with(self);

        // TODO: can also handle a[undefined] -> a.undefined, a[NaN] -> a.NaN, and a[null] -> a.null

        let (prop_name, old_node_id) = match &prop {
            PropName::Str(prop_name) => (prop_name, prop_name.node_id),
            PropName::Computed(computed_prop) => match computed_prop.expr.as_ref() {
                Expr::Lit(Lit::Str(prop_name)) => (prop_name, computed_prop.node_id),
                _ => return,
            },
            _ => return,
        };

        // ['constructor']() and constructor() define different things in
        // classes, so we can't substitute them.
        if is_class_prop && prop_name.value == js_word!("constructor") {
            return;
        }
        if is_valid_prop_ident(&prop_name.value) {
            *prop = PropName::Ident(Ident {
                ctxt: SyntaxContext::empty(),
                node_id: self.program_data.new_id_from(old_node_id),
                sym: prop_name.value.clone(),
            });
        }
    }
}

impl VisitMut<'_> for Visitor<'_> {
    fn visit_mut_member_expr(&mut self, n: &mut MemberExpr) {
        n.visit_mut_children_with(self);
        // TODO: can also handle a[undefined] -> a.undefined, a[NaN] -> a.NaN, and a[null] -> a.null
        if let Expr::Lit(Lit::Str(prop)) = n.prop.as_ref() {
            if is_valid_prop_ident(&prop.value) {
                n.computed = false;
                n.prop = Box::new(Expr::Ident(Ident {
                    ctxt: SyntaxContext::empty(),
                    node_id: self.program_data.new_id_from(prop.node_id),
                    sym: prop.value.clone(),
                }));
            }
        }
    }

    fn visit_mut_class(&mut self, n: &mut Class) {
        n.extends.visit_mut_with(self);

        for member in n.body.iter_mut() {
            match member {
                ClassMember::Method(member) => {
                    member.function.visit_mut_with(self);
                    self.handle_prop_name(&mut member.key, true);
                }
                ClassMember::ClassProp(member) => {
                    member.value.visit_mut_with(self);
                    self.handle_prop_name(&mut member.key, true);
                }

                ClassMember::Constructor(_)
                | ClassMember::PrivateMethod(_)
                | ClassMember::PrivateProp(_) => {
                    member.visit_mut_with(self);
                }
            }
        }
    }

    fn visit_mut_prop_name(&mut self, n: &mut PropName) {
        self.handle_prop_name(n, false);
    }
}

pub fn is_valid_prop_ident(s: &str) -> bool {
    s.starts_with(Ident::is_valid_start) && s.chars().all(Ident::is_valid_continue)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolver::resolver;
    use global_common::{Globals, Mark, GLOBALS};

    #[test]
    fn test_valid_computed_prop_accesses() {
        test_transform("a['p']", "a.p");
        test_transform("a['_p_']", "a._p_");
        test_transform("a['_']", "a._");
        test_transform("a['$']", "a.$");
        test_transform("a.b.c['p']", "a.b.c.p");
        test_transform("a.b['c'].p", "a.b.c.p");
        test_transform("a['p']();", "a.p();");
        test_transform("a()['p']", "a().p");
        test_transform("a['\u{0041}A']", "a.AA");
        test_transform("a['aøπœ']", "a.aøπœ");
        test_transform("a['break']", "a.break");
        test_transform("a['constructor']", "a.constructor");
    }

    #[test]
    fn test_invalid_computed_prop_accesses() {
        test_same("a[0]");
        test_same("a['']");
        test_same("a[' ']");
        test_same("a[',']");
        test_same("a[';']");
        test_same("a[':']");
        test_same("a['.']");
        test_same("a['0']");
        test_same("a['p ']");
        test_same("a['p' + '']");
        test_same("a[p]");
        test_same("a[P]");
        test_same("a[$]");
        test_same("a[p()]");
    }

    #[test]
    fn test_already_dotted() {
        test_same("a.b");
        test_same("var a = {b: 0};");
    }

    #[test]
    fn test_invalid_prop_definitions() {
        test_same("({'':0})");
        test_same("({'1.0':0})");
        test_same("({'❤️':0})");
        // Some wacky ones from closure compiler.
        test_same("var a = { '$\\\\' : 5 };");
        test_same("var a = { 'x\\\\u0041$\\\\' : 5 };");
    }

    #[test]
    fn test_optional_chaining() {
        test_transform("data?.['name']", "data?.name");
        test_transform("data?.['name']?.['first']", "data?.name?.first");
        test_transform("data['name']?.['first']", "data.name?.first");
        test_transform("a?.['break']", "a?.break");

        test_same("a?.[0]");
        test_same("a?.['']");
        test_same("a?.[' ']");
        test_same("a?.[',']");
        test_same("a?.[';']");
        test_same("a?.[':']");
        test_same("a?.['.']");
        test_same("a?.['0']");
        test_same("a?.['p ']");
        test_same("a?.['p' + '']");
        test_same("a?.[p]");
        test_same("a?.[P]");
        test_same("a?.[$]");
        test_same("a?.[p()]");

        test_transform(
            "const opt1 = window?.a?.['b'];",
            "const opt1 = window?.a?.b;",
        );

        test_transform("const opt2 = window?.a['b'];", "const opt2 = window?.a.b;");
        test_transform(
            "
const chain =
window['a'].x.y.b.x.y['c'].x.y?.d.x.y['e'].x.y
['f-f'].x.y?.['g-g'].x.y?.['h'].x.y['i'].x.y;
",
            "
const chain = window.a.x.y.b.x.y.c.x.y?.d.x.y.e.x.y
['f-f'].x.y?.['g-g'].x.y?.h.x.y.i.x.y;
",
        );
    }

    #[test]
    fn test_computed_props_and_fields() {
        test_transform("const test1 = {['prop1']:87};", "const test1 = {prop1:87};");
        test_transform(
            "const test1 = {['prop1']:87,['prop2']:bg,['prop3']:'hfd'};",
            "const test1 = {prop1:87,prop2:bg,prop3:'hfd'};",
        );
        test_transform(
            "o = {['x']: async function(x) { return await x + 1; }};",
            "o = {x:async function (x) { return await x + 1; }};",
        );
        test_transform("o = {['x']: function*(x) {}};", "o = {x: function*(x) {}};");
        test_transform(
            "o = {['x']: async function*(x) { return await x + 1; }};",
            "o = {x:async function*(x) { return await x + 1; }};",
        );
        test_transform("class C {'x' = 0;  ['y'] = 1;}", "class C { x= 0;y= 1;}");
        test_transform("class C {'m'() {} }", "class C {m() {}}");

        test_transform(
            "const o = {'b'() {}, ['c']() {}};",
            "const o = {b() {}, c(){}};",
        );
        test_transform("o = {['x']: () => this};", "o = {x: () => this};");

        test_transform("const o = {get ['d']() {}};", "const o = {get d() {}};");
        test_transform("const o = { set ['e'](x) {}};", "const o = { set e(x) {}};");
        test_transform(
            "class C {'m'() {}  ['n']() {} 'x' = 0;  ['y'] = 1;}",
            "class C {m() {}  n() {} x= 0;y= 1;}",
        );
        test_transform(
            "const o = { get ['d']() {},  set ['e'](x) {}};",
            "const o = {get d() {},  set e(x){}};",
        );
        test_transform(
            "const o = {['a']: 1,'b'() {}, ['c']() {},  get ['d']() {},  set ['e'](x) {}};",
            "const o = {a: 1,b() {}, c() {},  get d() {},  set e(x) {}};",
        );

        // test static keyword
        test_transform(
            "
class C {
    'm'(){}
    ['n'](){}
    static 'x' = 0;
    static ['y'] = 1;
}
",
            "
class C {
    m(){}
    n(){}
    static x = 0;
    static y= 1;
}
",
        );
        test_transform(
            "
window['MyClass'] = class {
    static ['Register'](){}
};
",
            "
window.MyClass = class {
    static Register(){}
};
",
        );
        test_transform(
            "
class C {
    'method'(){}
    async ['method1'](){}
    *['method2'](){}
    static ['smethod'](){}
    static async ['smethod1'](){}
    static *['smethod2'](){}
}
",
            "
class C {
    method(){}
    async method1(){}
    *method2(){}
    static smethod(){}
    static async smethod1(){}
    static *smethod2(){}
}
",
        );

        test_same("const o = {[fn()]: 0}");
        test_same("const test1 = {[0]:87};");
        test_transform("const test1 = {['break']:87};", "const test1 = {break:87};");
    }

    #[test]
    fn test_computed_prop_with_default_value() {
        test_transform("const {['o']: o = 0} = {};", "const {o:o = 0} = {};");
    }

    #[test]
    fn test_no_private_props_created() {
        test_same("const x = {['#notAPrivateProp']: 1}");
        test_same("class C {['#notAPrivateProp'] = 1}");
        test_same("const x = {get '#notAPrivateProp'() {}}");
        test_same("class C {get '#notAPrivateProp'() {}}");
        test_same("const x = {set '#notAPrivateProp'(x) {}}");
        test_same("class C {set '#notAPrivateProp'(x) {}}");
        test_same("const x = {'#notAPrivateProp': 1}");
        test_same("x?.['#notAPrivateProp']");
        test_same("x['#notAPrivateProp']");
    }

    #[test]
    fn test_constructor() {
        // ['constructor']() and constructor() define different things in classes...
        test_same("class C { ['constructor']() {} }");
        test_same("class C { ['constructor'] = 0 }");
        // ...but equivalent in objects.
        test_transform("({ ['constructor']: 1 })", "({ constructor: 1 })");
    }

    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, program_data| {
                GLOBALS.set(&Globals::new(), || {
                    let unresolved_mark = Mark::new();
                    let top_level_mark = Mark::new();

                    program.visit_mut_with(&mut resolver(unresolved_mark, top_level_mark));

                    process(&mut program, program_data);

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
