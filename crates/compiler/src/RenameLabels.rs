use atoms::JsWord;
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::util::take::Take;
use rustc_hash::FxHashMap;

use crate::DefaultNameGenerator::DefaultNameGenerator;

pub fn process(ast: &mut ast::Program) {
    let mut visitor = RenameLabels::default();
    ast.visit_mut_with(&mut visitor);
}

// TODO: change they span ctxt when changing the sym

/// RenameLabels renames all the labels so that they have short names, to reduce
/// code size and also to obfuscate the code.
///
/// Label names have a unique namespace, so variable or function names clashes
/// are not a concern, but keywords clashes are.
///
/// Additionally, labels names are only within the statements include in the
/// label and do not cross function boundaries. This means that it is possible to
/// create one label name that is used for labels at any given depth of label
/// nesting. Typically, the name "a" will be used for all top-level labels, "b"
/// for the next nested label, and so on. For example:
///
/// ```js
/// function bar() {
///   a: {
///     b: {
///       foo();
///     }
///   }
///
///   a: {
///     b: break a;
///   }
/// }
/// ```
///
/// The name for the label is selected during the decent into the AST, and the
/// references to the label name are renamed as they are encountered on the way
/// back up. This means that whether a label is unreferenced, and can therefore
/// be safely removed, is known when the label node is visited.
#[derive(Default)]
struct RenameLabels {
    name_supplier: DefaultNameGenerator,
    // A stack of labels namespaces. Labels in an outer scope aren't part of an
    // inner scope, so a new namespace is created each time a scope is entered.
    namespace_stack: Vec<LabelNamespace>,
    // The list of generated names. Typically, the first name will be "a",
    // the second "b", etc.
    names: Vec<JsWord>,
}

impl RenameLabels {
    /// Returns the short name of the identified label.
    ///
    /// `id` - The id, which is the depth of the label in the current context,
    /// for which to get a short name.
    fn get_name_for_id(&self, id: usize) -> &JsWord {
        &self.names[id - 1]
    }

    /// Returns the structure representing the name in the current context.
    ///
    /// `name` - The name to retrieve information about.
    fn get_label_info(&mut self, name: &JsWord) -> Option<&mut LabelInfo> {
        self.namespace_stack
            .last_mut()
            .unwrap()
            .rename_map
            .get_mut(name)
    }
}

macro_rules! handle_scope {
    ([$([$name:ident, $N:ident]$(,)?)*]) => {
        $(
            #[inline]
            fn $name(&mut self, n: &mut ast::$N) {
                // Start a new namespace for label names.
                self.namespace_stack.push(LabelNamespace::default());
                n.visit_mut_children_with(self);
                self.namespace_stack.pop();
            }
        )*
    };
}

impl VisitMut<'_> for RenameLabels {
    fn visit_mut_stmt(&mut self, node: &mut ast::Stmt) {
        node.map_with_mut(|mut stmt| {
            match stmt {
                ast::Stmt::Labeled(mut labeled_stmt) => {
                    // Determine the new name for this label.
                    let current = self.namespace_stack.last_mut().unwrap();
                    let current_depth = current.rename_map.len() + 1;
                    let id = current_depth;
                    let name = &labeled_stmt.label;

                    // Store the context for this label name.
                    let li = LabelInfo {
                        id,
                        referenced: false,
                    };
                    debug_assert!(!current.rename_map.contains_key(&name.sym));
                    current.rename_map.insert(name.sym.clone(), li);

                    // Create a new name, if needed, for this depth.
                    if self.names.len() < current_depth {
                        self.names.push(self.name_supplier.generate_next_name());
                    }

                    labeled_stmt.visit_mut_children_with(self);

                    let label = &mut labeled_stmt.label;
                    let name = label.sym.clone();

                    let li = self.get_label_info(&name).unwrap();
                    // This is a label...
                    let res = if li.referenced {
                        let new_name = self.get_name_for_id(id);
                        if &name != new_name {
                            // ... and it is used, give it the short name.
                            label.sym = new_name.clone();
                        }
                        ast::Stmt::Labeled(labeled_stmt)
                    } else {
                        // ... and it is not referenced, just remove it.
                        *labeled_stmt.body
                    };
                    // Remove the label from the current stack of labels.
                    self.namespace_stack
                        .last_mut()
                        .unwrap()
                        .rename_map
                        .remove(&name);

                    res
                }
                ast::Stmt::Break(ast::BreakStmt { ref mut label, .. })
                | ast::Stmt::Continue(ast::ContinueStmt { ref mut label, .. }) => {
                    if let Some(label) = label {
                        // This is a named break or continue;
                        let name = label.sym.clone();
                        let id = self.get_label_info(&name).map(|info| info.id);
                        if let Some(id) = id {
                            let new_name = self.get_name_for_id(id);
                            if &name != new_name {
                                // Give it the short name.
                                label.sym = new_name.clone();
                            }
                            // Mark the label as referenced so it isn't removed.
                            self.get_label_info(&name).unwrap().referenced = true;
                        }
                    }
                    stmt
                }
                _ => {
                    stmt.visit_mut_children_with(self);
                    stmt
                }
            }
        });
    }

    // Note:
    // Program is not a function; it creates the initial/global scope.
    // ClassMethod, PrivateMethod, and MethodProp are handled by their Function child.
    // Arrow functions do not create new label scopes.
    handle_scope!([
        [visit_mut_program, Program],
        [visit_mut_function, Function],
        [visit_mut_constructor, Constructor],
        [visit_mut_setter_prop, SetterProp],
        [visit_mut_getter_prop, GetterProp],
    ]);
}

#[derive(Default, Debug)]
struct LabelNamespace {
    rename_map: FxHashMap<JsWord, LabelInfo>,
}

#[derive(Debug)]
struct LabelInfo {
    referenced: bool,
    id: usize,
}

#[cfg(test)]
mod tests {
    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, _| {
                super::process(&mut program);
                program
            },
            input,
            expected,
        );
    }

    #[test]
    fn test_class_methods_create_scopes() {
        test_transform(
            "
class C {
    constructor() {
        Foo:{ 
            function f() {
                Foo: {
                    1; break Foo; 
                }
            }
        }
    }
    a() {
        Foo:{ 
            function f() {
                Foo: {
                    1; break Foo; 
                }
            }
        }
    }
}",
            "
class C {
    constructor() {
        { 
            function f() {
                a: {
                    1; break a; 
                }
            }
        }
    }
    a() {
        { 
            function f() {
                a: {
                    1; break a; 
                }
            }
        }
    }
}",
        );
    }

    #[test]
    fn test_object_methods_create_scopes() {
        test_transform(
            "
const C = {
    a() {
        Foo:{ 
            function f() {
                Foo: {
                    1; break Foo; 
                }
            }
        }
    },
    get b() {
        Foo:{ 
            function f() {
                Foo: {
                    1; break Foo; 
                }
            }
        }
    },
    set b(p) {
        Foo:{ 
            function f() {
                Foo: {
                    1; break Foo; 
                }
            }
        }
    }
}",
            "
const C = {
    a() {
        { 
            function f() {
                a: {
                    1; break a; 
                }
            }
        }
    },
    get b() {
        { 
            function f() {
                a: {
                    1; break a; 
                }
            }
        }
    },
    set b(p) {
        { 
            function f() {
                a: {
                    1; break a; 
                }
            }
        }
    }
}",
        );
    }

    // =================================================================
    // Tests from closure
    // =================================================================

    #[test]
    fn testRenameInFunction() {
        test_transform("function x(){ Foo:a(); }", "function x(){ a(); }");

        test_transform(
            "function x(){ Foo:{ a(); break Foo; } }",
            "function x(){ a:{ a(); break a; } }",
        );

        test_transform(
            "
function x() {
    Foo:{ 
        function goo() {
            Foo: {
                a(); 
                break Foo; 
            }
        }
    }
}",
            "function x(){{function goo(){a:{ a(); break a; }}}}",
        );

        test_transform(
            "
function x() {
    Foo:{ 
        function goo() {
            Foo: {
                a(); 
                break Foo; 
            }
        }
        break Foo;
    }
}",
            "function x(){a:{function goo(){a:{ a(); break a; }} break a;}}",
        );
    }

    #[test]
    fn testRenameForArrowFunction() {
        //remove label that is not referenced
        test_transform("() => { Foo:a(); } ", "() => {     a(); }");

        test_transform("Foo:() => { a(); }", "    () => { a(); }");

        //label is referenced
        test_transform(
            "() => { Foo:{ a(); break Foo; } }",
            "() => {   a:{ a(); break   a; } }",
        );
    }

    #[test]
    fn testRenameForOf() {
        test_transform(
            "
loop:
for (let x of [1, 2, 3]) {
    if (x > 2) {
    break loop;
    }
}",
            "
a:
for (let x of [1, 2, 3]) {
    if (x > 2) {
    break a;
    }
}",
        );
    }

    #[test]
    fn testRenameGlobals() {
        test_transform("Foo:{a();}", "{a();}");
        test_transform("Foo:{a(); break Foo;}", "a:{a(); break a;}");
        test_transform("Foo:{Goo:a(); break Foo;}", "a:{a(); break a;}");
        test_transform(
            "Foo:{Goo:while(1){a(); continue Goo; break Foo;}}",
            "a:{b:while(1){a(); continue b;break a;}}",
        );
        test_transform(
            "Foo:Goo:while(1){a(); continue Goo; break Foo;}",
            "a:b:while(1){a(); continue b;break a;}",
        );

        test_transform("Foo:Bar:X:{ break Bar; }", "b:{ break b; }");
        test_transform(
            "Foo:Bar:X:{ break Bar; break X; }",
            "b:c:{ break b; break c;}",
        );
        test_transform(
            "Foo:Bar:X:{ break Bar; break Foo; }",
            "a:b:{ break b; break a;}",
        );

        test_transform("Foo:while (1){a(); break;}", "while (1){a(); break;}");

        // Remove label that is not referenced.
        test_transform("Foo:{a(); while (1) break;}", "{a(); while (1) break;}");
    }

    #[test]
    fn testRenameReused() {
        test_transform(
            "foo:{break foo}; foo:{break foo}",
            "a:{break a};a:{break a}",
        );
    }
}
