use ast::{self, op, NodeId};
use ecma_visit::{VisitMut, VisitMutWith};
use global_common::{util::take::Take, DUMMY_SP};
use global_visit::util::move_map::MoveMap;

use crate::colors::{color_registry::ColorRegistry, ColorId};

pub fn transform_param_props(
    ast: &mut ast::Program,
    node_id_gen: &mut ast::NodeIdGen,
    colours: Option<&mut ColorRegistry>,
) {
    let mut visitor = ParamPropTransformer {
        node_id_gen,
        colours,
    };

    ast.visit_mut_with(&mut visitor);
}

struct ParamPropTransformer<'a> {
    node_id_gen: &'a mut ast::NodeIdGen,
    colours: Option<&'a mut ColorRegistry>,
}

impl ParamPropTransformer<'_> {
    fn get_color_of_node(&self, node_id: NodeId) -> Option<ColorId> {
        self.colours
            .as_ref()
            .and_then(|colours| colours.get_color_of_node(node_id))
    }
}

impl ParamPropTransformer<'_> {
    fn handle_class(&mut self, class: &mut ast::Class, class_colour: Option<ColorId>) {
        macro_rules! set_colour {
            ($colour:ident, $node:expr) => {{
                let node = $node;
                if let (Some(colours), Some(colour)) = (&mut self.colours, $colour) {
                    colours.set_color_of_node(node.node_id, colour);
                }
                node
            }};
        }

        for member in &mut class.body {
            if let ast::ClassMember::Constructor(c) = member {
                let mut assign_exprs = Vec::new();
                c.params.map_with_mut(|params| {
                    params.move_map(|param| match param {
                        ast::ParamOrTsParamProp::Param(..) => param,
                        ast::ParamOrTsParamProp::TsParamProp(param) => {
                            let (prop_colour, ident_sym, ident_span, param) = match param.param {
                                ast::TsParamPropParam::Ident(i) => {
                                    let colour = self.get_color_of_node(i.id.node_id);
                                    (
                                        colour,
                                        i.id.sym.clone(),
                                        i.id.span,
                                        set_colour!(
                                            colour,
                                            ast::Param {
                                                node_id: self.node_id_gen.next(),
                                                span: DUMMY_SP,
                                                decorators: Default::default(),
                                                pat: ast::Pat::Ident(i),
                                            }
                                        ),
                                    )
                                }
                                ast::TsParamPropParam::Assign(p) => {
                                    let i = match *p.left {
                                        ast::Pat::Ident(i) => i,
                                        _ => unreachable!(
                                            "destructuring pattern inside TsParameterProperty"
                                        ),
                                    };

                                    let colour = self.get_color_of_node(i.id.node_id);
                                    (
                                        colour,
                                        i.id.sym.clone(),
                                        i.id.span,
                                        set_colour!(
                                            colour,
                                            ast::Param {
                                                node_id: self.node_id_gen.next(),
                                                span: DUMMY_SP,
                                                decorators: Default::default(),
                                                pat: ast::Pat::Assign(ast::AssignPat {
                                                    node_id: self.node_id_gen.next(),
                                                    span: p.span,
                                                    left: Box::new(i.into()),
                                                    right: p.right,
                                                    type_ann: None,
                                                }),
                                            }
                                        ),
                                    )
                                }
                            };
                            let assign_expr = Box::new(ast::Expr::Assign(set_colour!(
                                prop_colour,
                                ast::AssignExpr {
                                    node_id: self.node_id_gen.next(),
                                    span: DUMMY_SP,
                                    left: ast::PatOrExpr::Expr(Box::new(ast::Expr::Member(
                                        set_colour!(
                                            prop_colour,
                                            ast::MemberExpr {
                                                node_id: self.node_id_gen.next(),
                                                span: DUMMY_SP,
                                                obj: ast::ExprOrSuper::Expr(Box::new(
                                                    ast::Expr::This(set_colour!(
                                                        class_colour,
                                                        ast::ThisExpr {
                                                            node_id: self.node_id_gen.next(),
                                                            span: DUMMY_SP,
                                                        }
                                                    ))
                                                )),
                                                prop: Box::new(ast::Expr::Ident(set_colour!(
                                                    prop_colour,
                                                    ast::Ident {
                                                        node_id: self.node_id_gen.next(),
                                                        span: ident_span,
                                                        sym: ident_sym.clone(),
                                                        optional: false,
                                                    }
                                                ))),
                                                computed: false,
                                            }
                                        )
                                    ))),
                                    op: op!("="),
                                    right: Box::new(ast::Expr::Ident(set_colour!(
                                        prop_colour,
                                        ast::Ident {
                                            node_id: self.node_id_gen.next(),
                                            span: ident_span,
                                            sym: ident_sym,
                                            optional: false,
                                        }
                                    ))),
                                }
                            )));
                            assign_exprs.push(assign_expr);

                            ast::ParamOrTsParamProp::Param(param)
                        }
                    })
                });
                if !assign_exprs.is_empty() {
                    let body = c.body.as_mut().unwrap();
                    let initial_super_call = body
                        .stmts
                        .iter()
                        .position(|stmt| match stmt {
                            ast::Stmt::Expr(e) => match e.expr.as_ref() {
                                ast::Expr::Call(e) => match &e.callee {
                                    ast::ExprOrSuper::Super(_) => true,
                                    _ => false,
                                },
                                _ => false,
                            },
                            _ => false,
                        })
                        .map(|p| p + 1)
                        .unwrap_or_default();
                    let new_stmts = assign_exprs.into_iter().map(|expr| {
                        ast::Stmt::Expr(ast::ExprStmt {
                            node_id: self.node_id_gen.next(),
                            span: DUMMY_SP,
                            expr,
                        })
                    });
                    body.stmts
                        .splice(initial_super_call..initial_super_call, new_stmts);
                }

                break;
            }
        }

        class.visit_mut_children_with(self);
    }
}

impl VisitMut<'_> for ParamPropTransformer<'_> {
    fn visit_mut_class_decl(&mut self, node: &mut ast::ClassDecl) {
        let class_colour = self.get_color_of_node(node.node_id);
        self.handle_class(&mut node.class, class_colour);
    }
    fn visit_mut_class_expr(&mut self, node: &mut ast::ClassExpr) {
        let class_colour = self.get_color_of_node(node.node_id);
        self.handle_class(&mut node.class, class_colour);
    }
}

#[cfg(test)]
mod tests {
    fn test_transform(input: &str, expected: &str) {
        crate::testing::test_transform(
            |mut program, mut node_id_gen| {
                let mut colours = crate::testing::get_colours(program.clone());
                super::transform_param_props(&mut program, &mut node_id_gen, Some(&mut colours));
                program
            },
            input,
            expected,
        );
    }

    // TODO: test to ensure types are copied to new nodes.
    // TODO: test to ensure node ids are unique

    #[test]
    fn test_simple_param_prop() {
        test_transform(
            "
class Foo {
    constructor(public a: string) {}
}",
            "
class Foo {
    constructor(a: string) {
        this.a = a;
    }
}",
        );
    }

    #[test]
    fn test_assignments_are_inserted_after_initial_super() {
        test_transform(
            "
class Parent {}
class Foo extends Parent {
    constructor(public a: string) {
        alert(1);
        super();
        super();
    }
}",
            "
class Parent {}
class Foo extends Parent {
    constructor(a: string) {
        alert(1);
        super();
        this.a = a;
        super();
    }
}",
        );
    }

    #[test]
    fn test_assignments_are_inserted_before_stmts() {
        test_transform(
            "
class Foo {
    constructor(public a: string) {
        alert(1);
    }
}",
            "
class Foo {
    constructor(a: string) {
        this.a = a;
        alert(1);
    }
}",
        );
    }

    #[test]
    fn test_mixed_params() {
        test_transform(
            "
class Foo {
    constructor(public a: string, b: string, private d: string, c?: string) {}
}",
            "
class Foo {
    constructor(a: string, b: string, d: string, c?: string) {
        this.a = a;
        this.d = d;
    }
}",
        );
    }
}
