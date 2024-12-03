use crate::Context;

use super::{list::ListFormat, Emitter, Result};
use ast::*;

impl Emitter<'_> {
    pub fn emit_decl(&mut self, node: &Decl) -> Result {
        match node {
            Decl::Class(n) => self.emit_class_decl(n)?,
            Decl::Fn(n) => self.emit_fn_decl(n)?,

            Decl::Var(n) => {
                self.emit_var_decl(n)?;
                formatting_semi!(self); // VarDecl is also used for for-loops
            }
        }
        Ok(())
    }

    fn emit_class_decl(&mut self, node: &ClassDecl) -> Result {
        keyword!(self, "class");
        space!(self);
        self.emit_ident(&node.ident)?;

        self.emit_class_trailing(&node.class)
    }

    fn emit_fn_decl(&mut self, node: &FnDecl) -> Result {
        if node.function.is_async {
            keyword!(self, "async");
            space!(self);
        }

        keyword!(self, "function");
        if node.function.is_generator {
            punct!(self, "*");
            formatting_space!(self);
        } else {
            space!(self);
        }

        self.emit_ident(&node.ident)?;

        self.emit_fn_trailing(&node.function)
    }

    pub fn emit_var_decl(&mut self, node: &VarDecl) -> Result {
        let span = get_span!(self, node.node_id);

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, node.kind.as_str());
        }

        let starts_with_ident = !matches!(
            node.decls.first(),
            Some(VarDeclarator {
                name: Pat::Array(..) | Pat::Rest(..) | Pat::Object(..),
                ..
            })
        );
        if starts_with_ident {
            space!(self);
        } else {
            formatting_space!(self);
        }

        self.emit_list(
            span,
            &node.decls,
            |e, n| e.emit_var_declarator(n),
            ListFormat::VariableDeclarationList,
        )
    }

    fn emit_var_declarator(&mut self, node: &VarDeclarator) -> Result {
        self.emit_pat(&node.name)?;

        if let Some(init) = &node.init {
            formatting_space!(self);
            punct!(self, "=");
            formatting_space!(self);
            let old = self.ctx;
            self.ctx = Context::ForcedExpr;
            self.emit_expr(init)?;
            self.ctx = old;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::assert_min;

    #[test]
    fn issue_275() {
        assert_min(
            "function* foo(){
            yield getServiceHosts()
        }",
            "function*foo(){yield getServiceHosts()}",
        );
    }

    #[test]
    fn issue_1764() {
        assert_min(
            "class Hoge {};
class HogeFuga extends Hoge {};",
            "class Hoge{}class HogeFuga extends Hoge{}",
        );
    }

    #[test]
    fn single_argument_arrow_expression() {
        assert_min("function* f(){ yield x => x}", "function*f(){yield x=>x}");
        assert_min(
            "function* f(){ yield ({x}) => x}",
            "function*f(){yield({x})=>x}",
        );
    }

    #[test]
    fn test_destructuring() {
        assert_min("const {a:a} = {};", "const{a}={}");
        assert_min("const {a:a = b} = {};", "const{a=b}={}");
    }

    #[test]
    fn test_class() {
        assert_min("class Foo extends Bar {}", "class Foo extends Bar{}");
        assert_min(
            "class Foo extends (1||Bar) {}",
            "class Foo extends(1||Bar){}",
        );
    }
}
