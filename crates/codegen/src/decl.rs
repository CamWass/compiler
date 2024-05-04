use super::{list::ListFormat, Emitter, Result};
use ast::*;

impl<'a> Emitter<'a> {
    pub fn emit_decl(&mut self, node: &Decl) -> Result {
        match *node {
            Decl::Class(ref n) => self.emit_class_decl(n)?,
            Decl::Fn(ref n) => self.emit_fn_decl(n)?,

            Decl::Var(ref n) => {
                self.emit_var_decl(n)?;
                formatting_semi!(self); // VarDecl is also used for for-loops
            }
        }
        Ok(())
    }

    fn emit_class_decl(&mut self, node: &ClassDecl) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        for dec in &node.class.decorators {
            self.emit_decorator(dec)?;
        }
        keyword!(self, "class");
        space!(self);
        self.emit_ident(&node.ident)?;

        self.emit_class_trailing(&node.class)
    }

    fn emit_fn_decl(&mut self, node: &FnDecl) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

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

        self.emit_leading_comments_of_span(span, false)?;

        {
            let span = self.cm.span_until_char(span, ' ');
            keyword!(self, span, node.kind.as_str());
        }
        space!(self);

        self.emit_list(
            span,
            &node.decls,
            |e, n| e.emit_var_declarator(n).map(|_| Some(n.node_id)),
            ListFormat::VariableDeclarationList,
        )
    }

    fn emit_var_declarator(&mut self, node: &VarDeclarator) -> Result {
        self.emit_leading_comments_of_span(get_span!(self, node.node_id), false)?;

        self.emit_pat(&node.name)?;

        if let Some(ref init) = node.init {
            formatting_space!(self);
            punct!(self, "=");
            formatting_space!(self);
            self.emit_expr(init)?
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
            "class Hoge{};class HogeFuga extends Hoge{};",
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
}
