use super::{list::ListFormat, Emitter, Result};
use ast::*;
use codegen_macros::emitter;
use global_common::Spanned;

impl<'a> Emitter<'a> {
    #[emitter]
    fn emit_decl(&mut self, node: &Decl) -> Result {
        match *node {
            Decl::Class(ref n) => emit!(n),
            Decl::Fn(ref n) => emit!(n),

            Decl::Var(ref n) => {
                emit!(n);
                formatting_semi!(); // VarDecl is also used for for-loops
            }
        }
    }

    #[emitter]
    fn emit_class_decl(&mut self, node: &ClassDecl) -> Result {
        self.emit_leading_comments_of_span(node.span(), false)?;

        for dec in &node.class.decorators {
            emit!(dec);
        }
        keyword!("class");
        space!();
        emit!(node.ident);

        self.emit_class_trailing(&node.class)?;
    }

    #[emitter]
    fn emit_fn_decl(&mut self, node: &FnDecl) -> Result {
        self.emit_leading_comments_of_span(node.span(), false)?;

        if node.function.is_async {
            keyword!("async");
            space!();
        }

        keyword!("function");
        if node.function.is_generator {
            punct!("*");
            formatting_space!();
        } else {
            space!();
        }

        emit!(node.ident);

        self.emit_fn_trailing(&node.function)?;
    }

    #[emitter]
    fn emit_var_decl(&mut self, node: &VarDecl) -> Result {
        self.emit_leading_comments_of_span(node.span, false)?;

        {
            let span = self.cm.span_until_char(node.span, ' ');
            keyword!(span, node.kind.as_str());
        }
        space!();

        self.emit_list(
            node.span(),
            Some(&node.decls),
            ListFormat::VariableDeclarationList,
        )?;
    }

    #[emitter]
    fn emit_var_declarator(&mut self, node: &VarDeclarator) -> Result {
        self.emit_leading_comments_of_span(node.span(), false)?;

        emit!(node.name);

        if let Some(ref init) = node.init {
            formatting_space!();
            punct!("=");
            formatting_space!();
            emit!(init);
        }
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
