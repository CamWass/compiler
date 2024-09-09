use super::{Emitter, Result};
use ast::*;

impl<'a> Emitter<'a> {
    pub fn emit_jsx_element(&mut self, node: &JSXElement) -> Result {
        // emit!(self, node.opening);
        // self.emit_list(
        //     node.span(),
        //     Some(&node.children),
        //     ListFormat::JsxElementOrFragmentChildren,
        // )?;
        // if let Some(ref closing) = node.closing {
        //     emit!(self, closing)
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_opening_element(&mut self, node: &JSXOpeningElement) -> Result {
        // punct!(self, "<");
        // emit!(self, node.name);

        // space!(self);

        // self.emit_list(
        //     node.span(),
        //     Some(&node.attrs),
        //     ListFormat::JsxElementAttributes,
        // )?;

        // if node.self_closing {
        //     punct!(self, "/");
        // }
        // punct!(self, ">");
        // Ok(())
        todo!()
    }

    fn emit_jsx_element_name(&mut self, node: &JSXElementName) -> Result {
        // match *node {
        //     JSXElementName::Ident(ref n) => emit!(self, n),
        //     JSXElementName::JSXMemberExpr(ref n) => emit!(self, n),
        //     JSXElementName::JSXNamespacedName(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_attr(&mut self, node: &JSXAttr) -> Result {
        // emit!(self, node.name);

        // if let Some(ref value) = node.value {
        //     punct!(self, "=");

        //     emit!(self, value);
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_attr_value(&mut self, node: &JSXAttrValue) -> Result {
        // match *node {
        //     JSXAttrValue::Lit(ref n) => emit!(self, n),
        //     JSXAttrValue::JSXExprContainer(ref n) => emit!(self, n),
        //     JSXAttrValue::JSXElement(ref n) => emit!(self, n),
        //     JSXAttrValue::JSXFragment(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_attr_name(&mut self, node: &JSXAttrName) -> Result {
        // match *node {
        //     JSXAttrName::Ident(ref n) => emit!(self, n),
        //     JSXAttrName::JSXNamespacedName(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_attr_or_spread(&mut self, node: &JSXAttrOrSpread) -> Result {
        // match *node {
        //     JSXAttrOrSpread::JSXAttr(ref n) => emit!(self, n),
        //     JSXAttrOrSpread::SpreadElement(ref n) => {
        //         punct!(self, "{");
        //         emit!(self, n);
        //         punct!(self, "}");
        //     }
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_element_child(&mut self, node: &JSXElementChild) -> Result {
        // match *node {
        //     JSXElementChild::JSXElement(ref n) => emit!(self, n),
        //     JSXElementChild::JSXExprContainer(ref n) => emit!(self, n),
        //     JSXElementChild::JSXFragment(ref n) => emit!(self, n),
        //     JSXElementChild::JSXSpreadChild(ref n) => emit!(self, n),
        //     JSXElementChild::JSXText(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_spread_child(&mut self, node: &JSXSpreadChild) -> Result {
        // punct!(self, "{");
        // punct!(self, "...");
        // emit!(self, node.expr);
        // punct!(self, "}");
        // Ok(())
        todo!()
    }

    fn emit_jsx_expr_container(&mut self, node: &JSXExprContainer) -> Result {
        // punct!(self, "{");
        // emit!(self, node.expr);
        // punct!(self, "}");
        // Ok(())
        todo!()
    }

    fn emit_jsx_expr(&mut self, node: &JSXExpr) -> Result {
        // match *node {
        //     JSXExpr::Expr(ref n) => emit!(self, n),
        //     JSXExpr::JSXEmptyExpr(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }

    fn emit_jsx_closing_element(&mut self, node: &JSXClosingElement) -> Result {
        // punct!(self, "</");
        // emit!(self, node.name);
        // punct!(self, ">");
        // Ok(())
        todo!()
    }

    pub fn emit_jsx_fragment(&mut self, node: &JSXFragment) -> Result {
        // emit!(self, node.opening);

        // self.emit_list(
        //     node.span(),
        //     Some(&node.children),
        //     ListFormat::JsxElementOrFragmentChildren,
        // )?;

        // emit!(self, node.closing);
        // Ok(())
        todo!()
    }

    fn emit_jsx_opening_fragment(&mut self, node: &JSXOpeningFragment) -> Result {
        // punct!(self, "<>");
        // Ok(())
        todo!()
    }

    fn emit_jsx_closing_fragment(&mut self, node: &JSXClosingFragment) -> Result {
        punct!(self, "</>");
        Ok(())
    }

    pub fn emit_jsx_namespaced_name(&mut self, node: &JSXNamespacedName) -> Result {
        // emit!(self, node.ns);
        // punct!(self, ":");
        // emit!(self, node.name);
        // Ok(())
        todo!()
    }

    pub fn emit_jsx_empty_expr(&mut self, node: &JSXEmptyExpr) -> Result {
        Ok(())
    }

    pub fn emit_jsx_text(&mut self, node: &JSXText) -> Result {
        // self.emit_js_word(node.span(), &node.value)
        todo!()
    }

    pub fn emit_jsx_member_expr(&mut self, node: &JSXMemberExpr) -> Result {
        // emit!(self, node.obj);
        // punct!(self, ".");
        // emit!(self, node.prop);
        // Ok(())
        todo!()
    }

    fn emit_jsx_object(&mut self, node: &JSXObject) -> Result {
        // match *node {
        //     JSXObject::Ident(ref n) => emit!(self, n),
        //     JSXObject::JSXMemberExpr(ref n) => emit!(self, n),
        // }
        // Ok(())
        todo!()
    }
}
