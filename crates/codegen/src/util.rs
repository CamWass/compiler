use super::list::ListFormat;
use ast::*;
use global_common::{errors::SourceMapper, BytePos, SourceMap, SourceMapperDyn, Span};
use std::{rc::Rc, sync::Arc};

pub trait SourceMapperExt {
    fn get_code_map(&self) -> &dyn SourceMapper;

    fn is_on_same_line(&self, _lo: BytePos, _hi: BytePos) -> bool {
        // let cm = self.get_code_map();

        // let lo = cm.lookup_char_pos(lo);
        // let hi = cm.lookup_char_pos(hi);

        // lo.line == hi.line && lo.file.name_hash == hi.file.name_hash
        false
    }

    fn should_write_separating_line_terminator(&self, format: ListFormat) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return true;
        }

        if format.contains(ListFormat::PreserveLines) {
            return format.contains(ListFormat::PreferNewLine);
        }

        false
    }

    fn should_write_leading_line_terminator<N>(
        &self,
        parent_node: Span,
        children: &[N],
        format: ListFormat,
    ) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return true;
        }

        if format.contains(ListFormat::PreserveLines) {
            if children.is_empty() {
                return !self.is_on_same_line(parent_node.lo(), parent_node.hi());
            }

            format.contains(ListFormat::PreferNewLine)
        } else {
            false
        }
    }

    fn should_write_closing_line_terminator<N>(
        &self,
        parent_node: Span,
        children: &[N],
        format: ListFormat,
    ) -> bool {
        if format.contains(ListFormat::MultiLine) {
            return (format & ListFormat::NoTrailingNewLine) == ListFormat::None;
        }

        if format.contains(ListFormat::PreserveLines) {
            if children.is_empty() {
                return !self.is_on_same_line(parent_node.lo(), parent_node.hi());
            }

            format.contains(ListFormat::PreferNewLine)
        } else {
            false
        }
    }
}
impl SourceMapperExt for dyn SourceMapper {
    fn get_code_map(&self) -> &dyn SourceMapper {
        self
    }
}
impl SourceMapperExt for Arc<SourceMapperDyn> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Rc<SourceMapperDyn> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Arc<SourceMap> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

impl SourceMapperExt for Rc<SourceMap> {
    fn get_code_map(&self) -> &dyn SourceMapper {
        &**self
    }
}

pub fn for_var_ends_with_alpha_num(var: &VarDeclOrPat) -> bool {
    match var {
        VarDeclOrPat::VarDecl(n) => {
            assert!(n.decls.len() == 1 && n.decls[0].init.is_none());
            true
        }
        VarDeclOrPat::Pat(n) => match n {
            Pat::Object(_) | Pat::Array(_) => false,
            Pat::Ident(_) => true,
            _ => unreachable!(),
        },
    }
}

pub fn prop_name_starts_with_alpha_num(name: &PropName) -> bool {
    match name {
        PropName::Str(_) | PropName::Computed(_) => false,
        PropName::Ident(_) | PropName::Num(_) | PropName::BigInt(_) => true,
    }
}
