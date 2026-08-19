use crate::token::{Keyword, Word};
use ast::{NameId, id_for_built_in};
use bitflags::bitflags;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum YesNoMaybe {
    Yes,
    No,
    #[default]
    Maybe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum YesMaybe {
    Yes,
    #[default]
    Maybe,
}

bitflags! {
    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Default)]
    pub struct ContextFlags: u16 {
        const include_in_expr = 1 << 0;
        /// If true, await expression is parsed, and "await" is treated as a
        /// keyword.
        const in_async = 1 << 1;
        /// If true, yield expression is parsed, and "yield" is treated as a
        /// keyword.
        const in_generator = 1 << 2;

        const is_continue_allowed = 1 << 3;
        const is_break_allowed = 1 << 4;

        const in_type = 1 << 5;
        /// Typescript extension.
        const in_declare = 1 << 6;

        /// If true, `:` should not be treated as a type annotation.
        const in_cond_expr = 1 << 7;

        const in_function = 1 << 8;

        const in_parameters = 1 << 9;

        const has_super_class = 1 << 10;
        const in_class_prop = 1 << 11;

        const in_property_name = 1 << 12;

        /// If true, `:` should not be treated as a type annotation.
        const in_case_cond = 1 << 13;
    }
}

/// Syntactic context.
#[derive(Debug, Clone, Copy, Default)]
pub struct Context {
    /// Is in module code?
    pub module: YesNoMaybe,
    pub strict: YesMaybe,
    pub flags: ContextFlags,
}

impl Context {
    #[inline]
    pub(crate) fn is_strict(&self) -> bool {
        self.strict == YesMaybe::Yes
    }
    #[inline]
    pub(crate) fn is_module(&self) -> bool {
        self.module == YesNoMaybe::Yes
    }

    pub(crate) fn is_reserved(self, word: &Word) -> bool {
        match *word {
            Word::Keyword(Keyword::Let) => self.is_strict(),
            Word::Keyword(Keyword::Await) => {
                self.flags.contains(ContextFlags::in_async) || self.is_strict()
            }
            Word::Keyword(Keyword::Yield) => {
                self.flags.contains(ContextFlags::in_generator) || self.is_strict()
            }

            Word::Null
            | Word::True
            | Word::False
            | Word::Keyword(Keyword::Break)
            | Word::Keyword(Keyword::Case)
            | Word::Keyword(Keyword::Catch)
            | Word::Keyword(Keyword::Continue)
            | Word::Keyword(Keyword::Debugger)
            | Word::Keyword(Keyword::Default_)
            | Word::Keyword(Keyword::Do)
            | Word::Keyword(Keyword::Export)
            | Word::Keyword(Keyword::Else)
            | Word::Keyword(Keyword::Finally)
            | Word::Keyword(Keyword::For)
            | Word::Keyword(Keyword::Function)
            | Word::Keyword(Keyword::If)
            | Word::Keyword(Keyword::Return)
            | Word::Keyword(Keyword::Switch)
            | Word::Keyword(Keyword::Throw)
            | Word::Keyword(Keyword::Try)
            | Word::Keyword(Keyword::Var)
            | Word::Keyword(Keyword::Const)
            | Word::Keyword(Keyword::While)
            | Word::Keyword(Keyword::With)
            | Word::Keyword(Keyword::New)
            | Word::Keyword(Keyword::This)
            | Word::Keyword(Keyword::Super)
            | Word::Keyword(Keyword::Class)
            | Word::Keyword(Keyword::Extends)
            | Word::Keyword(Keyword::Import)
            | Word::Keyword(Keyword::In)
            | Word::Keyword(Keyword::InstanceOf)
            | Word::Keyword(Keyword::TypeOf)
            | Word::Keyword(Keyword::Void)
            | Word::Keyword(Keyword::Delete) => true,

            // Future reserved word
            Word::Ident(id_for_built_in!("enum")) => true,

            Word::Ident(id_for_built_in!("implements"))
            | Word::Ident(id_for_built_in!("package"))
            | Word::Ident(id_for_built_in!("protected"))
            | Word::Ident(id_for_built_in!("interface"))
            | Word::Ident(id_for_built_in!("private"))
            | Word::Ident(id_for_built_in!("public"))
                if self.is_strict() =>
            {
                true
            }

            _ => false,
        }
    }

    pub fn is_reserved_word(self, word: NameId) -> bool {
        match word {
            id_for_built_in!("let") => self.is_strict(),
            id_for_built_in!("await") => {
                self.flags.contains(ContextFlags::in_async) || self.is_strict()
            }
            id_for_built_in!("yield") => {
                self.flags.contains(ContextFlags::in_generator) || self.is_strict()
            }

            id_for_built_in!("null")
            | id_for_built_in!("true")
            | id_for_built_in!("false")
            | id_for_built_in!("break")
            | id_for_built_in!("case")
            | id_for_built_in!("catch")
            | id_for_built_in!("continue")
            | id_for_built_in!("debugger")
            | id_for_built_in!("default")
            | id_for_built_in!("do")
            | id_for_built_in!("export")
            | id_for_built_in!("else")
            | id_for_built_in!("finally")
            | id_for_built_in!("for")
            | id_for_built_in!("function")
            | id_for_built_in!("if")
            | id_for_built_in!("return")
            | id_for_built_in!("switch")
            | id_for_built_in!("throw")
            | id_for_built_in!("try")
            | id_for_built_in!("var")
            | id_for_built_in!("const")
            | id_for_built_in!("while")
            | id_for_built_in!("with")
            | id_for_built_in!("new")
            | id_for_built_in!("this")
            | id_for_built_in!("super")
            | id_for_built_in!("class")
            | id_for_built_in!("extends")
            | id_for_built_in!("import")
            | id_for_built_in!("in")
            | id_for_built_in!("instanceof")
            | id_for_built_in!("typeof")
            | id_for_built_in!("void")
            | id_for_built_in!("delete") => true,

            // Future reserved word
            id_for_built_in!("enum") => true,

            id_for_built_in!("implements")
            | id_for_built_in!("package")
            | id_for_built_in!("protected")
            | id_for_built_in!("interface")
            | id_for_built_in!("private")
            | id_for_built_in!("public")
                if self.is_strict() =>
            {
                true
            }

            _ => false,
        }
    }

    pub fn include_in_expr(&self) -> bool {
        self.flags.contains(ContextFlags::include_in_expr)
    }

    pub fn in_async(&self) -> bool {
        self.flags.contains(ContextFlags::in_async)
    }

    pub fn in_generator(&self) -> bool {
        self.flags.contains(ContextFlags::in_generator)
    }

    pub fn is_continue_allowed(&self) -> bool {
        self.flags.contains(ContextFlags::is_continue_allowed)
    }

    pub fn is_break_allowed(&self) -> bool {
        self.flags.contains(ContextFlags::is_break_allowed)
    }

    pub fn in_type(&self) -> bool {
        self.flags.contains(ContextFlags::in_type)
    }

    pub fn in_declare(&self) -> bool {
        self.flags.contains(ContextFlags::in_declare)
    }

    pub fn in_cond_expr(&self) -> bool {
        self.flags.contains(ContextFlags::in_cond_expr)
    }

    pub fn in_function(&self) -> bool {
        self.flags.contains(ContextFlags::in_function)
    }

    pub fn in_parameters(&self) -> bool {
        self.flags.contains(ContextFlags::in_parameters)
    }

    pub fn has_super_class(&self) -> bool {
        self.flags.contains(ContextFlags::has_super_class)
    }

    pub fn in_case_cond(&self) -> bool {
        self.flags.contains(ContextFlags::in_case_cond)
    }
}
