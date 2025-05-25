use crate::token::{Keyword, Word};
use atoms::{js_word, JsWord};
use bitflags::bitflags;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YesNoMaybe {
    Yes,
    No,
    Maybe,
}

impl Default for YesNoMaybe {
    fn default() -> Self {
        Self::Maybe
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YesMaybe {
    Yes,
    Maybe,
}

impl Default for YesMaybe {
    fn default() -> Self {
        Self::Maybe
    }
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
            Word::Ident(js_word!("enum")) => true,

            Word::Ident(js_word!("implements"))
            | Word::Ident(js_word!("package"))
            | Word::Ident(js_word!("protected"))
            | Word::Ident(js_word!("interface"))
            | Word::Ident(js_word!("private"))
            | Word::Ident(js_word!("public"))
                if self.is_strict() =>
            {
                true
            }

            _ => false,
        }
    }

    pub fn is_reserved_word(self, word: &JsWord) -> bool {
        match *word {
            js_word!("let") => self.is_strict(),
            js_word!("await") => self.flags.contains(ContextFlags::in_async) || self.is_strict(),
            js_word!("yield") => {
                self.flags.contains(ContextFlags::in_generator) || self.is_strict()
            }

            js_word!("null")
            | js_word!("true")
            | js_word!("false")
            | js_word!("break")
            | js_word!("case")
            | js_word!("catch")
            | js_word!("continue")
            | js_word!("debugger")
            | js_word!("default")
            | js_word!("do")
            | js_word!("export")
            | js_word!("else")
            | js_word!("finally")
            | js_word!("for")
            | js_word!("function")
            | js_word!("if")
            | js_word!("return")
            | js_word!("switch")
            | js_word!("throw")
            | js_word!("try")
            | js_word!("var")
            | js_word!("const")
            | js_word!("while")
            | js_word!("with")
            | js_word!("new")
            | js_word!("this")
            | js_word!("super")
            | js_word!("class")
            | js_word!("extends")
            | js_word!("import")
            | js_word!("in")
            | js_word!("instanceof")
            | js_word!("typeof")
            | js_word!("void")
            | js_word!("delete") => true,

            // Future reserved word
            js_word!("enum") => true,

            js_word!("implements")
            | js_word!("package")
            | js_word!("protected")
            | js_word!("interface")
            | js_word!("private")
            | js_word!("public")
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
