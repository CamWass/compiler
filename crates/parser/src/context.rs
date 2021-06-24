use global_common::Span;
use swc_atoms::{js_word, JsWord};

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

/// Syntactic context.
#[derive(Debug, Clone, Copy, Default)]
pub struct Context {
    /// Is in module code?
    pub module: YesNoMaybe,
    pub strict: YesMaybe,
    pub include_in_expr: bool,
    /// If true, await expression is parsed, and "await" is treated as a
    /// keyword.
    pub in_async: bool,
    /// If true, yield expression is parsed, and "yield" is treated as a
    /// keyword.
    pub in_generator: bool,

    pub is_continue_allowed: bool,
    pub is_break_allowed: bool,

    pub in_type: bool,
    /// Typescript extension.
    pub in_declare: bool,
    pub span_of_fn_name: Option<Span>,

    /// If true, `:` should not be treated as a type annotation.
    pub in_cond_expr: bool,

    pub in_function: bool,

    pub in_parameters: bool,

    pub has_super_class: bool,
    pub in_method: bool,
    pub in_class_prop: bool,

    pub in_property_name: bool,

    pub in_forced_jsx_context: bool,

    /// If true, `:` should not be treated as a type annotation.
    pub in_case_cond: bool,
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

    pub fn is_reserved_word(self, word: &JsWord) -> bool {
        match *word {
            js_word!("let") => self.is_strict(),
            js_word!("await") => self.in_async || self.is_strict(),
            js_word!("yield") => self.in_generator || self.is_strict(),

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
}
