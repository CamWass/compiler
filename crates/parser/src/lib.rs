pub use self::parser::*;
pub use ast::EsVersion as JscTarget;

#[macro_use]
mod macros;
mod context;
pub mod error;
pub mod lexer;
mod parser;
pub mod token;

#[derive(Debug, Clone, Copy)]
pub enum Syntax {
    /// Standard
    Es(EsConfig),
    Typescript(TsConfig),
}

impl Default for Syntax {
    fn default() -> Self {
        Syntax::Es(Default::default())
    }
}

impl Syntax {
    pub fn import_assertions(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                import_assertions, ..
            })
            | Syntax::Typescript(TsConfig {
                import_assertions, ..
            }) => import_assertions,
        }
    }

    /// Should we parse jsx?
    pub fn jsx(self) -> bool {
        match self {
            Syntax::Es(EsConfig { jsx: true, .. })
            | Syntax::Typescript(TsConfig { tsx: true, .. }) => true,
            _ => false,
        }
    }

    pub fn dynamic_import(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                dynamic_import: true,
                ..
            })
            | Syntax::Typescript(TsConfig {
                dynamic_import: true,
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn decorators(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                decorators: true, ..
            })
            | Syntax::Typescript(TsConfig {
                decorators: true, ..
            }) => true,
            _ => false,
        }
    }

    pub fn class_private_props(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                class_private_props: true,
                ..
            })
            | Syntax::Typescript(..) => true,
            _ => false,
        }
    }

    pub fn decorators_before_export(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                decorators_before_export: true,
                ..
            })
            | Syntax::Typescript(..) => true,
            _ => false,
        }
    }

    /// Should we pare typescript?
    pub fn typescript(self) -> bool {
        match self {
            Syntax::Typescript(..) => true,
            _ => false,
        }
    }

    pub fn export_default_from(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                export_default_from: true,
                ..
            }) => true,
            _ => false,
        }
    }

    pub fn import_meta(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                import_meta: true, ..
            })
            | Syntax::Typescript(..) => true,

            _ => false,
        }
    }

    pub fn top_level_await(self) -> bool {
        match self {
            Syntax::Es(EsConfig {
                top_level_await: true,
                ..
            })
            | Syntax::Typescript(..) => true,

            _ => false,
        }
    }

    pub fn dts(self) -> bool {
        match self {
            Syntax::Typescript(t) => t.dts,
            _ => false,
        }
    }

    pub(crate) fn early_errors(self) -> bool {
        match self {
            Syntax::Typescript(t) => !t.no_early_errors,
            Syntax::Es(..) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TsConfig {
    pub tsx: bool,

    pub decorators: bool,

    pub dynamic_import: bool,

    /// `.d.ts`
    pub dts: bool,

    pub no_early_errors: bool,

    /// Stage 3.
    pub import_assertions: bool,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EsConfig {
    pub jsx: bool,

    pub class_private_props: bool,

    /// Enable decorators.
    pub decorators: bool,

    /// babel: `decorators.decoratorsBeforeExport`
    ///
    /// Effective only if `decorator` is true.
    pub decorators_before_export: bool,

    pub export_default_from: bool,

    pub dynamic_import: bool,

    /// Stage 3.
    pub import_meta: bool,

    /// Stage 3.
    pub top_level_await: bool,

    /// Stage 3.
    pub import_assertions: bool,
}
