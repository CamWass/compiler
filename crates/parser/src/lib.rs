pub use self::parser::*;
pub use ast::EsVersion as JscTarget;
use serde::{Deserialize, Serialize};

#[macro_use]
mod macros;
mod context;
pub mod error;
pub mod lexer;
mod parser;
pub mod token;

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
#[serde(deny_unknown_fields, tag = "syntax")]
pub enum Syntax {
    /// Standard
    #[serde(rename = "ecmascript")]
    Es(EsConfig),
    #[serde(rename = "typescript")]
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

    pub fn dynamic_import(self) -> bool {
        matches!(
            self,
            Syntax::Es(EsConfig {
                dynamic_import: true,
                ..
            }) | Syntax::Typescript(TsConfig {
                dynamic_import: true,
                ..
            })
        )
    }

    pub fn class_private_props(self) -> bool {
        matches!(
            self,
            Syntax::Es(EsConfig {
                class_private_props: true,
                ..
            }) | Syntax::Typescript(..)
        )
    }

    /// Should we pare typescript?
    pub fn typescript(self) -> bool {
        matches!(self, Syntax::Typescript(..))
    }

    pub fn export_default_from(self) -> bool {
        matches!(
            self,
            Syntax::Es(EsConfig {
                export_default_from: true,
                ..
            })
        )
    }

    pub fn import_meta(self) -> bool {
        matches!(
            self,
            Syntax::Es(EsConfig {
                import_meta: true,
                ..
            }) | Syntax::Typescript(..)
        )
    }

    pub fn top_level_await(self) -> bool {
        matches!(
            self,
            Syntax::Es(EsConfig {
                top_level_await: true,
                ..
            }) | Syntax::Typescript(..)
        )
    }

    pub fn dts(self) -> bool {
        match self {
            Syntax::Typescript(t) => t.dts,
            Syntax::Es(_) => false,
        }
    }

    pub(crate) fn early_errors(self) -> bool {
        match self {
            Syntax::Typescript(t) => !t.no_early_errors,
            Syntax::Es(..) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct TsConfig {
    #[serde(default)]
    pub tsx: bool,

    #[serde(default)]
    pub dynamic_import: bool,

    /// `.d.ts`
    #[serde(skip, default)]
    pub dts: bool,

    #[serde(skip, default)]
    pub no_early_errors: bool,

    /// Stage 3.
    #[serde(default)]
    pub import_assertions: bool,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
pub struct EsConfig {
    #[serde(rename = "classPrivateProperty")]
    #[serde(default)]
    pub class_private_props: bool,

    #[serde(default)]
    pub export_default_from: bool,

    #[serde(default)]
    pub dynamic_import: bool,

    /// Stage 3.
    #[serde(default)]
    pub import_meta: bool,

    /// Stage 3.
    #[serde(default)]
    pub top_level_await: bool,

    /// Stage 3.
    #[serde(default)]
    pub import_assertions: bool,
}
