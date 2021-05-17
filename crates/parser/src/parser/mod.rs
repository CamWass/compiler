#[macro_use]
mod macros;
mod class_and_fn;
mod expression;
mod identifier;
mod input;
mod object;
mod pat;
mod statement;
mod util;

pub use self::input::Tokens;
use crate::{context::Context, lexer::Lexer, token::Token};
use ast::*;
use global_common::{input::Input, BytePos};
use input::Buffer;
use swc_atoms::JsWord;

#[derive(Clone, Default)]
struct State {
    labels: Vec<JsWord>,
    /// Start position of an assignment expression.
    potential_arrow_start: Option<BytePos>,
}

pub struct Parser<I: Tokens> {
    input: Buffer<I>,
    state: State,
}

impl<I: Input> Parser<Lexer<I>> {
    pub fn new(input: I) -> Self {
        Self::new_from(Lexer::new(input))
    }
}

impl<I: Tokens> Parser<I> {
    pub fn new_from(input: I) -> Self {
        Parser {
            state: Default::default(),
            input: Buffer::new(input),
        }
    }

    // pub fn take_errors(&mut self) -> Vec<Error> {
    //     self.input().take_errors()
    // }

    // pub(crate) fn target(&self) -> JscTarget {
    //     self.input.target()
    // }

    // pub fn parse_script(&mut self) -> Script {
    //     // trace_cur!(self, parse_script);

    //     let ctx = Context {
    //         module: false,
    //         ..self.ctx()
    //     };
    //     self.set_ctx(ctx);

    //     let start = self.input.cur_pos();

    //     let shebang = self.parse_shebang();

    //     let body =
    //         self.parse_block_body(true, true, None);

    //     Script {
    //         span: span!(self, start),
    //         body,
    //         shebang,
    //     }
    // }

    /// Returns [Module] if it's a module and returns [Script] if it's not a
    /// module.
    ///
    /// Note: This is not perfect yet. It means, some strict mode violations may
    /// not be reported even if the method returns [Module].
    pub fn parse_program(&mut self) -> Program {
        let start = self.input.cur_pos();
        let shebang = self.parse_shebang();

        let body: Vec<ModuleItem> = self.parse_block_body(true, true, None);
        let has_module_item = body.iter().any(|item| match item {
            ModuleItem::ModuleDecl(..) => true,
            _ => false,
        });
        if has_module_item && !self.ctx().module {
            let ctx = Context {
                module: true,
                strict: true,
                ..self.ctx()
            };
            // Emit buffered strict mode / module code violations
            self.input.set_ctx(ctx);
        }

        if has_module_item {
            Program::Module(Module {
                span: span!(self, start),
                body,
                shebang,
            })
        } else {
            let body = body
                .into_iter()
                .map(|item| match item {
                    ModuleItem::ModuleDecl(_) => unreachable!("Module is handled above"),
                    ModuleItem::Stmt(stmt) => stmt,
                })
                .collect();
            Program::Script(Script {
                span: span!(self, start),
                body,
                shebang,
            })
        }
    }

    // pub fn parse_module(&mut self) -> Module {
    //     let ctx = Context {
    //         module: true,
    //         strict: true,
    //         ..self.ctx()
    //     };
    //     // Module code is always in strict mode
    //     self.set_ctx(ctx);

    //     let start = self.input.cur_pos();
    //     let shebang = self.parse_shebang();

    //     let body =
    //         self.parse_block_body(true, true, None);

    //     Module {
    //         span: span!(self, start),
    //         body,
    //         shebang,
    //     }
    // }

    fn parse_shebang(&mut self) -> Option<JsWord> {
        match self.input.cur() {
            Some(Token::Shebang(..)) => match self.input.bump() {
                Token::Shebang(v) => Some(v),
                _ => unreachable!(),
            },
            _ => None,
        }
    }

    fn ctx(&self) -> Context {
        self.input.get_ctx()
    }
}
