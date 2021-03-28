// use crate::lexer::Lexer;

pub mod ast;
pub mod location;
mod node;
mod production_parameter;
mod scope;
mod scope_flags;

use production_parameter::{
    Flags::{PARAM, PARAM_AWAIT},
    ProductionParameterHandler,
};

use scope::ScopeHandler;

use scope_flags::Flags;

pub struct Parser {
    scope: ScopeHandler,
    in_module: bool,
    // lexer: Lexer<'a>,
    prod_param: ProductionParameterHandler,
}

impl Parser {
    pub fn new(
        input: &str,
        in_module: bool, /* = this.options.sourceType === "module",*/
    ) -> Self {
        // Initialize state
        // const oldLabels = this.state.labels;
        // this.state.labels = [];

        // const oldExportedIdentifiers = this.state.exportedIdentifiers;
        // this.state.exportedIdentifiers = [];

        // const oldProdParam = this.prodParam;
        // this.prodParam = new ProductionParameterHandler();

        // const oldClassScope = this.classScope;
        // this.classScope = new ClassScopeHandler(this.raise.bind(this));

        // const oldExpressionScope = this.expressionScope;
        // this.expressionScope = new ExpressionScopeHandler(this.raise.bind(this));

        // return () => {
        //   // Revert state
        //   this.state.labels = oldLabels;
        //   this.state.exportedIdentifiers = oldExportedIdentifiers;

        //   // Revert scopes
        //   this.inModule = oldInModule;
        //   this.scope = oldScope;
        //   this.prodParam = oldProdParam;
        //   this.classScope = oldClassScope;
        //   this.expressionScope = oldExpressionScope;
        // };

        Self {
            scope: ScopeHandler::new(in_module),
            in_module,
            // lexer: Lexer::from_str(input),
            prod_param: ProductionParameterHandler::new(),
        }
    }

    // fn parse(&self)
    // {
    //   self.enterInitialScopes();
    //   let file = self.startNode();
    //   let program = self.startNode();
    //   self.nextToken();
    //   // file.errors = null;
    //   self.parseTopLevel(file, program);
    //   // file.errors = this.state.errors;
    //   // return file;
    // }

    fn enterInitialScopes(&mut self) {
        let mut paramFlags = PARAM;

        if self.in_module {
            paramFlags |= PARAM_AWAIT;
        }
        self.scope.enter(Flags::SCOPE_PROGRAM);
        self.prod_param.enter(paramFlags);
    }
}
