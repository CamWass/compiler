pub mod Flags {
    pub const PARAM: u8 = 0b0000; // Initial Parameter flags
    pub const PARAM_YIELD: u8 = 0b0001; // track [Yield] production parameter
    pub const PARAM_AWAIT: u8 = 0b0010; // track [Await] production parameter
    pub const PARAM_RETURN: u8 = 0b0100; // track [Return] production parameter
    pub const PARAM_IN: u8 = 0b1000; // track [In] production parameter
}

// ProductionParameterHandler is a stack fashioned production parameter tracker
// https://tc39.es/ecma262/#sec-grammar-notation
// The tracked parameters are defined above.
//
// Whenever [+Await]/[+Yield] appears in the right-hand sides of a production,
// we must enter a new tracking stack. For example when parsing
//
// AsyncFunctionDeclaration [Yield, Await]:
//   async [no LineTerminator here] function BindingIdentifier[?Yield, ?Await]
//     ( FormalParameters[~Yield, +Await] ) { AsyncFunctionBody }
//
// we must follow such process:
//
// 1. parse async keyword
// 2. parse function keyword
// 3. parse bindingIdentifier <= inherit current parameters: [?Await]
// 4. enter new stack with (PARAM_AWAIT)
// 5. parse formal parameters <= must have [Await] parameter [+Await]
// 6. parse function body
// 7. exit current stack

pub struct ProductionParameterHandler {
    stacks: Vec<u8>,
}

impl ProductionParameterHandler {
    pub fn new() -> Self {
        Self { stacks: Vec::new() }
    }

    pub fn enter(&mut self, flags: u8) {
        self.stacks.push(flags);
    }

    fn exit(&mut self) {
        self.stacks.pop();
    }

    fn currentFlags(&self) -> Option<&u8> {
        self.stacks.last()
    }

    fn hasAwait(&self) -> bool {
        match self.currentFlags() {
            Some(flags) => (flags & Flags::PARAM_AWAIT) > 0,
            None => false,
        }
    }

    fn hasYield(&self) -> bool {
        match self.currentFlags() {
            Some(flags) => (flags & Flags::PARAM_YIELD) > 0,
            None => false,
        }
    }

    fn hasReturn(&self) -> bool {
        match self.currentFlags() {
            Some(flags) => (flags & Flags::PARAM_RETURN) > 0,
            None => false,
        }
    }

    fn hasIn(&self) -> bool {
        match self.currentFlags() {
            Some(flags) => (flags & Flags::PARAM_IN) > 0,
            None => false,
        }
    }
}
