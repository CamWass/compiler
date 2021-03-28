use std::collections::HashMap;

use super::scope_flags::Flags;

// Start an AST node, attaching a start offset.
pub struct Scope {
    flags: u16,
    // A list of var-declared names in the current lexical scope
    var: Vec<String>,
    // A list of lexically-declared names in the current lexical scope
    lexical: Vec<String>,
    // A list of lexically-declared FunctionDeclaration names in the current lexical scope
    functions: Vec<String>,
}

impl Scope {
    pub fn new(flags: u16) -> Self {
        Self {
            flags,
            var: Vec::new(),
            lexical: Vec::new(),
            functions: Vec::new(),
        }
    }
}

// The functions in this module keep track of declared variables in the
// current scope in order to detect duplicate variable names.
pub struct ScopeHandler {
    scope_stack: Vec<Scope>,
    in_module: bool,
    // TODO: narrow down from usize
    undefined_exports: HashMap<String, usize>,
    undefined_private_names: HashMap<String, usize>,
}

impl ScopeHandler {
    pub fn new(in_module: bool) -> Self {
        Self {
            scope_stack: Vec::new(),
            in_module,
            undefined_exports: HashMap::new(),
            undefined_private_names: HashMap::new(),
        }
    }

    fn in_function(&self) -> bool {
        match self.current_var_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_FUNCTION) > 0,
            None => false,
        }
    }

    fn allow_super(&self) -> bool {
        match self.current_this_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_SUPER) > 0,
            None => false,
        }
    }

    fn allow_direct_super(&self) -> bool {
        match self.current_this_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_DIRECT_SUPER) > 0,
            None => false,
        }
    }

    fn in_class(&self) -> bool {
        match self.current_this_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_CLASS) > 0,
            None => false,
        }
    }

    fn in_static_block(&self) -> bool {
        match self.current_this_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_STATIC_BLOCK) > 0,
            None => false,
        }
    }

    fn in_non_arrow_function(&self) -> bool {
        match self.current_this_scope() {
            Some(scope) => (scope.flags & Flags::SCOPE_FUNCTION) > 0,
            None => false,
        }
    }

    fn treat_functions_as_var(&self) -> bool {
        match self.current_scope() {
            Some(scope) => self.treat_functions_as_var_in_scope(scope),
            None => false,
        }
    }

    fn create_scope(flags: u16) -> Scope {
        Scope::new(flags)
    }

    // TODO: comment:
    // This method will be overwritten by subclasses
    /*:: +createScope: (flags: ScopeFlags) => IScope; */
    pub fn enter(&mut self, flags: u16) {
        self.scope_stack.push(ScopeHandler::create_scope(flags));
    }

    fn exit(&mut self) {
        self.scope_stack.pop();
    }

    // The spec says:
    // > At the top level of a function, or script, function declarations are
    // > treated like var declarations rather than like lexical declarations.
    fn treat_functions_as_var_in_scope(&self, scope: &Scope) -> bool {
        !!((scope.flags & Flags::SCOPE_FUNCTION > 0)
            || (!self.in_module && (scope.flags & Flags::SCOPE_PROGRAM > 0)))
    }

    // fn declare_name(
    //   &self,
    //   name: &String,
    //   binding_type: u16,
    //   /* narrow this number type*/ pos: usize,
    // ) {
    //   if let Some(scope) = self.current_scope() {
    //     if (binding_type & Flags::BIND_SCOPE_LEXICAL > 0)
    //       || (binding_type & Flags::BIND_SCOPE_FUNCTION > 0)
    //     {
    //       // self.checkRedeclarationInScope(scope, name, bindingType, pos);

    //       // if binding_type & Flags::BIND_SCOPE_FUNCTION > 0 {
    //       //   scope.functions.push(name);
    //       // } else {
    //       //   scope.lexical.push(name);
    //       // }

    //       if binding_type & Flags::BIND_SCOPE_LEXICAL > 0 {
    //         // self.maybeExportDefined(scope, name);
    //       }
    //     } else if binding_type & Flags::BIND_SCOPE_VAR > 0 {
    //       // for (let i = self.scopeStack.length - 1; i >= 0; --i) {
    //       //   scope = self.scopeStack[i];
    //       //   self.checkRedeclarationInScope(scope, name, bindingType, pos);
    //       //   scope.var.push(name);
    //       //   self.maybeExportDefined(scope, name);

    //       //   if (scope.flags & SCOPE_VAR) break;
    //       // }
    //     }
    //     if self.inModule && (scope.flags & Flags::SCOPE_PROGRAM > 0) {
    //       // self.undefinedExports.remove(name);
    //     }
    //   }
    // }

    fn maybe_export_defined(&mut self, scope: Scope, name: &str) {
        if self.in_module && (scope.flags & Flags::SCOPE_PROGRAM > 0) {
            self.undefined_exports.remove(name);
        }
    }

    fn check_redeclaration_in_scope(&self, scope: &Scope, name: &str, binding_type: u16) {
        if self.is_redeclared_in_scope(scope, name, binding_type) {
            // self.raise(pos, Errors.VarRedeclaration, name);
            panic!("Error");
        }
    }

    fn is_redeclared_in_scope(&self, scope: &Scope, name: &str, binding_type: u16) -> bool {
        if binding_type & Flags::BIND_KIND_VALUE == 0 {
            return false;
        };

        if binding_type & Flags::BIND_SCOPE_LEXICAL > 0 {
            return scope.lexical.iter().any(|i| i == name)
                || scope.functions.iter().any(|i| i == name)
                || scope.var.iter().any(|i| i == name);
        }

        if binding_type & Flags::BIND_SCOPE_FUNCTION > 0 {
            return scope.lexical.iter().any(|i| i == name)
                || (!self.treat_functions_as_var_in_scope(scope)
                    && scope.var.iter().any(|i| i == name));
        }

        match scope.lexical.get(0) {
            Some(s) => {
                (scope.lexical.iter().any(|i| i == name)
                    && !((scope.flags & Flags::SCOPE_SIMPLE_CATCH > 0) && s == name))
                    || (!self.treat_functions_as_var_in_scope(scope)
                        && scope.functions.iter().any(|i| i == name))
            }
            None => {
                !self.treat_functions_as_var_in_scope(scope)
                    && scope.functions.iter().any(|i| i == name)
            }
        }
    }

    // // TODO:
    //   checkLocalExport(id: N.Identifier) {
    //     if (
    //       this.scopeStack[0].lexical.indexOf(id.name) === -1 &&
    //       this.scopeStack[0].var.indexOf(id.name) === -1 &&
    //       // In strict mode, scope.functions will always be empty.
    //       // Modules are strict by default, but the `scriptMode` option
    //       // can overwrite this behavior.
    //       this.scopeStack[0].functions.indexOf(id.name) === -1
    //     ) {
    //       this.undefinedExports.set(id.name, id.start);
    //     }
    //   }

    fn current_scope(&self) -> Option<&Scope> {
        self.scope_stack.last()
    }

    fn current_var_scope(&self) -> Option<&Scope> {
        for scope in &self.scope_stack {
            if (scope.flags & Flags::SCOPE_VAR) > 0 {
                return Some(scope);
            }
        }
        None
    }

    // Could be useful for `arguments`, `this`, `new.target`, `super()`, `super.property`, and `super[property]`.
    fn current_this_scope(&self) -> Option<&Scope> {
        for scope in &self.scope_stack {
            if ((scope.flags & Flags::SCOPE_VAR > 0) || (scope.flags & Flags::SCOPE_CLASS) > 0)
                && (scope.flags & Flags::SCOPE_ARROW == 0)
            {
                return Some(scope);
            }
        }
        None
    }
}
