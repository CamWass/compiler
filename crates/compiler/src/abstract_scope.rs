// use crate::static_scope::StaticScope;

// /**
//  * Scope contains information about a variable scope in JavaScript. Scopes can be nested, a scope
//  * points back to its parent scope. A Scope contains information about variables defined in that
//  * scope.
//  *
//  * <p>ES 2015 introduces new scoping rules, which adds some complexity to this class. In particular,
//  * scopes fall into two mutually exclusive categories: <i>block</i> and <i>container</i>. Block
//  * scopes are all scopes whose roots are blocks, as well as any control structures whose optional
//  * blocks are omitted. These scopes did not exist at all prior to ES 2015. Container scopes comprise
//  * function scopes, global scopes, and module scopes, and (aside from modules, which didn't exist in
//  * ES5) corresponds to the ES5 scope rules. This corresponds roughly to one container scope per CFG
//  * root (but not exactly, due to SCRIPT-level CFGs).
//  *
//  * <p>All container scopes, plus the outermost block scope within a function (i.e. the <i>function
//  * block scope</i>) are considered <i>hoist scopes</i>. All functions thus have two hoist scopes:
//  * the function scope and the function block scope. Hoist scopes are relevant because "var"
//  * declarations are hoisted to the closest hoist scope, as opposed to "let" and "const" which always
//  * apply to the specific scope in which they occur.
//  *
//  * <p>Note that every function actually has two distinct hoist scopes: a container scope on the
//  * FUNCTION node, and a block-scope on the top-level BLOCK in the function (the "function block").
//  * Local variables are declared on the function block, while parameters and optionally the function
//  * name (if it bleeds, i.e. from a named function expression) are declared on the container scope.
//  * This is required so that default parameter initializers can refer to names from outside the
//  * function that could possibly be shadowed in the function block. But these scopes are not fully
//  * independent of one another, since the language does not allow a top-level local variable to
//  * shadow a parameter name - so in some situations these scopes must be treated as a single scope.
//  *
//  * @see NodeTraversal
//  */
// pub trait AbstractScope<S: AbstractScope<S, V>, V: AbstractVar<S, V>>: StaticScope {}
