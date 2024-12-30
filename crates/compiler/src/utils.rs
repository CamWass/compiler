use ast::*;
use ecma_visit::{Visit, VisitWith};

macro_rules! unwrap_as {
    ($expr:expr, $pat:pat, $res:expr) => {
        match $expr {
            $pat => $res,
            _ => unreachable!(),
        }
    };
}

pub(crate) use unwrap_as;

/// Returns whether the node may have side effects when executed.
pub fn may_have_side_effects(node: &VarDeclOrPat) -> bool {
    may_change_state(node, false)
}

/// Returns true if some node in `n`'s subtree changes application state. If
/// `check_for_new_objects` is true, we assume that newly created mutable objects
/// (like object literals) change state. Otherwise, we assume that they have no
/// side effects.
fn may_change_state(node: &VarDeclOrPat, check_for_new_objects: bool) -> bool {
    let mut visitor = StateChangeFinder {
        may_change_state: false,
        check_for_new_objects,
    };
    node.visit_with(&mut visitor);
    visitor.may_change_state
}

struct StateChangeFinder {
    may_change_state: bool,
    check_for_new_objects: bool,
}

macro_rules! will_change_state {
    ($([$name:ident, $t:ty],)*) => {
        $(
            fn $name(&mut self, _: &$t) {
                self.may_change_state = true;
            }
        )*
    };
}

impl Visit<'_> for StateChangeFinder {
    will_change_state!(
        // Throw is a side-effect by definition.
        [visit_throw_stmt, ThrowStmt],
        // Context switches can conceal side-effects.
        [visit_yield_expr, YieldExpr],
        [visit_await_expr, AwaitExpr],
        // Enhanced for loops are almost always side-effectful; it's not worth checking them
        // further. Particularly, they represent a kind of assignment op.
        [visit_for_of_stmt, ForOfStmt],
        [visit_for_in_stmt, ForInStmt],
        // Variable declarations are side-effects.
        [visit_var_declarator, VarDeclarator],
    );
    // TODO:
    //     case EXPORT:
    //     // Variable declarations are side-effects.
    //     return true;

    // TODO:
    //     // import() expressions have side effects
    //   case DYNAMIC_IMPORT:
    //     return true;

    //   case SUPER:
    //     // The super keyword is a noop on its own.
    //     return false;

    fn visit_array_lit(&mut self, n: &ArrayLit) {
        if self.check_for_new_objects {
            self.may_change_state = true;
        } else {
            n.visit_children_with(self);
        }
    }
    fn visit_object_lit(&mut self, n: &ObjectLit) {
        if self.check_for_new_objects {
            self.may_change_state = true;
        } else {
            n.visit_children_with(self);
        }
    }
    fn visit_regex(&mut self, n: &Regex) {
        if self.check_for_new_objects {
            self.may_change_state = true;
        } else {
            n.visit_children_with(self);
        }
    }

    //   case OBJECT_REST:
    //   case OBJECT_SPREAD:
    //     // Object-rest and object-spread may trigger a getter.
    //     if (assumeGettersArePure) {
    //       break; // We still need to inspect the children.
    //     }
    //     return true;

    //   case ITER_REST:
    //   case ITER_SPREAD:
    //     if (NodeUtil.iteratesImpureIterable(n)) {
    //       return true;
    //     }
    //     break;

    //   case NAME:
    //     // TODO(b/129564961): Consider EXPORT declarations.
    //     if (n.hasChildren()) {
    //       // This is the left side of a var/let/const
    //       return true;
    //     }
    //     break;

    //   case FUNCTION:
    //     // Function expressions don't have side-effects, but function
    //     // declarations change the namespace. Either way, we don't need to
    //     // check the children, since they aren't executed at declaration time.
    //     return checkForNewObjects || NodeUtil.isFunctionDeclaration(n);

    //   case GETTER_DEF:
    //   case SETTER_DEF:
    //   case MEMBER_FUNCTION_DEF:
    //     // simply defining a member function, getter, or setter has no side effects
    //     return false;

    //   case COMPUTED_PROP:
    //     if (n.getParent().isClassMembers()) {
    //       return checkForStateChangeHelper(n.getFirstChild(), checkForNewObjects);
    //     }
    //     if (parent.isObjectPattern() && parent.getLastChild().isObjectRest()) {
    //       // Due to language syntax, only the last child can be an OBJECT_REST.
    //       // `({ ['thisKey']: target, ...rest} = something())`
    //       // The presence of `thisKey` affects what properties get put into `rest`.
    //       return true;
    //     }
    //     break; // Assume that COMPUTED_PROP keys in OBJECT_PATTERN never trigger getters.
    //   case MEMBER_FIELD_DEF:
    //     if (n.isStaticMember()
    //         && n.hasChildren()
    //         && checkForStateChangeHelper(n.getFirstChild(), checkForNewObjects)) {
    //       return true;
    //     }
    //     return false;
    //   case COMPUTED_FIELD_DEF:
    //     if (checkForStateChangeHelper(n.getFirstChild(), checkForNewObjects)
    //         || (n.isStaticMember()
    //             && n.getSecondChild() != null
    //             && checkForStateChangeHelper(n.getSecondChild(), checkForNewObjects))) {
    //       return true;
    //     }
    //     return false;
    //   case CLASS:
    //     return checkForNewObjects
    //         || NodeUtil.isClassDeclaration(n)
    //         // Check the extends clause for side effects.
    //         || checkForStateChangeHelper(n.getSecondChild(), checkForNewObjects)
    //         // Check for class members that are computed properties with side effects.
    //         || checkForStateChangeHelper(n.getLastChild(), checkForNewObjects);

    //   case CLASS_MEMBERS:
    //     for (Node member = n.getFirstChild(); member != null; member = member.getNext()) {
    //       if (checkForStateChangeHelper(member, checkForNewObjects)) {
    //         return true;
    //       }
    //     }
    //     return false;

    fn visit_new_expr(&mut self, n: &NewExpr) {
        if self.check_for_new_objects {
            self.may_change_state = true;
            return;
        }

        if !constructorCallHasSideEffects(n) {
            // see if the constructor parameters have side-effects
            n.visit_children_with(self);
        } else {
            self.may_change_state = true;
        }
    }

    fn visit_call_expr(&mut self, n: &CallExpr) {
        // calls to functions that have no side effects have the no
        // side effect property set.
        if !functionCallHasSideEffects(n) {
            // see if the function parameters have side-effects
            n.visit_children_with(self);
        } else {
            self.may_change_state = true;
        }
    }

    fn visit_tagged_tpl(&mut self, n: &TaggedTpl) {
        if functionCallHasSideEffects(n) {
            self.may_change_state = true;
        } else {
            // Need to look at the children for their possible side-effects.
            n.visit_children_with(self);
        }
    }

    //   case CAST:
    //   case AND:
    //   case BLOCK:
    //   case ROOT:
    //   case EXPR_RESULT:
    //   case HOOK:
    //   case IF:
    //   case PARAM_LIST:
    //   case DEFAULT_VALUE:
    //     // Any context that supports DEFAULT_VALUE is already an assignment. The possiblity of a
    //     // default doesn't itself create a side-effect. Therefore, we prefer to defer the decision.
    //   case NUMBER:
    //   case BIGINT:
    //   case OR:
    //   case COALESCE:
    //   case THIS:
    //   case TRUE:
    //   case FALSE:
    //   case NULL:
    //   case STRINGLIT:
    //   case SWITCH:
    //   case TEMPLATELIT_SUB:
    //   case TRY:
    //   case EMPTY:
    //   case TEMPLATELIT:
    //   case TEMPLATELIT_STRING:
    //     break;
    //   case STRING_KEY:
    //     if (parent.isObjectPattern()) {
    //       // This STRING_KEY names a property being read from.
    //       // Assumption: GETELEM (via a COMPUTED_PROP) never triggers a getter or setter.
    //       if (getPropertyKind(n.getString()).hasGetter()) {
    //         return true;
    //       } else if (parent.getLastChild().isObjectRest()) {
    //         // Due to language syntax, only the last child can be an OBJECT_REST.
    //         // `({ thisKey: target, ...rest} = something())`
    //         // The presence of `thisKey` affects what properties get put into `rest`.
    //         return true;
    //       }
    //     }
    //     break;

    //   case GETELEM:
    //   case OPTCHAIN_GETELEM:
    //     // Since we can't see what property is accessed we cannot tell whether
    //     // obj[someProp]/obj?.[someProp] will
    //     // trigger a getter or setter, and thus could have side effects.
    //     // We will assume it does not. This introduces some risk of code breakage, but the code
    //     // size cost of assuming all GETELEM/OPTCHAIN_GETELEM nodes have side effects is completely
    //     // unacceptable.
    //     break;
    //   case GETPROP:
    //   case OPTCHAIN_GETPROP:
    //     if (getPropertyKind(n.getString()).hasGetterOrSetter()) {
    //       // TODO(b/135640150): Use the parent nodes to determine whether this is a get or set.
    //       return true;
    //     }
    //     break;

    fn visit_assign_expr(&mut self, n: &AssignExpr) {
        if n.op == AssignOp::Assign {
            n.visit_children_with(self);
            return;
        }

        if let PatOrExpr::Pat(lhs) = &n.left {
            if let Pat::Ident(_) = lhs.as_ref() {
                self.may_change_state = true;
                return;
            }
        }

        // Assignments will have side effects if
        // a) The RHS has side effects, or
        // b) The LHS has side effects, or
        // c) A name on the LHS will exist beyond the life of this statement.
        n.visit_children_with(self);
        if self.may_change_state {
            return;
        }
        let member = match &n.left {
            PatOrExpr::Expr(expr) => match expr.as_ref() {
                Expr::Member(m) => Some(m),
                _ => None,
            },
            PatOrExpr::Pat(pat) => match pat.as_ref() {
                Pat::Expr(expr) => match expr.as_ref() {
                    Expr::Member(m) => Some(m),
                    _ => None,
                },
                _ => None,
            },
        };
        if let Some(lhs) = member {
            todo!();
            // // If the object being assigned to is a local object, don't
            // // consider this a side-effect as it can't be referenced
            // // elsewhere.  Don't do this recursively as the property might
            // // be an alias of another object, unlike a literal below.
            // let mut current = &lhs.obj;
            // if NodeUtil.evaluatesToLocalValue(current) {
            //     return;
            // }

            // // A literal value as defined by "isLiteralValue" is guaranteed
            // // not to be an alias, or any components which are aliases of
            // // other objects.
            // // If the root object is a literal don't consider this a
            // // side-effect.
            // while let Some(member) = match current {
            //     ExprOrSuper::Super(_) => None,
            //     ExprOrSuper::Expr(expr) => match expr.as_ref() {
            //         Expr::Member(m) => Some(m),
            //         _ => None,
            //     },
            // } {
            //     current = &member.obj;
            // }

            // if !NodeUtil.isLiteralValue(current, true) {
            //     self.may_change_state = true;
            // }
        }
    }
}

// TODO: port from closure
fn functionCallHasSideEffects<T>(callNode: &T) -> bool {
    true
}

// TODO: port from closure
fn constructorCallHasSideEffects<T>(callNode: &T) -> bool {
    true
}
