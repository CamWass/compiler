function* foo(a = function* inner() {yield;}) {}


// // function func1() {
// //     this.inner.prop3;
// // }
// const func2 = function fun() {
//     this.inner.prop4;
// }

// const obj = {
//     inner: {
//         // prop1: 1,
//         // prop2: 2,
//         // prop3: 3,
//         prop4: 4,
//     },
//     // method1() {
//     //     this.inner.prop1;
//     // },
//     // method2: function() {
//     //     this.inner.prop2;
//     // },
//     // method3: func1,
//     method4: func2,
// };

////////////////////////////////
// const array = [{ prop1: 1 }];
// array[0].prop1;

// {
//     const obj = { prop: { inner: 1 } };
//     const thing1 = obj[unknown];
//     const thing2 = obj.prop.inner;
// }
// {
//     const obj = { outer: { inner: 1 } };
//     const ting = obj.outer;
//     const prop = obj[unknown];
//     prop.inner;
// }
// const a = 1;
// var ar  = Array.call();

// {
//     let v = { v };
//     v = v.v;
// }

// {
//     let a = 1;
//     const b = a;
//     a = b;

//     a.prop;
//     b.prop;
// }

// {
//     function foo() {
//         return 1;
//     }

//     const a = foo;
//     const b = a();
// }

// {
//     const a = function (param) { return param }
//     const b = a(a);
// }

// {
//     const obj = {
//         prop: obj
//     };
//     const thing = obj || obj.prop;

//     const other = obj.prop;

//     other.apple = 1;
// }
// {
//     const obj = {
//         prop: { inner: obj }
//     };
//     const thing = obj || obj.prop || obj.prop.inner;
// }
// {
//     const obj = {
//         outer1: { prop1: 1 },
//         outer2: { prop2: 2 }
//     };
//     const thing = obj.outer1 || obj.outer2;
//     const a = thing.prop1;
//     const b = thing.prop2;
// }
// {
//     const obj = {
//         outer1: { inner1: 1, shared: { p: 1 } },
//         outer2: { inner2: 2, shared: { foo: 4, p: 2 } }
//     };
//     const thing = obj.outer1.shared || obj.outer2.shared;
//     // obj.outer1.inner1;
//     // obj.outer2.inner2;
//     // (prop.inner1 || prop.inner2).shared.p;
// }
// {
//     const obj = { outer1: { inner1: 1, shared: 1 }, outer2: { inner2: 2, shared: 2 } };
//     obj.outer1.inner1;
//     obj.outer2.inner2;
//     const prop = obj[unknown];
//     prop.inner1;
//     prop.inner2;
// }
// {
//     const o = "outer";
//     const obj = { [o]: { inner: 1 } };
//     const prop = obj[unknown];
// }

////////////////////////////////
// const obj = { inner: { prop1: 1 } };
// const other = obj;
// const inner = obj.inner;
// window.foo = other;
// other.inner.prop1;
// obj.inner.prop1;
// inner.prop1;
////////////////////////////////
// function addInner(a) {
//     a.inner = { zCommon: 1, prop3: 3 };
//     return a;
// }
// function getInner(a) {
//     if (noInner) {
//         return addInner(a).inner;
//     } else {
//         return a.inner;
//     }
// }
// function f3() {
//     let obj = { inner: { zCommon: 1, prop2: 2 } };
//     if (cond) {
//         return obj;
//     }
//     const a = getInner(obj);
//     a.zCommon;
//     return a;
// }
// const o = f3();
// const i = o.inner;
// i.zCommon; i.zCommon; i.zCommon;
// i.prop3;
// o.prop3;
////////////////////////////////
// let obj = { inner: { zCommon: /e/, prop2: "", common1: 1, common2: 2, common2: 2 } };
// obj.inner = { zCommon: 1n, prop3: true, common1: 1, common2: 2 };
// obj.inner.prop2;
////////////////////////////////
// function addInner(arg1) {
//     arg1.inner = { zCommon: 1, prop3: true };
//     return arg1;
// }
// function getInner(arg2) {
//     return addInner(arg2).inner;
// }
// let obj = { inner: { zCommon: 1, prop2: "" } };
// const a = getInner(obj);
// a.prop3;
// a.zCommon;
////////////////////////////////
// // Extra arg. Should not invalidate
// function func(param1) {}
// func(1, {prop: 1});
////////////////////////////////
// // Arguments array invalidation
// function identity(arg) {
//     arguments[something];
//     arg.prop;
//     return arg;
// }
// const res = identity({prop: 1}).prop;
////////////////////////////////
// // Arguments array invalidation
// function identity() {
//     return arguments[0];
// }
// const res = identity({prop: 1}).prop;
////////////////////////////////
// // Basic invalidation
// const obj = { prop: 1 };
// obj.prop;
// obj[something];
// obj.prop;
////////////////////////////////
// function identity(arg) {
//     return arg;
// }
// const res = identity(1);
////////////////////////////////
// function identity(arg) {
//     return arg;
// }
// const res = identity({prop: 1}).prop;
////////////////////////////////
// function mk() {
//     return function inner() {return { prop: 1 } };
// }
// function foo() {
//     return mk()();
// }
// const res = foo() || { other: 2, prop: 1 };
// const value = res.prop;
////////////////////////////////
// function mk() {
//     return function inner() {return { prop: 1 } };
// }
// function foo() {
//     return mk()();
// }
// const res = foo().prop;
////////////////////////////////
// function mk() {
//     return function inner() {return 1};
// }
// function foo() {
//     return mk()();
// }
// const res = foo();
////////////////////////////////
// function foo() {
//     const a = { x: 1 };
//     const b = { y: 2 };
//     return a.x || b.y;
// }
// const res = foo();
////////////////////////////////
// function foo() {
//     const a = { x: 1, b: 2 };
//     return a.x || a.b;
// }
// const res = foo();
////////////////////////////////
// function foo() {
//     return 1;
// }
// function bar() {
//     return foo();
// }
// const res = bar();
////////////////////////////////
// function foo() {
//     return 1;
// }
// function bar() {
//     return 2;
// }
// const func = foo || bar || 1;
// const res = func();
////////////////////////////////
// const foo = 1;
// foo[someExpr]
// obj[someExpr] = foo
// unresolved = foo
// unresolved.prop = foo
// unresolved(foo)
// unresolved.prop(foo)
// function f(foo) {
//   arguments[i]
// }
// new foo()
// new thing(foo)
// yield foo
// tag`{foo}`
////////////////////////////////
// (function () {
//     window.foo = function() {
//         this.prop = 2;
//     }
//     const obj = { prop: 1, f: window.foo };
//     console.log(obj.prop);
//     obj.f();
//     console.log(obj.prop);
// })();
////////////////////////////////
// (function () {
//     function foo(arg) {
//         arg.prop = 1;
//         return { prop1: 1 };
//     }
//     const obj1 = { prop: 1 };
//     const obj2 = foo(obj1);
//     obj1.prop;
//     obj2.prop1;
//     window.foo = foo;
// })();
////////////////////////////////
// (function () {
//     function inner() {
//         return { prop1: 1, prop2: 2 };
//     }
//     function outer() {
//         window.foo = inner;
//     }
//     inner().prop2;
//     window.bar = outer;
// })();
////////////////////////////////
// (function () {
//     let v = { prop1: 1 };
//     function f1(arg) {
//         v = arg;
//     }
//     v.prop1;
//     window.foo = f1;
//     v.prop1;
//     window.foo({ prop2: 2, b: "banana" });
//     v.prop2; // should stay "prop2" so value is 2, not "banana"
// })();
////////////////////////////////
// (function () {
//     let v = { prop1: 1 };
//     function f1(arg) {
//         v = arg;
//     }
//     function f2() {
//         f1({ prop1:});
//     }
//     v.prop1;
//     window.foo = f1;
//     v.prop1;
//     window.foo({ prop2: 2, b: "banana" });
//     v.prop2; // should stay "prop2" so value is 2, not "banana"
// })();
////////////////////////////////

// (function (ts) {
//     let a = parseRange("hello");
//     var logicalOrRegExp = /\|\|/g;
//     function parseRange(text) {
//         var alternatives = [];
//         for (var _i = 0, _a = ts.trimString(text).split(logicalOrRegExp); _i < _a.length; _i++) {

//         }
//         return alternatives;
//     }

// })(ts || (ts = {}));

// function getNewCommentDirectives() {
//     var updatedDirective = {
//         range: {},
//         type: type
//     };
//     append(updatedDirective);
//     assert(updatedDirective.range);
// }
// getNewCommentDirectives();

// function f() {
//     const a = {};
//     a.foo = undefined;
//     return a;
// }
// f();

// (function () {
//     const obj = { prop: { prop1: 1 } };
//     function foo(arg) {
//         arg[window.er];
//     }
//     foo(obj);
//     obj.prop.prop1;
// })();

////////////////////////////////

// function foo(arg) {
//     arg.prop2 = 1;
// }

// const obj = { prop1: 1 };
// foo(obj);
// foo(obj);
// obj.prop2;

////////////////////////////////////////
// function addInner(a) {
//     a.inner = { zCommon: 1, prop3: 3 };
//     return a;
// }
// function getInner(a) {
//     if (noInner) {
//         return addInner(a).inner;
//     } else {
//         return a.inner;
//     }
// }
// function f3() {
//     let obj = { inner: { zCommon: 1, prop2: 2 } };
//     if (cond) {
//         return obj;
//     }
//     const a = getInner(obj);
//     a.zCommon;
//     return a;
// }
// const o = f3();
// const i = o.inner;
// i.zCommon; i.zCommon; i.zCommon;
// i.prop3;
// o.prop3;
////////////////////////////////////////

// ({ constructor: 1 } || 1).constructor;
// ({ constructor: 1 } || 1n).constructor;
// ({ constructor: 1 } || true).constructor;
// ({ constructor: 1 } || 'a').constructor;
// "use strict";
// if (a) function f() {};

// "use strict";
// if (a) function *foo(){};

// function getNormalizedType() {
//     while (true) {}
//     return;
// }
// function isRelatedTo() {
//     getNormalizedType()
//     if (cond) {
//         isRelatedTo()
//     }
// }

// function f1(a) {
//     a.prop1;
//     a.prop3;
// }

// function f2() {
//     f1({ prop1: 1, prop2: 2 });
// }

// function f3() {
//     f1({ prop1: 1, prop3: 3 });
// }

// function foo() {}

// if (cond) {
//     foo = { prop1: 1 };
// }
// foo.prop1;

// let obj1 = { prop1: 1 };
// (obj1 || null).prop1;
// let obj2 = { prop2: 2 };
// (obj2 || null).prop2;

// // (null).prop1 = 1;
// // (null).prop2 = 2;

// (undefined).prop1 = 1;
// (undefined).prop2 = 2;

// function assignProp1(arg) {
//     arg.inner = { prop1: 1 };
// }
// function assignProp2(arg) {
//     arg.inner = { prop2: 2 };
// }
// function foo(arg) {
//     if (cond) {
//         assignProp1(arg);
//     } else {
//         assignProp2(arg);
//     }
//     return arg;
// }
// foo({ prop: 1 }).inner.prop2;

// function foo(arg) {
//     let outer = { inner: arg };
//     window.thing = outer;
// }

// let obj = { prop1: 1 };
// foo(obj);

// let a;
// ({ prop1: 1 } || a).prop1;

// function getBaseFileName(path, extensions, ignoreCase) {

//     var extension = 1 ? getAnyExtensionFromPath(name, extensions, ignoreCase) : 1;
//     return extension;
// }

// function getAnyExtensionFromPath(path, extensions, ignoreCase) {

//     var baseFileName = getBaseFileName(path);
//     return "";
// }

// function f4() {}
// function f3() {}

// function f2() {
//     f3();
//     f4();
// }

// function f1() {
//     f2();
// }

// f1();

// function foo() {
//     if (cond) {
//         return foo();
//     } else {
//         return 1;
//     }
// }
// const r = foo();

// function rotate(a, b, c) {
//     if (cond) {
//         return rotate(c, a, b);
//     } else {
//         return a;
//     }
// }
// const res = rotate( { prop1: 1 }, { prop2: 2 }, { prop3: 3 } );
// res.prop2;

// function foo(a) {
//     if (cond) {
//         foo(a);
//     } else {
//         console.log(a);
//     }
// }

// foo();

// function FUNCTION() {
//   let obj = 1;
//   while(true) {
//     3
//     try {
//       2;
//     } catch (e) {
//       4
//     }
//     5
//     if (cond) {
//       break;
//     }
//   }
//   6

// }

// function FUNCTION() {

//   outer: for (let i = 0; i < 5; i++) {
//     inner: for (let j = 0; j < 5; j++) {
//       if (cond) {
//         continue outer;
//       } else {
//         continue inner;
//       }
//     }

//   }

//   // let obj = { prop1: 1 };
//   // do {
//   //     obj = { prop2: 2 };
//   //     do {
//   //       obj = { prop2: 2 };
//   //   } while (cond);
//   // } while (cond);

//   // for (let index = 0; index < array.length; index++) {
//   //   const element = array[index];

//   // }

//   // for (;;) {
//   //   const element = array[index];
//   //   if (cond)break;
//   // }

//   1

//   // for (const key in object) {
//   //   1
//   // }

//   1

// //   try {

// //     for (let index = f(); f(); f()) {
// //       2

// //     }
// // } catch(w){}
// //   1
// }

// function spanInNode(node) {
//   if (node) {
//       var parent = node.parent;
//       switch (node.kind) {
//           case 236 /* VariableStatement */:
//               // Span on first variable declaration
//               return spanInVariableDeclaration(node.declarationList.declarations[0]);
//           case 253 /* VariableDeclaration */:
//           case 166 /* PropertyDeclaration */:
//           case 165 /* PropertySignature */:
//               return spanInVariableDeclaration(node);
//           case 163 /* Parameter */:
//               return spanInParameterDeclaration(node);
//           case 255 /* FunctionDeclaration */:
//           case 168 /* MethodDeclaration */:
//           case 167 /* MethodSignature */:
//           case 171 /* GetAccessor */:
//           case 172 /* SetAccessor */:
//           case 170 /* Constructor */:
//           case 212 /* FunctionExpression */:
//           case 213 /* ArrowFunction */:
//               return spanInFunctionDeclaration(node);
//           case 234 /* Block */:
//               if (ts.isFunctionBlock(node)) {
//                   return spanInFunctionBlock(node);
//               }
//           // falls through
//           case 261 /* ModuleBlock */:
//               return spanInBlock(node);
//           case 291 /* CatchClause */:
//               return spanInBlock(node.block);
//           case 237 /* ExpressionStatement */:
//               // span on the expression
//               return textSpan(node.expression);
//           case 246 /* ReturnStatement */:
//               // span on return keyword and expression if present
//               return textSpan(node.getChildAt(0), node.expression);
//           case 240 /* WhileStatement */:
//               // Span on while(...)
//               return textSpanEndingAtNextToken(node, node.expression);
//           case 239 /* DoStatement */:
//               // span in statement of the do statement
//               return spanInNode(node.statement);
//           case 252 /* DebuggerStatement */:
//               // span on debugger keyword
//               return textSpan(node.getChildAt(0));
//           case 238 /* IfStatement */:
//               // set on if(..) span
//               return textSpanEndingAtNextToken(node, node.expression);
//           case 249 /* LabeledStatement */:
//               // span in statement
//               return spanInNode(node.statement);
//           case 245 /* BreakStatement */:
//           case 244 /* ContinueStatement */:
//               // On break or continue keyword and label if present
//               return textSpan(node.getChildAt(0), node.label);
//           case 241 /* ForStatement */:
//               return spanInForStatement(node);
//           case 242 /* ForInStatement */:
//               // span of for (a in ...)
//               return textSpanEndingAtNextToken(node, node.expression);
//           case 243 /* ForOfStatement */:
//               // span in initializer
//               return spanInInitializerOfForLike(node);
//           case 248 /* SwitchStatement */:
//               // span on switch(...)
//               return textSpanEndingAtNextToken(node, node.expression);
//           case 288 /* CaseClause */:
//           case 289 /* DefaultClause */:
//               // span in first statement of the clause
//               return spanInNode(node.statements[0]);
//           case 251 /* TryStatement */:
//               // span in try block
//               return spanInBlock(node.tryBlock);
//           case 250 /* ThrowStatement */:
//               // span in throw ...
//               return textSpan(node, node.expression);
//           case 270 /* ExportAssignment */:
//               // span on export = id
//               return textSpan(node, node.expression);
//           case 264 /* ImportEqualsDeclaration */:
//               // import statement without including semicolon
//               return textSpan(node, node.moduleReference);
//           case 265 /* ImportDeclaration */:
//               // import statement without including semicolon
//               return textSpan(node, node.moduleSpecifier);
//           case 271 /* ExportDeclaration */:
//               // import statement without including semicolon
//               return textSpan(node, node.moduleSpecifier);
//           case 260 /* ModuleDeclaration */:
//               // span on complete module if it is instantiated
//               if (ts.getModuleInstanceState(node) !== 1 /* Instantiated */) {
//                   return undefined;
//               }
//           // falls through
//           case 256 /* ClassDeclaration */:
//           case 259 /* EnumDeclaration */:
//           case 297 /* EnumMember */:
//           case 202 /* BindingElement */:
//               // span on complete node
//               return textSpan(node);
//           case 247 /* WithStatement */:
//               // span in statement
//               return spanInNode(node.statement);
//           case 164 /* Decorator */:
//               return spanInNodeArray(parent.decorators);
//           case 200 /* ObjectBindingPattern */:
//           case 201 /* ArrayBindingPattern */:
//               return spanInBindingPattern(node);
//           // No breakpoint in interface, type alias
//           case 257 /* InterfaceDeclaration */:
//           case 258 /* TypeAliasDeclaration */:
//               return undefined;
//           // Tokens:
//           case 26 /* SemicolonToken */:
//           case 1 /* EndOfFileToken */:
//               return spanInNodeIfStartsOnSameLine(ts.findPrecedingToken(node.pos, sourceFile));
//           case 27 /* CommaToken */:
//               return spanInPreviousNode(node);
//           case 18 /* OpenBraceToken */:
//               return spanInOpenBraceToken(node);
//           case 19 /* CloseBraceToken */:
//               return spanInCloseBraceToken(node);
//           case 23 /* CloseBracketToken */:
//               return spanInCloseBracketToken(node);
//           case 20 /* OpenParenToken */:
//               return spanInOpenParenToken(node);
//           case 21 /* CloseParenToken */:
//               return spanInCloseParenToken(node);
//           case 58 /* ColonToken */:
//               return spanInColonToken(node);
//           case 31 /* GreaterThanToken */:
//           case 29 /* LessThanToken */:
//               return spanInGreaterThanOrLessThanToken(node);
//           // Keywords:
//           case 115 /* WhileKeyword */:
//               return spanInWhileKeyword(node);
//           case 91 /* ElseKeyword */:
//           case 83 /* CatchKeyword */:
//           case 96 /* FinallyKeyword */:
//               return spanInNextNode(node);
//           case 159 /* OfKeyword */:
//               return spanInOfKeyword(node);
//           default:
//               // Destructuring pattern in destructuring assignment
//               // [a, b, c] of
//               // [a, b, c] = expression
//               if (ts.isArrayLiteralOrObjectLiteralDestructuringPattern(node)) {
//                   return spanInArrayLiteralOrObjectLiteralDestructuringPattern(node);
//               }
//               // Set breakpoint on identifier element of destructuring pattern
//               // `a` or `...c` or `d: x` from
//               // `[a, b, ...c]` or `{ a, b }` or `{ d: x }` from destructuring pattern
//               if ((node.kind === 79 /* Identifier */ ||
//                   node.kind === 224 /* SpreadElement */ ||
//                   node.kind === 294 /* PropertyAssignment */ ||
//                   node.kind === 295 /* ShorthandPropertyAssignment */) &&
//                   ts.isArrayLiteralOrObjectLiteralDestructuringPattern(parent)) {
//                   return textSpan(node);
//               }
//               if (node.kind === 220 /* BinaryExpression */) {
//                   var _a = node, left = _a.left, operatorToken = _a.operatorToken;
//                   // Set breakpoint in destructuring pattern if its destructuring assignment
//                   // [a, b, c] or {a, b, c} of
//                   // [a, b, c] = expression or
//                   // {a, b, c} = expression
//                   if (ts.isArrayLiteralOrObjectLiteralDestructuringPattern(left)) {
//                       return spanInArrayLiteralOrObjectLiteralDestructuringPattern(left);
//                   }
//                   if (operatorToken.kind === 63 /* EqualsToken */ && ts.isArrayLiteralOrObjectLiteralDestructuringPattern(node.parent)) {
//                       // Set breakpoint on assignment expression element of destructuring pattern
//                       // a = expression of
//                       // [a = expression, b, c] = someExpression or
//                       // { a = expression, b, c } = someExpression
//                       return textSpan(node);
//                   }
//                   if (operatorToken.kind === 27 /* CommaToken */) {
//                       return spanInNode(left);
//                   }
//               }
//               if (ts.isExpressionNode(node)) {
//                   switch (parent.kind) {
//                       case 239 /* DoStatement */:
//                           // Set span as if on while keyword
//                           return spanInPreviousNode(node);
//                       case 164 /* Decorator */:
//                           // Set breakpoint on the decorator emit
//                           return spanInNode(node.parent);
//                       case 241 /* ForStatement */:
//                       case 243 /* ForOfStatement */:
//                           return textSpan(node);
//                       case 220 /* BinaryExpression */:
//                           if (node.parent.operatorToken.kind === 27 /* CommaToken */) {
//                               // If this is a comma expression, the breakpoint is possible in this expression
//                               return textSpan(node);
//                           }
//                           break;
//                       case 213 /* ArrowFunction */:
//                           if (node.parent.body === node) {
//                               // If this is body of arrow function, it is allowed to have the breakpoint
//                               return textSpan(node);
//                           }
//                           break;
//                   }
//               }
//               switch (node.parent.kind) {
//                   case 294 /* PropertyAssignment */:
//                       // If this is name of property assignment, set breakpoint in the initializer
//                       if (node.parent.name === node &&
//                           !ts.isArrayLiteralOrObjectLiteralDestructuringPattern(node.parent.parent)) {
//                           return spanInNode(node.parent.initializer);
//                       }
//                       break;
//                   case 210 /* TypeAssertionExpression */:
//                       // Breakpoint in type assertion goes to its operand
//                       if (node.parent.type === node) {
//                           return spanInNextNode(node.parent.type);
//                       }
//                       break;
//                   case 253 /* VariableDeclaration */:
//                   case 163 /* Parameter */: {
//                       // initializer of variable/parameter declaration go to previous node
//                       var _b = node.parent, initializer = _b.initializer, type = _b.type;
//                       if (initializer === node || type === node || ts.isAssignmentOperator(node.kind)) {
//                           return spanInPreviousNode(node);
//                       }
//                       break;
//                   }
//                   case 220 /* BinaryExpression */: {
//                       var left = node.parent.left;
//                       if (ts.isArrayLiteralOrObjectLiteralDestructuringPattern(left) && node !== left) {
//                           // If initializer of destructuring assignment move to previous token
//                           return spanInPreviousNode(node);
//                       }
//                       break;
//                   }
//                   default:
//                       // return type of function go to previous token
//                       if (ts.isFunctionLike(node.parent) && node.parent.type === node) {
//                           return spanInPreviousNode(node);
//                       }
//               }
//               // Default go to parent to set the breakpoint
//               return spanInNode(node.parent);
//       }
//   }

// }

/////////////////

// {

// let obj = { shared: true, prop1: 1 };

// function f1() {
//   obj = { shared: true, prop2: 2 };
//   obj.prop2;
//   obj.prop2;
//   obj.prop2;
// }

// f1();

// obj.shared;

// }

/////////////////

// {

// let shared = { prop1: 1 };
// let obj = { prop1: 1};

// function f1() {
//   let common = shared;
//   obj = { f1: 1 };
// }

// f1();

// obj;

// shared = { prop2: 2 };

// f1();

// obj;

// }

/////////////////

// {
//   function create() {

//     if (window.cond) {
//       let captured = { prop1: 1 };
//       return function inner() {
//         return captured;
//       }
//     } else {
//       let captured = { prop2: 2 };
//       return function inner() {
//         return captured;
//       }
//     }

//   }

//   let f = create();
//   let res = f();
// }
/////////////////
// {
// function outer(a) {
//   function inner(a) {
//     return a;
//   }
//   return inner(a);
// }
// outer({ prop1: 1 }).prop1;
// outer({ prop2: 2 }).prop2;
// outer({ prop1: 1, prop2: 2 }).prop2;
// }

/////////////////

// {
//   () => {
//     1;
//     while (cond1) {
//       2;
//       if (cond2) {
//         3;
//       } else {
//         4;
//         return;
//       }
//       5;
//     }
//     6;
//   }
//   () => {
//     let obj = { prop1: 1 };
//     while (cond1) {
//       if (cond2) {
//         obj = { prop2: 2 };
//       }

//       if (cond3) {
//         break;
//       }
//     }
//   }
// }
/////////////////
// {
// function foo() {
//   let obj = { inner: { prop1: 1 } };
//   if (cond) {
//     return obj;
//   } else {
//     obj.inner = { prop2: 2 };
//     return { obj: obj };
//   }
// }

// // foo().inner.prop1;

// /*
// should be:
// { inner: { prop1: 1 } } or
// { obj: { inner: { prop2: 2 } } }

// { obj: { inner: { prop2: 2 } }, inner: { prop1: 1 } }

// but may incorrectly be
// { inner: { prop1: 1 } } or
// { obj: { inner: { prop1: 1, prop2: 2 } } }

// { obj: { inner: { prop1: 1, prop2: 2 } }, inner: { prop1: 1 } }
// */
// // foo().inner;
// // foo().obj;
// // foo().inner;
// // // foo().inner.prop1;
// // foo().inner.prop2;
// // foo().obj;
// // foo().obj.inner;
// // // foo().obj.inner.prop1;
// foo().obj.inner.prop2;
// }
/////////////////
// {
//   // function get(a) {
//   //   if (inner) {
//   //     return get(a).inner;
//   //   } else if (other) {
//   //     return get(a).outerProp;
//   //   } else if (x) {
//   //     return get(a).innerProp;
//   //   } else if (o) {
//   //     return get(a);
//   //   } else {
//   //     return a;
//   //   }
//   // }
//   function get(inner) {
//     if (inner) {
//       return get(false).inner;
//     } else if (other) {
//       return get(false).outerProp;
//     } else if (x) {
//       return get(false).innerProp;
//     } else {
//       return { inner: { innerProp: { prop1: 1 } }, outerProp: { prop2: 2 } };
//     }
//   }

//   // function foo10() {
//     get(true);
//     // const a = get({ inner: { innerProp: { prop1: 1 } }, outerProp: { prop2: 2 } });
//     // const obj = { inner: { innerProp: { prop1: 1 } } };
//     // const a = get(obj);
//     // obj.outerProp= { prop2: 2 };
//     // const b = get(obj);
//   // }
// }
/////////////////

////////////////////////////////////////////////////////////////////////////////////////

// const other_thing = 2;
// var someVar = 1;

// function someFunc(param1, param2, param3) {
//   var someVar = 1;
//   const eeeeee1 = (p33p3) => {
//     let thingbing = 3;
//   };
// }

// const apppppp = 4;

// function someFunc2(param1x, param2x, param3x) {
//   var someVar = 1;
// }

////////////////////////////////////////////////////////////////////////////////////////

// foo = 0;

// if (1) {
//   var foo = 1;
// } else {
//   var bar = 2;
//   bar;
// }

////////////////////////////////////////////////////////////////////////////////////////

// const other_thing = 2;
// var someVar = 1;

// function someFunc(param1, param2, param3) {
//   var someVar = 1;
//   const eeeeee1 = (p33p3) => {
//     let thingbing = 3;
//   };
// }

// const apppppp = 4;

// function someFunc2(param1x, param2x, param3x) {
//   var someVar = 1;

//   const a0 = 0;
//   const a1 = 1;
//   const a2 = 2;
//   const a3 = 3;
//   const a4 = 4;
//   const a5 = 5;
//   const a6 = 6;
//   const a7 = 7;
//   const a8 = 8;
//   const a9 = 9;
//   const a10 = 10;
//   const a11 = 11;
//   const a12 = 12;
//   const a13 = 13;
//   const a14 = 14;
//   const a15 = 15;
//   const a16 = 16;
//   const a17 = 17;
//   const a18 = 18;
//   const a19 = 19;
//   const a20 = 20;
//   const a21 = 21;
//   const a22 = 22;
//   const a23 = 23;
//   const a24 = 24;
//   const a25 = 25;
//   const a26 = 26;
//   const a27 = 27;
//   const a28 = 28;
//   const a29 = 29;
//   const a30 = 30;
//   const a31 = 31;
//   const a32 = 32;
//   const a33 = 33;
//   const a34 = 34;
//   const a35 = 35;
//   const a36 = 36;
//   const a37 = 37;
//   const a38 = 38;
//   const a39 = 39;
//   const a40 = 40;
//   const a41 = 41;
//   const a42 = 42;
//   const a43 = 43;
//   const a44 = 44;
//   const a45 = 45;
//   const a46 = 46;
//   const a47 = 47;
//   const a48 = 48;
//   const a49 = 49;
//   const a50 = 50;
//   const a51 = 51;
//   const a52 = 52;
//   const a53 = 53;
//   const a54 = 54;
//   const a55 = 55;
//   const a56 = 56;
//   const a57 = 57;
//   const a58 = 58;
//   const a59 = 59;
//   const a60 = 60;
//   const a61 = 61;
//   const a62 = 62;
//   const a63 = 63;
//   const a64 = 64;
//   const a65 = 65;
// }

// someFunc2();
// someFunc2();
// someFunc2();
// someFunc2();
// someFunc2();

////////////////////////////////////////////////////////////////////////////////////////

// if (1) {
//   var foo1 = 1;
// } else {
//   var foo2 = 2;
// }

////////////////////////////////////////////////////////////////////////////////////////

// function foo() {
//   return 5;
// }

// let a = "\u{00df}";

// let f\u{20BB5}d = "";

// let b = /abc/g;

// let c = /\u{0067}/;

// const array1 = ['a', 'b', 'c'];

// for (const element of array1) {
//   console.log(element);
// }

// import '/modules/my-module.js';

// import cun;

// '\ud83d\udc35'

// function fo(a, ...b,) {}
// [...a, ] = b
// (a, ...b,) => 1;
// [...a,]
// [...a,] = 1
// function foo(a, ...const, c) {}

// import "fooo";
// let await = 2;

// \u{0065}num

// (async function(){
//   "use strict";
// //   let a = \u{0061}wait foo;
//   \u{0069}mplements
// })()

// \u{cccccccccsccccccQcXt[uc(~).const[uctor().const[uctor())tbr())

// const foo = 1;
// const foo = 1;

// class Foo {
//   constructor() {}
// }

// new Foo();
// new Foo();

// class Foo0 {
//   f() {}
// }

// class Foo1 extends Foo0 {
//   f() {}
// }

// class Other {
//   f() {}
// }
