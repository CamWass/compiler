const passingResponse: [number?, boolean] = [];

///////////////////////////////////
// enum Color {
//   Aqua = "#00ffff",
//   Cyan = Aqua,
// }
///////////////////////////////////
// const obj1 = { prop1: 1 };
// const obj2 = { prop2: 2 };
// window.func = function () {
//   this.otherProp = 10;
//   this.prop1;
// };
// obj2.f = window.func;
// obj1.inner = obj1;
///////////////////////////////////
// var a;
// function f1() {
//     a = 1;
// }
///////////////////////////////////

// (function () {
//   const obj = { prop: { prop1: 1 } };
//   function foo(arg) {
//     arg[window.er];
//   }
//   foo(obj);
//   obj.prop.prop1;
// })();
// (function () {
//   var obj = { prop1: 1 };
//   function f() {
//     obj[window.foo];
//   }
//   window.a = f;
// })();
// (function () {
//   var obj = { prop1: 1 };
//   function f() {
//     obj[window.foo];
//   }
//   obj = { prop2: 2 };
//   window.a = f;
// })();
// (function () {
//   function f1() {
//     let variable = { prop1: 1, prop1: 1, prop1: 1 };
//     function get() {
//       return variable;
//     }

//     f2(get); // variable = { prop1: 1, prop2: 2 };
//     variable.prop2; // 2
//     return variable;
//   }
//   function f2(func) {
//     let obj = func();
//     obj.prop2 = 2;
//   }
//   f1().prop2;
// })();

// function assignProp1(arg) {
//   arg.inner = { prop1: 1 };
// }
// function assignProp2(arg) {
//   arg.inner = { prop2: 2 };
// }
// function foo(arg) {
//   if (cond) {
//     assignProp1(arg);
//   } else {
//     assignProp2(arg);
//   }
//   return arg;
// }
// foo({ prop: 1 }).inner.prop2;

// (function () {
//   function foo() {
//     console.log("foo");
//   }

//   function bar() {
//     console.log("bar");
//   }
//   // we wont detect that foo and bar are called here; they should be invalidated
//   /*
//         could this (and similar expressions) be transformed into:

//         if (Math.random() > 0.5) {
//             this["foo"]();
//         } else {
//             this["bar"]();
//         }
//     */
//   this[Math.random() > 0.5 ? "foo" : "bar"]();
// })();

// (function () {
//   function foo(arg) {
//     let value = null;
//     return {
//       set: () => {
//         value = arg;
//       },
//       get: () => {
//         return value;
//       },
//     };
//   }
//   const o = foo({ prop1: 1, prop2: 2, zCommon: 3 });
//   const v1 = o.get(); // null
//   o.set();
//   const v2 = o.get(); // { prop1: 1, prop2: 2, zCommon: 3 }
//   v2.zCommon;
// })();

// (function () {
//   function foo() {
//     const o = { prop1: 1, prop2: 2 };
//     function bar() {
//       return o;
//     }
//     return bar;
//   }
//   const get = foo();
//   const o = get();
// })();
// (function () {
//   function foo() {
//     return { prop1: 1, prop2: 2 };
//   }
//   var foo = 1;
//   function bar() {
//     foo().prop2;
//     return foo();
//   }
//   bar().prop2;
// })();
// (function () {
//   function f1() {
//     function f2(arg) {
//       function f3() {
//         return arg;
//       }
//       return f3;
//     }
//     const obj = { prop1: 1, prop2: 2 };
//     let f = f2(obj);
//     return f;
//   }
//   const get = f1();
//   const obj = get().prop2; // requires that functions can return functions.
// })();

///////////////////////////////////
// function foo(a) {
//   a.common;
// }
// foo({ prop1: 1, common: true });
// foo({ prop2: 2, common: true });
///////////////////////////////////

// function isSymbolUsedInConditionBody(expr, body, testedNode, testedSymbol) {
//     return !!ts.forEachChild(body, function check(childNode) {
//         return ts.forEachChild(childNode, check);
//     });
// }
///////////////////////////////////

// function access(arg) {
//     arg.prop1;
// }
// const obj = { prop1: 1 };
// access(obj || window.invalid);

// undefined = 1;

//////////////////////////

// function and(f, g) {
//     return function (arg) { return f(arg) && g(arg); };
// }

// function f1() {
//     let variable = { prop1: 1 };

//     function modifyVariable(arg) {
//         variable = arg;
//     }

//     modifyVariable({ prop2: 2 });

//     variable.prop2;
// }

// {
//     function f2() {
//         let variable = { initial: 1 };

//         function set1() {
//             variable = { prop1: 1};
//         }
//         function set2() {
//             variable = { prop2: 2};
//         }
//         function get() {
//             return variable;
//         }

//         return {
//             set1,
//             set2,
//             get,
//         };
//     }
//     const obj = f2();
//     const initial = obj.get();
//     obj.set1();
//     const p1 = obj.get();
//     obj.set2();
//     const p2 = obj.get();

//     initial.initial;
//     p1.prop1;
//     p2.prop2;
// }

////////////////////////////////////////

// function f() {
//   // if (invokeMap && invokeMap.get(rootDirName) === true);

//   // let prop;
//   // let obj;
//   // let arr = [];
//   // ({ prop1: prop = def } = { prop1: 1 }).prop1;

//   // op = [op[0] & 2, t.value];

//   // var gMap = globals === null || globals === void 0 ? void 0 : globals.Map;

//   // if (map[mid] <= code && code <= map[mid + 1]);

//   // a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= a= 1;

//   // if (pack || arguments.length === 2);

//   // Comparison[(Comparison["LessThan"] = -1)] = "LessThan";

//   var gMap = (1,b((false, 1)));

// }

// function b() {}

// (function () {
//   Map = getCollectionImplementation("Map", "tryGetNativeMap", "createMapShim");
//   function getCollectionImplementation(name, nativeFactory, shimFactory) {}
// })();

// function getTypeFromTypeNode() {
//   return getConditionalFlowTypeOfType();
// }
// function getTypeFromTypeNodeWorker(e) {
//   getTypeFromTypeNode(e.type);
// }
// function instantiateMappedType() {
//   if (a) instantiateMappedType();
//   while (true) {
//     var e = getTypeFromTypeNode(root.node);
//     if (!e) return;
//     root = e;
//   }
// }
////////////////////////////////////////////

// let obj = { name: "dave" };
// while (someCond) {
//   obj = { inner: obj };
// }

////////////////////

// // interface Node {
// //   parent: Node | null,
// //   x:
// // }

// function depth(obj: Node) {
//   if (obj.parent) {
//     // `depth` hasn't been checked yet. A restriction should be made
//     return depth(obj.parent) + 1;
//   } else {
//     return 0;
//   }
// }

///////////////////////////

// let obj = { name: "dave" };
// for (let i = 0; i < 10; i++) {
//   obj = { inner: obj };
// }

// let inner = obj;
// for (let i = 0; i < 10; i++) {
//   inner = inner.inner;
// }

// function depth(obj) {
//   if (obj.inner) {
//     // `depth` hasn't been checked yet. A restriction should be made
//     return depth(obj.inner) + 1;
//   } else {
//     return 0;
//   }
// }
// depth(obj);
// console.log(obj, inner);

////////////////////////////////////

// class Foo {
//   constructor(private a: string) {}
//   foo(): void;
//   foo(): void {}
// }

// function a(): void;
// function a(): void {}

// class Bar {
//   constructor(private a: string, private b: string, private c: string);
//   // constructor(private a: string) {}
// }

//////////////////////////////////////////////////////////////////

// class C2 {
//     x: string;
//     y: string;

//     constructor() {
//         let self = this;                // ok
//         let { x, y: y1 } = this;        // ok
//         ({ x, y: y1, "y": y1 } = this); // ok
//     }
// }

// let { x, y: y1 } = { x: 1, y: 2 };

//////////////////////////////////////////////////////////////////

// class A {
//     static create() { return new A(); }
//     aMethod(): boolean {
//         return false;
//     }
// };
// class B {
//     static create() { return new B(); }
//     aMethod(): number {
//         return 1;
//     }
// };
// const cond = !!document.getElementById("");
// let cls = document.getElementById("") ? A : B;
// const a = A.create();
// const b = B.create();
// const obj = cls.create();

// if (cond) {
//     const c = obj;
// } else {
//     const c = obj;
// }

// const k = document.getElementById("") ? A.create() : B.create();

//////////////////////////////////////////////////////////////////

// interface Person {
//     name: string;
//     age: number;
// }

// // function isPerson(o: any): o is Person {
// //     return "name" in o && "age" in o
// // }

// function parse(input: string): Person {
//     const person = JSON.parse(input);
//     person.name;
//     person.foo;

//     (person as Person).name;
//     (person as Person).age;

//     // if (isPerson(person)) {
//     //     // do something with now correctly typed object
//     //     person.name;
//     //     person.age;
//     // } else {
//     //     // error handling; invalid JSON format
//     //     person.name;
//     //     person.foo;
//     // }

//     return person;
// }

// const bob: Person = { name: "bob", age: 10 };
// bob.name;
// bob.age;

//////////////////////////////////////////////////////////////////

// function f() {
//     interface Person {
//         name: string;
//         age: number;
//     }

//     const bob: Person = { name: "bob", age: 10 };
//     bob.name;
//     bob.age;

//     function speak(person: Person) {
//         console.log(person.name);
//     }

//     interface Adult extends Person {
//         job: string;
//     }

//     const tim: Adult = { name: "tim", age: 20, job: "baker" };
//     tim.name;
//     tim.age;
//     tim.job;

//     class FireFighter implements Adult {
//         job = "firefighter";
//         // TODO: AmbiguateProperties (and possibly disambiguation) doesnt account
//         // for these param props:
//         constructor(public name: string, public age: number) { }

//     }

//     const alex = new FireFighter("alex", 40);
//     alex.name;
//     alex.age;
//     alex.job;
// }

////////////////////////////////////////////////////////

// class Foo0 {
//   f() { }
// }

// class Foo1 extends Foo0 {
//   f() { }
// }

// class Other {
//   f: string;
// }

////////////////////////////////////////////////////////////////////////

// class Foo0 {
//   f() {}
// }

// class Foo1 extends Foo0 {
//   f() {}
// }

// class Other {
//   f() {}
// }

// ////////////////////////////////////////////////////////////////////////

// class Foo0 {
//   f() {}
// }

// class Foo1 extends Foo0 {}
// class Foo2 extends Foo1 {}
// class Foo3 extends Foo2 {}
// class Foo4 extends Foo3 {}

// class Foo5 extends Foo4 {
//   f() {}
// }

// class Other {
//   f() {}
// }

////////////////////////////////////////////////////////////

// class A {
//   private foo: boolean;
//   private bar: boolean;
//   private baz: boolean;
// }

// interface A {
//   name: "Jeff";
// }

// interface B {
//   name: "Tim";
// }

// class Foo<T> {
//   person: T;
//   constructor() {}
// }

// const a = new Foo<A>();
// const b = new Foo<B>();

// const aName = a.person.name;
// const bName = b.person.name;

////////////////////////////////////////////////

// interface A {
//   name: "Jeff";
// }

// interface B {
//   name: "Tim";
// }

// class Foo<T> {
//   // person: T;
//   constructor(public person: T) {}
// }

// const a = new Foo<A>({ name: "Jeff" });
// const b = new Foo<B>({ name: "Tim" });

// const aName = a.person.name;
// const bName = b.person.name;

///////////////////////////////////////////////

// class A {
//   name: "Jeff";
//   constructor() {
//     this.name = "Jeff";
//   }
// }

// class B {
//   name: "Tim";
//   constructor() {
//     this.name = "Tim";
//   }
// }

// class Foo<T> {
//   // person: T;
//   constructor(public person: T) {}
// }

// const a = new Foo<A>(new A());
// const b = new Foo<B>(new B());

// const aName = a.person.name;
// const bName = b.person.name;
