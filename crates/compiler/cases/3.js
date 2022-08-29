class Foo1 {
    a = "hello";
    a(){return "method"}
    static a(){return "static method"}
}

let c = new Foo1;
console.log(c.a); // "hello"
console.log(c.a()); // Type error
console.log(Foo1.a()); // "static method"

// How does the overriding/shadowing behavior of class methods/fields work?
// According to this random article: https://rfrn.org/~shu/2018/05/02/the-semantics-of-all-js-class-elements.html#ex25
/*
Multiple same-named public fields and methods are allowed in the same class declaration, and their initializers are run in order.
Since public methods and fields are properties added with Object.defineProperty, the last field or method overrides all previous ones.
/*
// But in the above example, the last field/method is the method, yet it is overridden by the field.
// This happens in both chrome and firefox.