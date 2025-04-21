// Shold be minfied to just `console.log("bob")`
(function () {
  class Foo {
    name = "bob";
    constructor() {}
  }
  const f = new Foo();
  console.log(f.name);
})();

// Same as above:
(function () {
  class Bar {
    name: string;
    constructor(name: string) {
      this.name = name;
    }
  }
  const b = new Bar("bob");
  console.log(b.name);
})();

// Same as above:
(function () {
  class Bar {
    constructor(public name: string) {
      this.name = name;
    }
  }
  const b = new Bar("bob");
  console.log(b.name);
})();
