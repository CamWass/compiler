class Foo {
  constructor(){}

  get c() {"4";}
  // error here because there is no parameter.
  // wrong error is thrown;
  // error: A `set` accessor must have a corresponding `get` accessor
  set c() {"5";}
}