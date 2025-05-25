(function () {
  function foo5() {
    return { prop1: 1, prop2: 2 };
  }
  function foo6() {
    return foo5().prop1 + foo5().prop2;
  }
  function foo7() {
    return foo5().prop1 + foo5().prop2;
  }

  window.a = foo6();
  window.b = foo7();
})();

/*
UglifyJS is able to reduce it to:

!function(){window.a=3,window.b=3}();

Closure, Terser, SWC, don't seem to be able to. e.g. closure:

(function() {
  function a() {
    return {a:1, b:2};
  }
  window.c = a().a + a().b;
  window.d = a().a + a().b;
})();

*/
