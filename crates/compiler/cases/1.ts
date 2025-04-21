class A<T> {
  name: T;
}
// `A<string>` should be recorded as an instantiation/instance type of `A`.
// `name` is therefore known to have type(s) `string`
function f(v: A<string>) {}
