
import init, { process } from "./wasm/wasm.js";

const DEFAULT_CONFIG = `{
  "pretty_print": true,
  "passes": {
    "optimize_arguments_array": false,
    "rename_vars": false,
    "rename_labels": false,
    "coalesce_variable_names": false,
    "optimize_properties": true,
    "fuse_stmts": false,
    "optimise_equality": false,
    "remove_dead_code": false,
    "collapse_variable_declarations": false
  },
  "ecmascript": {
    "dynamicImport": true,
    "importMeta": true
  }
}`;

const DEFAULT_INPUT = `function addInner(a) {
    a.inner = { zCommon: 1, prop3: 3 };
    return a;
}

function getInner(a) {
    if (!("inner" in a)) {
        return addInner(a).inner;
    } else {
        return a.inner;
    }
}

function foo() {
    let obj = { inner: { zCommon: 1, prop2: 2 } };
    if (Math.random() > 0.5) {
        return obj;
    }
    const inner = getInner(obj);
    inner.zCommon++;
    return inner;
}

const result = foo();
const inner = result.inner;

inner.zCommon; inner.zCommon; inner.zCommon;
inner.prop3;
result.prop3;
`;

const input = document.getElementById("input");
const output = document.getElementById("output");
const config = document.getElementById("config");

input.value = DEFAULT_INPUT;
config.value = DEFAULT_CONFIG;

await init();

function run() {
  try {
    output.value = process(input.value, config.value);
  } catch (e) {
    console.error(e);
    output.value = e;
  }
}

input.addEventListener("input", () => {
  run();
});

config.addEventListener("input", () => {
  run();
});

run();
