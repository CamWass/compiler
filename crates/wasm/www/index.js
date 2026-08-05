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

const inputSizeLabel = document.getElementById("input-size");
const outputSizeLabel = document.getElementById("output-size");

input.value = DEFAULT_INPUT;
config.value = DEFAULT_CONFIG;

await init();

function run() {
  const inputSize = getStringSizeInBytes(input.value);
  inputSizeLabel.textContent = `${inputSize} bytes`;

  try {
    output.value = process(input.value, config.value);

    const outputSize = getStringSizeInBytes(output.value);

    const delta = inputSize - outputSize;
    const percentDelta = (delta / inputSize) * 100;
    const isDecrease = delta > 0;
    const changeSymbol = isDecrease ? '-' : '+';

    outputSizeLabel.textContent = `${outputSize} bytes • ${changeSymbol}${delta} bytes • ${changeSymbol}${percentDelta.toFixed(1)}%`;
  } catch (e) {
    console.error(e);
    output.value = e;
    outputSizeLabel.textContent = "error";
  }
}

input.addEventListener("input", () => {
  run();
});

config.addEventListener("input", () => {
  run();
});

run();

function getStringSizeInBytes(string) {
  return new Blob([string]).size;
}
