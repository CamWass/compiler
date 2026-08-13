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

const inputTextTextArea = document.getElementById("input-text");
const inputAstJsonTextArea = document.getElementById("input-ast-json");
const viewInputTextButton =  document.getElementById("view-input-text-button");
const viewInputAstButton =  document.getElementById("view-input-ast-button");

const outputTextTextArea = document.getElementById("output-text");
const outputAstJsonTextArea = document.getElementById("output-ast-json");
const viewOutputTextButton =  document.getElementById("view-output-text-button");
const viewOutputAstButton =  document.getElementById("view-output-ast-button");

const configTextArea = document.getElementById("config");

const inputSizeLabel = document.getElementById("input-size");
const outputSizeLabel = document.getElementById("output-size");

inputTextTextArea.value = DEFAULT_INPUT;
configTextArea.value = DEFAULT_CONFIG;

await init();

function run() {
  const inputSize = getStringSizeInBytes(inputTextTextArea.value);
  inputSizeLabel.textContent = `${inputSize} bytes`;

  try {
    const result = process(inputTextTextArea.value, configTextArea.value);
    outputTextTextArea.value = result.output;
    outputAstJsonTextArea.value = result.output_ast;
    inputAstJsonTextArea.value = result.input_ast;

    const outputSize = getStringSizeInBytes(outputTextTextArea.value);

    const delta = Math.abs(inputSize - outputSize);
    const percentDelta = (delta / inputSize) * 100;
    const changeSymbol =
      inputSize > outputSize ? "-" : inputSize === outputSize ? "" : "+";

    outputSizeLabel.textContent = `${outputSize} bytes • ${changeSymbol}${delta} bytes • ${changeSymbol}${percentDelta.toFixed(
      1,
    )}%`;
  } catch (e) {
    console.error(e);
    outputTextTextArea.value = e;
    outputSizeLabel.textContent = "error";
  }
}

inputTextTextArea.addEventListener("input", () => {
  run();
});

configTextArea.addEventListener("input", () => {
  run();
});

run();

viewInputTextButton.addEventListener("click", () => {
  inputAstJsonTextArea.style.display = "none";
  inputTextTextArea.style.display = "initial";
});
viewInputAstButton.addEventListener("click", () => {
  inputTextTextArea.style.display = "none";
  inputAstJsonTextArea.style.display = "initial";
});

viewOutputTextButton.addEventListener("click", () => {
  outputAstJsonTextArea.style.display = "none";
  outputTextTextArea.style.display = "initial";
});
viewOutputAstButton.addEventListener("click", () => {
  outputTextTextArea.style.display = "none";
  outputAstJsonTextArea.style.display = "initial";
});

function getStringSizeInBytes(string) {
  return new Blob([string]).size;
}
