import { TabGroup } from "./tabs.js";
import { INPUTS } from "./examples.js";
import init, { process } from "./wasm/wasm.js";

const inputTabGroup = new TabGroup({
  buttonContainer: document.getElementById("input-tab-group-buttons"),
  tabs: [
    {
      label: "Text",
      contentContainer: document.getElementById("input-text"),
      updateSuccess() {},
      updateError() {},
    },
    {
      label: "AST",
      contentContainer: document.getElementById("input-ast-json"),
      updateSuccess(result) {
        this.contentContainer.value = result.input_ast;
      },
      updateError(error) {
        this.contentContainer.value = error;
      },
    },
  ],
});
const outputTabGroup = new TabGroup({
  buttonContainer: document.getElementById("output-tab-group-buttons"),
  tabs: [
    {
      label: "Text",
      contentContainer: document.getElementById("output-text"),
      updateSuccess(result) {
        this.contentContainer.value = result.output;
      },
      updateError(error) {
        this.contentContainer.value = error;
      },
    },
    {
      label: "AST",
      contentContainer: document.getElementById("output-ast-json"),
      updateSuccess(result) {
        this.contentContainer.value = result.output_ast;
      },
      updateError(error) {
        this.contentContainer.value = error;
      },
    },
  ],
});

const exampleSelect = document.getElementById("example-select");

const inputTextTextArea = document.getElementById("input-text");
const outputTextTextArea = document.getElementById("output-text");

const configTextArea = document.getElementById("config");

const inputSizeLabel = document.getElementById("input-size");
const outputSizeLabel = document.getElementById("output-size");

const DEFAULT_INPUT = INPUTS.properties;

inputTextTextArea.value = DEFAULT_INPUT.input;
configTextArea.value = DEFAULT_INPUT.config;

for (const [inputId, input] of Object.entries(INPUTS)) {
  const option = document.createElement("option");
  option.value = inputId;
  option.textContent = input.label;
  exampleSelect.append(option);
}

await init();

function run() {
  const inputSize = getStringSizeInBytes(inputTextTextArea.value);
  inputSizeLabel.textContent = `${inputSize} bytes`;

  try {
    const result = process(inputTextTextArea.value, configTextArea.value);
    inputTabGroup.updateSuccess(result);
    outputTabGroup.updateSuccess(result);

    const outputSize = getStringSizeInBytes(outputTextTextArea.value);

    const delta = Math.abs(inputSize - outputSize);
    const percentDelta = (delta / inputSize) * 100 || 0;
    const changeSymbol =
      inputSize > outputSize ? "-" : inputSize === outputSize ? "" : "+";

    outputSizeLabel.textContent = `${outputSize} bytes • ${changeSymbol}${delta} bytes • ${changeSymbol}${percentDelta.toFixed(
      1,
    )}%`;
  } catch (e) {
    console.error(e);
    inputTabGroup.updateError(e);
    outputTabGroup.updateError(e);

    outputSizeLabel.textContent = "error";
  }
}

inputTextTextArea.addEventListener("input", () => {
  run();
});

configTextArea.addEventListener("input", () => {
  run();
});

exampleSelect.addEventListener("input", (e) => {
  if (INPUTS[exampleSelect.value]) {
    e.preventDefault();
    inputTextTextArea.value = INPUTS[exampleSelect.value].input;
    configTextArea.value = INPUTS[exampleSelect.value].config;
    exampleSelect.value = "";
    run();
  }
});

run();

function getStringSizeInBytes(string) {
  return new Blob([string]).size;
}
