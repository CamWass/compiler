
import init, { process } from "./wasm/wasm.js";

await init();

const input = document.getElementById("input");
const output = document.getElementById("output");
const config = document.getElementById("config");

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
