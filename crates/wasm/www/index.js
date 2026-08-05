
import init, { process } from "./wasm/wasm.js";

await init();

const input = document.getElementById("input");
const output = document.getElementById("output");

function run() {
  try {
    output.value = process(input.value, "{}");
  } catch (e) {
    console.error(e);
    output.value = e;
  }
}

input.addEventListener("input", () => {
  run();
});

run();
