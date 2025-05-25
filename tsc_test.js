const ts = require("./out.js");

const source = 'let x: () => string = () => "string"';
const result = ts.transpileModule(source, {
    compilerOptions: {
        target: ts.ScriptTarget.ES3,
    },
});
const resultText = result.outputText.trim();

const expected = 'var x = function () { return "string"; };';
const eq = resultText === expected;
console.log("equal: ", eq);
console.log("result:", result);