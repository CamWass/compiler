const util = require("util");
const exec = util.promisify(require("child_process").exec);
const fs = require("fs");
const https = require("https");
const { URL, URLSearchParams } = require("url");
const he = require("he");
const prettier = require("prettier");

function fetch(url, body) {
  const chunks = [];
  return new Promise((resolve, reject) => {
    const req = https.request(
      url,
      {
        method: "POST",
        headers: {
          //   "User-Agent":
          //     "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0",
          //   Referer: "https://closure-compiler.appspot.com/home",
          //   Origin: "https://closure-compiler.appspot.com",
          "Content-type": "application/x-www-form-urlencoded",
        },
      },
      (res) => {
        res.on("data", (chunk) => chunks.push(Buffer.from(chunk)));
        res.on("error", (err) => reject(err));
        res.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
      }
    );

    req.write(body);

    req.on("error", (err) => reject(err));

    req.end();
  });
}

async function getOurCode(normalizedInputFileName) {
  await exec(`cargo run -- ${normalizedInputFileName}`);

  return fs.readFileSync(`${__dirname}/out.ts`, {
    encoding: "utf-8",
  });
}

async function normalize(code) {
  const input = prettier
    .format(code, { parser: "babel" })
    .replaceAll(/\$jscomp\$(\d)+/g, (_match, id) =>
      id == 0 ? "" : `$jscomp$${id - 1}`
    );

  const params = new URLSearchParams([
    ["js_code", input],
    ["compilation_level", "WHITESPACE_ONLY"],
    ["output_info", "compiled_code"],
    ["output_format", "text"],
    ["formatting", "pretty_print"],
  ]);
  const url = new URL("https://closure-compiler.appspot.com/compile");

  //   return fetch(url, params.toString());

  const res = await fetch(url, params.toString());

  return prettier
    .format(res, { parser: "babel" })
    .replaceAll(/\$jscomp\$(\d)+/g, (_match, id) =>
      id == 0 ? "" : `$jscomp$${id - 1}`
    );
}

async function closureRequest(body) {
  const url = new URL("https://closure-compiler-debugger.appspot.com/compile");

  const res = await fetch(url, body);

  const startTag = '<div id="code">';

  const start = res.indexOf(startTag) + startTag.length;

  const endTag = "<br>";

  const end = res.indexOf(endTag, start);

  return he.decode(res.substring(start, end).trim());
}

async function getClosureCode(normalizedInput) {
  const params = new URLSearchParams([
    ["externs", ""],
    ["input0", normalizedInput],
    ["input1", ""],
    ["conformanceConfig", ""],
    ["COALESCE_VARIABLE_NAMES", "on"],
    ["PRETTY_PRINT", "on"],
  ]);

  return closureRequest(params.toString());
  //   const res = await closureRequest(params.toString());

  //   return res.replaceAll(/\$jscomp\$(\d)+/g, (_match, id) =>
  //     id == 0 ? "" : `$jscomp$${id - 1}`
  //   );
}

async function getNormalizedInput(input) {
  const params = new URLSearchParams([
    ["externs", ""],
    ["input0", input],
    ["input1", ""],
    ["conformanceConfig", ""],
    ["PRETTY_PRINT", "on"],
  ]);

  return closureRequest(params.toString());
}

(async function () {
  await fs.promises.mkdir(`${__dirname}/compare_result`);

  const inputFileName = process.argv[2];
  const input = fs.readFileSync(`${__dirname}/${inputFileName}`, {
    encoding: "utf-8",
  });

  const normalizedInput = await getNormalizedInput(input);
  const normalizedInputFileName = `${__dirname}/compare_result/normalizedInput.js`;
  await fs.promises.writeFile(normalizedInputFileName, normalizedInput);

  const getOurs = getOurCode(normalizedInputFileName);
  const getTheirs = getClosureCode(normalizedInput);
  //   const getOurs = getOurCode(normalizedInputFileName).then((ours) =>
  //     normalize(ours)
  //   );
  //   const getTheirs = getClosureCode(normalizedInput).then((theirs) =>
  //     normalize(theirs)
  //   );

  //   console.log("Getting our code");
  //   let ourCode = await getOurCode(inputFileName);
  //   console.log("Normalizing our code");
  //   ourCode = await normalize(ourCode);

  //   console.log("Getting closure code");
  //   let closureCode = await getClosureCode(input);
  //   console.log("Normalizing closure code");
  //   closureCode = await normalize(closureCode);

  const [ours, theirs] = await Promise.all([getOurs, getTheirs]);

  await fs.promises.writeFile(`${__dirname}/compare_result/ours.js`, ours);
  await fs.promises.writeFile(`${__dirname}/compare_result/theirs.js`, theirs);
})();
