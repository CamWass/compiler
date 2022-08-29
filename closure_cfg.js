const fs = require("fs");
const https = require("https");
const { URL, URLSearchParams } = require("url");

const js = fs.readFileSync(`${__dirname}/${process.argv[2]}`, {
  encoding: "utf-8",
});

const params = new URLSearchParams([
  ["externs", ""],
  ["input0", ""],
  ["input1", js],
  ["conformanceConfig", ""],
  ["CHECK_TYPES", "on"],
  ["REWRITE_MODULES_BEFORE_TYPECHECKING", "on"],
  ["CLOSURE_PASS", "on"],
  ["PRESERVE_TYPE_ANNOTATIONS", "on"],
  ["PRETTY_PRINT", "on"],
]);

function fetch(url) {
  const chunks = [];
  return new Promise((resolve, reject) => {
    const req = https.request(
      url,
      {
        method: "POST",
      },
      (res) => {
        res.on("data", (chunk) => chunks.push(Buffer.from(chunk)));
        res.on("error", (err) => reject(err));
        res.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
      }
    );

    req.on("error", (err) => reject(err));

    req.end();
  });
}

function recolour(dot) {
  return dot
    .replaceAll(
      '[label="UNCOND", fontcolor="red", weight=0.01, color="red"]',
      '[label="UNCOND", fontcolor="purple", weight=0.01, color="purple"]'
    )
    .replaceAll(
      '[label="ON_FALSE", fontcolor="red", weight=0.01, color="red"]',
      '[label="ON_FALSE", fontcolor="orange", weight=0.01, color="orange"]'
    )    .replaceAll(
      '[label="ON_TRUE", fontcolor="red", weight=0.01, color="red"]',
      '[label="ON_TRUE", fontcolor="green", weight=0.01, color="green"]'
    );
}

// function removeDeadNodes(dot) {
//   return dot.replaceAll(/node\d+ -> node\d+ \[weight=1\];/g, "");
// }

(async function () {
  const url = new URL("https://closure-compiler-debugger.appspot.com/compile");
  url.search = params.toString();

  const res = await fetch(url);

  const startTag = '<script type="text/dot" class="dot">';

  const start = res.indexOf(startTag) + startTag.length;

  const endTag = "</script>";

  const end = res.indexOf(endTag, start);

  let dot = res.substring(start, end);

  // let dot = fs.readFileSync(`${__dirname}/closure_cfg.dot`, {
  //   encoding: "utf8",
  // });

  dot = recolour(dot);
  // dot = removeDeadNodes(dot);

  await fs.promises.writeFile(`${__dirname}/closure_cfg.dot`, dot);
})();
