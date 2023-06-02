#!/usr/bin/env node
var execSucc = function (filename) {
  var process = require("child_process");
  var output = null;
  const options = {
    stdio: "pipe",
    encoding: "utf8",
    // timeout: 20 * 1000
  };

  try {
    output = process.execSync("cargo run -- " + filename, options);
    // console.log("out:", output);
  } catch (err) {
    // console.error("error", err);
    return true;
  }
  return false;

  //   if (output === null) {
  //     return false;
  //   }
  //   return output.indexOf("error") === -1;
};
exports.test = execSucc;
