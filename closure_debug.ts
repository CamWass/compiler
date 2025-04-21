import fs from "fs";
import https from "https";
import { URL, URLSearchParams } from "url";

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

function fetch(url: URL): Promise<string> {
  const chunks: Buffer[] = [];
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

type ColorId = string;

interface Node {
  color?: ColorId;
  type: string;
  loc: string;
  children: Node[];
}

interface Color {
  id: ColorId;
  prototypes: ColorId[];
  instanceColors: ColorId[];
  invalidating: boolean;
  propertiesKeepOriginalName: boolean;
  constructor: boolean;
  ownProperties: ColorId[];
  boxId: ColorId | null;
  closureAssert: boolean;
  unionElements: ColorId[];
}

interface Result {
  root: Node;
  colors: Color[];
}

// function extract_color_id(color: string): ColorId {
//   return color.match(/id=(\w+)/)[1];
// }

(async function () {
  const url = new URL("https://closure-compiler-debugger.appspot.com/compile");
  url.search = params.toString();

  const res = await fetch(url);

  const startTag = '<textarea id="ast" readonly rows="20" cols="120">';

  const start = res.indexOf(startTag) + startTag.length;

  const endTag = "</textarea>";

  const end = res.indexOf(endTag, start);

  let ast = res.substring(start, end);

  let lines = ast.split("\n");

  let script_count = 0;
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.startsWith("SCRIPT")) {
      if (script_count === 1) {
        lines = lines.slice(i, lines.length);
        break;
      }
      script_count++;
    }
  }

  const type_regex = /\w+/;
  const loc_regex = /\d+:\d+/;

  const nodes = [];
  const colors: Map<ColorId, Color> = new Map();

  function isAlphaNumeric(str: string): boolean {
    let code, i, len;

    for (i = 0, len = str.length; i < len; i++) {
      code = str.charCodeAt(i);
      if (
        !(code > 47 && code < 58) && // numeric (0-9)
        !(code > 64 && code < 91) && // upper alpha (A-Z)
        !(code > 96 && code < 123)
      ) {
        // lower alpha (a-z)
        return false;
      }
    }
    return true;
  }

  function parseColor(color_text: string): ColorId {
    let i = 0;

    function is(s: string): boolean {
      return color_text.substring(i).startsWith(s);
    }

    function eat(s: string): boolean {
      if (is(s)) {
        i += s.length;
        return true;
      } else {
        return false;
      }
    }

    function expect(s: string) {
      if (!eat(s)) {
        throw Error(`expected: "${s}", remaining input: "${color_text.substring(i)}"`);
      }
    }

    function parseIdent(): string {
      let r = "";
      while (i < color_text.length && isAlphaNumeric(color_text[i])) {
        r += color_text[i];
        i++;
      }
      return r;
    }

    function parseIdentArray(): string[] {
      expect("[");
      const res: string[] = [];
      while (i < color_text.length && !is("]")) {
        if (isAlphaNumeric(color_text[i])) {
          res.push(parseIdent());
          eat(",");
          skipSpace();
        } else {
          break;
        }
      }
      expect("]");
      return res;
    }

    function skipSpace() {
      while (i < color_text.length && is(" ")) {
        i++;
      }
    }

    function parseColorArray(): ColorId[] {
      expect("[");
      const res: string[] = [];
      while (i < color_text.length && !is("]")) {
        if (is("Color{")) {
          res.push(parseColorInner());
          eat(",");
          skipSpace();
        } else {
          break;
        }
      }
      expect("]");
      return res;
    }

    function parseBool(): boolean {
      if (eat("true")) {
        return true;
      } else if (eat("false")) {
        return false;
      } else {
        throw Error("expected boolean");
      }
    }

    function parseColorInner(): ColorId {
      expect("Color");
      expect("{");

      expect("id=");
      const id = parseIdent();
      expect(",");
      skipSpace();

      // expect("debugInfo=DebugInfo{compositeTypename=},");
      // skipSpace();

      expect("prototypes=");
      const prototypes = parseColorArray();
      expect(",");
      skipSpace();

      expect("instanceColors=");
      const instanceColors = parseColorArray();
      expect(",");
      skipSpace();

      expect("invalidating=");
      const invalidating = parseBool();
      expect(",");
      skipSpace();

      expect("propertiesKeepOriginalName=");
      const propertiesKeepOriginalName = parseBool();
      expect(",");
      skipSpace();

      expect("constructor=");
      const constructor = parseBool();
      expect(",");
      skipSpace();

      expect("ownProperties=");
      const ownProperties = parseIdentArray();
      expect(",");
      skipSpace();

      expect("boxId=");
      const boxIdText = parseIdent();
      const boxId = boxIdText === "null" ? null : boxIdText;
      expect(",");
      skipSpace();

      expect("closureAssert=");
      const closureAssert = parseBool();
      expect(",");
      skipSpace();

      expect("unionElements=");
      const unionElements = parseColorArray();
      eat(",");
      skipSpace();

      expect("}");

      colors.set(id, {
        id,
        prototypes,
        instanceColors,
        invalidating,
        propertiesKeepOriginalName,
        constructor,
        ownProperties,
        boxId,
        closureAssert,
        unionElements,
      });

      return id;
    }

    return parseColorInner();
  }

  const indent_size = 4;

  let lineNum = 0;

  const root = parseNode(lines[lineNum++]);

  let currentIndent = 0;

  function parseNode(line: string): Node {
    const node: Node = {
      type: type_regex.exec(line)![0],
      loc: loc_regex.exec(line)![0],
      color: undefined,
      children: [],
    };

    const color_idx = line.indexOf("Color");

    if (color_idx !== -1) {
      let color_text = line.substring(color_idx);
      const color_id = parseColor(color_text);
      node.color = color_id;
    }

    return node;
  }

  const parentStack: Node[] = [];
  let prevNode = root;

  for (; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];
    if (line.trim().length !== 0) {
      const node = parseNode(line);

      const indentation = (line.length - line.trimStart().length) / indent_size;

      if (indentation > currentIndent) {
        parentStack.push(prevNode);
      } else if (indentation < currentIndent) {
        while (currentIndent !== indentation) {
          parentStack.pop();
          currentIndent--;
        }
      }

      parentStack[parentStack.length - 1].children.push(node);

      currentIndent = indentation;
      prevNode = node;
    }
  }

  const result: Result = { root, colors: [...colors.values()] };

  await fs.promises.writeFile(
    `${__dirname}/closure_ast.json`,
    JSON.stringify(result, undefined, 2)
  );
})();
