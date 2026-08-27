const source = await fetch(new URL("out.js", import.meta.url)).then((r) =>
  r.text(),
);

export const INPUTS = {
  blank: {
    label: "Empty",
    config: `{
  "pretty_print": true,
  "passes": {
    "optimize_arguments_array": false,
    "rename_vars": false,
    "rename_labels": false,
    "coalesce_variable_names": false,
    "optimize_properties": false,
    "fuse_stmts": false,
    "optimise_equality": false,
    "remove_dead_code": false,
    "collapse_variable_declarations": false
  },
  "ecmascript": {
    "dynamicImport": true,
    "importMeta": true,
    "topLevelAwait": true
  }
}`,
    input: "",
  },
  blank: {
    label: "Control flow",
    config: `{
  "pretty_print": true,
  "passes": {
    "optimize_arguments_array": false,
    "rename_vars": false,
    "rename_labels": false,
    "coalesce_variable_names": false,
    "optimize_properties": false,
    "fuse_stmts": false,
    "optimise_equality": false,
    "remove_dead_code": false,
    "collapse_variable_declarations": false
  },
  "ecmascript": {
    "dynamicImport": true,
    "importMeta": true,
    "topLevelAwait": true
  }
}`,
    input: `try {
    for (let i = 0; i < array.length; i++) {
        func1();

        if (someCondition) {
            func3();
            continue;
        }

        func4();
    }
} catch {
    handleError();
} finally {
    func5();
}`,
  },
  properties: {
    label: "Property optimisation",
    config: `{
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
    "importMeta": true,
    "topLevelAwait": true
  }
}`,
    input: `function addInner(a) {
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
`,
  },
  website: {
    label: "This website's source code",
    config: `{
  "pretty_print": false,
  "passes": {
    "optimize_arguments_array": true,
    "rename_vars": true,
    "rename_labels": true,
    "coalesce_variable_names": true,
    "optimize_properties": false,
    "fuse_stmts": true,
    "optimise_equality": true,
    "remove_dead_code": true,
    "collapse_variable_declarations": true
  },
  "ecmascript": {
    "dynamicImport": true,
    "importMeta": true,
    "topLevelAwait": true
  }
}`,
    input: source,
  },
};