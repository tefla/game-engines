// Slate Standard Library

import {
  type SlateValue,
  type SlateNativeFunction,
  Num,
  Str,
  Bool,
  Null,
  List,
  Record,
  isNumber,
  isString,
  isList,
  isRecord,
  stringify,
  RuntimeError,
} from "@oort/core";

// Helper to create a native function
function native(
  name: string,
  arity: number | "variadic",
  fn: (args: SlateValue[]) => SlateValue
): SlateNativeFunction {
  return { type: "native", name, arity, fn };
}

// Helper to assert argument types
function assertNumber(v: SlateValue, name: string): number {
  if (!isNumber(v)) {
    throw new RuntimeError(`${name} expects a number, got ${v.type}`);
  }
  return v.value;
}

function assertString(v: SlateValue, name: string): string {
  if (!isString(v)) {
    throw new RuntimeError(`${name} expects a string, got ${v.type}`);
  }
  return v.value;
}

function assertList(v: SlateValue, name: string): SlateValue[] {
  if (!isList(v)) {
    throw new RuntimeError(`${name} expects a list, got ${v.type}`);
  }
  return v.elements;
}

// ============ Math Functions ============

export const abs = native("abs", 1, ([x]) => {
  return Num(Math.abs(assertNumber(x, "abs")));
});

export const min = native("min", 2, ([a, b]) => {
  return Num(Math.min(assertNumber(a, "min"), assertNumber(b, "min")));
});

export const max = native("max", 2, ([a, b]) => {
  return Num(Math.max(assertNumber(a, "max"), assertNumber(b, "max")));
});

export const clamp = native("clamp", 3, ([x, lo, hi]) => {
  const xn = assertNumber(x, "clamp");
  const lon = assertNumber(lo, "clamp");
  const hin = assertNumber(hi, "clamp");
  return Num(Math.max(lon, Math.min(hin, xn)));
});

export const lerp = native("lerp", 3, ([a, b, t]) => {
  const an = assertNumber(a, "lerp");
  const bn = assertNumber(b, "lerp");
  const tn = assertNumber(t, "lerp");
  return Num(an + (bn - an) * tn);
});

export const floor = native("floor", 1, ([x]) => {
  return Num(Math.floor(assertNumber(x, "floor")));
});

export const ceil = native("ceil", 1, ([x]) => {
  return Num(Math.ceil(assertNumber(x, "ceil")));
});

export const round = native("round", 1, ([x]) => {
  return Num(Math.round(assertNumber(x, "round")));
});

export const sqrt = native("sqrt", 1, ([x]) => {
  return Num(Math.sqrt(assertNumber(x, "sqrt")));
});

export const pow = native("pow", 2, ([base, exp]) => {
  return Num(
    Math.pow(assertNumber(base, "pow"), assertNumber(exp, "pow"))
  );
});

export const sin = native("sin", 1, ([x]) => {
  return Num(Math.sin(assertNumber(x, "sin")));
});

export const cos = native("cos", 1, ([x]) => {
  return Num(Math.cos(assertNumber(x, "cos")));
});

export const random = native("random", 0, () => {
  return Num(Math.random());
});

// ============ Vector Functions ============

export const vec3 = native("vec3", 3, ([x, y, z]) => {
  const fields = new Map<string, SlateValue>();
  fields.set("x", Num(assertNumber(x, "vec3")));
  fields.set("y", Num(assertNumber(y, "vec3")));
  fields.set("z", Num(assertNumber(z, "vec3")));
  return Record(fields);
});

export const dot = native("dot", 2, ([a, b]) => {
  if (!isRecord(a) || !isRecord(b)) {
    throw new RuntimeError("dot expects two vectors (records)");
  }
  const ax = assertNumber(a.fields.get("x") || Num(0), "dot");
  const ay = assertNumber(a.fields.get("y") || Num(0), "dot");
  const az = assertNumber(a.fields.get("z") || Num(0), "dot");
  const bx = assertNumber(b.fields.get("x") || Num(0), "dot");
  const by = assertNumber(b.fields.get("y") || Num(0), "dot");
  const bz = assertNumber(b.fields.get("z") || Num(0), "dot");
  return Num(ax * bx + ay * by + az * bz);
});

export const normalize = native("normalize", 1, ([v]) => {
  if (!isRecord(v)) {
    throw new RuntimeError("normalize expects a vector (record)");
  }
  const x = assertNumber(v.fields.get("x") || Num(0), "normalize");
  const y = assertNumber(v.fields.get("y") || Num(0), "normalize");
  const z = assertNumber(v.fields.get("z") || Num(0), "normalize");
  const len = Math.sqrt(x * x + y * y + z * z);
  if (len === 0) {
    return v;
  }
  const fields = new Map<string, SlateValue>();
  fields.set("x", Num(x / len));
  fields.set("y", Num(y / len));
  fields.set("z", Num(z / len));
  return Record(fields);
});

export const distance = native("distance", 2, ([a, b]) => {
  if (!isRecord(a) || !isRecord(b)) {
    throw new RuntimeError("distance expects two vectors (records)");
  }
  const ax = assertNumber(a.fields.get("x") || Num(0), "distance");
  const ay = assertNumber(a.fields.get("y") || Num(0), "distance");
  const az = assertNumber(a.fields.get("z") || Num(0), "distance");
  const bx = assertNumber(b.fields.get("x") || Num(0), "distance");
  const by = assertNumber(b.fields.get("y") || Num(0), "distance");
  const bz = assertNumber(b.fields.get("z") || Num(0), "distance");
  const dx = bx - ax;
  const dy = by - ay;
  const dz = bz - az;
  return Num(Math.sqrt(dx * dx + dy * dy + dz * dz));
});

export const length = native("length", 1, ([v]) => {
  if (isList(v)) {
    return Num(v.elements.length);
  }
  if (isString(v)) {
    return Num(v.value.length);
  }
  if (isRecord(v)) {
    // Vector length
    const x = assertNumber(v.fields.get("x") || Num(0), "length");
    const y = assertNumber(v.fields.get("y") || Num(0), "length");
    const z = assertNumber(v.fields.get("z") || Num(0), "length");
    return Num(Math.sqrt(x * x + y * y + z * z));
  }
  throw new RuntimeError("length expects a list, string, or vector");
});

// ============ List Functions ============

export const push = native("push", 2, ([list, item]) => {
  const elements = assertList(list, "push");
  return List([...elements, item]);
});

export const pop = native("pop", 1, ([list]) => {
  const elements = assertList(list, "pop");
  if (elements.length === 0) {
    throw new RuntimeError("Cannot pop from empty list");
  }
  return List(elements.slice(0, -1));
});

export const first = native("first", 1, ([list]) => {
  const elements = assertList(list, "first");
  if (elements.length === 0) {
    return Null();
  }
  return elements[0];
});

export const last = native("last", 1, ([list]) => {
  const elements = assertList(list, "last");
  if (elements.length === 0) {
    return Null();
  }
  return elements[elements.length - 1];
});

export const slice = native("slice", 3, ([list, start, end]) => {
  const elements = assertList(list, "slice");
  const startIdx = assertNumber(start, "slice");
  const endIdx = assertNumber(end, "slice");
  return List(elements.slice(startIdx, endIdx));
});

export const concat = native("concat", 2, ([a, b]) => {
  const aElements = assertList(a, "concat");
  const bElements = assertList(b, "concat");
  return List([...aElements, ...bElements]);
});

export const reverse = native("reverse", 1, ([list]) => {
  const elements = assertList(list, "reverse");
  return List([...elements].reverse());
});

export const contains = native("contains", 2, ([list, item]) => {
  const elements = assertList(list, "contains");
  for (const el of elements) {
    if (valuesEqual(el, item)) {
      return Bool(true);
    }
  }
  return Bool(false);
});

export const indexOf = native("indexOf", 2, ([list, item]) => {
  const elements = assertList(list, "indexOf");
  for (let i = 0; i < elements.length; i++) {
    if (valuesEqual(elements[i], item)) {
      return Num(i);
    }
  }
  return Num(-1);
});

export const range = native("range", 2, ([start, end]) => {
  const startN = assertNumber(start, "range");
  const endN = assertNumber(end, "range");
  const elements: SlateValue[] = [];
  for (let i = startN; i < endN; i++) {
    elements.push(Num(i));
  }
  return List(elements);
});

// ============ String Functions ============

export const split = native("split", 2, ([str, delimiter]) => {
  const s = assertString(str, "split");
  const d = assertString(delimiter, "split");
  return List(s.split(d).map((part) => Str(part)));
});

export const join = native("join", 2, ([list, delimiter]) => {
  const elements = assertList(list, "join");
  const d = assertString(delimiter, "join");
  return Str(elements.map((e) => stringify(e)).join(d));
});

export const trim = native("trim", 1, ([str]) => {
  return Str(assertString(str, "trim").trim());
});

export const upper = native("upper", 1, ([str]) => {
  return Str(assertString(str, "upper").toUpperCase());
});

export const lower = native("lower", 1, ([str]) => {
  return Str(assertString(str, "lower").toLowerCase());
});

export const startsWith = native("startsWith", 2, ([str, prefix]) => {
  return Bool(
    assertString(str, "startsWith").startsWith(
      assertString(prefix, "startsWith")
    )
  );
});

export const endsWith = native("endsWith", 2, ([str, suffix]) => {
  return Bool(
    assertString(str, "endsWith").endsWith(assertString(suffix, "endsWith"))
  );
});

// ============ Type Functions ============

export const typeOf = native("typeof", 1, ([v]) => {
  return Str(v.type);
});

export const toString = native("toString", 1, ([v]) => {
  return Str(stringify(v));
});

export const toNumber = native("toNumber", 1, ([v]) => {
  if (isNumber(v)) return v;
  if (isString(v)) {
    const n = parseFloat(v.value);
    if (isNaN(n)) {
      throw new RuntimeError(`Cannot convert "${v.value}" to number`);
    }
    return Num(n);
  }
  throw new RuntimeError(`Cannot convert ${v.type} to number`);
});

// ============ Record Functions ============

export const keys = native("keys", 1, ([record]) => {
  if (!isRecord(record)) {
    throw new RuntimeError("keys expects a record");
  }
  return List(Array.from(record.fields.keys()).map((k) => Str(k)));
});

export const values = native("values", 1, ([record]) => {
  if (!isRecord(record)) {
    throw new RuntimeError("values expects a record");
  }
  return List(Array.from(record.fields.values()));
});

export const has = native("has", 2, ([record, key]) => {
  if (!isRecord(record)) {
    throw new RuntimeError("has expects a record");
  }
  const k = assertString(key, "has");
  return Bool(record.fields.has(k));
});

// ============ I/O Functions (will be bound by runtime) ============

export const say = native("say", "variadic", (args) => {
  // This is a placeholder - actual output will be handled by the interpreter
  console.log(...args.map((a) => stringify(a)));
  return Null();
});

export const print = native("print", "variadic", (args) => {
  console.log(...args.map((a) => stringify(a)));
  return Null();
});

// ============ Helper for value equality ============

function valuesEqual(a: SlateValue, b: SlateValue): boolean {
  if (a.type !== b.type) return false;

  switch (a.type) {
    case "number":
      return a.value === (b as any).value;
    case "string":
      return a.value === (b as any).value;
    case "bool":
      return a.value === (b as any).value;
    case "null":
      return true;
    case "list": {
      const bList = b as any;
      if (a.elements.length !== bList.elements.length) return false;
      return a.elements.every((e, i) => valuesEqual(e, bList.elements[i]));
    }
    case "record": {
      const bRecord = b as any;
      if (a.fields.size !== bRecord.fields.size) return false;
      for (const [key, value] of a.fields) {
        const bValue = bRecord.fields.get(key);
        if (bValue === undefined || !valuesEqual(value, bValue)) {
          return false;
        }
      }
      return true;
    }
    default:
      return false;
  }
}

// ============ All stdlib functions ============

export const stdlib: Map<string, SlateNativeFunction> = new Map([
  // Math
  ["abs", abs],
  ["min", min],
  ["max", max],
  ["clamp", clamp],
  ["lerp", lerp],
  ["floor", floor],
  ["ceil", ceil],
  ["round", round],
  ["sqrt", sqrt],
  ["pow", pow],
  ["sin", sin],
  ["cos", cos],
  ["random", random],

  // Vector
  ["vec3", vec3],
  ["dot", dot],
  ["normalize", normalize],
  ["distance", distance],
  ["length", length],

  // List
  ["push", push],
  ["pop", pop],
  ["first", first],
  ["last", last],
  ["slice", slice],
  ["concat", concat],
  ["reverse", reverse],
  ["contains", contains],
  ["indexOf", indexOf],
  ["range", range],

  // String
  ["split", split],
  ["join", join],
  ["trim", trim],
  ["upper", upper],
  ["lower", lower],
  ["startsWith", startsWith],
  ["endsWith", endsWith],

  // Type
  ["typeof", typeOf],
  ["toString", toString],
  ["toNumber", toNumber],

  // Record
  ["keys", keys],
  ["values", values],
  ["has", has],

  // I/O
  ["say", say],
  ["print", print],
]);
