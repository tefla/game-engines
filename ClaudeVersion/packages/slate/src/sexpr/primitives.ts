/**
 * Primitive Functions for S-Expression Evaluator
 *
 * Categories:
 * - Arithmetic: +, -, *, /, %, <, >, <=, >=, =
 * - Strings: string-append, substring, string-length, string-ref, string->list
 * - Lists: cons, car, cdr, list, length, nth, append, reverse, map, filter
 * - Predicates: null?, pair?, symbol?, string?, number?, boolean?
 * - Comparison: eq?, equal?
 * - I/O: display, newline
 * - Regex: regex-match, regex-split
 */

import { Evaluator, Value, vNum, vStr, vBool, vNil, vList, valueToString } from "./evaluator";

export function registerPrimitives(evaluator: Evaluator): void {
  // ============ Arithmetic ============

  evaluator.addPrimitive("+", (...args) => {
    let sum = 0;
    for (const arg of args) {
      if (arg.type !== "number") throw new Error("+ requires numbers");
      sum += arg.value;
    }
    return vNum(sum);
  });

  evaluator.addPrimitive("-", (...args) => {
    if (args.length === 0) throw new Error("- requires at least one argument");
    if (args[0].type !== "number") throw new Error("- requires numbers");

    if (args.length === 1) return vNum(-args[0].value);

    let result = args[0].value;
    for (let i = 1; i < args.length; i++) {
      if (args[i].type !== "number") throw new Error("- requires numbers");
      result -= args[i].value;
    }
    return vNum(result);
  });

  evaluator.addPrimitive("*", (...args) => {
    let product = 1;
    for (const arg of args) {
      if (arg.type !== "number") throw new Error("* requires numbers");
      product *= arg.value;
    }
    return vNum(product);
  });

  evaluator.addPrimitive("/", (...args) => {
    if (args.length < 2) throw new Error("/ requires at least two arguments");
    if (args[0].type !== "number") throw new Error("/ requires numbers");

    let result = args[0].value;
    for (let i = 1; i < args.length; i++) {
      if (args[i].type !== "number") throw new Error("/ requires numbers");
      result /= args[i].value;
    }
    return vNum(result);
  });

  evaluator.addPrimitive("%", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error("% requires numbers");
    return vNum(a.value % b.value);
  });

  evaluator.addPrimitive("<", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error("< requires numbers");
    return vBool(a.value < b.value);
  });

  evaluator.addPrimitive(">", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error("> requires numbers");
    return vBool(a.value > b.value);
  });

  evaluator.addPrimitive("<=", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error("<= requires numbers");
    return vBool(a.value <= b.value);
  });

  evaluator.addPrimitive(">=", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error(">= requires numbers");
    return vBool(a.value >= b.value);
  });

  evaluator.addPrimitive("=", (a, b) => {
    if (a.type !== "number" || b.type !== "number") throw new Error("= requires numbers");
    return vBool(a.value === b.value);
  });

  evaluator.addPrimitive("max", (...args) => {
    if (args.length === 0) throw new Error("max requires at least one argument");
    let result = -Infinity;
    for (const arg of args) {
      if (arg.type !== "number") throw new Error("max requires numbers");
      if (arg.value > result) result = arg.value;
    }
    return vNum(result);
  });

  evaluator.addPrimitive("min", (...args) => {
    if (args.length === 0) throw new Error("min requires at least one argument");
    let result = Infinity;
    for (const arg of args) {
      if (arg.type !== "number") throw new Error("min requires numbers");
      if (arg.value < result) result = arg.value;
    }
    return vNum(result);
  });

  // ============ Strings ============

  evaluator.addPrimitive("string-append", (...args) => {
    let result = "";
    for (const arg of args) {
      if (arg.type !== "string") throw new Error("string-append requires strings");
      result += arg.value;
    }
    return vStr(result);
  });

  evaluator.addPrimitive("substring", (s, start, end) => {
    if (s.type !== "string") throw new Error("substring requires a string");
    if (start.type !== "number") throw new Error("substring requires number start");
    const endVal = end ? (end.type === "number" ? end.value : s.value.length) : s.value.length;
    return vStr(s.value.substring(start.value, endVal));
  });

  evaluator.addPrimitive("string-length", s => {
    if (s.type !== "string") throw new Error("string-length requires a string");
    return vNum(s.value.length);
  });

  evaluator.addPrimitive("string-ref", (s, index) => {
    if (s.type !== "string") throw new Error("string-ref requires a string");
    if (index.type !== "number") throw new Error("string-ref requires number index");
    const ch = s.value[index.value];
    if (ch === undefined) throw new Error("string-ref index out of bounds");
    return vStr(ch);
  });

  evaluator.addPrimitive("string->list", s => {
    if (s.type !== "string") throw new Error("string->list requires a string");
    return vList(...s.value.split("").map(ch => vStr(ch)));
  });

  evaluator.addPrimitive("list->string", lst => {
    if (lst.type !== "list") throw new Error("list->string requires a list");
    let result = "";
    for (const elem of lst.elements) {
      if (elem.type !== "string") throw new Error("list->string requires list of strings");
      result += elem.value;
    }
    return vStr(result);
  });

  evaluator.addPrimitive("string=?", (a, b) => {
    if (a.type !== "string" || b.type !== "string") throw new Error("string=? requires strings");
    return vBool(a.value === b.value);
  });

  evaluator.addPrimitive("string-contains?", (s, sub) => {
    if (s.type !== "string" || sub.type !== "string") throw new Error("string-contains? requires strings");
    return vBool(s.value.includes(sub.value));
  });

  evaluator.addPrimitive("string-starts-with?", (s, prefix) => {
    if (s.type !== "string" || prefix.type !== "string") throw new Error("string-starts-with? requires strings");
    return vBool(s.value.startsWith(prefix.value));
  });

  evaluator.addPrimitive("string-ends-with?", (s, suffix) => {
    if (s.type !== "string" || suffix.type !== "string") throw new Error("string-ends-with? requires strings");
    return vBool(s.value.endsWith(suffix.value));
  });

  evaluator.addPrimitive("string-trim", s => {
    if (s.type !== "string") throw new Error("string-trim requires a string");
    return vStr(s.value.trim());
  });

  evaluator.addPrimitive("string-split", (s, delim) => {
    if (s.type !== "string") throw new Error("string-split requires a string");
    if (delim.type !== "string") throw new Error("string-split requires string delimiter");
    return vList(...s.value.split(delim.value).map(part => vStr(part)));
  });

  evaluator.addPrimitive("string-join", (lst, delim) => {
    if (lst.type !== "list") throw new Error("string-join requires a list");
    if (delim.type !== "string") throw new Error("string-join requires string delimiter");
    const parts = lst.elements.map(e => {
      if (e.type !== "string") throw new Error("string-join requires list of strings");
      return e.value;
    });
    return vStr(parts.join(delim.value));
  });

  evaluator.addPrimitive("number->string", n => {
    if (n.type !== "number") throw new Error("number->string requires a number");
    return vStr(String(n.value));
  });

  evaluator.addPrimitive("string->number", s => {
    if (s.type !== "string") throw new Error("string->number requires a string");
    const n = parseFloat(s.value);
    if (isNaN(n)) return vNil;
    return vNum(n);
  });

  // ============ Lists ============

  evaluator.addPrimitive("cons", (head, tail) => {
    // If tail is a list, prepend head to it
    if (tail.type === "list") {
      return vList(head, ...tail.elements);
    }
    // If tail is not a list, create a pair (dotted pair representation)
    // Represented as a two-element list for simplicity
    return vList(head, tail);
  });

  evaluator.addPrimitive("car", lst => {
    if (lst.type !== "list" || lst.elements.length === 0) {
      throw new Error("car requires non-empty list");
    }
    return lst.elements[0];
  });

  evaluator.addPrimitive("cdr", lst => {
    if (lst.type !== "list" || lst.elements.length === 0) {
      throw new Error("cdr requires non-empty list");
    }
    return vList(...lst.elements.slice(1));
  });

  evaluator.addPrimitive("list", (...args) => vList(...args));

  evaluator.addPrimitive("length", lst => {
    if (lst.type !== "list") throw new Error("length requires a list");
    return vNum(lst.elements.length);
  });

  evaluator.addPrimitive("nth", (lst, index) => {
    if (lst.type !== "list") throw new Error(`nth requires a list, got ${lst.type}: ${valueToString(lst)}`);
    if (index.type !== "number") throw new Error("nth requires number index");
    const elem = lst.elements[index.value];
    if (elem === undefined) throw new Error(`nth index ${index.value} out of bounds for list of length ${lst.elements.length}`);
    return elem;
  });

  evaluator.addPrimitive("append", (...lists) => {
    const result: Value[] = [];
    for (const lst of lists) {
      if (lst.type !== "list") throw new Error("append requires lists");
      result.push(...lst.elements);
    }
    return vList(...result);
  });

  evaluator.addPrimitive("reverse", lst => {
    if (lst.type !== "list") throw new Error("reverse requires a list");
    return vList(...[...lst.elements].reverse());
  });

  evaluator.addPrimitive("list-ref", (lst, index) => {
    if (lst.type !== "list") throw new Error("list-ref requires a list");
    if (index.type !== "number") throw new Error("list-ref requires number index");
    const elem = lst.elements[index.value];
    if (elem === undefined) throw new Error("list-ref index out of bounds");
    return elem;
  });

  evaluator.addPrimitive("list-set", (lst, index, value) => {
    if (lst.type !== "list") throw new Error("list-set requires a list");
    if (index.type !== "number") throw new Error("list-set requires number index");
    const newElements = [...lst.elements];
    newElements[index.value] = value;
    return vList(...newElements);
  });

  evaluator.addPrimitive("list-tail", (lst, k) => {
    if (lst.type !== "list") throw new Error("list-tail requires a list");
    if (k.type !== "number") throw new Error("list-tail requires number");
    return vList(...lst.elements.slice(k.value));
  });

  evaluator.addPrimitive("take", (lst, n) => {
    if (lst.type !== "list") throw new Error("take requires a list");
    if (n.type !== "number") throw new Error("take requires number");
    return vList(...lst.elements.slice(0, n.value));
  });

  evaluator.addPrimitive("drop", (lst, n) => {
    if (lst.type !== "list") throw new Error("drop requires a list");
    if (n.type !== "number") throw new Error("drop requires number");
    return vList(...lst.elements.slice(n.value));
  });

  evaluator.addPrimitive("member", (elem, lst) => {
    if (lst.type !== "list") throw new Error("member requires a list");
    for (let i = 0; i < lst.elements.length; i++) {
      if (valuesEqual(elem, lst.elements[i])) {
        return vList(...lst.elements.slice(i));
      }
    }
    return vBool(false);
  });

  evaluator.addPrimitive("assoc", (key, alist) => {
    if (alist.type !== "list") throw new Error("assoc requires a list");
    for (const pair of alist.elements) {
      if (pair.type === "list" && pair.elements.length >= 2) {
        if (valuesEqual(key, pair.elements[0])) {
          return pair;
        }
      }
    }
    return vBool(false);
  });

  // ============ Predicates ============

  evaluator.addPrimitive("null?", v => vBool(v.type === "nil" || (v.type === "list" && v.elements.length === 0)));
  evaluator.addPrimitive("pair?", v => vBool(v.type === "list" && v.elements.length > 0));
  evaluator.addPrimitive("list?", v => vBool(v.type === "list"));
  evaluator.addPrimitive("symbol?", v => {
    // Symbols are stored as (symbol name)
    if (v.type === "list" && v.elements.length === 2) {
      const tag = v.elements[0];
      return vBool(tag.type === "string" && tag.value === "symbol");
    }
    return vBool(false);
  });
  evaluator.addPrimitive("string?", v => vBool(v.type === "string"));
  evaluator.addPrimitive("number?", v => vBool(v.type === "number"));
  evaluator.addPrimitive("boolean?", v => vBool(v.type === "boolean"));
  evaluator.addPrimitive("procedure?", v => vBool(v.type === "closure" || v.type === "primitive"));

  evaluator.addPrimitive("not", v => {
    if (v.type === "boolean") return vBool(!v.value);
    if (v.type === "nil") return vBool(true);
    return vBool(false);
  });

  // ============ Comparison ============

  evaluator.addPrimitive("eq?", (a, b) => vBool(a === b));

  evaluator.addPrimitive("equal?", (a, b) => vBool(valuesEqual(a, b)));

  // ============ I/O ============

  evaluator.addPrimitive("display", v => {
    console.log(valueToString(v));
    return vNil;
  });

  evaluator.addPrimitive("newline", () => {
    console.log();
    return vNil;
  });

  evaluator.addPrimitive("error", (msg) => {
    if (msg.type !== "string") throw new Error("error requires a string message");
    throw new Error(msg.value);
  });

  // ============ Regex ============

  evaluator.addPrimitive("regex-match", (pattern, str) => {
    if (pattern.type !== "string") throw new Error("regex-match requires string pattern");
    if (str.type !== "string") throw new Error("regex-match requires string input");

    const regex = new RegExp(pattern.value);
    const match = str.value.match(regex);
    if (!match) return vBool(false);
    return vList(...match.map(m => vStr(m ?? "")));
  });

  evaluator.addPrimitive("regex-match-all", (pattern, str) => {
    if (pattern.type !== "string") throw new Error("regex-match-all requires string pattern");
    if (str.type !== "string") throw new Error("regex-match-all requires string input");

    const regex = new RegExp(pattern.value, "g");
    const matches = [...str.value.matchAll(regex)];
    return vList(...matches.map(m => vList(...m.map(s => vStr(s ?? "")))));
  });

  evaluator.addPrimitive("regex-replace", (pattern, replacement, str) => {
    if (pattern.type !== "string") throw new Error("regex-replace requires string pattern");
    if (replacement.type !== "string") throw new Error("regex-replace requires string replacement");
    if (str.type !== "string") throw new Error("regex-replace requires string input");

    const regex = new RegExp(pattern.value, "g");
    return vStr(str.value.replace(regex, replacement.value));
  });

  evaluator.addPrimitive("regex-test", (pattern, str) => {
    if (pattern.type !== "string") throw new Error("regex-test requires string pattern");
    if (str.type !== "string") throw new Error("regex-test requires string input");

    const regex = new RegExp(pattern.value);
    return vBool(regex.test(str.value));
  });

  // ============ Character utilities ============

  evaluator.addPrimitive("char-code", s => {
    if (s.type !== "string" || s.value.length !== 1) {
      throw new Error("char-code requires a single character");
    }
    return vNum(s.value.charCodeAt(0));
  });

  evaluator.addPrimitive("code-char", n => {
    if (n.type !== "number") throw new Error("code-char requires a number");
    return vStr(String.fromCharCode(n.value));
  });

  evaluator.addPrimitive("char-whitespace?", s => {
    if (s.type !== "string") throw new Error("char-whitespace? requires a string");
    return vBool(/^\s$/.test(s.value));
  });

  evaluator.addPrimitive("char-alphabetic?", s => {
    if (s.type !== "string") throw new Error("char-alphabetic? requires a string");
    return vBool(/^[a-zA-Z]$/.test(s.value));
  });

  evaluator.addPrimitive("char-numeric?", s => {
    if (s.type !== "string") throw new Error("char-numeric? requires a string");
    return vBool(/^[0-9]$/.test(s.value));
  });

  // ============ Hash/Map utilities ============

  evaluator.addPrimitive("make-hash", () => {
    // Return empty assoc list for now
    return vList();
  });

  evaluator.addPrimitive("hash-set", (hash, key, value) => {
    if (hash.type !== "list") throw new Error("hash-set requires a hash (list)");
    // Filter out existing key and add new
    const filtered = hash.elements.filter(pair => {
      if (pair.type === "list" && pair.elements.length >= 1) {
        return !valuesEqual(pair.elements[0], key);
      }
      return true;
    });
    return vList(...filtered, vList(key, value));
  });

  evaluator.addPrimitive("hash-get", (hash, key, defaultVal) => {
    if (hash.type !== "list") throw new Error("hash-get requires a hash (list)");
    for (const pair of hash.elements) {
      if (pair.type === "list" && pair.elements.length >= 2) {
        if (valuesEqual(pair.elements[0], key)) {
          return pair.elements[1];
        }
      }
    }
    return defaultVal ?? vNil;
  });

  evaluator.addPrimitive("hash-has?", (hash, key) => {
    if (hash.type !== "list") throw new Error("hash-has? requires a hash (list)");
    for (const pair of hash.elements) {
      if (pair.type === "list" && pair.elements.length >= 1) {
        if (valuesEqual(pair.elements[0], key)) {
          return vBool(true);
        }
      }
    }
    return vBool(false);
  });

  // ============ Utility ============

  evaluator.addPrimitive("gensym", (() => {
    let counter = 0;
    return (prefix) => {
      const p = prefix?.type === "string" ? prefix.value : "g";
      return vList(vStr("symbol"), vStr(`${p}${counter++}`));
    };
  })());

  evaluator.addPrimitive("apply", (fn, args) => {
    if (args.type !== "list") throw new Error("apply requires a list of arguments");
    if (fn.type === "primitive") {
      return fn.fn(...args.elements);
    }
    if (fn.type === "closure") {
      if (args.elements.length !== fn.params.length) {
        throw new Error(`Expected ${fn.params.length} arguments, got ${args.elements.length}`);
      }
      // We need to use the evaluator here, but we don't have access to it
      // This is a limitation of the current design
      throw new Error("apply with closures not yet supported in primitives");
    }
    throw new Error("apply requires a function");
  });

  // ============ Debugging ============

  // trace: print a value with optional label, return the value unchanged
  // Usage: (trace value) or (trace "label" value)
  evaluator.addPrimitive("trace", (...args) => {
    if (args.length === 1) {
      console.log("[trace]", valueToString(args[0]));
      return args[0];
    } else if (args.length === 2) {
      const label = args[0].type === "string" ? args[0].value : valueToString(args[0]);
      console.log(`[trace:${label}]`, valueToString(args[1]));
      return args[1];
    }
    throw new Error("trace requires 1 or 2 arguments");
  });

  // trace-if: conditionally trace based on a predicate
  // Usage: (trace-if condition "label" value)
  evaluator.addPrimitive("trace-if", (condition, label, value) => {
    if (condition.type === "boolean" && condition.value) {
      const labelStr = label.type === "string" ? label.value : valueToString(label);
      console.log(`[trace:${labelStr}]`, valueToString(value));
    }
    return value;
  });

  // trace-stack: print the current indent stack (for lexer debugging)
  evaluator.addPrimitive("trace-stack", (label, stack) => {
    const labelStr = label.type === "string" ? label.value : valueToString(label);
    console.log(`[stack:${labelStr}]`, valueToString(stack));
    return stack;
  });

  // assert: throw error if condition is false
  evaluator.addPrimitive("assert", (condition, msg) => {
    const isTruthy = condition.type === "boolean" ? condition.value :
                     condition.type !== "nil";
    if (!isTruthy) {
      const message = msg?.type === "string" ? msg.value : "Assertion failed";
      throw new Error(message);
    }
    return vNil;
  });

  // type-of: return the type of a value as a string
  evaluator.addPrimitive("type-of", v => vStr(v.type));

  // pp (pretty-print): formatted output with indentation
  evaluator.addPrimitive("pp", (v, indentLevel) => {
    const indent = indentLevel?.type === "number" ? indentLevel.value : 0;
    console.log(prettyPrint(v, indent));
    return vNil;
  });

  // dump: detailed output showing type and structure
  evaluator.addPrimitive("dump", v => {
    console.log("=== DUMP ===");
    console.log("Type:", v.type);
    console.log("Value:", JSON.stringify(v, null, 2));
    console.log("String:", valueToString(v));
    console.log("=============");
    return v;
  });

  // inspect: show internal structure, return as string
  evaluator.addPrimitive("inspect", v => {
    return vStr(`<${v.type}> ${valueToString(v)}`);
  });

  // dbg: like Rust's dbg! macro - print and return value
  evaluator.addPrimitive("dbg", (...args) => {
    const parts = args.map(v => `${v.type}:${valueToString(v)}`);
    console.log("[dbg]", parts.join(" | "));
    return args.length === 1 ? args[0] : vList(...args);
  });

  // time: measure execution time of a thunk
  evaluator.addPrimitive("time-start", () => {
    return vNum(Date.now());
  });

  evaluator.addPrimitive("time-elapsed", (start) => {
    if (start.type !== "number") throw new Error("time-elapsed requires start time");
    return vNum(Date.now() - start.value);
  });

  // log-level: set/get logging verbosity
  let logLevel = 1; // 0=silent, 1=normal, 2=verbose, 3=debug
  evaluator.addPrimitive("log-level", (level) => {
    if (level) {
      if (level.type !== "number") throw new Error("log-level requires number");
      logLevel = level.value;
    }
    return vNum(logLevel);
  });

  evaluator.addPrimitive("log", (level, ...msgs) => {
    if (level.type !== "number") throw new Error("log requires level number");
    if (level.value <= logLevel) {
      console.log(...msgs.map(m => valueToString(m)));
    }
    return vNil;
  });

  // count: count occurrences (for profiling loops)
  const counters: Record<string, number> = {};
  evaluator.addPrimitive("count", (name) => {
    if (name.type !== "string") throw new Error("count requires string name");
    counters[name.value] = (counters[name.value] || 0) + 1;
    return vNum(counters[name.value]);
  });

  evaluator.addPrimitive("count-reset", (name) => {
    if (name) {
      if (name.type !== "string") throw new Error("count-reset requires string name");
      delete counters[name.value];
    } else {
      for (const key in counters) delete counters[key];
    }
    return vNil;
  });

  evaluator.addPrimitive("count-report", () => {
    console.log("=== COUNTERS ===");
    for (const [name, count] of Object.entries(counters).sort((a, b) => b[1] - a[1])) {
      console.log(`  ${name}: ${count}`);
    }
    console.log("================");
    return vNil;
  });

  // format: sprintf-like string formatting
  evaluator.addPrimitive("format", (template, ...args) => {
    if (template.type !== "string") throw new Error("format requires string template");
    let result = template.value;
    let argIndex = 0;
    result = result.replace(/~[aAsS%]/g, (match) => {
      if (match === "~%") return "\n";
      if (argIndex >= args.length) return match;
      const arg = args[argIndex++];
      if (match === "~a" || match === "~A") {
        return arg.type === "string" ? arg.value : valueToString(arg);
      }
      if (match === "~s" || match === "~S") {
        return valueToString(arg);
      }
      return match;
    });
    return vStr(result);
  });
}

/**
 * Pretty print a value with indentation
 */
function prettyPrint(v: Value, indent: number = 0): string {
  const spaces = "  ".repeat(indent);

  if (v.type === "list" && v.elements.length > 0) {
    // Check if it's a simple list that fits on one line
    const simple = v.elements.every(e =>
      e.type === "number" || e.type === "string" || e.type === "boolean" || e.type === "nil"
    );
    if (simple && v.elements.length <= 5) {
      return spaces + "(" + v.elements.map(e => valueToString(e)).join(" ") + ")";
    }

    // Multi-line format for complex lists
    const lines = [spaces + "("];
    for (const elem of v.elements) {
      lines.push(prettyPrint(elem, indent + 1));
    }
    lines.push(spaces + ")");
    return lines.join("\n");
  }

  return spaces + valueToString(v);
}

/**
 * Deep equality check for values
 */
function valuesEqual(a: Value, b: Value): boolean {
  if (a.type !== b.type) return false;

  switch (a.type) {
    case "number":
    case "string":
    case "boolean":
      return a.value === (b as typeof a).value;
    case "nil":
      return true;
    case "list": {
      const bList = b as { type: "list"; elements: Value[] };
      if (a.elements.length !== bList.elements.length) return false;
      return a.elements.every((elem, i) => valuesEqual(elem, bList.elements[i]));
    }
    default:
      return false;
  }
}
