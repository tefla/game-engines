/**
 * Tests for the Racket-style Macro System
 */

import { describe, it, expect, beforeEach } from "vitest";
import {
  // Syntax
  stx,
  stxSym,
  stxList,
  stxRecord,
  loc,
  isSymbol,
  isList,
  symbolName,
  listElements,
  syntaxToString,

  // Reader
  Reader,
  read,

  // Pattern
  parsePattern,
  matchPattern,
  applyTemplate,
  SyntaxRules,

  // Expander
  Expander,
  createExpander,
  expandWithTracking,

  // Interpreter
  CoreInterpreter,
  Environment,

  // Base macros
  installBaseMacros,
  installRuntimeFunctions,

  // Full runtime
  createSlateRuntime,
} from "./index";

import { Lexer } from "../lexer";

// Helper to lex and read
function readSource(source: string) {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  return read(tokens);
}

describe("Syntax Objects", () => {
  it("creates primitive syntax", () => {
    const num = stx(42, loc(1, 1));
    expect(num.datum).toBe(42);
    expect(num.loc.line).toBe(1);
  });

  it("creates symbol syntax", () => {
    const sym = stxSym("foo", loc(1, 1));
    expect(isSymbol(sym)).toBe(true);
    expect(symbolName(sym)).toBe("foo");
  });

  it("creates list syntax", () => {
    const list = stxList([
      stxSym("add", loc(1, 1)),
      stx(1, loc(1, 5)),
      stx(2, loc(1, 7)),
    ], loc(1, 1));

    expect(isList(list)).toBe(true);
    expect(listElements(list).length).toBe(3);
  });

  it("converts to string", () => {
    const list = stxList([
      stxSym("fn", loc(1, 1)),
      stxSym("foo", loc(1, 4)),
      stxList([stxSym("x", loc(1, 8))], loc(1, 7)),
      stxList([
        stxSym("+", loc(2, 5)),
        stxSym("x", loc(2, 7)),
        stx(1, loc(2, 9)),
      ], loc(2, 4)),
    ], loc(1, 1));

    const str = syntaxToString(list);
    expect(str).toContain("fn");
    expect(str).toContain("foo");
  });
});

describe("Reader", () => {
  it("reads simple let statement", () => {
    const syntax = readSource("let x = 42");
    const forms = listElements(syntax);

    expect(forms.length).toBe(1);
    const letForm = forms[0];

    expect(isList(letForm)).toBe(true);
    const elements = listElements(letForm as any);
    expect(symbolName(elements[0] as any)).toBe("let");
    expect(symbolName(elements[1] as any)).toBe("x");
    expect((elements[2] as any).datum).toBe(42);
  });

  it("reads function definition", () => {
    const syntax = readSource(`
fn greet name:
    say "Hello"
`);
    const forms = listElements(syntax);
    expect(forms.length).toBe(1);

    const fnForm = forms[0];
    expect(isList(fnForm)).toBe(true);

    const elements = listElements(fnForm as any);
    expect(symbolName(elements[0] as any)).toBe("fn");
    expect(symbolName(elements[1] as any)).toBe("greet");
  });

  it("reads if expression", () => {
    const syntax = readSource(`
if x > 0:
    "positive"
else:
    "non-positive"
`);
    const forms = listElements(syntax);
    expect(forms.length).toBe(1);

    const ifForm = forms[0];
    expect(isList(ifForm)).toBe(true);

    const elements = listElements(ifForm as any);
    expect(symbolName(elements[0] as any)).toBe("if");
  });

  it("reads record literal", () => {
    const syntax = readSource(`let player = {name: "Hero", health: 100}`);
    const forms = listElements(syntax);
    const letForm = forms[0];
    const elements = listElements(letForm as any);

    // The value should be a record
    const record = elements[2];
    expect(record.datum instanceof Map).toBe(true);
  });

  it("reads list literal", () => {
    const syntax = readSource(`let items = [1, 2, 3]`);
    const forms = listElements(syntax);
    const letForm = forms[0];
    const elements = listElements(letForm as any);

    // The value should be a (list ...) form
    const listForm = elements[2];
    expect(isList(listForm)).toBe(true);
    expect(symbolName(listElements(listForm as any)[0] as any)).toBe("list");
  });
});

describe("Pattern Matching", () => {
  it("matches literal pattern", () => {
    const pattern = parsePattern(stx(42, loc(1, 1)));
    const result = matchPattern(pattern, stx(42, loc(1, 1)));
    expect(result).not.toBeNull();
  });

  it("rejects non-matching literal", () => {
    const pattern = parsePattern(stx(42, loc(1, 1)));
    const result = matchPattern(pattern, stx(99, loc(1, 1)));
    expect(result).toBeNull();
  });

  it("matches variable pattern and binds", () => {
    const pattern = parsePattern(stxSym("$x", loc(1, 1)));
    const input = stx(42, loc(1, 1));
    const result = matchPattern(pattern, input);

    expect(result).not.toBeNull();
    expect(result!.get("x")).toBe(input);
  });

  it("matches list pattern", () => {
    const pattern = parsePattern(stxList([
      stxSym("add", loc(1, 1)),
      stxSym("$a", loc(1, 5)),
      stxSym("$b", loc(1, 8)),
    ], loc(1, 1)));

    const input = stxList([
      stxSym("add", loc(1, 1)),
      stx(1, loc(1, 5)),
      stx(2, loc(1, 8)),
    ], loc(1, 1));

    const result = matchPattern(pattern, input);
    expect(result).not.toBeNull();
    expect((result!.get("a") as any).datum).toBe(1);
    expect((result!.get("b") as any).datum).toBe(2);
  });

  it("matches ellipsis pattern", () => {
    const pattern = parsePattern(stxList([
      stxSym("begin", loc(1, 1)),
      stxSym("$body...", loc(1, 7)),
    ], loc(1, 1)));

    const input = stxList([
      stxSym("begin", loc(1, 1)),
      stx(1, loc(1, 7)),
      stx(2, loc(1, 9)),
      stx(3, loc(1, 11)),
    ], loc(1, 1));

    const result = matchPattern(pattern, input);
    expect(result).not.toBeNull();

    const body = result!.get("body") as any[];
    expect(body.length).toBe(3);
  });

  it("applies template with bindings", () => {
    const template = stxList([
      stxSym("+", loc(1, 1)),
      stxSym("$x", loc(1, 3)),
      stxSym("$y", loc(1, 6)),
    ], loc(1, 1));

    const bindings = new Map<string, any>([
      ["x", stx(10, loc(1, 1))],
      ["y", stx(20, loc(1, 1))],
    ]);

    const result = applyTemplate(template, bindings);
    expect(isList(result)).toBe(true);

    const elements = listElements(result as any);
    expect((elements[1] as any).datum).toBe(10);
    expect((elements[2] as any).datum).toBe(20);
  });
});

describe("Expander", () => {
  let expander: Expander;

  beforeEach(() => {
    expander = createExpander();
  });

  it("expands simple let through unchanged", () => {
    const syntax = stxList([
      stxSym("let", loc(1, 1)),
      stxSym("x", loc(1, 5)),
      stx(42, loc(1, 9)),
    ], loc(1, 1));

    const expanded = expander.expand(syntax);
    expect(syntaxToString(expanded)).toContain("let");
  });

  it("expands custom macro", () => {
    // Define a simple macro: (double $x) => (+ $x $x)
    const rules = new SyntaxRules("double", [
      {
        pattern: parsePattern(stxList([
          stxSym("double", loc(1, 1)),
          stxSym("$x", loc(1, 8)),
        ], loc(1, 1))),
        template: stxList([
          stxSym("+", loc(1, 1)),
          stxSym("$x", loc(1, 3)),
          stxSym("$x", loc(1, 6)),
        ], loc(1, 1)),
      },
    ]);

    expander.define("double", rules);

    const syntax = stxList([
      stxSym("double", loc(1, 1)),
      stx(5, loc(1, 8)),
    ], loc(1, 1));

    const expanded = expander.expand(syntax);
    expect(syntaxToString(expanded)).toContain("+");
  });

  it("tracks macro usage", () => {
    // Define a macro
    expander.defineProc("my-macro", (stx) => {
      return stxList([stxSym("begin", stx.loc)], stx.loc);
    });

    const syntax = stxList([
      stxList([stxSym("my-macro", loc(1, 1))], loc(1, 1)),
    ], loc(1, 1));

    const result = expandWithTracking(expander, syntax);
    expect(result.macrosUsed.has("my-macro")).toBe(true);
  });
});

describe("Core Interpreter", () => {
  let interp: CoreInterpreter;

  beforeEach(() => {
    interp = new CoreInterpreter();
  });

  it("evaluates literals", async () => {
    const program = stxList([stx(42, loc(1, 1))], loc(1, 1));
    const result = await interp.run(program);
    expect(result).toBe(42);
  });

  it("evaluates let binding", async () => {
    const program = stxList([
      stxList([
        stxSym("let", loc(1, 1)),
        stxSym("x", loc(1, 5)),
        stx(10, loc(1, 9)),
      ], loc(1, 1)),
      stxSym("x", loc(2, 1)),
    ], loc(1, 1));

    const result = await interp.run(program);
    expect(result).toBe(10);
  });

  it("evaluates arithmetic", async () => {
    const program = stxList([
      stxList([
        stxSym("+", loc(1, 1)),
        stx(2, loc(1, 3)),
        stx(3, loc(1, 5)),
      ], loc(1, 1)),
    ], loc(1, 1));

    const result = await interp.run(program);
    expect(result).toBe(5);
  });

  it("evaluates if expression", async () => {
    const program = stxList([
      stxList([
        stxSym("if", loc(1, 1)),
        stx(true, loc(1, 4)),
        stx(1, loc(1, 9)),
        stx(2, loc(1, 11)),
      ], loc(1, 1)),
    ], loc(1, 1));

    const result = await interp.run(program);
    expect(result).toBe(1);
  });

  it("evaluates function definition and call", async () => {
    const program = stxList([
      // (fn add (a b) (+ a b))
      stxList([
        stxSym("fn", loc(1, 1)),
        stxSym("add", loc(1, 4)),
        stxList([stxSym("a", loc(1, 8)), stxSym("b", loc(1, 10))], loc(1, 7)),
        stxList([
          stxSym("+", loc(1, 13)),
          stxSym("a", loc(1, 15)),
          stxSym("b", loc(1, 17)),
        ], loc(1, 13)),
      ], loc(1, 1)),
      // (add 3 4)
      stxList([
        stxSym("add", loc(2, 1)),
        stx(3, loc(2, 5)),
        stx(4, loc(2, 7)),
      ], loc(2, 1)),
    ], loc(1, 1));

    const result = await interp.run(program);
    expect(result).toBe(7);
  });

  it("evaluates list construction", async () => {
    const program = stxList([
      stxList([
        stxSym("list", loc(1, 1)),
        stx(1, loc(1, 6)),
        stx(2, loc(1, 8)),
        stx(3, loc(1, 10)),
      ], loc(1, 1)),
    ], loc(1, 1));

    const result = await interp.run(program);
    expect(Array.isArray(result)).toBe(true);
    expect(result).toEqual([1, 2, 3]);
  });
});

describe("Base Macros", () => {
  let expander: Expander;
  let interp: CoreInterpreter;

  beforeEach(() => {
    expander = createExpander();
    installBaseMacros(expander);
    interp = new CoreInterpreter();
    installRuntimeFunctions(interp);
  });

  it("expands 'when' macro", () => {
    const syntax = stxList([
      stxSym("when", loc(1, 1)),
      stx(true, loc(1, 6)),
      stxList([
        stxSym("begin", loc(2, 5)),
        stx(42, loc(2, 11)),
      ], loc(2, 5)),
    ], loc(1, 1));

    const expanded = expander.expand(syntax);
    expect(syntaxToString(expanded)).toContain("if");
  });

  it("expands 'unless' macro", () => {
    const syntax = stxList([
      stxSym("unless", loc(1, 1)),
      stx(false, loc(1, 8)),
      stxList([
        stxSym("begin", loc(2, 5)),
        stx(42, loc(2, 11)),
      ], loc(2, 5)),
    ], loc(1, 1));

    const expanded = expander.expand(syntax);
    expect(syntaxToString(expanded)).toContain("if");
    expect(syntaxToString(expanded)).toContain("not");
  });

  it("expands 'entity' macro", () => {
    const syntax = stxList([
      stxSym("entity", loc(1, 1)),
      stxSym("Door", loc(1, 8)),
      stxRecord(new Map([
        ["locked", stx(true, loc(2, 13))],
      ]), loc(2, 5)),
    ], loc(1, 1));

    const expanded = expander.expand(syntax);
    expect(syntaxToString(expanded)).toContain("begin");
    expect(syntaxToString(expanded)).toContain("create-template");
  });
});

describe("Full Pipeline", () => {
  it("runs simple program", async () => {
    const { run } = createSlateRuntime();

    const result = await run(`
let x = 10
let y = 20
x + y
`);

    expect(result).toBe(30);
  });

  it("runs function definition and call", async () => {
    const { run } = createSlateRuntime();

    const result = await run(`
fn double n:
    n * 2

double 21
`);

    expect(result).toBe(42);
  });

  it("runs conditional", async () => {
    const { run } = createSlateRuntime();

    const result = await run(`
let x = 5
if x > 0:
    "positive"
else:
    "non-positive"
`);

    expect(result).toBe("positive");
  });
});
