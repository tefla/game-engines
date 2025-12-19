/**
 * Tests for the self-hosted Slate reader (parser)
 */

import { describe, it, expect } from "vitest";
import { readFileSync } from "fs";
import { join } from "path";
import { read } from "./reader";
import { createSexprRuntime } from "./index";
import { Value, valueToString } from "./evaluator";

const SLATE_DIR = join(__dirname, "slate");

// Load and evaluate a .sl file
function loadSlateFile(filename: string): Value {
  const path = join(SLATE_DIR, filename);
  const source = readFileSync(path, "utf-8");
  const exprs = read(source);
  const { evaluator } = createSexprRuntime();
  return evaluator.evalProgram(exprs);
}

// Helper to create the full pipeline: lexer -> parser
function createPipeline() {
  const { evaluator } = createSexprRuntime();

  // Load lexer
  const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
  const tokenizeFn = evaluator.evalProgram(read(lexerSource));

  // Load parser
  const readerSource = readFileSync(join(SLATE_DIR, "reader.sl"), "utf-8");
  const parseFn = evaluator.evalProgram(read(readerSource));

  if (tokenizeFn.type !== "closure" || parseFn.type !== "closure") {
    throw new Error("Expected closures");
  }

  return (slateSource: string): Value => {
    // First tokenize
    const tokenEnv = tokenizeFn.env.extend();
    tokenEnv.define(tokenizeFn.params[0], { type: "string", value: slateSource });
    let tokens: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      tokens = evaluator.eval(expr, tokenEnv);
    }

    // Then parse
    const parseEnv = parseFn.env.extend();
    parseEnv.define(parseFn.params[0], tokens);
    let ast: Value = { type: "nil" };
    for (const expr of parseFn.body) {
      ast = evaluator.eval(expr, parseEnv);
    }

    return ast;
  };
}

// Helper to extract structure from parsed result
// The reader produces syntax objects like:
//   (("symbol" "syntax") datum line col)  - syntax wrapper
//   (("symbol" "symbol") "name")  - symbol with name
//   (("symbol" "number") 42)  - number
function simplifyAst(value: Value): any {
  if (value.type === "number") return value.value;
  if (value.type === "string") return value.value;
  if (value.type === "boolean") return value.value;
  if (value.type === "nil") return null;
  if (value.type === "list") {
    // Check if it's a tagged value: (("symbol" TYPE) value ...)
    if (value.elements.length >= 2) {
      const first = value.elements[0];
      if (first.type === "list" && first.elements.length === 2) {
        const tag = first.elements[0];
        const tagType = first.elements[1];
        if (tag.type === "string" && tag.value === "symbol" &&
            tagType.type === "string") {
          const typeName = tagType.value;
          // ("symbol" "syntax") - syntax wrapper, extract the datum
          if (typeName === "syntax") {
            return simplifyAst(value.elements[1]);
          }
          // ("symbol" "symbol") - symbol, extract the name from elements[1]
          if (typeName === "symbol") {
            const nameVal = value.elements[1];
            if (nameVal.type === "string") {
              return nameVal.value;
            }
          }
          // ("symbol" "number") or other - just extract elements[1]
          if (typeName === "number" || typeName === "string" || typeName === "boolean") {
            return simplifyAst(value.elements[1]);
          }
        }
      }
      // Old-style string "syntax" tag
      if (first.type === "string" && first.value === "syntax") {
        return simplifyAst(value.elements[1]);
      }
    }
    return value.elements.map(simplifyAst);
  }
  return value;
}

describe("Self-hosted Slate Reader", () => {
  it("loads the reader module", () => {
    const parse = loadSlateFile("reader.sl");
    expect(parse.type).toBe("closure");
  });

  it("parses a number", () => {
    const parse = createPipeline();
    const ast = parse("42");
    const simple = simplifyAst(ast);
    expect(simple).toContainEqual(42);
  });

  it("parses arithmetic", () => {
    const parse = createPipeline();
    const ast = parse("1 + 2");
    const simple = simplifyAst(ast);
    // Should produce something like [["syntax", ["+", 1, 2], ...]]
    expect(JSON.stringify(simple)).toContain("+");
  });

  it("parses let binding", () => {
    const parse = createPipeline();
    const ast = parse("let x = 5");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("let");
    expect(JSON.stringify(simple)).toContain("x");
  });

  it("parses function definition", () => {
    const parse = createPipeline();
    const ast = parse("fn add a b:\n    a + b");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("fn");
    expect(JSON.stringify(simple)).toContain("add");
  });

  it("parses if expression", () => {
    const parse = createPipeline();
    const ast = parse("if true:\n    1\nelse:\n    2");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("if");
  });

  it("parses match expression", () => {
    const parse = createPipeline();
    const ast = parse("match x:\n    1 => \"one\"\n    _ => \"other\"");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("match");
  });

  it("parses list literal", () => {
    const parse = createPipeline();
    const ast = parse("[1, 2, 3]");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("list");
  });

  it("parses record literal", () => {
    const parse = createPipeline();
    const ast = parse("{x: 1, y: 2}");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("record");
  });

  it("parses lambda expression", () => {
    const parse = createPipeline();
    const ast = parse("|x| x + 1");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("lambda");
  });

  it("parses member access", () => {
    const parse = createPipeline();
    const ast = parse("obj.field");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain(".");
    expect(JSON.stringify(simple)).toContain("field");
  });

  it("parses function call", () => {
    const parse = createPipeline();
    const ast = parse("foo(1, 2)");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("call");
    expect(JSON.stringify(simple)).toContain("foo");
  });

  it("parses for loop", () => {
    const parse = createPipeline();
    const ast = parse("for x in items:\n    x");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("for");
  });

  it("parses while loop", () => {
    const parse = createPipeline();
    const ast = parse("while true:\n    1");
    const simple = simplifyAst(ast);
    expect(JSON.stringify(simple)).toContain("while");
  });
});
