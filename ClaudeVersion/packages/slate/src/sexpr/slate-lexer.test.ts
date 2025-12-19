/**
 * Tests for the self-hosted Slate lexer
 */

import { describe, it, expect } from "vitest";
import { readFileSync } from "fs";
import { join } from "path";
import { read } from "./reader";
import { createSexprRuntime } from "./index";
import { Value, valueToString } from "./evaluator";

// Helper to load and run the lexer
function createLexer() {
  const path = join(__dirname, "slate", "lexer.sl");
  const source = readFileSync(path, "utf-8");
  const exprs = read(source);
  const { evaluator } = createSexprRuntime();

  // Evaluate the lexer module - last expression is the tokenize function
  const tokenizeFn = evaluator.evalProgram(exprs);

  if (tokenizeFn.type !== "closure") {
    throw new Error(`Expected closure, got ${tokenizeFn.type}`);
  }

  return (slateSource: string): Value => {
    const callEnv = tokenizeFn.env.extend();
    callEnv.define(tokenizeFn.params[0], { type: "string", value: slateSource });

    let result: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      result = evaluator.eval(expr, callEnv);
    }
    return result;
  };
}

// Helper to extract token types from result
function getTokenTypes(tokens: Value): string[] {
  if (tokens.type !== "list") return [];
  return tokens.elements.map(tok => {
    if (tok.type !== "list" || tok.elements.length < 1) return "?";
    const typeVal = tok.elements[0];
    if (typeVal.type === "list" && typeVal.elements.length === 2) {
      // Symbol is (symbol name)
      const nameVal = typeVal.elements[1];
      if (nameVal.type === "string") return nameVal.value;
    }
    return "?";
  });
}

describe("Self-hosted Slate Lexer", () => {
  it("loads the lexer module", () => {
    const tokenize = createLexer();
    expect(typeof tokenize).toBe("function");
  });

  it("tokenizes a simple number", () => {
    const tokenize = createLexer();
    const tokens = tokenize("42");
    const types = getTokenTypes(tokens);
    expect(types).toContain("NUMBER");
    expect(types).toContain("EOF");
  });

  it("tokenizes arithmetic", () => {
    const tokenize = createLexer();
    const tokens = tokenize("1 + 2");
    const types = getTokenTypes(tokens);
    expect(types).toContain("NUMBER");
    expect(types).toContain("PLUS");
  });

  it("tokenizes identifiers", () => {
    const tokenize = createLexer();
    const tokens = tokenize("foo bar");
    const types = getTokenTypes(tokens);
    expect(types).toContain("IDENTIFIER");
  });

  it("tokenizes keywords", () => {
    const tokenize = createLexer();
    const tokens = tokenize("let x = 5");
    const types = getTokenTypes(tokens);
    expect(types).toContain("LET");
    expect(types).toContain("IDENTIFIER");
    expect(types).toContain("EQUALS");
    expect(types).toContain("NUMBER");
  });

  it("tokenizes a function definition", () => {
    const tokenize = createLexer();
    const tokens = tokenize("fn add a b:\n    a + b");
    const types = getTokenTypes(tokens);
    expect(types).toContain("FN");
    expect(types).toContain("COLON");
    expect(types).toContain("INDENT");
    expect(types).toContain("PLUS");
  });

  it("tokenizes strings", () => {
    const tokenize = createLexer();
    const tokens = tokenize('"hello world"');
    const types = getTokenTypes(tokens);
    expect(types).toContain("STRING");
  });

  it("tokenizes comparison operators", () => {
    const tokenize = createLexer();
    const tokens = tokenize("a < b >= c == d != e");
    const types = getTokenTypes(tokens);
    expect(types).toContain("LESS");
    expect(types).toContain("GREATER_EQUALS");
    expect(types).toContain("DOUBLE_EQUALS");
    expect(types).toContain("NOT_EQUALS");
  });

  it("handles indentation correctly", () => {
    const tokenize = createLexer();
    const tokens = tokenize("if true:\n    x\ny");
    const types = getTokenTypes(tokens);

    // Should have: IF TRUE COLON NEWLINE INDENT IDENTIFIER NEWLINE DEDENT IDENTIFIER EOF
    expect(types).toContain("IF");
    expect(types).toContain("INDENT");
    expect(types).toContain("DEDENT");
  });
});
