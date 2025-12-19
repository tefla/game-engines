/**
 * Tests for the self-hosted Slate expander
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

// Create the full pipeline: lexer -> parser -> expander
function createPipeline() {
  const { evaluator } = createSexprRuntime();

  // Load lexer
  const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
  const tokenizeFn = evaluator.evalProgram(read(lexerSource));

  // Load parser
  const readerSource = readFileSync(join(SLATE_DIR, "reader.sl"), "utf-8");
  const parseFn = evaluator.evalProgram(read(readerSource));

  // Load expander
  const expanderSource = readFileSync(join(SLATE_DIR, "expander.sl"), "utf-8");
  const expandFn = evaluator.evalProgram(read(expanderSource));

  if (
    tokenizeFn.type !== "closure" ||
    parseFn.type !== "closure" ||
    expandFn.type !== "closure"
  ) {
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

    // Then expand
    const expandEnv = expandFn.env.extend();
    expandEnv.define(expandFn.params[0], ast);
    let expanded: Value = { type: "nil" };
    for (const expr of expandFn.body) {
      expanded = evaluator.eval(expr, expandEnv);
    }

    return expanded;
  };
}

// Helper to extract structure from expanded result
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
        if (
          tag.type === "string" &&
          tag.value === "symbol" &&
          tagType.type === "string"
        ) {
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
          if (
            typeName === "number" ||
            typeName === "string" ||
            typeName === "boolean"
          ) {
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

describe("Self-hosted Slate Expander", () => {
  it("loads the expander module", () => {
    const expand = loadSlateFile("expander.sl");
    expect(expand.type).toBe("closure");
  });

  it("expands a simple number", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("42");
    const simple = simplifyAst(expanded);
    expect(simple).toContainEqual(42);
  });

  it("expands arithmetic expression", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("1 + 2");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("+");
    expect(JSON.stringify(simple)).toContain("1");
    expect(JSON.stringify(simple)).toContain("2");
  });

  it("expands let binding", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let x = 5");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("let");
    expect(JSON.stringify(simple)).toContain("x");
  });

  it("expands function definition", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("fn add a b:\n    a + b");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("fn");
    expect(JSON.stringify(simple)).toContain("add");
  });

  it("expands if expression", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("if true:\n    1\nelse:\n    2");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("if");
  });

  it("expands nested expressions", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let x = 1 + 2 * 3");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("let");
    expect(JSON.stringify(simple)).toContain("+");
    expect(JSON.stringify(simple)).toContain("*");
  });

  it("expands while loop", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("while true:\n    1");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("while");
  });

  it("expands for loop", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("for x in items:\n    x");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("for");
  });

  it("expands match expression", () => {
    const pipeline = createPipeline();
    const expanded = pipeline('match x:\n    1 => "one"\n    _ => "other"');
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("match");
  });

  it("expands function call", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("foo(1, 2)");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("call");
    expect(JSON.stringify(simple)).toContain("foo");
  });

  it("expands member access", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("obj.field");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain(".");
    expect(JSON.stringify(simple)).toContain("field");
  });

  it("expands list literal", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("[1, 2, 3]");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("list");
  });

  it("expands record literal", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("{x: 1, y: 2}");
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("record");
  });

  it("expands if-else with proper dedent handling", () => {
    // Regression test for dedent bug: at-line-start must be #f after dedents
    const pipeline = createPipeline();
    const source = `fn test:
    if true:
        say "a"
    else:
        say "b"`;
    const expanded = pipeline(source);
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    // Should have one fn with if-else, not crash or produce wrong structure
    expect(json).toContain("fn");
    expect(json).toContain("if");
    // else is represented as the 4th element of (if cond then else)
    expect(json).toContain('["begin","say","b"]');
  });

  it("expands emit and on statements", () => {
    const pipeline = createPipeline();
    const expanded = pipeline(`emit @game.start "data"
on @player.interact "door":
    say "opened"`);
    const simple = simplifyAst(expanded);
    expect(JSON.stringify(simple)).toContain("emit");
    expect(JSON.stringify(simple)).toContain("on");
    expect(JSON.stringify(simple)).toContain("signal");
  });

  it("tokenizes color literals", () => {
    const { evaluator } = createSexprRuntime();
    const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
    const tokenizeFn = evaluator.evalProgram(read(lexerSource));
    if (tokenizeFn.type !== "closure") throw new Error("Expected closure");

    const tokenEnv = tokenizeFn.env.extend();
    tokenEnv.define(tokenizeFn.params[0], { type: "string", value: "let color = #FF0000" });
    let tokens: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      tokens = evaluator.eval(expr, tokenEnv);
    }

    expect(tokens.type).toBe("list");
    const str = valueToString(tokens);
    expect(str).toContain("COLOR");
    expect(str).toContain("#FF0000");
  });

  it("tokenizes optional chaining operators", () => {
    const { evaluator } = createSexprRuntime();
    const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
    const tokenizeFn = evaluator.evalProgram(read(lexerSource));
    if (tokenizeFn.type !== "closure") throw new Error("Expected closure");

    const tokenEnv = tokenizeFn.env.extend();
    tokenEnv.define(tokenizeFn.params[0], { type: "string", value: "a?.b?[0]" });
    let tokens: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      tokens = evaluator.eval(expr, tokenEnv);
    }

    expect(tokens.type).toBe("list");
    const str = valueToString(tokens);
    expect(str).toContain("QUESTION_DOT");
    expect(str).toContain("QUESTION_BRACKET");
  });

  it("tokenizes #lang directive", () => {
    const { evaluator } = createSexprRuntime();
    const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
    const tokenizeFn = evaluator.evalProgram(read(lexerSource));
    if (tokenizeFn.type !== "closure") throw new Error("Expected closure");

    const tokenEnv = tokenizeFn.env.extend();
    tokenEnv.define(tokenizeFn.params[0], { type: "string", value: "#lang slate/strict\nlet x = 1" });
    let tokens: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      tokens = evaluator.eval(expr, tokenEnv);
    }

    expect(tokens.type).toBe("list");
    const str = valueToString(tokens);
    expect(str).toContain("LANG_DIRECTIVE");
    expect(str).toContain("slate/strict");
    expect(str).toContain("LET");
  });

  it("tokenizes spread operator", () => {
    const { evaluator } = createSexprRuntime();
    const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
    const tokenizeFn = evaluator.evalProgram(read(lexerSource));
    if (tokenizeFn.type !== "closure") throw new Error("Expected closure");

    const tokenEnv = tokenizeFn.env.extend();
    tokenEnv.define(tokenizeFn.params[0], { type: "string", value: "fn foo ...args" });
    let tokens: Value = { type: "nil" };
    for (const expr of tokenizeFn.body) {
      tokens = evaluator.eval(expr, tokenEnv);
    }

    expect(tokens.type).toBe("list");
    const str = valueToString(tokens);
    expect(str).toContain("SPREAD");
    expect(str).toContain("...");
  });

  it("parses optional chaining expressions", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let x = obj?.field?[0]");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("?.");
    expect(json).toContain("?index");
  });

  it("parses spread in function parameters", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("fn foo a b ...rest:\n    a");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("rest");
    expect(json).toContain("fn");
  });

  it("parses spread in list literals", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let x = [1, 2, ...items]");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("spread");
    expect(json).toContain("items");
  });

  it("parses spread in function calls", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("foo(1, ...args)");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("spread");
    expect(json).toContain("args");
  });

  it("parses type annotations on let bindings", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let x: Int = 5");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("let");
    expect(json).toContain("Int");
  });

  it("parses generic type annotations", () => {
    const pipeline = createPipeline();
    const expanded = pipeline("let items: List[Int] = []");
    const simple = simplifyAst(expanded);
    const json = JSON.stringify(simple);
    expect(json).toContain("List");
    expect(json).toContain("Int");
  });

  it("loads the evaluator module", () => {
    const { evaluator } = createSexprRuntime();
    const evaluatorSource = readFileSync(join(SLATE_DIR, "evaluator.sl"), "utf-8");
    const makeEvaluator = evaluator.evalProgram(read(evaluatorSource));
    expect(makeEvaluator.type).toBe("closure");
  });

  it("parses real game file: math_lock.sl structure", () => {
    // Test that the pipeline can handle the structure of real game files
    const pipeline = createPipeline();
    const source = `let lock = create_entity("math_lock", {
    position: vec3(10, 1, 0),
    locked: true,
    combination: 7
})

fn check_answer answer:
    if answer = 7:
        say "Correct!"
        lock.props.locked = false
        emit @puzzle.solved "math_lock"
    else:
        say "Wrong!"

on @player.interact "math_lock":
    say "A riddle"

fn solve_math answer:
    check_answer(answer)`;

    const expanded = pipeline(source);
    expect(expanded.type).toBe("list");

    if (expanded.type === "list") {
      // Should have 4 top-level statements
      expect(expanded.elements.length).toBe(4);
    }
  });
});

// Import createSlateRuntime for integration tests
import { createSlateRuntime } from "./index";

describe("Self-hosted Slate Runtime", () => {
  it("creates a runtime successfully", () => {
    const runtime = createSlateRuntime();
    expect(runtime.run).toBeDefined();
    expect(runtime.tokenize).toBeDefined();
    expect(runtime.parse).toBeDefined();
    expect(runtime.expand).toBeDefined();
    expect(runtime.evaluate).toBeDefined();
  });

  it("tokenizes source code", () => {
    const runtime = createSlateRuntime();
    const tokens = runtime.tokenize("let x = 5");
    expect(tokens.type).toBe("list");
  });

  it("parses tokens to AST", () => {
    const runtime = createSlateRuntime();
    const tokens = runtime.tokenize("let x = 5");
    const ast = runtime.parse(tokens);
    expect(ast.type).toBe("list");
  });

  it("expands AST", () => {
    const runtime = createSlateRuntime();
    const tokens = runtime.tokenize("let x = 5");
    const ast = runtime.parse(tokens);
    const expanded = runtime.expand(ast);
    expect(expanded.type).toBe("list");
  });
});
