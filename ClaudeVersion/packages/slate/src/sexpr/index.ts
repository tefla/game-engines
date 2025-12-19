/**
 * Self-Hosted Slate Runtime
 *
 * This module provides both the bootstrap S-expression runtime
 * and the full self-hosted Slate pipeline:
 *
 * S-expression Runtime:
 * - S-expression reader (TypeScript)
 * - Core evaluator (TypeScript)
 * - Primitives for strings, lists, regex, etc.
 *
 * Self-Hosted Slate Pipeline:
 * - lexer.sl   - tokenizes Slate source
 * - reader.sl  - parses tokens to syntax objects
 * - expander.sl - expands macros/syntax
 * - evaluator.sl - evaluates the AST
 */

export * from "./reader";
export * from "./evaluator";
export { registerPrimitives } from "./primitives";

import { readFileSync } from "fs";
import { join } from "path";
import { read, SExpr } from "./reader";
import { Evaluator, Value, valueToString } from "./evaluator";
import { registerPrimitives } from "./primitives";

const SLATE_DIR = join(__dirname, "slate");

/**
 * Create a new S-expression runtime with all primitives
 */
export function createSexprRuntime(): {
  eval: (source: string) => Value;
  evalExprs: (exprs: SExpr[]) => Value;
  evaluator: Evaluator;
} {
  const evaluator = new Evaluator();
  registerPrimitives(evaluator);

  return {
    eval: (source: string) => {
      const exprs = read(source);
      return evaluator.evalProgram(exprs);
    },
    evalExprs: (exprs: SExpr[]) => {
      return evaluator.evalProgram(exprs);
    },
    evaluator,
  };
}

/**
 * Quick eval for testing
 */
export function sexprEval(source: string): Value {
  const { eval: evaluate } = createSexprRuntime();
  return evaluate(source);
}

/**
 * SlateRuntime - Self-hosted Slate interpreter
 * Uses lexer.sl, reader.sl, expander.sl, and evaluator.sl
 */
export interface SlateRuntime {
  /** Run Slate source code and return the result */
  run: (source: string) => unknown;

  /** Tokenize Slate source into tokens */
  tokenize: (source: string) => Value;

  /** Parse tokens into AST */
  parse: (tokens: Value) => Value;

  /** Expand AST (macro expansion) */
  expand: (ast: Value) => Value;

  /** Evaluate expanded AST */
  evaluate: (ast: Value) => unknown;

  /** The underlying S-expr evaluator */
  sexprEvaluator: Evaluator;
}

/**
 * Create a new self-hosted Slate runtime
 *
 * This loads and compiles the .sl files (lexer, reader, expander, evaluator)
 * using the S-expression runtime, then provides a unified API for running
 * Slate code through the full pipeline.
 */
export function createSlateRuntime(): SlateRuntime {
  const { evaluator } = createSexprRuntime();

  // Load and compile the .sl modules
  const lexerSource = readFileSync(join(SLATE_DIR, "lexer.sl"), "utf-8");
  const tokenizeFn = evaluator.evalProgram(read(lexerSource));

  const readerSource = readFileSync(join(SLATE_DIR, "reader.sl"), "utf-8");
  const parseFn = evaluator.evalProgram(read(readerSource));

  const expanderSource = readFileSync(join(SLATE_DIR, "expander.sl"), "utf-8");
  const expandFn = evaluator.evalProgram(read(expanderSource));

  const evaluatorSource = readFileSync(join(SLATE_DIR, "evaluator.sl"), "utf-8");
  const makeEvaluatorFn = evaluator.evalProgram(read(evaluatorSource));

  if (
    tokenizeFn.type !== "closure" ||
    parseFn.type !== "closure" ||
    expandFn.type !== "closure" ||
    makeEvaluatorFn.type !== "closure"
  ) {
    throw new Error("Failed to load Slate modules - expected closures");
  }

  // Helper to call a closure with an argument
  const callClosure = (fn: Value, arg: Value): Value => {
    if (fn.type !== "closure") throw new Error("Expected closure");
    const env = fn.env.extend();
    env.define(fn.params[0], arg);
    let result: Value = { type: "nil" };
    for (const expr of fn.body) {
      result = evaluator.eval(expr, env);
    }
    return result;
  };

  // Create the Slate evaluator by calling make-evaluator
  const slateEvaluatorFn = callClosure(makeEvaluatorFn, { type: "nil" });

  const tokenize = (source: string): Value => {
    return callClosure(tokenizeFn, { type: "string", value: source });
  };

  const parse = (tokens: Value): Value => {
    return callClosure(parseFn, tokens);
  };

  const expand = (ast: Value): Value => {
    return callClosure(expandFn, ast);
  };

  const evaluate = (expandedAst: Value): unknown => {
    const result = callClosure(slateEvaluatorFn, expandedAst);
    return valueToNative(result);
  };

  const run = (source: string): unknown => {
    const tokens = tokenize(source);
    const ast = parse(tokens);
    const expanded = expand(ast);
    return evaluate(expanded);
  };

  return {
    run,
    tokenize,
    parse,
    expand,
    evaluate,
    sexprEvaluator: evaluator,
  };
}

/**
 * Get the tag name from a Value if it's a string or symbol
 * Handles both native symbols and the S-expr representation ("symbol" "name")
 */
function getTagName(value: Value): string | null {
  if (value.type === "string") return value.value;
  if (value.type === "symbol") return value.value;
  // Handle S-expr symbol representation: ("symbol" "name")
  if (
    value.type === "list" &&
    value.elements.length === 2 &&
    value.elements[0].type === "string" &&
    value.elements[0].value === "symbol" &&
    value.elements[1].type === "string"
  ) {
    return value.elements[1].value;
  }
  return null;
}

/**
 * Convert a Slate Value to native JavaScript value
 */
function valueToNative(value: Value): unknown {
  switch (value.type) {
    case "number":
      return value.value;
    case "string":
      return value.value;
    case "boolean":
      return value.value;
    case "nil":
      return null;
    case "symbol":
      // Symbols should generally not appear in final output
      return value.value;
    case "list": {
      // Empty list is null
      if (value.elements.length === 0) {
        return null;
      }
      // Check for special tagged values (tag can be string or symbol)
      const first = value.elements[0];
      const tag = getTagName(first);
      if (tag) {
        // list-val -> array
        if (tag === "list-val") {
          return value.elements.slice(1).map(valueToNative);
        }
        // record-val -> object
        if (tag === "record-val") {
          const obj: Record<string, unknown> = {};
          for (let i = 1; i < value.elements.length; i++) {
            const field = value.elements[i];
            if (field.type === "list" && field.elements.length === 2) {
              const key = field.elements[0];
              const val = field.elements[1];
              const keyName = getTagName(key);
              if (keyName) {
                obj[keyName] = valueToNative(val);
              }
            }
          }
          return obj;
        }
        // closure -> function marker
        if (tag === "closure") {
          return { type: "function", params: value.elements[1] };
        }
      }
      // Regular list
      return value.elements.map(valueToNative);
    }
    case "closure":
      return { type: "function", params: value.params };
    default:
      return value;
  }
}
