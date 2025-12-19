/**
 * Loader for Slate files written in S-expressions
 *
 * Reads .sl files and evaluates them in the S-expr runtime,
 * returning any exported functions/values.
 */

import { readFileSync } from "fs";
import { join } from "path";
import { read } from "./reader";
import { createSexprRuntime } from "./index";
import { Value } from "./evaluator";

const SLATE_DIR = join(__dirname, "slate");

/**
 * Load and evaluate a .sl file
 */
export function loadSlateFile(filename: string): Value {
  const path = join(SLATE_DIR, filename);
  const source = readFileSync(path, "utf-8");
  const exprs = read(source);
  const { evaluator } = createSexprRuntime();
  return evaluator.evalProgram(exprs);
}

/**
 * Load the Slate lexer
 */
export function loadLexer(): (source: string) => Value {
  const tokenizeFn = loadSlateFile("lexer.sl");

  if (tokenizeFn.type !== "closure" && tokenizeFn.type !== "primitive") {
    throw new Error("lexer.sl should export a tokenize function");
  }

  const { evaluator } = createSexprRuntime();

  return (source: string) => {
    if (tokenizeFn.type === "closure") {
      const callEnv = tokenizeFn.env.extend();
      callEnv.define(tokenizeFn.params[0], { type: "string", value: source });

      let result: Value = { type: "nil" };
      for (const expr of tokenizeFn.body) {
        result = evaluator.eval(expr, callEnv);
      }
      return result;
    }
    throw new Error("Expected closure");
  };
}
