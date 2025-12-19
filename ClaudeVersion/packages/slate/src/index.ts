// Slate Language Package

// Legacy exports for backward compatibility (used by editor, vcs, and engine)
export { Lexer } from "./lexer";
export { Parser } from "./parser";
export { Interpreter } from "./interpreter";
export type { SignalHandler, InterpreterOptions } from "./interpreter";
export {
  ModuleLoader,
  InMemoryModuleResolver,
  type ModuleResolver,
  type ModuleInterpreter,
} from "./interpreter/module-loader";

// Standard library
export { stdlib } from "./stdlib";

// New S-expression based implementation
export { createSlateRuntime, createSexprRuntime } from "./sexpr";

// Re-export core types
export {
  // Tokens
  TokenType,
  KEYWORDS,
  type Token,

  // AST
  type Program,
  type Stmt,
  type Expr,
  type Block,
  type Pattern,

  // Values
  type SlateValue,
  type SlateNativeFunction,
  type SlateFunction,
  Num,
  Str,
  Bool,
  Null,
  List,
  Record,
  Color,
  Signal,
  stringify,
  isTruthy,
  isNumber,
  isString,
  isBool,
  isNull,
  isList,
  isRecord,
  isFunction,
  isNativeFunction,
  isCallable,
  isColor,
  isSignal,

  // Environment
  Environment,
  RuntimeError,
  ParseError,
} from "@oort/core";

// Imports for convenience functions
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { Interpreter } from "./interpreter";
import { stdlib } from "./stdlib";
import type { SlateValue } from "@oort/core";

export interface SlateOptions {
  globals?: Map<string, SlateValue>;
  includeStdlib?: boolean;
}

// Convenience function to run Slate code (uses old interpreter for now)
export function runSlate(
  source: string,
  options: SlateOptions = {}
): SlateValue {
  const { globals = new Map(), includeStdlib = true } = options;

  // Merge stdlib with provided globals
  const allGlobals = new Map(globals);
  if (includeStdlib) {
    for (const [name, fn] of stdlib) {
      if (!allGlobals.has(name)) {
        allGlobals.set(name, fn);
      }
    }
  }

  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();

  const parser = new Parser(tokens);
  const ast = parser.parse();

  const interpreter = new Interpreter(allGlobals);
  return interpreter.run(ast);
}

// Parse Slate source to AST
export function parseSlate(source: string) {
  const lexer = new Lexer(source);
  const tokens = lexer.tokenize();
  const parser = new Parser(tokens);
  return parser.parse();
}

// Tokenize Slate source
export function tokenizeSlate(source: string) {
  const lexer = new Lexer(source);
  return lexer.tokenize();
}
