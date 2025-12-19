/**
 * Racket-Style Macro System for Slate
 *
 * This module provides a powerful macro system inspired by Racket:
 * - Syntax objects with source locations and hygiene scopes
 * - Pattern-based macro definitions (syntax-rules)
 * - Procedural macros for full control
 * - Hot reload with macro dependency tracking
 *
 * Architecture:
 *   Source → Lexer → Reader → Expander → Core Interpreter
 *                      ↓
 *                 Syntax Objects
 *                      ↓
 *                 Macro Expansion
 *                      ↓
 *                 Core Forms Only
 */

// Syntax Objects
export type {
  Syntax,
  SyntaxList,
  SyntaxSymbol,
  SyntaxRecord,
  SyntaxPrimitive,
  Datum,
  Scope,
  SourceLoc,
} from "./syntax";

export {
  stx,
  stxSym,
  stxList,
  stxRecord,
  datum,
  loc,
  makeScope,
  addScope,
  removeScope,
  isSymbol,
  isList,
  isRecord,
  isPrimitive,
  symbolName,
  listElements,
  listHead,
  listTail,
  recordGet,
  recordKeys,
  syntaxToString,
  syntaxDebug,
  CORE_FORMS,
} from "./syntax";

// Reader
export { Reader, ReaderError, read } from "./reader";

// Pattern Matching
export type {
  Pattern,
  PatternVar,
  PatternLiteral,
  PatternSymbol,
  PatternList,
  PatternRecord,
  PatternWildcard,
  Bindings,
} from "./pattern";

export {
  SyntaxRules,
  parsePattern,
  matchPattern,
  applyTemplate,
  syntaxRule,
} from "./pattern";

// Expander
export type {
  Transformer,
  ProceduralTransformer,
  ExpansionContext,
  ExpansionResult,
} from "./expander";

export {
  Expander,
  expandWithTracking,
  createExpander,
} from "./expander";

// Core Interpreter
export type {
  Value,
  RecordValue,
  FnValue,
  NativeFnValue,
} from "./core-interp";

export {
  CoreInterpreter,
  Environment,
} from "./core-interp";

// Base Macros
export { installBaseMacros, installRuntimeFunctions } from "./base-macros";

// Hot Reload
export type {
  CachedExpansion,
  ASTDiff,
} from "./hot-reload";

export {
  HotReloader,
  DependencyGraph,
  ExpansionCache,
  diffExpandedAST,
  createHotReloader,
} from "./hot-reload";

// ============ Convenience: Full Pipeline ============

import { Lexer } from "../lexer";
import { Reader } from "./reader";
import { Expander, createExpander } from "./expander";
import { CoreInterpreter, type Value } from "./core-interp";
import { installBaseMacros, installRuntimeFunctions } from "./base-macros";

/**
 * Create a fully configured Slate runtime with macro support
 */
export function createSlateRuntime(): {
  expander: Expander;
  interpreter: CoreInterpreter;
  run: (source: string) => Promise<Value>;
} {
  // Create expander with base macros
  const expander = createExpander();
  installBaseMacros(expander);

  // Create interpreter with runtime functions
  const interpreter = new CoreInterpreter();
  installRuntimeFunctions(interpreter);

  // Full pipeline
  async function run(source: string): Promise<Value> {
    // Lex
    const lexer = new Lexer(source);
    const tokens = lexer.tokenize();

    // Read to syntax objects
    const reader = new Reader(tokens);
    const surfaceSyntax = reader.read();

    // Expand macros
    const expanded = expander.expandProgram(surfaceSyntax);

    // Interpret
    return interpreter.run(expanded);
  }

  return { expander, interpreter, run };
}

/**
 * Convenience function to run Slate code
 */
export async function runSlate(source: string): Promise<Value> {
  const { run } = createSlateRuntime();
  return run(source);
}
