/**
 * Hot Reload with Macro Dependency Tracking
 *
 * This module handles hot reloading in the presence of macros:
 * 1. Track which macros each file uses
 * 2. When a macro changes, re-expand all dependent files
 * 3. Diff the expanded code and patch the runtime
 *
 * Key insight: We diff AFTER macro expansion, not before.
 */

import {
  Syntax,
  SyntaxList,
  isSymbol,
  isList,
  symbolName,
  listElements,
  syntaxToString,
} from "./syntax";
import { Expander, ExpansionResult, expandWithTracking } from "./expander";
import { CoreInterpreter, Value, FnValue } from "./core-interp";

// ============ Dependency Graph ============

export class DependencyGraph {
  // file -> macros used in that file
  private fileMacros = new Map<string, Set<string>>();

  // macro -> files that use that macro
  private macroUsers = new Map<string, Set<string>>();

  /**
   * Record that a file uses certain macros
   */
  recordDependencies(file: string, macros: Set<string>): void {
    // Remove old dependencies
    const oldMacros = this.fileMacros.get(file) ?? new Set();
    for (const macro of oldMacros) {
      this.macroUsers.get(macro)?.delete(file);
    }

    // Add new dependencies
    this.fileMacros.set(file, macros);
    for (const macro of macros) {
      if (!this.macroUsers.has(macro)) {
        this.macroUsers.set(macro, new Set());
      }
      this.macroUsers.get(macro)!.add(file);
    }
  }

  /**
   * Get all files that depend on a macro
   */
  getDependents(macro: string): Set<string> {
    return this.macroUsers.get(macro) ?? new Set();
  }

  /**
   * Get all files that depend on any of the given macros
   */
  getAllDependents(macros: Iterable<string>): Set<string> {
    const result = new Set<string>();
    for (const macro of macros) {
      for (const file of this.getDependents(macro)) {
        result.add(file);
      }
    }
    return result;
  }

  /**
   * Remove a file from the graph
   */
  removeFile(file: string): void {
    const macros = this.fileMacros.get(file) ?? new Set();
    for (const macro of macros) {
      this.macroUsers.get(macro)?.delete(file);
    }
    this.fileMacros.delete(file);
  }
}

// ============ Expansion Cache ============

export interface CachedExpansion {
  source: string;
  surfaceSyntax: SyntaxList;
  expandedSyntax: SyntaxList;
  macrosUsed: Set<string>;
  macroVersions: Map<string, number>;
  timestamp: number;
}

export class ExpansionCache {
  private cache = new Map<string, CachedExpansion>();

  get(file: string): CachedExpansion | undefined {
    return this.cache.get(file);
  }

  set(file: string, expansion: CachedExpansion): void {
    this.cache.set(file, expansion);
  }

  delete(file: string): void {
    this.cache.delete(file);
  }

  clear(): void {
    this.cache.clear();
  }
}

// ============ AST Diffing ============

export type ASTDiff =
  | { type: "function-added"; name: string; fn: Syntax }
  | { type: "function-removed"; name: string }
  | { type: "function-changed"; name: string; oldFn: Syntax; newFn: Syntax }
  | { type: "binding-added"; name: string; value: Syntax }
  | { type: "binding-removed"; name: string }
  | { type: "binding-changed"; name: string; oldValue: Syntax; newValue: Syntax }
  | { type: "handler-added"; signal: string; handler: Syntax }
  | { type: "handler-removed"; signal: string }
  | { type: "handler-changed"; signal: string; oldHandler: Syntax; newHandler: Syntax };

/**
 * Compute differences between two expanded programs
 */
export function diffExpandedAST(oldAST: SyntaxList, newAST: SyntaxList): ASTDiff[] {
  const diffs: ASTDiff[] = [];

  const oldDefs = extractDefinitions(oldAST);
  const newDefs = extractDefinitions(newAST);

  // Check for added/changed/removed functions
  for (const [name, def] of newDefs.functions) {
    const oldDef = oldDefs.functions.get(name);
    if (!oldDef) {
      diffs.push({ type: "function-added", name, fn: def });
    } else if (!syntaxEqual(oldDef, def)) {
      diffs.push({ type: "function-changed", name, oldFn: oldDef, newFn: def });
    }
  }

  for (const name of oldDefs.functions.keys()) {
    if (!newDefs.functions.has(name)) {
      diffs.push({ type: "function-removed", name });
    }
  }

  // Check for added/changed/removed bindings
  for (const [name, value] of newDefs.bindings) {
    const oldValue = oldDefs.bindings.get(name);
    if (!oldValue) {
      diffs.push({ type: "binding-added", name, value });
    } else if (!syntaxEqual(oldValue, value)) {
      diffs.push({ type: "binding-changed", name, oldValue, newValue: value });
    }
  }

  for (const name of oldDefs.bindings.keys()) {
    if (!newDefs.bindings.has(name)) {
      diffs.push({ type: "binding-removed", name });
    }
  }

  // Check for added/changed/removed handlers
  for (const [signal, handler] of newDefs.handlers) {
    const oldHandler = oldDefs.handlers.get(signal);
    if (!oldHandler) {
      diffs.push({ type: "handler-added", signal, handler });
    } else if (!syntaxEqual(oldHandler, handler)) {
      diffs.push({ type: "handler-changed", signal, oldHandler, newHandler: handler });
    }
  }

  for (const signal of oldDefs.handlers.keys()) {
    if (!newDefs.handlers.has(signal)) {
      diffs.push({ type: "handler-removed", signal });
    }
  }

  return diffs;
}

interface Definitions {
  functions: Map<string, Syntax>;
  bindings: Map<string, Syntax>;
  handlers: Map<string, Syntax>;
}

function extractDefinitions(ast: SyntaxList): Definitions {
  const functions = new Map<string, Syntax>();
  const bindings = new Map<string, Syntax>();
  const handlers = new Map<string, Syntax>();

  for (const form of listElements(ast)) {
    if (!isList(form)) continue;

    const elements = listElements(form);
    if (elements.length < 2) continue;

    const head = elements[0];
    if (!isSymbol(head)) continue;

    const formType = symbolName(head);

    switch (formType) {
      case "fn": {
        const name = elements[1];
        if (isSymbol(name)) {
          functions.set(symbolName(name), form);
        }
        break;
      }

      case "let":
      case "var": {
        const name = elements[1];
        if (isSymbol(name)) {
          bindings.set(symbolName(name), elements[2]);
        }
        break;
      }

      case "register-handler": {
        const signal = elements[1];
        if (signal) {
          const signalKey = syntaxToString(signal);
          handlers.set(signalKey, form);
        }
        break;
      }
    }
  }

  return { functions, bindings, handlers };
}

function syntaxEqual(a: Syntax, b: Syntax): boolean {
  // Simple structural equality (ignoring source locations)
  return syntaxToString(a) === syntaxToString(b);
}

// ============ Hot Reloader ============

export class HotReloader {
  private expander: Expander;
  private interpreter: CoreInterpreter;
  private depGraph = new DependencyGraph();
  private cache = new ExpansionCache();
  private macroVersions = new Map<string, number>();
  private sourceLoader: (file: string) => Promise<string>;
  private syntaxReader: (source: string, file: string) => SyntaxList;

  constructor(
    expander: Expander,
    interpreter: CoreInterpreter,
    sourceLoader: (file: string) => Promise<string>,
    syntaxReader: (source: string, file: string) => SyntaxList
  ) {
    this.expander = expander;
    this.interpreter = interpreter;
    this.sourceLoader = sourceLoader;
    this.syntaxReader = syntaxReader;
  }

  /**
   * Load and run a file for the first time
   */
  async loadFile(file: string): Promise<Value> {
    const source = await this.sourceLoader(file);
    const surfaceSyntax = this.syntaxReader(source, file);

    // Expand with tracking
    const { expanded, macrosUsed } = expandWithTracking(
      this.expander,
      surfaceSyntax,
      file
    );

    // Record dependencies
    this.depGraph.recordDependencies(file, macrosUsed);

    // Cache the expansion
    this.cache.set(file, {
      source,
      surfaceSyntax,
      expandedSyntax: expanded,
      macrosUsed,
      macroVersions: new Map(this.macroVersions),
      timestamp: Date.now(),
    });

    // Run the expanded code
    return this.interpreter.run(expanded);
  }

  /**
   * Reload a file that has changed
   */
  async reloadFile(file: string): Promise<ASTDiff[]> {
    const source = await this.sourceLoader(file);
    const surfaceSyntax = this.syntaxReader(source, file);

    // Check if this file defines macros
    const macroChanges = this.detectMacroChanges(surfaceSyntax, file);

    // If macros changed, bump their versions
    for (const macro of macroChanges) {
      const current = this.macroVersions.get(macro) ?? 0;
      this.macroVersions.set(macro, current + 1);
    }

    // If macros changed, re-expand dependent files first
    if (macroChanges.length > 0) {
      const dependents = this.depGraph.getAllDependents(macroChanges);
      dependents.delete(file); // Don't re-expand the current file twice

      for (const dep of dependents) {
        await this.reExpandFile(dep);
      }
    }

    // Expand current file
    const { expanded, macrosUsed } = expandWithTracking(
      this.expander,
      surfaceSyntax,
      file
    );

    // Get previous expansion
    const previous = this.cache.get(file);

    // Compute diff
    const diffs = previous
      ? diffExpandedAST(previous.expandedSyntax, expanded)
      : [];

    // Update cache
    this.depGraph.recordDependencies(file, macrosUsed);
    this.cache.set(file, {
      source,
      surfaceSyntax,
      expandedSyntax: expanded,
      macrosUsed,
      macroVersions: new Map(this.macroVersions),
      timestamp: Date.now(),
    });

    // Apply patches
    await this.applyDiffs(diffs);

    return diffs;
  }

  /**
   * Re-expand a file due to macro changes
   */
  private async reExpandFile(file: string): Promise<void> {
    const cached = this.cache.get(file);
    if (!cached) return;

    // Re-expand from cached surface syntax
    const { expanded, macrosUsed } = expandWithTracking(
      this.expander,
      cached.surfaceSyntax,
      file
    );

    // Compute diff
    const diffs = diffExpandedAST(cached.expandedSyntax, expanded);

    // Update cache
    this.depGraph.recordDependencies(file, macrosUsed);
    this.cache.set(file, {
      ...cached,
      expandedSyntax: expanded,
      macrosUsed,
      macroVersions: new Map(this.macroVersions),
      timestamp: Date.now(),
    });

    // Apply patches
    await this.applyDiffs(diffs);
  }

  /**
   * Detect if a file defines new/changed macros
   */
  private detectMacroChanges(syntax: SyntaxList, file: string): string[] {
    const changes: string[] = [];

    for (const form of listElements(syntax)) {
      if (!isList(form)) continue;

      const elements = listElements(form);
      if (elements.length < 2) continue;

      const head = elements[0];
      if (!isSymbol(head)) continue;

      // Check for macro definition
      if (symbolName(head) === "define-syntax") {
        const name = elements[1];
        if (isSymbol(name)) {
          changes.push(symbolName(name));
        }
      }
    }

    return changes;
  }

  /**
   * Apply diffs to the runtime
   */
  private async applyDiffs(diffs: ASTDiff[]): Promise<void> {
    const env = this.interpreter.getGlobalEnv();

    for (const diff of diffs) {
      switch (diff.type) {
        case "function-added":
        case "function-changed":
          // Re-evaluate the function definition
          await this.interpreter.run(
            { type: "syntax", datum: [diff.fn], loc: diff.fn.loc, scopes: new Set() } as SyntaxList
          );
          break;

        case "function-removed":
          // Remove from environment (if possible)
          // Note: Current Environment doesn't support deletion
          break;

        case "binding-added":
        case "binding-changed":
          // Re-evaluate the binding
          // This is trickier - would need to track the full let form
          break;

        case "binding-removed":
          // Remove from environment
          break;

        case "handler-added":
        case "handler-changed":
          // Re-register the handler
          await this.interpreter.run(
            { type: "syntax", datum: [diff.handler], loc: diff.handler.loc, scopes: new Set() } as SyntaxList
          );
          break;

        case "handler-removed":
          // Unregister the handler
          break;
      }
    }
  }

  /**
   * Get statistics about the cache
   */
  getStats(): { files: number; macros: number; dependencies: number } {
    let dependencies = 0;
    // Note: Would need to expose more from DependencyGraph
    return {
      files: this.cache["cache"].size,
      macros: this.macroVersions.size,
      dependencies,
    };
  }
}

// ============ Convenience ============

export function createHotReloader(
  expander: Expander,
  interpreter: CoreInterpreter,
  sourceLoader: (file: string) => Promise<string>,
  syntaxReader: (source: string, file: string) => SyntaxList
): HotReloader {
  return new HotReloader(expander, interpreter, sourceLoader, syntaxReader);
}
