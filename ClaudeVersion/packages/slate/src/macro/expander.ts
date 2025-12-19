/**
 * Macro Expander
 *
 * Transforms syntax objects by expanding macros into core forms.
 * Supports:
 * - Hygienic macros (via scope sets)
 * - Dependency tracking (for hot reload)
 * - Recursive expansion
 *
 * Core forms (not expanded):
 * - let, fn, if, begin, set!, quote, syntax
 */

import {
  Syntax,
  SyntaxList,
  SyntaxSymbol,
  Scope,
  SourceLoc,
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
  stx,
  stxSym,
  stxList,
  syntaxToString,
  CORE_FORMS,
} from "./syntax";
import { SyntaxRules, parsePattern, matchPattern, applyTemplate } from "./pattern";

// ============ Transformer Types ============

export type Transformer = SyntaxRules | ProceduralTransformer;

export interface ProceduralTransformer {
  type: "procedural";
  name: string;
  transform: (stx: Syntax, expander: Expander) => Syntax;
}

// ============ Expansion Context ============

export interface ExpansionContext {
  // Current file being expanded
  file?: string;

  // Macros used during expansion (for dependency tracking)
  macrosUsed: Set<string>;

  // Current expansion depth (for cycle detection)
  depth: number;

  // Maximum expansion depth
  maxDepth: number;
}

function defaultContext(): ExpansionContext {
  return {
    macrosUsed: new Set(),
    depth: 0,
    maxDepth: 1000,
  };
}

// ============ Expander ============

export class Expander {
  private transformers = new Map<string, Transformer>();
  private introScopes = new Map<string, Scope>();

  constructor() {
    // Register built-in core form handlers
    this.registerCoreForms();
  }

  /**
   * Register a macro transformer
   */
  define(name: string, transformer: Transformer): void {
    this.transformers.set(name, transformer);
    // Create introduction scope for hygiene
    this.introScopes.set(name, makeScope(name));
  }

  /**
   * Define a syntax-rules macro
   */
  defineSyntaxRules(name: string, rules: SyntaxRules): void {
    this.define(name, rules);
  }

  /**
   * Define a procedural macro
   */
  defineProc(
    name: string,
    transform: (stx: Syntax, expander: Expander) => Syntax
  ): void {
    this.define(name, { type: "procedural", name, transform });
  }

  /**
   * Check if a name is a macro
   */
  isMacro(name: string): boolean {
    return this.transformers.has(name);
  }

  /**
   * Get a transformer by name
   */
  getTransformer(name: string): Transformer | undefined {
    return this.transformers.get(name);
  }

  /**
   * Expand a program (list of forms)
   */
  expandProgram(program: SyntaxList, ctx?: ExpansionContext): SyntaxList {
    ctx = ctx ?? defaultContext();
    const expanded = listElements(program).map(form => this.expand(form, ctx!));
    return stxList(expanded, program.loc);
  }

  /**
   * Expand a single syntax object
   */
  expand(stx: Syntax, ctx?: ExpansionContext): Syntax {
    ctx = ctx ?? defaultContext();

    // Check depth
    if (ctx.depth > ctx.maxDepth) {
      throw new Error(`Macro expansion depth exceeded at ${syntaxToString(stx)}`);
    }

    // Primitives don't expand
    if (isPrimitive(stx)) {
      return stx;
    }

    // Symbols might be references, don't expand
    if (isSymbol(stx)) {
      return stx;
    }

    // Records: expand values
    if (isRecord(stx)) {
      const entries = new Map<string, Syntax>();
      for (const [key, value] of stx.datum) {
        entries.set(key, this.expand(value, ctx));
      }
      return { ...stx, datum: entries };
    }

    // Lists: check for macro/core form at head
    if (isList(stx)) {
      const elements = listElements(stx);
      if (elements.length === 0) {
        return stx;
      }

      const head = elements[0];

      // Check if head is a symbol
      if (isSymbol(head)) {
        const name = symbolName(head);

        // Check for core forms
        if (this.isCoreForm(name)) {
          return this.expandCoreForm(name, stx, ctx);
        }

        // Check for macro
        const transformer = this.transformers.get(name);
        if (transformer) {
          ctx.macrosUsed.add(name);

          // Apply transformation
          const expanded = this.applyTransformer(transformer, stx, ctx);

          // Recursively expand the result
          return this.expand(expanded, { ...ctx, depth: ctx.depth + 1 });
        }
      }

      // Not a macro or core form: expand as function call
      return this.expandCall(stx, ctx);
    }

    return stx;
  }

  /**
   * Apply a transformer to syntax
   */
  private applyTransformer(transformer: Transformer, stx: Syntax, ctx: ExpansionContext): Syntax {
    if ("rules" in transformer) {
      // SyntaxRules
      const result = transformer.transform(stx);
      if (result === null) {
        throw new Error(`No matching rule for macro ${transformer.name}: ${syntaxToString(stx)}`);
      }

      // Add introduction scope for hygiene
      const scope = this.introScopes.get(transformer.name);
      if (scope) {
        return addScope(result, scope);
      }
      return result;
    }

    // Procedural transformer
    return transformer.transform(stx, this);
  }

  /**
   * Check if a name is a core form
   */
  private isCoreForm(name: string): boolean {
    return [
      "let", "var", "fn", "if", "begin", "set!", "quote", "syntax",
      "quasiquote", "unquote", "unquote-splicing",
      "define-syntax", "list", "and", "or"
    ].includes(name);
  }

  /**
   * Expand a core form
   */
  private expandCoreForm(name: string, stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const elements = listElements(stx);

    switch (name) {
      case "let":
      case "var":
        return this.expandLetVar(stx, ctx);

      case "fn":
        return this.expandFn(stx, ctx);

      case "if":
        return this.expandIf(stx, ctx);

      case "begin":
        return this.expandBegin(stx, ctx);

      case "set!":
        return this.expandSet(stx, ctx);

      case "quote":
        // Don't expand inside quote
        return stx;

      case "syntax":
        // Don't expand inside syntax
        return stx;

      case "quasiquote":
        return this.expandQuasiquote(stx, ctx);

      case "define-syntax":
        return this.expandDefineSyntax(stx, ctx);

      case "list":
        return this.expandList(stx, ctx);

      case "and":
      case "or":
        return this.expandLogical(stx, ctx);

      default:
        // Unknown core form, expand children
        return stxList(
          elements.map(e => this.expand(e, ctx)),
          stx.loc
        );
    }
  }

  private expandLetVar(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, name, value] = listElements(stx);
    return stxList([head, name, this.expand(value, ctx)], stx.loc);
  }

  private expandFn(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const elements = listElements(stx);
    // (fn name (params...) body...)
    const [head, name, params, ...body] = elements;

    // Create a new scope for the function body
    const fnScope = makeScope("fn");
    const expandedBody = body.map(b => this.expand(addScope(b, fnScope), ctx));

    return stxList([head, name, params, ...expandedBody], stx.loc);
  }

  private expandIf(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const elements = listElements(stx);
    const [head, cond, then, else_] = elements;

    const expanded: Syntax[] = [
      head,
      this.expand(cond, ctx),
      this.expand(then, ctx),
    ];

    if (else_) {
      expanded.push(this.expand(else_, ctx));
    }

    return stxList(expanded, stx.loc);
  }

  private expandBegin(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, ...body] = listElements(stx);
    return stxList([head, ...body.map(b => this.expand(b, ctx))], stx.loc);
  }

  private expandSet(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, target, value] = listElements(stx);
    return stxList([head, target, this.expand(value, ctx)], stx.loc);
  }

  private expandQuasiquote(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, template] = listElements(stx);
    return stxList([head, this.expandQuasiquoteTemplate(template, ctx, 1)], stx.loc);
  }

  private expandQuasiquoteTemplate(stx: Syntax, ctx: ExpansionContext, depth: number): Syntax {
    if (isList(stx)) {
      const elements = listElements(stx);
      if (elements.length > 0 && isSymbol(elements[0])) {
        const name = symbolName(elements[0]);

        if (name === "unquote" && depth === 1) {
          // Expand the unquoted expression
          return stxList([elements[0], this.expand(elements[1], ctx)], stx.loc);
        }

        if (name === "unquote-splicing" && depth === 1) {
          return stxList([elements[0], this.expand(elements[1], ctx)], stx.loc);
        }

        if (name === "quasiquote") {
          // Increase depth
          return stxList(
            [elements[0], this.expandQuasiquoteTemplate(elements[1], ctx, depth + 1)],
            stx.loc
          );
        }

        if ((name === "unquote" || name === "unquote-splicing") && depth > 1) {
          // Decrease depth
          return stxList(
            [elements[0], this.expandQuasiquoteTemplate(elements[1], ctx, depth - 1)],
            stx.loc
          );
        }
      }

      // Recursively process list elements
      return stxList(
        elements.map(e => this.expandQuasiquoteTemplate(e, ctx, depth)),
        stx.loc
      );
    }

    return stx;
  }

  private expandDefineSyntax(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, name, params, body] = listElements(stx);

    if (!isSymbol(name)) {
      throw new Error("define-syntax requires a name");
    }

    // Register the macro
    // For now, we treat the body as a template and params as pattern vars
    const macroName = symbolName(name);

    if (!isList(params)) {
      throw new Error("define-syntax requires parameter list");
    }

    // Create a simple syntax-rules transformer
    // Pattern: (macro-name $param1 $param2 ...)
    const paramNames = listElements(params).map(p => {
      if (!isSymbol(p)) throw new Error("Parameters must be identifiers");
      return "$" + symbolName(p);
    });

    const patternElements: Syntax[] = [
      stxSym(macroName, name.loc),
      ...paramNames.map(p => stxSym(p, name.loc))
    ];

    const pattern = stxList(patternElements, name.loc);
    const rules = new SyntaxRules(macroName, [
      { pattern: parsePattern(pattern), template: body }
    ]);

    this.define(macroName, rules);

    // Return null/void
    return stxList([stxSym("begin", stx.loc)], stx.loc);
  }

  private expandList(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, ...elements] = listElements(stx);
    return stxList([head, ...elements.map(e => this.expand(e, ctx))], stx.loc);
  }

  private expandLogical(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const [head, ...args] = listElements(stx);
    return stxList([head, ...args.map(a => this.expand(a, ctx))], stx.loc);
  }

  private expandCall(stx: SyntaxList, ctx: ExpansionContext): Syntax {
    const elements = listElements(stx);
    return stxList(elements.map(e => this.expand(e, ctx)), stx.loc);
  }

  /**
   * Register core form handlers
   */
  private registerCoreForms(): void {
    // Core forms are handled directly in expandCoreForm
    // This is where we could add special handling if needed
  }
}

// ============ Hot Reload Support ============

export interface ExpansionResult {
  expanded: SyntaxList;
  macrosUsed: Set<string>;
}

/**
 * Expand with dependency tracking for hot reload
 */
export function expandWithTracking(
  expander: Expander,
  program: SyntaxList,
  file?: string
): ExpansionResult {
  const ctx: ExpansionContext = {
    file,
    macrosUsed: new Set(),
    depth: 0,
    maxDepth: 1000,
  };

  const expanded = expander.expandProgram(program, ctx);

  return {
    expanded,
    macrosUsed: ctx.macrosUsed,
  };
}

// ============ Convenience Functions ============

/**
 * Create a new expander with standard macros
 */
export function createExpander(): Expander {
  const expander = new Expander();

  // We'll register standard macros in base-macros.ts
  return expander;
}
