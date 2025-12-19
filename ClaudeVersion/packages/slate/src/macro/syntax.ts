/**
 * Syntax Objects - The foundation of Racket-style macros
 *
 * Syntax objects wrap datums (values) with source location and lexical scope
 * information. This enables:
 * 1. Hygienic macros (scopes prevent variable capture)
 * 2. Good error messages (source locations preserved through expansion)
 * 3. Pattern matching on code structure
 */

// ============ Source Location ============

export interface SourceLoc {
  readonly file?: string;
  readonly line: number;
  readonly col: number;
  readonly span?: number;
}

export function loc(line: number, col: number, file?: string): SourceLoc {
  return { file, line, col };
}

export function locToString(loc: SourceLoc): string {
  const file = loc.file ? `${loc.file}:` : "";
  return `${file}${loc.line}:${loc.col}`;
}

// ============ Scopes (for hygiene) ============

let scopeCounter = 0;

export interface Scope {
  readonly id: number;
  readonly name?: string;
}

export function makeScope(name?: string): Scope {
  return { id: scopeCounter++, name };
}

// ============ Syntax Objects ============

/**
 * A Syntax object wraps a datum with metadata for macro expansion.
 *
 * The datum can be:
 * - Primitives: number, string, boolean, null
 * - Symbols: identifiers (wrapped as SyntaxSymbol)
 * - Lists: arrays of Syntax objects
 * - Records: maps of string to Syntax objects
 */
export interface Syntax<T = Datum> {
  readonly type: "syntax";
  readonly datum: T;
  readonly loc: SourceLoc;
  readonly scopes: ReadonlySet<Scope>;
  readonly props?: Map<string, unknown>;  // Arbitrary properties
}

export type SyntaxSymbol = Syntax<symbol>;
export type SyntaxList = Syntax<Syntax[]>;
export type SyntaxRecord = Syntax<Map<string, Syntax>>;
export type SyntaxPrimitive = Syntax<number | string | boolean | null>;

// A Datum is the unwrapped content of a Syntax object
export type Datum =
  | number
  | string
  | boolean
  | null
  | symbol                    // Identifiers
  | Datum[]                   // Lists
  | Map<string, Datum>;       // Records

// ============ Constructors ============

/**
 * Create a syntax object wrapping a datum
 */
export function stx<T extends Datum>(datum: T, loc: SourceLoc, scopes?: Set<Scope>): Syntax<T> {
  return {
    type: "syntax",
    datum,
    loc,
    scopes: scopes ?? new Set(),
  };
}

/**
 * Create a syntax symbol (identifier)
 */
export function stxSym(name: string, loc: SourceLoc, scopes?: Set<Scope>): SyntaxSymbol {
  return stx(Symbol.for(name), loc, scopes);
}

/**
 * Create a syntax list
 */
export function stxList(elements: Syntax[], loc: SourceLoc, scopes?: Set<Scope>): SyntaxList {
  return stx(elements, loc, scopes);
}

/**
 * Create a syntax record
 */
export function stxRecord(entries: Map<string, Syntax>, loc: SourceLoc, scopes?: Set<Scope>): SyntaxRecord {
  return stx(entries, loc, scopes);
}

/**
 * Create syntax from a JavaScript value (auto-wrap)
 */
export function datum(value: unknown, loc: SourceLoc): Syntax {
  if (value === null || value === undefined) {
    return stx(null, loc);
  }
  if (typeof value === "number" || typeof value === "string" || typeof value === "boolean") {
    return stx(value, loc);
  }
  if (typeof value === "symbol") {
    return stx(value, loc);
  }
  if (Array.isArray(value)) {
    return stxList(value.map((v, i) => datum(v, loc)), loc);
  }
  if (value instanceof Map) {
    const entries = new Map<string, Syntax>();
    for (const [k, v] of value) {
      entries.set(k, datum(v, loc));
    }
    return stxRecord(entries, loc);
  }
  throw new Error(`Cannot convert to syntax: ${typeof value}`);
}

// ============ Predicates ============

export function isSyntax(x: unknown): x is Syntax {
  return typeof x === "object" && x !== null && (x as any).type === "syntax";
}

export function isSymbol(stx: Syntax): stx is SyntaxSymbol {
  return typeof stx.datum === "symbol";
}

export function isList(stx: Syntax): stx is SyntaxList {
  return Array.isArray(stx.datum);
}

export function isRecord(stx: Syntax): stx is SyntaxRecord {
  return stx.datum instanceof Map;
}

export function isPrimitive(stx: Syntax): stx is SyntaxPrimitive {
  const d = stx.datum;
  return d === null || typeof d === "number" || typeof d === "string" || typeof d === "boolean";
}

export function isNull(stx: Syntax): boolean {
  return stx.datum === null;
}

// ============ Accessors ============

/**
 * Get the symbol name from a syntax symbol
 */
export function symbolName(stx: SyntaxSymbol): string {
  return Symbol.keyFor(stx.datum) ?? stx.datum.description ?? "";
}

/**
 * Check if a syntax object is a specific symbol
 */
export function isSymbolNamed(stx: Syntax, name: string): boolean {
  return isSymbol(stx) && symbolName(stx) === name;
}

/**
 * Get elements of a syntax list
 */
export function listElements(stx: SyntaxList): Syntax[] {
  return stx.datum;
}

/**
 * Get the first element of a syntax list (head)
 */
export function listHead(stx: SyntaxList): Syntax | undefined {
  return stx.datum[0];
}

/**
 * Get all but the first element (tail)
 */
export function listTail(stx: SyntaxList): Syntax[] {
  return stx.datum.slice(1);
}

/**
 * Get a record field
 */
export function recordGet(stx: SyntaxRecord, key: string): Syntax | undefined {
  return stx.datum.get(key);
}

/**
 * Get all record keys
 */
export function recordKeys(stx: SyntaxRecord): string[] {
  return Array.from(stx.datum.keys());
}

// ============ Transformations ============

/**
 * Add a scope to a syntax object (for hygiene)
 */
export function addScope(stx: Syntax, scope: Scope): Syntax {
  const newScopes = new Set(stx.scopes);
  newScopes.add(scope);

  if (isList(stx)) {
    return stxList(
      stx.datum.map(child => addScope(child, scope)),
      stx.loc,
      newScopes
    );
  }
  if (isRecord(stx)) {
    const newEntries = new Map<string, Syntax>();
    for (const [k, v] of stx.datum) {
      newEntries.set(k, addScope(v, scope));
    }
    return stxRecord(newEntries, stx.loc, newScopes);
  }

  return { ...stx, scopes: newScopes };
}

/**
 * Remove a scope from a syntax object
 */
export function removeScope(stx: Syntax, scope: Scope): Syntax {
  const newScopes = new Set(stx.scopes);
  newScopes.delete(scope);

  if (isList(stx)) {
    return stxList(
      stx.datum.map(child => removeScope(child, scope)),
      stx.loc,
      newScopes
    );
  }
  if (isRecord(stx)) {
    const newEntries = new Map<string, Syntax>();
    for (const [k, v] of stx.datum) {
      newEntries.set(k, removeScope(v, scope));
    }
    return stxRecord(newEntries, stx.loc, newScopes);
  }

  return { ...stx, scopes: newScopes };
}

/**
 * Replace the datum while preserving location and scopes
 */
export function withDatum<T extends Datum>(stx: Syntax, newDatum: T): Syntax<T> {
  return { ...stx, datum: newDatum };
}

/**
 * Replace the location
 */
export function withLoc(stx: Syntax, newLoc: SourceLoc): Syntax {
  return { ...stx, loc: newLoc };
}

// ============ Syntax Object Display ============

/**
 * Convert syntax to a readable string (for debugging)
 */
export function syntaxToString(stx: Syntax, depth = 0): string {
  const indent = "  ".repeat(depth);

  if (isNull(stx)) return "null";
  if (typeof stx.datum === "number") return String(stx.datum);
  if (typeof stx.datum === "string") return JSON.stringify(stx.datum);
  if (typeof stx.datum === "boolean") return String(stx.datum);
  if (typeof stx.datum === "symbol") return symbolName(stx as SyntaxSymbol);

  if (isList(stx)) {
    if (stx.datum.length === 0) return "()";
    const elements = stx.datum.map(e => syntaxToString(e, depth + 1));
    if (elements.join(" ").length < 60) {
      return `(${elements.join(" ")})`;
    }
    return `(\n${indent}  ${elements.join(`\n${indent}  `)}\n${indent})`;
  }

  if (isRecord(stx)) {
    const entries = Array.from(stx.datum.entries())
      .map(([k, v]) => `${k}: ${syntaxToString(v, depth + 1)}`);
    if (entries.join(", ").length < 60) {
      return `{${entries.join(", ")}}`;
    }
    return `{\n${indent}  ${entries.join(`,\n${indent}  `)}\n${indent}}`;
  }

  return `#<syntax ${typeof stx.datum}>`;
}

/**
 * Pretty print with source location
 */
export function syntaxDebug(stx: Syntax): string {
  return `#<syntax:${locToString(stx.loc)} ${syntaxToString(stx)}>`;
}

// ============ Core Form Symbols ============

// These are the primitive forms that macros expand into
export const CORE_FORMS = {
  let: Symbol.for("let"),
  fn: Symbol.for("fn"),
  if: Symbol.for("if"),
  begin: Symbol.for("begin"),
  set: Symbol.for("set!"),
  quote: Symbol.for("quote"),
  syntax: Symbol.for("syntax"),
  quasiquote: Symbol.for("quasiquote"),
  unquote: Symbol.for("unquote"),
  unquoteSplicing: Symbol.for("unquote-splicing"),
} as const;

export function isCoreForm(stx: Syntax): boolean {
  if (!isSymbol(stx)) return false;
  const name = symbolName(stx);
  return name in CORE_FORMS || Object.values(CORE_FORMS).includes(stx.datum as any);
}
