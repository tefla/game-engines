/**
 * Pattern Matching for Macros
 *
 * Supports Racket-style syntax patterns:
 * - $name: Match anything, bind to 'name'
 * - $items...: Match zero or more, bind to 'items' as list
 * - Literals: Match exact values
 * - Lists: Match list structure
 * - Records: Match record structure
 *
 * Example:
 *   Pattern: (entity $name: $fields...)
 *   Input:   (entity Door: (locked: true) (color: #8B4513))
 *   Bindings: {name: Door, fields: [(locked: true), (color: #8B4513)]}
 */

import {
  Syntax,
  SyntaxList,
  SyntaxSymbol,
  isSymbol,
  isList,
  isRecord,
  isPrimitive,
  symbolName,
  listElements,
  stxList,
  stx,
} from "./syntax";

// ============ Pattern Types ============

export interface PatternVar {
  type: "var";
  name: string;
  ellipsis: boolean; // true if $name...
}

export interface PatternLiteral {
  type: "literal";
  value: number | string | boolean | null;
}

export interface PatternSymbol {
  type: "symbol";
  name: string;
}

export interface PatternList {
  type: "list";
  elements: Pattern[];
  tail?: PatternVar; // For patterns like (a b $rest...)
}

export interface PatternRecord {
  type: "record";
  fields: Map<string, Pattern>;
}

export interface PatternWildcard {
  type: "wildcard";
}

export type Pattern =
  | PatternVar
  | PatternLiteral
  | PatternSymbol
  | PatternList
  | PatternRecord
  | PatternWildcard;

// ============ Pattern Parsing ============

/**
 * Parse a syntax object into a pattern
 */
export function parsePattern(stx: Syntax): Pattern {
  // Wildcard
  if (isSymbol(stx) && symbolName(stx) === "_") {
    return { type: "wildcard" };
  }

  // Pattern variable: $name or $name...
  if (isSymbol(stx)) {
    const name = symbolName(stx);
    if (name.startsWith("$")) {
      const isEllipsis = name.endsWith("...");
      const varName = isEllipsis ? name.slice(1, -3) : name.slice(1);
      return { type: "var", name: varName, ellipsis: isEllipsis };
    }
    return { type: "symbol", name };
  }

  // Literal patterns
  if (isPrimitive(stx)) {
    return { type: "literal", value: stx.datum as any };
  }

  // List patterns
  if (isList(stx)) {
    const elements = listElements(stx);
    const parsedElements: Pattern[] = [];
    let tail: PatternVar | undefined;

    for (let i = 0; i < elements.length; i++) {
      const elem = elements[i];
      const parsed = parsePattern(elem);

      // Check if this is an ellipsis variable at the end
      if (parsed.type === "var" && parsed.ellipsis) {
        tail = parsed;
        break;
      }

      parsedElements.push(parsed);
    }

    return { type: "list", elements: parsedElements, tail };
  }

  // Record patterns
  if (isRecord(stx)) {
    const fields = new Map<string, Pattern>();
    for (const [key, value] of stx.datum) {
      fields.set(key, parsePattern(value));
    }
    return { type: "record", fields };
  }

  throw new Error(`Cannot parse pattern: ${JSON.stringify(stx.datum)}`);
}

// ============ Pattern Matching ============

export type Bindings = Map<string, Syntax | Syntax[]>;

/**
 * Match a syntax object against a pattern
 * Returns bindings if successful, null if no match
 */
export function matchPattern(pattern: Pattern, stx: Syntax): Bindings | null {
  const bindings = new Map<string, Syntax | Syntax[]>();

  if (matchPatternInner(pattern, stx, bindings)) {
    return bindings;
  }

  return null;
}

function matchPatternInner(pattern: Pattern, stx: Syntax, bindings: Bindings): boolean {
  switch (pattern.type) {
    case "wildcard":
      return true;

    case "var":
      if (pattern.ellipsis) {
        // Ellipsis variables should be handled by list matching
        // If we get here, it's a single-element match
        bindings.set(pattern.name, [stx]);
        return true;
      }
      bindings.set(pattern.name, stx);
      return true;

    case "literal":
      if (!isPrimitive(stx)) return false;
      return stx.datum === pattern.value;

    case "symbol":
      if (!isSymbol(stx)) return false;
      return symbolName(stx) === pattern.name;

    case "list":
      return matchListPattern(pattern, stx, bindings);

    case "record":
      return matchRecordPattern(pattern, stx, bindings);
  }
}

function matchListPattern(pattern: PatternList, stx: Syntax, bindings: Bindings): boolean {
  if (!isList(stx)) return false;

  const elements = listElements(stx);
  const patternElements = pattern.elements;

  if (pattern.tail) {
    // Pattern has ellipsis: (a b $rest...)
    if (elements.length < patternElements.length) {
      return false;
    }

    // Match fixed elements
    for (let i = 0; i < patternElements.length; i++) {
      if (!matchPatternInner(patternElements[i], elements[i], bindings)) {
        return false;
      }
    }

    // Bind rest to tail variable
    bindings.set(pattern.tail.name, elements.slice(patternElements.length));
    return true;
  }

  // No ellipsis: exact length match
  if (elements.length !== patternElements.length) {
    return false;
  }

  for (let i = 0; i < patternElements.length; i++) {
    if (!matchPatternInner(patternElements[i], elements[i], bindings)) {
      return false;
    }
  }

  return true;
}

function matchRecordPattern(pattern: PatternRecord, stx: Syntax, bindings: Bindings): boolean {
  if (!isRecord(stx)) return false;

  for (const [key, fieldPattern] of pattern.fields) {
    const fieldValue = stx.datum.get(key);
    if (fieldValue === undefined) {
      return false;
    }
    if (!matchPatternInner(fieldPattern, fieldValue, bindings)) {
      return false;
    }
  }

  return true;
}

// ============ Template Substitution ============

/**
 * Apply bindings to a template syntax object
 *
 * Template syntax:
 * - $name: Substitute with bound value
 * - $items...: Splice bound list
 * - ~expr: Unquote (evaluate and substitute)
 * - ~@expr: Unquote-splicing (evaluate and splice)
 */
export function applyTemplate(template: Syntax, bindings: Bindings): Syntax {
  // Variable substitution
  if (isSymbol(template)) {
    const name = symbolName(template);

    // Check for $var reference
    if (name.startsWith("$")) {
      const varName = name.endsWith("...") ? name.slice(1, -3) : name.slice(1);
      const bound = bindings.get(varName);
      if (bound === undefined) {
        throw new Error(`Unbound pattern variable: ${name}`);
      }
      if (Array.isArray(bound)) {
        // Return as list for ellipsis vars
        return stxList(bound, template.loc);
      }
      return bound;
    }

    // Check for ~unquote (we use ~ prefix)
    if (name.startsWith("~")) {
      const varName = name.slice(1);
      if (varName.startsWith("@")) {
        // Splice: ~@var
        const spliceVar = varName.slice(1);
        const bound = bindings.get(spliceVar);
        if (!bound || !Array.isArray(bound)) {
          throw new Error(`Splice requires list binding: ${spliceVar}`);
        }
        // This will be handled by list template application
        return stxList(bound, template.loc);
      }
      const bound = bindings.get(varName);
      if (bound === undefined) {
        throw new Error(`Unbound template variable: ${varName}`);
      }
      if (Array.isArray(bound)) {
        throw new Error(`Cannot substitute list variable ${varName} in non-splice context`);
      }
      return bound;
    }

    return template;
  }

  // List template: recursively apply, handle splicing
  if (isList(template)) {
    const result: Syntax[] = [];

    for (const elem of listElements(template)) {
      // Check for splice
      if (isSymbol(elem)) {
        const name = symbolName(elem);
        if (name.startsWith("$") && name.endsWith("...")) {
          const varName = name.slice(1, -3);
          const bound = bindings.get(varName);
          if (bound && Array.isArray(bound)) {
            result.push(...bound);
            continue;
          }
        }
        if (name.startsWith("~@")) {
          const varName = name.slice(2);
          const bound = bindings.get(varName);
          if (bound && Array.isArray(bound)) {
            result.push(...bound);
            continue;
          }
        }
      }

      result.push(applyTemplate(elem, bindings));
    }

    return stxList(result, template.loc);
  }

  // Record template
  if (isRecord(template)) {
    const entries = new Map<string, Syntax>();
    for (const [key, value] of template.datum) {
      entries.set(key, applyTemplate(value, bindings));
    }
    return { ...template, datum: entries };
  }

  // Primitives pass through
  return template;
}

// ============ Syntax Rules ============

export interface SyntaxRule {
  pattern: Pattern;
  template: Syntax;
}

/**
 * A syntax transformer defined by pattern-matching rules
 */
export class SyntaxRules {
  constructor(
    public readonly name: string,
    public readonly rules: SyntaxRule[]
  ) {}

  /**
   * Transform a syntax object using the first matching rule
   */
  transform(stx: Syntax): Syntax | null {
    for (const rule of this.rules) {
      const bindings = matchPattern(rule.pattern, stx);
      if (bindings) {
        return applyTemplate(rule.template, bindings);
      }
    }
    return null;
  }
}

/**
 * Create a simple single-rule transformer
 */
export function syntaxRule(pattern: Syntax, template: Syntax): SyntaxRules {
  return new SyntaxRules("anonymous", [
    {
      pattern: parsePattern(pattern),
      template,
    },
  ]);
}
