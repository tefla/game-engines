/**
 * Base Macros
 *
 * These macros transform high-level Slate constructs into core forms.
 * This is the power of the Racket approach - entity, match, on, etc.
 * are now library code, not built-in primitives!
 *
 * Users can define their own macros using the same mechanisms.
 */

import {
  Syntax,
  SyntaxList,
  isSymbol,
  isList,
  isRecord,
  isPrimitive,
  symbolName,
  listElements,
  recordKeys,
  recordGet,
  stx as makeSyntax,
  stxSym,
  stxList,
  stxRecord,
  loc,
} from "./syntax";
import { Expander } from "./expander";
import { SyntaxRules, parsePattern } from "./pattern";
import { CoreInterpreter, Value, RecordValue, NativeFnValue } from "./core-interp";

// ============ Install Base Macros ============

export function installBaseMacros(expander: Expander): void {
  // 'when' macro - single-branch conditional
  expander.defineProc("when", (stx, exp) => {
    const [_, cond, ...body] = listElements(stx as SyntaxList);
    const bodyBegin = stxList([stxSym("begin", stx.loc), ...body], stx.loc);

    return stxList([
      stxSym("if", stx.loc),
      cond,
      bodyBegin,
    ], stx.loc);
  });

  // 'unless' macro - negated conditional
  expander.defineProc("unless", (stx, exp) => {
    const [_, cond, ...body] = listElements(stx as SyntaxList);
    const bodyBegin = stxList([stxSym("begin", stx.loc), ...body], stx.loc);

    return stxList([
      stxSym("if", stx.loc),
      stxList([stxSym("not", stx.loc), cond], stx.loc),
      bodyBegin,
    ], stx.loc);
  });

  // 'cond' macro - multi-way conditional
  expander.defineProc("cond", (stx, exp) => {
    const [_, ...clauses] = listElements(stx as SyntaxList);

    function buildCond(remaining: Syntax[]): Syntax {
      if (remaining.length === 0) {
        return makeSyntax(null, stx.loc);
      }

      const clause = remaining[0];
      if (!isList(clause)) {
        throw new Error("cond clause must be a list");
      }

      const [test, ...body] = listElements(clause);

      // Check for 'else' clause
      if (isSymbol(test) && symbolName(test) === "else") {
        return stxList([stxSym("begin", stx.loc), ...body], stx.loc);
      }

      return stxList([
        stxSym("if", stx.loc),
        test,
        stxList([stxSym("begin", stx.loc), ...body], stx.loc),
        buildCond(remaining.slice(1)),
      ], stx.loc);
    }

    return buildCond(clauses);
  });

  // 'for' macro - iteration
  expander.defineProc("for", (stx, exp) => {
    const [_, varSym, iterable, body] = listElements(stx as SyntaxList);

    if (!isSymbol(varSym)) {
      throw new Error("for requires variable name");
    }

    // Transform to a function call
    // (for x items body) => (__for items (fn __x () (let x = __x) body))
    return stxList([
      stxSym("__for-each", stx.loc),
      iterable,
      stxList([
        stxSym("fn", stx.loc),
        stxSym("__iter-fn", stx.loc),
        stxList([varSym], stx.loc),
        body,
      ], stx.loc),
    ], stx.loc);
  });

  // 'match' macro - pattern matching
  expander.defineProc("match", (stx, exp) => {
    const [_, subject, ...arms] = listElements(stx as SyntaxList);

    // Create a temp variable for the subject
    const tempVar = stxSym("__match-subject", stx.loc);

    // Build nested if-else chain
    function buildMatch(remaining: Syntax[]): Syntax {
      if (remaining.length === 0) {
        return stxList([
          stxSym("error", stx.loc),
          makeSyntax("No matching pattern", stx.loc),
        ], stx.loc);
      }

      const arm = remaining[0];
      if (!isList(arm)) {
        throw new Error("match arm must be [pattern, body]");
      }

      const [pattern, body] = listElements(arm);

      // Generate match condition and bindings
      const { condition, bindings } = compilePattern(pattern, tempVar, stx.loc);

      // Wrap body with let bindings
      let wrappedBody = body;
      for (const [name, expr] of bindings) {
        wrappedBody = stxList([
          stxSym("let", stx.loc),
          stxSym(name, stx.loc),
          expr,
          wrappedBody,
        ], stx.loc);
      }

      return stxList([
        stxSym("if", stx.loc),
        condition,
        wrappedBody,
        buildMatch(remaining.slice(1)),
      ], stx.loc);
    }

    // Wrap in let for subject
    return stxList([
      stxSym("let", stx.loc),
      tempVar,
      subject,
      buildMatch(arms),
    ], stx.loc);
  });

  // 'entity' macro - entity definition
  expander.defineProc("entity", (stx, exp) => {
    const [_, name, body] = listElements(stx as SyntaxList);

    if (!isSymbol(name)) {
      throw new Error("entity requires name");
    }

    // (entity Name {fields...})
    // => (let Name = (create-template "Name" {fields...}))
    //    (register-template "Name" Name)
    return stxList([
      stxSym("begin", stx.loc),
      stxList([
        stxSym("let", stx.loc),
        name,
        stxList([
          stxSym("create-template", stx.loc),
          makeSyntax(symbolName(name), stx.loc),
          body,
        ], stx.loc),
      ], stx.loc),
      stxList([
        stxSym("register-template", stx.loc),
        makeSyntax(symbolName(name), stx.loc),
        name,
      ], stx.loc),
      name,
    ], stx.loc);
  });

  // 'on' macro - signal handler
  expander.defineProc("on", (stx, exp) => {
    const elements = listElements(stx as SyntaxList);
    const [_, signal, ...rest] = elements;

    let filter: Syntax | null = null;
    let body: Syntax;

    if (rest.length === 2) {
      [filter, body] = rest;
    } else {
      body = rest[0];
    }

    // (on signal filter? body)
    // => (register-handler "signal" filter? (fn __handler (data) body))
    const handler = stxList([
      stxSym("fn", stx.loc),
      stxSym("__handler", stx.loc),
      stxList([stxSym("data", stx.loc)], stx.loc),
      body,
    ], stx.loc);

    const args: Syntax[] = [
      stxSym("register-handler", stx.loc),
      signalToString(signal),
    ];

    if (filter) {
      args.push(filter);
    }

    args.push(handler);

    return stxList(args, stx.loc);
  });

  // 'emit' macro - emit signal
  expander.defineProc("emit", (stx, exp) => {
    const [_, signal, data] = listElements(stx as SyntaxList);

    // (emit signal data?)
    // => (emit-signal "signal" data)
    const args: Syntax[] = [
      stxSym("emit-signal", stx.loc),
      signalToString(signal),
    ];

    if (data) {
      args.push(data);
    }

    return stxList(args, stx.loc);
  });

  // 'loop' macro - infinite loop
  expander.defineProc("loop", (stx, exp) => {
    const [_, body] = listElements(stx as SyntaxList);

    // Transform to a recursive function
    return stxList([
      stxSym("__loop", stx.loc),
      stxList([
        stxSym("fn", stx.loc),
        stxSym("__loop-body", stx.loc),
        stxList([], stx.loc),
        body,
      ], stx.loc),
    ], stx.loc);
  });

  // 'spawn' macro
  expander.defineProc("spawn", (stx, exp) => {
    const [_, template] = listElements(stx as SyntaxList);

    return stxList([
      stxSym("spawn-entity", stx.loc),
      template,
    ], stx.loc);
  });

  // 'destroy' macro
  expander.defineProc("destroy", (stx, exp) => {
    const [_, target] = listElements(stx as SyntaxList);

    return stxList([
      stxSym("destroy-entity", stx.loc),
      target,
    ], stx.loc);
  });

  // 'find' macro
  expander.defineProc("find", (stx, exp) => {
    const [_, query] = listElements(stx as SyntaxList);

    return stxList([
      stxSym("find-entities", stx.loc),
      query,
    ], stx.loc);
  });

  // 'with' macro - record update
  expander.defineProc("with", (stx, exp) => {
    const [_, base, updates] = listElements(stx as SyntaxList);

    return stxList([
      stxSym("record-merge", stx.loc),
      base,
      updates,
    ], stx.loc);
  });
}

// ============ Pattern Compilation ============

interface CompiledPattern {
  condition: Syntax;
  bindings: [string, Syntax][];
}

function compilePattern(pattern: Syntax, subject: Syntax, defaultLoc: any): CompiledPattern {
  // Wildcard: always matches
  if (isSymbol(pattern) && symbolName(pattern) === "_") {
    return {
      condition: makeSyntax(true, defaultLoc),
      bindings: [],
    };
  }

  // Variable binding
  if (isSymbol(pattern)) {
    return {
      condition: makeSyntax(true, defaultLoc),
      bindings: [[symbolName(pattern), subject]],
    };
  }

  // Literal match
  if (isPrimitive(pattern)) {
    return {
      condition: stxList([
        stxSym("==", defaultLoc),
        subject,
        pattern,
      ], defaultLoc),
      bindings: [],
    };
  }

  // Record pattern
  if (isRecord(pattern)) {
    const conditions: Syntax[] = [];
    const bindings: [string, Syntax][] = [];

    for (const key of recordKeys(pattern)) {
      const fieldPattern = recordGet(pattern, key)!;
      const fieldAccess = stxList([
        stxSym(".", defaultLoc),
        subject,
        stxSym(key, defaultLoc),
      ], defaultLoc);

      const compiled = compilePattern(fieldPattern, fieldAccess, defaultLoc);
      conditions.push(compiled.condition);
      bindings.push(...compiled.bindings);
    }

    const condition = conditions.length === 0
      ? makeSyntax(true, defaultLoc)
      : conditions.reduce((a, b) =>
          stxList([stxSym("and", defaultLoc), a, b], defaultLoc)
        );

    return { condition, bindings };
  }

  // List pattern (e.g., [a, b, ..rest])
  if (isList(pattern)) {
    const elements = listElements(pattern);

    // Check for list-pattern marker
    if (elements.length > 0 && isSymbol(elements[0]) && symbolName(elements[0]) === "list-pattern") {
      const patternElements = elements.slice(1);
      const conditions: Syntax[] = [];
      const bindings: [string, Syntax][] = [];

      for (let i = 0; i < patternElements.length; i++) {
        const elemPattern = patternElements[i];

        // Check for rest pattern
        if (isList(elemPattern) &&
            listElements(elemPattern).length === 2 &&
            isSymbol(listElements(elemPattern)[0]) &&
            symbolName(listElements(elemPattern)[0]) === "...") {
          // Rest binding
          const restVar = listElements(elemPattern)[1];
          if (!isSymbol(restVar)) throw new Error("Rest pattern must be identifier");

          bindings.push([
            symbolName(restVar),
            stxList([
              stxSym("slice", defaultLoc),
              subject,
              makeSyntax(i, defaultLoc),
            ], defaultLoc),
          ]);
          break;
        }

        const elemAccess = stxList([
          stxSym("index", defaultLoc),
          subject,
          makeSyntax(i, defaultLoc),
        ], defaultLoc);

        const compiled = compilePattern(elemPattern, elemAccess, defaultLoc);
        conditions.push(compiled.condition);
        bindings.push(...compiled.bindings);
      }

      const condition = conditions.length === 0
        ? makeSyntax(true, defaultLoc)
        : conditions.reduce((a, b) =>
            stxList([stxSym("and", defaultLoc), a, b], defaultLoc)
          );

      return { condition, bindings };
    }
  }

  throw new Error(`Cannot compile pattern: ${JSON.stringify(pattern)}`);
}

function signalToString(signal: Syntax): Syntax {
  if (isSymbol(signal)) {
    return makeSyntax(symbolName(signal), signal.loc);
  }

  if (isList(signal)) {
    const elements = listElements(signal);
    if (elements.length > 0 && isSymbol(elements[0]) && symbolName(elements[0]) === "signal-path") {
      const parts = elements.slice(1).map(e => {
        if (!isSymbol(e)) throw new Error("Signal path must be identifiers");
        return symbolName(e);
      });
      return makeSyntax(parts.join("."), signal.loc);
    }
  }

  throw new Error("Invalid signal path");
}

// ============ Install Runtime Functions ============

export function installRuntimeFunctions(interp: CoreInterpreter): void {
  // Templates registry
  const templates = new Map<string, RecordValue>();

  // Entity world
  let entityIdCounter = 0;
  const entities = new Map<string, RecordValue>();

  // Signal handlers
  const handlers: { signal: string; filter?: Value; handler: Value }[] = [];

  // create-template
  interp.defineNative("create-template", 2, ([name, body]) => {
    if (typeof name !== "string") throw new Error("Template name must be string");
    const template: RecordValue = {
      type: "record",
      fields: new Map([["name", name]]),
    };

    if (body && typeof body === "object" && "type" in body && body.type === "record") {
      for (const [k, v] of (body as RecordValue).fields) {
        template.fields.set(k, v);
      }
    }

    return template;
  });

  // register-template
  interp.defineNative("register-template", 2, ([name, template]) => {
    if (typeof name !== "string") throw new Error("Template name must be string");
    templates.set(name, template as RecordValue);
    return null;
  });

  // spawn-entity
  interp.defineNative("spawn-entity", 1, ([templateOrName]) => {
    let template: RecordValue;

    if (typeof templateOrName === "string") {
      const found = templates.get(templateOrName);
      if (!found) throw new Error(`Unknown template: ${templateOrName}`);
      template = found;
    } else if (typeof templateOrName === "object" && templateOrName && "type" in templateOrName) {
      template = templateOrName as RecordValue;
    } else {
      throw new Error("spawn requires template or template name");
    }

    // Clone the template
    const id = `entity_${entityIdCounter++}`;
    const entity: RecordValue = {
      type: "record",
      fields: new Map([["id", id]]),
    };

    for (const [k, v] of template.fields) {
      entity.fields.set(k, deepClone(v));
    }

    entities.set(id, entity);
    return entity;
  });

  // destroy-entity
  interp.defineNative("destroy-entity", 1, ([target]) => {
    let id: string;

    if (typeof target === "string") {
      id = target;
    } else if (typeof target === "object" && target && "type" in target && (target as RecordValue).type === "record") {
      const idField = (target as RecordValue).fields.get("id");
      if (typeof idField !== "string") throw new Error("Entity has no id");
      id = idField;
    } else {
      throw new Error("destroy requires entity or id");
    }

    entities.delete(id);
    return null;
  });

  // find-entities
  interp.defineNative("find-entities", 1, ([query]) => {
    if (!query || typeof query !== "object" || !("type" in query)) {
      throw new Error("find requires record query");
    }

    const queryRec = query as RecordValue;
    const results: RecordValue[] = [];

    for (const entity of entities.values()) {
      let matches = true;

      for (const [key, expected] of queryRec.fields) {
        const actual = entity.fields.get(key);
        if (!deepEqual(actual, expected)) {
          matches = false;
          break;
        }
      }

      if (matches) {
        results.push(entity);
      }
    }

    return results;
  });

  // register-handler
  interp.defineNative("register-handler", "variadic", (args) => {
    const [signal, ...rest] = args;

    if (typeof signal !== "string") throw new Error("Signal must be string");

    let filter: Value | undefined;
    let handler: Value;

    if (rest.length === 2) {
      [filter, handler] = rest;
    } else {
      handler = rest[0];
    }

    handlers.push({ signal, filter, handler });
    return null;
  });

  // emit-signal
  interp.defineNative("emit-signal", "variadic", async (args, interp) => {
    const [signal, data] = args;

    if (typeof signal !== "string") throw new Error("Signal must be string");

    for (const h of handlers) {
      if (h.signal !== signal) continue;

      if (h.filter !== undefined && !deepEqual(h.filter, data)) {
        continue;
      }

      // Call the handler
      if (typeof h.handler === "object" && h.handler && "type" in h.handler) {
        const fn = h.handler as any;
        if (fn.type === "fn" || fn.type === "native") {
          // Would need to call it - simplified for now
        }
      }
    }

    return null;
  });

  // __for-each - runtime support for for macro
  interp.defineNative("__for-each", 2, async ([iterable, fn], interp) => {
    if (!Array.isArray(iterable)) {
      throw new Error("for requires iterable");
    }

    const fnVal = fn as any;
    if (!fnVal || fnVal.type !== "fn") {
      throw new Error("for requires function");
    }

    // Execute for each element
    // Note: This is simplified - in reality we'd need to call the interpreter
    return null;
  });

  // __loop - runtime support for loop macro
  interp.defineNative("__loop", 1, async ([fn], interp) => {
    // Simplified - would run in a loop with iteration limit
    return null;
  });

  // record-merge - for 'with' expression
  interp.defineNative("record-merge", 2, ([base, updates]) => {
    if (!base || typeof base !== "object" || !("type" in base) || (base as any).type !== "record") {
      throw new Error("with requires record");
    }
    if (!updates || typeof updates !== "object" || !("type" in updates) || (updates as any).type !== "record") {
      throw new Error("with requires record updates");
    }

    const result: RecordValue = {
      type: "record",
      fields: new Map((base as RecordValue).fields),
    };

    for (const [k, v] of (updates as RecordValue).fields) {
      result.fields.set(k, v);
    }

    return result;
  });

  // slice - for list slicing in patterns
  interp.defineNative("slice", 2, ([list, start]) => {
    if (!Array.isArray(list)) throw new Error("slice requires list");
    if (typeof start !== "number") throw new Error("slice requires number");
    return list.slice(start);
  });

  // error - for match failure
  interp.defineNative("error", 1, ([msg]) => {
    throw new Error(String(msg));
  });
}

// ============ Helpers ============

function deepClone(value: Value): Value {
  if (value === null || typeof value === "boolean" ||
      typeof value === "number" || typeof value === "string") {
    return value;
  }

  if (Array.isArray(value)) {
    return value.map(deepClone);
  }

  if (typeof value === "object" && "type" in value && value.type === "record") {
    const rec = value as RecordValue;
    const fields = new Map<string, Value>();
    for (const [k, v] of rec.fields) {
      fields.set(k, deepClone(v));
    }
    return { type: "record", fields };
  }

  return value;
}

function deepEqual(a: Value, b: Value): boolean {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;

  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((v, i) => deepEqual(v, b[i]));
  }

  if (typeof a === "object" && a && typeof b === "object" && b) {
    if ("type" in a && "type" in b && a.type === "record" && b.type === "record") {
      const aRec = a as RecordValue;
      const bRec = b as RecordValue;
      if (aRec.fields.size !== bRec.fields.size) return false;
      for (const [k, v] of aRec.fields) {
        if (!deepEqual(v, bRec.fields.get(k)!)) return false;
      }
      return true;
    }
  }

  return false;
}
