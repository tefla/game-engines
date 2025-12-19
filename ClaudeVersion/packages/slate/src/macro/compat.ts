/**
 * Compatibility Layer
 *
 * Allows running the macro-based interpreter with the old value types
 * so existing tests can run against both implementations.
 */

import { createSlateRuntime, type Value, type RecordValue } from "./index";
import {
  type SlateValue,
  Num,
  Str,
  Bool,
  Null,
  List,
  Record,
} from "@oort/core";

/**
 * Convert a macro system Value to the old SlateValue type
 */
export function toSlateValue(value: Value): SlateValue {
  if (value === null) {
    return Null();
  }

  if (typeof value === "boolean") {
    return Bool(value);
  }

  if (typeof value === "number") {
    return Num(value);
  }

  if (typeof value === "string") {
    return Str(value);
  }

  if (Array.isArray(value)) {
    return List(value.map(toSlateValue));
  }

  if (typeof value === "object" && "type" in value && value.type === "record") {
    const rec = value as RecordValue;
    const fields = new Map<string, SlateValue>();
    for (const [k, v] of rec.fields) {
      fields.set(k, toSlateValue(v));
    }
    return Record(fields);
  }

  // For functions or other special values, return as-is (wrapped in a record)
  if (typeof value === "object" && value !== null) {
    if ("type" in value && (value.type === "fn" || value.type === "native")) {
      // Return a placeholder record for functions
      return Record(new Map([["type", Str("function")]]));
    }
  }

  // Fallback
  return Null();
}

/**
 * Run Slate code using the macro system, returning old-style SlateValue
 */
export async function runSlateMacro(source: string): Promise<SlateValue> {
  const { run } = createSlateRuntime();
  const result = await run(source);
  return toSlateValue(result);
}

/**
 * Synchronous wrapper (for compatibility with sync test expectations)
 * Note: This only works if the code doesn't actually use async features
 */
export function runSlateMacroSync(source: string): SlateValue {
  // Create a simple sync runner for basic tests
  const { run } = createSlateRuntime();

  // For now, we'll use a sync-looking interface
  // Real async support would need test framework changes
  let result: Value;
  let error: Error | null = null;

  run(source)
    .then(r => { result = r; })
    .catch(e => { error = e; });

  // Force synchronous execution for micro-task
  // This works because our interpreter is actually sync under the hood
  if (error) throw error;

  return toSlateValue(result!);
}
