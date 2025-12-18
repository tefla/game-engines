// Runtime value types for the Slate interpreter

import type { Block, Pattern } from "./ast";
import type { Environment } from "./environment";

export type SlateValue =
  | SlateNumber
  | SlateString
  | SlateBool
  | SlateNull
  | SlateList
  | SlateRecord
  | SlateFunction
  | SlateNativeFunction
  | SlateColor
  | SlateSignalPath;

export interface SlateNumber {
  type: "number";
  value: number;
}

export interface SlateString {
  type: "string";
  value: string;
}

export interface SlateBool {
  type: "bool";
  value: boolean;
}

export interface SlateNull {
  type: "null";
}

export interface SlateList {
  type: "list";
  elements: SlateValue[];
}

export interface SlateRecord {
  type: "record";
  fields: Map<string, SlateValue>;
}

export interface SlateFunction {
  type: "function";
  name: string;
  params: Array<{ name: string }>;
  body: Block;
  closure: Environment;
}

export interface SlateNativeFunction {
  type: "native";
  name: string;
  arity: number | "variadic";
  fn: (args: SlateValue[]) => SlateValue;
}

export interface SlateColor {
  type: "color";
  hex: string;
  r: number;
  g: number;
  b: number;
}

export interface SlateSignalPath {
  type: "signal";
  parts: string[];
}

// Helper functions to create values
export const Num = (value: number): SlateNumber => ({ type: "number", value });
export const Str = (value: string): SlateString => ({ type: "string", value });
export const Bool = (value: boolean): SlateBool => ({ type: "bool", value });
export const Null = (): SlateNull => ({ type: "null" });
export const List = (elements: SlateValue[]): SlateList => ({ type: "list", elements });
export const Record = (fields: Map<string, SlateValue>): SlateRecord => ({ type: "record", fields });
export const Color = (hex: string): SlateColor => {
  const cleaned = hex.replace("#", "");
  const r = parseInt(cleaned.slice(0, 2), 16);
  const g = parseInt(cleaned.slice(2, 4), 16);
  const b = parseInt(cleaned.slice(4, 6), 16);
  return { type: "color", hex, r, g, b };
};
export const Signal = (parts: string[]): SlateSignalPath => ({ type: "signal", parts });

// Type guards
export const isNumber = (v: SlateValue): v is SlateNumber => v.type === "number";
export const isString = (v: SlateValue): v is SlateString => v.type === "string";
export const isBool = (v: SlateValue): v is SlateBool => v.type === "bool";
export const isNull = (v: SlateValue): v is SlateNull => v.type === "null";
export const isList = (v: SlateValue): v is SlateList => v.type === "list";
export const isRecord = (v: SlateValue): v is SlateRecord => v.type === "record";
export const isFunction = (v: SlateValue): v is SlateFunction => v.type === "function";
export const isNativeFunction = (v: SlateValue): v is SlateNativeFunction => v.type === "native";
export const isCallable = (v: SlateValue): v is SlateFunction | SlateNativeFunction =>
  v.type === "function" || v.type === "native";
export const isColor = (v: SlateValue): v is SlateColor => v.type === "color";
export const isSignal = (v: SlateValue): v is SlateSignalPath => v.type === "signal";

// Truthiness
export const isTruthy = (v: SlateValue): boolean => {
  if (v.type === "null") return false;
  if (v.type === "bool") return v.value;
  if (v.type === "number") return v.value !== 0;
  if (v.type === "string") return v.value.length > 0;
  if (v.type === "list") return v.elements.length > 0;
  return true;
};

// Value to string for display
export const stringify = (v: SlateValue): string => {
  switch (v.type) {
    case "number":
      return String(v.value);
    case "string":
      return v.value;
    case "bool":
      return v.value ? "true" : "false";
    case "null":
      return "null";
    case "list":
      return `[${v.elements.map(stringify).join(", ")}]`;
    case "record": {
      const entries = Array.from(v.fields.entries())
        .map(([k, val]) => `${k}: ${stringify(val)}`)
        .join(", ");
      return `{${entries}}`;
    }
    case "function":
      return `<fn ${v.name}>`;
    case "native":
      return `<native ${v.name}>`;
    case "color":
      return v.hex;
    case "signal":
      return v.parts.join(".");
  }
};
