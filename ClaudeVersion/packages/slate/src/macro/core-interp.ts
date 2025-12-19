/**
 * Core Interpreter
 *
 * Evaluates expanded syntax objects containing only core forms.
 * This is the minimal runtime - all high-level constructs have
 * been expanded to these primitives by the macro expander.
 *
 * Core forms:
 * - (let name value)
 * - (var name value)
 * - (fn name (params...) body...)
 * - (if cond then else?)
 * - (begin expr...)
 * - (set! target value)
 * - (quote datum)
 * - Binary ops: +, -, *, /, ==, !=, <, <=, >, >=
 * - Logical: and, or, not
 * - (list items...)
 * - (. obj prop) - member access
 * - (index obj key) - index access
 * - Function calls: (fn args...)
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
  recordKeys,
  recordGet,
  stx,
  stxList,
  syntaxToString,
  SourceLoc,
} from "./syntax";

// ============ Values ============

export type Value =
  | null
  | boolean
  | number
  | string
  | Value[]
  | RecordValue
  | FnValue
  | NativeFnValue;

export interface RecordValue {
  type: "record";
  fields: Map<string, Value>;
}

export interface FnValue {
  type: "fn";
  name: string;
  params: string[];
  body: Syntax[];
  closure: Environment;
}

export interface NativeFnValue {
  type: "native";
  name: string;
  arity: number | "variadic";
  fn: (args: Value[], interp: CoreInterpreter) => Value | Promise<Value>;
}

// ============ Environment ============

export class Environment {
  private bindings = new Map<string, { value: Value; mutable: boolean }>();

  constructor(private parent?: Environment) {}

  define(name: string, value: Value, mutable = false): void {
    if (this.bindings.has(name)) {
      throw new Error(`Already defined: ${name}`);
    }
    this.bindings.set(name, { value, mutable });
  }

  set(name: string, value: Value): void {
    const binding = this.resolve(name);
    if (!binding) {
      throw new Error(`Undefined variable: ${name}`);
    }
    if (!binding.mutable) {
      throw new Error(`Cannot assign to immutable binding: ${name}`);
    }
    binding.value = value;
  }

  get(name: string): Value {
    const binding = this.resolve(name);
    if (!binding) {
      throw new Error(`Undefined variable: ${name}`);
    }
    return binding.value;
  }

  has(name: string): boolean {
    return this.resolve(name) !== undefined;
  }

  private resolve(name: string): { value: Value; mutable: boolean } | undefined {
    const local = this.bindings.get(name);
    if (local) return local;
    if (this.parent) return this.parent.resolve(name);
    return undefined;
  }

  child(): Environment {
    return new Environment(this);
  }
}

// ============ Interpreter ============

export class CoreInterpreter {
  private globalEnv: Environment;

  constructor() {
    this.globalEnv = new Environment();
    this.registerBuiltins();
  }

  /**
   * Evaluate a program
   */
  async run(program: SyntaxList, env?: Environment): Promise<Value> {
    env = env ?? this.globalEnv;
    let result: Value = null;

    for (const form of listElements(program)) {
      result = await this.eval(form, env);
    }

    return result;
  }

  /**
   * Evaluate a single form
   */
  async eval(stx: Syntax, env: Environment): Promise<Value> {
    // Null
    if (stx.datum === null) {
      return null;
    }

    // Primitives
    if (isPrimitive(stx)) {
      return stx.datum as Value;
    }

    // Symbols (variable lookup)
    if (isSymbol(stx)) {
      const name = symbolName(stx);

      // Check for uppercase identifier (entity name)
      if (name[0] >= "A" && name[0] <= "Z" && !env.has(name)) {
        return name;
      }

      return env.get(name);
    }

    // Records
    if (isRecord(stx)) {
      const fields = new Map<string, Value>();
      for (const key of recordKeys(stx)) {
        fields.set(key, await this.eval(recordGet(stx, key)!, env));
      }
      return { type: "record", fields };
    }

    // Lists (forms)
    if (isList(stx)) {
      const elements = listElements(stx);
      if (elements.length === 0) {
        return null;
      }

      const head = elements[0];

      if (isSymbol(head)) {
        const name = symbolName(head);

        // Core forms
        switch (name) {
          case "let":
            return this.evalLet(stx, env);
          case "var":
            return this.evalVar(stx, env);
          case "fn":
            return this.evalFn(stx, env);
          case "if":
            return this.evalIf(stx, env);
          case "begin":
            return this.evalBegin(stx, env);
          case "set!":
            return this.evalSet(stx, env);
          case "quote":
            return this.evalQuote(stx);
          case "list":
            return this.evalList(stx, env);
          case ".":
            return this.evalMember(stx, env);
          case "index":
            return this.evalIndex(stx, env);
          case "color":
            return this.evalColor(stx);

          // Operators
          case "+":
          case "-":
          case "*":
          case "/":
          case "%":
            return this.evalArithmetic(name, stx, env);

          case "==":
          case "!=":
          case "<":
          case "<=":
          case ">":
          case ">=":
            return this.evalComparison(name, stx, env);

          case "and":
            return this.evalAnd(stx, env);
          case "or":
            return this.evalOr(stx, env);
          case "not":
            return this.evalNot(stx, env);
        }
      }

      // Function call
      return this.evalCall(stx, env);
    }

    throw new Error(`Cannot evaluate: ${syntaxToString(stx)}`);
  }

  private async evalLet(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, nameSym, valueExpr] = listElements(stx);

    if (!isSymbol(nameSym)) {
      throw new Error("let requires identifier");
    }

    const name = symbolName(nameSym);
    const value = await this.eval(valueExpr, env);
    env.define(name, value, false);
    return value;
  }

  private async evalVar(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, nameSym, valueExpr] = listElements(stx);

    if (!isSymbol(nameSym)) {
      throw new Error("var requires identifier");
    }

    const name = symbolName(nameSym);
    const value = await this.eval(valueExpr, env);
    env.define(name, value, true);
    return value;
  }

  private evalFn(stx: SyntaxList, env: Environment): Value {
    const [_, nameSym, paramsList, ...body] = listElements(stx);

    if (!isSymbol(nameSym)) {
      throw new Error("fn requires name");
    }
    if (!isList(paramsList)) {
      throw new Error("fn requires parameter list");
    }

    const name = symbolName(nameSym);
    const params = listElements(paramsList).map(p => {
      if (!isSymbol(p)) throw new Error("Parameters must be identifiers");
      return symbolName(p);
    });

    const fn: FnValue = {
      type: "fn",
      name,
      params,
      body,
      closure: env,
    };

    env.define(name, fn, false);
    return fn;
  }

  private async evalIf(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, cond, then, else_] = listElements(stx);

    const condValue = await this.eval(cond, env);
    if (this.isTruthy(condValue)) {
      return this.eval(then, env);
    } else if (else_) {
      return this.eval(else_, env);
    }
    return null;
  }

  private async evalBegin(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, ...body] = listElements(stx);
    let result: Value = null;

    for (const form of body) {
      result = await this.eval(form, env);
    }

    return result;
  }

  private async evalSet(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, target, valueExpr] = listElements(stx);
    const value = await this.eval(valueExpr, env);

    if (isSymbol(target)) {
      env.set(symbolName(target), value);
      return value;
    }

    // Member assignment: (set! (. obj prop) value)
    if (isList(target)) {
      const targetElems = listElements(target);
      if (targetElems.length === 3 && isSymbol(targetElems[0]) && symbolName(targetElems[0]) === ".") {
        const obj = await this.eval(targetElems[1], env);
        const prop = isSymbol(targetElems[2]) ? symbolName(targetElems[2]) : null;

        if (!prop || !this.isRecord(obj)) {
          throw new Error("Cannot assign to non-record");
        }

        obj.fields.set(prop, value);
        return value;
      }
    }

    throw new Error(`Invalid assignment target: ${syntaxToString(target)}`);
  }

  private evalQuote(stx: SyntaxList): Value {
    const [_, datum] = listElements(stx);
    return this.syntaxToValue(datum);
  }

  private async evalList(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, ...items] = listElements(stx);
    const values: Value[] = [];

    for (const item of items) {
      values.push(await this.eval(item, env));
    }

    return values;
  }

  private async evalMember(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, objExpr, propSym] = listElements(stx);

    const obj = await this.eval(objExpr, env);
    if (!isSymbol(propSym)) {
      throw new Error("Property must be identifier");
    }

    const prop = symbolName(propSym);

    if (this.isRecord(obj)) {
      const value = obj.fields.get(prop);
      if (value === undefined) {
        throw new Error(`Property not found: ${prop}`);
      }
      return value;
    }

    throw new Error(`Cannot access property on ${typeof obj}`);
  }

  private async evalIndex(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, objExpr, indexExpr] = listElements(stx);

    const obj = await this.eval(objExpr, env);
    const index = await this.eval(indexExpr, env);

    if (Array.isArray(obj) && typeof index === "number") {
      return obj[index] ?? null;
    }

    if (this.isRecord(obj) && typeof index === "string") {
      return obj.fields.get(index) ?? null;
    }

    throw new Error(`Cannot index ${typeof obj} with ${typeof index}`);
  }

  private evalColor(stx: SyntaxList): Value {
    const [_, hex] = listElements(stx);
    if (!isPrimitive(hex) || typeof hex.datum !== "string") {
      throw new Error("Color requires hex string");
    }
    // Return as a record with hex value
    return {
      type: "record",
      fields: new Map([
        ["type", "color"],
        ["hex", hex.datum],
      ]),
    };
  }

  private async evalArithmetic(op: string, stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, leftExpr, rightExpr] = listElements(stx);

    // Unary minus
    if (!rightExpr && op === "-") {
      const val = await this.eval(leftExpr, env);
      if (typeof val !== "number") throw new Error("Operand must be number");
      return -val;
    }

    const left = await this.eval(leftExpr, env);
    const right = await this.eval(rightExpr, env);

    // String/list concatenation
    if (op === "+") {
      if (typeof left === "string" || typeof right === "string") {
        return String(left) + String(right);
      }
      if (Array.isArray(left) && Array.isArray(right)) {
        return [...left, ...right];
      }
    }

    if (typeof left !== "number" || typeof right !== "number") {
      throw new Error(`Operands must be numbers for ${op}`);
    }

    switch (op) {
      case "+": return left + right;
      case "-": return left - right;
      case "*": return left * right;
      case "/": return left / right;
      case "%": return left % right;
    }

    throw new Error(`Unknown operator: ${op}`);
  }

  private async evalComparison(op: string, stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, leftExpr, rightExpr] = listElements(stx);

    const left = await this.eval(leftExpr, env);
    const right = await this.eval(rightExpr, env);

    switch (op) {
      case "==": return this.valuesEqual(left, right);
      case "!=": return !this.valuesEqual(left, right);
      case "<":
        if (typeof left !== "number" || typeof right !== "number") throw new Error("Operands must be numbers");
        return left < right;
      case "<=":
        if (typeof left !== "number" || typeof right !== "number") throw new Error("Operands must be numbers");
        return left <= right;
      case ">":
        if (typeof left !== "number" || typeof right !== "number") throw new Error("Operands must be numbers");
        return left > right;
      case ">=":
        if (typeof left !== "number" || typeof right !== "number") throw new Error("Operands must be numbers");
        return left >= right;
    }

    throw new Error(`Unknown comparison: ${op}`);
  }

  private async evalAnd(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, left, right] = listElements(stx);

    const leftVal = await this.eval(left, env);
    if (!this.isTruthy(leftVal)) return leftVal;
    return this.eval(right, env);
  }

  private async evalOr(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, left, right] = listElements(stx);

    const leftVal = await this.eval(left, env);
    if (this.isTruthy(leftVal)) return leftVal;
    return this.eval(right, env);
  }

  private async evalNot(stx: SyntaxList, env: Environment): Promise<Value> {
    const [_, expr] = listElements(stx);
    const val = await this.eval(expr, env);
    return !this.isTruthy(val);
  }

  private async evalCall(stx: SyntaxList, env: Environment): Promise<Value> {
    const [calleeExpr, ...argExprs] = listElements(stx);

    const callee = await this.eval(calleeExpr, env);
    const args: Value[] = [];

    for (const arg of argExprs) {
      args.push(await this.eval(arg, env));
    }

    if (this.isFn(callee)) {
      if (args.length !== callee.params.length) {
        throw new Error(`Expected ${callee.params.length} args, got ${args.length}`);
      }

      const callEnv = callee.closure.child();
      for (let i = 0; i < callee.params.length; i++) {
        callEnv.define(callee.params[i], args[i], false);
      }

      let result: Value = null;
      for (const form of callee.body) {
        result = await this.eval(form, callEnv);
      }
      return result;
    }

    if (this.isNative(callee)) {
      if (callee.arity !== "variadic" && args.length !== callee.arity) {
        throw new Error(`Expected ${callee.arity} args, got ${args.length}`);
      }
      return callee.fn(args, this);
    }

    throw new Error(`Not callable: ${typeof callee}`);
  }

  // ============ Helpers ============

  private isTruthy(val: Value): boolean {
    return val !== null && val !== false;
  }

  private isRecord(val: Value): val is RecordValue {
    return typeof val === "object" && val !== null && !Array.isArray(val) && "type" in val && val.type === "record";
  }

  private isFn(val: Value): val is FnValue {
    return typeof val === "object" && val !== null && "type" in val && val.type === "fn";
  }

  private isNative(val: Value): val is NativeFnValue {
    return typeof val === "object" && val !== null && "type" in val && val.type === "native";
  }

  private valuesEqual(a: Value, b: Value): boolean {
    if (a === b) return true;
    if (typeof a !== typeof b) return false;

    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      return a.every((v, i) => this.valuesEqual(v, b[i]));
    }

    if (this.isRecord(a) && this.isRecord(b)) {
      if (a.fields.size !== b.fields.size) return false;
      for (const [k, v] of a.fields) {
        if (!this.valuesEqual(v, b.fields.get(k)!)) return false;
      }
      return true;
    }

    return false;
  }

  private syntaxToValue(stx: Syntax): Value {
    if (isPrimitive(stx)) {
      return stx.datum as Value;
    }
    if (isSymbol(stx)) {
      return symbolName(stx);
    }
    if (isList(stx)) {
      return listElements(stx).map(e => this.syntaxToValue(e));
    }
    if (isRecord(stx)) {
      const fields = new Map<string, Value>();
      for (const key of recordKeys(stx)) {
        fields.set(key, this.syntaxToValue(recordGet(stx, key)!));
      }
      return { type: "record", fields };
    }
    return null;
  }

  // ============ Built-ins ============

  private registerBuiltins(): void {
    this.defineNative("print", "variadic", (args) => {
      console.log(...args.map(a => this.valueToString(a)));
      return null;
    });

    this.defineNative("say", "variadic", (args) => {
      console.log(...args.map(a => this.valueToString(a)));
      return null;
    });

    this.defineNative("abs", 1, ([x]) => {
      if (typeof x !== "number") throw new Error("abs requires number");
      return Math.abs(x);
    });

    this.defineNative("min", 2, ([a, b]) => {
      if (typeof a !== "number" || typeof b !== "number") throw new Error("min requires numbers");
      return Math.min(a, b);
    });

    this.defineNative("max", 2, ([a, b]) => {
      if (typeof a !== "number" || typeof b !== "number") throw new Error("max requires numbers");
      return Math.max(a, b);
    });

    this.defineNative("len", 1, ([x]) => {
      if (Array.isArray(x)) return x.length;
      if (typeof x === "string") return x.length;
      throw new Error("len requires list or string");
    });
  }

  defineNative(name: string, arity: number | "variadic", fn: NativeFnValue["fn"]): void {
    this.globalEnv.define(name, { type: "native", name, arity, fn }, false);
  }

  private valueToString(val: Value): string {
    if (val === null) return "null";
    if (typeof val === "boolean" || typeof val === "number") return String(val);
    if (typeof val === "string") return val;
    if (Array.isArray(val)) return `[${val.map(v => this.valueToString(v)).join(", ")}]`;
    if (this.isRecord(val)) {
      const entries = Array.from(val.fields.entries())
        .map(([k, v]) => `${k}: ${this.valueToString(v)}`);
      return `{${entries.join(", ")}}`;
    }
    if (this.isFn(val)) return `<fn ${val.name}>`;
    if (this.isNative(val)) return `<native ${val.name}>`;
    return String(val);
  }

  getGlobalEnv(): Environment {
    return this.globalEnv;
  }
}
