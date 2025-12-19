/**
 * Minimal S-Expression Evaluator
 *
 * Core forms:
 * - (define name value)
 * - (let ((name value) ...) body...)
 * - (fn (params...) body...)  or  (lambda (params...) body...)
 * - (if condition then else?)
 * - (quote expr)
 * - (begin expr...)
 * - (set! name value)
 * - (cond (test expr)... (else expr)?)
 */

import {
  SExpr,
  sym,
  num,
  str,
  bool,
  list,
  nil,
  isSymbol,
  isNumber,
  isString,
  isBoolean,
  isList,
  isNil,
  sexprToString,
} from "./reader";

// ============ Values ============

export type Value =
  | { type: "number"; value: number }
  | { type: "string"; value: string }
  | { type: "boolean"; value: boolean }
  | { type: "nil" }
  | { type: "list"; elements: Value[] }
  | { type: "closure"; params: string[]; body: SExpr[]; env: Environment }
  | { type: "primitive"; name: string; fn: (...args: Value[]) => Value }
  | { type: "syntax"; datum: SExpr };  // For syntax objects

// Value constructors
export const vNum = (value: number): Value => ({ type: "number", value });
export const vStr = (value: string): Value => ({ type: "string", value });
export const vBool = (value: boolean): Value => ({ type: "boolean", value });
export const vNil: Value = { type: "nil" };
export const vList = (...elements: Value[]): Value => ({ type: "list", elements });
export const vClosure = (params: string[], body: SExpr[], env: Environment): Value =>
  ({ type: "closure", params, body, env });
export const vPrimitive = (name: string, fn: (...args: Value[]) => Value): Value =>
  ({ type: "primitive", name, fn });
export const vSyntax = (datum: SExpr): Value => ({ type: "syntax", datum });

// ============ Environment ============

export class Environment {
  private bindings = new Map<string, Value>();
  private parent: Environment | null;

  constructor(parent: Environment | null = null) {
    this.parent = parent;
  }

  define(name: string, value: Value): void {
    this.bindings.set(name, value);
  }

  set(name: string, value: Value): void {
    if (this.bindings.has(name)) {
      this.bindings.set(name, value);
    } else if (this.parent) {
      this.parent.set(name, value);
    } else {
      throw new Error(`Undefined variable: ${name}`);
    }
  }

  lookup(name: string): Value {
    if (this.bindings.has(name)) {
      return this.bindings.get(name)!;
    }
    if (this.parent) {
      return this.parent.lookup(name);
    }
    throw new Error(`Undefined variable: ${name}`);
  }

  has(name: string): boolean {
    if (this.bindings.has(name)) return true;
    if (this.parent) return this.parent.has(name);
    return false;
  }

  extend(): Environment {
    return new Environment(this);
  }
}

// ============ Evaluator ============

export class Evaluator {
  private globalEnv: Environment;

  constructor() {
    this.globalEnv = new Environment();
  }

  /**
   * Add a primitive function
   */
  addPrimitive(name: string, fn: (...args: Value[]) => Value): void {
    this.globalEnv.define(name, vPrimitive(name, fn));
  }

  /**
   * Define a global binding
   */
  define(name: string, value: Value): void {
    this.globalEnv.define(name, value);
  }

  /**
   * Evaluate a list of expressions, return last value
   */
  evalProgram(exprs: SExpr[]): Value {
    let result: Value = vNil;
    for (const expr of exprs) {
      result = this.eval(expr, this.globalEnv);
    }
    return result;
  }

  /**
   * Evaluate a single expression
   */
  eval(expr: SExpr, env: Environment): Value {
    // Self-evaluating
    if (isNumber(expr)) return vNum(expr.value);
    if (isString(expr)) return vStr(expr.value);
    if (isBoolean(expr)) return vBool(expr.value);
    if (isNil(expr)) return vNil;

    // Symbol lookup
    if (isSymbol(expr)) {
      return env.lookup(expr.name);
    }

    // List (special forms or application)
    if (isList(expr)) {
      if (expr.elements.length === 0) {
        return vList(); // Empty list
      }

      const head = expr.elements[0];
      if (isSymbol(head)) {
        const name = head.name;

        // Special forms
        switch (name) {
          case "define": return this.evalDefine(expr.elements, env);
          case "let": return this.evalLet(expr.elements, env);
          case "let*": return this.evalLetStar(expr.elements, env);
          case "fn":
          case "lambda": return this.evalLambda(expr.elements, env);
          case "if": return this.evalIf(expr.elements, env);
          case "cond": return this.evalCond(expr.elements, env);
          case "quote": return this.evalQuote(expr.elements);
          case "quasiquote": return this.evalQuasiquote(expr.elements, env);
          case "begin": return this.evalBegin(expr.elements, env);
          case "set!": return this.evalSet(expr.elements, env);
          case "and": return this.evalAnd(expr.elements, env);
          case "or": return this.evalOr(expr.elements, env);
        }
      }

      // Function application
      return this.evalApplication(expr.elements, env);
    }

    throw new Error(`Cannot evaluate: ${sexprToString(expr)}`);
  }

  private evalDefine(elements: SExpr[], env: Environment): Value {
    // (define name value) or (define (name params...) body...)
    if (elements.length < 3) {
      throw new Error("define requires name and value");
    }

    const nameOrSig = elements[1];

    if (isSymbol(nameOrSig)) {
      // (define name value)
      const value = this.eval(elements[2], env);
      env.define(nameOrSig.name, value);
      return vNil;
    }

    if (isList(nameOrSig) && nameOrSig.elements.length > 0) {
      // (define (name params...) body...)
      const [nameExpr, ...paramExprs] = nameOrSig.elements;
      if (!isSymbol(nameExpr)) {
        throw new Error("Function name must be a symbol");
      }

      const params = paramExprs.map(p => {
        if (!isSymbol(p)) throw new Error("Parameters must be symbols");
        return p.name;
      });

      const body = elements.slice(2);
      env.define(nameExpr.name, vClosure(params, body, env));
      return vNil;
    }

    throw new Error("Invalid define syntax");
  }

  private evalLet(elements: SExpr[], env: Environment): Value {
    // (let ((name value) ...) body...)
    if (elements.length < 3) {
      throw new Error("let requires bindings and body");
    }

    const bindingsExpr = elements[1];
    if (!isList(bindingsExpr)) {
      throw new Error("let bindings must be a list");
    }

    const letEnv = env.extend();

    for (const binding of bindingsExpr.elements) {
      if (!isList(binding) || binding.elements.length !== 2) {
        throw new Error("Invalid let binding");
      }
      const [nameExpr, valueExpr] = binding.elements;
      if (!isSymbol(nameExpr)) {
        throw new Error("Binding name must be a symbol");
      }
      const value = this.eval(valueExpr, env); // Evaluate in outer env
      letEnv.define(nameExpr.name, value);
    }

    // Evaluate body
    let result: Value = vNil;
    for (let i = 2; i < elements.length; i++) {
      result = this.eval(elements[i], letEnv);
    }
    return result;
  }

  private evalLetStar(elements: SExpr[], env: Environment): Value {
    // (let* ((name value) ...) body...)
    if (elements.length < 3) {
      throw new Error("let* requires bindings and body");
    }

    const bindingsExpr = elements[1];
    if (!isList(bindingsExpr)) {
      throw new Error("let* bindings must be a list");
    }

    let letEnv = env.extend();

    for (const binding of bindingsExpr.elements) {
      if (!isList(binding) || binding.elements.length !== 2) {
        throw new Error("Invalid let* binding");
      }
      const [nameExpr, valueExpr] = binding.elements;
      if (!isSymbol(nameExpr)) {
        throw new Error("Binding name must be a symbol");
      }
      const value = this.eval(valueExpr, letEnv); // Evaluate in current env
      letEnv.define(nameExpr.name, value);
    }

    // Evaluate body
    let result: Value = vNil;
    for (let i = 2; i < elements.length; i++) {
      result = this.eval(elements[i], letEnv);
    }
    return result;
  }

  private evalLambda(elements: SExpr[], env: Environment): Value {
    // (fn (params...) body...) or (lambda (params...) body...)
    if (elements.length < 3) {
      throw new Error("fn requires parameters and body");
    }

    const paramsExpr = elements[1];
    if (!isList(paramsExpr)) {
      throw new Error("fn parameters must be a list");
    }

    const params = paramsExpr.elements.map(p => {
      if (!isSymbol(p)) throw new Error("Parameters must be symbols");
      return p.name;
    });

    const body = elements.slice(2);
    return vClosure(params, body, env);
  }

  private evalIf(elements: SExpr[], env: Environment): Value {
    // (if condition then else?)
    if (elements.length < 3) {
      throw new Error("if requires condition and then branch");
    }

    const condition = this.eval(elements[1], env);
    const isTruthy = this.isTruthy(condition);

    if (isTruthy) {
      return this.eval(elements[2], env);
    } else if (elements.length > 3) {
      return this.eval(elements[3], env);
    }
    return vNil;
  }

  private evalCond(elements: SExpr[], env: Environment): Value {
    // (cond (test expr)... (else expr)?)
    for (let i = 1; i < elements.length; i++) {
      const clause = elements[i];
      if (!isList(clause) || clause.elements.length < 2) {
        throw new Error("Invalid cond clause");
      }

      const [test, ...body] = clause.elements;

      // Check for else
      if (isSymbol(test) && test.name === "else") {
        let result: Value = vNil;
        for (const expr of body) {
          result = this.eval(expr, env);
        }
        return result;
      }

      const testResult = this.eval(test, env);
      if (this.isTruthy(testResult)) {
        let result: Value = vNil;
        for (const expr of body) {
          result = this.eval(expr, env);
        }
        return result;
      }
    }

    return vNil;
  }

  private evalQuote(elements: SExpr[]): Value {
    // (quote expr)
    if (elements.length !== 2) {
      throw new Error("quote requires exactly one argument");
    }
    return this.sexprToValue(elements[1]);
  }

  private evalQuasiquote(elements: SExpr[], env: Environment): Value {
    // (quasiquote expr)
    if (elements.length !== 2) {
      throw new Error("quasiquote requires exactly one argument");
    }
    return this.evalQuasiquoteExpr(elements[1], env, 1);
  }

  private evalQuasiquoteExpr(expr: SExpr, env: Environment, depth: number): Value {
    if (!isList(expr)) {
      return this.sexprToValue(expr);
    }

    const elements = expr.elements;
    if (elements.length === 0) {
      return vList();
    }

    const head = elements[0];
    if (isSymbol(head)) {
      if (head.name === "unquote" && depth === 1) {
        if (elements.length !== 2) {
          throw new Error("unquote requires exactly one argument");
        }
        return this.eval(elements[1], env);
      }

      if (head.name === "quasiquote") {
        return vList(
          this.sexprToValue(head),
          this.evalQuasiquoteExpr(elements[1], env, depth + 1)
        );
      }

      if (head.name === "unquote" && depth > 1) {
        return vList(
          this.sexprToValue(head),
          this.evalQuasiquoteExpr(elements[1], env, depth - 1)
        );
      }
    }

    // Process list, handling unquote-splicing
    const result: Value[] = [];
    for (const elem of elements) {
      if (isList(elem) && elem.elements.length === 2) {
        const elemHead = elem.elements[0];
        if (isSymbol(elemHead) && elemHead.name === "unquote-splicing" && depth === 1) {
          const spliced = this.eval(elem.elements[1], env);
          if (spliced.type === "list") {
            result.push(...spliced.elements);
          } else {
            throw new Error("unquote-splicing requires a list");
          }
          continue;
        }
      }
      result.push(this.evalQuasiquoteExpr(elem, env, depth));
    }

    return vList(...result);
  }

  private evalBegin(elements: SExpr[], env: Environment): Value {
    // (begin expr...)
    let result: Value = vNil;
    for (let i = 1; i < elements.length; i++) {
      result = this.eval(elements[i], env);
    }
    return result;
  }

  private evalSet(elements: SExpr[], env: Environment): Value {
    // (set! name value)
    if (elements.length !== 3) {
      throw new Error("set! requires name and value");
    }

    const nameExpr = elements[1];
    if (!isSymbol(nameExpr)) {
      throw new Error("set! target must be a symbol");
    }

    const value = this.eval(elements[2], env);
    env.set(nameExpr.name, value);
    return vNil;
  }

  private evalAnd(elements: SExpr[], env: Environment): Value {
    // (and expr...)
    let result: Value = vBool(true);
    for (let i = 1; i < elements.length; i++) {
      result = this.eval(elements[i], env);
      if (!this.isTruthy(result)) {
        return result;
      }
    }
    return result;
  }

  private evalOr(elements: SExpr[], env: Environment): Value {
    // (or expr...)
    let result: Value = vBool(false);
    for (let i = 1; i < elements.length; i++) {
      result = this.eval(elements[i], env);
      if (this.isTruthy(result)) {
        return result;
      }
    }
    return result;
  }

  private evalApplication(elements: SExpr[], env: Environment): Value {
    const fn = this.eval(elements[0], env);
    const args = elements.slice(1).map(e => this.eval(e, env));

    if (fn.type === "primitive") {
      return fn.fn(...args);
    }

    if (fn.type === "closure") {
      if (args.length !== fn.params.length) {
        throw new Error(`Expected ${fn.params.length} arguments, got ${args.length}`);
      }

      const callEnv = fn.env.extend();
      for (let i = 0; i < fn.params.length; i++) {
        callEnv.define(fn.params[i], args[i]);
      }

      let result: Value = vNil;
      for (const expr of fn.body) {
        result = this.eval(expr, callEnv);
      }
      return result;
    }

    throw new Error(`Cannot call: ${valueToString(fn)}`);
  }

  private isTruthy(value: Value): boolean {
    if (value.type === "boolean") return value.value;
    if (value.type === "nil") return false;
    return true;
  }

  /**
   * Convert S-expression to runtime value (for quote)
   */
  private sexprToValue(expr: SExpr): Value {
    switch (expr.type) {
      case "number": return vNum(expr.value);
      case "string": return vStr(expr.value);
      case "boolean": return vBool(expr.value);
      case "nil": return vNil;
      case "symbol": return vList(vStr("symbol"), vStr(expr.name));
      case "list": return vList(...expr.elements.map(e => this.sexprToValue(e)));
    }
  }
}

/**
 * Convert value to string for display
 */
export function valueToString(value: Value): string {
  switch (value.type) {
    case "number": return String(value.value);
    case "string": return `"${value.value}"`;
    case "boolean": return value.value ? "#t" : "#f";
    case "nil": return "nil";
    case "list": return `(${value.elements.map(valueToString).join(" ")})`;
    case "closure": return `#<closure (${value.params.join(" ")})>`;
    case "primitive": return `#<primitive ${value.name}>`;
    case "syntax": return `#<syntax>`;
  }
}

/**
 * Convert value back to S-expression (for macros)
 */
export function valueToSexpr(value: Value): SExpr {
  switch (value.type) {
    case "number": return num(value.value);
    case "string": return str(value.value);
    case "boolean": return bool(value.value);
    case "nil": return nil;
    case "list": return list(...value.elements.map(valueToSexpr));
    case "syntax": return value.datum;
    default:
      throw new Error(`Cannot convert ${value.type} to S-expression`);
  }
}
