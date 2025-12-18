// Slate Interpreter - Tree-walking evaluator

import {
  type Program,
  type Stmt,
  type Expr,
  type Block,
  type Pattern,
  type SlateValue,
  Environment,
  RuntimeError,
  TokenType,
  Num,
  Str,
  Bool,
  Null,
  List,
  Record,
  Color,
  Signal,
  isTruthy,
  isNumber,
  isString,
  isBool,
  isList,
  isRecord,
  isCallable,
  stringify,
  type SlateFunction,
  type SlateNativeFunction,
  type SlateRecord,
  type SlateList,
} from "@oort/core";

export type SignalHandler = {
  signal: string[];
  filter?: SlateValue;
  handler: (data: SlateValue) => void;
};

export class Interpreter {
  private globalEnv: Environment;
  private currentEnv: Environment;
  private signalHandlers: SignalHandler[] = [];
  private output: string[] = [];

  constructor(globals?: Map<string, SlateValue>) {
    this.globalEnv = new Environment();
    this.currentEnv = this.globalEnv;

    // Register any provided globals
    if (globals) {
      for (const [name, value] of globals) {
        this.globalEnv.define(name, value);
      }
    }
  }

  run(program: Program): SlateValue {
    let result: SlateValue = Null();

    for (const stmt of program.statements) {
      result = this.executeStmt(stmt);
    }

    return result;
  }

  getOutput(): string[] {
    return this.output;
  }

  getSignalHandlers(): SignalHandler[] {
    return this.signalHandlers;
  }

  emit(signalPath: string[], data: SlateValue = Null()): void {
    const signalStr = signalPath.join(".");

    for (const handler of this.signalHandlers) {
      const handlerStr = handler.signal.join(".");
      if (handlerStr === signalStr) {
        // Check filter if present
        if (handler.filter) {
          if (!this.valuesEqual(handler.filter, data)) {
            continue;
          }
        }
        handler.handler(data);
      }
    }
  }

  // ============ Statements ============

  private executeStmt(stmt: Stmt): SlateValue {
    switch (stmt.type) {
      case "Let":
        return this.executeLet(stmt);
      case "Var":
        return this.executeVar(stmt);
      case "Fn":
        return this.executeFn(stmt);
      case "On":
        return this.executeOn(stmt);
      case "Emit":
        return this.executeEmit(stmt);
      case "ExprStmt":
        return this.evaluate(stmt.expression);
      case "Import":
        return this.executeImport(stmt);
      case "Loop":
        return this.executeLoop(stmt);
      case "For":
        return this.executeFor(stmt);
      case "Block":
        return this.executeBlock(stmt, this.currentEnv.child());
      case "TypeDef":
        // Types are compile-time only, no runtime effect
        return Null();
      default:
        throw new RuntimeError(`Unknown statement type: ${(stmt as any).type}`);
    }
  }

  private executeLet(stmt: { name: string; value: Expr }): SlateValue {
    const value = this.evaluate(stmt.value);
    this.currentEnv.define(stmt.name, value, false);
    return value;
  }

  private executeVar(stmt: { name: string; value: Expr }): SlateValue {
    const value = this.evaluate(stmt.value);
    this.currentEnv.define(stmt.name, value, true);
    return value;
  }

  private executeFn(stmt: {
    name: string;
    params: Array<{ name: string }>;
    body: Block;
  }): SlateValue {
    const fn: SlateFunction = {
      type: "function",
      name: stmt.name,
      params: stmt.params,
      body: stmt.body,
      closure: this.currentEnv,
    };
    this.currentEnv.define(stmt.name, fn);
    return fn;
  }

  private executeOn(stmt: {
    signal: { parts: string[] };
    filter?: Expr;
    body: Block;
  }): SlateValue {
    const filter = stmt.filter ? this.evaluate(stmt.filter) : undefined;

    this.signalHandlers.push({
      signal: stmt.signal.parts,
      filter,
      handler: (data) => {
        const env = this.currentEnv.child();
        env.define("data", data);
        this.executeBlock(stmt.body, env);
      },
    });

    return Null();
  }

  private executeEmit(stmt: {
    signal: { parts: string[] };
    data?: Expr;
  }): SlateValue {
    const data = stmt.data ? this.evaluate(stmt.data) : Null();
    this.emit(stmt.signal.parts, data);
    return Null();
  }

  private executeImport(stmt: { path: string }): SlateValue {
    // Import handling will be implemented with VFS
    // For now, just return null
    return Null();
  }

  private executeLoop(stmt: { body: Block }): SlateValue {
    let result: SlateValue = Null();
    let iterations = 0;
    const maxIterations = 10000; // Safety limit

    while (iterations < maxIterations) {
      result = this.executeBlock(stmt.body, this.currentEnv.child());
      iterations++;
    }

    if (iterations >= maxIterations) {
      throw new RuntimeError("Loop exceeded maximum iterations");
    }

    return result;
  }

  private executeFor(stmt: {
    variable: string;
    iterable: Expr;
    body: Block;
  }): SlateValue {
    const iterable = this.evaluate(stmt.iterable);
    let result: SlateValue = Null();

    if (isList(iterable)) {
      for (const element of iterable.elements) {
        const env = this.currentEnv.child();
        env.define(stmt.variable, element);
        result = this.executeBlock(stmt.body, env);
      }
    } else if (iterable.type === "number") {
      // Range from 0 to n
      for (let i = 0; i < iterable.value; i++) {
        const env = this.currentEnv.child();
        env.define(stmt.variable, Num(i));
        result = this.executeBlock(stmt.body, env);
      }
    } else {
      throw new RuntimeError(
        `Cannot iterate over ${iterable.type}`,
        stmt.body.line,
        stmt.body.column
      );
    }

    return result;
  }

  executeBlock(block: Block, env: Environment): SlateValue {
    const previousEnv = this.currentEnv;
    this.currentEnv = env;

    let result: SlateValue = Null();
    try {
      for (const stmt of block.statements) {
        result = this.executeStmt(stmt);
      }
    } finally {
      this.currentEnv = previousEnv;
    }

    return result;
  }

  // ============ Expressions ============

  evaluate(expr: Expr): SlateValue {
    switch (expr.type) {
      case "Literal":
        return this.evaluateLiteral(expr);
      case "Identifier":
        return this.currentEnv.get(expr.name);
      case "Binary":
        return this.evaluateBinary(expr);
      case "Unary":
        return this.evaluateUnary(expr);
      case "Logical":
        return this.evaluateLogical(expr);
      case "Call":
        return this.evaluateCall(expr);
      case "Member":
        return this.evaluateMember(expr);
      case "Index":
        return this.evaluateIndex(expr);
      case "Record":
        return this.evaluateRecord(expr);
      case "List":
        return this.evaluateList(expr);
      case "If":
        return this.evaluateIf(expr);
      case "Match":
        return this.evaluateMatch(expr);
      case "Assign":
        return this.evaluateAssign(expr);
      case "Color":
        return Color(expr.hex);
      case "With":
        return this.evaluateWith(expr);
      default:
        throw new RuntimeError(`Unknown expression type: ${(expr as any).type}`);
    }
  }

  private evaluateLiteral(expr: {
    value: number | string | boolean | null;
  }): SlateValue {
    if (expr.value === null) return Null();
    if (typeof expr.value === "number") return Num(expr.value);
    if (typeof expr.value === "string") return Str(expr.value);
    if (typeof expr.value === "boolean") return Bool(expr.value);
    return Null();
  }

  private evaluateBinary(expr: {
    left: Expr;
    operator: { type: TokenType };
    right: Expr;
  }): SlateValue {
    const left = this.evaluate(expr.left);
    const right = this.evaluate(expr.right);

    switch (expr.operator.type) {
      case TokenType.PLUS:
        if (isNumber(left) && isNumber(right)) {
          return Num(left.value + right.value);
        }
        if (isString(left) || isString(right)) {
          return Str(stringify(left) + stringify(right));
        }
        if (isList(left) && isList(right)) {
          return List([...left.elements, ...right.elements]);
        }
        throw new RuntimeError("Cannot add these types");

      case TokenType.MINUS:
        this.checkNumberOperands(left, right);
        return Num((left as any).value - (right as any).value);

      case TokenType.STAR:
        this.checkNumberOperands(left, right);
        return Num((left as any).value * (right as any).value);

      case TokenType.SLASH:
        this.checkNumberOperands(left, right);
        if ((right as any).value === 0) {
          throw new RuntimeError("Division by zero");
        }
        return Num((left as any).value / (right as any).value);

      case TokenType.PERCENT:
        this.checkNumberOperands(left, right);
        return Num((left as any).value % (right as any).value);

      case TokenType.LESS:
        this.checkNumberOperands(left, right);
        return Bool((left as any).value < (right as any).value);

      case TokenType.LESS_EQUAL:
        this.checkNumberOperands(left, right);
        return Bool((left as any).value <= (right as any).value);

      case TokenType.GREATER:
        this.checkNumberOperands(left, right);
        return Bool((left as any).value > (right as any).value);

      case TokenType.GREATER_EQUAL:
        this.checkNumberOperands(left, right);
        return Bool((left as any).value >= (right as any).value);

      case TokenType.EQUAL_EQUAL:
        return Bool(this.valuesEqual(left, right));

      case TokenType.BANG_EQUAL:
        return Bool(!this.valuesEqual(left, right));

      default:
        throw new RuntimeError(`Unknown binary operator: ${expr.operator.type}`);
    }
  }

  private evaluateUnary(expr: {
    operator: { type: TokenType };
    operand: Expr;
  }): SlateValue {
    const operand = this.evaluate(expr.operand);

    switch (expr.operator.type) {
      case TokenType.MINUS:
        if (!isNumber(operand)) {
          throw new RuntimeError("Operand must be a number");
        }
        return Num(-operand.value);

      case TokenType.NOT:
        return Bool(!isTruthy(operand));

      default:
        throw new RuntimeError(`Unknown unary operator: ${expr.operator.type}`);
    }
  }

  private evaluateLogical(expr: {
    left: Expr;
    operator: { type: TokenType };
    right: Expr;
  }): SlateValue {
    const left = this.evaluate(expr.left);

    if (expr.operator.type === TokenType.OR) {
      if (isTruthy(left)) return left;
    } else {
      // AND
      if (!isTruthy(left)) return left;
    }

    return this.evaluate(expr.right);
  }

  private evaluateCall(expr: { callee: Expr; args: Expr[] }): SlateValue {
    const callee = this.evaluate(expr.callee);
    const args = expr.args.map((arg) => this.evaluate(arg));

    if (!isCallable(callee)) {
      throw new RuntimeError(
        `Cannot call ${callee.type}`,
        expr.callee.line,
        expr.callee.column
      );
    }

    if (callee.type === "native") {
      if (callee.arity !== "variadic" && args.length !== callee.arity) {
        throw new RuntimeError(
          `Expected ${callee.arity} arguments but got ${args.length}`
        );
      }
      return callee.fn(args);
    }

    // User-defined function
    if (args.length !== callee.params.length) {
      throw new RuntimeError(
        `Expected ${callee.params.length} arguments but got ${args.length}`
      );
    }

    const env = callee.closure.child();
    for (let i = 0; i < callee.params.length; i++) {
      env.define(callee.params[i].name, args[i]);
    }

    return this.executeBlock(callee.body, env);
  }

  private evaluateMember(expr: { object: Expr; property: string }): SlateValue {
    const object = this.evaluate(expr.object);

    if (isRecord(object)) {
      const value = object.fields.get(expr.property);
      if (value === undefined) {
        throw new RuntimeError(`Property '${expr.property}' does not exist`);
      }
      return value;
    }

    throw new RuntimeError(`Cannot access property on ${object.type}`);
  }

  private evaluateIndex(expr: { object: Expr; index: Expr }): SlateValue {
    const object = this.evaluate(expr.object);
    const index = this.evaluate(expr.index);

    if (isList(object) && isNumber(index)) {
      const i = Math.floor(index.value);
      if (i < 0 || i >= object.elements.length) {
        throw new RuntimeError(`Index ${i} out of bounds`);
      }
      return object.elements[i];
    }

    if (isRecord(object) && isString(index)) {
      const value = object.fields.get(index.value);
      if (value === undefined) {
        throw new RuntimeError(`Property '${index.value}' does not exist`);
      }
      return value;
    }

    throw new RuntimeError(`Cannot index ${object.type} with ${index.type}`);
  }

  private evaluateRecord(expr: {
    fields: Array<{ key: string; value: Expr }>;
  }): SlateValue {
    const fields = new Map<string, SlateValue>();
    for (const field of expr.fields) {
      fields.set(field.key, this.evaluate(field.value));
    }
    return Record(fields);
  }

  private evaluateList(expr: { elements: Expr[] }): SlateValue {
    return List(expr.elements.map((e) => this.evaluate(e)));
  }

  private evaluateIf(expr: {
    condition: Expr;
    thenBranch: Block;
    elseBranch?: Block | any;
  }): SlateValue {
    const condition = this.evaluate(expr.condition);

    if (isTruthy(condition)) {
      return this.executeBlock(expr.thenBranch, this.currentEnv.child());
    } else if (expr.elseBranch) {
      if (expr.elseBranch.type === "Block") {
        return this.executeBlock(expr.elseBranch, this.currentEnv.child());
      } else {
        // else if
        return this.evaluateIf(expr.elseBranch);
      }
    }

    return Null();
  }

  private evaluateMatch(expr: {
    subject: Expr;
    arms: Array<{ pattern: Pattern; body: Expr }>;
  }): SlateValue {
    const subject = this.evaluate(expr.subject);

    for (const arm of expr.arms) {
      const bindings = this.matchPattern(arm.pattern, subject);
      if (bindings !== null) {
        const env = this.currentEnv.child();
        for (const [name, value] of bindings) {
          env.define(name, value);
        }
        const previousEnv = this.currentEnv;
        this.currentEnv = env;
        try {
          return this.evaluate(arm.body);
        } finally {
          this.currentEnv = previousEnv;
        }
      }
    }

    throw new RuntimeError("No matching pattern");
  }

  private evaluateAssign(expr: {
    target: { type: string; name?: string; object?: Expr; property?: string; index?: Expr };
    value: Expr;
  }): SlateValue {
    const value = this.evaluate(expr.value);

    if (expr.target.type === "Identifier") {
      this.currentEnv.assign(expr.target.name!, value);
      return value;
    }

    if (expr.target.type === "Member") {
      const object = this.evaluate(expr.target.object!);
      if (isRecord(object)) {
        object.fields.set(expr.target.property!, value);
        return value;
      }
      throw new RuntimeError("Cannot assign to property on non-record");
    }

    if (expr.target.type === "Index") {
      const object = this.evaluate(expr.target.object!);
      const index = this.evaluate(expr.target.index!);

      if (isList(object) && isNumber(index)) {
        const i = Math.floor(index.value);
        if (i < 0 || i >= object.elements.length) {
          throw new RuntimeError(`Index ${i} out of bounds`);
        }
        object.elements[i] = value;
        return value;
      }

      if (isRecord(object) && isString(index)) {
        object.fields.set(index.value, value);
        return value;
      }

      throw new RuntimeError(`Cannot index ${object.type}`);
    }

    throw new RuntimeError("Invalid assignment target");
  }

  private evaluateWith(expr: {
    base: Expr;
    updates: { fields: Array<{ key: string; value: Expr }> };
  }): SlateValue {
    const base = this.evaluate(expr.base);
    if (!isRecord(base)) {
      throw new RuntimeError("'with' can only be used with records");
    }

    const newFields = new Map(base.fields);
    for (const field of expr.updates.fields) {
      newFields.set(field.key, this.evaluate(field.value));
    }

    return Record(newFields);
  }

  // ============ Pattern Matching ============

  private matchPattern(
    pattern: Pattern,
    value: SlateValue
  ): Map<string, SlateValue> | null {
    switch (pattern.type) {
      case "WildcardPattern":
        return new Map();

      case "IdentifierPattern":
        return new Map([[pattern.name, value]]);

      case "LiteralPattern":
        if (isNumber(value) && typeof pattern.value === "number") {
          return value.value === pattern.value ? new Map() : null;
        }
        if (isString(value) && typeof pattern.value === "string") {
          return value.value === pattern.value ? new Map() : null;
        }
        if (isBool(value) && typeof pattern.value === "boolean") {
          return value.value === pattern.value ? new Map() : null;
        }
        return null;

      case "RecordPattern":
        if (!isRecord(value)) return null;
        const recordBindings = new Map<string, SlateValue>();

        for (const field of pattern.fields) {
          const fieldValue = value.fields.get(field.key);
          if (fieldValue === undefined) return null;

          if (field.pattern) {
            const subBindings = this.matchPattern(field.pattern, fieldValue);
            if (subBindings === null) return null;
            for (const [k, v] of subBindings) {
              recordBindings.set(k, v);
            }
          } else {
            // Shorthand: {key} binds key to its value
            recordBindings.set(field.key, fieldValue);
          }
        }

        return recordBindings;

      case "ListPattern":
        if (!isList(value)) return null;
        const listBindings = new Map<string, SlateValue>();

        if (pattern.rest) {
          // Pattern with rest: [a, b, ..rest]
          if (value.elements.length < pattern.elements.length) return null;

          for (let i = 0; i < pattern.elements.length; i++) {
            const subBindings = this.matchPattern(
              pattern.elements[i],
              value.elements[i]
            );
            if (subBindings === null) return null;
            for (const [k, v] of subBindings) {
              listBindings.set(k, v);
            }
          }

          listBindings.set(
            pattern.rest,
            List(value.elements.slice(pattern.elements.length))
          );
        } else {
          // Exact length match
          if (value.elements.length !== pattern.elements.length) return null;

          for (let i = 0; i < pattern.elements.length; i++) {
            const subBindings = this.matchPattern(
              pattern.elements[i],
              value.elements[i]
            );
            if (subBindings === null) return null;
            for (const [k, v] of subBindings) {
              listBindings.set(k, v);
            }
          }
        }

        return listBindings;

      default:
        return null;
    }
  }

  // ============ Helpers ============

  private checkNumberOperands(left: SlateValue, right: SlateValue): void {
    if (!isNumber(left) || !isNumber(right)) {
      throw new RuntimeError("Operands must be numbers");
    }
  }

  private valuesEqual(a: SlateValue, b: SlateValue): boolean {
    if (a.type !== b.type) return false;

    switch (a.type) {
      case "number":
        return a.value === (b as any).value;
      case "string":
        return a.value === (b as any).value;
      case "bool":
        return a.value === (b as any).value;
      case "null":
        return true;
      case "list": {
        const bList = b as SlateList;
        if (a.elements.length !== bList.elements.length) return false;
        return a.elements.every((e, i) => this.valuesEqual(e, bList.elements[i]));
      }
      case "record": {
        const bRecord = b as SlateRecord;
        if (a.fields.size !== bRecord.fields.size) return false;
        for (const [key, value] of a.fields) {
          const bValue = bRecord.fields.get(key);
          if (bValue === undefined || !this.valuesEqual(value, bValue)) {
            return false;
          }
        }
        return true;
      }
      case "function":
      case "native":
        return a === b;
      case "color":
        return a.hex === (b as any).hex;
      case "signal":
        return a.parts.join(".") === (b as any).parts.join(".");
      default:
        return false;
    }
  }

  // Public method to add output
  addOutput(message: string): void {
    this.output.push(message);
  }

  // Get current environment (for testing/debugging)
  getEnvironment(): Environment {
    return this.currentEnv;
  }
}
