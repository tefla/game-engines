import type {
  AssignExpr,
  BinaryExpr,
  CallExpr,
  Expr,
  IdentExpr,
  IfExpr,
  ListExpr,
  MatchExpr,
  MemberExpr,
  Pattern,
  Program,
  RecordExpr,
  Stmt,
  UnaryExpr,
} from "./ast";

export type Value =
  | null
  | boolean
  | number
  | string
  | Value[]
  | RecordValue
  | FnValue
  | BuiltinFnValue;

export type RecordValue = {
  readonly kind: "record";
  readonly entries: Record<string, Value>;
};

export type FnValue = {
  readonly kind: "fn";
  readonly params: string[];
  readonly body: Stmt[];
  readonly env: Environment;
};

export type BuiltinFnValue = {
  readonly kind: "builtin";
  readonly name: string;
  readonly call: (args: Value[], interpreter: Interpreter) => Value | Promise<Value>;
};

export type SignalHandler = (data: Value) => void | Promise<void>;

export interface SignalBus {
  on(signal: string, handler: SignalHandler): () => void;
  emit(signal: string, data?: Value): void | Promise<void>;
}

type Binding = { value: Value; mutable: boolean };

export class Environment {
  private readonly bindings = new Map<string, Binding>();

  constructor(readonly parent?: Environment) {}

  forceSet(name: string, value: Value): void {
    const binding = this.bindings.get(name);
    if (binding) {
      binding.value = value;
      return;
    }
    this.bindings.set(name, { value, mutable: false });
  }

  delete(name: string): boolean {
    return this.bindings.delete(name);
  }

  defineLet(name: string, value: Value): void {
    if (this.bindings.has(name)) throw new Error(`Already defined: ${name}`);
    this.bindings.set(name, { value, mutable: false });
  }

  defineVar(name: string, value: Value): void {
    if (this.bindings.has(name)) throw new Error(`Already defined: ${name}`);
    this.bindings.set(name, { value, mutable: true });
  }

  assign(name: string, value: Value): void {
    const binding = this.resolveBinding(name);
    if (!binding.mutable) throw new Error(`Cannot assign to immutable binding: ${name}`);
    binding.value = value;
  }

  get(name: string): Value {
    const found = this.tryGet(name);
    if (found.found) return found.value;
    throw new Error(`Undefined identifier: ${name}`);
  }

  tryGet(name: string): { found: true; value: Value } | { found: false } {
    const own = this.bindings.get(name);
    if (own) return { found: true, value: own.value };
    if (this.parent) return this.parent.tryGet(name);
    return { found: false };
  }

  private resolveBinding(name: string): Binding {
    const own = this.bindings.get(name);
    if (own) return own;
    if (!this.parent) throw new Error(`Undefined identifier: ${name}`);
    return this.parent.resolveBinding(name);
  }
}

function record(entries: Record<string, Value> = Object.create(null)): RecordValue {
  return { kind: "record", entries };
}

function isRecord(value: Value): value is RecordValue {
  return typeof value === "object" && value !== null && !Array.isArray(value) && (value as any).kind === "record";
}

function isTruthy(value: Value): boolean {
  return !(value === false || value === null);
}

function deepEqual(a: Value, b: Value): boolean {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!deepEqual(a[i]!, b[i]!)) return false;
    return true;
  }
  if (isRecord(a) && isRecord(b)) {
    const aKeys = Object.keys(a.entries).sort();
    const bKeys = Object.keys(b.entries).sort();
    if (!deepEqual(aKeys as any, bKeys as any)) return false;
    for (const key of aKeys) if (!deepEqual(a.entries[key]!, b.entries[key]!)) return false;
    return true;
  }
  return false;
}

function stableAstKey(value: unknown): string {
  return JSON.stringify(stableAstValue(value));
}

function stableAstValue(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(stableAstValue);
  if (value && typeof value === "object") {
    const obj = value as Record<string, unknown>;
    const out = Object.create(null) as Record<string, unknown>;
    const keys = Object.keys(obj)
      .filter((k) => k !== "loc")
      .sort();
    for (const key of keys) out[key] = stableAstValue(obj[key]);
    return out;
  }
  return value;
}

export class Interpreter {
  readonly globals: Environment;
  readonly signals?: SignalBus;
  private readonly modules = new Map<string, RecordValue>();
  private readonly loadScript?: (path: string) => Program | Promise<Program>;
  private readonly importedScripts = new Set<string>();
  private readonly fnHashesBySource = new Map<string, Map<string, string>>();
  private readonly onHashBySource = new Map<string, string>();
  private readonly entityHashBySource = new Map<string, string>();
  private readonly unsubscribersBySource = new Map<string, (() => void)[]>();
  private activeSourceKey: string | undefined;

  constructor(opts?: { globals?: Environment; signals?: SignalBus; loadScript?: (path: string) => Program | Promise<Program> }) {
    this.globals = opts?.globals ?? new Environment();
    this.signals = opts?.signals;
    this.loadScript = opts?.loadScript;
  }

  async run(program: Program, env: Environment = this.globals): Promise<Value> {
    let last: Value = null;
    for (const stmt of program.body) last = await this.evalStmt(stmt, env);
    return last;
  }

  async runWithSource(program: Program, env: Environment, sourceKey: string): Promise<Value> {
    const prev = this.activeSourceKey;
    this.activeSourceKey = sourceKey;
    try {
      return await this.run(program, env);
    } finally {
      this.activeSourceKey = prev;
    }
  }

  async hotReload(program: Program, env: Environment, sourceKey: string): Promise<void> {
    const prevFnHashes = this.fnHashesBySource.get(sourceKey) ?? new Map<string, string>();
    const nextFnHashes = new Map<string, string>();
    const nextFnStmts = new Map<string, Extract<Stmt, { type: "Fn" | "Extend" }>>();

    const onStmts: Extract<Stmt, { type: "On" }>[] = [];
    const entityCalls: CallExpr[] = [];

    for (const stmt of program.body) {
      if (stmt.type === "Fn" || stmt.type === "Extend") {
        nextFnHashes.set(stmt.name, stableAstKey(stmt));
        nextFnStmts.set(stmt.name, stmt);
        continue;
      }
      if (stmt.type === "On") {
        onStmts.push(stmt);
        continue;
      }
      if (stmt.type === "Expr" && stmt.expr.type === "Call") {
        const callee = stmt.expr.callee;
        if (callee.type === "Ident" && callee.name === "entity") entityCalls.push(stmt.expr);
      }
    }

    for (const oldName of prevFnHashes.keys()) {
      if (!nextFnHashes.has(oldName)) env.delete(oldName);
    }
    for (const [name, hash] of nextFnHashes) {
      if (prevFnHashes.get(name) === hash) continue;
      const stmt = nextFnStmts.get(name)!;
      const fn: FnValue = { kind: "fn", params: stmt.params, body: stmt.body, env };
      env.forceSet(name, fn);
    }
    this.fnHashesBySource.set(sourceKey, nextFnHashes);

    const nextEntityHash = stableAstKey(entityCalls);
    const prevEntityHash = this.entityHashBySource.get(sourceKey);
    const entitiesChanged = prevEntityHash !== nextEntityHash;
    this.entityHashBySource.set(sourceKey, nextEntityHash);

    if (entitiesChanged) {
      for (const call of entityCalls) {
        await this.evalExpr(call, env);
      }
    }

    const nextOnHash = stableAstKey(onStmts);
    const prevOnHash = this.onHashBySource.get(sourceKey);
    const handlersChanged = prevOnHash !== nextOnHash;
    this.onHashBySource.set(sourceKey, nextOnHash);

    if (!handlersChanged) return;

    const oldUnsubs = this.unsubscribersBySource.get(sourceKey) ?? [];
    const nextUnsubs: (() => void)[] = [];
    this.unsubscribersBySource.set(sourceKey, nextUnsubs);

    const prev = this.activeSourceKey;
    this.activeSourceKey = sourceKey;
    try {
      for (const stmt of onStmts) {
        await this.evalStmt(stmt, env);
      }
    } catch (err) {
      for (const unsub of nextUnsubs) unsub();
      if (oldUnsubs.length === 0) this.unsubscribersBySource.delete(sourceKey);
      else this.unsubscribersBySource.set(sourceKey, oldUnsubs);
      throw err;
    } finally {
      this.activeSourceKey = prev;
    }

    for (const unsub of oldUnsubs) unsub();
  }

  defineBuiltin(name: string, call: BuiltinFnValue["call"]): void {
    this.globals.defineLet(name, { kind: "builtin", name, call });
  }

  registerModule(name: string, module: RecordValue): void {
    this.modules.set(name, module);
  }

  async evalStmt(stmt: Stmt, env: Environment): Promise<Value> {
    switch (stmt.type) {
      case "Let": {
        const value = await this.evalExpr(stmt.expr, env);
        env.defineLet(stmt.name, value);
        return value;
      }
      case "Var": {
        const value = await this.evalExpr(stmt.expr, env);
        env.defineVar(stmt.name, value);
        return value;
      }
      case "Fn": {
        const fn: FnValue = { kind: "fn", params: stmt.params, body: stmt.body, env };
        env.defineLet(stmt.name, fn);
        return null;
      }
      case "Extend": {
        const fn: FnValue = { kind: "fn", params: stmt.params, body: stmt.body, env };
        env.defineLet(stmt.name, fn);
        return null;
      }
      case "Import": {
        if (stmt.kind === "module") {
          const mod = this.modules.get(stmt.name);
          if (!mod) throw new Error(`Unknown module: ${stmt.name}`);
          env.defineLet(stmt.name, mod);
          return mod;
        }

        if (!this.loadScript) throw new Error("Script loader is not configured");
        const path = stmt.name;
        if (!path.startsWith("/")) throw new Error(`Only absolute VFS paths are supported: ${path}`);
        if (this.importedScripts.has(path)) return null;
        this.importedScripts.add(path);
        try {
          const program = await this.loadScript(path);
          await this.runWithSource(program, env, path);
        } catch (err) {
          this.importedScripts.delete(path);
          throw err;
        }
        return null;
      }
      case "On": {
        if (!this.signals) throw new Error("Signals are not configured");
        const filterValues: Value[] = [];
        for (const filter of stmt.filters) filterValues.push(await this.evalExpr(filter, env));

        const unsub = this.signals.on(stmt.signal, async (data) => {
          if (filterValues.length === 1 && !deepEqual(filterValues[0]!, data ?? null)) return;
          if (filterValues.length > 1) return;

          const handlerEnv = new Environment(env);
          handlerEnv.defineLet("data", data ?? null);
          await this.evalBlock(stmt.body, handlerEnv);
        });
        if (this.activeSourceKey) {
          const list = this.unsubscribersBySource.get(this.activeSourceKey) ?? [];
          list.push(unsub);
          this.unsubscribersBySource.set(this.activeSourceKey, list);
        }
        return null;
      }
      case "Emit": {
        if (!this.signals) throw new Error("Signals are not configured");
        const data = stmt.data ? await this.evalExpr(stmt.data, env) : null;
        await this.signals.emit(stmt.signal, data);
        return null;
      }
      case "Expr": {
        const value = await this.evalExpr(stmt.expr, env);
        if (typeof value === "object" && value !== null) {
          if ((value as any).kind === "builtin") return (value as BuiltinFnValue).call([], this);
          if ((value as any).kind === "fn") return this.evalCall({ type: "Call", loc: stmt.loc, callee: stmt.expr, args: [] }, env);
        }
        return value;
      }
    }
  }

  async evalExpr(expr: Expr, env: Environment): Promise<Value> {
    switch (expr.type) {
      case "Literal":
        return expr.value;
      case "Ident": {
        const found = env.tryGet(expr.name);
        if (found.found) return found.value;
        const first = expr.name[0];
        if (first && first >= "A" && first <= "Z") return expr.name;
        throw new Error(`Undefined identifier: ${expr.name}`);
      }
      case "List":
        return this.evalList(expr, env);
      case "Record":
        return this.evalRecord(expr, env);
      case "Unary":
        return this.evalUnary(expr, env);
      case "Binary":
        return this.evalBinary(expr, env);
      case "If":
        return this.evalIf(expr, env);
      case "Match":
        return this.evalMatch(expr, env);
      case "Member":
        return this.evalMember(expr, env);
      case "Assign":
        return this.evalAssign(expr, env);
      case "Call":
        return this.evalCall(expr, env);
    }
  }

  private async evalBlock(stmts: Stmt[], env: Environment): Promise<Value> {
    let last: Value = null;
    for (const stmt of stmts) last = await this.evalStmt(stmt, env);
    return last;
  }

  private async evalList(expr: ListExpr, env: Environment): Promise<Value> {
    const items: Value[] = [];
    for (const item of expr.items) items.push(await this.evalExpr(item, env));
    return items;
  }

  private async evalRecord(expr: RecordExpr, env: Environment): Promise<Value> {
    const out = Object.create(null) as Record<string, Value>;
    for (const { key, value } of expr.entries) out[key] = await this.evalExpr(value, env);
    return record(out);
  }

  private async evalUnary(expr: UnaryExpr, env: Environment): Promise<Value> {
    const value = await this.evalExpr(expr.expr, env);
    switch (expr.op) {
      case "not":
        return !isTruthy(value);
      case "-":
        if (typeof value !== "number") throw new Error("Unary '-' expects a number");
        return -value;
    }
  }

  private async evalBinary(expr: BinaryExpr, env: Environment): Promise<Value> {
    if (expr.op === "and") {
      const left = await this.evalExpr(expr.left, env);
      if (!isTruthy(left)) return left;
      return this.evalExpr(expr.right, env);
    }
    if (expr.op === "or") {
      const left = await this.evalExpr(expr.left, env);
      if (isTruthy(left)) return left;
      return this.evalExpr(expr.right, env);
    }

    const left = await this.evalExpr(expr.left, env);
    const right = await this.evalExpr(expr.right, env);

    switch (expr.op) {
      case "+": {
        if (typeof left === "number" && typeof right === "number") return left + right;
        if (typeof left === "string" || typeof right === "string") return String(left) + String(right);
        if (Array.isArray(left) && Array.isArray(right)) return [...left, ...right];
        throw new Error("Unsupported '+' operands");
      }
      case "-":
      case "*":
      case "/": {
        if (typeof left !== "number" || typeof right !== "number") throw new Error(`'${expr.op}' expects numbers`);
        if (expr.op === "-") return left - right;
        if (expr.op === "*") return left * right;
        return left / right;
      }
      case "==":
        return deepEqual(left, right);
      case "!=":
        return !deepEqual(left, right);
      case ">":
      case ">=":
      case "<":
      case "<=": {
        if (typeof left !== "number" || typeof right !== "number") {
          throw new Error(`'${expr.op}' expects numbers`);
        }
        if (expr.op === ">") return left > right;
        if (expr.op === ">=") return left >= right;
        if (expr.op === "<") return left < right;
        return left <= right;
      }
    }
  }

  private async evalIf(expr: IfExpr, env: Environment): Promise<Value> {
    const cond = await this.evalExpr(expr.condition, env);
    if (isTruthy(cond)) return this.evalBlock(expr.thenBlock, new Environment(env));
    if (expr.elseBlock) return this.evalBlock(expr.elseBlock, new Environment(env));
    return null;
  }

  private async evalMatch(expr: MatchExpr, env: Environment): Promise<Value> {
    const value = await this.evalExpr(expr.expr, env);
    for (const { pattern, expr: caseExpr } of expr.cases) {
      const bindings = Object.create(null) as Record<string, Value>;
      if (!this.matchPattern(value, pattern, bindings)) continue;
      const caseEnv = new Environment(env);
      for (const [name, v] of Object.entries(bindings)) caseEnv.defineLet(name, v);
      return this.evalExpr(caseExpr, caseEnv);
    }
    return null;
  }

  private matchPattern(value: Value, pattern: Pattern, bindings: Record<string, Value>): boolean {
    switch (pattern.type) {
      case "Wildcard":
        return true;
      case "PLiteral":
        return deepEqual(value, pattern.value);
      case "PIdent":
        bindings[pattern.name] = value;
        return true;
      case "PRecord": {
        if (!isRecord(value)) return false;
        for (const field of pattern.fields) {
          const v = value.entries[field.key];
          if (v === undefined) return false;
          if (!this.matchPattern(v, field.pattern, bindings)) return false;
        }
        return true;
      }
    }
  }

  private async evalMember(expr: MemberExpr, env: Environment): Promise<Value> {
    const object = await this.evalExpr(expr.object, env);
    if (!isRecord(object)) throw new Error("Can only access properties on records");
    if (!(expr.property in object.entries)) throw new Error(`Unknown property: ${expr.property}`);
    return object.entries[expr.property]!;
  }

  private async evalAssign(expr: AssignExpr, env: Environment): Promise<Value> {
    const value = await this.evalExpr(expr.value, env);
    if (expr.target.type === "Ident") {
      env.assign(expr.target.name, value);
      return value;
    }

    const object = await this.evalExpr(expr.target.object, env);
    if (!isRecord(object)) throw new Error("Can only assign properties on records");
    object.entries[expr.target.property] = value;
    return value;
  }

  private async evalCall(expr: CallExpr, env: Environment): Promise<Value> {
    const callee = await this.evalExpr(expr.callee, env);
    const args: Value[] = [];
    for (const arg of expr.args) args.push(await this.evalExpr(arg, env));
    if (expr.block) args.push(await this.evalExpr(expr.block, env));

    if (typeof callee === "object" && callee !== null) {
      if ((callee as any).kind === "fn") {
        const fn = callee as FnValue;
        if (args.length !== fn.params.length) {
          throw new Error(`Expected ${fn.params.length} args, got ${args.length}`);
        }
        const callEnv = new Environment(fn.env);
        for (let i = 0; i < fn.params.length; i++) callEnv.defineLet(fn.params[i]!, args[i]!);
        return this.evalBlock(fn.body, callEnv);
      }
      if ((callee as any).kind === "builtin") {
        const fn = callee as BuiltinFnValue;
        return fn.call(args, this);
      }
    }

    throw new Error("Callee is not a function");
  }
}
