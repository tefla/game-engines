// Environment for variable scope management

import type { SlateValue, SlateRecord } from "./values";
import { Record } from "./values";

export class Environment {
  private values: Map<string, SlateValue> = new Map();
  private mutables: Set<string> = new Set();
  readonly parent: Environment | null;

  constructor(parent: Environment | null = null) {
    this.parent = parent;
  }

  define(name: string, value: SlateValue, mutable: boolean = false): void {
    this.values.set(name, value);
    if (mutable) {
      this.mutables.add(name);
    }
  }

  get(name: string): SlateValue {
    if (this.values.has(name)) {
      return this.values.get(name)!;
    }
    if (this.parent) {
      return this.parent.get(name);
    }
    throw new RuntimeError(`Undefined variable: ${name}`);
  }

  has(name: string): boolean {
    if (this.values.has(name)) {
      return true;
    }
    if (this.parent) {
      return this.parent.has(name);
    }
    return false;
  }

  assign(name: string, value: SlateValue): void {
    if (this.values.has(name)) {
      if (!this.mutables.has(name)) {
        throw new RuntimeError(`Cannot assign to immutable variable: ${name}`);
      }
      this.values.set(name, value);
      return;
    }
    if (this.parent) {
      this.parent.assign(name, value);
      return;
    }
    throw new RuntimeError(`Undefined variable: ${name}`);
  }

  isMutable(name: string): boolean {
    if (this.values.has(name)) {
      return this.mutables.has(name);
    }
    if (this.parent) {
      return this.parent.isMutable(name);
    }
    return false;
  }

  // Create a child environment
  child(): Environment {
    return new Environment(this);
  }

  // Get all variable names (for debugging)
  allNames(): string[] {
    const names = Array.from(this.values.keys());
    if (this.parent) {
      return [...names, ...this.parent.allNames()];
    }
    return names;
  }

  // Get all bindings in this environment as a record (for module exports)
  getAllBindings(): SlateRecord {
    return Record(new Map(this.values));
  }
}

export class RuntimeError extends Error {
  constructor(
    message: string,
    public line?: number,
    public column?: number
  ) {
    super(message);
    this.name = "RuntimeError";
  }
}

export class ParseError extends Error {
  constructor(
    message: string,
    public line: number,
    public column: number
  ) {
    super(`[${line}:${column}] ${message}`);
    this.name = "ParseError";
  }
}
