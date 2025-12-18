// AST Diffing - Semantic comparison of AST trees

import type {
  Program,
  Stmt,
  Expr,
  ASTNode,
  FnStmt,
  LetStmt,
  VarStmt,
  OnStmt,
  Block,
} from "@oort/core";

// Types of changes that can occur
export type ChangeType =
  | "added"
  | "removed"
  | "modified"
  | "moved"
  | "unchanged";

// A single diff entry
export interface DiffEntry {
  type: ChangeType;
  path: string; // Path within the AST (e.g., "fn:add/body/stmt:0")
  description: string; // Human-readable description
  nodeBefore?: ASTNode;
  nodeAfter?: ASTNode;
  line?: number;
  column?: number;
}

// Result of comparing two programs
export interface DiffResult {
  entries: DiffEntry[];
  summary: {
    added: number;
    removed: number;
    modified: number;
    unchanged: number;
  };
}

// Compare two programs and produce a diff
export function diffPrograms(
  before: Program,
  after: Program,
  filePath?: string
): DiffResult {
  const entries: DiffEntry[] = [];
  const prefix = filePath ? `${filePath}:` : "";

  // Build maps of named statements for comparison
  const beforeMap = buildStatementMap(before.statements);
  const afterMap = buildStatementMap(after.statements);

  // Find added, removed, and modified statements
  const beforeNames = new Set(beforeMap.keys());
  const afterNames = new Set(afterMap.keys());

  // Check for removed statements
  for (const name of beforeNames) {
    if (!afterNames.has(name)) {
      const stmt = beforeMap.get(name)!;
      entries.push({
        type: "removed",
        path: `${prefix}${name}`,
        description: `Removed: ${describeStatement(stmt)}`,
        nodeBefore: stmt,
        line: stmt.line,
        column: stmt.column,
      });
    }
  }

  // Check for added statements
  for (const name of afterNames) {
    if (!beforeNames.has(name)) {
      const stmt = afterMap.get(name)!;
      entries.push({
        type: "added",
        path: `${prefix}${name}`,
        description: `Added: ${describeStatement(stmt)}`,
        nodeAfter: stmt,
        line: stmt.line,
        column: stmt.column,
      });
    }
  }

  // Check for modified statements
  for (const name of beforeNames) {
    if (afterNames.has(name)) {
      const beforeStmt = beforeMap.get(name)!;
      const afterStmt = afterMap.get(name)!;

      const changes = diffStatements(beforeStmt, afterStmt, `${prefix}${name}`);
      entries.push(...changes);
    }
  }

  // Calculate summary
  const summary = {
    added: entries.filter((e) => e.type === "added").length,
    removed: entries.filter((e) => e.type === "removed").length,
    modified: entries.filter((e) => e.type === "modified").length,
    unchanged: 0,
  };

  // Count unchanged (statements present in both with no changes)
  for (const name of beforeNames) {
    if (afterNames.has(name)) {
      const relatedChanges = entries.filter((e) => e.path.startsWith(`${prefix}${name}`));
      if (relatedChanges.length === 0) {
        summary.unchanged++;
      }
    }
  }

  return { entries, summary };
}

// Build a map of statement name -> statement for named statements
function buildStatementMap(statements: Stmt[]): Map<string, Stmt> {
  const map = new Map<string, Stmt>();
  let anonIndex = 0;

  for (const stmt of statements) {
    const name = getStatementName(stmt) ?? `anon_${anonIndex++}`;
    map.set(name, stmt);
  }

  return map;
}

// Get a unique name for a statement (if it has one)
function getStatementName(stmt: Stmt): string | null {
  switch (stmt.type) {
    case "Fn":
      return `fn:${(stmt as FnStmt).name}`;
    case "Let":
      return `let:${(stmt as LetStmt).name}`;
    case "Var":
      return `var:${(stmt as VarStmt).name}`;
    case "On":
      const onStmt = stmt as OnStmt;
      return `on:${onStmt.signal.parts.join(".")}`;
    case "TypeDef":
      return `type:${(stmt as any).name}`;
    default:
      return null;
  }
}

// Get a human-readable description of a statement
function describeStatement(stmt: Stmt): string {
  switch (stmt.type) {
    case "Fn":
      const fn = stmt as FnStmt;
      const params = fn.params.map((p) => p.name).join(", ");
      return `function ${fn.name}(${params})`;
    case "Let":
      return `let ${(stmt as LetStmt).name}`;
    case "Var":
      return `var ${(stmt as VarStmt).name}`;
    case "On":
      const on = stmt as OnStmt;
      return `on ${on.signal.parts.join(".")}`;
    case "TypeDef":
      return `type ${(stmt as any).name}`;
    case "ExprStmt":
      return `expression`;
    default:
      return stmt.type.toLowerCase();
  }
}

// Compare two statements and return changes
function diffStatements(
  before: Stmt,
  after: Stmt,
  path: string
): DiffEntry[] {
  const entries: DiffEntry[] = [];

  // If types don't match, it's a replacement
  if (before.type !== after.type) {
    entries.push({
      type: "modified",
      path,
      description: `Changed from ${before.type} to ${after.type}`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
    return entries;
  }

  // Type-specific comparison
  switch (before.type) {
    case "Fn":
      entries.push(...diffFunctions(before as FnStmt, after as FnStmt, path));
      break;
    case "Let":
    case "Var":
      entries.push(...diffBindings(before as LetStmt, after as LetStmt, path));
      break;
    case "On":
      entries.push(...diffOnHandlers(before as OnStmt, after as OnStmt, path));
      break;
    default:
      // For other types, do a simple structural comparison
      if (!deepEqual(before, after)) {
        entries.push({
          type: "modified",
          path,
          description: `Modified ${describeStatement(after)}`,
          nodeBefore: before,
          nodeAfter: after,
          line: after.line,
          column: after.column,
        });
      }
  }

  return entries;
}

// Compare two function definitions
function diffFunctions(
  before: FnStmt,
  after: FnStmt,
  path: string
): DiffEntry[] {
  const entries: DiffEntry[] = [];

  // Check parameter changes
  const beforeParams = before.params.map((p) => p.name).join(", ");
  const afterParams = after.params.map((p) => p.name).join(", ");

  if (beforeParams !== afterParams) {
    entries.push({
      type: "modified",
      path: `${path}/params`,
      description: `Parameters changed: (${beforeParams}) -> (${afterParams})`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
  }

  // Check return type changes
  const beforeRet = before.returnType ? stringifyType(before.returnType) : null;
  const afterRet = after.returnType ? stringifyType(after.returnType) : null;

  if (beforeRet !== afterRet) {
    entries.push({
      type: "modified",
      path: `${path}/returnType`,
      description: `Return type changed: ${beforeRet ?? "untyped"} -> ${afterRet ?? "untyped"}`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
  }

  // Check body changes
  const bodyChanges = diffBlocks(before.body, after.body, `${path}/body`);
  entries.push(...bodyChanges);

  return entries;
}

// Compare two let/var bindings
function diffBindings(
  before: LetStmt | VarStmt,
  after: LetStmt | VarStmt,
  path: string
): DiffEntry[] {
  const entries: DiffEntry[] = [];

  // Check value expression changes
  if (!deepEqual(before.value, after.value)) {
    entries.push({
      type: "modified",
      path: `${path}/value`,
      description: `Value expression changed in ${before.type.toLowerCase()} ${before.name}`,
      nodeBefore: before.value,
      nodeAfter: after.value,
      line: after.line,
      column: after.column,
    });
  }

  // Check type annotation changes
  const beforeType = before.typeAnnotation ? stringifyType(before.typeAnnotation) : null;
  const afterType = after.typeAnnotation ? stringifyType(after.typeAnnotation) : null;

  if (beforeType !== afterType) {
    entries.push({
      type: "modified",
      path: `${path}/type`,
      description: `Type annotation changed: ${beforeType ?? "untyped"} -> ${afterType ?? "untyped"}`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
  }

  return entries;
}

// Compare two on handlers
function diffOnHandlers(
  before: OnStmt,
  after: OnStmt,
  path: string
): DiffEntry[] {
  const entries: DiffEntry[] = [];

  // Check signal path changes
  const beforeSignal = before.signal.parts.join(".");
  const afterSignal = after.signal.parts.join(".");

  if (beforeSignal !== afterSignal) {
    entries.push({
      type: "modified",
      path: `${path}/signal`,
      description: `Signal changed: @${beforeSignal} -> @${afterSignal}`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
  }

  // Check filter changes
  if (!deepEqual(before.filter, after.filter)) {
    entries.push({
      type: "modified",
      path: `${path}/filter`,
      description: `Filter expression changed in on @${afterSignal}`,
      nodeBefore: before.filter,
      nodeAfter: after.filter,
      line: after.line,
      column: after.column,
    });
  }

  // Check body changes
  const bodyChanges = diffBlocks(before.body, after.body, `${path}/body`);
  entries.push(...bodyChanges);

  return entries;
}

// Compare two blocks
function diffBlocks(before: Block, after: Block, path: string): DiffEntry[] {
  const entries: DiffEntry[] = [];

  // Compare statement counts
  if (before.statements.length !== after.statements.length) {
    entries.push({
      type: "modified",
      path,
      description: `Block size changed: ${before.statements.length} -> ${after.statements.length} statements`,
      nodeBefore: before,
      nodeAfter: after,
      line: after.line,
      column: after.column,
    });
  }

  // Compare individual statements (positional comparison)
  const maxLen = Math.max(before.statements.length, after.statements.length);

  for (let i = 0; i < maxLen; i++) {
    const beforeStmt = before.statements[i];
    const afterStmt = after.statements[i];

    if (!beforeStmt) {
      entries.push({
        type: "added",
        path: `${path}/stmt:${i}`,
        description: `Added statement at position ${i}`,
        nodeAfter: afterStmt,
        line: afterStmt.line,
        column: afterStmt.column,
      });
    } else if (!afterStmt) {
      entries.push({
        type: "removed",
        path: `${path}/stmt:${i}`,
        description: `Removed statement at position ${i}`,
        nodeBefore: beforeStmt,
        line: beforeStmt.line,
        column: beforeStmt.column,
      });
    } else if (!deepEqual(beforeStmt, afterStmt)) {
      entries.push({
        type: "modified",
        path: `${path}/stmt:${i}`,
        description: `Modified statement at position ${i}`,
        nodeBefore: beforeStmt,
        nodeAfter: afterStmt,
        line: afterStmt.line,
        column: afterStmt.column,
      });
    }
  }

  return entries;
}

// Simple type stringification for comparison
function stringifyType(type: any): string {
  if (!type) return "";
  switch (type.type) {
    case "NamedType":
      const args = type.typeArgs
        ? `[${type.typeArgs.map(stringifyType).join(", ")}]`
        : "";
      return `${type.name}${args}`;
    case "RecordType":
      return `{${type.fields.map((f: any) => `${f.key}: ${stringifyType(f.typeExpr)}`).join(", ")}}`;
    default:
      return type.type;
  }
}

// Deep equality check (ignoring line/column)
function deepEqual(a: any, b: any): boolean {
  if (a === b) return true;
  if (a == null || b == null) return a === b;
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object") return a === b;

  // Skip line/column for comparison
  const aKeys = Object.keys(a).filter((k) => k !== "line" && k !== "column");
  const bKeys = Object.keys(b).filter((k) => k !== "line" && k !== "column");

  if (aKeys.length !== bKeys.length) return false;

  for (const key of aKeys) {
    if (!bKeys.includes(key)) return false;
    if (!deepEqual(a[key], b[key])) return false;
  }

  return true;
}

// Format a diff result as a human-readable string
export function formatDiff(result: DiffResult): string {
  const lines: string[] = [];

  if (result.entries.length === 0) {
    lines.push("No changes detected.");
    return lines.join("\n");
  }

  lines.push(`Changes: ${result.entries.length} total`);
  lines.push(`  + ${result.summary.added} added`);
  lines.push(`  - ${result.summary.removed} removed`);
  lines.push(`  ~ ${result.summary.modified} modified`);
  lines.push("");

  for (const entry of result.entries) {
    const prefix =
      entry.type === "added"
        ? "+"
        : entry.type === "removed"
          ? "-"
          : entry.type === "modified"
            ? "~"
            : " ";

    const location = entry.line ? ` (line ${entry.line})` : "";
    lines.push(`${prefix} ${entry.description}${location}`);
  }

  return lines.join("\n");
}

// Compare source code strings by parsing and diffing ASTs
export function diffSource(
  beforeSource: string,
  afterSource: string,
  parse: (source: string) => Program,
  filePath?: string
): DiffResult {
  const beforeAst = parse(beforeSource);
  const afterAst = parse(afterSource);
  return diffPrograms(beforeAst, afterAst, filePath);
}
