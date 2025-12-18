// VCS Standard Library Functions
// These functions are bound to a History instance

import {
  type SlateValue,
  type SlateNativeFunction,
  Str,
  Bool,
  Null,
  List,
  Record,
  Num,
  isString,
  isNumber,
  RuntimeError,
} from "@oort/core";

import type { History } from "./history";

// Helper to create a native function
function native(
  name: string,
  arity: number | "variadic",
  fn: (args: SlateValue[]) => SlateValue
): SlateNativeFunction {
  return { type: "native", name, arity, fn };
}

// Helper to assert argument types
function assertString(v: SlateValue, name: string): string {
  if (!isString(v)) {
    throw new RuntimeError(`${name} expects a string, got ${v.type}`);
  }
  return v.value;
}

function assertNumber(v: SlateValue, name: string): number {
  if (!isNumber(v)) {
    throw new RuntimeError(`${name} expects a number, got ${v.type}`);
  }
  return v.value;
}

// Create VCS stdlib functions bound to a specific History instance
export function createVcsStdlib(
  history: History
): Map<string, SlateNativeFunction> {
  // vcs.snapshot "name" - Create a named snapshot
  const snapshot = native("snapshot", 1, ([name]) => {
    const n = assertString(name, "snapshot");
    history.saveSnapshot(n);
    return Null();
  });

  // vcs.restore "name" - Restore a named snapshot
  const restore = native("restore", 1, ([name]) => {
    const n = assertString(name, "restore");
    const success = history.loadSnapshot(n);
    if (!success) {
      throw new RuntimeError(`Snapshot not found: ${n}`);
    }
    return Null();
  });

  // vcs.undo - Undo last change
  const undo = native("undo", 0, () => {
    const success = history.undo();
    return Bool(success);
  });

  // vcs.redo - Redo last undone change
  const redo = native("redo", 0, () => {
    const success = history.redo();
    return Bool(success);
  });

  // vcs.commit "description" - Create a checkpoint
  const commit = native("commit", "variadic", (args) => {
    const description = args.length > 0 ? assertString(args[0], "commit") : undefined;
    history.commit(description);
    return Null();
  });

  // vcs.canUndo - Check if undo is available
  const canUndo = native("canUndo", 0, () => {
    return Bool(history.canUndo());
  });

  // vcs.canRedo - Check if redo is available
  const canRedo = native("canRedo", 0, () => {
    return Bool(history.canRedo());
  });

  // vcs.hasChanges - Check if there are uncommitted changes
  const hasChanges = native("hasChanges", 0, () => {
    return Bool(history.hasChanges());
  });

  // vcs.history - Get list of history entries
  const historyList = native("history", 0, () => {
    const entries = history.getHistory();
    return List(
      entries.map((entry) => {
        const fields = new Map<string, SlateValue>();
        fields.set("index", Num(entry.index));
        fields.set("timestamp", Num(entry.timestamp));
        fields.set("isCurrent", Bool(entry.isCurrent));
        if (entry.description) {
          fields.set("description", Str(entry.description));
        }
        return Record(fields);
      })
    );
  });

  // vcs.goto index - Jump to specific history index
  const goto = native("goto", 1, ([index]) => {
    const i = assertNumber(index, "goto");
    const success = history.goto(Math.floor(i));
    return Bool(success);
  });

  // vcs.snapshots - List all named snapshots
  const snapshots = native("snapshots", 0, () => {
    const snaps = history.listSnapshots();
    return List(
      snaps.map((snap) => {
        const fields = new Map<string, SlateValue>();
        fields.set("name", Str(snap.name));
        fields.set("timestamp", Num(snap.timestamp));
        return Record(fields);
      })
    );
  });

  // vcs.deleteSnapshot "name" - Delete a named snapshot
  const deleteSnapshot = native("deleteSnapshot", 1, ([name]) => {
    const n = assertString(name, "deleteSnapshot");
    const success = history.deleteSnapshot(n);
    return Bool(success);
  });

  // vcs.hasSnapshot "name" - Check if a named snapshot exists
  const hasSnapshot = native("hasSnapshot", 1, ([name]) => {
    const n = assertString(name, "hasSnapshot");
    return Bool(history.hasSnapshot(n));
  });

  // vcs.changes - Get uncommitted changes
  const changes = native("changes", 0, () => {
    const ch = history.getUncommittedChanges();
    if (!ch) {
      return Null();
    }

    const fields = new Map<string, SlateValue>();
    fields.set("added", List(ch.added.map(Str)));
    fields.set("modified", List(ch.modified.map(Str)));
    fields.set("removed", List(ch.removed.map(Str)));
    return Record(fields);
  });

  // vcs.undoSteps - Get number of available undo steps
  const undoSteps = native("undoSteps", 0, () => {
    return Num(history.undoSteps());
  });

  // vcs.redoSteps - Get number of available redo steps
  const redoSteps = native("redoSteps", 0, () => {
    return Num(history.redoSteps());
  });

  // vcs.diff fromIndex, toIndex - Get diff between two history entries
  const diff = native("diff", 2, ([from, to]) => {
    const fromIdx = assertNumber(from, "diff");
    const toIdx = assertNumber(to, "diff");
    const d = history.diff(Math.floor(fromIdx), Math.floor(toIdx));

    if (!d) {
      return Null();
    }

    const fields = new Map<string, SlateValue>();
    fields.set("added", List(d.added.map(Str)));
    fields.set("modified", List(d.modified.map(Str)));
    fields.set("removed", List(d.removed.map(Str)));
    return Record(fields);
  });

  // vcs.reset - Clear all history and start fresh
  const reset = native("reset", 0, () => {
    history.reset();
    return Null();
  });

  return new Map<string, SlateNativeFunction>([
    ["snapshot", snapshot],
    ["restore", restore],
    ["undo", undo],
    ["redo", redo],
    ["commit", commit],
    ["canUndo", canUndo],
    ["canRedo", canRedo],
    ["hasChanges", hasChanges],
    ["history", historyList],
    ["goto", goto],
    ["snapshots", snapshots],
    ["deleteSnapshot", deleteSnapshot],
    ["hasSnapshot", hasSnapshot],
    ["changes", changes],
    ["undoSteps", undoSteps],
    ["redoSteps", redoSteps],
    ["diff", diff],
    ["reset", reset],
  ]);
}

// Create a VCS namespace object that can be used in Slate
// Usage: vcs.snapshot("name"), vcs.undo(), etc.
export function createVcsNamespace(
  history: History
): SlateValue {
  const stdlib = createVcsStdlib(history);
  const fields = new Map<string, SlateValue>();

  for (const [name, fn] of stdlib) {
    fields.set(name, fn);
  }

  return { type: "record", fields };
}
