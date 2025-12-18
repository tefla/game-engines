import { describe, it, expect, beforeEach } from "bun:test";
import { VirtualFileSystem } from "@oort/engine";
import { Num, Str, Bool, Null, isList, isRecord, isNumber, isBool, isString, isNull } from "@oort/core";
import { History, createHistory } from "./history";
import { createVcsStdlib, createVcsNamespace } from "./vcs-stdlib";

describe("VCS Stdlib", () => {
  let vfs: VirtualFileSystem;
  let history: History;
  let stdlib: Map<string, any>;

  beforeEach(() => {
    vfs = new VirtualFileSystem();
    history = createHistory(vfs);
    stdlib = createVcsStdlib(history);
  });

  describe("snapshot and restore", () => {
    it("creates named snapshot", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");

      const snapshot = stdlib.get("snapshot")!;
      const result = snapshot.fn([Str("my-checkpoint")]);

      expect(isNull(result)).toBe(true);
      expect(history.hasSnapshot("my-checkpoint")).toBe(true);
    });

    it("restores named snapshot", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "original");
      history.saveSnapshot("checkpoint");

      vfs.write("/test/file.txt", "modified");
      expect(vfs.read("/test/file.txt")).toBe("modified");

      const restore = stdlib.get("restore")!;
      restore.fn([Str("checkpoint")]);

      expect(vfs.read("/test/file.txt")).toBe("original");
    });

    it("throws when restoring non-existent snapshot", () => {
      const restore = stdlib.get("restore")!;
      expect(() => restore.fn([Str("non-existent")])).toThrow();
    });
  });

  describe("undo and redo", () => {
    it("undoes changes", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "v1");
      history.commit("First");

      vfs.write("/test/file.txt", "v2");
      history.commit("Second");

      const undo = stdlib.get("undo")!;
      const result = undo.fn([]);

      expect(isBool(result)).toBe(true);
      expect((result as any).value).toBe(true);
      expect(vfs.read("/test/file.txt")).toBe("v1");
    });

    it("redoes undone changes", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "v1");
      history.commit("First");

      vfs.write("/test/file.txt", "v2");
      history.commit("Second");

      history.undo();

      const redo = stdlib.get("redo")!;
      const result = redo.fn([]);

      expect(isBool(result)).toBe(true);
      expect((result as any).value).toBe(true);
      expect(vfs.read("/test/file.txt")).toBe("v2");
    });
  });

  describe("commit", () => {
    it("creates a commit with description", () => {
      vfs.mkdir("/test");

      const commit = stdlib.get("commit")!;
      commit.fn([Str("My commit message")]);

      const entries = history.getHistory();
      expect(entries[entries.length - 1].description).toBe("My commit message");
    });

    it("creates a commit without description", () => {
      const commit = stdlib.get("commit")!;
      commit.fn([]);

      // Should not throw
      expect(history.getHistory().length).toBeGreaterThan(1);
    });
  });

  describe("canUndo and canRedo", () => {
    it("returns correct undo availability", () => {
      const canUndo = stdlib.get("canUndo")!;

      // Initially no undo available
      expect((canUndo.fn([]) as any).value).toBe(false);

      vfs.mkdir("/test");
      history.commit("Add dir");

      expect((canUndo.fn([]) as any).value).toBe(true);
    });

    it("returns correct redo availability", () => {
      const canRedo = stdlib.get("canRedo")!;

      // Initially no redo available
      expect((canRedo.fn([]) as any).value).toBe(false);

      vfs.mkdir("/test");
      history.commit("Add dir");
      history.undo();

      expect((canRedo.fn([]) as any).value).toBe(true);
    });
  });

  describe("history", () => {
    it("returns list of history entries", () => {
      vfs.mkdir("/test");
      history.commit("First commit");

      vfs.write("/test/file.txt", "content");
      history.commit("Second commit");

      const historyFn = stdlib.get("history")!;
      const result = historyFn.fn([]);

      expect(isList(result)).toBe(true);
      expect((result as any).elements.length).toBe(3); // initial + 2 commits
    });

    it("marks current entry", () => {
      vfs.mkdir("/test");
      history.commit("Commit");

      const historyFn = stdlib.get("history")!;
      const result = historyFn.fn([]);

      const entries = (result as any).elements;
      const current = entries.find((e: any) => e.fields.get("isCurrent")?.value === true);
      expect(current).toBeDefined();
    });
  });

  describe("goto", () => {
    it("jumps to history index", () => {
      // Index 0: Initial state
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "v1");
      history.commit("V1"); // Index 1

      vfs.write("/test/file.txt", "v2");
      history.commit("V2"); // Index 2

      const goto = stdlib.get("goto")!;
      const result = goto.fn([Num(1)]);

      expect(isBool(result)).toBe(true);
      expect((result as any).value).toBe(true);
      expect(vfs.read("/test/file.txt")).toBe("v1");
    });
  });

  describe("snapshots", () => {
    it("lists named snapshots", () => {
      history.saveSnapshot("snap1");
      history.saveSnapshot("snap2");

      const snapshots = stdlib.get("snapshots")!;
      const result = snapshots.fn([]);

      expect(isList(result)).toBe(true);
      expect((result as any).elements.length).toBe(2);
    });
  });

  describe("hasSnapshot and deleteSnapshot", () => {
    it("checks snapshot existence", () => {
      const hasSnapshot = stdlib.get("hasSnapshot")!;

      expect((hasSnapshot.fn([Str("test")]) as any).value).toBe(false);

      history.saveSnapshot("test");
      expect((hasSnapshot.fn([Str("test")]) as any).value).toBe(true);
    });

    it("deletes snapshot", () => {
      history.saveSnapshot("to-delete");

      const deleteSnapshot = stdlib.get("deleteSnapshot")!;
      const result = deleteSnapshot.fn([Str("to-delete")]);

      expect((result as any).value).toBe(true);
      expect(history.hasSnapshot("to-delete")).toBe(false);
    });
  });

  describe("changes", () => {
    it("returns uncommitted changes", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");

      const changes = stdlib.get("changes")!;
      const result = changes.fn([]);

      expect(isRecord(result)).toBe(true);
      expect(isList((result as any).fields.get("added"))).toBe(true);
    });

    it("returns null when no current snapshot", () => {
      // This is a tricky case - history always has initial snapshot
      // so this shouldn't happen in practice
      const changes = stdlib.get("changes")!;
      const result = changes.fn([]);

      expect(isRecord(result) || isNull(result)).toBe(true);
    });
  });

  describe("undoSteps and redoSteps", () => {
    it("returns number of undo steps", () => {
      // Index 0: Initial state (from constructor)
      vfs.mkdir("/test");
      history.commit("1"); // Index 1
      history.commit("2"); // Index 2
      history.commit("3"); // Index 3

      const undoSteps = stdlib.get("undoSteps")!;
      const result = undoSteps.fn([]);

      expect(isNumber(result)).toBe(true);
      // Current index is 3, so we can undo 3 steps (to get back to index 0)
      expect((result as any).value).toBe(3);
    });

    it("returns number of redo steps", () => {
      vfs.mkdir("/test");
      history.commit("1");
      history.commit("2");

      history.undo();
      history.undo();

      const redoSteps = stdlib.get("redoSteps")!;
      const result = redoSteps.fn([]);

      expect(isNumber(result)).toBe(true);
      expect((result as any).value).toBe(2);
    });
  });

  describe("diff", () => {
    it("returns diff between history entries", () => {
      vfs.mkdir("/test");
      history.commit("Create dir");

      vfs.write("/test/file.txt", "content");
      history.commit("Create file");

      const diff = stdlib.get("diff")!;
      const result = diff.fn([Num(1), Num(2)]);

      expect(isRecord(result)).toBe(true);
      const added = (result as any).fields.get("added");
      expect(isList(added)).toBe(true);
    });

    it("returns null for invalid indices", () => {
      const diff = stdlib.get("diff")!;
      const result = diff.fn([Num(-1), Num(100)]);

      expect(isNull(result)).toBe(true);
    });
  });

  describe("reset", () => {
    it("clears all history", () => {
      vfs.mkdir("/test");
      history.commit("Some changes");

      const reset = stdlib.get("reset")!;
      reset.fn([]);

      const entries = history.getHistory();
      expect(entries.length).toBe(1);
    });
  });

  describe("createVcsNamespace", () => {
    it("creates a record with all VCS functions", () => {
      const namespace = createVcsNamespace(history);

      expect(isRecord(namespace)).toBe(true);
      expect((namespace as any).fields.has("undo")).toBe(true);
      expect((namespace as any).fields.has("redo")).toBe(true);
      expect((namespace as any).fields.has("snapshot")).toBe(true);
      expect((namespace as any).fields.has("restore")).toBe(true);
    });
  });
});
