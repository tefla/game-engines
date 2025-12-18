import { describe, it, expect, beforeEach } from "bun:test";
import { VirtualFileSystem } from "@oort/engine";
import { History, createHistory } from "./history";

describe("History", () => {
  let vfs: VirtualFileSystem;
  let history: History;

  beforeEach(() => {
    vfs = new VirtualFileSystem();
    history = createHistory(vfs);
  });

  describe("basic operations", () => {
    it("creates initial snapshot on construction", () => {
      const entries = history.getHistory();
      expect(entries.length).toBe(1);
      expect(entries[0].description).toBe("Initial state");
    });

    it("commits changes", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");

      history.commit("Added test file");

      const entries = history.getHistory();
      expect(entries.length).toBe(2);
      expect(entries[1].description).toBe("Added test file");
    });
  });

  describe("undo/redo", () => {
    it("undoes last change", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "original");
      history.commit("First version");

      vfs.write("/test/file.txt", "modified");
      history.commit("Modified version");

      expect(vfs.read("/test/file.txt")).toBe("modified");

      history.undo();
      expect(vfs.read("/test/file.txt")).toBe("original");
    });

    it("redoes undone change", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "original");
      history.commit("First");

      vfs.write("/test/file.txt", "modified");
      history.commit("Second");

      history.undo();
      expect(vfs.read("/test/file.txt")).toBe("original");

      history.redo();
      expect(vfs.read("/test/file.txt")).toBe("modified");
    });

    it("returns false when no undo available", () => {
      expect(history.canUndo()).toBe(false);
      expect(history.undo()).toBe(false);
    });

    it("returns false when no redo available", () => {
      expect(history.canRedo()).toBe(false);
      expect(history.redo()).toBe(false);
    });

    it("clears redo history when new commit is made", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "v1");
      history.commit("V1");

      vfs.write("/test/file.txt", "v2");
      history.commit("V2");

      history.undo();
      expect(history.canRedo()).toBe(true);

      vfs.write("/test/file.txt", "v3");
      history.commit("V3 - branch");

      expect(history.canRedo()).toBe(false);
    });

    it("tracks undo/redo steps", () => {
      vfs.mkdir("/test");

      for (let i = 0; i < 5; i++) {
        vfs.write("/test/file.txt", `version ${i}`);
        history.commit(`V${i}`);
      }

      expect(history.undoSteps()).toBe(5); // 5 commits + 1 initial
      expect(history.redoSteps()).toBe(0);

      history.undo();
      history.undo();

      expect(history.undoSteps()).toBe(3);
      expect(history.redoSteps()).toBe(2);
    });
  });

  describe("named snapshots", () => {
    it("saves and restores named snapshots", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "checkpoint content");

      history.saveSnapshot("checkpoint-1");

      vfs.write("/test/file.txt", "new content");
      expect(vfs.read("/test/file.txt")).toBe("new content");

      history.loadSnapshot("checkpoint-1");
      expect(vfs.read("/test/file.txt")).toBe("checkpoint content");
    });

    it("lists named snapshots", () => {
      history.saveSnapshot("snap1");
      history.saveSnapshot("snap2");

      const snapshots = history.listSnapshots();
      expect(snapshots.length).toBe(2);
      expect(snapshots.map((s) => s.name)).toContain("snap1");
      expect(snapshots.map((s) => s.name)).toContain("snap2");
    });

    it("deletes named snapshots", () => {
      history.saveSnapshot("to-delete");
      expect(history.hasSnapshot("to-delete")).toBe(true);

      history.deleteSnapshot("to-delete");
      expect(history.hasSnapshot("to-delete")).toBe(false);
    });

    it("returns false when restoring non-existent snapshot", () => {
      const result = history.loadSnapshot("does-not-exist");
      expect(result).toBe(false);
    });
  });

  describe("goto", () => {
    it("jumps to specific history index", () => {
      // Index 0: Initial state (created by History constructor)
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "v1");
      history.commit("Version 1"); // Index 1

      vfs.write("/test/file.txt", "v2");
      history.commit("Version 2"); // Index 2

      vfs.write("/test/file.txt", "v3");
      history.commit("Version 3"); // Index 3

      // Go to index 1 (Version 1)
      history.goto(1);
      expect(vfs.read("/test/file.txt")).toBe("v1");

      // Go to index 3 (Version 3)
      history.goto(3);
      expect(vfs.read("/test/file.txt")).toBe("v3");
    });

    it("returns false for invalid index", () => {
      expect(history.goto(-1)).toBe(false);
      expect(history.goto(100)).toBe(false);
    });
  });

  describe("changes detection", () => {
    it("detects uncommitted changes", () => {
      expect(history.hasChanges()).toBe(false);

      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "new");

      expect(history.hasChanges()).toBe(true);
    });

    it("gets uncommitted changes details", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");

      const changes = history.getUncommittedChanges();
      expect(changes?.added).toContain("/test");
      expect(changes?.added).toContain("/test/file.txt");
    });
  });

  describe("diff between entries", () => {
    it("diffs between two history entries", () => {
      vfs.mkdir("/test");
      history.commit("Create dir");

      vfs.write("/test/file.txt", "content");
      history.commit("Create file");

      const diff = history.diff(1, 2);
      expect(diff?.added).toContain("/test/file.txt");
    });

    it("returns null for invalid indices", () => {
      expect(history.diff(-1, 0)).toBeNull();
      expect(history.diff(0, 100)).toBeNull();
    });
  });

  describe("max history limit", () => {
    it("trims history when exceeding max", () => {
      const smallHistory = createHistory(vfs, { maxHistory: 5 });

      for (let i = 0; i < 10; i++) {
        vfs.mkdir(`/dir${i}`);
        smallHistory.commit(`Commit ${i}`);
      }

      const entries = smallHistory.getHistory();
      expect(entries.length).toBeLessThanOrEqual(5);
    });
  });

  describe("serialization", () => {
    it("exports and imports history", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");
      history.commit("Add file");

      history.saveSnapshot("named-snap");

      const exported = history.exportHistory();

      // Create new VFS and history
      const newVfs = new VirtualFileSystem();
      const newHistory = createHistory(newVfs);
      newHistory.importHistory(exported);

      expect(newVfs.read("/test/file.txt")).toBe("content");
      expect(newHistory.hasSnapshot("named-snap")).toBe(true);
    });
  });

  describe("reset", () => {
    it("clears all history", () => {
      vfs.mkdir("/test");
      history.commit("Some changes");

      history.reset();

      const entries = history.getHistory();
      expect(entries.length).toBe(1);
      expect(entries[0].description).toBe("Reset");
    });
  });
});
