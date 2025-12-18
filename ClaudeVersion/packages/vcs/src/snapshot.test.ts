import { describe, it, expect, beforeEach } from "bun:test";
import {
  VirtualFileSystem,
} from "@oort/engine";
import {
  createSnapshot,
  restoreSnapshot,
  getChangedFiles,
  serializeSnapshot,
  deserializeSnapshot,
} from "./snapshot";

describe("Snapshot", () => {
  let vfs: VirtualFileSystem;

  beforeEach(() => {
    vfs = new VirtualFileSystem();
  });

  describe("createSnapshot", () => {
    it("creates a snapshot with ID and timestamp", () => {
      const snapshot = createSnapshot(vfs);
      expect(snapshot.id).toMatch(/^snap_/);
      expect(snapshot.timestamp).toBeGreaterThan(0);
    });

    it("creates a snapshot with name", () => {
      const snapshot = createSnapshot(vfs, "test-snapshot");
      expect(snapshot.name).toBe("test-snapshot");
    });

    it("captures file contents", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "hello world");

      const snapshot = createSnapshot(vfs);
      const file = snapshot.files.find((f) => f.path === "/test/file.txt");

      expect(file).toBeDefined();
      expect(file?.content).toBe("hello world");
      expect(file?.isDirectory).toBe(false);
    });

    it("captures directories", () => {
      vfs.mkdir("/mydir");

      const snapshot = createSnapshot(vfs);
      const dir = snapshot.files.find((f) => f.path === "/mydir");

      expect(dir).toBeDefined();
      expect(dir?.isDirectory).toBe(true);
    });

    it("captures file permissions", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");
      vfs.chmod("/test/file.txt", "r--");

      const snapshot = createSnapshot(vfs);
      const file = snapshot.files.find((f) => f.path === "/test/file.txt");

      expect(file?.permissions.read).toBe(true);
      expect(file?.permissions.write).toBe(false);
      expect(file?.permissions.execute).toBe(false);
    });
  });

  describe("restoreSnapshot", () => {
    it("restores file contents", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "original");

      const snapshot = createSnapshot(vfs);

      vfs.write("/test/file.txt", "modified");
      expect(vfs.read("/test/file.txt")).toBe("modified");

      restoreSnapshot(vfs, snapshot);
      expect(vfs.read("/test/file.txt")).toBe("original");
    });

    it("creates missing directories", () => {
      vfs.mkdir("/dir1");
      vfs.mkdir("/dir1/subdir");
      vfs.write("/dir1/subdir/file.txt", "test");

      const snapshot = createSnapshot(vfs);

      // Create new VFS
      const newVfs = new VirtualFileSystem();
      restoreSnapshot(newVfs, snapshot);

      expect(newVfs.exists("/dir1")).toBe(true);
      expect(newVfs.exists("/dir1/subdir")).toBe(true);
      expect(newVfs.read("/dir1/subdir/file.txt")).toBe("test");
    });
  });

  describe("getChangedFiles", () => {
    it("detects added files", () => {
      const before = createSnapshot(vfs);

      vfs.mkdir("/new");
      vfs.write("/new/file.txt", "new content");

      const after = createSnapshot(vfs);
      const changes = getChangedFiles(before, after);

      expect(changes.added).toContain("/new");
      expect(changes.added).toContain("/new/file.txt");
      expect(changes.removed.length).toBe(0);
    });

    it("detects removed files", () => {
      vfs.mkdir("/old");
      vfs.write("/old/file.txt", "old content");

      const before = createSnapshot(vfs);

      vfs.rm("/old/file.txt");
      vfs.rm("/old");

      const after = createSnapshot(vfs);
      const changes = getChangedFiles(before, after);

      expect(changes.removed).toContain("/old");
      expect(changes.removed).toContain("/old/file.txt");
      expect(changes.added.length).toBe(0);
    });

    it("detects modified files", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "original");

      const before = createSnapshot(vfs);

      vfs.write("/test/file.txt", "modified");

      const after = createSnapshot(vfs);
      const changes = getChangedFiles(before, after);

      expect(changes.modified).toContain("/test/file.txt");
      expect(changes.added.length).toBe(0);
      expect(changes.removed.length).toBe(0);
    });

    it("detects permission changes", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "content");

      const before = createSnapshot(vfs);

      vfs.chmod("/test/file.txt", "r--");

      const after = createSnapshot(vfs);
      const changes = getChangedFiles(before, after);

      expect(changes.modified).toContain("/test/file.txt");
    });
  });

  describe("serialization", () => {
    it("serializes and deserializes a snapshot", () => {
      vfs.mkdir("/test");
      vfs.write("/test/file.txt", "hello");

      const original = createSnapshot(vfs, "my-snapshot");
      original.metadata.set("key", "value");

      const json = serializeSnapshot(original);
      const restored = deserializeSnapshot(json);

      expect(restored.name).toBe("my-snapshot");
      expect(restored.files.length).toBe(original.files.length);
      expect(restored.metadata.get("key")).toBe("value");
    });
  });
});
