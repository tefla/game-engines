import { describe, it, expect, beforeEach } from "bun:test";
import {
  VirtualFileSystem,
  VFile,
  VDirectory,
  PermissionError,
  parsePermissions,
  permissionsToString,
  DEFAULT_PERMISSIONS,
  READ_ONLY,
  HIDDEN,
} from "./";

describe("Permissions", () => {
  it("parses rwx permission string", () => {
    const perms = parsePermissions("rwx");
    expect(perms.read).toBe(true);
    expect(perms.write).toBe(true);
    expect(perms.execute).toBe(true);
  });

  it("parses r-- permission string", () => {
    const perms = parsePermissions("r--");
    expect(perms.read).toBe(true);
    expect(perms.write).toBe(false);
    expect(perms.execute).toBe(false);
  });

  it("parses rw- permission string", () => {
    const perms = parsePermissions("rw-");
    expect(perms.read).toBe(true);
    expect(perms.write).toBe(true);
    expect(perms.execute).toBe(false);
  });

  it("converts permissions to string", () => {
    expect(permissionsToString(DEFAULT_PERMISSIONS)).toBe("rwx");
    expect(permissionsToString(READ_ONLY)).toBe("r-x");
    expect(permissionsToString(HIDDEN)).toBe("---");
  });

  it("throws on invalid permission string", () => {
    expect(() => parsePermissions("rw")).toThrow();
    expect(() => parsePermissions("rwxx")).toThrow();
  });
});

describe("VFile", () => {
  it("creates file with default permissions", () => {
    const file = new VFile("test.txt", "hello");
    expect(file.name).toBe("test.txt");
    expect(file.content).toBe("hello");
    expect(file.permissions.read).toBe(true);
    expect(file.permissions.write).toBe(true);
    expect(file.isFile()).toBe(true);
    expect(file.isDirectory()).toBe(false);
  });

  it("creates file with custom permissions", () => {
    const file = new VFile("test.txt", "hello", READ_ONLY);
    expect(file.permissions.read).toBe(true);
    expect(file.permissions.write).toBe(false);
  });

  it("computes path correctly", () => {
    const root = new VDirectory("");
    const dir = new VDirectory("home");
    root.addChild(dir);
    const file = new VFile("test.txt");
    dir.addChild(file);
    expect(file.getPath()).toBe("/home/test.txt");
  });
});

describe("VDirectory", () => {
  it("creates directory with default permissions", () => {
    const dir = new VDirectory("home");
    expect(dir.name).toBe("home");
    expect(dir.isFile()).toBe(false);
    expect(dir.isDirectory()).toBe(true);
  });

  it("manages children correctly", () => {
    const dir = new VDirectory("home");
    const file = new VFile("test.txt");

    dir.addChild(file);
    expect(dir.getChild("test.txt")).toBe(file);
    expect(file.parent).toBe(dir);

    const children = dir.listChildren();
    expect(children.length).toBe(1);
    expect(children[0]).toBe(file);

    dir.removeChild("test.txt");
    expect(dir.getChild("test.txt")).toBeUndefined();
  });
});

describe("VirtualFileSystem", () => {
  let vfs: VirtualFileSystem;

  beforeEach(() => {
    vfs = new VirtualFileSystem();
  });

  describe("mkdir", () => {
    it("creates directory at root", () => {
      vfs.mkdir("/test");
      expect(vfs.exists("/test")).toBe(true);
    });

    it("creates nested directories", () => {
      vfs.mkdir("/a");
      vfs.mkdir("/a/b");
      vfs.mkdir("/a/b/c");
      expect(vfs.exists("/a/b/c")).toBe(true);
    });

    it("throws when creating existing directory", () => {
      vfs.mkdir("/test");
      expect(() => vfs.mkdir("/test")).toThrow("Already exists");
    });

    it("creates directory with custom permissions", () => {
      vfs.mkdir("/readonly", READ_ONLY);
      expect(vfs.getPermissions("/readonly")).toBe("r-x");
    });
  });

  describe("write and read", () => {
    it("creates and reads file", () => {
      vfs.write("/test.txt", "hello world");
      expect(vfs.read("/test.txt")).toBe("hello world");
    });

    it("overwrites existing file", () => {
      vfs.write("/test.txt", "hello");
      vfs.write("/test.txt", "world");
      expect(vfs.read("/test.txt")).toBe("world");
    });

    it("throws when reading non-existent file", () => {
      expect(() => vfs.read("/missing.txt")).toThrow("File not found");
    });

    it("throws when reading directory", () => {
      vfs.mkdir("/dir");
      expect(() => vfs.read("/dir")).toThrow("Not a file");
    });
  });

  describe("ls", () => {
    it("lists root directory", () => {
      vfs.mkdir("/a");
      vfs.mkdir("/b");
      vfs.write("/c.txt", "content");

      const files = vfs.ls("/");
      expect(files.length).toBe(3);
      // Directories first, then alphabetically
      expect(files[0].name).toBe("a");
      expect(files[0].isDirectory).toBe(true);
      expect(files[1].name).toBe("b");
      expect(files[2].name).toBe("c.txt");
      expect(files[2].isDirectory).toBe(false);
    });

    it("shows file size", () => {
      vfs.write("/test.txt", "hello");
      const files = vfs.ls("/");
      expect(files[0].size).toBe(5);
    });

    it("throws when listing non-directory", () => {
      vfs.write("/test.txt", "hello");
      expect(() => vfs.ls("/test.txt")).toThrow("Not a directory");
    });
  });

  describe("rm", () => {
    it("removes file", () => {
      vfs.write("/test.txt", "hello");
      vfs.rm("/test.txt");
      expect(vfs.exists("/test.txt")).toBe(false);
    });

    it("removes empty directory", () => {
      vfs.mkdir("/empty");
      vfs.rm("/empty");
      expect(vfs.exists("/empty")).toBe(false);
    });

    it("throws when removing non-empty directory", () => {
      vfs.mkdir("/dir");
      vfs.write("/dir/file.txt", "content");
      expect(() => vfs.rm("/dir")).toThrow("Directory not empty");
    });

    it("throws when removing root", () => {
      expect(() => vfs.rm("/")).toThrow("Cannot remove root");
    });
  });

  describe("cp and mv", () => {
    it("copies file", () => {
      vfs.write("/src.txt", "hello");
      vfs.cp("/src.txt", "/dest.txt");
      expect(vfs.read("/dest.txt")).toBe("hello");
      expect(vfs.exists("/src.txt")).toBe(true);
    });

    it("moves file", () => {
      vfs.write("/src.txt", "hello");
      vfs.mv("/src.txt", "/dest.txt");
      expect(vfs.read("/dest.txt")).toBe("hello");
      expect(vfs.exists("/src.txt")).toBe(false);
    });
  });

  describe("cd and pwd", () => {
    it("starts at root", () => {
      expect(vfs.pwd()).toBe("/");
    });

    it("changes directory", () => {
      vfs.mkdir("/home");
      vfs.cd("/home");
      expect(vfs.pwd()).toBe("/home");
    });

    it("handles relative paths after cd", () => {
      vfs.mkdir("/home");
      vfs.cd("/home");
      vfs.write("test.txt", "hello");
      expect(vfs.read("/home/test.txt")).toBe("hello");
    });

    it("throws when changing to file", () => {
      vfs.write("/test.txt", "hello");
      expect(() => vfs.cd("/test.txt")).toThrow("Not a directory");
    });
  });

  describe("stat", () => {
    it("returns file info", () => {
      vfs.write("/test.txt", "hello");
      const info = vfs.stat("/test.txt");
      expect(info).not.toBeNull();
      expect(info!.name).toBe("test.txt");
      expect(info!.path).toBe("/test.txt");
      expect(info!.isDirectory).toBe(false);
      expect(info!.size).toBe(5);
    });

    it("returns null for non-existent path", () => {
      expect(vfs.stat("/missing")).toBeNull();
    });
  });

  describe("chmod", () => {
    it("changes permissions with string", () => {
      vfs.write("/test.txt", "hello");
      vfs.chmod("/test.txt", "r--");
      expect(vfs.getPermissions("/test.txt")).toBe("r--");
    });

    it("changes permissions with object", () => {
      vfs.write("/test.txt", "hello");
      vfs.chmod("/test.txt", { read: true, write: false, execute: true });
      expect(vfs.getPermissions("/test.txt")).toBe("r-x");
    });
  });

  describe("permission enforcement", () => {
    it("blocks write to read-only file", () => {
      vfs.write("/test.txt", "hello");
      vfs.chmod("/test.txt", "r--");
      expect(() => vfs.write("/test.txt", "world")).toThrow(PermissionError);
    });

    it("blocks write to read-only directory", () => {
      vfs.mkdir("/readonly", READ_ONLY);
      expect(() => vfs.write("/readonly/test.txt", "hello")).toThrow(PermissionError);
    });

    it("blocks read of hidden file", () => {
      vfs.mkdir("/secrets", HIDDEN);
      // Can't read because permission denied
      expect(() => vfs.read("/secrets")).toThrow();
    });

    it("blocks cd to directory without execute permission", () => {
      vfs.mkdir("/noexec", { read: true, write: true, execute: false });
      expect(() => vfs.cd("/noexec")).toThrow(PermissionError);
    });
  });

  describe("path normalization", () => {
    it("handles . in paths", () => {
      vfs.mkdir("/dir");
      vfs.write("/dir/./test.txt", "hello");
      expect(vfs.read("/dir/test.txt")).toBe("hello");
    });

    it("handles .. in paths", () => {
      vfs.mkdir("/dir");
      vfs.write("/dir/../root.txt", "hello");
      expect(vfs.read("/root.txt")).toBe("hello");
    });

    it("handles multiple slashes", () => {
      vfs.mkdir("/dir");
      expect(vfs.exists("/dir")).toBe(true);
    });
  });

  describe("createGameFileSystem", () => {
    it("creates standard game directories", () => {
      const gameVfs = VirtualFileSystem.createGameFileSystem();

      expect(gameVfs.exists("/engine")).toBe(true);
      expect(gameVfs.exists("/game")).toBe(true);
      expect(gameVfs.exists("/game/entities")).toBe(true);
      expect(gameVfs.exists("/game/systems")).toBe(true);
      expect(gameVfs.exists("/puzzles")).toBe(true);
      expect(gameVfs.exists("/player")).toBe(true);

      // Engine should be read-only
      expect(gameVfs.getPermissions("/engine")).toBe("r-x");

      // Puzzles should be read-write but not executable
      expect(gameVfs.getPermissions("/puzzles")).toBe("rw-");
    });
  });
});
