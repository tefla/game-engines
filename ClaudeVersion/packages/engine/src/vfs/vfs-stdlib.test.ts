import { describe, it, expect, beforeEach } from "vitest";
import { VirtualFileSystem, createVfsStdlib } from "./";
import { type SlateNativeFunction, Str, Num, Bool, isList, isString, isRecord, isBool, isNull } from "@oort/core";

describe("VFS Stdlib", () => {
  let vfs: VirtualFileSystem;
  let stdlib: Map<string, SlateNativeFunction>;

  beforeEach(() => {
    vfs = new VirtualFileSystem();
    stdlib = createVfsStdlib(vfs);
  });

  function call(name: string, ...args: any[]): any {
    const fn = stdlib.get(name);
    if (!fn) throw new Error(`Function ${name} not found`);
    return fn.fn(args);
  }

  describe("read and write", () => {
    it("writes and reads a file", () => {
      call("write", Str("/test.txt"), Str("hello world"));
      const result = call("read", Str("/test.txt"));
      expect(isString(result)).toBe(true);
      expect(result.value).toBe("hello world");
    });

    it("throws when reading non-existent file", () => {
      expect(() => call("read", Str("/missing.txt"))).toThrow();
    });
  });

  describe("ls", () => {
    it("lists directory contents", () => {
      call("mkdir", Str("/dir"));
      call("write", Str("/file.txt"), Str("content"));

      const result = call("ls", Str("/"));
      expect(isList(result)).toBe(true);
      expect(result.elements.length).toBe(2);

      // Should be sorted: directories first
      const first = result.elements[0];
      expect(isRecord(first)).toBe(true);
      expect(first.fields.get("name").value).toBe("dir");
      expect(first.fields.get("isDirectory").value).toBe(true);
    });

    it("lists current directory when no argument", () => {
      call("write", Str("/test.txt"), Str("content"));
      const result = call("ls");
      expect(isList(result)).toBe(true);
      expect(result.elements.length).toBe(1);
    });
  });

  describe("exists", () => {
    it("returns true for existing file", () => {
      call("write", Str("/test.txt"), Str("content"));
      const result = call("exists", Str("/test.txt"));
      expect(isBool(result)).toBe(true);
      expect(result.value).toBe(true);
    });

    it("returns false for non-existing file", () => {
      const result = call("exists", Str("/missing.txt"));
      expect(isBool(result)).toBe(true);
      expect(result.value).toBe(false);
    });
  });

  describe("mkdir", () => {
    it("creates a directory", () => {
      call("mkdir", Str("/newdir"));
      const result = call("exists", Str("/newdir"));
      expect(result.value).toBe(true);
    });

    it("creates directory with permissions", () => {
      call("mkdir", Str("/readonly"), Str("r--"));
      const perms = call("getPermissions", Str("/readonly"));
      expect(perms.value).toBe("r--");
    });
  });

  describe("rm", () => {
    it("removes a file", () => {
      call("write", Str("/test.txt"), Str("content"));
      call("rm", Str("/test.txt"));
      expect(call("exists", Str("/test.txt")).value).toBe(false);
    });
  });

  describe("cp and mv", () => {
    it("copies a file", () => {
      call("write", Str("/src.txt"), Str("content"));
      call("cp", Str("/src.txt"), Str("/dest.txt"));

      expect(call("exists", Str("/src.txt")).value).toBe(true);
      expect(call("exists", Str("/dest.txt")).value).toBe(true);
      expect(call("read", Str("/dest.txt")).value).toBe("content");
    });

    it("moves a file", () => {
      call("write", Str("/src.txt"), Str("content"));
      call("mv", Str("/src.txt"), Str("/dest.txt"));

      expect(call("exists", Str("/src.txt")).value).toBe(false);
      expect(call("exists", Str("/dest.txt")).value).toBe(true);
      expect(call("read", Str("/dest.txt")).value).toBe("content");
    });
  });

  describe("pwd and cd", () => {
    it("returns current directory", () => {
      const result = call("pwd");
      expect(result.value).toBe("/");
    });

    it("changes directory", () => {
      call("mkdir", Str("/home"));
      call("cd", Str("/home"));
      expect(call("pwd").value).toBe("/home");
    });
  });

  describe("stat", () => {
    it("returns file info", () => {
      call("write", Str("/test.txt"), Str("hello"));
      const result = call("stat", Str("/test.txt"));

      expect(isRecord(result)).toBe(true);
      expect(result.fields.get("name").value).toBe("test.txt");
      expect(result.fields.get("path").value).toBe("/test.txt");
      expect(result.fields.get("isDirectory").value).toBe(false);
      expect(result.fields.get("size").value).toBe(5);
    });

    it("returns null for non-existent path", () => {
      const result = call("stat", Str("/missing"));
      expect(isNull(result)).toBe(true);
    });
  });

  describe("chmod and getPermissions", () => {
    it("changes and retrieves permissions", () => {
      call("write", Str("/test.txt"), Str("content"));
      call("chmod", Str("/test.txt"), Str("r--"));

      const perms = call("getPermissions", Str("/test.txt"));
      expect(perms.value).toBe("r--");
    });
  });
});
