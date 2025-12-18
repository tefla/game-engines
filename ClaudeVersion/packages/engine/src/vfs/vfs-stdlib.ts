// VFS Standard Library Functions
// These functions are bound to a VirtualFileSystem instance

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
  RuntimeError,
} from "@oort/core";

import { type VirtualFileSystem, type FileInfo, parsePermissions } from "./";

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

// Convert FileInfo to Slate record
function fileInfoToRecord(info: FileInfo): SlateValue {
  const fields = new Map<string, SlateValue>();
  fields.set("name", Str(info.name));
  fields.set("path", Str(info.path));
  fields.set("isDirectory", Bool(info.isDirectory));
  fields.set("permissions", Str(info.permissions));
  if (info.size !== undefined) {
    fields.set("size", Num(info.size));
  }
  return Record(fields);
}

// Create VFS stdlib functions bound to a specific VFS instance
export function createVfsStdlib(
  vfs: VirtualFileSystem
): Map<string, SlateNativeFunction> {
  const read = native("read", 1, ([path]) => {
    const p = assertString(path, "read");
    try {
      return Str(vfs.read(p));
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const write = native("write", 2, ([path, content]) => {
    const p = assertString(path, "write");
    const c = assertString(content, "write");
    try {
      vfs.write(p, c);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const chmod = native("chmod", 2, ([path, perms]) => {
    const p = assertString(path, "chmod");
    const perm = assertString(perms, "chmod");
    try {
      vfs.chmod(p, perm);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const ls = native("ls", "variadic", (args) => {
    const path = args.length > 0 ? assertString(args[0], "ls") : ".";
    try {
      const files = vfs.ls(path);
      return List(files.map(fileInfoToRecord));
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const exists = native("exists", 1, ([path]) => {
    const p = assertString(path, "exists");
    return Bool(vfs.exists(p));
  });

  const mkdir = native("mkdir", "variadic", (args) => {
    if (args.length < 1) {
      throw new RuntimeError("mkdir requires a path argument");
    }
    const path = assertString(args[0], "mkdir");
    const perms = args.length > 1 ? assertString(args[1], "mkdir") : undefined;
    try {
      if (perms) {
        vfs.mkdir(path, parsePermissions(perms));
      } else {
        vfs.mkdir(path);
      }
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const rm = native("rm", 1, ([path]) => {
    const p = assertString(path, "rm");
    try {
      vfs.rm(p);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const cp = native("cp", 2, ([src, dest]) => {
    const s = assertString(src, "cp");
    const d = assertString(dest, "cp");
    try {
      vfs.cp(s, d);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const mv = native("mv", 2, ([src, dest]) => {
    const s = assertString(src, "mv");
    const d = assertString(dest, "mv");
    try {
      vfs.mv(s, d);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const pwd = native("pwd", 0, () => {
    return Str(vfs.pwd());
  });

  const cd = native("cd", 1, ([path]) => {
    const p = assertString(path, "cd");
    try {
      vfs.cd(p);
      return Null();
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  const stat = native("stat", 1, ([path]) => {
    const p = assertString(path, "stat");
    const info = vfs.stat(p);
    if (!info) {
      return Null();
    }
    return fileInfoToRecord(info);
  });

  const getPermissions = native("getPermissions", 1, ([path]) => {
    const p = assertString(path, "getPermissions");
    try {
      return Str(vfs.getPermissions(p));
    } catch (e: any) {
      throw new RuntimeError(e.message);
    }
  });

  return new Map<string, SlateNativeFunction>([
    ["read", read],
    ["write", write],
    ["chmod", chmod],
    ["ls", ls],
    ["exists", exists],
    ["mkdir", mkdir],
    ["rm", rm],
    ["cp", cp],
    ["mv", mv],
    ["pwd", pwd],
    ["cd", cd],
    ["stat", stat],
    ["getPermissions", getPermissions],
  ]);
}
