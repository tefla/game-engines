// Virtual Filesystem Implementation

import {
  type Permissions,
  parsePermissions,
  permissionsToString,
  DEFAULT_PERMISSIONS,
  READ_ONLY,
  HIDDEN,
  PermissionError,
} from "./permissions";

// Base interface for filesystem nodes
export interface VNode {
  name: string;
  permissions: Permissions;
  parent: VDirectory | null;
  getPath(): string;
  isFile(): this is VFile;
  isDirectory(): this is VDirectory;
}

// Virtual File
export class VFile implements VNode {
  name: string;
  permissions: Permissions;
  parent: VDirectory | null = null;
  content: string;
  metadata: Map<string, unknown> = new Map();

  constructor(name: string, content: string = "", permissions?: Permissions) {
    this.name = name;
    this.content = content;
    this.permissions = permissions ?? { ...DEFAULT_PERMISSIONS };
  }

  getPath(): string {
    if (!this.parent) return "/" + this.name;
    const parentPath = this.parent.getPath();
    return parentPath === "/" ? "/" + this.name : parentPath + "/" + this.name;
  }

  isFile(): this is VFile {
    return true;
  }

  isDirectory(): this is VDirectory {
    return false;
  }
}

// Virtual Directory
export class VDirectory implements VNode {
  name: string;
  permissions: Permissions;
  parent: VDirectory | null = null;
  children: Map<string, VNode> = new Map();

  constructor(name: string, permissions?: Permissions) {
    this.name = name;
    this.permissions = permissions ?? { ...DEFAULT_PERMISSIONS };
  }

  getPath(): string {
    if (!this.parent) return "/";
    const parentPath = this.parent.getPath();
    return parentPath === "/" ? "/" + this.name : parentPath + "/" + this.name;
  }

  isFile(): this is VFile {
    return false;
  }

  isDirectory(): this is VDirectory {
    return true;
  }

  addChild(node: VNode): void {
    node.parent = this;
    this.children.set(node.name, node);
  }

  getChild(name: string): VNode | undefined {
    return this.children.get(name);
  }

  removeChild(name: string): boolean {
    return this.children.delete(name);
  }

  listChildren(): VNode[] {
    return Array.from(this.children.values());
  }
}

// File info returned by ls
export interface FileInfo {
  name: string;
  path: string;
  isDirectory: boolean;
  permissions: string;
  size?: number;
}

// Main VFS class
export class VirtualFileSystem {
  private root: VDirectory;
  private currentDir: VDirectory;

  constructor() {
    this.root = new VDirectory("");
    this.currentDir = this.root;
  }

  // Initialize with default game structure
  static createGameFileSystem(): VirtualFileSystem {
    const vfs = new VirtualFileSystem();

    // Create standard directories per spec
    // Create all directories first with default permissions
    vfs.mkdir("/engine");
    vfs.mkdir("/engine/physics.sl");
    vfs.mkdir("/engine/render.sl");

    vfs.mkdir("/game");
    vfs.mkdir("/game/entities");
    vfs.mkdir("/game/systems");

    vfs.mkdir("/puzzles");

    vfs.mkdir("/secrets");

    vfs.mkdir("/player");

    // Now set permissions (children before parents to avoid permission issues)
    vfs.chmod("/engine/physics.sl", READ_ONLY);
    vfs.chmod("/engine/render.sl", READ_ONLY);
    vfs.chmod("/engine", READ_ONLY);

    vfs.chmod("/game/entities", READ_ONLY);
    vfs.chmod("/game/systems", READ_ONLY);
    vfs.chmod("/game", READ_ONLY);

    vfs.chmod("/puzzles", { read: true, write: true, execute: false });

    vfs.chmod("/secrets", HIDDEN);

    return vfs;
  }

  // Resolve a path to a node
  private resolve(path: string): VNode | null {
    const normalized = this.normalizePath(path);
    if (normalized === "/") return this.root;

    const parts = normalized.split("/").filter((p) => p.length > 0);
    let current: VNode = this.root;

    for (const part of parts) {
      if (!current.isDirectory()) return null;
      const child = current.getChild(part);
      if (!child) return null;
      current = child;
    }

    return current;
  }

  // Resolve parent directory and get target name
  private resolveParent(path: string): { parent: VDirectory; name: string } | null {
    const normalized = this.normalizePath(path);
    const parts = normalized.split("/").filter((p) => p.length > 0);

    if (parts.length === 0) return null;

    const name = parts.pop()!;
    const parentPath = "/" + parts.join("/");
    const parent = this.resolve(parentPath);

    if (!parent || !parent.isDirectory()) return null;

    return { parent, name };
  }

  // Normalize a path (resolve ., .., etc.)
  private normalizePath(path: string): string {
    // Handle relative paths
    if (!path.startsWith("/")) {
      path = this.currentDir.getPath() + "/" + path;
    }

    const parts = path.split("/").filter((p) => p.length > 0);
    const result: string[] = [];

    for (const part of parts) {
      if (part === ".") continue;
      if (part === "..") {
        result.pop();
      } else {
        result.push(part);
      }
    }

    return "/" + result.join("/");
  }

  // Check permissions before operation
  private checkPermission(
    node: VNode,
    operation: "read" | "write" | "execute"
  ): void {
    // Also check parent directory permissions for read operations
    if (operation === "read") {
      let current = node.parent;
      while (current) {
        if (!current.permissions.read) {
          throw new PermissionError(current.getPath(), "read");
        }
        current = current.parent;
      }
    }

    if (!node.permissions[operation]) {
      throw new PermissionError(node.getPath(), operation);
    }
  }

  // ============ Public API ============

  // Read file contents
  read(path: string): string {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`File not found: ${path}`);
    }
    if (!node.isFile()) {
      throw new Error(`Not a file: ${path}`);
    }

    this.checkPermission(node, "read");
    return node.content;
  }

  // Write file contents
  write(path: string, content: string): void {
    let node = this.resolve(path);

    if (node) {
      if (!node.isFile()) {
        throw new Error(`Not a file: ${path}`);
      }
      this.checkPermission(node, "write");
      node.content = content;
    } else {
      // Create new file
      const resolved = this.resolveParent(path);
      if (!resolved) {
        throw new Error(`Invalid path: ${path}`);
      }

      this.checkPermission(resolved.parent, "write");

      const file = new VFile(resolved.name, content);
      resolved.parent.addChild(file);
    }
  }

  // Check if path exists
  exists(path: string): boolean {
    const node = this.resolve(path);
    if (!node) return false;

    // Check if we have permission to see it
    try {
      this.checkPermission(node, "read");
      return true;
    } catch {
      return false;
    }
  }

  // List directory contents
  ls(path: string = "/"): FileInfo[] {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`Directory not found: ${path}`);
    }
    if (!node.isDirectory()) {
      throw new Error(`Not a directory: ${path}`);
    }

    this.checkPermission(node, "read");

    const result: FileInfo[] = [];
    for (const child of node.listChildren()) {
      // Only show files we have permission to see
      if (child.permissions.read || this.canSeeChild(node, child)) {
        result.push({
          name: child.name,
          path: child.getPath(),
          isDirectory: child.isDirectory(),
          permissions: permissionsToString(child.permissions),
          size: child.isFile() ? child.content.length : undefined,
        });
      }
    }

    return result.sort((a, b) => {
      // Directories first, then alphabetically
      if (a.isDirectory && !b.isDirectory) return -1;
      if (!a.isDirectory && b.isDirectory) return 1;
      return a.name.localeCompare(b.name);
    });
  }

  // Helper to determine if a child should be visible
  private canSeeChild(parent: VDirectory, child: VNode): boolean {
    // Parent has read permission implies children are visible in listing
    // but content may not be readable
    return parent.permissions.read;
  }

  // Create directory
  mkdir(path: string, permissions?: Permissions): void {
    const resolved = this.resolveParent(path);
    if (!resolved) {
      throw new Error(`Invalid path: ${path}`);
    }

    if (resolved.parent.getChild(resolved.name)) {
      throw new Error(`Already exists: ${path}`);
    }

    this.checkPermission(resolved.parent, "write");

    const dir = new VDirectory(resolved.name, permissions);
    resolved.parent.addChild(dir);
  }

  // Remove file or empty directory
  rm(path: string): void {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`Not found: ${path}`);
    }
    if (!node.parent) {
      throw new Error(`Cannot remove root directory`);
    }

    this.checkPermission(node.parent, "write");

    if (node.isDirectory() && node.children.size > 0) {
      throw new Error(`Directory not empty: ${path}`);
    }

    node.parent.removeChild(node.name);
  }

  // Change permissions
  chmod(path: string, permissions: string | Permissions): void {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`Not found: ${path}`);
    }

    // Need write permission on parent to change permissions
    if (node.parent) {
      this.checkPermission(node.parent, "write");
    }

    if (typeof permissions === "string") {
      node.permissions = parsePermissions(permissions);
    } else {
      node.permissions = { ...permissions };
    }
  }

  // Get permissions
  getPermissions(path: string): string {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`Not found: ${path}`);
    }
    return permissionsToString(node.permissions);
  }

  // Get current directory
  pwd(): string {
    return this.currentDir.getPath();
  }

  // Change directory
  cd(path: string): void {
    const node = this.resolve(path);
    if (!node) {
      throw new Error(`Directory not found: ${path}`);
    }
    if (!node.isDirectory()) {
      throw new Error(`Not a directory: ${path}`);
    }

    this.checkPermission(node, "execute");
    this.currentDir = node;
  }

  // Get file/directory info
  stat(path: string): FileInfo | null {
    const node = this.resolve(path);
    if (!node) return null;

    try {
      this.checkPermission(node, "read");
    } catch {
      return null;
    }

    return {
      name: node.name,
      path: node.getPath(),
      isDirectory: node.isDirectory(),
      permissions: permissionsToString(node.permissions),
      size: node.isFile() ? node.content.length : undefined,
    };
  }

  // Copy file
  cp(src: string, dest: string): void {
    const srcNode = this.resolve(src);
    if (!srcNode) {
      throw new Error(`Source not found: ${src}`);
    }
    if (!srcNode.isFile()) {
      throw new Error(`Cannot copy directory: ${src}`);
    }

    this.checkPermission(srcNode, "read");

    const content = srcNode.content;
    this.write(dest, content);
  }

  // Move/rename
  mv(src: string, dest: string): void {
    this.cp(src, dest);
    this.rm(src);
  }

  // Get a node directly (for advanced operations)
  getNode(path: string): VNode | null {
    const node = this.resolve(path);
    if (!node) return null;

    try {
      this.checkPermission(node, "read");
      return node;
    } catch {
      return null;
    }
  }

  // Create a file with specific permissions
  createFile(
    path: string,
    content: string,
    permissions?: Permissions
  ): VFile {
    const resolved = this.resolveParent(path);
    if (!resolved) {
      throw new Error(`Invalid path: ${path}`);
    }

    this.checkPermission(resolved.parent, "write");

    const file = new VFile(resolved.name, content, permissions);
    resolved.parent.addChild(file);
    return file;
  }

  // Import Slate source file
  importSlateFile(path: string, content: string, permissions?: Permissions): void {
    if (!path.endsWith(".sl")) {
      path = path + ".sl";
    }
    this.createFile(path, content, permissions);
  }
}
