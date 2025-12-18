export type VfsPerm = "r" | "w" | "x" | "-";
export type VfsPerms = `${VfsPerm}${VfsPerm}${VfsPerm}`;

export type VfsNode =
  | {
      readonly kind: "dir";
      readonly name: string;
      perms: VfsPerms;
      readonly children: Map<string, VfsNode>;
    }
  | {
      readonly kind: "file";
      readonly name: string;
      perms: VfsPerms;
      content: string;
    };

export class Vfs {
  readonly root: VfsNode = {
    kind: "dir",
    name: "",
    perms: "rwx",
    children: new Map(),
  };

  constructor(init?: (vfs: Vfs) => void) {
    init?.(this);
  }

  mkdirp(path: string, perms: VfsPerms = "rwx"): void {
    const { parent, name } = this.resolveParent(path);
    const existing = parent.children.get(name);
    if (existing) {
      if (existing.kind !== "dir") throw new Error(`Not a directory: ${path}`);
      return;
    }
    parent.children.set(name, { kind: "dir", name, perms, children: new Map() });
  }

  write(path: string, content: string, permsIfCreate: VfsPerms = "rw-"): void {
    const { parent, name } = this.resolveParent(path);
    this.requirePerm(parent, "w", path);
    const existing = parent.children.get(name);
    if (!existing) {
      parent.children.set(name, { kind: "file", name, perms: permsIfCreate, content });
      return;
    }
    if (existing.kind !== "file") throw new Error(`Not a file: ${path}`);
    this.requirePerm(existing, "w", path);
    existing.content = content;
  }

  read(path: string): string {
    const node = this.resolve(path);
    if (node.kind !== "file") throw new Error(`Not a file: ${path}`);
    this.requirePerm(node, "r", path);
    return node.content;
  }

  exec(path: string): string {
    const node = this.resolve(path);
    if (node.kind !== "file") throw new Error(`Not a file: ${path}`);
    this.requirePerm(node, "x", path);
    return node.content;
  }

  ls(path: string): string[] {
    const node = this.resolve(path);
    if (node.kind !== "dir") throw new Error(`Not a directory: ${path}`);
    this.requirePerm(node, "r", path);
    return [...node.children.keys()].sort();
  }

  chmod(path: string, perms: VfsPerms): void {
    const node = this.resolve(path);
    node.perms = perms;
  }

  exists(path: string): boolean {
    try {
      this.resolve(path);
      return true;
    } catch {
      return false;
    }
  }

  serialize(): VfsNode {
    const clone = (node: VfsNode): VfsNode => {
      if (node.kind === "file") {
        return { kind: "file", name: node.name, perms: node.perms, content: node.content };
      }
      const children = new Map<string, VfsNode>();
      for (const [name, child] of node.children) children.set(name, clone(child));
      return { kind: "dir", name: node.name, perms: node.perms, children };
    };
    return clone(this.root);
  }

  restore(snapshot: VfsNode): void {
    const rebuild = (node: VfsNode): VfsNode => {
      if (node.kind === "file") {
        return { kind: "file", name: node.name, perms: node.perms, content: node.content };
      }
      const children = new Map<string, VfsNode>();
      for (const [name, child] of node.children) children.set(name, rebuild(child));
      return { kind: "dir", name: node.name, perms: node.perms, children };
    };
    const rebuilt = rebuild(snapshot);
    if (rebuilt.kind !== "dir") throw new Error("Invalid VFS snapshot: root must be dir");
    (this.root as any).perms = rebuilt.perms;
    (this.root as any).children = rebuilt.children;
  }

  private resolve(path: string): VfsNode {
    if (!path.startsWith("/")) throw new Error(`Path must be absolute: ${path}`);
    const parts = path.split("/").filter(Boolean);
    let node: VfsNode = this.root;
    for (const part of parts) {
      if (node.kind !== "dir") throw new Error(`Not a directory: /${parts.join("/")}`);
      const child = node.children.get(part);
      if (!child) throw new Error(`No such path: ${path}`);
      node = child;
    }
    return node;
  }

  private resolveParent(path: string): { parent: Extract<VfsNode, { kind: "dir" }>; name: string } {
    if (!path.startsWith("/")) throw new Error(`Path must be absolute: ${path}`);
    const parts = path.split("/").filter(Boolean);
    if (parts.length === 0) throw new Error("Path has no name");
    const name = parts.pop()!;
    const parentPath = `/${parts.join("/")}`;
    const parent = this.resolve(parentPath === "/" ? "/" : parentPath);
    if (parent.kind !== "dir") throw new Error(`Not a directory: ${parentPath}`);
    return { parent, name };
  }

  private requirePerm(node: VfsNode, perm: Exclude<VfsPerm, "-">, path: string): void {
    const [r, w, x] = node.perms;
    const has = perm === "r" ? r === "r" : perm === "w" ? w === "w" : x === "x";
    if (!has) throw new Error(`Permission denied (${perm}) at ${path}`);
  }
}
