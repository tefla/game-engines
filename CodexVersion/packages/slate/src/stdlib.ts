import { History, type Snapshot, Signals, Vfs, World, type VfsNode, type VfsPerms } from "@oort/engine";
import type { BuiltinFnValue, Interpreter, RecordValue, Value } from "./interpreter";
import { parse } from "./parser";

function isRecord(value: Value): value is RecordValue {
  return typeof value === "object" && value !== null && !Array.isArray(value) && (value as any).kind === "record";
}

function expectString(value: Value, name: string): string {
  if (typeof value !== "string") throw new Error(`${name} must be a string`);
  return value;
}

function expectNumber(value: Value, name: string): number {
  if (typeof value !== "number") throw new Error(`${name} must be a number`);
  return value;
}

function expectRecord(value: Value, name: string): RecordValue {
  if (!isRecord(value)) throw new Error(`${name} must be a record`);
  return value;
}

function builtin(name: string, call: BuiltinFnValue["call"]): BuiltinFnValue {
  return { kind: "builtin", name, call };
}

function setRecordField(target: RecordValue, key: string, value: Value): void {
  target.entries[key] = value;
}

function deepClone(value: Value): Value {
  if (value === null || typeof value === "boolean" || typeof value === "number" || typeof value === "string") {
    return value;
  }
  if (Array.isArray(value)) return value.map(deepClone);
  if (isRecord(value)) {
    const entries = Object.create(null) as Record<string, Value>;
    for (const [k, v] of Object.entries(value.entries)) entries[k] = deepClone(v);
    return { kind: "record", entries };
  }
  return value;
}

function deepEqual(a: Value, b: Value): boolean {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (!deepEqual(a[i]!, b[i]!)) return false;
    }
    return true;
  }
  if (isRecord(a) && isRecord(b)) {
    const aKeys = Object.keys(a.entries).sort();
    const bKeys = Object.keys(b.entries).sort();
    if (aKeys.length !== bKeys.length) return false;
    for (let i = 0; i < aKeys.length; i++) {
      if (aKeys[i] !== bKeys[i]) return false;
    }
    for (const key of aKeys) {
      if (!deepEqual(a.entries[key]!, b.entries[key]!)) return false;
    }
    return true;
  }
  return false;
}

export type Runtime = {
  readonly vfs: Vfs;
  readonly signals: Signals;
  readonly vcs: History<VfsNode>;
  readonly namedSnapshots: Map<string, VfsNode>;
  readonly templates: Map<string, RecordValue>;
  readonly world: World<RecordValue>;
  readonly playerInventory: Set<string>;
};

function seedSpecVfs(vfs: Vfs): void {
  vfs.mkdirp("/engine", "rwx");
  vfs.mkdirp("/game", "rwx");
  vfs.mkdirp("/game/entities", "rwx");
  vfs.mkdirp("/game/systems", "rwx");
  vfs.mkdirp("/puzzles", "rwx");
  vfs.mkdirp("/secrets", "rwx");
  vfs.mkdirp("/player", "rwx");

  vfs.write(
    "/engine/physics.sl",
    `# /engine/physics.sl\n\nfn clamp_health x:\n    clamp(x, 0, 100)\n`,
    "r--",
  );
  vfs.write(
    "/engine/render.sl",
    `# /engine/render.sl\n\nfn tint color:\n    color\n`,
    "r--",
  );

  vfs.write(
    "/puzzles/door.sl",
    `# /puzzles/broken_door.sl (rw-)\n\nentity Door:\n    position: [5, 1, 0]\n    visual: box [2, 2.5, 0.1]\n    color: #8B4513\n    locked: true\n\n# BUG: Logic is inverted! Player must fix this.\non player.interact Door:\n    if not player.has \"gold_key\":\n        unlock Door\n    else:\n        say \"Door won't open\"\n\nfn unlock door:\n    door.locked = false\n    door.color = #00FF00\n    emit door.opened\n`,
    "rw-",
  );
  vfs.write("/puzzles/lever.sl", `# /puzzles/lever.sl (rw-)\n`, "rw-");
  vfs.write("/player/notes.sl", `# /player/notes.sl\n`, "rw-");
  vfs.write("/secrets/final_boss.sl", `# /secrets/final_boss.sl\n`, "---");

  vfs.chmod("/engine", "r--");
  vfs.chmod("/game", "r--");
  vfs.chmod("/game/entities", "r--");
  vfs.chmod("/game/systems", "r--");
  vfs.chmod("/puzzles", "rw-");
  vfs.chmod("/secrets", "---");
  vfs.chmod("/player", "rwx");
}

export function createRuntime(init?: (runtime: Runtime) => void): Runtime {
  const vfs = new Vfs();
  const signals = new Signals();
  const vcs = new History<VfsNode>();
  const namedSnapshots = new Map<string, VfsNode>();
  const templates = new Map<string, RecordValue>();
  const world = new World<RecordValue>();
  const playerInventory = new Set<string>();

  const runtime: Runtime = { vfs, signals, vcs, namedSnapshots, templates, world, playerInventory };
  seedSpecVfs(runtime.vfs);
  init?.(runtime);
  return runtime;
}

export function installStdlib(interpreter: Interpreter, runtime: Runtime): void {
  runtime.vcs.commit({ label: "init", state: runtime.vfs.serialize() });

  const player: RecordValue = {
    kind: "record",
    entries: Object.create(null),
  };
  player.entries.has = builtin("has", (args) => runtime.playerInventory.has(expectString(args[0], "player.has(key)")));
  player.entries.give = builtin("give", (args) => {
    runtime.playerInventory.add(expectString(args[0], "player.give(key)"));
    return null;
  });
  interpreter.globals.defineLet("player", player);

  interpreter.defineBuiltin(
    "say",
    (args) => {
      const message = args.map(String).join(" ");
      console.log(message);
      return null;
    },
  );
  interpreter.defineBuiltin("notify", (args) => {
    const message = args.map(String).join(" ");
    console.log(message);
    return null;
  });
  interpreter.defineBuiltin("play_sound", (args) => {
    const name = args.map(String).join(" ");
    console.log(`[sound] ${name}`);
    return null;
  });

  interpreter.defineBuiltin("abs", (args) => Math.abs(expectNumber(args[0], "abs(x)")));
  interpreter.defineBuiltin("min", (args) => Math.min(expectNumber(args[0], "min(a,b)"), expectNumber(args[1], "min(a,b)")));
  interpreter.defineBuiltin("max", (args) => Math.max(expectNumber(args[0], "max(a,b)"), expectNumber(args[1], "max(a,b)")));
  interpreter.defineBuiltin("clamp", (args) => {
    const x = expectNumber(args[0], "clamp(x,lo,hi)");
    const lo = expectNumber(args[1], "clamp(x,lo,hi)");
    const hi = expectNumber(args[2], "clamp(x,lo,hi)");
    return Math.min(hi, Math.max(lo, x));
  });
  interpreter.defineBuiltin("lerp", (args) => {
    const a = expectNumber(args[0], "lerp(a,b,t)");
    const b = expectNumber(args[1], "lerp(a,b,t)");
    const t = expectNumber(args[2], "lerp(a,b,t)");
    return a + (b - a) * t;
  });

  interpreter.defineBuiltin("vec3", (args) => [expectNumber(args[0], "vec3(x,y,z)"), expectNumber(args[1], "vec3(x,y,z)"), expectNumber(args[2], "vec3(x,y,z)")]);
  interpreter.defineBuiltin("dot", (args) => {
    const a = args[0];
    const b = args[1];
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== 3 || b.length !== 3) throw new Error("dot(a,b) expects vec3");
    return expectNumber(a[0]!, "dot") * expectNumber(b[0]!, "dot") + expectNumber(a[1]!, "dot") * expectNumber(b[1]!, "dot") + expectNumber(a[2]!, "dot") * expectNumber(b[2]!, "dot");
  });
  interpreter.defineBuiltin("distance", (args) => {
    const a = args[0];
    const b = args[1];
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== 3 || b.length !== 3) throw new Error("distance(a,b) expects vec3");
    const dx = expectNumber(a[0]!, "distance") - expectNumber(b[0]!, "distance");
    const dy = expectNumber(a[1]!, "distance") - expectNumber(b[1]!, "distance");
    const dz = expectNumber(a[2]!, "distance") - expectNumber(b[2]!, "distance");
    return Math.sqrt(dx * dx + dy * dy + dz * dz);
  });
  interpreter.defineBuiltin("normalize", (args) => {
    const v = args[0];
    if (!Array.isArray(v) || v.length !== 3) throw new Error("normalize(v) expects vec3");
    const x = expectNumber(v[0]!, "normalize");
    const y = expectNumber(v[1]!, "normalize");
    const z = expectNumber(v[2]!, "normalize");
    const len = Math.sqrt(x * x + y * y + z * z);
    if (len === 0) return [0, 0, 0];
    return [x / len, y / len, z / len];
  });

  interpreter.defineBuiltin("read", (args) => runtime.vfs.read(expectString(args[0], "read(path)")));
  interpreter.defineBuiltin("ls", (args) => runtime.vfs.ls(expectString(args[0], "ls(path)")));
  interpreter.defineBuiltin("chmod", (args) => {
    const path = expectString(args[0], "chmod(path,perms)");
    const perms = expectString(args[1], "chmod(path,perms)") as VfsPerms;
    runtime.vfs.chmod(path, perms);
    runtime.vcs.commit({ state: runtime.vfs.serialize() });
    return null;
  });
  interpreter.defineBuiltin("write", async (args, it) => {
    const path = expectString(args[0], "write(path,content)");
    const content = expectString(args[1], "write(path,content)");
    runtime.vfs.write(path, content);
    runtime.vcs.commit({ state: runtime.vfs.serialize() });
    if (path.endsWith(".sl")) {
      const program = parse(content);
      await it.hotReload(program, it.globals, path);
    }
    return null;
  });

  interpreter.defineBuiltin("box", (args) => {
    const size = args.length === 1 && Array.isArray(args[0]) ? args[0] : args;
    if (!Array.isArray(size) || size.length !== 3) throw new Error("box expects [x,y,z]");
    return { kind: "record", entries: Object.assign(Object.create(null), { kind: "box", size }) };
  });
  interpreter.defineBuiltin("plane", (args) => ({ kind: "record", entries: Object.assign(Object.create(null), { kind: "plane", args }) }));
  interpreter.defineBuiltin("line", (args) => ({ kind: "record", entries: Object.assign(Object.create(null), { kind: "line", args }) }));
  interpreter.defineBuiltin("point", (args) => ({ kind: "record", entries: Object.assign(Object.create(null), { kind: "point", args }) }));

  interpreter.defineBuiltin("create_entity", (args) => {
    const name = expectString(args[0], "create_entity(name,body)");
    const body = expectRecord(args[1], "create_entity(name,body)");
    const existing = runtime.templates.get(name);
    if (existing) {
      for (const k of Object.keys(existing.entries)) delete existing.entries[k];
      for (const [k, v] of Object.entries(body.entries)) existing.entries[k] = v;
      setRecordField(existing, "name", name);
      runtime.templates.set(name, existing);
      return existing;
    }

    const out: RecordValue = {
      kind: "record",
      entries: Object.assign(Object.create(null), body.entries, { name }),
    };
    runtime.templates.set(name, out);
    return out;
  });

  interpreter.defineBuiltin("entity", (args, it) => {
    const nameArg = args[0];
    const name =
      typeof nameArg === "string"
        ? nameArg
        : isRecord(nameArg) && typeof nameArg.entries.name === "string"
          ? (nameArg.entries.name as string)
          : null;
    if (!name) throw new Error("entity Name: {..} requires a name");
    const body = expectRecord(args[1], "entity Name: {..}");
    const created = (it.globals.get("create_entity") as BuiltinFnValue).call([name, body], it) as any as RecordValue;
    const existing = it.globals.tryGet(name);
    if (existing.found && isRecord(existing.value)) {
      if (existing.value === created) return created;
      for (const k of Object.keys(existing.value.entries)) delete existing.value.entries[k];
      for (const [k, v] of Object.entries(created.entries)) existing.value.entries[k] = v;
      return existing.value;
    }
    it.globals.defineLet(name, created);
    return created;
  });

  interpreter.defineBuiltin("spawn", (args, it) => {
    const input = args[0];
    let template: RecordValue;
    if (typeof input === "string") {
      const found = it.globals.tryGet(input);
      if (!found.found || !isRecord(found.value)) throw new Error(`Unknown entity template: ${input}`);
      template = found.value;
    } else {
      template = expectRecord(input, "spawn(entity)");
    }

    const instance = deepClone(template) as RecordValue;
    const id = runtime.world.spawn(instance);
    setRecordField(instance, "id", id);
    return instance;
  });

  interpreter.defineBuiltin("destroy", (args) => {
    const target = args[0];
    const id =
      typeof target === "string"
        ? target
        : isRecord(target) && typeof target.entries.id === "string"
          ? (target.entries.id as string)
          : null;
    if (!id) throw new Error("destroy(entity|id) requires an entity id");
    runtime.world.destroy(id);
    return null;
  });

  interpreter.defineBuiltin("find", (args) => {
    const query = expectRecord(args[0], "find(query)");
    return runtime.world.find((entity) => {
      for (const [key, expected] of Object.entries(query.entries)) {
        const actual = entity.entries[key];
        if (actual === undefined) return false;
        if (!deepEqual(actual, expected)) return false;
      }
      return true;
    });
  });

  const vcsModule: RecordValue = {
    kind: "record",
    entries: Object.create(null),
  };

  const snapshotFn = builtin("snapshot", (args) => {
    const label = args[0] ? expectString(args[0], "vcs.snapshot(label)") : undefined;
    const state = runtime.vfs.serialize();
    const snap: Snapshot<VfsNode> = { label, state };
    if (label) runtime.namedSnapshots.set(label, state);
    runtime.vcs.commit(snap);
    return null;
  });

  const restoreFn = builtin("restore", (args) => {
    const label = expectString(args[0], "vcs.restore(label)");
    const state = runtime.namedSnapshots.get(label);
    if (!state) throw new Error(`No snapshot named '${label}'`);
    runtime.vfs.restore(state);
    runtime.vcs.commit({ label: `restore:${label}`, state: runtime.vfs.serialize() });
    return null;
  });

  const undoFn = builtin("undo", () => {
    const prev = runtime.vcs.undo();
    if (!prev) return null;
    runtime.vfs.restore(prev.state);
    return null;
  });

  const redoFn = builtin("redo", () => {
    const next = runtime.vcs.redo();
    if (!next) return null;
    runtime.vfs.restore(next.state);
    return null;
  });

  vcsModule.entries.snapshot = snapshotFn;
  vcsModule.entries.restore = restoreFn;
  vcsModule.entries.undo = undoFn;
  vcsModule.entries.redo = redoFn;

  interpreter.registerModule("vcs", vcsModule);
}
