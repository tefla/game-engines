import { describe, expect, test } from "bun:test";

import { lex } from "../src/lexer";
import { parse } from "../src/parser";
import { Interpreter } from "../src/interpreter";
import { createRuntime, installStdlib } from "../src/stdlib";

describe("slate", () => {
  test("lexer emits INDENT/DEDENT", () => {
    const tokens = lex(`fn hello:\n    1\n`);
    const types = tokens.map((t) => t.type);
    expect(types).toContain("INDENT");
    expect(types).toContain("DEDENT");
  });

  test("if is an expression", async () => {
    const program = parse(
      `var health = 100\nlet status = if health > 50:\n    "healthy"\nelse:\n    "wounded"\nstatus\n`,
    );
    const it = new Interpreter();
    const out = await it.run(program);
    expect(out).toBe("healthy");
  });

  test("match binds record fields", async () => {
    const program = parse(
      `let item = {type: "key", color: "red"}\nmatch item:\n    {type: "key", color} => color\n    _ => "no"\n`,
    );
    const it = new Interpreter();
    const out = await it.run(program);
    expect(out).toBe("red");
  });

  test("signals on/emit can mutate vars", async () => {
    const runtime = createRuntime();
    const it = new Interpreter({ signals: runtime.signals });
    installStdlib(it, runtime);

    const program = parse(`var x = 0\non door.opened:\n    x = 1\nemit door.opened\nx\n`);
    const out = await it.run(program);
    expect(out).toBe(1);
  });

  test("vfs permissions deny writes to /engine", async () => {
    const runtime = createRuntime();
    const it = new Interpreter();
    installStdlib(it, runtime);

    const program = parse(`write "/engine/physics.sl", "nope"\n`);
    await expect(it.run(program)).rejects.toThrow(/Permission denied/);
  });

  test("vcs undo restores previous vfs snapshot", async () => {
    const runtime = createRuntime();
    const it = new Interpreter();
    installStdlib(it, runtime);

    const program = parse(
      `write "/puzzles/tmp.sl", "hi"\nimport vcs\nvcs.undo\nls "/puzzles"\n`,
    );
    const out = await it.run(program);
    expect(Array.isArray(out)).toBe(true);
    expect(out).not.toContain("tmp.sl");
  });

  test("spawn/find/destroy manages world entities", async () => {
    const runtime = createRuntime();
    const it = new Interpreter();
    installStdlib(it, runtime);

    const program = parse(
      `entity Enemy:\n    type: "Enemy"\n    hp: 10\n\nlet a = spawn Enemy\nlet b = spawn Enemy\na.hp = 5\ndestroy a\nfind {type: "Enemy"}\n`,
    );
    const out = await it.run(program);

    expect(Array.isArray(out)).toBe(true);
    expect(out).toHaveLength(1);
    expect(runtime.world.size()).toBe(1);
    expect(runtime.templates.get("Enemy")?.entries.hp).toBe(10);
  });

  test("import path requires execute permission", async () => {
    const runtime = createRuntime();
    runtime.vfs.write("/puzzles/lib.sl", `let x = 1\n`, "rwx");
    runtime.vfs.write("/puzzles/noexec.sl", `let y = 2\n`, "rw-");

    const it = new Interpreter({
      loadScript: async (path) => parse(runtime.vfs.exec(path)),
    });

    expect(await it.run(parse(`import "/puzzles/lib.sl"\nx\n`))).toBe(1);
    await expect(it.run(parse(`import "/puzzles/noexec.sl"\ny\n`))).rejects.toThrow(/Permission denied \(x\)/);
  });

  test("write hot-reloads on-handlers per file", async () => {
    const runtime = createRuntime();
    const it = new Interpreter({ signals: runtime.signals });
    installStdlib(it, runtime);

    const path = "/puzzles/hot.sl";
    const initial = `var x = 0\non door.opened:\n    x = 1\n`;
    await it.runWithSource(parse(initial), it.globals, path);

    expect(await it.run(parse(`emit door.opened\nx\n`))).toBe(1);

    const updated = `var x = 0\non door.opened:\n    x = 2\n`;
    const updatedEscaped = updated.replaceAll("\\", "\\\\").replaceAll("\"", "\\\"").replaceAll("\n", "\\n");
    await it.run(parse(`write "${path}", "${updatedEscaped}"\nemit door.opened\nx\n`));

    expect(await it.run(parse(`x\n`))).toBe(2);
  });
});
